/* Practical Assessment
 *
 *  ==Overview==
 * PNumber:   P2672522
 * Module:    CTEC1903
 * Objective: Complete an application library in Scala to meet a provided test harness.
 */
package cw

/** Represents a direction */
sealed private trait Direction { val Key: String; val Tuple: Game.VectorTuple }

/** Represents the direction left */
private case object Left extends Direction {
  val Key = "a"; val Tuple = (-1, 0)
}

/** Represents the direction right */
private case object Right extends Direction {
  val Key = "d"; val Tuple = (1, 0)
}

/** Represents the direction up */
private case object Up extends Direction { val Key = "w"; val Tuple = (0, -1) }

/** Represents the direction down */
private case object Down extends Direction { val Key = "s"; val Tuple = (0, 1) }

/** Represents a position on the field
  *
  * @constructor
  *   Returns an immutable vector
  * @param x
  *   Value of x component
  * @param y
  *   Value of y component
  */
sealed private class Vector(vectorTuple: (Int, Int)) {

  /** Returns vector's x component */
  def x = vectorTuple._1

  /** Returns vector's y component */
  def y = vectorTuple._2

  /** Returns the sum of two vectors
    *
    * @param v
    *   Vector to add to this vector
    */
  def +(v: Vector): Vector = new Vector(x + v.x, y + v.y)

  /** Returns the difference of two vectors
    *
    * @param v
    *   Vector to add to this vector
    */
  def -(v: Vector): Vector = new Vector(x - v.x, y - v.y)

  /** Returns true if components are equal to the given vector
    *
    * @param v
    *   Vector to compare with this vector
    */
  def ==(v: Vector): Boolean = (x == v.x && y == v.y)

  /** Returns true if components are not equal to the given vector
    *
    * @param v
    *   Vector to compare with this vector
    */
  def !=(v: Vector): Boolean = !(x == v.x && y == v.y)

  /** Returns a vector with the minimum components of the compared vectors
    *
    * @param v
    *   Vector to compare with this vector
    */
  def min(v: Vector): Vector = Vector(x min v.x, y min v.y)

  /** Returns a vector with the maximum components of the compared vectors
    *
    * @param v
    *   Vector to compare with this vector
    */
  def max(v: Vector): Vector = Vector(x max v.x, y max v.y)

  /** Returns true if vector is within the rectangle described by the given
    * vectors
    *
    * @param v1
    *   Vector representing one corner of rectangle
    * @param v2
    *   Vector representing opposing corner to v1
    */
  def isWithin(v1: Vector, v2: Vector): Boolean = {
    val Min = v1 min v2; val Max = v1 max v2
    (x >= Min.x && x <= Max.x) && (y >= Min.y && y <= Max.y)
  }

  /** Returns vector value as an (x, y) tuple */
  def toTuple(): Game.VectorTuple = (x, y)

  /** Returns vector as a formatted string */
  override def toString(): String = s"{ x: $x, y: $y }"
}

/** Companion object for Vector */
private object Vector {

  /** Returns vector from given vector tuple */
  def apply(v: Game.VectorTuple) = new Vector(v)

  /** Returns vector from given components */
  def apply(x: Int = 0, y: Int = 0) = new Vector((x, y))

  /** Returns unit vector from given direction */
  def apply(direction: Direction) = new Vector(direction.Tuple)

  /** Returns 'empty' vector (-1, -1) */
  def empty() = Vector(-1, -1)
}

/** Represents a simple game where a player moves on a field and collects coins
  *
  * @constructor
  *   Returns a game instance
  * @param Data
  *   Object containing game parameters
  */
sealed class Game(val Data: GameBuilder.GameData) {

  /** Represents the game field */
  private object Field {

    /** Cells array length */
    private val Size = 10

    /** Represents a position on the field
      *
      * @constructor
      *   Returns a vector
      * @param IsWall
      *   True if cell cannot be moved through
      * @param value
      *   Value of coin in cell
      */
    class Cell(val IsWall: Boolean = false, private var value: Int = 0) {

      /** Returns the current coin value */
      def getValue(): Int = value

      /** Returns true if value is above 0 */
      def isCoin(): Boolean = (value > 0)

      /** Sets coin value to 0 and returns the coin value */
      def collect(): Int = {
        val Res = value
        value = 0; Res
      }
    }

    /** Cells array */
    val Cells = Array ofDim [Cell] (Size, Size)

    /* Initialise field with walls and coins */
    for { x <- 0 until Size; y <- 0 until Size } Cells(x)(y) = new Cell()
    Data.Walls foreach (wall => Cells(wall._1)(wall._2) = new Cell(true))
    Data.Coins foreach (coin =>
      Cells(coin._1)(coin._2) = new Cell(value = coin._3)
    )

    /** Returns cell value at the given position
      *
      * @param position
      *   Position vector of cell
      */
    def getCell(position: Vector): Cell = Cells(position.x)(position.y)

    /** Returns cell value at the given position
      *
      * @param x
      *   Cell x coordinate
      * @param y
      *   Cell y coordinate
      */
    def getCell(x: Int, y: Int): Cell = Cells(x)(y)

    /** Returns true if given position is valid and empty */
    def isValidPosition(position: Vector): Boolean = {
      position.x >= 0 && position.x < Size &&
      position.y >= 0 && position.y < Size &&
      !getCell(position).IsWall
    }

    /** Returns string of moves to collect all coins on field */
    def getSolution(): String = {
      var result = ""

      /* Get all coin positions on field */
      val Positions: Seq[Vector] = for {
        x <- 0 until Size;
        y <- 0 until Size
        if (getCell(x, y).isCoin)
      } yield (Vector(x, y))

      /* Create array of corresponding visited flags set to false */
      val Visited = Array.fill(Positions.length)(false)

      /* Returns path from given position to remaining coins
       *
       * @param pathTo
       *   String Path string to start position
       * @param start
       *   Vector Position to start at
       */
      def findPath(
          pathTo: String = "",
          start: Vector = Player.position
      ): String = {
        var result = ""
        for (i <- 0 until Positions.length) {
          val Position = Positions(i)
          if (!Visited(i)) {
            // Get path from start to position
            var path = getPath(start, Position)
            // If path found and no further points are unvisited,
            // set visited flag to true and find path to other coins
            if (!path.isEmpty) {
              Visited(i) = true
              if (!Visited.reduce((a, c) => a && c))
                path = findPath(path, Position)
            }
            // If final path found, set it to the result
            if (!path.isEmpty) result = pathTo + path
            // Otherwise, remove visited flag
            else Visited(i) = false
          }
        }
        result
      }
      /* Start recursively searching paths and return the result */
      findPath()
    }

    /** Returns a string of moves to traverse between two points
      *
      * @param start
      *   Start point of path
      * @param end
      *   Destination point
      * @param [recursive]
      *   Whether function was called recursively
      */
    def getPath(
        start: Vector,
        end: Vector,
        recursive: Boolean = false
    ): String = {
      /* Return empty string if end point is invalid */
      if (!isValidPosition(end) || start == end) ""

      var path = ""
      var turn = Vector()

      /* Reduces position values to true if all positions are valid */
      def reducer(x: Int = -1, y: Int = -1): (Boolean, Int) => Boolean =
        (acc: Boolean, cur: Int) =>
          acc && isValidPosition(
            Vector(if (x < 0) cur else x, if (y < 0) cur else y)
          )

      if (start.y > end.y) { // End is up
        // If path upwards is not obstructed, set path and turn point
        if ((end.y to start.y).foldLeft(true)(reducer(x = start.x))) {
          path = Up.Key * (start.y - end.y)
          turn = Vector(start.x, end.y)
        }
      }
      if (start.y < end.y) { // End is down
        // If path downwards is not obstructed, set path and turn point
        if ((start.y to end.y).foldLeft(true)(reducer(x = start.x))) {
          path = Down.Key * (end.y - start.y)
          turn = Vector(start.x, end.y)
        }
      }

      /* Recursively append the path from turn to end if valid */
      if (!recursive && !path.isEmpty) {
        val pathFromTurn = getPath(turn, end, true)
        if (pathFromTurn.isEmpty && turn != end) path = ""
        else path += pathFromTurn
      }

      if (path.isEmpty) {
        if (start.x > end.x) { // End is left
          // If path to left is not obstructed, set path and turn point
          if ((end.x to start.x).foldLeft(true)(reducer(y = start.y))) {
            path = Left.Key * (start.x - end.x)
            turn = Vector(end.x, start.y)
          }
        }
        if (start.x < end.x) { // End is right
          // If path to right is not obstructed, set path and turn point
          if ((start.x to end.x).foldLeft(true)(reducer(y = start.y))) {
            path = Right.Key * (end.x - start.x)
            turn = Vector(end.x, start.y)
          }
        }

        /* Recursively append the path from turn to end if valid */
        if (!recursive && !path.isEmpty) {
          val pathFromTurn = getPath(turn, end, true)
          if (pathFromTurn.isEmpty && turn != end) path = ""
          else path += pathFromTurn
        }
      }

      path /* Return path */
    }

    /** Returns the total value of coins remaining */
    def remainingScore(): Int = {
      Cells.flatten.foldLeft(0)((acc, cell) =>
        if (cell.isCoin) acc + cell.getValue else acc
      )
    }

    /** Print field to console */
    def display(): Unit = {
      for (y <- 0 until Size) {
        for (x <- 0 until Size) {
          val position = Vector(x, y)
          val AtPlayer = position == Player.position
          val AtSave = position == Player.save
          val SaveActive = Player.save != Vector.empty
          val IsWall = getCell(x, y).IsWall
          val IsCoin = getCell(x, y).isCoin
          val IsSaved =
            SaveActive && position.isWithin(Player.position, Player.save)

          if (AtPlayer && SaveActive) print("(P)")
          else if (AtPlayer) print(" P ")
          else if (AtSave) print("(S)")
          else if (IsWall) print(" X ")
          else if (IsCoin) print(getCell(x, y).getValue)
          else if (IsSaved) print("(+)")
          else print(" + ")
        }
        println()
      }
    }
  }

  /** Represents the game's player */
  private object Player {

    /** Vector representing player position */
    var position = Vector(Data.PlayerPosition)

    /** Vector representing a visited, 'saved', point
      *
      *   - When not in use, is set to (-1, -1)
      *   - From a saved point a rectangle is drawn
      *   - Any coins within the rectangle are collected
      *   - If there are 9 or more cells in the save area it resets
      */
    var save = Vector.empty

    /** Current player score from collecting coins */
    var score: Int = 0

    /** Moves the player one cell in the given direction on the field
      *
      * @param direction
      *   Direction to move player
      */
    def move(direction: Direction): Unit = {
      if (Field isValidPosition (position + Vector(direction))) {
        position += Vector(direction); collectCoins
      }
    }

    /** Moves the player by the given string of moves
      *
      * @param moves
      *   String of w, a, s, and d characters
      */
    def move(moves: String): Unit = {
      moves split ("") foreach (key => {
        key match {
          case Left.Key  => Player move Left
          case Up.Key    => Player move Up
          case Right.Key => Player move Right
          case Down.Key  => Player move Down
        }
      })
    }

    /** Collects coins within save rectangle
      *
      *   - Checks for coins at the player position
      *   - Checks for coins in cells within the 'save' rectangle
      *   - Any coins within the 'save' rectangle are collected
      *   - Player score is increased by the total amount
      *   - Coins are cleared from field
      *   - The 'save' rectangle is the rectangle described by a diagonal equal
      *     to the line segment connecting the player and the save point, for
      *     example:
      *     {{{
      *        1  2  3  4  5
      *      1   (P)( )( )   // Brackets show 'saved' cells
      *      2   ( )( )(C)   // P, S, and C respectively denote:
      *      3   ( )( )( )   //  a player, save point, and coin
      *      4   ( )( )(S)
      *      5
      *     }}}
      */
    def collectCoins(): Unit = {
      /* Collect coin at position */
      val Cell = Field getCell position
      if (Cell.isCoin) score += Cell.collect

      /* Collect coins in rectangle if it covers 9 or more cells */
      if (save != Vector.empty) {
        // Get opposing corners of rectangle
        val Min: Vector = position min save // Top-left
        val Max: Vector = position max save // Bottom-right
        val Diff: Vector = (Max - Min) + Vector(1, 1)
        val Area: Int = Diff.x * Diff.y
        if (Area >= 9) {
          // Add each coin value to the score
          for (x <- Min.x to Max.x; y <- Min.y to Max.y) {
            val Cell = Field getCell (x, y)
            if (Cell.isCoin) score += Cell.collect
          }
          // Reset save vector to (-1, -1)
          save = Vector.empty
        }
      }
    }
  }

  /* The following methods in this class are all required for GameTest.scala or GameApp.scala */

  /** Print field to console */
  def printField(): Unit = { Field.display }

  /** Returns player position as a tuple, in (x,y) order */
  def getPlayerPos(): Game.VectorTuple = Player.position.toTuple

  /** Returns the current score */
  def getScore(): Int = Player.score

  /** Moves player one cell left */
  def moveLeft(): Unit = { Player move Left }

  /** Moves player one cell right */
  def moveRight(): Unit = { Player move Right }

  /** Moves player one cell up */
  def moveUp(): Unit = { Player move Up }

  /** Moves player one cell down */
  def moveDown(): Unit = { Player move Down }

  /** Moves player a given number of cells left
    *
    * @param n
    *   Number of cells to move player
    */
  def moveLeft(n: Int): Unit = { Game.repeat(n)(Player move Left) }

  /** Moves player a given number of cells right
    *
    * @param n
    *   Number of cells to move player
    */
  def moveRight(n: Int): Unit = { Game.repeat(n)(Player move Right) }

  /** Moves player a given number of cells up
    *
    * @param n
    *   Number of cells to move player
    */
  def moveUp(n: Int): Unit = { Game.repeat(n)(Player move Up) }

  /** Moves player a given number of cells down
    *
    * @param n
    *   Number of cells to move player
    */
  def moveDown(n: Int): Unit = { Game.repeat(n)(Player move Down) }

  /** Moves the player according to a string
    *
    * @param moves
    *   String of w, a, s, and d characters
    */
  def move(moves: String): Unit = { Player move moves }

  /** Collects coins on field */
  def checkCoin(): Unit = { Player.collectCoins }

  /** Collects coins on field */
  def checkCoins(): Unit = { Player.collectCoins }

  /** Returns the maximum score for the game */
  def maxScore(): Int = Player.score + Field.remainingScore

  /** Returns a string of moves that move player to the given position */
  def suggestMove(x: Int, y: Int): String = {
    Field getPath (Player.position, Vector(x, y))
  }

  /** Returns a string of moves that will collect all coins */
  def suggestSolution(): String = Field.getSolution

  /** Updates save position to the current player position */
  def save(): Unit = { Player.save = Player.position }

  /** Returns the save position as a tuple, in (x,y) order */
  def getSavePos(): Game.VectorTuple = Player.save.toTuple

  /** Sets the savePos to the given position
    *
    * @param position
    *   New save position
    */
  def setSavePos(position: Game.VectorTuple): Unit = {
    Player.save = Vector(position)
  }
}

/** Companion object for Game */
object Game {

  /** Returns game from given game data */
  def apply(data: GameBuilder.GameData) = new Game(data)

  /** Defines an (x, y) tuple representing a vector */
  type VectorTuple = (Int, Int)

  /** Defines an (x, y, value) tuple representing a coin */
  type CoinTuple = (Int, Int, Int)

  /** Repeatedly run a sequence of commands
    *
    * @param n
    *   Number of repetitions
    * @param c
    *   Commands to be executed
    */
  private def repeat(n: Int)(c: => Unit): Unit = { for (i <- 1 to n) { c } }
}

/** Builds and returns a standard instance of Game */
object GameBuilder {

  /** Defines the data required to start a game */
  trait GameData {
    val Walls: List[Game.VectorTuple]
    val Coins: List[Game.CoinTuple]
    val PlayerPosition: Game.VectorTuple
  }

  /** First test game object */
  private case object Game1 extends GameData {
    val Walls = List((3, 0), (3, 1), (3, 2))
    val Coins = List((4, 1, 100), (3, 3, 250))
    val PlayerPosition = (0, 0)
  }

  /** Second test game object */
  private case object Game2 extends GameData {
    val Walls = List((3, 3), (3, 4), (3, 5), (5, 3), (5, 4), (5, 5))
    val Coins = List((4, 4, 200), (6, 3, 200))
    val PlayerPosition = (3, 2)
  }

  /** Third test game object */
  private case object Game3 extends GameData {
    val Walls = List((3, 0), (3, 1), (3, 2))
    val Coins = List((4, 1, 300), (3, 3, 150))
    val PlayerPosition = (4, 1)
  }

  /** Returns first test game */
  def initialiseGame1(): Game = Game(Game1)

  /** Returns second test game */
  def initialiseGame2(): Game = Game(Game2)

  /** Returns third test game */
  def initialiseGame3(): Game = Game(Game3)
}
