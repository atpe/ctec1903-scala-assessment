package cw

import scala.io.StdIn

object GameApp {
  def main(args: Array[String]): Unit = {
    var input: String = ""
    var round: Int = 1
    var game: Game = GameBuilder.initialiseGame1()
    println(game.maxScore())
    while (!input.equals("q")) {
      println("You are in round " + round + " and your score is " + game.getScore())
      println("You could get up to " + game.maxScore() + " score");
      game.printField()
      input = StdIn.readLine("Enter next move (w,a,s,d,save):")
      if (input.equals("a"))
        game.moveLeft()
      else if (input.equals("d"))
        game.moveRight()
      else if (input.equals("w"))
        game.moveUp()
      else if (input.equals("s"))
        game.moveDown()
      else if (input.equals("save"))
        game.save()
      else if (input.equals("solution")) {
        println(game.suggestMove(4, 1))
        round -= 1
      }
      round += 1
    }
  }
}