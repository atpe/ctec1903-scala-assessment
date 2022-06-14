package cw

import org.junit.Test
import org.junit.Assert._

class GameTest {

  @Test def test_00_mr {
    var game: Game = GameBuilder.initialiseGame1()
    game.moveRight()
    assertEquals((1, 0), game.getPlayerPos())
  }

  @Test def test_01_mr {
    var game: Game = GameBuilder.initialiseGame2()
    game.moveRight()
    assertEquals((4, 2), game.getPlayerPos())
  }

  @Test def test_02_mr {
    var game: Game = GameBuilder.initialiseGame3()
    game.moveRight()
    assertEquals((5, 1), game.getPlayerPos())
  }

  @Test def test_03_ml {
    var game: Game = GameBuilder.initialiseGame1()
    game.moveLeft()
    assertEquals((0, 0), game.getPlayerPos())
  }

  @Test def test_04_ml {
    var game: Game = GameBuilder.initialiseGame2()
    game.moveLeft()
    assertEquals((2, 2), game.getPlayerPos())
  }

  @Test def test_05_ml {
    var game: Game = GameBuilder.initialiseGame3()
    game.moveLeft()
    assertEquals((4, 1), game.getPlayerPos())
  }

  @Test def test_06_mr {
    var game: Game = GameBuilder.initialiseGame2()
    game.moveRight()
    game.moveRight()
    assertEquals((5, 2), game.getPlayerPos())
  }

  @Test def test_07_mr {
    var game: Game = GameBuilder.initialiseGame1()
    game.moveRight(3)
    assertEquals((2, 0), game.getPlayerPos())
  }

  @Test def test_08_md {
    var game: Game = GameBuilder.initialiseGame1()
    game.moveDown()
    assertEquals((0, 1), game.getPlayerPos())
  }

  @Test def test_09_md {
    var game: Game = GameBuilder.initialiseGame2()
    game.moveDown()
    assertEquals((3, 2), game.getPlayerPos())
  }

  @Test def test_10_md {
    var game: Game = GameBuilder.initialiseGame3()
    game.moveDown()
    assertEquals((4, 2), game.getPlayerPos())
  }

  @Test def test_11_mu {
    var game: Game = GameBuilder.initialiseGame1()
    game.moveUp()
    assertEquals((0, 0), game.getPlayerPos())
  }

  @Test def test_12_mu {
    var game: Game = GameBuilder.initialiseGame2()
    game.moveUp()
    assertEquals((3, 1), game.getPlayerPos())
  }

  @Test def test_13_md {
    var game: Game = GameBuilder.initialiseGame1()
    game.moveDown()
    game.moveDown()
    assertEquals((0, 2), game.getPlayerPos())
  }

  @Test def test_14_md {
    var game: Game = GameBuilder.initialiseGame1()
    game.moveDown(5)
    assertEquals((0, 5), game.getPlayerPos())
  }

  @Test def test_15_md {
    var game: Game = GameBuilder.initialiseGame1()
    game.moveDown(13)
    assertEquals((0, 9), game.getPlayerPos())
  }

  @Test def test_16_md {
    var game: Game = GameBuilder.initialiseGame2()
    game.moveDown(2)
    assertEquals((3, 2), game.getPlayerPos())
  }

  @Test def test_17_md {
    var game: Game = GameBuilder.initialiseGame1()
    game.moveDown(-2)
    assertEquals((0, 0), game.getPlayerPos())
  }

  @Test def test_18_mdmr {
    var game: Game = GameBuilder.initialiseGame1()
    game.moveDown(3)
    game.moveRight(3)
    assertEquals((3, 3), game.getPlayerPos())
    assertEquals(250, game.getScore())
  }

  @Test def test_19_playeratcoin {
    var game: Game = GameBuilder.initialiseGame3()
    assertEquals(0, game.getScore())
    game.checkCoin()
    assertEquals(300, game.getScore())
  }

  // The tests beyond this point are generally more difficult to pass based on the methods they are assessing,
  // therefore you may wish to focus on passing the tests above first.

  @Test def test_20_mdmrsave {
    var game: Game = GameBuilder.initialiseGame2()
    game.save()
    game.moveRight(5)
    game.moveDown()
    assertEquals((8, 3), game.getPlayerPos())
    assertEquals(200, game.getScore())
  }

  @Test def test_21_mdmrsave {
    var game: Game = GameBuilder.initialiseGame2()
    game.save()
    game.moveRight(3)
    game.moveDown()
    assertEquals(200, game.getScore())
    assertEquals((3, 2), game.getSavePos())
    game.moveRight()
    assertEquals((7, 3), game.getPlayerPos())
    assertEquals(200, game.getScore())
    assertEquals((-1, -1), game.getSavePos())
  }

  @Test def test_22_save {
    var game: Game = GameBuilder.initialiseGame2()
    game.save()
    game.moveRight(3)
    game.moveDown(2)
    assertEquals((-1, -1), game.getSavePos())
    assertEquals((6, 4), game.getPlayerPos())
    assertEquals(400, game.getScore())
  }

  @Test def test_23_save {
    var game: Game = GameBuilder.initialiseGame2()
    game.moveRight(1)
    game.moveDown(4)
    game.save()
    game.moveRight(2)
    game.moveUp(4)
    assertEquals((-1, -1), game.getSavePos())
    assertEquals((6, 2), game.getPlayerPos())
    assertEquals(400, game.getScore())
  }

  @Test def test_24_save {
    var game: Game = GameBuilder.initialiseGame1()
    game.moveRight(2)
    game.moveDown(3)
    game.save()
    game.moveDown()
    game.moveRight()
    game.save()
    assertEquals((3, 4), game.getSavePos())
    assertEquals((3, 4), game.getPlayerPos())
    assertEquals(0, game.getScore())
  }

  @Test def test_25_move {
    var game: Game = GameBuilder.initialiseGame1()
    game.move("ddww")
    assertEquals((2, 0), game.getPlayerPos())
  }

  @Test def test_26_move {
    var game: Game = GameBuilder.initialiseGame1()
    game.move("ddswwd")
    assertEquals((2, 0), game.getPlayerPos())
  }

  @Test def test_27_move {
    var game: Game = GameBuilder.initialiseGame1()
    game.move("ddswwdaa")
    assertEquals((0, 0), game.getPlayerPos())
  }

  @Test def test_28_move {
    var game: Game = GameBuilder.initialiseGame1()
    game.move("dswwd")
    assertEquals((2, 0), game.getPlayerPos())
  }

  @Test def test_29_move {
    var game: Game = GameBuilder.initialiseGame1()
    game.move("ddddwws")
    assertEquals((2, 1), game.getPlayerPos())
  }

  @Test def test_30_move {
    var game: Game = GameBuilder.initialiseGame2()
    game.move("dss")
    assertEquals((4, 4), game.getPlayerPos())
    assertEquals(200, game.getScore())
  }

  @Test def test_31_move {
    var game: Game = GameBuilder.initialiseGame1()
    game.moveDown(2)
    game.moveRight(2)
    game.save()
    game.move("sssddd")
    assertEquals((5, 5), game.getPlayerPos())
    assertEquals(250, game.getScore())
  }

  @Test def test_32_move {
    var game: Game = GameBuilder.initialiseGame1()
    game.move("sssddddaaa")
    assertEquals((1, 3), game.getPlayerPos())
    assertEquals(250, game.getScore())
  }

  @Test def test_33_move {
    var game: Game = GameBuilder.initialiseGame1()
    game.move("ssssssssssss")
    assertEquals((0, 9), game.getPlayerPos())
    assertEquals(0, game.getScore())
  }

  @Test def test_34_suggestmove {
    var game: Game = GameBuilder.initialiseGame1()
    var sol = game.suggestMove(3, 3)
    assertEquals("sssddd", sol)
    assertEquals((0, 0), game.getPlayerPos())
    assertEquals(0, game.getScore())
  }

  @Test def test_35_suggestmove {
    var game: Game = GameBuilder.initialiseGame1()
    var sol = game.suggestMove(4, 2)
    assertEquals("", sol)
    assertEquals((0, 0), game.getPlayerPos())
    assertEquals(0, game.getScore())
  }

  @Test def test_36_suggestmove {
    var game: Game = GameBuilder.initialiseGame2()
    game.move("dss")
    assertEquals((4, 4), game.getPlayerPos())
    var sol = game.suggestMove(3, 2)
    assertEquals("wwa", sol)
    assertEquals(200, game.getScore())
  }

  @Test def test_37_suggestmove {
    var game: Game = GameBuilder.initialiseGame2()
    var sol = game.suggestMove(5, 6)
    assertEquals("", sol)
    assertEquals((3, 2), game.getPlayerPos())
    assertEquals(0, game.getScore())
  }

  @Test def test_38_suggestmove {
    var game: Game = GameBuilder.initialiseGame1()
    var sol = game.suggestMove(10, 10)
    assertEquals("", sol)
    assertEquals((0, 0), game.getPlayerPos())
    assertEquals(0, game.getScore())
  }

  @Test def test_39_suggestmove {
    var game: Game = GameBuilder.initialiseGame1()
    game.move("sssddd")
    var sol = game.suggestMove(4, 1)
    assertEquals("dww", sol)
    assertEquals((3, 3), game.getPlayerPos())
    assertEquals(250, game.getScore())
  }

  @Test def test_40_suggestmove {
    var game: Game = GameBuilder.initialiseGame1()
    game.moveDown(5)
    assertTrue(
      game.suggestMove(4, 1).equals("ddddwwww") || game
        .suggestMove(4, 1)
        .equals("wwwwdddd")
    )
    assertEquals(0, game.getScore())
  }

  @Test def test_41_maxscore {
    var game: Game = GameBuilder.initialiseGame1()
    assertEquals(350, game.maxScore())
    assertEquals((0, 0), game.getPlayerPos())
    assertEquals(0, game.getScore())
  }

  @Test def test_42_maxscore {
    var game: Game = GameBuilder.initialiseGame2()
    game.move("ddds")
    assertEquals(400, game.maxScore())
    assertEquals(200, game.getScore())
  }

  @Test def test_43_maxscore {
    var game: Game = GameBuilder.initialiseGame2()
    game.move("ddds")
    assertEquals(400, game.maxScore())
    assertEquals(200, game.getScore())
    game.move("waass")
    assertEquals(400, game.maxScore())
    assertEquals(400, game.getScore())
  }

  @Test def test_44_maxscore {
    var game: Game = GameBuilder.initialiseGame2()
    game.move("dss")
    assertEquals(400, game.maxScore())
    assertEquals(200, game.getScore())
    game.move("wwdds")
    assertEquals(400, game.maxScore())
    assertEquals(400, game.getScore())
  }

  @Test def test_45_suggestsolution {
    var game: Game = GameBuilder.initialiseGame2()
    game.move("ddds")
    assertEquals("", game.suggestSolution())
  }

  @Test def test_46_suggestsolution {
    var game: Game = GameBuilder.initialiseGame1()
    assertEquals("sssddddww", game.suggestSolution())
  }

  @Test def test_47_suggestsolution {
    var game: Game = GameBuilder.initialiseGame1()
    game.moveDown(5)
    assertTrue(
      game.suggestSolution().equals("wwddddww") || game
        .suggestSolution()
        .equals("dddwwdww")
    )
  }

  @Test def test_48_checkcoins {
    var game: Game = GameBuilder.initialiseGame1()
    game.move("ssssdddd")
    assertEquals(0, game.getScore())
    assertEquals((4, 4), game.getPlayerPos())
    game.setSavePos(0, 0)
    game.checkCoins()
    assertEquals(350, game.getScore())
    assertEquals((4, 4), game.getPlayerPos())
  }

  @Test def test_49_checkcoins {
    var game: Game = GameBuilder.initialiseGame1()
    game.save()
    game.move("sssddd")
    assertEquals((-1, -1), game.getSavePos())
    game.moveRight()
    assertEquals(250, game.getScore())
    assertEquals((4, 3), game.getPlayerPos())
  }
}
