package ex4

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ex4.ConnectThree.*
import ex4.ConnectThree.Player.*

class ConnectThreeSpec extends AnyFlatSpec with Matchers:


  "Method find (EX1)" should "work correctly" in:
    val boardToTest: Board = List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X), Disk(0, 3, X))
    find(boardToTest, 0, 0 ) should be (Some(X))
    find(boardToTest, 0, 1 ) should be (Some(O))
    find(boardToTest, 1, 1 ) should be (None)

  "Method firstAvailableRow (EX2)" should "work correctly" in:
    firstAvailableRow(emptyBoard, 0) should be (Some(0))
    firstAvailableRow(List(Disk(0, 0, X)), 0) should be (Some(1))
    firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0) should be (Some(2))
    firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0) should be (Some(3))
    firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0) should be (None)
    firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 1) should be (Some(0))

  "Method placeAnyDisk (EX3)" should "work correctly" in:
    val game1: Game = List(
      List(Disk(0, 0, X)),
      List(Disk(1, 0, X)),
      List(Disk(2, 0, X)),
      List(Disk(3, 0, X))
    )
    for
      board <- placeAnyDisk(emptyBoard, X)
    yield game1.contains(board) should be (true)

    val game2: Game = List(
      List(Disk(3, 0, O), Disk(0, 0, X)),
      List(Disk(3, 0, O), Disk(1, 0, X)),
      List(Disk(3, 0, O), Disk(2, 0, X)),
      List(Disk(3, 0, O), Disk(3, 1, X))
    )
    for
      board <- placeAnyDisk(List(Disk(3, 0, O)), X)
    yield game2.contains(board) should be (true)

  "Method computeAnyGame (EX4)" should "work correctly" in:
    val firstPlayer = X
    for
      moves <- 4 to 5
      game <- computeAnyGame(firstPlayer, moves)
    yield {
      game.last.size should be (moves)
      game.last.count(d => d.player == firstPlayer) should be (if moves % 2 == 0 then moves / 2 else moves / 2 + 1)
      for
        board <- game
        disk <- board
      yield
        (disk.y == 0 || find(board, disk.x, disk.y - 1).isDefined) should be (true)
    }

  "Method placeDisk" should "place disk correctly" in:
    var board = placeDisk(emptyBoard, X, 2)
    board.size == 1 && board.contains(Disk(2, 0, X)) should be (true)
    board = placeDisk(board, O, 2)
    board.size == 2 && board.contains(Disk(2, 1, O)) should be (true)

  it should "stop placing disk" in:
    val board = placeDisk(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), O, 0)
    board.equals(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X))) should be (true)

  val boardXWin00H: Board = List(Disk(0, 0, X), Disk(1, 0, X), Disk(2, 0, X))
  val boardOWin11H: Board = List(Disk(1, 1, O), Disk(2, 1, O), Disk(3, 1, O))
  val boardXWinUpperRightH: Board = List(Disk(1, 3, X), Disk(2, 3, X), Disk(3, 3, X))
  val interruptedStreakH: Board = List(Disk(0, 0, X), Disk(1, 0, O), Disk(2, 0, X))
  val emptySpaceInterruptedH: Board = List(Disk(0, 0, X), Disk(1, 0, X), Disk(3, 0, X))
  val twoDisksH: Board = List(Disk(0, 0, X), Disk(1, 0, X))
  val boardXWin00V: Board = List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X))
  val boardOWin11V: Board = List(Disk(1, 1, O), Disk(1, 2, O), Disk(1, 3, O))
  val boardXWinUpperRightColV: Board = List(Disk(3, 1, X), Disk(3, 2, X), Disk(3, 3, X))
  val interruptedStreakV: Board =List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X))
  val twoDisksV: Board = List(Disk(0, 0, X), Disk(0, 1, X))
  val boardXWin00D1: Board = List(Disk(0, 0, X), Disk(1, 1, X), Disk(2, 2, X))
  val boardOWin10D1: Board = List(Disk(1, 0, O), Disk(2, 1, O), Disk(3, 2, O))
  val interruptedD1: Board = List(Disk(0, 0, X), Disk(1, 1, O), Disk(2, 2, X))
  val twoDisksD1: Board = List(Disk(0, 0, X), Disk(1, 1, X))
  val boardXWin02D2: Board = List(Disk(0, 2, X), Disk(1, 1, X), Disk(2, 0, X))
  val boardOWin13D2: Board = List(Disk(1, 3, O), Disk(2, 2, O), Disk(3, 1, O))
  val interruptedD2: Board = List(Disk(0, 2, X), Disk(1, 1, O), Disk(2, 0, X))
  val twoDisksD2: Board = List(Disk(0, 2, X), Disk(1, 1, X))

  "Method someoneIsWinning" should "detect a win" in:
    val pX = X
    pX.IsWinning(boardXWin00H) should be (true)

  it should "not detect a win" in:
    val pO = O

  "Method computeAnyGameUntilSomeoneWinOrUntilNMoves" should "work correctly" in:
    for
      n <- 4 to 6
      game <- computeAnyGameUntilSomeoneWinOrUntilNMoves(X, n)
    yield {
      val size = game.last.size
      if (Player.values.exists(p => p.IsWinning(game.last)))
        size >= 5 && size <= n should be (true)
      else
        size should be (n)
      for
        b <- game.reverse.tail
      yield
        Player.values.exists(p => p.IsWinning(b)) should be (false)
    }

  "Method getMaxHorizontalStreak" should "work correctly" in:
    val boardX1: Board = List(Disk(0, 0, X))
    val boardX2: Board = List(Disk(0, 0, X), Disk(1, 0, X))
    val boardX3: Board = List(Disk(0, 0, X), Disk(1, 0, X), Disk(2, 0, X))
    val boardX1Interrupted: Board = List(Disk(0, 0, X), Disk(1, 0, O), Disk(2, 0, X))
    getMaxHorizontalStreak(boardToMap(emptyBoard), O) should be (0)
    getMaxHorizontalStreak(boardToMap(emptyBoard), X) should be (0)
    getMaxHorizontalStreak(boardToMap(boardX1), X) should be (1)
    getMaxHorizontalStreak(boardToMap(boardX2), X) should be (2)
    getMaxHorizontalStreak(boardToMap(boardX3), X) should be (3)
    getMaxHorizontalStreak(boardToMap(boardX1), O) should be (0)
    getMaxHorizontalStreak(boardToMap(boardX2), O) should be (0)
    getMaxHorizontalStreak(boardToMap(boardX1Interrupted), X) should be (1)
    getMaxHorizontalStreak(boardToMap(boardX1Interrupted), O) should be (1)

  "Method getMaxVerticalStreak" should "work correctly" in:
    val boardX1: Board = List(Disk(0, 0, X))
    val boardX2: Board = List(Disk(0, 0, X), Disk(0, 1, X))
    val boardX3: Board = List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X))
    val boardX1Interrupted: Board = List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X))
    getMaxVerticalStreak(boardToMap(emptyBoard), O) should be (0)
    getMaxVerticalStreak(boardToMap(emptyBoard), X) should be (0)
    getMaxVerticalStreak(boardToMap(boardX1), X) should be (1)
    getMaxVerticalStreak(boardToMap(boardX2), X) should be (2)
    getMaxVerticalStreak(boardToMap(boardX3), X) should be (3)
    getMaxVerticalStreak(boardToMap(boardX1), O) should be (0)
    getMaxVerticalStreak(boardToMap(boardX2), O) should be (0)
    getMaxVerticalStreak(boardToMap(boardX1Interrupted), X) should be (1)
    getMaxVerticalStreak(boardToMap(boardX1Interrupted), O) should be (1)

  "Method getMaxDiagonal1Streak" should "work correctly" in:
    val boardX1: Board = List(Disk(0, 0, X))
    val boardX2: Board = List(Disk(0, 0, X), Disk(1, 1, X))
    val boardX3: Board = List(Disk(0, 0, X), Disk(1, 1, X), Disk(2, 2, X))
    val boardX1Interrupted: Board = List(Disk(0, 0, X), Disk(1, 1, O), Disk(2, 2, X))
    getMaxDiagonal1Streak(boardToMap(emptyBoard), O) should be (0)
    getMaxDiagonal1Streak(boardToMap(emptyBoard), X) should be (0)
    getMaxDiagonal1Streak(boardToMap(boardX1), X) should be (1)
    getMaxDiagonal1Streak(boardToMap(boardX2), X) should be (2)
    getMaxDiagonal1Streak(boardToMap(boardX3), X) should be (3)
    getMaxDiagonal1Streak(boardToMap(boardX1), O) should be (0)
    getMaxDiagonal1Streak(boardToMap(boardX2), O) should be (0)
    getMaxDiagonal1Streak(boardToMap(boardX1Interrupted), X) should be (1)
    getMaxDiagonal1Streak(boardToMap(boardX1Interrupted), O) should be (1)

  "Method getMaxDiagonal2Streak" should "work correctly" in:
    val boardX1: Board = List(Disk(0, 3, X))
    val boardX2: Board = List(Disk(0, 3, X), Disk(1, 2, X))
    val boardX3: Board = List(Disk(0, 3, X), Disk(1, 2, X), Disk(2, 1, X))
    val boardX1Interrupted: Board = List(Disk(0, 3, X), Disk(1, 2, O), Disk(2, 1, X))
    getMaxDiagonal2Streak(boardToMap(emptyBoard), O) should be (0)
    getMaxDiagonal2Streak(boardToMap(emptyBoard), X) should be (0)
    getMaxDiagonal2Streak(boardToMap(boardX1), X) should be (1)
    getMaxDiagonal2Streak(boardToMap(boardX2), X) should be (2)
    getMaxDiagonal2Streak(boardToMap(boardX3), X) should be (3)
    getMaxDiagonal2Streak(boardToMap(boardX1), O) should be (0)
    getMaxDiagonal2Streak(boardToMap(boardX2), O) should be (0)
    getMaxDiagonal2Streak(boardToMap(boardX1Interrupted), X) should be (1)
    getMaxDiagonal2Streak(boardToMap(boardX1Interrupted), O) should be (1)




