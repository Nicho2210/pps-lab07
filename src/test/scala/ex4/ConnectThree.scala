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

  "Method checkHorizontal" should "detect a win" in:
    checkHorizontal(boardToMap(boardXWin00H)) should be (true)
    checkHorizontal(boardToMap(boardOWin11H)) should be (true)
    checkHorizontal(boardToMap(boardXWinUpperRightH)) should be(true)

  val interruptedStreakH: Board = List(Disk(0, 0, X), Disk(1, 0, O), Disk(2, 0, X))
  val emptySpaceInterruptedH: Board = List(Disk(0, 0, X), Disk(1, 0, X), Disk(3, 0, X))
  val twoDisksH: Board = List(Disk(0, 0, X), Disk(1, 0, X))
  it should "not detect a win" in:
    checkHorizontal(boardToMap(emptyBoard)) should be (false)
    checkHorizontal(boardToMap(interruptedStreakH)) should be (false)
    checkHorizontal(boardToMap(emptySpaceInterruptedH)) should be (false)
    checkHorizontal(boardToMap(twoDisksH)) should be (false)

  val boardXWin00V: Board = List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X))
  val boardOWin11V: Board = List(Disk(1, 1, O), Disk(1, 2, O), Disk(1, 3, O))
  val boardXWinUpperRightColV: Board = List(Disk(3, 1, X), Disk(3, 2, X), Disk(3, 3, X))
  "Method checkVertical" should "detect a win" in:
    checkVertical(boardToMap(boardXWin00V)) should be (true)
    checkVertical(boardToMap(boardOWin11V)) should be (true)
    checkVertical(boardToMap(boardXWinUpperRightColV)) should be (true)

  val interruptedStreakV: Board =List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X))
  val twoDisksV: Board = List(Disk(0, 0, X), Disk(0, 1, X))
  it should "not detect a win" in:
    checkVertical(boardToMap(emptyBoard)) should be(false)
    checkVertical(boardToMap(interruptedStreakV)) should be(false)
    checkVertical(boardToMap(twoDisksV)) should be(false)

  val boardXWin00D1: Board = List(Disk(0, 0, X), Disk(1, 1, X), Disk(2, 2, X))
  val boardOWin10D1: Board = List(Disk(1, 0, O), Disk(2, 1, O), Disk(3, 2, O))
  "Method checkDiagonal1" should "detect a win" in:
    checkDiagonal1(boardToMap(boardXWin00D1)) should be(true)
    checkDiagonal1(boardToMap(boardOWin10D1)) should be(true)

  val interruptedD1: Board = List(Disk(0, 0, X), Disk(1, 1, O), Disk(2, 2, X))
  val twoDisksD1: Board = List(Disk(0, 0, X), Disk(1, 1, X))
  it should "not detect a win" in:
    checkDiagonal1(boardToMap(emptyBoard)) should be (false)
    checkDiagonal1(boardToMap(interruptedD1)) should be(false)
    checkDiagonal1(boardToMap(twoDisksD1)) should be(false)

  val boardXWin02D2: Board = List(Disk(0, 2, X), Disk(1, 1, X), Disk(2, 0, X))
  val boardOWin13D2: Board = List(Disk(1, 3, O), Disk(2, 2, O), Disk(3, 1, O))
  "Method checkDiagonal2" should "detect a win" in:
    checkDiagonal2(boardToMap(boardXWin02D2)) should be(true)
    checkDiagonal2(boardToMap(boardOWin13D2)) should be(true)

  val interruptedD2: Board = List(Disk(0, 2, X), Disk(1, 1, O), Disk(2, 0, X))
  val twoDisksD2: Board = List(Disk(0, 2, X), Disk(1, 1, X))
  it should "not detect a win" in:
    checkDiagonal2(boardToMap(emptyBoard)) should be (false)
    checkDiagonal2(boardToMap(interruptedD2)) should be(false)
    checkDiagonal2(boardToMap(twoDisksD2)) should be(false)

  "Method someoneIsWinning" should "detect a win" in:
    someoneIsWinning(boardXWin00H) should be(true)
    someoneIsWinning(boardOWin11V) should be(true)
    someoneIsWinning(boardXWin00D1) should be(true)
    someoneIsWinning(boardOWin13D2) should be(true)

  it should "not detect a win" in:
    someoneIsWinning(emptyBoard) should be(false)
    someoneIsWinning(emptySpaceInterruptedH) should be(false)
    someoneIsWinning(interruptedStreakV) should be(false)
    someoneIsWinning(interruptedD1) should be(false)
    someoneIsWinning(twoDisksD2) should be(false)


