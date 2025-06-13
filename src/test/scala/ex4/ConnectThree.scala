package ex4

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ex4.ConnectThree.*
import ex4.ConnectThree.Player.*

class ConnectThreeSpec extends AnyFlatSpec with Matchers:


  "Method find (EX1)" should "work correctly" in:
    val boardToTest: Board = List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2,X), Disk(0, 3, X))
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