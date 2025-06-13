package ex4

object ConnectThree extends App:
  val bound = 3
  val streakToWin = 3
  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X

  case class Disk(x: Int, y: Int, player: Player)
  /**
   * Board:
   * y
   *
   * 3
   * 2
   * 1
   * 0
   *   0 1 2 3 <-- x
   */
  type Board = Seq[Disk]
  type Game = Seq[Board]

  def emptyBoard: Board = Seq()
  def newGame: Game = Seq(emptyBoard)
  import Player.*

  def find(board: Board, x: Int, y: Int): Option[Player] = board.find(d => (d.x == x) && (d.y == y)).map(_.player)

  def firstAvailableRow(board: Board, x: Int): Option[Int] =
    (for
      y <- 0 to bound
      if find(board, x, y).isEmpty
    yield y).headOption

  def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 to bound
      y <- firstAvailableRow(board, x)
    yield board :+ Disk(x, y, player)

  def placeDisk(board: Board, player: Player, x: Int): Board = firstAvailableRow(board, x) match {
    case Some(y) => board :+ Disk(x, y, player)
    case _ => board
  }

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match
    case 0 => LazyList(newGame)
    case _ =>
      for
        game <- computeAnyGame(player.other, moves - 1)
        new_board <- placeAnyDisk(game.last, player)
      yield
        game :+ new_board

  def boardToMap(board: Board): Map[(Int, Int), Player] = board.map(d => (d.x, d.y) -> d.player).toMap

  def someoneIsWinning(board: Board): Boolean = {
    val boardMap = boardToMap(board)
    checkHorizontal(boardMap) || checkVertical(boardMap) || checkDiagonal1(boardMap) || checkDiagonal2(boardMap)
  }

  def checkStreak(board: Map[(Int, Int), Player], xRange: Range, yRange: Range, nextPos: (Int, Int, Int) => (Int, Int)): Boolean =
    val isThereAStreak: Seq[Boolean] =
      for
        p <- Player.values
        x <- xRange
        y <- yRange
      yield
        val currentSegment: Boolean =
          (0 until streakToWin).forall(
            i =>
              val (cx, cy) = nextPos(x, y, i)
              board.get((cx, cy)).contains(p)
          )
        currentSegment
    isThereAStreak.exists(identity)

  def checkHorizontal(board: Map[(Int, Int), Player]): Boolean =
    checkStreak(
      board,
      0 to bound - streakToWin + 1,
      0 to bound,
      (x, y, i) => (x + i, y)
    )

  def checkVertical(board: Map[(Int, Int), Player]): Boolean =
    checkStreak(
      board,
      0 to bound,
      0 to bound - streakToWin + 1,
      (x, y, i) => (x, y + i)
    )

  def checkDiagonal1(board: Map[(Int, Int), Player]): Boolean =
    checkStreak(
      board,
      0 to bound - streakToWin + 1,
      0 to bound - streakToWin + 1,
      (x, y, i) => (x + i, y + i)
    )

  def checkDiagonal2(board: Map[(Int, Int), Player]): Boolean =
    checkStreak(
      board,
      0 to bound - streakToWin + 1,
      0 + streakToWin - 1 to bound,
      (x, y, i) => (x + i, y - i)
    )

  def computeAnyGameUntilSomeoneWinOrUntilNMoves(player: Player, moves: Int): LazyList[Game] = moves match
    case 0 => LazyList(newGame)
    case _ =>
      for
        game <- computeAnyGameUntilSomeoneWinOrUntilNMoves(player.other, moves - 1)
        if game.lastOption.forall(b => !someoneIsWinning(b))
        new_board <- placeAnyDisk(game.last, player)
      yield game :+ new_board


  def printBoards(game: Seq[Board]): Unit = {
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()
  }

  // Exercise 3: implement placeAnyDisk such that...
  println("EX 3:")
  printBoards(placeAnyDisk(List(), X))
  // .... .... .... ....
  // .... .... .... ....
  // .... .... .... ....
  // ...X ..X. .X.. X...
  printBoards(placeAnyDisk(List(Disk(3, 0, O)), X))
  // .... .... .... ....
  // .... .... .... ....
  // ...X .... .... ....
  // ...O ..XO .X.O X..O

  // Exercise 4 (ADVANCED!): implement computeAnyGame such that...
  println("EX 4:")
  computeAnyGame(O, 5).foreach { g =>
    if g.lastOption.forall(b => someoneIsWinning(b)) then
      printBoards(g)
      println()
  }
  //  .... .... .... .... ...O
  //  .... .... .... ...X ...X
  //  .... .... ...O ...O ...O
  //  .... ...X ...X ...X ...X
  //
  //
  // .... .... .... .... O...
  // .... .... .... X... X...
  // .... .... O... O... O...
  // .... X... X... X... X...

// Exercise 4 (VERY ADVANCED!) -- modify the above one to stop each game when someone won!!
  println("EX 5: ")
  computeAnyGameUntilSomeoneWinOrUntilNMoves(O, 6).zipWithIndex.foreach {(g, n) =>
    if g.lastOption.forall(b => someoneIsWinning(b)) then
      println(s"Game $n")
      printBoards(g)
      println()
  }
