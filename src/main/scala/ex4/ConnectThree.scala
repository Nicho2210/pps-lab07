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

  def someoneIsWinning(board: Board): Boolean = {
    checkHorizontal(board)
  }

  def checkStreak(board: Board): Boolean =
    val isThereAStreak: Seq[Boolean] =
      for
        p <- Seq(X, O)
        x <- 0 to bound - streakToWin + 1
        y <- 0 to bound
        if find(board, x, y).isDefined
      yield
        val row: Seq[Boolean] =
          for
            i <- 0 until streakToWin
          yield
            find(board, x + i, y) match
              case Some(player) => player == p
              case _ => false
        row.forall(identity)
    isThereAStreak.exists(identity)


  def checkHorizontal(board: Board): Boolean = checkStreak(board)

  def checkVertical(board: Board): Boolean = checkStreak(board.map(d => Disk(d.y, d.x, d.player)))

  def computeAnyGameThatStop(player: Player, moves: Int): LazyList[Game] = moves match
    case 0 => LazyList(newGame)
    case _ =>
      for
        game <- computeAnyGameThatStop(player.other, moves - 1)
        new_board <- placeAnyDisk(game.last, player)
        //if !someoneIsWinning(new_board)
      yield game :+ new_board


  def printBoards(game: Seq[Board]): Unit = {
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      if y == bound && x == 0 && board == game.last then {
        println("Board")
      }
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()
  } // Exercise 1: implement find such that..
  println("EX 1: ")
  println(find(List(Disk(0,
    0,
    X)),
    0,
    0)) // Some(X)
  println(find(List(Disk(0,
    0,
    X),
    Disk(0,
      1,
      O),
    Disk(0,
      2,
      X)),
    0,
    1)) // Some(O)
  println(find(List(Disk(0,
    0,
    X),
    Disk(0,
      1,
      O),
    Disk(0,
      2,
      X)),
    1,
    1)) // None

  // Exercise 2: implement firstAvailableRow such that..
  println("EX 2: ")
  println(firstAvailableRow(List(),
    0)) // Some(0)
  println(firstAvailableRow(List(Disk(0,
    0,
    X)),
    0)) // Some(1)
  println(firstAvailableRow(List(Disk(0,
    0,
    X),
    Disk(0,
      1,
      X)),
    0)) // Some(2)
  println(firstAvailableRow(List(Disk(0,
    0,
    X),
    Disk(0,
      1,
      X),
    Disk(0,
      2,
      X)),
    0)) // Some(3)
  println(firstAvailableRow(List(Disk(0,
    0,
    X),
    Disk(0,
      1,
      X),
    Disk(0,
      2,
      X),
    Disk(0,
      3,
      X)),
    0)) // None
  // Exercise 3: implement placeAnyDisk such that..
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

  // Exercise 4 (ADVANCED!): implement computeAnyGame such that..
  println("EX 4:")
  computeAnyGame(O, 5).foreach { g =>
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

// Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
  println("EX 5: ")
  computeAnyGameThatStop(O, 4).foreach { g =>
    printBoards(g)
    println()
  }

  println("TEST SOMEONE WINNING")
  println(someoneIsWinning(List(Disk(1, 1, X), Disk(2, 2, X), Disk(3, 3, X))))