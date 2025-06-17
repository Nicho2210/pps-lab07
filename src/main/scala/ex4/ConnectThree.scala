package ex4

import scala.util.Random
import scala.io.StdIn.*

object ConnectThree extends App:
  val bound = 3
  val streakToWin = 3
  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X

  case class Disk(x: Int, y: Int, player: Player)
  type Board = Seq[Disk]
  type Game = Seq[Board]

  def emptyBoard: Board = Seq()
  def newGame: Game = Seq(emptyBoard)
  import Player.*

  def find(board: Board, x: Int, y: Int): Option[Player] =
    board.find(d => (d.x == x) && (d.y == y)).map(_.player)

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

  def placeDisk(board: Board, player: Player, x: Int): Board =
    firstAvailableRow(board, x) match
      case Some(y) => board :+ Disk(x, y, player)
      case _ => board

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match
    case 0 => LazyList(newGame)
    case _ =>
      for
        game <- computeAnyGame(player.other, moves - 1)
        new_board <- placeAnyDisk(game.last, player)
      yield
        game :+ new_board

  def boardToMap(board: Board): Map[(Int, Int), Player] =
    board.map(d => (d.x, d.y) -> d.player).toMap

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
    checkStreak(board, 0 to bound - streakToWin + 1, 0 to bound, (x, y, i) => (x + i, y))

  def checkVertical(board: Map[(Int, Int), Player]): Boolean =
    checkStreak(board, 0 to bound, 0 to bound - streakToWin + 1, (x, y, i) => (x, y + i))

  def checkDiagonal1(board: Map[(Int, Int), Player]): Boolean =
    checkStreak(board, 0 to bound - streakToWin + 1, 0 to bound - streakToWin + 1, (x, y, i) => (x + i, y + i))

  def checkDiagonal2(board: Map[(Int, Int), Player]): Boolean =
    checkStreak(board, 0 to bound - streakToWin + 1, 0 + streakToWin - 1 to bound, (x, y, i) => (x + i, y - i))

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
    for
      n <- 1 to game.size
      x <- 0 to bound
    do {
      print(s"$x")
      if x == bound then print(" ")
    }
  }

  def getMaxStreak(boardMap: Map[(Int, Int), Player], p: Player, xRange: Range, yRange: Range, nextPos: (Int, Int, Int) => (Int, Int)): Int =
    var maxFoundStreak = 0
    for
      x <- xRange
      y <- yRange
    do
      val currentStreak = (0 until streakToWin).takeWhile{i =>
      val (cx, cy) = nextPos(x, y, i)
      boardMap.get((cx, cy)).contains(p)}.size
      maxFoundStreak = maxFoundStreak.max(currentStreak)
    maxFoundStreak

  def getMaxHorizontalStreak(boardMap: Map[(Int, Int), Player], p: Player): Int =
    getMaxStreak(boardMap, p, 0 to bound - streakToWin + 1, 0 to bound, (x, y, i) => (x + i, y))

  def getMaxVerticalStreak(boardMap: Map[(Int, Int), Player], p: Player): Int =
    getMaxStreak(boardMap, p, 0 to bound, 0 to bound - streakToWin + 1, (x, y, i) => (x, y + i))

  def getMaxDiagonal1Streak(boardMap: Map[(Int, Int), Player], p: Player): Int =
    getMaxStreak(boardMap, p, 0 to bound - streakToWin + 1, 0 to bound - streakToWin + 1, (x, y, i) => (x, y + i))

  def getMaxDiagonal2Streak(boardMap: Map[(Int, Int), Player], p: Player): Int =
    getMaxStreak(boardMap, p, 0 to bound - streakToWin + 1, 0 + streakToWin - 1 to bound, (x, y, i) => (x + i, y - i))

  trait AI:
    def player: Player
    def placeNextDisk(game: Game): Game

  object AI:

    def randomAI(player: Player): AI = randomAIImpl(player)

    def smartAI(player: Player): AI = smartAIImpl(player)

    private case class smartAIImpl(player: ConnectThree.Player) extends AI:
      assert(Player.values.contains(player))

      override def placeNextDisk(game: Game): Game = ???

    private case class randomAIImpl(override val player: Player) extends AI:
      assert(Player.values.contains(player))

      override def placeNextDisk(game: Game): Game =
        val currentBoard = game.lastOption.getOrElse(emptyBoard)
        val availableCol: Seq[Int] = (0 to bound).filter(x => firstAvailableRow(currentBoard, x).isDefined)
        if availableCol.nonEmpty then
          val chosenX = availableCol(Random.nextInt(availableCol.size))
          val newBoard = placeDisk(currentBoard, player, chosenX)
          game :+ newBoard
        else
          game

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

//  println("EX 6 (random AI): ")
//  var gameVSAI: Game = Seq(emptyBoard)
//  var player: Option[Player] = None
//  while player.isEmpty do
//    println("Digitare il player (X or O):")
//    readLine() match
//      case "X" => player = Option(X)
//      case "O" => player = Option(O)
//      case _ => println("Valore non valido: digitare 'X' or 'O'")
//  var currentPlayer: Option[Int] = None
//  val randomAI = AI.randomAI(player.get.other)
//  while currentPlayer.isEmpty do
//    println("Vuoi essere il primo giocatore? (Y/N)")
//    readLine() match
//      case "Y" => currentPlayer = Option(0)
//      case "N" => currentPlayer = Option(1)
//      case _ => println("Valore non valido: digitare 'Y' or 'N'")
//  val cols = (0 to bound).toList
//  while !someoneIsWinning(gameVSAI.last) &&  gameVSAI.last.size < (bound + 1) * (bound + 1) do {
//    val currentDisks: Int = gameVSAI.last.size
//      println("Current Board:")
//      printBoards(Seq(gameVSAI.last))
//      println("")
//      currentPlayer.get % 2 match {
//        case 0 =>
//          var validMoveMade = false
//          while !validMoveMade do
//            println("Seleziona colonna:")
//            val selectedCol = readLine().toIntOption
//            selectedCol match {
//              case Some(col) if col >= 0 && col <= bound =>
//                val nextBoard = placeDisk(gameVSAI.last, player.get, col)
//                if gameVSAI.last != nextBoard then
//                  gameVSAI = gameVSAI :+ nextBoard
//                  validMoveMade = true
//                else
//                  println("Selezione non valida")
//              case _ => println(s"Input non valido. Digitare un numero tra 0 e $bound")
//            }
//        case 1 =>
//          gameVSAI = randomAI.placeNextDisk(gameVSAI)
//      }
//    currentPlayer = Option((currentPlayer.get + 1) % 2)
//  }
//  if gameVSAI.last.size == (bound + 1) * (bound + 1) then
//    println("It-s a draw")
//  else
//    val winner = if ((currentPlayer.get + 1) % 2) == 0 then player.get else player.get.other
//    println(s"$winner is the winner")
//  println("Final Board:")
//  printBoards(Seq(gameVSAI.last))