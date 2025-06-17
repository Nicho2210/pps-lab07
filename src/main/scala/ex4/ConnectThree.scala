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

  extension (p: Player)
    def IsWinning(board: Board): Boolean = getMaxStreak(boardToMap(board), p) >= streakToWin

  def computeAnyGameUntilSomeoneWinOrUntilNMoves(player: Player, moves: Int): LazyList[Game] = moves match
    case 0 => LazyList(newGame)
    case _ =>
      for
        game <- computeAnyGameUntilSomeoneWinOrUntilNMoves(player.other, moves - 1)
        if !Player.values.exists(p => p.IsWinning(game.last))
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

  def getMaxStreak(boardMap: Map[(Int, Int), Player], p: Player): Int =
    Seq(
      getMaxHorizontalStreak(boardMap, p),
      getMaxVerticalStreak(boardMap, p),
      getMaxDiagonal1Streak(boardMap, p),
      getMaxDiagonal2Streak(boardMap, p)
    ).max

  def getMaxStreakOn(boardMap: Map[(Int, Int), Player], p: Player, xRange: Range, yRange: Range, nextPos: (Int, Int, Int) => (Int, Int)): Int =
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
    getMaxStreakOn(boardMap, p, 0 to bound - streakToWin + 1, 0 to bound, (x, y, i) => (x + i, y))

  def getMaxVerticalStreak(boardMap: Map[(Int, Int), Player], p: Player): Int =
    getMaxStreakOn(boardMap, p, 0 to bound, 0 to bound - streakToWin + 1, (x, y, i) => (x, y + i))

  def getMaxDiagonal1Streak(boardMap: Map[(Int, Int), Player], p: Player): Int =
    getMaxStreakOn(boardMap, p, 0 to bound - streakToWin + 1, 0 to bound - streakToWin + 1, (x, y, i) => (x + i, y + i))

  def getMaxDiagonal2Streak(boardMap: Map[(Int, Int), Player], p: Player): Int =
    getMaxStreakOn(boardMap, p, 0 to bound - streakToWin + 1, 0 + streakToWin - 1 to bound, (x, y, i) => (x + i, y - i))

  trait AI:
    def player: Player
    def placeNextDisk(game: Game): Game

  object AI:

    def randomAI(player: Player): AI = randomAIImpl(player)

    def smartAI(player: Player): AI = smartAIImpl(player)

    private case class smartAIImpl(player: ConnectThree.Player) extends AI:
      assert(Player.values.contains(player))

      override def placeNextDisk(game: Game): Game =
        val currentBoard = game.lastOption.getOrElse(emptyBoard)
        val availableCol: Seq[Int] = (0 to bound).filter(x => firstAvailableRow(currentBoard, x).isDefined)
        if availableCol.nonEmpty then {
          val opponent = player.other
          val possibleMoves: Map[Int, (Board, Int, Boolean)] = {for
            x <- availableCol
            yield {
              val board = placeDisk(currentBoard, player, x)
              val streak = getMaxStreak(boardToMap(currentBoard), player)
              val opponentCouldWin: Boolean = placeAnyDisk(board, opponent).exists(b => getMaxStreak(boardToMap(b), opponent) >= streakToWin)
              x -> (board, streak, opponentCouldWin)
            }}.toMap
          val winningCol: Option[Int] = possibleMoves.filter((k, v) => v._2 >= streakToWin).keys.headOption
          if winningCol.isDefined then {
            val chosenX = winningCol.get
            val newBoard = placeDisk(currentBoard, player, chosenX)
            game :+ newBoard
          } else {
            val opponentMoves: Map[Int, Int] = {for
              x <- availableCol
            yield x -> getMaxStreak(boardToMap(placeDisk(currentBoard, opponent, x)), opponent)
            }.toMap
            val blockOpponent: Option[Int] = opponentMoves.filter((k, v) => v >= streakToWin).keys.headOption
            if blockOpponent.isDefined then {
              val chosenX = blockOpponent.get
              val newBoard = placeDisk(currentBoard, player, chosenX)
              game :+ newBoard
            } else {
              val chosenX: Int = possibleMoves.toSeq.sortWith {
                case ((_, (_, s1, bool1)), (_, (_, s2, bool2))) =>
                  if !bool1 && bool2 then true
                  else if bool1 && !bool2 then false
                  else s1 > s2
              }.map((k, _) => k).head
              val newBoard = placeDisk(currentBoard, player, chosenX)
              game :+ newBoard
            }
          }
        }
        else {
          game
        }

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
  computeAnyGame(O, 5).zipWithIndex.foreach { (g, n) =>
    println(s"Game $n")
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
    if Player.values.exists(p => p.IsWinning(g.last)) then
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
//  val opponent = AI.randomAI(player.get.other)
//  while currentPlayer.isEmpty do
//    println("Vuoi essere il primo giocatore? (Y/N)")
//    readLine() match
//      case "Y" => currentPlayer = Option(0)
//      case "N" => currentPlayer = Option(1)
//      case _ => println("Valore non valido: digitare 'Y' or 'N'")
//  val cols = (0 to bound).toList
//  while !Player.values.exists(p => p.IsWinning(gameVSAI.last)) &&  gameVSAI.last.size < (bound + 1) * (bound + 1) do {
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
//          gameVSAI = opponent.placeNextDisk(gameVSAI)
//      }
//    currentPlayer = Option((currentPlayer.get +  1) % 2)
//  }
//  if gameVSAI.last.size == (bound + 1) * (bound + 1) then
//    println("It-s a draw")
//  else
//    val winner = Player.values.filter(p => p.IsWinning(gameVSAI.last)).head
//    println(s"$winner is the winner")
//  println("Final Board:")
//  printBoards(Seq(gameVSAI.last))