package ex2

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

  override def turn(dir: Direction): Unit =
    robot.turn(dir)
    println(robot.toString)

class RobotWithBattery(val robot: Robot, var batteryLevel: Int = 100, private var turnCost: Int = 5, private var actCost: Int = 10) extends  Robot:
  export robot.{position, direction}
  if batteryLevel < 0  || batteryLevel > 100 then throw IllegalArgumentException("Battery level must be between 0 and 100")
  private def doIt(a: => Unit, cost: Int): Unit =
    if batteryLevel >= cost then {a; batteryLevel -= cost; println(this.toString)}
    else println(s"Not enough battery to perform the action. Battery level = $batteryLevel. Battery required = $cost")
  override def turn(dir: Direction): Unit = doIt(robot.turn(dir), turnCost)
  override def act(): Unit = doIt(robot.act(), actCost)
  override def toString: String = s"robot at $position facing $direction ($batteryLevel%)"

class RobotCanFail(val robot: Robot, val failureChance: Int = 50) extends Robot:
  import scala.util.Random
  export robot.{position, direction}
  if failureChance < 0 || failureChance > 100 then throw IllegalArgumentException("Failure chance must be between 0 and 100")
  private def tryIt(a: => Unit): Unit =
    if Random.nextInt(100) > failureChance then {a}
    else println("Action failed")
  override def turn(dir: Direction): Unit = tryIt(robot.turn(dir))
  override def act(): Unit = tryIt(robot.act())

class RobotRepeated(val robot: Robot) extends Robot:
  export robot.{position, direction, turn, act}
  def act(rep: Int): Unit = 1 to rep foreach(_ => act())


@main def testRobot(): Unit =
  val robot = LoggingRobot(SimpleRobot((0, 0), Direction.North))
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East

  val robotWithBattery = RobotWithBattery(SimpleRobot((0, 0), Direction.North), batteryLevel = 100)
  robotWithBattery.act()
  robotWithBattery.act()
  robotWithBattery.act()
  robotWithBattery.act()
  robotWithBattery.turn(robotWithBattery.direction.turnRight)
  robotWithBattery.act()
  robotWithBattery.act()
  robotWithBattery.act()
  robotWithBattery.act()
  robotWithBattery.act()
  robotWithBattery.act()

  val robotCanFail = RobotCanFail(LoggingRobot(SimpleRobot((0, 0), Direction.North)), failureChance = 50)
  robotCanFail.act()
  robotCanFail.act()
  robotCanFail.act()
  robotCanFail.act()
  robotCanFail.turn(robotCanFail.direction.turnRight)
  robotCanFail.act()
  robotCanFail.act()
  robotCanFail.act()
  robotCanFail.act()
  robotCanFail.act()
  robotCanFail.act()

  val repeatedRobot = RobotRepeated(LoggingRobot(SimpleRobot((0, 0), Direction.North)))
  repeatedRobot.act(5)
  repeatedRobot.turn(repeatedRobot.direction.turnRight)
  repeatedRobot.act(10)

