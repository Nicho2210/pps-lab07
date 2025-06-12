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
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(val robot: Robot, private var _battery: Int = 100) extends  Robot:
  export robot.{position, direction}
  private var _batteryLevel = _battery
  private var _batteryDecAmout = 15
  override def turn(dir: Direction): Unit =
    if _batteryLevel > _batteryDecAmout then
      _batteryLevel -= _batteryDecAmout
      robot.turn(dir)
      println(this.toString)
    else
      println(s"Not enough battery ($_batteryLevel%) to perform the action")


  override def act(): Unit =
    if _batteryLevel > _batteryDecAmout then
      _batteryLevel -= _batteryDecAmout
      robot.act()
      println(this.toString)
    else
      println(s"Not enough battery ($_batteryLevel%) to perform the action")

  override def toString: String = s"robot at $position facing $direction ($_batteryLevel%)"


@main def testRobot(): Unit =
  val robot = LoggingRobot(SimpleRobot((0, 0), Direction.North))
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East

  val robotWithBattery = RobotWithBattery(SimpleRobot((0, 0), Direction.North))
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
  robotWithBattery.act()
  robotWithBattery.act()

