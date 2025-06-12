package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotWithBatterySpec extends AnyFlatSpec with Matchers:
  "A RobotWithBattery " should "turn correctly" in:
    val robot = new RobotWithBattery(SimpleRobot((0, 0), Direction.North))

    robot.turn(Direction.East)
    robot.direction should be(Direction.East)

    robot.turn(Direction.South)
    robot.direction should be(Direction.South)

    robot.turn(Direction.West)
    robot.direction should be(Direction.West)

    robot.turn(Direction.North)
    robot.direction should be(Direction.North)

  it should "act correctly" in:
    val robot = new RobotWithBattery(SimpleRobot((0, 0), Direction.North))

    robot.act()
    robot.position should be((0, 1))

    robot.turn(Direction.East)
    robot.act()
    robot.position should be((1, 1))

    robot.turn(Direction.South)
    robot.act()
    robot.position should be((1, 0))

    robot.turn(Direction.West)
    robot.act()
    robot.position should be((0, 0))

  it should "consume battery correctly" in:
    val initBattery = 100
    val actC = 5
    val actN = 10
    val turnC = 1
    val turnN = 9

    val robot = new RobotWithBattery(SimpleRobot((0, 0), Direction.North),
      batteryLevel = initBattery,
      actCost = actC,
      turnCost = turnC)
    1 to actN foreach(_ => robot.act())
    1 to turnN foreach(_ => robot.turn(Direction.North))

    robot.batteryLevel should be (initBattery - turnN * turnC - actN * actC)

  it should "launch illegal argument exception" in:
    an[IllegalArgumentException] should be thrownBy new RobotWithBattery(SimpleRobot((0,0), Direction.North),
      batteryLevel = 101)
    an[IllegalArgumentException] should be thrownBy new RobotWithBattery(SimpleRobot((0, 0), Direction.North),
      batteryLevel = -1)