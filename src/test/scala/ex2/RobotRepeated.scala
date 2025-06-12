package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotRepeatedSpec extends AnyFlatSpec with Matchers:
  "A RobotRepeated " should "turn correctly" in:
    val robot = new RobotRepeated(SimpleRobot((0, 0), Direction.North))

    robot.turn(Direction.East)
    robot.direction should be(Direction.East)

    robot.turn(Direction.South)
    robot.direction should be(Direction.South)

    robot.turn(Direction.West)
    robot.direction should be(Direction.West)

    robot.turn(Direction.North)
    robot.direction should be(Direction.North)

  it should "act correctly" in:
    val robot = new RobotRepeated(SimpleRobot((0, 0), Direction.North))

    val northTimes: Int = 100
    val eastTimes: Int = 200
    val westTimes: Int = 300
    val southTimes: Int = 400

    robot.act(northTimes)
    robot.position should be((0, northTimes))

    robot.turn(Direction.East)
    robot.act(eastTimes)
    robot.position should be((eastTimes, northTimes))

    robot.turn(Direction.South)
    robot.act(southTimes)
    robot.position should be((eastTimes, northTimes - southTimes))

    robot.turn(Direction.West)
    robot.act(westTimes)
    robot.position should be((eastTimes - westTimes, northTimes - southTimes))