package Day02

import AoC.Utils
import org.scalatest.funspec.AnyFunSpec

class DiveTests extends AnyFunSpec {
  describe("Calculate position tests") {
    describe("When given the example course") {
      it("Should return 150") {
        val exampleData = Array("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")
        assert(Dive.calculatePosition(exampleData) == 15 * 10)
      }
    }
    describe("When given the task course") {
      it("Should return 1648020") {
        val taskData = Utils.readFile("src/test/scala/Day02/input.txt")
        assert(Dive.calculatePosition(taskData) == 1648020)
      }
    }
  }
  describe("Calculate position with aim tests") {
    describe("When given the example course") {
      it("Should return 900") {
        val exampleData = Array("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")
        assert(Dive.calculatePositionWithAim(exampleData) == 15 * 60)
      }
    }
    describe("When given the task course") {
      it("Should return 1759818555") {
        val taskData = Utils.readFile("src/test/scala/Day02/input.txt")
        assert(Dive.calculatePositionWithAim(taskData) == 1759818555)
      }
    }
  }
}
