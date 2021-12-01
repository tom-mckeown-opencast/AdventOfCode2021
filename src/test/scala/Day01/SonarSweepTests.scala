package Day01

import AoC.Utils
import org.scalatest.funspec.AnyFunSpec

class SonarSweepTests extends AnyFunSpec {
  describe("Measuring depth tests") {
    describe("When given the example depth report") {
      it("Should return 7") {
        val exampleData = Array[Int](199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
        assert(SonarSweep.measureDepth(exampleData) == 7)
      }
    }
    describe("When given the task depth report") {
      it("Should return 1754") {
        val taskData: Array[Int] = Utils.readFile("src/test/scala/Day01/input.txt").map(_.toInt)
        assert(SonarSweep.measureDepth(taskData) == 1754)
      }
    }
  }
  describe("Measuring depth with 3-measurement sliding window tests") {
    describe("When given the example depth report") {
      it("Should return 5") {
        val exampleData = Array[Int](199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
        assert(SonarSweep.measureDepthWindowed(exampleData) == 5)
      }
    }
    describe("When given the task depth report") {
      it("Should return 1789") {
        val taskData: Array[Int] = Utils.readFile("src/test/scala/Day01/input.txt").map(_.toInt)
        assert(SonarSweep.measureDepthWindowed(taskData) == 1789)
      }
    }
  }
}
