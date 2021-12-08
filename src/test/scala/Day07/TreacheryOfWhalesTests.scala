package Day07

import AoC.Utils
import org.scalatest.funspec.AnyFunSpec

class TreacheryOfWhalesTests extends AnyFunSpec {
  val exampleData = "16,1,2,0,4,2,7,1,2,14"
  val taskData = Utils.readFile("src/test/scala/Day07/input.txt")(0)

  describe("Standard fuel consumption calculation tests") {
    describe("Calculate the fuel required to move to position 2 given the example data") {
      it("Should return 37") {
        assert(TreacheryOfWhales.getFuelCost(exampleData, 2) == 37)
      }
    }
    describe("Find the most efficient position given the example data") {
      it("Should return 2") {
        assert(TreacheryOfWhales.getMostEfficientPosition(exampleData) == 2)
      }
    }

    describe("Find the most efficient position given the task data") {
      it("Should return 371") {
        assert(TreacheryOfWhales.getMostEfficientPosition(taskData) == 371)
      }
    }
    describe("Find the cost to move to the most efficient position given the task data") {
      it("Should return 341558") {
        assert(TreacheryOfWhales.getFuelCost(taskData, 371) == 341558)
      }
    }
  }
  describe("Increased fuel consumption calculation tests") {
    describe("Calculate the fuel required to move to position 2 given the example data") {
      it("Should return 168") {
        assert(TreacheryOfWhales.getFuelCostIncreasingRate(exampleData, 5) == 168)
      }
    }
    describe("Find the most efficient position given the example data") {
      it("Should return 5") {
        assert(TreacheryOfWhales.getMostEfficientPositionIncreasingRate(exampleData) == 5)
      }
    }

    describe("Find the most efficient position given the task data") {
      it("Should return 484") {
        assert(TreacheryOfWhales.getMostEfficientPositionIncreasingRate(taskData) == 484)
      }
    }
    describe("Find the cost to move to the most efficient position given the task data") {
      it("Should return 93214037") {
        assert(TreacheryOfWhales.getFuelCostIncreasingRate(taskData, 484) == 93214037)
      }
    }
  }
}
