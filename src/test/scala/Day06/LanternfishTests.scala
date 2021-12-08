package Day06

import AoC.Utils
import org.scalatest.funspec.AnyFunSpec

class LanternfishTests extends AnyFunSpec {
  val exampleData = "3,4,3,1,2"
  val taskData = Utils.readFile("src/test/scala/Day06/input.txt")(0)

  describe("Simulation tests") {
    describe("After 1 day of simulating the example data") {
      it("Should return an array (2,3,2,0,1)") {
        val expected = Array(2,3,2,0,1)

        assert(Lanternfish.getFishSequence(exampleData, 1) sameElements expected)
      }
    }
    describe("After 2 days of simulating the example data") {
      it("Should return an array (1,2,1,6,0,8)") {
        val expected = Array(1,2,1,6,0,8)

        assert(Lanternfish.getFishSequence(exampleData, 2) sameElements expected)
      }
    }
    describe("After 3 days of simulating the example data") {
      it("Should return an array (0,1,0,5,6,7,8)") {
        val expected = Array(0,1,0,5,6,7,8)

        assert(Lanternfish.getFishSequence(exampleData, 3) sameElements expected)
      }
    }
    describe("After 18 days of simulating the example data") {
      it("Should return 26") {
        assert(Lanternfish.getFishCount(exampleData, 18) == 26)
      }
    }
    describe("After 21 days of simulating the example data") {
      it("Should return 37") {
        assert(Lanternfish.getFishCount(exampleData, 21) == 37)
      }
    }
    describe("After 80 days of simulating the example data") {
      it("Should return 5934") {
        assert(Lanternfish.getFishCount(exampleData, 80) == 5934)
      }
    }

    describe("After 80 days of simulating the task data") {
      it("Should return 363101") {
        assert(Lanternfish.getFishCount(taskData, 80) == 363101)
      }
    }
    describe("After 256 days of simulating the task data") {
      it("Should return 1644286074024") {
        assert(Lanternfish.getFishCount(taskData, 256) == 1644286074024L)
      }
    }
  }
}
