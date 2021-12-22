package Day11

import AoC.Utils
import org.scalatest.funspec.AnyFunSpec

class DumboOctopusTests extends AnyFunSpec {
  val exampleData = Array(
    "5483143223",
    "2745854711",
    "5264556173",
    "6141336146",
    "6357385478",
    "4167524645",
    "2176841721",
    "6882881134",
    "4846848554",
    "5283751526"
  )
  val taskData = Utils.readFile("src/test/scala/Day11/input.txt")

  describe("Step-through model tests") {
    describe("Step causes a single flash") {
      it("Should return 1") {
        val model = Array(
          "000",
          "090",
          "000"
        )
        assert(DumboOctopus.getFlashCount(model, 1) == 1)
      }
    }
    describe("Step causes a neighbour to flash") {
      it("Should return 2") {
        val model = Array(
          "080",
          "090",
          "000"
        )
        assert(DumboOctopus.getFlashCount(model, 1) == 2)
      }
    }
    describe("Step causes a chain of flashes") {
      it("Should return 3") {
        val model = Array(
          "080",
          "080",
          "090"
        )
        assert(DumboOctopus.getFlashCount(model, 1) == 3)
      }
    }

    describe("After 1 step on exampleData") {
      it("Should return 0") {
        assert(DumboOctopus.getFlashCount(exampleData, 1) == 0)
      }
    }
    describe("After 2 step on exampleData") {
      it("Should return 35") {
        assert(DumboOctopus.getFlashCount(exampleData, 2) == 35)
      }
    }
    describe("After 3 step on exampleData") {
      it("Should return 80") {
        assert(DumboOctopus.getFlashCount(exampleData, 3) == 80)
      }
    }
    describe("After 4 step on exampleData") {
      it("Should return 96") {
        assert(DumboOctopus.getFlashCount(exampleData, 4) == 96)
      }
    }
    describe("After 5 step on exampleData") {
      it("Should return 104") {
        assert(DumboOctopus.getFlashCount(exampleData, 5) == 104)
      }
    }
    describe("After 6 step on exampleData") {
      it("Should return 105") {
        assert(DumboOctopus.getFlashCount(exampleData, 6) == 105)
      }
    }
    describe("After 7 step on exampleData") {
      it("Should return 112") {
        assert(DumboOctopus.getFlashCount(exampleData, 7) == 112)
      }
    }
    describe("After 8 step on exampleData") {
      it("Should return 136") {
        assert(DumboOctopus.getFlashCount(exampleData, 8) == 136)
      }
    }
    describe("After 9 step on exampleData") {
      it("Should return 175") {
        assert(DumboOctopus.getFlashCount(exampleData, 9) == 175)
      }
    }
    describe("After 10 steps on exampleData") {
      it("Should return 204") {
        assert(DumboOctopus.getFlashCount(exampleData, 10) == 204)
      }
    }
    describe("After 100 steps on exampleData") {
      it("Should return 1656") {
        assert(DumboOctopus.getFlashCount(exampleData, 100) == 1656)
      }
    }

    describe("After 100 steps on taskData") {
      it("Should return 1705") {
        assert(DumboOctopus.getFlashCount(taskData, 100) == 1705)
      }
    }
  }
  describe("Sync point tests") {
    describe("Sync point for exampleData") {
      it("Should return 195") {
        assert(DumboOctopus.getSyncStepIndex(exampleData) == 195)
      }
    }
    describe("Sync point for taskData") {
      it("Should return 265") {
        assert(DumboOctopus.getSyncStepIndex(taskData) == 265)
      }
    }
  }
}
