package Day05

import AoC.Utils
import org.scalatest.funspec.AnyFunSpec

class HydrothermalVentureTests extends AnyFunSpec {
  val exampleData = Array(
    "0,9 -> 5,9",
    "8,0 -> 0,8",
    "9,4 -> 3,4",
    "2,2 -> 2,1",
    "7,0 -> 7,4",
    "6,4 -> 2,0",
    "0,9 -> 2,9",
    "3,4 -> 1,4",
    "0,0 -> 8,8",
    "5,5 -> 8,2"
  )
  val taskData = Utils.readFile("src/test/scala/Day05/input.txt")

  describe("Vent input tests") {
    describe("Get axial vent line from the example string '0,9 -> 5,9'") {
      it("Should return an array of integer tuples { (0,9), (1,9), (2,9), (3,9), (4,9), (5,9) }") {
        val exampleLine = "0,9 -> 5,9"
        val expected = Array((0,9), (1,9), (2,9), (3,9), (4,9), (5,9))

        assert(HydrothermalVenture.getVentLine(exampleLine) sameElements expected)
      }
    }
    describe("Get axial vent line from the example string '9,7 -> 7,7'") {
      it("Should return an array of integer tuples { (9,7), (8,7), (7,7) } ") {
        val exampleLine = "9,7 -> 7,7"
        val expected = Array((9,7), (8,7), (7,7))

        assert(HydrothermalVenture.getVentLine(exampleLine) sameElements expected)
      }
    }
    describe("Get axial vent line from the example string '0,0 -> 0,2'") {
      it("Should return an array of integer tuples { (0,0), (0,1), (0,2) } ") {
        val exampleLine = "0,0 -> 0,2"
        val expected = Array((0,0), (0,1), (0,2))

        assert(HydrothermalVenture.getVentLine(exampleLine) sameElements expected)
      }
    }
    describe("Get axial vent line from the example string '4,7 -> 4,5'") {
      it("Should return an array of integer tuples { (4,7), (4,6), (4,5) } ") {
        val exampleLine = "4,7 -> 4,5"
        val expected = Array((4,7), (4,6), (4,5))

        assert(HydrothermalVenture.getVentLine(exampleLine) sameElements expected)
      }
    }
    describe("Get diagonal vent line from the example string '3,0 -> 0,3'") {
      it("Should return an array of integer tuples { (3,0), (2,1), (1,2), (0,3) } ") {
        val exampleLine = "3,0 -> 0,3"
        val expected = Array((3,0), (2,1), (1,2), (0,3))

        assert(HydrothermalVenture.getVentLine(exampleLine) sameElements expected)
      }
    }

    describe("Get example data filtered for only axial lines") {
      it("Should return the example data with only vent lines that are either horizontal or vertical (axial)") {
        val expected = Array(
          "0,9 -> 5,9",
          "9,4 -> 3,4",
          "2,2 -> 2,1",
          "7,0 -> 7,4",
          "0,9 -> 2,9",
          "3,4 -> 1,4"
        )

        assert(HydrothermalVenture.getAxialVentLines(exampleData) sameElements expected)
      }
    }

    describe("Get all vent lines from example data (filtered for only axial lines)") {
      it("Should return an array of arrays of integer tuples { (0,9)->(5,9), (9,4)->(3,4), (2,2)->(2,1), (7,0)->(7,4), (0,9)->(2,9), (3,4)->(1,4) }") {
        val filteredExampleData = HydrothermalVenture.getAxialVentLines(exampleData)
        val expected: Array[Array[(Int, Int)]] = Array(
          Array((0,9), (1,9), (2,9), (3,9), (4,9), (5,9)),
          Array((9,4), (8,4), (7,4), (6,4), (5,4), (4,4), (3,4)),
          Array((2,2), (2,1)),
          Array((7,0), (7,1), (7,2), (7,3), (7,4)),
          Array((0,9), (1,9), (2,9)),
          Array((3,4), (2,4), (1,4))
        )
        assert(Utils.deepCompare(HydrothermalVenture.getVentLines(filteredExampleData), expected))
      }
    }
    describe("Get all vent lines from example data") {
      it("Should return an array of integer tuples matching the example data input") {
        val expected = Array(
          Array((0,9), (1,9), (2,9), (3,9), (4,9), (5,9)),
          Array((8,0), (7,1), (6,2), (5,3), (4,4), (3,5), (2,6), (1,7), (0,8)),
          Array((9,4), (8,4), (7,4), (6,4), (5,4), (4,4), (3,4)),
          Array((2,2), (2,1)),
          Array((7,0), (7,1), (7,2), (7,3), (7,4)),
          Array((6,4), (5,3), (4,2), (3,1), (2,0)),
          Array((0,9), (1,9), (2,9)),
          Array((3,4), (2,4), (1,4)),
          Array((0,0), (1,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7), (8,8)),
          Array((5,5), (6,4), (7,3), (8,2))
        )
        assert(Utils.deepCompare(HydrothermalVenture.getVentLines(exampleData), expected))
      }
    }
  }
  describe("Diagram tests") {
    describe("Get the vent diagram values for example data (filtered for only axial lines)") {
      it("Should return an accurate diagram") {
        val filteredExampleData = HydrothermalVenture.getAxialVentLines(exampleData)
        val expected = Array(
          Array(0,0,0,0,0,0,0,1,0,0),
          Array(0,0,1,0,0,0,0,1,0,0),
          Array(0,0,1,0,0,0,0,1,0,0),
          Array(0,0,0,0,0,0,0,1,0,0),
          Array(0,1,1,2,1,1,1,2,1,1),
          Array(0,0,0,0,0,0,0,0,0,0),
          Array(0,0,0,0,0,0,0,0,0,0),
          Array(0,0,0,0,0,0,0,0,0,0),
          Array(0,0,0,0,0,0,0,0,0,0),
          Array(2,2,2,1,1,1,0,0,0,0)
        )

        assert(Utils.deepCompare(HydrothermalVenture.getVentDiagram(filteredExampleData), expected))
      }
    }
    describe("Get the vent diagram for example data (filtered for only axial lines)") {
      it("Should return an accurate diagram") {
        val filteredExampleData = HydrothermalVenture.getAxialVentLines(exampleData)
        val expected = Array(
          ".......1..",
          "..1....1..",
          "..1....1..",
          ".......1..",
          ".112111211",
          "..........",
          "..........",
          "..........",
          "..........",
          "222111...."
        )

        assert(HydrothermalVenture.getVentDiagramDisplay(filteredExampleData) sameElements expected)
      }
    }
    describe("Get the number of overlaps for example data (filtered for only axial lines)") {
      it("Should return 5") {
        val filteredExampleData = HydrothermalVenture.getAxialVentLines(exampleData)

        assert(HydrothermalVenture.getVentOverlaps(filteredExampleData) == 5)
      }
    }

    describe("Get the number of overlaps for task data (filtered for only axial lines)") {
      it("Should return 5632") {
        val filteredTaskData = HydrothermalVenture.getAxialVentLines(taskData)

        assert(HydrothermalVenture.getVentOverlaps(filteredTaskData) == 5632)
      }
    }

    describe("Get the vent diagram values for example data") {
      it("Should return an accurate diagram") {
        val expected = Array(
          Array(1,0,1,0,0,0,0,1,1,0),
          Array(0,1,1,1,0,0,0,2,0,0),
          Array(0,0,2,0,1,0,1,1,1,0),
          Array(0,0,0,1,0,2,0,2,0,0),
          Array(0,1,1,2,3,1,3,2,1,1),
          Array(0,0,0,1,0,2,0,0,0,0),
          Array(0,0,1,0,0,0,1,0,0,0),
          Array(0,1,0,0,0,0,0,1,0,0),
          Array(1,0,0,0,0,0,0,0,1,0),
          Array(2,2,2,1,1,1,0,0,0,0)
        )

        assert(Utils.deepCompare(HydrothermalVenture.getVentDiagram(exampleData), expected))
      }
    }
    describe("Get the vent diagram for example data") {
      it("Should return an accurate diagram") {
        val expected = Array(
          "1.1....11.",
          ".111...2..",
          "..2.1.111.",
          "...1.2.2..",
          ".112313211",
          "...1.2....",
          "..1...1...",
          ".1.....1..",
          "1.......1.",
          "222111...."
        )

        assert(HydrothermalVenture.getVentDiagramDisplay(exampleData) sameElements expected)
      }
    }
    describe("Get the number of overlaps for example data") {
      it("Should return 12") {
        assert(HydrothermalVenture.getVentOverlaps(exampleData) == 12)
      }
    }

    describe("Get the number of overlaps for task data") {
      it("Should return 22213") {
        assert(HydrothermalVenture.getVentOverlaps(taskData) == 22213)
      }
    }
  }
}
