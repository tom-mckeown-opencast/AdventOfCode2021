package Day09

import AoC.Utils
import org.scalatest.funspec.AnyFunSpec

class SmokeBasinTests extends AnyFunSpec {
  val exampleData = Array(
    "2199943210",
    "3987894921",
    "9856789892",
    "8767896789",
    "9899965678"
  )
  val taskData = Utils.readFile("src/test/scala/Day09/input.txt")

  describe("Map tests") {
    describe("Get the map from example data") {
      it("Should return an 2D array of integers matching the example data") {
        val expected = Array(
          Array(2,1,9,9,9,4,3,2,1,0),
          Array(3,9,8,7,8,9,4,9,2,1),
          Array(9,8,5,6,7,8,9,8,9,2),
          Array(8,7,6,7,8,9,6,7,8,9),
          Array(9,8,9,9,9,6,5,6,7,8)
        )

        assert(Utils.deepCompare(SmokeBasin.getMap(exampleData), expected))
      }
    }
  }
  describe("Low point tests") {
    describe("Get all low points from example data") {
      it("Should have 4 low points [(0,1), (0,9), (2,2), (4,6)] { format: (row,col) }") {
        val map = SmokeBasin.getMap(exampleData)
        val expected = Array(
          (0,1),
          (0,9),
          (2,2),
          (4,6)
        )

        assert(SmokeBasin.getLowPoints(map) sameElements expected)
      }
    }

    describe("Get sum of risk levels for low points from example data") {
      it("Should return 15") {
        val map = SmokeBasin.getMap(exampleData)
        val expected = 15 // 2 + 1 + 6 + 6

        assert(SmokeBasin.getTotalRiskLevel(map) == expected)
      }
    }
    describe("Get sum of risk levels for low points from task data") {
      it("Should return 436") {
        val map = SmokeBasin.getMap(taskData)
        val expected = 436

        assert(SmokeBasin.getTotalRiskLevel(map) == expected)
      }
    }
  }
  describe("Basin tests") {
    describe("Get basins for example data") {
      it("Should return array of basins") {
        val map = SmokeBasin.getMap(exampleData)
        val expected = Array(
          Array((0,1),(0,0),(1,0)),
          Array((0,9),(0,8),(1,9),(0,7),(1,8),(2,9),(0,6),(0,5),(1,6)),
          Array((2,2),(2,1),(1,2),(2,3),(3,2),(3,1),(1,3),(2,4),(3,3),(3,0),(4,1),(1,4),(2,5),(3,4)),
          Array((4,6),(4,5),(3,6),(4,7),(3,7),(4,8),(2,7),(3,8),(4,9))
        )

        assert(Utils.deepCompare(SmokeBasin.getBasins(map), expected))
      }
    }

    describe("Get largest basins product for example data") {
      it("Should return 1134") {
        val map = SmokeBasin.getMap(exampleData)
        val expected = 1134 // 14 * 9 * 9

        assert(SmokeBasin.getLargestBasinsProduct(map) == expected)
      }
    }
    describe("Get largest basins product for task data") {
      it("Should return 1317792") {
        val map = SmokeBasin.getMap(taskData)
        val expected = 1317792

        assert(SmokeBasin.getLargestBasinsProduct(map) == expected)
      }
    }
  }
}