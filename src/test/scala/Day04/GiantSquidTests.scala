package Day04

import AoC.Utils
import org.scalatest.funspec.AnyFunSpec

class GiantSquidTests extends AnyFunSpec {
  val exampleData = Array("7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1",
    "",
    "22 13 17 11  0",
    " 8  2 23  4 24",
    "21  9 14 16  7",
    " 6 10  3 18  5",
    " 1 12 20 15 19",
    "",
    " 3 15  0  2 22",
    " 9 18 13 17  5",
    "19  8  7 25 23",
    "20 11 10 24  4",
    "14 21 16 12  6",
    "",
    "14 21 17 24  4",
    "10 16 15  9 19",
    "18  8 23 26 20",
    "22 11 13  6  5",
    " 2  0 12  3  7")
  val taskData = Utils.readFile("src/test/scala/Day04/input.txt")

  val exampleBoard0 = Array[Array[Int]](
    Array(22, 13, 17, 11,  0),
    Array( 8,  2, 23,  4, 24),
    Array(21,  9, 14, 16,  7),
    Array( 6, 10,  3, 18,  5),
    Array( 1, 12, 20, 15, 19)
  )
  val exampleBoard1 = Array[Array[Int]](
    Array( 3, 15,  0,  2, 22),
    Array( 9, 18, 13, 17,  5),
    Array(19,  8,  7, 25, 23),
    Array(20, 11, 10, 24,  4),
    Array(14, 21, 16, 12,  6)
  )
  val exampleBoard2 = Array[Array[Int]](
    Array(14, 21, 17, 24,  4),
    Array(10, 16, 15,  9, 19),
    Array(18,  8, 23, 26, 20),
    Array(22, 11, 13,  6,  5),
    Array( 2,  0, 12,  3,  7)
  )

  describe("Reading bingo data tests") {
    describe("Draw order of example data") {
      it("Should return an array of integers matching the example data string") {
        assert(GiantSquid.getDrawOrder(exampleData) sameElements
          Array(7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1))
      }
    }
    describe("Bingo boards of example data") {
      it("Should return an array of bingo boards, each represented by a 2D array of integers matching the example data string") {


        //Array(board0).foreach(x => print(x.map(_.mkString(", ")).mkString(System.lineSeparator())))

        assert(Utils.deepCompare(GiantSquid.getBoards(exampleData), Array(exampleBoard0, exampleBoard1, exampleBoard2)))
      }
    }
  }
  describe("Winning board tests") {
    describe("Winning board given the drawn numbers (7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24)") {
      it("Should return 2") {
        val drawnNumbers = Array(7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24)
        assert(GiantSquid.getWinningBoard(GiantSquid.getBoards(exampleData), drawnNumbers) == 2)
      }
    }
    describe("Winning board given the drawn numbers (17, 0, 22, 13, 16, 11)") {
      it("Should return 0") {
        val drawnNumbers = Array(17, 0, 22, 13, 16, 11)
        assert(GiantSquid.getWinningBoard(GiantSquid.getBoards(exampleData), drawnNumbers) == 0)
      }
    }
    describe("Winning board given the drawn numbers (15, 18, 0, 21, 11, 8)") {
      it("Should return 1") {
        val drawnNumbers = Array(15, 18, 0, 21, 11, 8)
        assert(GiantSquid.getWinningBoard(GiantSquid.getBoards(exampleData), drawnNumbers) == 1)
      }
    }
  }
  describe("Bingo score tests") {
    describe("Playing through with example data") {
      it("Should return 4512") {
        assert(GiantSquid.getBingoScore(exampleData) == 188 * 24)
      }
    }
    describe("Playing through with task data") {
      it("Should return 46920") {
        assert(GiantSquid.getBingoScore(taskData) == 782 * 60)
      }
    }
  }
  describe("Bingo last board tests") {
    describe("Get last board score with example data") {
      it("Should return 1924") {
        assert(GiantSquid.getLastBoardScore(exampleData) == 148 * 13)
      }
    }
    describe("Get last board score with task data") {
      it("Should return 12635") {
        assert(GiantSquid.getLastBoardScore(taskData) == 12635)
      }
    }
  }
}
