package Day08

import AoC.Utils
import org.scalatest.funspec.AnyFunSpec

class SevenSegmentTests extends AnyFunSpec {
  val exampleData = Array(
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
    "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
    "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
    "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
    "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
    "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
    "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
    "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
    "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
    "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
  )
  val taskData = Utils.readFile("src/test/scala/Day08/input.txt")

  describe("Determine pattern tests") {
    describe("Get the answers for the first line of example data") {
      it("Should return an array containing (8,3,9,4)") {
        val line = exampleData(0)
        val expected = Array(8,3,9,4)

        assert(SevenSegments.solve(line) sameElements expected)
      }
    }
    describe("Get the answers for all of the example data") {
      it("Should return an array containing the 4-digit answers for all the lines in the array") {
        val expected: Array[String] = Array(
          "8394",
          "9781",
          "1197",
          "9361",
          "4873",
          "8418",
          "4548",
          "1625",
          "8717",
          "4315"
        )

        assert(SevenSegments.solveAll(exampleData) sameElements expected)
      }
    }

    describe("Get number of '1','4','7','8' digits in output from example data") {
      it("Should count 26 instances of '1','4','7','8'") {
        val answers = SevenSegments.solveAll(exampleData)
        val digitCount = SevenSegments.getDigitCount(answers)
        val expected = 26 // 8+6+5+7 ('1'*8 + '4'*6 + '7'*5 + '8'*7)

        assert(digitCount('1') + digitCount('4') + digitCount('7') + digitCount('8') == expected)
      }
    }
    describe("Get number of '1','4','7','8' digits in output from task data") {
      it("Should count 26 instances of '1','4','7','8'") {
        val answers = SevenSegments.solveAll(taskData)
        val digitCount = SevenSegments.getDigitCount(answers)
        val expected = 362 // 80+89+103+90 ('1'*80 + '4'*89 + '7'*103 + '8'*90)

        assert(digitCount('1') + digitCount('4') + digitCount('7') + digitCount('8') == expected)
      }
    }

    describe("Get output from example data and get the sum") {
      it("Should return 61229") {
        val answers = SevenSegments.solveAll(exampleData)
        val expected = 61229 // 8394 + 9781 + 1197 + 9361 + 4873 + 8418 + 4548 + 1625 + 8717 + 4315

        assert(SevenSegments.getAnswerSum(answers) == expected)
      }
    }
    describe("Get output from task data and get the sum") {
      it("Should return 1020159") {
        val answers = SevenSegments.solveAll(taskData)
        val expected = 1020159

        assert(SevenSegments.getAnswerSum(answers) == expected)
      }
    }
  }
}