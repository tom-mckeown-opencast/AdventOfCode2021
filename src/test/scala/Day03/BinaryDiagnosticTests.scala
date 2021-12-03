package Day03

import AoC.Utils
import org.scalatest.funspec.AnyFunSpec

class BinaryDiagnosticTests extends AnyFunSpec {
  val exampleData = Array("00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010")
  val taskData = Utils.readFile("src/test/scala/Day03/input.txt")

  describe("Calculate power consumption tests") {
    describe("Gamma rate of example data") {
      it("Should return 22") {
        assert(BinaryDiagnostic.calcGammaRate(exampleData) == 22)
      }
    }
    describe("Epsilon rate of example data") {
      it("Should return 9") {
        assert(BinaryDiagnostic.calcEpsilonRate(exampleData) == 9)
      }
    }
    describe("Power consumption of example data") {
      it("Should return 198") {
        assert(BinaryDiagnostic.calcPowerConsumption(exampleData) == 198)
      }
    }

    describe("Gamma rate of task data") {
      it("Should return 3797") {
        assert(BinaryDiagnostic.calcGammaRate(taskData) == 3797)
      }
    }
    describe("Epsilon rate of task data") {
      it("Should return 298") {
        assert(BinaryDiagnostic.calcEpsilonRate(taskData) == 298)
      }
    }
    describe("Power consumption of task data") {
      it("Should return 1131506") {
        assert(BinaryDiagnostic.calcPowerConsumption(taskData) == 1131506)
      }
    }
  }
  describe("Calculate life support rating tests") {
    describe("Oxygen generator rating of example data") {
      it("Should return 23") {
        assert(BinaryDiagnostic.calcOxygenGeneratorRating(exampleData) == 23)
      }
    }
    describe("CO2 scrubber rating of example data") {
      it("Should return 10") {
        assert(BinaryDiagnostic.calcCO2ScrubberRating(exampleData) == 10)
      }
    }
    describe("Life support rating of example data") {
      it("Should return 230") {
        assert(BinaryDiagnostic.calcLifeSupportRating(exampleData) == 230)
      }
    }

    describe("Oxygen generator rating of task data") {
      it("Should return 4089") {
        assert(BinaryDiagnostic.calcOxygenGeneratorRating(taskData) == 4089)
      }
    }
    describe("CO2 scrubber rating of task data") {
      it("Should return 1923") {
        assert(BinaryDiagnostic.calcCO2ScrubberRating(taskData) == 1923)
      }
    }
    describe("Life support rating of task data") {
      it("Should return 7863147") {
        assert(BinaryDiagnostic.calcLifeSupportRating(taskData) == 7863147)
      }
    }
  }
}
