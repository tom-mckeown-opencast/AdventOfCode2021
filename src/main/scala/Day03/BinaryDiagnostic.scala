package Day03

object BinaryDiagnostic {
  def calcGammaRate(data: Array[String]): Int = {
    def calcGammaRateBinary(data: Array[String], position: Int = 0, acc: String = ""): String = {
      if (position >= data(0).length) {
        return acc
      }

      def mcb(data: Array[String], position: Int, index: Int = 0, acc: Int = 0): Int = {
        if (index >= data.length) {
          return if (acc > 0) 1 else 0
        }

        var newAcc = acc
        data(index)(position) match {
          case '0' => newAcc -= 1
          case '1' => newAcc += 1
        }

        mcb(data, position, index + 1, newAcc)
      }

      calcGammaRateBinary(data, position + 1, acc + mcb(data, position))
    }

    Integer.parseInt(calcGammaRateBinary(data), 2)
  }
  def calcEpsilonRate(data: Array[String]): Int = {
    def calcEpsilonRateBinary(data: Array[String], position: Int = 0, acc: String = ""): String = {
      if (position >= data(0).length) {
        return acc
      }

      def lcb(data: Array[String], position: Int, index: Int = 0, acc: Int = 0): Int = {
        if (index >= data.length) {
          return if (acc > 0) 0 else 1
        }

        var newAcc = acc
        data(index)(position) match {
          case '0' => newAcc -= 1
          case '1' => newAcc += 1
        }

        lcb(data, position, index + 1, newAcc)
      }

      calcEpsilonRateBinary(data, position + 1, acc + lcb(data, position))
    }

    Integer.parseInt(calcEpsilonRateBinary(data), 2)
  }
  def calcPowerConsumption(data: Array[String]): Int = {
    calcGammaRate(data) * calcEpsilonRate(data)
  }

  def calcOxygenGeneratorRating(data: Array[String]): Int = {
    def bitCriteria(data: Array[String], position: Int, index: Int = 0, acc: Int = 0): Char = {
      if (index >= data.length) {
        // If 0 and 1 are equally common then bitCriteria == 1
        return if (acc >= 0) '1' else '0'
      }

      var newAcc = acc
      data(index)(position) match {
        case '0' => newAcc -= 1
        case '1' => newAcc += 1
      }

      bitCriteria(data, position, index + 1, newAcc)
    }
    def calcOxygenGeneratorRatingRecursive(data: Array[String], position: Int = 0): String = {
      if (data.length == 1) {
        return data(0)
      }

      calcOxygenGeneratorRatingRecursive(data.filter(_(position) == bitCriteria(data, position)), position + 1)
    }

    Integer.parseInt(calcOxygenGeneratorRatingRecursive(data), 2)
  }
  def calcCO2ScrubberRating(data: Array[String]): Int = {
    def bitCriteria(data: Array[String], position: Int, index: Int = 0, acc: Int = 0): Char = {
      if (index >= data.length) {
        // If 0 and 1 are equally common then bitCriteria == 0
        return if (acc >= 0) '0' else '1'
      }

      var newAcc = acc
      data(index)(position) match {
        case '0' => newAcc -= 1
        case '1' => newAcc += 1
      }

      bitCriteria(data, position, index + 1, newAcc)
    }
    def calcCO2ScrubberRatingRecursive(data: Array[String], position: Int = 0): String = {
      if (data.length == 1) {
        return data(0)
      }

      calcCO2ScrubberRatingRecursive(data.filter(_(position) == bitCriteria(data, position)), position + 1)
    }

    Integer.parseInt(calcCO2ScrubberRatingRecursive(data), 2)
  }
  def calcLifeSupportRating(data: Array[String]): Int = {
    calcOxygenGeneratorRating(data) * calcCO2ScrubberRating(data)
  }
}
