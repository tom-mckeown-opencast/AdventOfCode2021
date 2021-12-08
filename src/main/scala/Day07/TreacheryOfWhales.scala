package Day07

object TreacheryOfWhales {
  def getFuelCost(data: String, targetPosition: Int): Int = getFuelCost(data.split(",").map(_.toInt), targetPosition)
  def getFuelCost(positions: Array[Int], targetPosition: Int): Int = {
     def getFuelCostRecursive(positions: Array[Int], targetPosition: Int, index: Int = 0, acc: Int = 0): Int = {
       if (index >= positions.length) {
         return acc
       }
       getFuelCostRecursive(positions, targetPosition, index + 1, acc + Math.abs(positions(index) - targetPosition))
     }

    getFuelCostRecursive(positions, targetPosition)
  }

  def getFuelCostIncreasingRate(data: String, targetPosition: Int): Int = getFuelCostIncreasingRate(data.split(",").map(_.toInt), targetPosition)
  def getFuelCostIncreasingRate(positions: Array[Int], targetPosition: Int): Int = {
    def getFuelCostRecursive(positions: Array[Int], targetPosition: Int, index: Int = 0, acc: Int = 0): Int = {
      if (index >= positions.length) {
        return acc
      }

      // n(n+1) / 2
      val n = Math.abs(positions(index) - targetPosition)
      val cost = (n * (n+1)) / 2

      getFuelCostRecursive(positions, targetPosition, index + 1, acc + cost)
    }
    getFuelCostRecursive(positions, targetPosition)
  }

  def getMostEfficientPosition(data: String): Int = {
    val positions = data.split(",").map(_.toInt)
    val sorted = positions.sorted

    if (sorted.length % 2 == 1) {
      sorted(sorted.length / 2)
    } else {
      (sorted(sorted.length / 2) + sorted((sorted.length - 1) / 2)) / 2
    }
  }

  def getMostEfficientPositionIncreasingRate(data: String): Int = {
    def getMostEfficientPositionRecursive(positions: Array[Int], stop: Int, current: Int = 0, bestIndex: Int = 0, bestFuelCost: Int = Integer.MAX_VALUE): Int = {
      if (current > stop) {
        return bestIndex
      }

      var newBestIndex = bestIndex
      var newBestFuelCost = bestFuelCost
      val cost = getFuelCostIncreasingRate(positions, current)
      if (bestFuelCost > cost) {
        newBestIndex = current
        newBestFuelCost = cost
      }

      getMostEfficientPositionRecursive(positions, stop, current + 1, newBestIndex, newBestFuelCost)
    }

    val positions = data.split(",").map(_.toInt)
    getMostEfficientPositionRecursive(positions, positions.max)
  }
}
