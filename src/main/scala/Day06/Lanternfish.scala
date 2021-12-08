package Day06

import scala.collection.mutable.ArrayBuffer

object Lanternfish {
  // This function will get the sequence of fish after a number of days
  // WARNING: This function will run out of memory after a certain point
  def getFishSequence(data: String, days: Int): Array[Int] = {
    def runDay(fish: ArrayBuffer[Int], stopIndex: Int, index: Int = 0): ArrayBuffer[Int] = {
      if (index >= stopIndex) {
        return fish
      }

      fish(index) -= 1
      if (fish(index) < 0) {
        fish(index) = 6
        fish += 8
      }

      runDay(fish, stopIndex, index + 1)
    }
    def simulateRecursive(fish: ArrayBuffer[Int], days: Int, index: Int = 0): Array[Int] = {
      if (index % 7 == 0) println()
      if (index >= days) {
        return fish.toArray
      }
      simulateRecursive(runDay(fish, fish.length), days, index + 1)
    }

    simulateRecursive(data.split(',').map(_.toInt).to(ArrayBuffer), days)
  }

  // This function will calculate only the number of fish after a number of days
  def getFishCount(data: String, days: Int): Long = {
    def getWeekCount(fish: String, week: Int, index: Int = 0, acc: Array[Long] = Array.ofDim[Long](9)): Array[Long] = {
      if (index > week) {
        return acc
      }

      val newAcc = Array.ofDim[Long](9)
      if (index == 0) {
        newAcc(0) = fish.count(c => c == '0')
        newAcc(1) = fish.count(c => c == '1')
        newAcc(2) = fish.count(c => c == '2')
        newAcc(3) = fish.count(c => c == '3')
        newAcc(4) = fish.count(c => c == '4')
        newAcc(5) = fish.count(c => c == '5')
        newAcc(6) = fish.count(c => c == '6')
        newAcc(7) = fish.count(c => c == '7')
        newAcc(8) = fish.count(c => c == '8')
      } else {
        newAcc(0) = acc(0) + acc(7)
        newAcc(1) = acc(1) + acc(8)
        newAcc(2) = acc(2) + acc(0)
        newAcc(3) = acc(3) + acc(1)
        newAcc(4) = acc(4) + acc(2)
        newAcc(5) = acc(5) + acc(3)
        newAcc(6) = acc(6) + acc(4)
        newAcc(7) = acc(5)
        newAcc(8) = acc(6)
      }

      getWeekCount(fish, week, index + 1, newAcc)
    }
    def getDayCount(weekCount: Array[Long], day: Int, index: Int = 0, acc: Long): Long = {
      if (index > day) {
        return acc
      }
      getDayCount(weekCount, day, index + 1, acc + weekCount(index))
    }

    val week: Int = (days - 1) / 7
    val day: Int = (days - 1) % 7

    val weekCount = getWeekCount(data, week)
    getDayCount(weekCount, day, acc = weekCount.sum)
  }
}
