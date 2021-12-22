package Day11

import scala.collection.mutable

object DumboOctopus {
  def getFlashCount(data: Array[String], steps: Int): Int = {
    def incrementEnergyLevels(m: Array[Array[Int]], rowIndex: Int = 0, colIndex: Int = 0, flashQueue: mutable.Queue[(Int, Int)] = mutable.Queue()): mutable.Queue[(Int, Int)] = {
      if (rowIndex >= m.length) {
        return flashQueue
      }

      m(rowIndex)(colIndex) += 1
      if (m(rowIndex)(colIndex) > 9) {
        flashQueue += Tuple2(rowIndex, colIndex)
      }

      var newRowIndex = rowIndex
      var newColIndex = colIndex + 1
      if (newColIndex >= m(rowIndex).length) {
        newRowIndex += 1
        newColIndex = 0
      }
      incrementEnergyLevels(m, newRowIndex, newColIndex, flashQueue)
    }
    def flash(m: Array[Array[Int]], flashQueue: mutable.Queue[(Int, Int)], completedList: mutable.ArrayBuffer[(Int, Int)] = mutable.ArrayBuffer()): Int = {
      if (flashQueue.isEmpty) {
        return completedList.length
      }

      // Dequeue coords
      val (row, col) = flashQueue.dequeue()

      // Increment neighbours
      // - If neighbour > 9 && !flashQueue.contains(n) && !completedList.contains(n) then add to flashQueue
      // Left
      if (col > 0) {
        m(row)(col - 1) += 1
        val n = Tuple2(row, col - 1)
        if (m(row)(col - 1) > 9 && !flashQueue.contains(n) && !completedList.contains(n)) {
          flashQueue.enqueue(n)
        }
      }
      // Top-Left
      if (row > 0 && col > 0) {
        m(row - 1)(col - 1) += 1
        val n = Tuple2(row - 1, col - 1)
        if (m(row - 1)(col - 1) > 9 && !flashQueue.contains(n) && !completedList.contains(n)) {
          flashQueue.enqueue(n)
        }
      }
      // Top
      if (row > 0) {
        m(row - 1)(col) += 1
        val n = Tuple2(row - 1, col)
        if (m(row - 1)(col) > 9 && !flashQueue.contains(n) && !completedList.contains(n)) {
          flashQueue.enqueue(n)
        }
      }
      // Top-Right
      if (row > 0 && col < m(row).length - 1) {
        m(row - 1)(col + 1) += 1
        val n = Tuple2(row - 1, col + 1)
        if (m(row - 1)(col + 1) > 9 && !flashQueue.contains(n) && !completedList.contains(n)) {
          flashQueue.enqueue(n)
        }
      }
      // Right
      if (col < m(row).length - 1) {
        m(row)(col + 1) += 1
        val n = Tuple2(row, col + 1)
        if (m(row)(col + 1) > 9 && !flashQueue.contains(n) && !completedList.contains(n)) {
          flashQueue.enqueue(n)
        }
      }
      // Bottom-Right
      if (row < m.length - 1 && col < m(row).length - 1) {
        m(row + 1)(col + 1) += 1
        val n = Tuple2(row + 1, col + 1)
        if (m(row + 1)(col + 1) > 9 && !flashQueue.contains(n) && !completedList.contains(n)) {
          flashQueue.enqueue(n)
        }
      }
      // Bottom
      if (row < m.length - 1) {
        m(row + 1)(col) += 1
        val n = Tuple2(row + 1, col)
        if (m(row + 1)(col) > 9 && !flashQueue.contains(n) && !completedList.contains(n)) {
          flashQueue.enqueue(n)
        }
      }
      // Bottom-Left
      if (row < m.length - 1 && col > 0) {
        m(row + 1)(col - 1) += 1
        val n = Tuple2(row + 1, col - 1)
        if (m(row + 1)(col - 1) > 9 && !flashQueue.contains(n) && !completedList.contains(n)) {
          flashQueue.enqueue(n)
        }
      }

      // Add coords to completedList
      completedList += Tuple2(row, col)

      flash(m, flashQueue, completedList)
    }
    def resetEnergyLevels(m: Array[Array[Int]]): Array[Array[Int]] = {
      m.map(row => row.map(value => if (value > 9) 0 else value))
    }

    def simulateSteps(m: Array[Array[Int]], steps: Int, stepIndex: Int = 0, flashAcc: Int = 0): Int = {
      if (stepIndex >= steps) {
        return flashAcc
      }

      val flashQueue = incrementEnergyLevels(m)
      val numFlashes = flash(m, flashQueue)

      simulateSteps(resetEnergyLevels(m), steps, stepIndex + 1, flashAcc + numFlashes)
    }

    simulateSteps(data.map(line => line.split("").map(digit => digit.toInt)), steps)
  }
  def getSyncStepIndex(data: Array[String]): Int = {
    def incrementEnergyLevels(m: Array[Array[Int]], rowIndex: Int = 0, colIndex: Int = 0, flashQueue: mutable.Queue[(Int, Int)] = mutable.Queue()): mutable.Queue[(Int, Int)] = {
      if (rowIndex >= m.length) {
        return flashQueue
      }

      m(rowIndex)(colIndex) += 1
      if (m(rowIndex)(colIndex) > 9) {
        flashQueue += Tuple2(rowIndex, colIndex)
      }

      var newRowIndex = rowIndex
      var newColIndex = colIndex + 1
      if (newColIndex >= m(rowIndex).length) {
        newRowIndex += 1
        newColIndex = 0
      }
      incrementEnergyLevels(m, newRowIndex, newColIndex, flashQueue)
    }
    def flash(m: Array[Array[Int]], flashQueue: mutable.Queue[(Int, Int)], completedList: mutable.ArrayBuffer[(Int, Int)] = mutable.ArrayBuffer()): Int = {
      if (flashQueue.isEmpty) {
        return completedList.length
      }

      // Dequeue coords
      val (row, col) = flashQueue.dequeue()

      // Increment neighbours
      // - If neighbour > 9 && !flashQueue.contains(n) && !completedList.contains(n) then add to flashQueue
      // Left
      if (col > 0) {
        m(row)(col - 1) += 1
        val n = Tuple2(row, col - 1)
        if (m(row)(col - 1) > 9 && !flashQueue.contains(n) && !completedList.contains(n)) {
          flashQueue.enqueue(n)
        }
      }
      // Top-Left
      if (row > 0 && col > 0) {
        m(row - 1)(col - 1) += 1
        val n = Tuple2(row - 1, col - 1)
        if (m(row - 1)(col - 1) > 9 && !flashQueue.contains(n) && !completedList.contains(n)) {
          flashQueue.enqueue(n)
        }
      }
      // Top
      if (row > 0) {
        m(row - 1)(col) += 1
        val n = Tuple2(row - 1, col)
        if (m(row - 1)(col) > 9 && !flashQueue.contains(n) && !completedList.contains(n)) {
          flashQueue.enqueue(n)
        }
      }
      // Top-Right
      if (row > 0 && col < m(row).length - 1) {
        m(row - 1)(col + 1) += 1
        val n = Tuple2(row - 1, col + 1)
        if (m(row - 1)(col + 1) > 9 && !flashQueue.contains(n) && !completedList.contains(n)) {
          flashQueue.enqueue(n)
        }
      }
      // Right
      if (col < m(row).length - 1) {
        m(row)(col + 1) += 1
        val n = Tuple2(row, col + 1)
        if (m(row)(col + 1) > 9 && !flashQueue.contains(n) && !completedList.contains(n)) {
          flashQueue.enqueue(n)
        }
      }
      // Bottom-Right
      if (row < m.length - 1 && col < m(row).length - 1) {
        m(row + 1)(col + 1) += 1
        val n = Tuple2(row + 1, col + 1)
        if (m(row + 1)(col + 1) > 9 && !flashQueue.contains(n) && !completedList.contains(n)) {
          flashQueue.enqueue(n)
        }
      }
      // Bottom
      if (row < m.length - 1) {
        m(row + 1)(col) += 1
        val n = Tuple2(row + 1, col)
        if (m(row + 1)(col) > 9 && !flashQueue.contains(n) && !completedList.contains(n)) {
          flashQueue.enqueue(n)
        }
      }
      // Bottom-Left
      if (row < m.length - 1 && col > 0) {
        m(row + 1)(col - 1) += 1
        val n = Tuple2(row + 1, col - 1)
        if (m(row + 1)(col - 1) > 9 && !flashQueue.contains(n) && !completedList.contains(n)) {
          flashQueue.enqueue(n)
        }
      }

      // Add coords to completedList
      completedList += Tuple2(row, col)

      flash(m, flashQueue, completedList)
    }
    def resetEnergyLevels(m: Array[Array[Int]]): Array[Array[Int]] = {
      m.map(row => row.map(value => if (value > 9) 0 else value))
    }

    def simulateUntilSynced(m: Array[Array[Int]], numItems: Int, stepIndex: Int = 0): Int = {
      val flashQueue = incrementEnergyLevels(m)
      val numFlashes = flash(m, flashQueue)

      if (numFlashes == numItems) {
        return stepIndex + 1
      }

      simulateUntilSynced(resetEnergyLevels(m), numItems, stepIndex + 1)
    }

    val m = data.map(line => line.split("").map(digit => digit.toInt))
    val numItems = m.map(row => row.length).sum
    simulateUntilSynced(m, numItems)
  }
}
