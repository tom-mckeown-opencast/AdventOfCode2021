package Day04

import AoC.Utils

import scala.collection.mutable.ArrayBuffer

object GiantSquid {
  def getDrawOrder(data: Array[String]): Array[Int] = {
    data(0).split(',').map(_.toInt)
  }
  def getBoards(data: Array[String]): Array[Array[Array[Int]]] = {
    def getBoard(data: Array[String], dataIndex: Int, boardIndex: Int = 0, boardAcc: Array[Array[Int]] = Array.ofDim[Int](5, 5)): Array[Array[Int]] = {
      if (dataIndex >= data.length || data(dataIndex).isEmpty) {
        return boardAcc
      }

      boardAcc(boardIndex) = data(dataIndex).split(' ').filter(_.nonEmpty).map(_.toInt)

      getBoard(data, dataIndex + 1, boardIndex + 1, boardAcc)
    }
    def getBoardsRecursive(data: Array[String], boardIndex: Int = 0, acc: ArrayBuffer[Array[Array[Int]]] = ArrayBuffer[Array[Array[Int]]]()): Array[Array[Array[Int]]] = {
      val dataIndex = 6 * boardIndex + 2
      if (dataIndex >= data.length) {
        return acc.toArray
      }

      acc += getBoard(data, dataIndex)

      getBoardsRecursive(data, boardIndex + 1, acc)
    }

    getBoardsRecursive(data)
  }

  def getWinningBoards(boards: Array[Array[Array[Int]]], drawnNumbers: Array[Int], ignoreList: Array[Int] = Array(), index: Int = 0, acc: ArrayBuffer[Int] = ArrayBuffer()): Array[Int] = {
    if (index >= boards.length) {
      return acc.toArray
    }
    if (!ignoreList.contains(index) && checkBoard(boards(index), drawnNumbers)) {
      acc += index
    }
    getWinningBoards(boards, drawnNumbers, ignoreList, index + 1, acc)
  }
  def getWinningBoard(boards: Array[Array[Array[Int]]], drawnNumbers: Array[Int], ignoreList: Array[Int] = Array(), index: Int = 0): Int = {
    if (index >= boards.length) {
      return -1
    } else if (!ignoreList.contains(index) && checkBoard(boards(index), drawnNumbers)) {
      return index
    }

    getWinningBoard(boards, drawnNumbers, ignoreList, index + 1)
  }
  def checkBoard(board: Array[Array[Int]], drawnNumbers: Array[Int]): Boolean = {
    def getColumn(board: Array[Array[Int]], colIndex: Int, rowIndex: Int = 0, acc: ArrayBuffer[Int] = ArrayBuffer()): Array[Int] = {
      if (rowIndex >= board.length) {
        return acc.toArray
      }

      acc += board(rowIndex)(colIndex)

      getColumn(board, colIndex, rowIndex + 1, acc)
    }
    def checkRows(board: Array[Array[Int]], drawnNumbers: Array[Int], rowIndex: Int = 0): Boolean = {
      if (rowIndex >= board.length) {
        return false
      } else if (board(rowIndex).forall(x => drawnNumbers.contains(x))) {
        return true
      }

      checkRows(board, drawnNumbers, rowIndex + 1)
    }
    def checkColumns(board: Array[Array[Int]], drawnNumbers: Array[Int], colIndex: Int = 0): Boolean = {
      if (colIndex >= board.length) {
        return false
      } else if (getColumn(board, colIndex).forall(x => drawnNumbers.contains(x))) {
        return true
      }

      checkColumns(board, drawnNumbers, colIndex + 1)
    }

    checkRows(board, drawnNumbers) || checkColumns(board, drawnNumbers)
  }

  def sumUnmarkedNumbers(board: Array[Array[Int]], drawnNumbers: Array[Int], index: Int = 0, acc: Int = 0): Int = {
    if (index >= board.length) {
      return acc
    }

    sumUnmarkedNumbers(board, drawnNumbers, index + 1, acc + board(index).filter(x => !drawnNumbers.contains(x)).sum)
  }

  def getBingoScore(data: Array[String]): Int = {
    def getBingoScoreRecursive(boards: Array[Array[Array[Int]]], drawOrder: Array[Int], index: Int = 0, drawnNumbers: ArrayBuffer[Int] = ArrayBuffer()): Int = {
      if (index >= drawOrder.length) {
        return 0
      }

      drawnNumbers += drawOrder(index)
      val winningBoardIndex = getWinningBoard(boards, drawnNumbers.toArray)
      if (winningBoardIndex >= 0) {
        return sumUnmarkedNumbers(boards(winningBoardIndex), drawnNumbers.toArray) * drawnNumbers(index)
      }

      getBingoScoreRecursive(boards, drawOrder, index + 1, drawnNumbers)
    }

    getBingoScoreRecursive(getBoards(data), getDrawOrder(data))
  }

  def getLastBoardScore(data: Array[String]): Int = {
    def getLastBoardScoreRecursive(boards: Array[Array[Array[Int]]], drawOrder: Array[Int], index: Int = 0, drawnNumbers: ArrayBuffer[Int] = ArrayBuffer(), winningBoards: ArrayBuffer[Int] = ArrayBuffer()): Int = {
      if (winningBoards.length == boards.length) {
        return sumUnmarkedNumbers(boards(winningBoards.last), drawnNumbers.toArray) * drawnNumbers.last
      }

      drawnNumbers += drawOrder(index)
      getWinningBoards(boards, drawnNumbers.toArray, winningBoards.toArray).foreach(x => winningBoards += x)

      getLastBoardScoreRecursive(boards, drawOrder, index + 1, drawnNumbers, winningBoards)
    }

    getLastBoardScoreRecursive(getBoards(data), getDrawOrder(data))
  }
}
