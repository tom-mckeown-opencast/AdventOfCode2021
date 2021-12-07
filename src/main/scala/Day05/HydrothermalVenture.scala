package Day05

import scala.collection.mutable.ArrayBuffer

object HydrothermalVenture {
  def getAxialVentLines(data: Array[String]): Array[String] = {
    data.filter(x => {
      val split = x.split(" -> ")
      val startSplit = split(0).split(',')
      val endSplit = split(1).split(',')
      val start = Tuple2[Int, Int](startSplit(0).toInt, startSplit(1).toInt)
      val end = Tuple2[Int, Int](endSplit(0).toInt, endSplit(1).toInt)

      start._1 == end._1 || start._2 == end._2
    })
  }

  def getVentLine(line: String): Array[(Int, Int)] = {
    val split = line.split(" -> ")
    val startSplit = split(0).split(',')
    val endSplit = split(1).split(',')
    val start = Tuple2[Int, Int](startSplit(0).toInt, startSplit(1).toInt)
    val end = Tuple2[Int, Int](endSplit(0).toInt, endSplit(1).toInt)

    def getVentLineHorizontal(start: (Int, Int), end: (Int, Int), x: Int = start._1, acc: ArrayBuffer[(Int, Int)] = ArrayBuffer()): Array[(Int, Int)] = {
      acc += Tuple2(x, start._2)

      if (x == end._1) {
        return acc.toArray
      }

      getVentLineHorizontal(start, end, if (x > end._1) x - 1 else x + 1, acc)
    }
    def getVentLineVertical(start: (Int, Int), end: (Int, Int), y: Int = start._2, acc: ArrayBuffer[(Int, Int)] = ArrayBuffer()): Array[(Int, Int)] = {
      acc += Tuple2(start._1, y)

      if (y == end._2) {
        return acc.toArray
      }

      getVentLineVertical(start, end, if (y > end._2) y - 1 else y + 1, acc)
    }
    def getVentLineDiagonal(start: (Int, Int), end: (Int, Int), x: Int = start._1, y: Int = start._2, acc: ArrayBuffer[(Int, Int)] = ArrayBuffer()): Array[(Int, Int)] = {
      acc += Tuple2(x, y)

      if (x == end._1 && y == end._2) {
        return acc.toArray
      }

      getVentLineDiagonal(start, end, if (x > end._1) x - 1 else x + 1, if (y > end._2) y - 1 else y + 1, acc)
    }

    if (start._1 == end._1) {
      getVentLineVertical(start, end)
    } else if (start._2 == end._2) {
      getVentLineHorizontal(start, end)
    } else if (Math.abs(start._1 - end._1) == Math.abs(start._2 - end._2)) {
     getVentLineDiagonal(start, end)
    } else {
      Array()
    }
  }
  def getVentLines(data: Array[String]): Array[Array[(Int, Int)]] = {
    def getVentLinesRecursive(data: Array[String], index: Int = 0, acc: ArrayBuffer[Array[(Int, Int)]] = ArrayBuffer()): Array[Array[(Int, Int)]] = {
      if (index >= data.length) {
        return acc.toArray
      }

      acc += getVentLine(data(index))
      getVentLinesRecursive(data, index + 1, acc)
    }

    getVentLinesRecursive(data)
  }

  def getVentDiagram(data: Array[String]): Array[Array[Int]] = {
    def getVentDiagramBounds(lines: Array[Array[(Int, Int)]], index: Int = 0, acc: (Int, Int) = (0, 0)): (Int, Int) = {
      if (index >= lines.length) {
        return acc
      }

      val maxX = Math.max(acc._1, Math.max(lines(index)(0)._1, lines(index).last._1))
      val maxY = Math.max(acc._2, Math.max(lines(index)(0)._2, lines(index).last._2))

      getVentDiagramBounds(lines, index + 1, (maxX, maxY))
    }
    def getVentDiagramValues(lines: Array[Array[(Int, Int)]], index: Int = 0, acc: Array[Array[Int]]): Array[Array[Int]] = {
      if (index >= lines.length) {
        return acc
      }

      lines(index).foreach(coord => acc(coord._2)(coord._1) += 1)

      getVentDiagramValues(lines, index + 1, acc)
    }

    val lines = getVentLines(data)
    val (maxX, maxY) = getVentDiagramBounds(lines)

    getVentDiagramValues(lines, acc = Array.ofDim[Int](maxY + 1, maxX + 1))
  }
  def getVentDiagramDisplay(data: Array[String]): Array[String] = {
    def getReadableDiagramLine(diagramLine: Array[Int], index: Int = 0, acc: String = ""): String = {
      if (index >= diagramLine.length) {
        return acc
      }
      getReadableDiagramLine(diagramLine, index + 1, acc + { if (diagramLine(index) > 0) diagramLine(index).toString else "." })
    }

    getVentDiagram(data).map(x => getReadableDiagramLine(x))
  }
  def getVentOverlaps(data: Array[String]): Int = {
    val ventDiagram = getVentDiagram(data)
    var overlaps = 0
    ventDiagram.foreach(row => row.foreach(x => if (x > 1) overlaps += 1))
    overlaps
  }
}
