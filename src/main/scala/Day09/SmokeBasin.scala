package Day09

import scala.collection.mutable.ArrayBuffer

object SmokeBasin {
  def getMap(data: Array[String]): Array[Array[Int]] = {
    val mapBuffer: ArrayBuffer[Array[Int]] = ArrayBuffer()

    data.foreach(line => {
      val row: ArrayBuffer[Int] = ArrayBuffer()
      line.foreach(c => row += c.toString.toInt)
      mapBuffer += row.toArray
    })

    mapBuffer.toArray
  }

  def getLowPoints(map: Array[Array[Int]]): Array[(Int, Int)] = {
    def getLowerNeighbours(map: Array[Array[Int]], x: Int = 0, y: Int = 0): Array[(Int, Int)] = {
      val neighbours: ArrayBuffer[(Int, Int)] = ArrayBuffer()

      // Check left
      if (x > 0) {
        if (map(y)(x - 1) <= map(y)(x)) {
          neighbours += Tuple2(y, x - 1)
        }
      }
      // Check up
      if (y > 0) {
        if (map(y - 1)(x) <= map(y)(x)) {
          neighbours += Tuple2(y - 1, x)
        }
      }
      // Check right
      if (x < map(y).length - 1) {
        if (map(y)(x + 1) <= map(y)(x)) {
          neighbours += Tuple2(y, x + 1)
        }
      }
      // Check down
      if (y < map.length - 1) {
        if (map(y + 1)(x) <= map(y)(x)) {
          neighbours += Tuple2(y + 1, x)
        }
      }

      neighbours.toArray
    }
    def getLowPointsRecursive(map: Array[Array[Int]], x: Int = 0, y: Int = 0, acc: ArrayBuffer[(Int, Int)] = ArrayBuffer()): Array[(Int, Int)] = {
      if (y >= map.length) {
        return acc.toArray
      }

      val lowerNeighbours = getLowerNeighbours(map, x, y)
      if (lowerNeighbours.length == 0) {
        acc += Tuple2(y, x)
      }

      var newX = x + 1
      var newY = y
      if (newX >= map(y).length) {
        newX = 0
        newY += 1
      }

      getLowPointsRecursive(map, newX, newY, acc)
    }

    getLowPointsRecursive(map)
  }
  def getBasins(map: Array[Array[Int]]): Array[Array[(Int, Int)]] = {
    def getNeighboursNon9(map: Array[Array[Int]], x: Int, y: Int): Array[(Int, Int)] = {
      val neighbours: ArrayBuffer[(Int, Int)] = ArrayBuffer()

      // Check left
      if (x > 0) {
        if (map(y)(x - 1) < 9) {
          neighbours += Tuple2(y, x - 1)
        }
      }
      // Check up
      if (y > 0) {
        if (map(y - 1)(x) < 9) {
          neighbours += Tuple2(y - 1, x)
        }
      }
      // Check right
      if (x < map(y).length - 1) {
        if (map(y)(x + 1) < 9) {
          neighbours += Tuple2(y, x + 1)
        }
      }
      // Check down
      if (y < map.length - 1) {
        if (map(y + 1)(x) < 9) {
          neighbours += Tuple2(y + 1, x)
        }
      }

      neighbours.toArray
    }
    def getBasin(map: Array[Array[Int]], lowPoint: (Int, Int)): Array[(Int, Int)] = {
      def getBasinRecursive(map: Array[Array[Int]], searchList: ArrayBuffer[(Int, Int)], index: Int = 0, acc: ArrayBuffer[(Int, Int)] = ArrayBuffer()): Array[(Int, Int)] = {
        if (index >= searchList.length) {
          return acc.toArray
        }

        val (y, x) = searchList(index)
        getNeighboursNon9(map, x, y).foreach(coord => if (!searchList.contains(coord)) searchList += coord)
        acc += searchList(index)

        getBasinRecursive(map, searchList, index + 1, acc)
      }
      getBasinRecursive(map, ArrayBuffer(lowPoint))
    }

    val basins: ArrayBuffer[Array[(Int, Int)]] = ArrayBuffer()
    getLowPoints(map).foreach(lowPoint => basins += getBasin(map, lowPoint))
    basins.toArray
  }

  def getTotalRiskLevel(map: Array[Array[Int]]): Int = {
    val lowPoints = getLowPoints(map)
    lowPoints.map(coord => map(coord._1)(coord._2) + 1).sum
  }
  def getLargestBasinsProduct(map: Array[Array[Int]]): Int = {
    val basinSizes = getBasins(map).map(_.length).sorted(Ordering.Int.reverse)
    basinSizes(0) * basinSizes(1) * basinSizes(2)
  }
}
