package Day01

object SonarSweep {
  def measureDepth(depth: Array[Int]): Int = {
    def calcDepthIncreases(depth: Array[Int], acc: Int, currentIndex: Int): Int = {
      if (currentIndex == depth.length) {
        return acc
      }

      var newAcc = acc
      if (currentIndex > 0 && depth(currentIndex) > depth(currentIndex - 1)) {
        newAcc += 1
      }

      calcDepthIncreases(depth, newAcc, currentIndex + 1)
    }

    calcDepthIncreases(depth, 0, 0)
  }

  def measureDepthWindowed(depth: Array[Int]): Int = {
    def calcWindowedDepthIncreases(depth: Array[Int], acc: Int, currentIndex: Int, prevWindow: Int = 0): Int = {
      if (currentIndex >= depth.length - 2) {
        return acc
      }

      val window: Int = depth(currentIndex) + depth(currentIndex + 1) + depth(currentIndex + 2)
      var newAcc: Int = acc
      if (currentIndex > 0 && window > prevWindow) {
        newAcc += 1
      }

      calcWindowedDepthIncreases(depth, newAcc, currentIndex + 1, window)
    }

    calcWindowedDepthIncreases(depth, 0, 0)
  }
}
