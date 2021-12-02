package Day02

object Dive {
  def calculatePosition(movements: Array[String]): Int = {
    def calculateMovements(movements: Array[String], currentIndex: Int, horizontalAcc: Int, depthAcc: Int): Int = {
      if (currentIndex > movements.length - 1) {
        return horizontalAcc * depthAcc
      }

      var newHorizontalAcc = horizontalAcc
      var newDepthAcc = depthAcc

      val moveSplit = movements(currentIndex).split(" ")
      val moveType = moveSplit(0).toLowerCase
      var moveVal = moveSplit(1).toInt

      moveType match {
        case "forward" => newHorizontalAcc += moveVal
        case "down" => newDepthAcc += moveVal
        case "up" => newDepthAcc -= moveVal
      }

      calculateMovements(movements, currentIndex + 1, newHorizontalAcc, newDepthAcc)
    }

    calculateMovements(movements, 0, 0, 0)
  }

  def calculatePositionWithAim(movements: Array[String]): Int = {
    def calculateMovements(movements: Array[String], currentIndex: Int, horizontalAcc: Int, depthAcc: Int, aimAcc: Int): Int = {
      if (currentIndex > movements.length - 1) {
        return horizontalAcc * depthAcc
      }

      var newHorizontalAcc = horizontalAcc
      var newDepthAcc = depthAcc
      var newAimAcc = aimAcc

      val moveSplit = movements(currentIndex).split(" ")
      val moveType = moveSplit(0).toLowerCase
      var moveVal = moveSplit(1).toInt

      moveType match {
        case "forward" => {
          newHorizontalAcc += moveVal
          newDepthAcc += aimAcc * moveVal
        }
        case "down" => newAimAcc += moveVal
        case "up" => newAimAcc -= moveVal
      }

      calculateMovements(movements, currentIndex + 1, newHorizontalAcc, newDepthAcc, newAimAcc)
    }

    calculateMovements(movements, 0, 0, 0, 0)
  }
}
