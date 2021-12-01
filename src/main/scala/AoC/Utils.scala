package AoC

import java.nio.file.Paths

object Utils {
  def readFile(filename: String): Array[String] = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines = (for (line <- bufferedSource.getLines()) yield line).toArray[String]
    bufferedSource.close()
    lines
  }
}
