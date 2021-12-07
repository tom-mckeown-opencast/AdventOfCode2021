package AoC

import java.nio.file.Paths

object Utils {
  def readFile(filename: String): Array[String] = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines = (for (line <- bufferedSource.getLines()) yield line).toArray[String]
    bufferedSource.close()
    lines
  }

  def deepCompare[T](a1: Array[Array[T]], a2: Array[Array[T]]): Boolean = {
    def deepCompareRecursive(a1: Array[Array[T]], a2: Array[Array[T]], index: Int = 0, acc: Boolean = true): Boolean = {
      if (index >= a1.length || !acc) {
        return acc
      }
      deepCompareRecursive(a1, a2, index + 1, acc && (a1(index) sameElements a2(index)))
    }
    deepCompareRecursive(a1, a2)
  }
  def deepCompare[T](a1: Array[Array[Array[T]]], a2: Array[Array[Array[T]]]): Boolean = {
    def deepCompareRecursive(a1: Array[Array[Array[T]]], a2: Array[Array[Array[T]]], index: Int = 0, acc: Boolean = true): Boolean = {
      if (index >= a1.length || !acc) {
        return acc
      }
      deepCompareRecursive(a1, a2, index + 1, acc && deepCompare(a1(index), a2(index)))
    }
    deepCompareRecursive(a1, a2)
  }
}
