package Day08

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object SevenSegments {
  private val ExpectedSignalsMap: Map[Int, String] = Map(
    0 -> "abcefg",
    1 -> "cf",
    2 -> "acdeg",
    3 -> "acdfg",
    4 -> "bcdf",
    5 -> "abdfg",
    6 -> "abdefg",
    7 -> "acf",
    8 -> "abcdefg",
    9 -> "abcdfg"
  )
  private val SignalAnswersMap: Map[String, Int] = Map(
    "abcefg" -> 0,
    "cf" -> 1,
    "acdeg" -> 2,
    "acdfg" -> 3,
    "bcdf" -> 4,
    "abdfg" -> 5,
    "abdefg" -> 6,
    "acf" -> 7,
    "abcdefg" -> 8,
    "abcdfg" -> 9
  )

  def solve(line: String): Array[Int] = {
    def sortSignals(signals: Array[String]): Array[String] = {
      val a: ArrayBuffer[String] = ArrayBuffer()
      a += signals.find(s => s.length == 7).mkString
      a += signals.find(s => s.length == 2).mkString
      a += signals.find(s => s.length == 3).mkString
      a += signals.find(s => s.length == 4).mkString
      a += signals.find(s => s.length == 6).mkString
      a += signals.find(s => s.length == 6 && !a.contains(s)).mkString
      a += signals.find(s => s.length == 6 && !a.contains(s)).mkString
      a += signals.find(s => s.length == 5).mkString
      a += signals.find(s => s.length == 5 && !a.contains(s)).mkString
      a += signals.find(s => s.length == 5 && !a.contains(s)).mkString
      a.toArray
    }
    def compareSignals(left: String, right: String): (Array[Char], Array[Char]) = {
      val matches: ArrayBuffer[Char] = ArrayBuffer()
      val nonmatches: ArrayBuffer[Char] = ArrayBuffer()

      right.foreach(c => {
        if (left.contains(c)) {
          matches += c
        } else {
          nonmatches += c
        }
      })

      (matches.toArray, nonmatches.toArray)
    }
    def solveDigit(signal: String, signalMap: mutable.Map[Char, Char], digitMap: mutable.Map[Int, String]): Int = {
      var digit: Int = -1

      if (signal.length == 7) {
        digit = 8
      } else if (signal.length == 2) {
        digit = 1
      } else if (signal.length == 3) {
        digit = 7
        val (matches, nonmatches) = compareSignals(digitMap(1), signal)
        signalMap += 'a' -> nonmatches(0)
      } else if (signal.length == 4) {
        digit = 4
      } else if (signal.length == 5) {
        val (matches, nonmatches) = compareSignals(signal, digitMap(1))
        if (nonmatches.length == 0) {
          digit = 3
        } else {
          if (matches(0) == signalMap('c')) {
            digit = 2
          } else if (matches(0) == signalMap('f')) {
            digit = 5
          }
        }
      } else if (signal.length == 6) {
        val (matches, nonmatches) = compareSignals(signal, digitMap(1))
        if (matches.length == 1) {
          digit = 6
          signalMap += 'c' -> nonmatches(0)
          signalMap += 'f' -> matches(0)
        } else {
          val (matches, nonmatches) = compareSignals(signal, digitMap(4))
          if (nonmatches.length == 0) {
            digit = 9
          } else {
            digit = 0
            signalMap += 'd' -> nonmatches(0)
          }
        }
      }

      digitMap += digit -> signal
      digit
    }
    def solveAnswers(signals: Array[String], signalMap: mutable.Map[Char, Char]): Array[Int] = {
      val solvingSignalMap = signalMap.map(_.swap)

      Array(
        SignalAnswersMap(signals(0).map(c => solvingSignalMap(c)).sorted),
        SignalAnswersMap(signals(1).map(c => solvingSignalMap(c)).sorted),
        SignalAnswersMap(signals(2).map(c => solvingSignalMap(c)).sorted),
        SignalAnswersMap(signals(3).map(c => solvingSignalMap(c)).sorted)
      )
    }
    def finishMappings(signals: Array[String], signalMap: mutable.Map[Char, Char], digitMap: mutable.Map[Int, String]): Unit = {
      val solvingSignalMap: mutable.Map[Char, Char] = signalMap.map(_.swap)
      val solvingDigitMap: mutable.Map[String, Int] = digitMap.map(_.swap)

      signals.foreach(signal => {
        var acc: String = signal
        var expectedSignal: String = ExpectedSignalsMap(solvingDigitMap(signal))

        signal.foreach(c => {
          if (solvingSignalMap.keys.exists(_ == c)) {
            expectedSignal = expectedSignal.replace(solvingSignalMap(c).toString, "")
            acc = acc.replace(c.toString, "")
          }
        })

        if (expectedSignal.length == 1) {
          signalMap += expectedSignal(0) -> acc(0)
          solvingSignalMap += acc(0) -> expectedSignal(0)
        }
      })
    }

    val split = line.split(""" \| """)
    val workingSignals = sortSignals(split(0).split(" "))
    val answerSignals = split(1).split(" ")

    val signalMap: mutable.Map[Char, Char] = mutable.Map()
    val digitMap: mutable.Map[Int, String] = mutable.Map()

    workingSignals.foreach(signal => solveDigit(signal, signalMap, digitMap))
    finishMappings(workingSignals, signalMap, digitMap)

    solveAnswers(answerSignals, signalMap)
  }
  def solveAll(data: Array[String]): Array[String] = {
    val buffer: ArrayBuffer[String] = ArrayBuffer()
    data.foreach(line => buffer += solve(line).mkString)
    buffer.toArray
  }

  def getDigitCount(solved: Array[String]): Map[Char, Int] = {
    val digitCount: mutable.Map[Char, Int] = mutable.Map(
      '0' -> 0,
      '1' -> 0,
      '2' -> 0,
      '3' -> 0,
      '4' -> 0,
      '5' -> 0,
      '6' -> 0,
      '7' -> 0,
      '8' -> 0,
      '9' -> 0
    )
    solved.foreach(a => a.foreach(d => digitCount(d) += 1))
    digitCount.toMap
  }
  def getAnswerSum(solved: Array[String]): Int = {
    solved.map(_.toInt).sum
  }
}
