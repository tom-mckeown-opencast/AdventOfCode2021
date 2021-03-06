package Day10

import scala.collection.mutable

object SyntaxScoring {
  private val OPEN_CHUNK_CHARACTERS = Array('(', '[', '{', '<')
  private val CLOSE_CHUNK_CHARACTERS = Array(')', ']', '}', '>')
  private val MATCHING_CHUNK_CHARACTERS = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')
  private val ERROR_SCORES = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137, ' ' -> 0)
  private val COMPLETION_SCORES = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)

  def getErrorChar(line: String): Char = {
    val openChunks: mutable.Stack[Char] = mutable.Stack()

    line.foreach(c => {
      if (OPEN_CHUNK_CHARACTERS.contains(c)) {
        openChunks.push(c)
      } else if (CLOSE_CHUNK_CHARACTERS.contains(c)) {
        if (openChunks.nonEmpty && MATCHING_CHUNK_CHARACTERS(openChunks.top) == c) {
          openChunks.pop()
        } else {
          return c
        }
      }
    })

    0
  }
  def getErrorScore(line: String): Int = {
    ERROR_SCORES(getErrorChar(line))
  }
  def getTotalErrorScore(data: Array[String]): Int = {
    data.map(line => getErrorScore(line)).sum
  }

  def removeCorruptedLines(data: Array[String]): Array[String] = {
    data.filter(line => getErrorScore(line) == 0)
  }
  def getAutocompletion(line: String): String = {
    val openChunks: mutable.Stack[Char] = mutable.Stack()

    line.foreach(c => {
      if (OPEN_CHUNK_CHARACTERS.contains(c)) {
        openChunks.push(c)
      } else if (CLOSE_CHUNK_CHARACTERS.contains(c)) {
        if (openChunks.nonEmpty && MATCHING_CHUNK_CHARACTERS(openChunks.top) == c) {
          openChunks.pop()
        } else {
          println(s"ERROR: Expected '${openChunks.top}', but found '$c' instead.")
        }
      }
    })

    openChunks.map(c => MATCHING_CHUNK_CHARACTERS(c)).mkString
  }
  def getAutocompletionScore(line: String): Long = {
    var score = 0L
    getAutocompletion(line).foreach(c => score = score * 5 + COMPLETION_SCORES(c))
    score
  }
  def getTotalAutocompletionScore(data: Array[String]): Long = {
    val mapped = removeCorruptedLines(data).map(line => getAutocompletionScore(line)).sorted
    mapped(mapped.length / 2)
  }
}
