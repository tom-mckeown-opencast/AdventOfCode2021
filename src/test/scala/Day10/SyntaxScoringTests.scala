package Day10

import AoC.Utils
import org.scalatest.funspec.AnyFunSpec

class SyntaxScoringTests extends AnyFunSpec {
  val exampleData = Array(
    "[({(<(())[]>[[{[]{<()<>>",
    "[(()[<>])]({[<{<<[]>>(",
    "{([(<{}[<>[]}>{[]{[(<()>",
    "(((({<>}<{<{<>}{[]{[]{}",
    "[[<[([]))<([[{}[[()]]]",
    "[{[{({}]{}}([{[{{{}}([]",
    "{<[[]]>}<{[{[{[]{()[[[]",
    "[<(<(<(<{}))><([]([]()",
    "<{([([[(<>()){}]>(<<{{",
    "<{([{{}}[<[[[<>{}]]]>[]]"
  )
  val taskData = Utils.readFile("src/test/scala/Day10/input.txt")

  describe("Corrupted syntax tests") {
    describe("Check basic single-pair chunks") {
      describe("Check the chunk '()'") {
        it("Should return no error character") {
          assert(SyntaxScoring.getErrorChar("()") == 0)
        }
      }
      describe("Check the chunk '[]'") {
        it("Should return no error character") {
          assert(SyntaxScoring.getErrorChar("[]") == 0)
        }
      }
      describe("Check the chunk '{}'") {
        it("Should return no error character") {
          assert(SyntaxScoring.getErrorChar("{}") == 0)
        }
      }
      describe("Check the chunk '<>'") {
        it("Should return no error character") {
          assert(SyntaxScoring.getErrorChar("<>") == 0)
        }
      }
    }
    describe("Check basic 2-pair chunks") {
      describe("Check the chunk '(())'") {
        it("Should return no error character") {
          assert(SyntaxScoring.getErrorChar("(())") == 0)
        }
      }
      describe("Check the chunk '[[]]'") {
        it("Should return no error character") {
          assert(SyntaxScoring.getErrorChar("[[]]") == 0)
        }
      }
      describe("Check the chunk '{{}}'") {
        it("Should return no error character") {
          assert(SyntaxScoring.getErrorChar("{{}}") == 0)
        }
      }
      describe("Check the chunk '<<>>'") {
        it("Should return no error character") {
          assert(SyntaxScoring.getErrorChar("<<>>") == 0)
        }
      }
    }
    describe("Check different types 2-pair chunks") {
      describe("Check the chunk '([])'") {
        it("Should return no error character") {
          assert(SyntaxScoring.getErrorChar("([])") == 0)
        }
      }
      describe("Check the chunk '[{}]'") {
        it("Should return no error character") {
          assert(SyntaxScoring.getErrorChar("[{}]") == 0)
        }
      }
      describe("Check the chunk '{<>}'") {
        it("Should return no error character") {
          assert(SyntaxScoring.getErrorChar("{<>}") == 0)
        }
      }
      describe("Check the chunk '<()>'") {
        it("Should return no error character") {
          assert(SyntaxScoring.getErrorChar("<()>") == 0)
        }
      }
    }
    describe("Check corrupted chunks") {
      describe("Check the chunk '(]'") {
        it("Should return the error character ']'") {
          assert(SyntaxScoring.getErrorChar("(]") == ']')
        }
      }
      describe("Check the chunk '}'") {
        it("Should return the error character '}'") {
          assert(SyntaxScoring.getErrorChar("}") == '}')
        }
      }
      describe("Check the chunk '{(){}]'") {
        it("Should return the error character ']'") {
          assert(SyntaxScoring.getErrorChar("{(){}]") == ']')
        }
      }
      describe("Check the chunk '{()()()>'") {
        it("Should return the error character '>'") {
          assert(SyntaxScoring.getErrorChar("{()()()>") == '>')
        }
      }
    }
    describe("Check unfinished chunks") {
      describe("Check the chunk '(()'") {
        it("Should return no error character") {
          assert(SyntaxScoring.getErrorChar("(()") == 0)
        }
      }
      describe("Check the chunk '{[]'") {
        it("Should return no error character") {
          assert(SyntaxScoring.getErrorChar("[[]") == 0)
        }
      }
      describe("Check the chunk '{([['") {
        it("Should return no error character") {
          assert(SyntaxScoring.getErrorChar("{([[") == 0)
        }
      }
    }
  }
  describe("Syntax score tests") {
    describe("Get score for '()'") {
      it("Should return 0") {
        assert(SyntaxScoring.getErrorScore("()") == 0)
      }
    }
    describe("Get score for '<)'") {
      it("Should return 3") {
        assert(SyntaxScoring.getErrorScore("<)") == 3)
      }
    }
    describe("Get score for '(]'") {
      it("Should return 57") {
        assert(SyntaxScoring.getErrorScore("(]") == 57)
      }
    }
    describe("Get score for '[}'") {
      it("Should return 1197") {
        assert(SyntaxScoring.getErrorScore("[}") == 1197)
      }
    }
    describe("Get score for '{>'") {
      it("Should return 25137") {
        assert(SyntaxScoring.getErrorScore("{>") == 25137)
      }
    }
    describe("Get score for example data") {
      it("Should return 26397 (6+57+1197+25137)") {
        assert(SyntaxScoring.getTotalErrorScore(exampleData) == 26397)
      }
    }
    describe("Get score for task data") {
      it("Should return 469755") {
        assert(SyntaxScoring.getTotalErrorScore(taskData) == 469755)
      }
    }
  }
  describe("Solve unfinished lines") {
    describe("Remove all corrupted lines") {
      it("Should remove corrupted lines from example data") {
        val expected = Array(
          "[({(<(())[]>[[{[]{<()<>>",
          "[(()[<>])]({[<{<<[]>>(",
          "(((({<>}<{<{<>}{[]{[]{}",
          "{<[[]]>}<{[{[{[]{()[[[]",
          "<{([{{}}[<[[[<>{}]]]>[]]"
        )
        assert(SyntaxScoring.removeCorruptedLines(exampleData) sameElements expected)
      }
    }
    describe("Autocomplete line '[({(<(())[]>[[{[]{<()<>>'") {
      it("Should return '}}]])})]'") {
        assert(SyntaxScoring.getAutocompletion("[({(<(())[]>[[{[]{<()<>>") == "}}]])})]")
      }
    }
    describe("Autocomplete line '[(()[<>])]({[<{<<[]>>('") {
      it("Should return ')}>]})'") {
        assert(SyntaxScoring.getAutocompletion("[(()[<>])]({[<{<<[]>>(") == ")}>]})")
      }
    }
    describe("Autocomplete line '(((({<>}<{<{<>}{[]{[]{}'") {
      it("Should return '}}>}>))))'") {
        assert(SyntaxScoring.getAutocompletion("(((({<>}<{<{<>}{[]{[]{}") == "}}>}>))))")
      }
    }
    describe("Autocomplete line '{<[[]]>}<{[{[{[]{()[[[]'") {
      it("Should return ']]}}]}]}>'") {
        assert(SyntaxScoring.getAutocompletion("{<[[]]>}<{[{[{[]{()[[[]") == "]]}}]}]}>")
      }
    }
    describe("Autocomplete line '<{([{{}}[<[[[<>{}]]]>[]]'") {
      it("Should return '])}>'") {
        assert(SyntaxScoring.getAutocompletion("<{([{{}}[<[[[<>{}]]]>[]]") == "])}>")
      }
    }
    describe("Get autocomplete score for '[({(<(())[]>[[{[]{<()<>>'") {
      it("Should return 288957") {
        assert(SyntaxScoring.getAutocompletionScore("[({(<(())[]>[[{[]{<()<>>") == 288957)
      }
    }
    describe("Get autocomplete score for '[(()[<>])]({[<{<<[]>>('") {
      it("Should return 5566") {
        assert(SyntaxScoring.getAutocompletionScore("[(()[<>])]({[<{<<[]>>(") == 5566)
      }
    }
    describe("Get autocomplete score for '(((({<>}<{<{<>}{[]{[]{}'") {
      it("Should return 1480781") {
        assert(SyntaxScoring.getAutocompletionScore("(((({<>}<{<{<>}{[]{[]{}") == 1480781)
      }
    }
    describe("Get autocomplete score for '{<[[]]>}<{[{[{[]{()[[[]'") {
      it("Should return 995444") {
        assert(SyntaxScoring.getAutocompletionScore("{<[[]]>}<{[{[{[]{()[[[]") == 995444)
      }
    }
    describe("Get autocomplete score for '<{([{{}}[<[[[<>{}]]]>[]]'") {
      it("Should return 294") {
        assert(SyntaxScoring.getAutocompletionScore("<{([{{}}[<[[[<>{}]]]>[]]") == 294)
      }
    }
    describe("Get autocomplete score for example data") {
      it("Should return 288957") {
        assert(SyntaxScoring.getTotalAutocompletionScore(exampleData) == 288957)
      }
    }
    describe("Get autocomplete score for task data") {
      it("Should return 2762335572") {
        assert(SyntaxScoring.getTotalAutocompletionScore(taskData) == 2762335572L)
      }
    }
  }
}