
import suggestions.gui.WikipediaApi

import language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import rx.lang.scala._
import org.scalatest._

object tester extends WikipediaApi {
  def wikipediaSuggestion(term: String) = Future {
    if (term.head.isLetter) {
      for (suffix <- List(" (Computer Scientist)", " (Footballer)")) yield term + suffix
    } else {
      List(term)
    }
  }

  def wikipediaPage(term: String) = Future {
    "Title: " + term
  }

  def tem() = {
    Observable(1, 2, 3).concatRecovered(x => Observable(x, x, x)).map(x => println("hi"))
  }

  tem()
}
