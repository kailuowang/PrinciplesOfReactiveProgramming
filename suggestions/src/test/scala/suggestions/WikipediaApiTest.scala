package suggestions



import language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import rx.lang.scala._
import org.scalatest._
import gui._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.collection.mutable
import rx.lang.scala.subjects.ReplaySubject


@RunWith(classOf[JUnitRunner])
class WikipediaApiTest extends FunSuite {

  object mockApi extends WikipediaApi {
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
  }

  import mockApi._

  test("WikipediaApi should make the stream valid using sanitized") {
    val notvalid = Observable("erik", "erik meijer", "martin")
    val valid = notvalid.sanitized

    var count = 0
    var completed = false

    val sub = valid.subscribe(
      term => {
        assert(term.forall(_ != ' '))
        count += 1
      },
      t => assert(false, s"stream error $t"),
      () => completed = true
    )
    assert(completed && count == 3, "completed: " + completed + ", event count: " + count)
  }

  test("WikipediaApi should correctly use concatRecovered") {
    val requests = Observable(1, 2, 3)
    val remoteComputation = (n: Int) => Observable(0 to n)
    val responses = requests concatRecovered remoteComputation
    val sum = responses.foldLeft(0) { (acc, tn) =>
      tn match {
        case Success(n) => acc + n
        case Failure(t) => throw t
      }
    }
    var total = -1
    val sub = sum.subscribe {
      s => total = s
    }
    assert(total == (1 + 1 + 2 + 1 + 2 + 3), s"Sum: $total")
  }

  test("recovered should return Try[T] and terminates on Error") {
    val subject = ReplaySubject[Int]
    val observed = mutable.Buffer[Try[Int]]()
    val recovered = subject.recovered
    var completed = false
    var errored = false
    recovered subscribe(
      observed += _,
      (e) => errored = true,
      () => completed = true
    )
    subject.onNext(1)
    subject.onNext(2)
    assert(observed.toList == List(Success(1),Success(2)))

    val e = new Throwable("test")

    subject.onError(e)
    assert(observed.last == Failure(e))

    assert(completed)
    assert(!errored)
  }

  test("recovered should return Try[T] and terminates on complete") {
    val subject = ReplaySubject[Int]
    val observed = mutable.Buffer[Try[Int]]()
    val recovered = subject.recovered
    var completed = false
    recovered subscribe(
      observed += _,
      (e) => throw e,
      () => completed = true
    )
    subject.onNext(1)
    assert(observed.last == Success(1))

    subject.onCompleted()

    assert(completed, "didn't complete")
  }


  test("timeout should complete on time out"){
    val result = Observable(1, 2, 3).zip(Observable.interval(0.5 second)).timedOut(1L).toBlockingObservable.toList

    assert(result == List((1, 0L)), result.toString)
  }

}
