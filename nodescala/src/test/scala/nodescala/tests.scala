package nodescala



import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  def testExceptionOccurred[T](f: Future[T]): Boolean =
    try {
      Await.result(f, 1 second)
      false
    } catch {
      case TestException => true
    }

  case object TestException extends Exception
  test("A Future should always be created") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }


  test("all success when all future succeed") {
    val all = Future.all[Int](List(Future.always(1), Future.always(2), Future.always(3)))

    assert(Await.result(all, 1 second) == List(1,2,3))
  }


  test("all returns a failure that returns when all future succeed") {
    val all = Future.all[Int](List(Future.always(1), Future.always(2), Future.failed(TestException)))

    assert(testExceptionOccurred(all))
  }

  test("delay timeout before delay") {
    val delay = Future.delay(100 milliseconds)
    try {
      Await.result(delay, 50 milliseconds)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("delay resolves after delay") {
    val delay = Future.delay(20 milliseconds)
    assert(Await.result(delay, 25 milliseconds).isInstanceOf[Unit])
  }

  test("any success when any future succeed") {
    val any = Future.any[Int](List(Future.always(1), Future.delay(2 seconds).map(u => 4)))

    assert(Await.result(any, 1 second) == 1, Await.result(any, 0 nanosecond))
  }

  test("any fails when any future failed") {
    val any = Future.any[Int](List(Future.failed(TestException), Future.delay(2 seconds).map(u => 4)))

    assert(testExceptionOccurred(any))
  }


  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  test("CancellationTokenSource should remain cancelled after unsubscribed multiple times") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    cts.unsubscribe()
    cts.unsubscribe()
    assert(ct.isCancelled)
  }

  test("Future run can be stopped") {
    var state: Int = 0
    val work = Future.run() { token =>
      Future {
        while(token.nonCancelled) {
          state = 1
        }
        state = 2
      }
    }
    val working  = Future.delay(50 milliseconds).map { u =>
      state == 1
    }
    Future.delay(75 milliseconds) onSuccess {
      case _ => work.unsubscribe()
    }
    val stopped = Future.delay(100 milliseconds) map { u =>
      state == 2
    }
    assert(Await.result(working, 1 second))
    assert(Await.result(stopped, 1 second))
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}




