package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala._

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("orGate") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(true)
    in2.setSignal(false)
    run

    assert(out.getSignal === true, "or 1")

    in1.setSignal(false)
    run

    assert(out.getSignal === false, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")
  }

  test("orGate2") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(true)
    in2.setSignal(false)
    run

    assert(out.getSignal === true, "or2 1")

    in1.setSignal(false)
    run

    assert(out.getSignal === false, "or2 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or2 3")
  }

  test("2-to-4 decoder") {
    val a1, a0 = new Wire
    val List(d3, d2, d1, d0) = decoder(List(a1, a0))
    a1.setSignal(false)
    a0.setSignal(false)
    run

    assert(d0.getSignal === true, "decode 1")
    assert(List(d3, d2, d1).forall(!_.getSignal) === true, "decode 1a")

    a1.setSignal(false)
    a0.setSignal(true)
    run

    assert(d1.getSignal === true, "decode 1")
    assert(List(d3, d2, d0).forall(!_.getSignal) === true, "decode 1a")

    a1.setSignal(true)
    a0.setSignal(false)
    run

    assert(d2.getSignal === true, "decode 1")
    assert(List(d3, d1, d0).forall(!_.getSignal) === true, "decode 1a")

    a1.setSignal(true)
    a0.setSignal(true)
    run

    assert(d3.getSignal === true, "decode 1")
    assert(List(d2, d1, d0).forall(!_.getSignal) === true, "decode 1a")

  }

  test("3-to-8 decoder") {
    val a2, a1, a0 = new Wire
    val out = decoder(List(a2, a1, a0))
    val List(d7, d6, d5, d4, d3, d2, d1, d0) = out
    a2.setSignal(false)
    a1.setSignal(true)
    a0.setSignal(false)
    run

    assert(d2.getSignal === true, "3-8 decode 1")
    assert(out.filterNot(_ == d2).forall(!_.getSignal) === true, "3-8 decode 1a")

    a2.setSignal(true)
    a1.setSignal(false)
    a0.setSignal(true)
    run

    assert(d5.getSignal === true, "3-8 decode 2")
    assert(out.filterNot(_ == d5).forall(!_.getSignal) === true, "3-8 decode 2a")


    a2.setSignal(true)
    a1.setSignal(true)
    a0.setSignal(true)
    run

    assert(d7.getSignal === true, "3-8 decode 3")
    assert(out.filterNot(_ == d7).forall(!_.getSignal) === true, "3-8 decode 3a")



  }

  //
  // to complete with tests for orGate, demux, ...
  //

}
