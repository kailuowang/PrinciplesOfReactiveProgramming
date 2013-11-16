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

  test("1:16 demux") {
    val i, s3, s2, s1, s0 = new Wire
    val out = Range(0, 16).map((_) => new Wire)

    demux(i, List(s3, s2, s1, s0), out.reverse.toList)

    s3.setSignal(false)
    s2.setSignal(true)
    s1.setSignal(true)
    s0.setSignal(false)
    i.setSignal(true)
    run

    assert(out(6).getSignal === true)
    assert(out.filterNot(_ == out(6)).forall(!_.getSignal) === true)

    s3.setSignal(false)
    s2.setSignal(true)
    s1.setSignal(false)
    s0.setSignal(true)
    i.setSignal(true)
    run

    assert(out(5).getSignal === true)
    assert(out.filterNot(_ == out(5)).forall(!_.getSignal) === true)

    s3.setSignal(true)
    s2.setSignal(true)
    s1.setSignal(true)
    s0.setSignal(false)
    i.setSignal(true)
    run

    assert(out(14).getSignal === true)
    assert(out.filterNot(_ == out(14)).forall(!_.getSignal) === true)

  }
}
