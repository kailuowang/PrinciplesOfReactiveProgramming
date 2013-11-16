package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal

  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def connect(input: Wire, output: Wire) {
    def connectAction() {
      val inputSig = input.getSignal
      afterDelay(0) { output.setSignal(inputSig) }
    }
    input addAction connectAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val notIn1, notIn2, notOut = new Wire
    inverter(a1, notIn1)
    inverter(a2, notIn2)
    andGate(notIn1, notIn2, notOut)
    inverter(notOut, output)
  }

  def decoder(in: List[Wire]): List[Wire] = {
     in match {
       case a0 :: Nil => {
         val d0, d1 = new Wire
         connect(a0, d1)
         inverter(a0, d0)
         List(d1, d0)
       }
       case a1 :: tail => {
         val rest = decoder(tail)
         val a1D = decoder(a1 :: Nil)
         for( a1r <- a1D; dr <- rest) yield {
           val d = new Wire
           andGate(dr, a1r, d)
           d
         }
       }
     }
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    val decoderWires = decoder(c)
    decoderWires.zip(out).foreach {
      case (d, o) => andGate(in, d, o)
    }
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  def orGateExample {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(true)
    in2.setSignal(false)
    run

    in1.setSignal(false)
    run

    in2.setSignal(true)
    run
  }

  def demuxExample {
    val i, s3, s2, s1, s0 = new Wire
    probe("s3", s3)
    probe("s2", s2)
    probe("s1", s1)
    probe("s0", s0)
    val out = Range(0, 16).map( i => {
      val w = new Wire
      probe( "f" + i.toString, w)
      w
    })

    demux(i, List(s3, s2, s1, s0), out.reverse.toList)
    i.setSignal(true)

    s3.setSignal(false)
    s2.setSignal(true)
    s1.setSignal(true)
    s0.setSignal(false)
    run

    s3.setSignal(true)
    s2.setSignal(true)
    s1.setSignal(true)
    s0.setSignal(false)
    run
  }
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
  Circuit.demuxExample
}
