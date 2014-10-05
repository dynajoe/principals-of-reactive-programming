package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

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

  //
  // to complete with tests for orGate, demux, ...
  //

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")
  }

  test("demuxSimple1") {
    val in, c0, out0, out1 = new Wire
    demux(in, List(c0), List(out1, out0)) // NOTE ORDER!
    for (
      (inv, c0v, out0v, out1v) <- Seq(
        (false, false, false, false),
        (false, true, false, false),
        (true, false, true, false),
        (true, true, false, true))
    ) {
      c0.setSignal(c0v)
      in.setSignal(inv)
      run
      assert(out0.getSignal == out0v, s"1 cline demux out0 $inv @ $c0v")
      assert(out1.getSignal == out1v, s"1 cline demux out1 $inv @ $c0v")
    }
  }

  test("demuxSimple0") {
    val in, out0 = new Wire
    demux(in, List(), List(out0))
    for (s <- Seq(false, true)) {
      in.setSignal(s)
      run
      assert(out0.getSignal == s, s"base demux $s")
    }
  }
}
