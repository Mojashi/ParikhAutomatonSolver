package com.github.Mojashi
package solver.mp

import dk.brics.automaton.RegExp
import org.scalatest.funsuite.AnyFunSuiteLike
import com.github.Mojashi.automaton.{NFTransducer, ParikhAutomaton}
import com.github.Mojashi.automaton.impl.BricsNFAAdapter.BricsNFAAdapter
import com.github.Mojashi.automaton.impl.ToParikhVectorTransducer.NFA_Parikh
import com.github.Mojashi.formula.{And, Constant, EQ, GTEQ, Sub, Times, Var}
import com.github.Mojashi.solver.ParikhAutomatonSolver
import com.github.Mojashi.solver.common.Implicits.IntDoubleNumericCast
import com.github.Mojashi.solver.smt.{SMTConventionalExactSolver, SMTCutExactSolver}
import com.github.Mojashi.utils.{NFAToDot, showDotInBrowser}

class MIPExactSolverTest extends AnyFunSuiteLike {
  com.google.ortools.Loader.loadNativeLibraries()

  val solverMakers = Seq(
//    (pa: ParikhAutomaton[Char, Char, Int]) => new MIPSinglePointSolver(pa),
//    (pa: ParikhAutomaton[Char, Char, Int]) => new SMTConventionalExactSolver(pa),
//    (pa: ParikhAutomaton[Char, Char, Int]) => new SMTCutExactSolver(pa)
    (pa: ParikhAutomaton[Char, Char, Int]) => new MIPCutWithLargeMExactSolver(pa)
  )

  test("testSolveInput") {
    val r = new RegExp("xy+z+k*l")
    val a = r.toAutomaton().toNFA.toParikhVectorTransducer

    val pa = ParikhAutomaton(
      constraint = And(Seq(
        GTEQ(Var[Char, Int]('z'), Constant[Char, Int](11)),
        GTEQ(Var[Char, Int]('z'), Var[Char, Int]('k')),
        EQ(Var[Char, Int]('z'), Times(Constant[Char, Int](2), Var[Char, Int]('y'))),
      )),
      voa = a
    )

    // showDotInBrowser(pa.voa.toDot)

    // showDotInBrowser(a.toDot)
    def testIt(solver: ParikhAutomatonSolver[Char, Char, Int]) = {
      val inputOpt = for {
        neu <- solver.solve()
        in <- Some(getInputFromNEU(pa.voa, neu))
      } yield in
      assert(inputOpt.isDefined)

      val input = inputOpt.get
      println(s"ans: ${input.mkString}")

      assert(input.count(ch => ch == 'z') >= 11)
      assert(input.count(ch => ch == 'z') == 2 * input.count(ch => ch == 'y'))


      // incremental solving
      solver.addAtomPredicateConstraint(GTEQ(Var[Char,Int]('k'), Constant(10)))

      val inputOpt2 = for {
        neu <- solver.solve()
        in <- Some(getInputFromNEU(pa.voa, neu))
      } yield in
      assert(inputOpt2.isDefined)

      val input2 = inputOpt2.get
      println(s"ans: ${input2.mkString}")

      assert(input2.count(ch => ch == 'z') >= 11)
      assert(input2.count(ch => ch == 'k') >= 10)
      assert(input2.count(ch => ch == 'z') == 2 * input2.count(ch => ch == 'y'))
    }

    solverMakers.foreach(maker => testIt(maker(pa)))
  }
}
