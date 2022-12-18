package xyz.mojashi
package solver.mp

import dk.brics.automaton.RegExp
import org.scalatest.funsuite.AnyFunSuiteLike
import xyz.mojashi.automaton.ParikhAutomaton
import xyz.mojashi.automaton.impl.BricsNFAAdapter.BricsNFAAdapter
import xyz.mojashi.automaton.impl.ToParikhVectorTransducer.NFA_Parikh
import xyz.mojashi.formula.{And, Constant, EQ, GTEQ, Sub, Times, Var}
import xyz.mojashi.solver.ParikhAutomatonSolver
import xyz.mojashi.solver.algorithm.Implicits.IntDoubleNumericCast
import xyz.mojashi.solver.smt.SMTConventionalExactSolver
import xyz.mojashi.utils.{NFAToDot, showDotInBrowser}

class MIPExactSolverTest extends AnyFunSuiteLike {
  com.google.ortools.Loader.loadNativeLibraries()

  val solverMakers = Seq(
    (pa: ParikhAutomaton[Char, Int, Char, Int]) => new MIPSinglePointSolver(pa),
    (pa: ParikhAutomaton[Char, Int, Char, Int]) => new SMTConventionalExactSolver(pa)
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

    //showDotInBrowser(a.toDot)
    def testIt(solver: ParikhAutomatonSolver[Char, Int, Char, Int]) = {
      val inputOpt = for {
        neu <- solver.solve()
        in <- Some(getInputFromNEU(pa.voa, neu))
      } yield in
      assert(inputOpt.isDefined)

      val input = inputOpt.get
      println(s"ans: ${input.mkString}")

      assert(input.count(ch => ch == 'z') >= 11)
      assert(input.count(ch => ch == 'z') == 2 * input.count(ch => ch == 'y'))
    }

    solverMakers.foreach(maker => testIt(maker(pa)))
  }
}
