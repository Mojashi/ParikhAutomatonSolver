package xyz.mojashi
package solver.mp

import com.sun.org.apache.xpath.internal.operations.Minus
import dk.brics.automaton.RegExp
import org.scalatest.funsuite.AnyFunSuiteLike
import xyz.mojashi.automaton.ParikhAutomaton
import xyz.mojashi.automaton.impl.BricsNFAAdapter.BricsNFAAdapter
import xyz.mojashi.automaton.impl.ToParikhVectorTransducer.NFA_Parikh
import xyz.mojashi.utils.{GraphToDot, showDotInBrowser}

class MIPExactSolverTest extends AnyFunSuiteLike {
  com.google.ortools.Loader.loadNativeLibraries()

  test("testSolveInput") {
    val r = new RegExp("xy+z+k*l")
    val a = r.toAutomaton().toNFA.toParikhVectorTransducer

    val pa = ParikhAutomaton(
      constraint = Seq(
        GTEQ(Var[Char, Int]('z'), Constant[Char, Int](11)),
        GTEQ(Var[Char, Int]('z'), Var[Char, Int]('k')),
        EQ(Var[Char, Int]('z'), Times(Constant[Char, Int](2), Var[Char, Int]('y'))),
      ),
      voa = a
    )

    val solver = new MIPExactSolver(pa)
    solver.setObjective(Sub(Var[Char, Int]('z'), Var[Char, Int]('k')))

    val inputOpt = solver.solveInput()
    assert(inputOpt.isDefined)

    val input = inputOpt.get
    println(s"ans: ${input.mkString}")

    assert(input.count(ch => ch == 'z') >= 11)
    assert(input.count(ch=>ch=='z') == 2 * input.count(ch=>ch == 'y'))
  }

}
