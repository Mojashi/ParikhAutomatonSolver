package com.github.Mojashi
package solver.mp

import automaton.ParikhAutomaton
import automaton.impl.BricsNFAAdapter.BricsNFAAdapter
import automaton.impl.ToParikhVectorTransducer.NFA_Parikh
import formula.{GTEQ, _}
import solver.ParikhAutomatonSolver
import solver.common.Implicits.IntDoubleNumericCast

import dk.brics.automaton.RegExp
import org.scalatest.Inspectors.forAll
import org.scalatest.funsuite.AnyFunSuiteLike

class OrTest extends AnyFunSuiteLike {
  com.google.ortools.Loader.loadNativeLibraries()

  val solverMakers = Seq(
    (pa: ParikhAutomaton[Char, Char, Int]) => new MIPSinglePointSolver(pa),
    (pa: ParikhAutomaton[Char, Char, Int]) => new MIPCutWithLargeMExactSolver(pa)
  )

  test("orSolve") {
    val r = new RegExp("xy+z+k*l+z(k*l+z+k*l+z+((k*l+z+k*)+aa|(l+z+k*lxy+z+k*l)+)*a+aaaaz+k*l+z+k*)*(k*l+z+k*l+z+((k*l+z+k*)+aa|(l+z+k*lxy+z+k*l)+)*a+aaaaz+k*l+z+k*)*(k*l+z+k*l+z+((k*l+z+k*)+aa|(l+z+k*lxy+z+k*l)+)*a+aaaaz+k*l+z+k*)*(k*l+z+k*l+z+((k*l+z+k*)+aa|(l+z+k*lxy+z+k*l)+)*a+aaaaz+k*l+z+k*)*l+z+k*l+z+k*l+z+k*lxy+z+k*l+z+k*l+z+k*l+z+k*l+z+k*l+z+k*l")
    val a = r.toAutomaton().toNFA.toParikhVectorTransducer

    def testIt(solver: ParikhAutomatonSolver[Char, Char, Int]): Boolean = {
      val ans = solver.solve()
      println(ans)

      ans.isDefined
    }


    val pas = Seq(
      (ParikhAutomaton(
        constraint = Or(Seq(
          EQ(Constant[Char, Int](11), Constant[Char, Int](12)),
          GTEQ(Constant[Char, Int](11), Constant[Char, Int](12)),
        )),
        voa = a
      ), false),
      (ParikhAutomaton(
        constraint = Or(Seq(
          EQ(Constant[Char, Int](11), Constant[Char, Int](12)),
          EQ(Constant[Char, Int](13), Constant[Char, Int](13)),
        )),
        voa = a
      ), true),
      (ParikhAutomaton(
        constraint = Or(Seq(
          EQ(Constant[Char, Int](11), Constant[Char, Int](12)),
          Or(Seq(
            EQ(Constant[Char, Int](15), Constant[Char, Int](13)),
            EQ(Constant[Char, Int](14), Constant[Char, Int](13)),
          ))
        )),
        voa = a
      ), false),
      (ParikhAutomaton(
        constraint = Or(Seq(
          EQ(Constant[Char, Int](32), Constant[Char, Int](16)),
          Or(Seq(
            LTEQ(Var[Char, Int]('x'), Constant[Char, Int](15)),
            LTEQ(Var[Char, Int]('x'), Constant[Char, Int](14)),
          )),
          GTEQ(Var[Char, Int]('x'), Constant[Char, Int](16)),
        )),
        voa = a
      ), true),
      (ParikhAutomaton(
        constraint = And(Seq(
          EQ(Var[Char, Int]('x'), Constant[Char, Int](16)),
          EQ(Var[Char, Int]('x'), Constant[Char, Int](15)),
        )),
        voa = a
      ), false),
      (ParikhAutomaton(
        constraint = And(Seq(
          EQ(Constant[Char, Int](16), Constant[Char, Int](16)),
          Or(Seq(
            EQ(Var[Char, Int]('x'), Constant[Char, Int](20)),
            EQ(Var[Char, Int]('x'), Constant[Char, Int](14)),
          )),
          GTEQ(Var[Char, Int]('x'), Constant[Char, Int](16)),
        )),
        voa = a
      ), true),
    )

    forAll(pas) { pa =>
      forAll(solverMakers) { maker =>
        assert(testIt(maker(pa._1)) == pa._2)
      }
    }
  }
}
