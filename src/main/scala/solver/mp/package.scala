package com.github.Mojashi
package solver


import com.github.Mojashi.automaton.{NFA, Transition}
import com.github.Mojashi.formula.{Add, Constant, Expression, Sub, Times, Var}
import com.github.Mojashi.graph.{Edge, EdgeID}
import com.github.Mojashi.utils.{subVector, addVector, mulVector}

package object mp {
  def getCoefficients[Label, Value: Numeric]
    (expression: Expression[Label, Value]): (Map[Label, Value], Value) = {
    val m = implicitly[Numeric[Value]]
    expression match {
      case Add(terms) =>
        terms.map(t=>getCoefficients(t))
          .fold((Map[Label, Value](), m.zero))((l,r) => (addVector(l._1, r._1), m.plus(l._2, r._2)))
      case Sub(left, right) =>
        val (l, lc) = getCoefficients(left)
        val (r, rc) = getCoefficients(right)
        (subVector(l, r)  , m.minus(lc, rc))
      case Times(constant, term) =>
        val (r, rc) = getCoefficients(term)
        (mulVector[Label, Value](constant.v, r), m.times(constant.v, rc))
      case Var(v) => (Map((v, m.one)), m.zero)
      case Constant(v) => (Map(), v)
    }
  }

  def getInputFromNEU[In](nfa: NFA[In, Transition[In]], neu: Map[EdgeID, Double]): Seq[In] = {
    graph.getEulerTrail(
      nfa,
      neu.map { case (e, c) =>
        assert(Math.abs(Math.round(c) - c) < 0.01)
        (e, Math.round(c).toInt)
    }).flatMap(t => t.in)
  }

}

object MPSolvers {}