package xyz.mojashi
package solver

import utils.{addVector, mulVector, subVector}

package object mp {
  def getCoefficients[Label, Value: Numeric]
    (expression: Expression[Label, Value]): (Map[Label, Value], Value) = {
    val m = implicitly[Numeric[Value]]
    expression match {
      case Add(left, right) =>
        val (l, lc) = getCoefficients(left)
        val (r, rc) = getCoefficients(right)
        (addVector(l,r), m.plus(lc, rc))
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
}