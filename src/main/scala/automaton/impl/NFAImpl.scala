package xyz.mojashi
package automaton.impl

import automaton.{NFA, Transition}
import scala.collection.mutable
import xyz.mojashi.graph.GraphImpl

class NFAImpl[In, State, T <: Transition[Option[In], State]]
(
  override val start: State,
  override val fin: State,
  transitions: Seq[T],
)extends GraphImpl[State, T](transitions) with NFA[In, State, T] {
  override def accept(in: Seq[In]): Boolean = {
    val reached = mutable.Set[(State, Int)]()

    def dfs(pos: State, word: Seq[In]): Boolean = {
      if(pos == fin) return true
      if(reached.contains((pos, word.length))) return false

      reached.add((pos, word.length))

      sourceFrom(pos).exists(t=> {
        t.in match {
          case Some(ch) =>
            if(word.head == ch)
              dfs(t.to, word.tail)
            else false
          case None => dfs(t.to, word)
        }
      })
    }

    dfs(start, in)
  }
}
