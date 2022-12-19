package com.github.Mojashi
package automaton.impl

import automaton.{NFA, Transition}

import scala.collection.mutable
import com.github.Mojashi.graph.{GraphImpl, StateID}

class NFAImpl[In, T <: Transition[In]]
(
  override val start: StateID,
  override val fin: StateID,
  transitions: Seq[T],
)extends GraphImpl[T](transitions) with NFA[In, T] {
  override def accept(in: Seq[In]): Boolean = {
    val reached = mutable.Set[(StateID, Int)]()

    def dfs(pos: StateID, word: Seq[In]): Boolean = {
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
