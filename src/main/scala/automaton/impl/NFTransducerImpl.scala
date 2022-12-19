package com.github.Mojashi
package automaton.impl

import automaton.{NFTransducer, TransducerTransition}

import com.github.Mojashi.graph.StateID

import scala.collection.mutable

class NFTransducerImpl[In, Out, T <: TransducerTransition[In, Out]]
(
  start: StateID,
  fin: StateID,
  transitions: Seq[T],
) extends NFAImpl[In, T] (
  start, fin, transitions
) with NFTransducer[In, Out, T] {
  override def run(in: Seq[In]): Set[Seq[Out]] = {
    val reached = mutable.Set[(StateID, Int)]()

    def dfs(pos: StateID, word: Seq[In]): Set[Seq[Out]] = {
      if (reached.contains((pos, word.length))) return Set()
      reached.add((pos, word.length))

      (
        if (pos == fin && word.isEmpty) Set(Seq())
        else Set()
      ) ++ (
        sourceFrom(pos).flatMap(t => {
          (t.in match {
            case Some(ch) =>
              if (word.nonEmpty && word.head == ch)
                dfs(t.to, word.tail)
              else Set()
            case None => dfs(t.to, word)
          }).map(s => t.out.toSeq ++ s)
        })
      )
    }

    dfs(start, in)
  }
}
