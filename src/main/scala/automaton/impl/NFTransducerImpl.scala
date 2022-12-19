package com.github.Mojashi
package automaton.impl

import automaton.{NFTransducer, TransducerTransition}

import scala.collection.mutable

class NFTransducerImpl[In, Out, State, T <: TransducerTransition[Option[In], Out,State]]
(
  start: State,
  fin: State,
  transitions: Seq[T],
) extends NFAImpl[In, State, T] (
  start, fin, transitions
) with NFTransducer[In, Out, State, T] {
  override def run(in: Seq[In]): Set[Seq[Out]] = {
    val reached = mutable.Set[(State, Int)]()

    def dfs(pos: State, word: Seq[In]): Set[Seq[Out]] = {
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
          }).map(s => t.out +: s)
        })
      )
    }

    dfs(start, in)
  }
}
