package com.github.Mojashi
package automaton.impl

import automaton.{NFA, TransducerTransitionImpl, Transition}

object ToParikhVectorTransducer {
  implicit class NFA_Parikh[In, T <: Transition[In]](nfa: NFA[In, T]) {
    def toParikhVectorTransducer: VectorOutputTransducerImpl[In, In, Int] = {
      new VectorOutputTransducerImpl[In, In, Int](
        start = nfa.start,
        fin = nfa.fin,
        transitions = nfa.transitions.map(t =>
          TransducerTransitionImpl(
            from = t.from,
            to = t.to,
            in = t.in,
            out = t.in.flatMap(ch => Some(Map((ch, 1)))),
            id = t.id,
          )
        )
      )
    }
  }

}