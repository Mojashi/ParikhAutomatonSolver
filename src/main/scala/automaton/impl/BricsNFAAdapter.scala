package xyz.mojashi
package automaton.impl

import automaton.{NFA, Transition, TransitionImpl}

import dk.brics.automaton.Automaton
import graph.UniqueEdgeId

import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`

  package object BricsNFAAdapter {
    implicit class BricsNFAAdapter(automaton: Automaton){
      def toNFA(): NFA[Char, Int, Transition[Option[Char], Int]] = {
        val stateId = automaton.getStates.zipWithIndex.toMap
        val fin = automaton.getStates.size()

        new NFAImpl(
          start = stateId(automaton.getInitialState),
          fin = fin,
          transitions = automaton.getStates.flatMap(s => s.getTransitions.flatMap(t =>
            (t.getMin to t.getMax).map(ch =>
              TransitionImpl[Option[Char], Int](
                in = Some(ch),
                from = stateId(s),
                to = stateId(t.getDest),
                id = UniqueEdgeId.get
              )
            )
          )).toSeq ++
            automaton.getAcceptStates.map(s =>
              TransitionImpl(
                in = None,
                from = stateId(s),
                to = fin,
                id = UniqueEdgeId.get
              )
            )
        )
      }
    }
  }

