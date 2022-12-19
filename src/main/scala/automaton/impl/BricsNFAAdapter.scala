package com.github.Mojashi
package automaton.impl

import automaton.{NFA, Transition, TransitionImpl}

import dk.brics.automaton.Automaton
import graph.UniqueEdgeId

import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`

  package object BricsNFAAdapter {
    implicit class BricsNFAAdapter(automaton: Automaton){
      def toNFA(): NFA[Char, Transition[Char]] = {
        val stateId = automaton.getStates.zipWithIndex.toMap
        val fin = automaton.getStates.size()

        new NFAImpl(
          start = stateId(automaton.getInitialState).toString,
          fin = fin.toString,
          transitions = automaton.getStates.flatMap(s => s.getTransitions.flatMap(t =>
            (t.getMin to t.getMax).map(ch =>
              TransitionImpl[Char](
                in = Some(ch),
                from = stateId(s).toString,
                to = stateId(t.getDest).toString,
                id = UniqueEdgeId.get
              )
            )
          )).toSeq ++
            automaton.getAcceptStates.map(s =>
              TransitionImpl(
                in = None,
                from = stateId(s).toString,
                to = fin.toString,
                id = UniqueEdgeId.get
              )
            )
        )
      }
    }
  }

