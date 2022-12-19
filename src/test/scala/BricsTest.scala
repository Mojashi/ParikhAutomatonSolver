package com.github.Mojashi

import dk.brics.automaton.{Automaton, RegExp}
import com.github.Mojashi.automaton.impl.BricsNFAAdapter.BricsNFAAdapter
import com.github.Mojashi.utils.{NFAToDot, showDotInBrowser}

class BricsTest extends org.scalatest.funsuite.AnyFunSuiteLike {

  test("regexp") {
    val r = new RegExp("xy+z")
    val a = r.toAutomaton(true)
    val a2 = a.toNFA

    showDotInBrowser(a2.toDot)

    assert(a2.accept("xyyyyyz".toSeq))
    assert(a2.accept("xyz".toSeq))
    assert(!a2.accept("xz".toSeq))
  }
}
