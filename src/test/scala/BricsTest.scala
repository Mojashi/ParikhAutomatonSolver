package xyz.mojashi

import dk.brics.automaton.{Automaton, RegExp}
import xyz.mojashi.automaton.impl.BricsNFAAdapter.BricsNFAAdapter
import xyz.mojashi.utils.{NFAToDot, showDotInBrowser}

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
