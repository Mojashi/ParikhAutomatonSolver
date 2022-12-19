package com.github.Mojashi

import graph.{Edge, Graph}

import com.github.Mojashi.automaton.{NFA, Transition}

import java.awt.Desktop
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.{Base64, Date}
import scala.collection.immutable.Map
import scala.sys.process.{Process, ProcessIO}

object utils {

  def addVector[Key, Value: Numeric](l: Map[Key, Value], r: Map[Key, Value])
                                    (implicit m: Numeric[Value]): Map[Key, Value] = {
    (l.keys ++ r.keys).map(key =>
      (key, m.plus(l.getOrElse(key, m.zero), r.getOrElse(key, m.zero)))
    ).toMap
  }

  def subVector[Key, Value: Numeric](l: Map[Key, Value], r: Map[Key, Value])
                                    (implicit m: Numeric[Value]): Map[Key, Value] = {
    (l.keys ++ r.keys).map(key =>
      (key, m.minus(l.getOrElse(key, m.zero), r.getOrElse(key, m.zero)))
    ).toMap
  }

  def mulVector[Key, Value: Numeric](constant: Value, r: Map[Key, Value])
                                    (implicit m: Numeric[Value]): Map[Key, Value] = {
    r.map { case (key, value) =>
      (key, m.times(constant, value))
    }
  }

  def dotToSVG(dot: String): Path = {
    val fileName = "/tmp/dot"
    Files.write(Paths.get(fileName), dot.getBytes)
    Files.write(
      Paths.get(s"/tmp/${new Date().toString}.svg"),
      Process(s"dot -Kdot -Tsvg $fileName").!!.getBytes
    )
  }

  def showDotInBrowser(dot: String) = {
    val svgFilePath = dotToSVG(dot)
    if (Desktop.isDesktopSupported && Desktop.getDesktop.isSupported(Desktop.Action.BROWSE)) {
      Desktop.getDesktop.open(svgFilePath.toFile)
    }

  }

  implicit class GraphToDot[State](g: Graph[State, Edge[State]]) {
    def toDot = {
      s"digraph {\nrankdir=LR;\n" + {
        g.transitions.map { e => s"\"${e.from}\" -> \"${e.to}\" [label=\"${e}\", style = solid ];" }.mkString("\n")
      } + "nodesep=\"1\";}"
    }
  }

  implicit class NFAToDot[In, State](g: NFA[In, State, Transition[Option[In], State]]) {
    def toDot = {
      s"digraph {\nrankdir=LR;\n" + {
        "\n superstart[shape = point ];\n" + s"superstart->\"${g.start}\"\n" + s"\"${g.fin}\" [shape=doublecircle];\n" +
        g.transitions.map { e => s"\"${e.from}\" -> \"${e.to}\" [label=\"$e\", style = solid ];" }.mkString("\n")
      } + "nodesep=\"1\";}"
    }
  }

}