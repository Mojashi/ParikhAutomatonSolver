package xyz.mojashi
package solver.mp

import scala.collection.mutable.{HashMap, Map}

class UnionFind[T]() {
  private val parent: Map[T, T] = new HashMap[T, T] {
    override def default(s: T) = {
      get(s) match {
        case Some(v) => v
        case None => put(s, s); s
      }
    }
  }

  def findRoots: Set[T] = parent.filter{case (c, p) => c == p}.keys.toSet

  val rank: Map[T, Int] = new HashMap[T, Int] {
    override def default(s: T) = {
      get(s) match {
        case Some(v) => v
        case None => put(s, 1); 1
      }
    }
  }

  /**
   * Return the parent (representant) of the equivalence class.
   * Uses path compression.
   */
  def find(s: T): T = {
    val ps = parent(s)
    if (ps == s) s else {
      val cs = find(ps)
      parent(s) = cs
      cs
    }
  }

  /**
   * Unify equivalence classes of elements.
   * Uses union by rank.
   */
  def union(x: T, y: T): Unit = {
    val cx = find(x)
    val cy = find(y)
    if (cx != cy) {
      val rx = rank(x)
      val ry = rank(y)
      if (rx > ry) parent(cy) = cx
      else if (rx < ry) parent(cx) = cy
      else {
        rank(cx) += 1
        parent(cy) = cx
      }
    }
  }

  /**
   * Enumerates the equivalence class of element x
   */
  def equivalenceClass(x: T): List[T] = {
    val px = parent(x)
    parent.keys.filter(key => parent(key: T) == px).toList
  }

}
