package xyz.mojashi

import com.google.ortools.linearsolver.MPSolver


import com.sun.jna.Platform
import scala.reflect.io.File

class ORToolsTest extends org.scalatest.funsuite.AnyFunSuiteLike {

  test("solve") {
    com.google.ortools.Loader.loadNativeLibraries()

    val solver = MPSolver.createSolver("SCIP")
    val cons1 = solver.makeConstraint(1.0, 10.0, "")
    val v1 = solver.makeVar(-MPSolver.infinity(), MPSolver.infinity(), true, "v1")
    cons1.setCoefficient(v1, 3)

    val cons2 = solver.makeConstraint(-10.0, 10.0, "")
    cons2.setCoefficient(v1, 3)

    solver.objective().setCoefficient(v1, 1)
    solver.objective().setMinimization()

    solver.solve()
    println(solver.objective().value())
    println(v1.solutionValue())
    println(cons1.name())
    println(cons2.name())

    cons1.setBounds(-MPSolver.infinity(), MPSolver.infinity())
    cons1.delete()

    solver.solve()
    println(solver.objective().value())
    println(v1.solutionValue())

    println(MPSolver.infinity())
    println(MPSolver.infinity() + 12)
    println(-MPSolver.infinity())
    println(-MPSolver.infinity() - 12)
  }
}
