package com.github.Mojashi
package solver.mp

sealed abstract class ORToolsMIPSolver(val name: String, val supportInt: Boolean)
object ORToolsMIPSolver {
  case object CLP extends ORToolsMIPSolver(name = "CLP", supportInt = false)

  case object CBC extends ORToolsMIPSolver(name = "CBC", supportInt = true)

  case object GLOP extends ORToolsMIPSolver(name = "GLOP", supportInt = false)

  case object BOP extends ORToolsMIPSolver(name = "BOP", supportInt = true)

  case object CP_SAT extends ORToolsMIPSolver(name = "CP_SAT", supportInt = true)

  case object SCIP extends ORToolsMIPSolver(name = "SCIP", supportInt = true)

  case object GUROBI_LP extends ORToolsMIPSolver(name = "GUROBI_LP", supportInt = false)

  case object GUROBI_MIP extends ORToolsMIPSolver(name = "GUROBI_MIP", supportInt = true)

  case object CPLEX_LP extends ORToolsMIPSolver(name = "CPLEX_LP", supportInt = false)

  case object CPLEX_MIP extends ORToolsMIPSolver(name = "CPLEX_MIP", supportInt = true)

  case object XPRESS_LP extends ORToolsMIPSolver(name = "XPRESS_LP", supportInt = false)

  case object XPRESS_MIP extends ORToolsMIPSolver(name = "XPRESS_MIP", supportInt = true)

  case object GLPK_LP extends ORToolsMIPSolver(name = "GLPK_LP", supportInt = false)

  case object GLPK_MIP extends ORToolsMIPSolver(name = "GLPK_MIP", supportInt = true)
}