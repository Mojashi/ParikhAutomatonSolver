package xyz.mojashi

import org.sosy_lab.common.ShutdownManager
import org.sosy_lab.common.configuration.Configuration
import org.sosy_lab.common.log.BasicLogManager
import org.sosy_lab.java_smt.SolverContextFactory
import org.sosy_lab.java_smt.SolverContextFactory.Solvers
import org.sosy_lab.java_smt.api.SolverContext.ProverOptions

class JavaSMTTest extends org.scalatest.funsuite.AnyFunSuiteLike {

  test("solve") {
    // Instantiate JavaSMT with SMTInterpol as backend (for dependencies cf. documentation)// Instantiate JavaSMT with SMTInterpol as backend (for dependencies cf. documentation)

    val config = Configuration.defaultConfiguration()
    val logger = BasicLogManager.create(config)
    val shutdown = ShutdownManager.create

    // SolverContext is a class wrapping a solver context.
    // Solver can be selected either using an argument or a configuration option
    // inside `config`.
    try {
      val context = SolverContextFactory.createSolverContext(config, logger, shutdown.getNotifier, Solvers.Z3)
      try {
        val imgr = context.getFormulaManager.getIntegerFormulaManager
        // Create formula "a = b" with two integer variables
        val a = imgr.makeVariable("a")
        val b = imgr.makeVariable("b")
        val g = imgr.greaterThan(a,b)
        // Solve formula, get model, and print variable assignment
        try {
          val prover = context.newProverEnvironment(ProverOptions.GENERATE_MODELS)
          try {
//            prover.addConstraint(f)
            prover.addConstraint(g)
            val isUnsat = prover.isUnsat
            assert(!isUnsat)
            try {
              val model = prover.getModel
              try System.out.printf("SAT with a = %s, b = %s", model.evaluate(a), model.evaluate(b))
              finally if (model != null) model.close()
            }
          } finally if (prover != null) prover.close()
        }
      } finally if (context != null) context.close()
    }
  }
}
