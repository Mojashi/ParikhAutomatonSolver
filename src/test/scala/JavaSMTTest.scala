package com.github.Mojashi

import org.sosy_lab.common.ShutdownManager
import org.sosy_lab.common.configuration.Configuration
import org.sosy_lab.common.log.BasicLogManager
import org.sosy_lab.java_smt.SolverContextFactory
import org.sosy_lab.java_smt.SolverContextFactory.Solvers
import org.sosy_lab.java_smt.api.SolverContext.ProverOptions

import scala.util.Using

class JavaSMTTest extends org.scalatest.funsuite.AnyFunSuiteLike {

  test("solve") {
    val config = Configuration.defaultConfiguration()
    val logger = BasicLogManager.create(config)
    val shutdown = ShutdownManager.create

    // SolverContext is a class wrapping a solver context.
    // Solver can be selected either using an argument or a configuration option
    // inside `config`.
    Using(SolverContextFactory.createSolverContext(config, logger, shutdown.getNotifier, Solvers.Z3)) { context =>
      val imgr = context.getFormulaManager.getIntegerFormulaManager
      // Create formula "a = b" with two integer variables
      val a = imgr.makeVariable("a")
      val b = imgr.makeVariable("b")
      val g = imgr.greaterThan(a, b)
      val h = imgr.equal(b, imgr.makeNumber(2))
      // Solve formula, get model, and print variable assignment
        Using(context.newProverEnvironment(ProverOptions.GENERATE_MODELS)) { prover =>
          //            prover.addConstraint(f)
          prover.addConstraint(g)
          prover.addConstraint(imgr.equal(a, b))
          val isUnsat = prover.isUnsat
          //            assert(!isUnsat)

          println(prover.getModel)
          Using(prover.getModel) { model =>
            System.out.printf("SAT with a = %s, b = %s", model.evaluate(a), model.evaluate(b))
          }

          prover.addConstraint(h)
          val isUnsat2 = prover.isUnsat
          assert(!isUnsat2)
          Using(prover.getModel) { model =>
            System.out.printf("SAT with a = %s, b = %s", model.evaluate(a), model.evaluate(b))
          }
        }
    }
  }
}
