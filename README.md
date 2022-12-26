[![](https://jitpack.io/v/Mojashi/ParikhAutomatonSolver.svg)](https://jitpack.io/#Mojashi/ParikhAutomatonSolver)


## Installation
- Add JitPack repository and dependency to your build.sbt
```
resolvers += "jitpack" at "https://jitpack.io"
libraryDependencies += "com.github.Mojashi" % "ParikhAutomatonSolver" % "v0.5"	
```
- If you use other build files(maven, gradle, leiningen), follow the instructions at https://jitpack.io/#Mojashi/ParikhAutomatonSolver/v0.5

- If you wish to use **Z3**, **CVC5**, **CPLEX**, **GUROBI** or other backend solvers, they must be installed separately by you.

## Usage
[Example](https://github.com/Mojashi/ParikhAutomatonSolver/blob/master/src/test/scala/solver/mp/MIPExactSolverTest.scala)