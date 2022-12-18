## Getting started
- Add JitPack repository and dependency to your build.sbt
```
resolvers += "jitpack" at "https://jitpack.io"
libraryDependencies += "com.github.Mojashi" % "ParikhAutomatonSolver" % "0.2"	
```
- If you use other build files(maven, gradle, leiningen), follow the instructions at https://jitpack.io/#Mojashi/ParikhAutomatonSolver/0.2

- If you wish to use **Z3**, **CVC5**, **CPLEX**, **GUROBI** or other backend solvers, they must be installed separately by you.