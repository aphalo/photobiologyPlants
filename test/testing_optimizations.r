library(photobiology)
library(photobiologyPhy)
library(microbenchmark)

data(sun.data)
attach(sun.data)

Pr_P_ratio(w.length, s.e.irrad)

test_Pr_P_ratio <- function(w.length, s.irrad){
  microbenchmark(Pr_P_ratio(w.length, s.irrad),
                 Pr_P_ratio(w.length, s.irrad, check.spectrum=FALSE),
                 Pr_P_ratio(w.length, s.irrad, use.cached.mult=TRUE),
                 Pr_P_ratio(w.length, s.irrad, use.cpp.code=FALSE),
                 Pr_P_ratio(w.length, s.irrad, check.spectrum=FALSE, use.cpp.code=FALSE),
                 Pr_P_ratio(w.length, s.irrad, use.cached.mult=TRUE, use.cpp.code=FALSE))
}

test_Pr_P_ratio(w.length, s.e.irrad)
