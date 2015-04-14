library(photobiology)
library(photobiologyPhy)
library(microbenchmark)

data(sun.data)
attach(sun.data)

Pfr_P_ratio(w.length, s.e.irrad)

test_Pfr_P_ratio <- function(w.length, s.irrad){
  microbenchmark(Pfr_P_ratio(w.length, s.irrad),
                 Pfr_P_ratio(w.length, s.irrad, check.spectrum=FALSE),
                 Pfr_P_ratio(w.length, s.irrad, use.cached.mult=TRUE),
                 Pfr_P_ratio(w.length, s.irrad, use.cpp.code=FALSE),
                 Pfr_P_ratio(w.length, s.irrad, check.spectrum=FALSE, use.cpp.code=FALSE),
                 Pfr_P_ratio(w.length, s.irrad, use.cached.mult=TRUE, use.cpp.code=FALSE))
}

test_Pfr_P_ratio(w.length, s.e.irrad)
