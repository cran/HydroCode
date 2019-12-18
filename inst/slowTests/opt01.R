## check the performance of optimization
require(HydroCode)
set.seed(777)
bas <- runif(25000,
             min = 100000,
             max = 999999)

bas <- as.integer(bas) ## truncates

## get the time
system.time(
  out1 <- bas[1] %up% bas
)

## check with is.upstream
system.time(
  out2 <- is.upstream(bas[1], bas)
)
all.equal(out1, out2)

## check with downstream
system.time(
  out1 <- bas[1] %down% bas
)

system.time(
  out2 <- is.downstream(bas[1], bas)
)
all.equal(out1, out2)
