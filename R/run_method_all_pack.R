library(foreach)
library(doParallel)
library(doRNG)
library(randomForest)
library(mice)
registerDoParallel(detectCores()-1)

numworkers <- getDoParWorkers()

nrep <- 50
seed_rng <- 93450

mcar10 <- foreach(i = 1:nrep, .combine = 'cbind', .options.RNG = seed_rng) %dorng%{
  dataset <- mcar(train, .1)
  all_six_methods(dataset)
}
mcar20 <- foreach(i = 1:nrep, .combine = 'cbind', .options.RNG = seed_rng) %dopar%{
  dataset <- mcar(train, .2)
  all_six_methods(dataset)
}
mcar30 <- foreach(i = 1:nrep, .combine = 'cbind', .options.RNG = seed_rng) %dopar%{
  dataset <- mcar(train, .3)
  all_six_methods(dataset)
}
mcar40 <- foreach(i = 1:nrep, .combine = 'cbind', .options.RNG = seed_rng) %dopar%{
  dataset <- mcar(train, .4)
  all_six_methods(dataset)
}
mcar50 <- foreach(i = 1:nrep, .combine = 'cbind', .options.RNG = seed_rng) %dopar%{
  dataset <- mcar(train, .5)
  all_six_methods(dataset)
}
mcar60 <- foreach(i = 1:nrep, .combine = 'cbind', .options.RNG = seed_rng) %dopar%{
  dataset <- mcar(train, .6)
  all_six_methods(dataset)
}

mean_pcc <- rbind(
  apply(mcar10, 1, mean),
  apply(mcar20, 1, mean),
  apply(mcar30, 1, mean),
  apply(mcar40, 1, mean),
  apply(mcar50, 1, mean),
  apply(mcar60, 1, mean)
)

