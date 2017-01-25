soybean = read.csv('/Users/joshyoung/Documents/GradProj/soybean.csv', header=TRUE)
cols = c(1:36)
soybean[,cols] = lapply(soybean[,cols], factor)
sapply(soybean, function(x)is.factor(x))

soybean[soybean=='?'] = NA
sapply(soybean, function(x) sum(is.na(x)))
sum(is.na(soybean))

dataset = droplevels(soybean)

registerDoParallel(detectCores()-1)

numworkers <- getDoParWorkers()

nrep <- 100
seed_rng <- 93450

soybean_run <- foreach(i = 1:nrep, .combine = 'cbind', .options.RNG = seed_rng) %dorng%{
  all_six_methods_mar(dataset)
}
soybean_mean <- apply(soybean_run, 1, mean)

write.csv(soybean_run, '/Users/joshyoung/Documents/GradProj/Imputation/soybean_table.csv')
write.csv(soybean_mean, '/Users/joshyoung/Documents/GradProj/Imputation/soybean3.csv')
