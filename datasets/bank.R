#read in the data
bank <- read.csv('/Users/joshyoung/Documents/GradProj/bank/bank-full.csv', sep = ';',  header=TRUE)
y <- bank[, 17]
bankMcar <- bank[, 1:16]

#Missing Coimpletely at Random Example
prop.m <- .08
mcar <- runif(sampleSize, min = 0, max = 1)
y.mcar = ifelse(mcar < prop.m, 0, 1)
mat <- matrix(y.mcar, nrow = 45211, ncol = 16)
mat <- structure(y.mcar, class = 'data.frame')
bankMcar[y.mcar] <- NA
sampleSize <- (ncol(bank) - 1) * nrow(bank)
del <- sample(1:sampleSize, sampleSize * .1, replace = FALSE)
bank[del]
sample(bank, sampleSize * .1, replace = FALSE)
View(cbind(y.mcar))
