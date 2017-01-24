######### Get wine dataset ready ###############

wine <- read.csv('/Users/joshyoung/Documents/GradProj/wine.csv', header = FALSE)
wine.class <- wine[, 1]
wine <- wine[, -1]
wine[, 14] <- wine.class
colnames(wine)[14] <- 'class'
wine$class <- as.factor(wine$class)

################ separate groups into train and test sets and introduce missingness ############

set.seed(91972)
xvs <- sample(2, nrow(wine), replace = TRUE, prob = c(.7, .3))
train <- wine[xvs == 1, ]
test <- wine[xvs == 2, ]

# write.csv(mean_pcc, '/Users/joshyoung/Documents/GradProj/Imputation/wine3.csv')
