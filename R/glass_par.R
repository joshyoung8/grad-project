################ Glass Dataset ####################

glass <- read.csv('/Users/joshyoung/Documents/GradProj/glass.csv', header = FALSE)
colnames(glass) <- c('id', 'ri', 'na', 'mg', 'al', 'si', 'k', 'ca', 'ba', 'fe', 'class')
glass <- glass[, -1]
glass[, 10] <- as.factor(glass[, 10])

################ separate groups into train and test sets and introduce missingness ############
set.seed(1927)
xvs <- sample(2, nrow(glass), replace = TRUE, prob = c(.8, .2))
train <- glass[xvs == 1, ]
test <- glass[xvs == 2, ]

# write.csv(mean_pcc, '/Users/joshyoung/Documents/GradProj/Imputation/glass3.csv')



