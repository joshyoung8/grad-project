########## Get ion dataset ready ################

ion <- read.csv('/Users/joshyoung/Documents/GradProj/ion.csv', header = FALSE)
colnames(ion)[35] <- c('class')
ion <- ion[, -(1:2)]

################ separate groups into train and test sets and introduce missingness ############

set.seed(2670)
xvs <- sample(2, nrow(ion), replace = TRUE, prob = c(.5, .5))
train <- ion[xvs == 1, ]
test <- ion[xvs == 2, ]

# write.csv(mean_pcc, '/Users/joshyoung/Documents/GradProj/Imputation/ion3.csv')
