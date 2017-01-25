hep = read.csv("/Users/joshyoung/Documents/GradProj/hepatitis.csv", header=TRUE)

#turn columns to factors
cols = c(1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 20)
hep[,cols] = lapply(hep[,cols], factor)
sapply(hep, function(x)is.factor(x))

#turn columns to numeric
hep[, 2] = as.numeric(as.character((hep[, 2])))
hep[, 15] = as.numeric(as.character(hep[, 15]))
hep[, 16] = as.numeric(as.character(hep[, 16]))
hep[, 17] = as.numeric(as.character(hep[, 17]))
hep[, 18] = as.numeric(as.character(hep[, 18]))
hep[, 19] = as.numeric(as.character(hep[, 19]))
sapply(hep, function(x)is.numeric(x))

hep[hep=='?'] <- NA
sapply(hep, function(x) sum(is.na(x)))

dataset = droplevels(hep)

reps = 100
total_rfnew <- vector('numeric', reps)
total_nanew <- vector('numeric', reps)
total_rough <- vector('numeric', reps)
total_prox <- vector('numeric', reps)
total_micerf <- vector('numeric', reps)
total_micepmm <- vector('numeric', reps)

# the following lines can be used "as is" if the dataset is called "dataset"
set.seed(3780)
for (j in 1:reps){
  
  #run New randomForest
  newTry <- impForest(dataset)
  rfnew <- randomForest(class~., data = newTry$RFImputed)
  total_rfnew[j] <- rfnew$err.rate[500, 1]
  
  #randomly Imputed Dataset
  newNa <- impForestNa(dataset)
  rfnewNa <- randomForest(class~., data = newNa)
  total_nanew[j] <- rfnewNa$err.rate[500, 1]
  
  #na.roughfix
  imp.rough <- na.roughfix(dataset)
  rfrough <- randomForest(class~., data = imp.rough)
  total_rough[j] <- rfrough$err.rate[500, 1]
  
  #proximity-imputed dataset
  imp.prox <- rfImpute(class~., data=dataset, iter=5)
  rfprox <- randomForest(class~., data = imp.prox)
  total_prox[j] <- rfprox$err.rate[500, 1]
  
  #mice method random forest
  imp.mice.rf <- mice(dataset, meth = 'rf')
  rfMiceRf <- randomForest(class~., data = complete(imp.mice.rf))
  total_micerf[j] <- rfMiceRf$err.rate[500, 1]
  
  #mice method pmm
  imp.mice.pmm <- mice(dataset, meth = 'pmm')
  rfMicePmm <- randomForest(class~., data = complete(imp.mice.pmm))
  total_micepmm[j] <- rfMicePmm$err.rate[500, 1]
  
  print(j)
}

hep_table <- c(mean(total_rfnew), mean(total_nanew), mean(total_rough), mean(total_prox), 
               mean(total_micerf), mean(total_micepmm))
hep_data <- cbind(total_rfnew, total_nanew, total_rough, total_prox, total_micerf, total_micepmm)
hep_data <- read.csv('/Users/joshyoung/Documents/GradProj/Imputation/hep_data.csv', header = TRUE)
hep_data <- hep_data[, -1]
apply(hep_data, 2, sd)
write.csv(hep_table, '/Users/joshyoung/Documents/GradProj/Imputation/hep3.csv')
#write.csv(hep_data, '/Users/joshyoung/Documents/GradProj/Imputation/hep_data.csv')
