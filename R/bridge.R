bridge <- read.csv('/Users/joshyoung/Documents/GradProj/imputation/bridge.csv', header=FALSE)
bridge[, 6] <- as.numeric(as.character(bridge[, 6]))
#bridge[, 7] <- as.numeric(as.character(bridge[, 7]))
bridge[bridge == "?"] = NA
sapply(bridge, function(x) sum(is.na(x)))
check <- is.na(bridge[, 13])
bridge_new <- bridge[check == FALSE, ]
fact_col <- c(1, 2, 3, 5, 7, 8, 9, 10, 11, 12, 13)
bridge_new[,fact_col] = lapply(bridge_new[,fact_col], factor)
colnames(bridge_new)[13] <- 'class'
sapply(bridge_new, function(x) is.numeric(x))
sapply(bridge_new, function(x) is.factor(x))
dataset <- droplevels(bridge_new[, -c(1, 3)])


reps = 100
total_rfnew <- vector('numeric', reps)
total_nanew <- vector('numeric', reps)
total_rough <- vector('numeric', reps)
total_prox <- vector('numeric', reps)
total_micerf <- vector('numeric', reps)
total_micepmm <- vector('numeric', reps)

# the following lines can be used "as is" if the dataset is called "dataset"
set.seed(89699)
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

bridge_table <- c(mean(total_rfnew), mean(total_nanew), mean(total_rough), mean(total_prox), 
                 mean(total_micerf), mean(total_micepmm))

# write.csv(bridge_table, '/Users/joshyoung/Documents/GradProj/Imputation/bridge3.csv')
