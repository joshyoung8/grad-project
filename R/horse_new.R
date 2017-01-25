horse <- read.table("/Users/joshyoung/Documents/GradProj/horse.txt"
                    , header=TRUE)
horseN = horse[,-3]
cols = c(1, 2, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 20, 22, 23, 27)
horseN[,cols] = lapply(horseN[,cols], factor)
sapply(horseN, function(x)is.factor(x))

horseN[, 3] = as.numeric(as.character(horseN[, 3]))
horseN[, 4] = as.numeric(as.character(horseN[, 4]))
horseN[, 5] = as.numeric(as.character(horseN[, 5]))
horseN[, 15] = as.numeric(as.character(horseN[, 15]))
horseN[, 18] = as.numeric(as.character(horseN[, 18]))
horseN[, 19] = as.numeric(as.character(horseN[, 19]))
horseN[, 21] = as.numeric(as.character(horseN[, 21]))
horseN[, 22] <- as.numeric(horseN[, 22])

#change to factor of 1 or 0 and impute missing data
horseN[, 22] <- sapply(horseN[, 22], function(x) ifelse(x==2, 1, 0))
horseN[, 22] <- as.factor(horseN[, 22])

horseN[horseN == "?"] = NA
sapply(horseN, function (x) sum(is.na(x)))
sum(is.na(horseN))
dataset = droplevels(horseN)

reps = 100
total_rfnew <- vector('numeric', reps)
total_nanew <- vector('numeric', reps)
total_rough <- vector('numeric', reps)
total_prox <- vector('numeric', reps)
total_micerf <- vector('numeric', reps)
total_micepmm <- vector('numeric', reps)

# the following lines can be used "as is" if the dataset is called "dataset"
set.seed(14082)
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

horse_table <- c(mean(total_rfnew), mean(total_nanew), mean(total_rough), mean(total_prox), 
               mean(total_micerf), mean(total_micepmm))
horse_data <- cbind(total_rfnew, total_nanew, total_rough, total_prox, total_micerf, total_micepmm)
apply(horse_data, 2, sd)
write.csv(horse_table, '/Users/joshyoung/Documents/GradProj/Imputation/horse3.csv')
write.csv(horse_data, '/Users/joshyoung/Documents/GradProj/Imputation/horse_data.csv')
