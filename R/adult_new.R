adult <- read.csv('/Users/joshyoung/Documents/GradProj/adult.csv', header=FALSE)
colnames(adult) <- c('age', 'work', 'fnl', 'education', 'edNum', 'marital',
                     'occupation', 'relationship', 'race', 'sex', 'capGain',
                     'capLoss', 'HourPW', 'nativeCountry', 'class')


# next several lines are sampling the same number of observations from each class
table(adult$class)
#random sample of 5000 observations from adult dataset
adult.class1 = adult[adult$class==" <=50K",]
adult.class2 = adult[adult$class==" >50K",]
# check dimensions
dim(adult.class1)
dim(adult.class2)

set.seed(2846)
adult.rand = rbind(adult.class1[sample(nrow(adult.class1), 200), ],
                   adult.class2[sample(nrow(adult.class2), 200), ])
adult.rand[adult.rand == " ?"] = NA
adult.rand = droplevels(adult.rand)
table(adult.rand$class)

dataset = adult.rand[, -14]

reps = 100
total_rfnew <- vector('numeric', reps)
total_nanew <- vector('numeric', reps)
total_rough <- vector('numeric', reps)
total_prox <- vector('numeric', reps)
total_micerf <- vector('numeric', reps)
total_micepmm <- vector('numeric', reps)

# the following lines can be used "as is" if the dataset is called "dataset"
set.seed(8905)
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

adult_table <- c(mean(total_rfnew), mean(total_nanew), mean(total_rough), mean(total_prox), 
                 mean(total_micerf), mean(total_micepmm))

write.csv(adult_table, '/Users/joshyoung/Documents/GradProj/Imputation/adult3.csv')
