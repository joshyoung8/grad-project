all_six_methods_mar <- function(dataset) {
  
  #run New randomForest
  newTry <- impForest(dataset)
  rfnew <- randomForest(class~., data = newTry$RFImputed)
  total_rfnew <- rfnew$err.rate[500, 1]
  
  #new na
  newTry <- impForestNa(dataset)
  rfnew <- randomForest(class~., data = newTry)
  total_nanew <- rfnew$err.rate[500, 1]
  
  #na.roughfix
  imp.rough <- na.roughfix(dataset)
  rfrough <- randomForest(class~., data = imp.rough)
  total_rough <- rfrough$err.rate[500, 1]

  #proximity-imputed dataset
  imp.prox <- rfImpute(class~., data=dataset, iter=5)
  rfprox <- randomForest(class~., data = imp.prox)
  total_prox <- rfprox$err.rate[500, 1]

  #mice method random forest
  imp.mice.rf <- mice(dataset, meth = 'rf')
  rfMiceRf <- randomForest(class~., data = mice::complete(imp.mice.rf))
  total_micerf <- rfMiceRf$err.rate[500, 1]
  
  #mice method pmm
  imp.mice.pmm <- mice(dataset, meth = 'pmm')
  rfMicePmm <- randomForest(class~., data = mice::complete(imp.mice.pmm))
  total_micepmm <- rfMicePmm$err.rate[500, 1]
  
  return(c(total_rfnew, total_nanew, total_rough, total_prox, total_micerf, total_micepmm))
}
