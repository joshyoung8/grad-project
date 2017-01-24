all_six_methods <- function(dataset) {
  
  #run New randomForest
  newTry <- impForest(dataset)
  rfnew <- randomForest(class~., data = newTry$RFImputed)
  xt <- table(test$class, predict(rfnew, test, type='response'))
  total_rfnew <- 100*sum(diag(xt))/sum(xt)
  
  #new na
  newTry <- impForestNa(dataset)
  rfnew <- randomForest(class~., data = newTry)
  xt <- table(test$class, predict(rfnew, test, type='response'))
  total_nanew <- 100*sum(diag(xt))/sum(xt)
  
  #na.roughfix
  imp.rough <- na.roughfix(dataset)
  rfrough <- randomForest(class~., data = imp.rough)
  xt <- table(test$class, predict(rfrough, test, type='response'))
  total_rough <- 100*sum(diag(xt))/sum(xt)

  #proximity-imputed dataset
  imp.prox <- rfImpute(class~., data=dataset, iter=5)
  rfprox <- randomForest(class~., data = imp.prox)
  xt <- table(test$class, predict(rfprox, test, type='response'))
  total_prox <- 100*sum(diag(xt))/sum(xt)

  #mice method random forest
  imp.mice.rf <- mice(dataset, meth = 'rf')
  rfMiceRf <- randomForest(class~., data = mice::complete(imp.mice.rf))
  xt <- table(test$class, predict(rfMiceRf, test, type='response'))
  total_micerf <- 100*sum(diag(xt))/sum(xt)
  
  #mice method pmm
  imp.mice.pmm <- mice(dataset, meth = 'pmm')
  rfMicePmm <- randomForest(class~., data = mice::complete(imp.mice.pmm))
  xt <- table(test$class, predict(rfMicePmm, test, type='response'))
  total_micepmm <- 100*sum(diag(xt))/sum(xt)
  
  return(c(total_rfnew, total_nanew, total_rough, total_prox, total_micerf, total_micepmm))
}
