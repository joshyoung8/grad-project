####### Original Function where we impute a random value in the original matrix ##########

impForest = function(mat){
  
  #perform a random imputation
  random.imp = function(a){
    missing = is.na(a)
    n.missing = sum(missing)
    a.obs = a[!missing]
    imputed = a
    imputed[missing] = sample(a.obs, n.missing, replace=TRUE)
    return(imputed)
  }
  
  #Find column names with missing values
  colNA = sapply(mat, function(x) sum(is.na(x)))
  
  #set up new matrix (y matrix contains missing variables and puts them in the front of the matrix)
  yCol = names(colNA[colNA>0])
  yColLen = length(yCol)
  xCol = names(colNA[colNA==0])
  mat.y = subset(mat, select = yCol)
  mat.x = subset(mat, select = xCol)
  mat.new = cbind(mat.y, mat.x)
  mat.new = subset(mat.new, select=-which( colnames(mat.new)=="class" ))
  mat.imp = mat.new
  
  #for loop to impute data randomly
  for(i in 1:yColLen){
    mat.imp[, i] = random.imp(mat.new[, i])
  }
  rimp = cbind(mat.imp, class=mat$class)
  
  #predict and impute for missing variables
  for(i in 1:yColLen){
    rf.out = randomForest(mat.imp[, i]~., data = mat.imp)
    mat.imp[is.na(mat.new[, i]), i] = rf.out$predicted[is.na(mat.new[, i])]
  }
  
  mat.final = cbind(mat.imp, class=mat$class)
  list(RFImputed = mat.final, randomImputed = rimp)
}

######################## with na.roughfix #################################

impForestNa = function(mat){
  
  colNA = sapply(mat, function(x) sum(is.na(x)))
  
  #set up new matrix (y matrix contains missing variables and puts them in the front of the matrix)
  yCol = names(colNA[colNA>0])
  yColLen = length(yCol)
  xCol = names(colNA[colNA==0])
  mat.y = subset(mat, select = yCol)
  mat.x = subset(mat, select = xCol)
  mat.new = cbind(mat.y, mat.x)
  mat.new = subset(mat.new, select=-which( colnames(mat.new)=="class" ))
  mat.imp = na.roughfix(mat.new)
  
  #predict and impute for missing variables
  for(i in 1:yColLen){
    rf.out = randomForest(mat.imp[, i]~., data = mat.imp)
    mat.imp[is.na(mat.new[, i]), i] = rf.out$predicted[is.na(mat.new[, i])]
  }
  
  mat.final = cbind(mat.imp, class=mat$class)
  return(RFImputed = mat.final)
}
