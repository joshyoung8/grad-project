############### Function to introduce missingness completely at random ################

mcar <- function(x, pmiss){
  #calculate number of columns in dataset and make a new dataset without class variable
  colToDel <- ncol(x)
  temp <- na.omit(x)
  temp <- temp[, colToDel]
  x.new <- x[, -colToDel]
  x.new <- na.omit(x.new)
  
  insert_nas <- function(x, p=pmiss){
    #pmiss is the proportion of missing values
    len <- length(x)
    mcar <- 1 - rbinom(len, 1, p)
    x[mcar == 0] <- NA
    return (x)
  }
  
  #introduce missingness into dataset and put class variable back into dataset
  x.new <- sapply(x.new, insert_nas)
  x.new <- data.frame(x.new)
  x.new[, colToDel] <- temp
  colnames(x.new)[colToDel] <- c('class')
  return(x.new)
}