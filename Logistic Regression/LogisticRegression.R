#' ---
#' title: "Implementing Logistic Regression from Scratch"
#' author: Sanjaya J Shetty
#' date: '2022-06-29'
#' output: github_document
#' ---
#' 
#' 

#' ### theory part
#' 


#' ### Implementation of Logistic Regression 
#' 


sigmoid <- function(theta, X){
  
  z = X%*%theta
  yhat = 1/1+exp(-z)
  
  return(yhat)
}


costFunction <- function(y, yhat){

  cost = mean((-y *log(yhat)+ (1-y)*(log(1-yhat))))
  
  return(cost)
}

gradient = function(X,y,yhat){
  
  grad = (t(X) %*% (yhat - y))/length(y)
  
  return(grad)
}


logisticRegression <- function(X,y, lr = 0.1, iters = 1000){
  m = length(y)
  theta = matrix(rep(0, m), nrow = m)
  
  for(i in 1:iters){
    
    y_sig = sigmoid(theta,X)
    
    costs = costFunction(y, y_sig)
    
    grads = gradient(X,y, y_sig)
    
    theta = theta - (lr*grads)
    
  }
  
  weight <<- theta
  
  print("Logistic Model is fitted successfully")
}

predictLogistic <- function(X, threshold = 0.5){
  
  y_pred = sigmoid(theta, X)
  
  y_predNew = ifelse(y_pred >= threshold, 1, 0)
  
  return(y_pred)
}




