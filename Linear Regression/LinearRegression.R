#' ---
#' title: "Implementing Linear Regression from Scratch"
#' author: Sanjaya J Shetty
#' date: '2022-06-29'
#' output: github_document
#' ---
#' 
#' ## Theory
#' 
#' ### The Formula for Linear Regression:
#' 
#' ####  $$ y = a + bx $$

#' where, 
#' 
#' $a = intercept$
#' 
#' $b = slope$
#' 
#' $x = independent variable$
#' 
#' $y = output$

#'
#' So how do we calculate `a` and `b` ?
#'
#' $$ b = \frac{\sum{(X_i - \overline{X}) * ( Y_i - \overline{y})}} {\sum{X_i - \overline{X}}} $$
#'
#' $$ a = \overline{y} - b * \overline{x} $$
#'
#' ## Implementing the formulaes 
#' 
#' ### Function to calculate the slope `b`

calcB = function(x,y){ 
  #' calculates the slope `b` for the equation `a+bx`
  #' 
  #' @param x the dependent variable
  #' @param y the independent variable
  #' 
  #' @returns A named vector, which includes slope, mean of X and Y variables
  #' 
  meanX = mean(x)
  meanY = mean(y)

  nemA = sum((x - meanX)*(y - meanY))
  denA = sum((x - meanY)^2)
  
  b = nemA/denA
  
  values = c(b, meanX, meanY)
  names(values) = c('b', 'meanX', 'meanY')
  return(values)
}

#' ### Function to calculate the intercept `a`

calcA = function(x,y){
  #' Calculates intercept `b` for the equation `a+bx`
  #' 
  #' @param x the dependent variable
  #' @param y the independent variable
  #' 
  #' @returns A named vector, which includes slope and intercept
  
  reqValues = calcB(x,y)
  
  b = reqValues['b']
  a = reqValues['meanY'] - (b * reqValues['meanX'])
  
  coeff = c(a,b)
  names(coeff) = c('a', 'b')
  return(coeff)
}

#' ### Function to fit a linear regression line

linearRegression = function(x,y){
  #' Fitting Linear Regression
  #' 
  #' @param x the dependent variable
  #' @param y the independent variable
  #' 
  #' @return The fitted values
  
  values = calcA(x,y)
  
  # using `<<-` so that the parameters would be available for `predictlm` Function
  
  a <<- values['a']
  b <<- values['b']
  
  fittedValues = a + (b * x)
  
  return(fittedValues)
}

#' ### Function to predict the values for the fitted model

predictlm = function(x){
  #' Function to predict the value
  #' 
  #' @param x the dependent variable
  #' 
  #' @return The predicted values
  #' 
  
  predicted = a + (b* x)
  return(predicted)
}

#' ### comparing the results
#' 
values = linearRegression(iris$Sepal.Length, iris$Sepal.Width)

fittedValue = lm(iris$Sepal.Width~iris$Sepal.Length)$fitted

plot(iris$Sepal.Length, iris$Sepal.Width,
     xlab = "Sepal Length", ylab = "Sepal Width")
lines(values , col="green")
lines(fittedValue, col  = 'red')
legend("topleft",legend=c("Our Model", "lm Model"),
       col=c("green", "red"), lty=1, cex=0.7, horiz=TRUE)


