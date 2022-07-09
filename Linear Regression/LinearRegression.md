Implementing Linear Regression from Scratch
================
Sanjaya J Shetty
2022-06-29

## Theory

### The Formula for Linear Regression:

#### 

![ y = a + bx ](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%20y%20%3D%20a%20%2B%20bx%20 " y = a + bx ")

where,

![a = intercept](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;a%20%3D%20intercept "a = intercept")

![b = slope](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;b%20%3D%20slope "b = slope")

![x = independent variable](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x%20%3D%20independent%20variable "x = independent variable")

![y = output](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y%20%3D%20output "y = output")

So how do we calculate `a` and `b` ?

![ b = \\frac{\\sum{(X_i - \\overline{X}) \* ( Y_i - \\overline{y})}} {\\sum{X_i - \\overline{X}}} ](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%20b%20%3D%20%5Cfrac%7B%5Csum%7B%28X_i%20-%20%5Coverline%7BX%7D%29%20%2A%20%28%20Y_i%20-%20%5Coverline%7By%7D%29%7D%7D%20%7B%5Csum%7BX_i%20-%20%5Coverline%7BX%7D%7D%7D%20 " b = \frac{\sum{(X_i - \overline{X}) * ( Y_i - \overline{y})}} {\sum{X_i - \overline{X}}} ")

![ a = \\overline{y} - b \* \\overline{x} ](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%20a%20%3D%20%5Coverline%7By%7D%20-%20b%20%2A%20%5Coverline%7Bx%7D%20 " a = \overline{y} - b * \overline{x} ")

## Implementing the formulaes

### Function to calculate the slope `b`

``` r
calcB = function(x,y){ 
  #' calculates the slope `b` for the equation `a+bx`
  #' The slope is calculated using the formula
  #' 
  #' math::
  #' $$ b = \frac{\sum{(X_i - \overline{X}) * ( Y_i - \overline{y})}} {\sum{X_i - \overline{X}}} $$
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
```

### Function to calculate the intercept `a`

``` r
calcA = function(x,y){
  #' Calculates intercept `b` for the equation `a+bx`
  #' The intercept is calculated using the formula
  #' 
  #' math::
  #' $$ a = \overline{y} - b * \overline{x} $$
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
```

### Function to fit a linear regression line

``` r
linearRegression = function(x,y){
  
  values = calcA(x,y)
  
  # using `<<-` so that the parameters would be available for `predictlm` Function
  
  a <<- values['a']
  b <<- values['b']
  
  fittedValues = a + (b * x)
  
  return(fittedValues)
}
```

### Function to predict the values for the fitted model

``` r
predictlm = function(x){
  predicted = a + (b* x)
  return(predicted)
}
```

### comparing the results

``` r
values = linearRegression(iris$Sepal.Length, iris$Sepal.Width)

fittedValue = lm(iris$Sepal.Width~iris$Sepal.Length)$fitted

plot(iris$Sepal.Length, iris$Sepal.Width,
     xlab = "Sepal Length", ylab = "Sepal Width")
lines(values , col="green")
lines(fittedValue, col  = 'red')
legend("topleft",legend=c("Our Model", "lm Model"),
       col=c("green", "red"), lty=1, cex=0.7, horiz=TRUE)
```

![](LinearRegression_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
