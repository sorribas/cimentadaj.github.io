---
layout: post
title:  "Producing stargazer tables with odds ratios and standard errors in R"
comments: true
---

Whoa, what a day. I've been using the stargazer package for producing my (beautiful) regression tables in R for a while now. Among all the arguments of its main function (`stargazer()` ) are `apply.coef`,  `apply.se`,  `apply.ci`, … and so on for all the other statistics of a regression output. Each of these arguments, if specified, applies a function over the specified statistic. So, for calculating the odds ratios I would simply apply the `exp()` function over the set of log odds. It turns out that if you apply any function over the coefficients (or any other statistic), stargazer automatically recalculates t values with the new coefficients! This means that the significance of my model will depend on the new values and we surely wouldn't want that.


Let's show a reproducible example:


```r
# install.packages("stargazer") # in case you don't have this package
suppressMessages(library(stargazer))

m1 <- glm(mtcars$vs ~ mtcars$hp + mtcars$mpg)

stargazer(m1, type = "text") # Our standard log odds
```

```
## 
## =============================================
##                       Dependent variable:    
##                   ---------------------------
##                               vs             
## ---------------------------------------------
## hp                         -0.004**          
##                             (0.001)          
##                                              
## mpg                          0.022           
##                             (0.017)          
##                                              
## Constant                     0.566           
##                             (0.519)          
##                                              
## ---------------------------------------------
## Observations                  32             
## Log Likelihood              -11.217          
## Akaike Inf. Crit.           28.434           
## =============================================
## Note:             *p<0.1; **p<0.05; ***p<0.01
```

```r
stargazer(m1, apply.coef = exp, type = "text")
```

```
## 
## =============================================
##                       Dependent variable:    
##                   ---------------------------
##                               vs             
## ---------------------------------------------
## hp                         0.996***          
##                             (0.001)          
##                                              
## mpg                        1.022***          
##                             (0.017)          
##                                              
## Constant                   1.762***          
##                             (0.519)          
##                                              
## ---------------------------------------------
## Observations                  32             
## Log Likelihood              -11.217          
## Akaike Inf. Crit.           28.434           
## =============================================
## Note:             *p<0.1; **p<0.05; ***p<0.01
```

The coefficients are correct, but look at the significance levels! Those are some really undesirable results. I was actually using this for quite some time without noticing. In light of this problem I decided to create a small function that extracted the statistics separately and applied the appropriate conversion when needed. It's far from being a flexible function, but it can surely help you run some quick-and-dirty logistic regressions with odds ratios instead of log odds.

Here's the function and an example:


```r
stargazer2 <- function(model, odd.ratio = F, ...) {
  if(!("list" %in% class(model))) model <- list(model)
    
  if (odd.ratio) {
    coefOR2 <- lapply(model, function(x) exp(coef(x)))
    seOR2 <- lapply(model, function(x) exp(coef(x)) * summary(x)$coef[, 2])
    p2 <- lapply(model, function(x) summary(x)$coefficients[, 4])
    stargazer(model, coef = coefOR2, se = seOR2, p = p2, ...)
    
  } else {
    stargazer(model, ...)
  }
}

stargazer(m1, type = "text") # Our standard log odds
```

```
## 
## =============================================
##                       Dependent variable:    
##                   ---------------------------
##                               vs             
## ---------------------------------------------
## hp                         -0.004**          
##                             (0.001)          
##                                              
## mpg                          0.022           
##                             (0.017)          
##                                              
## Constant                     0.566           
##                             (0.519)          
##                                              
## ---------------------------------------------
## Observations                  32             
## Log Likelihood              -11.217          
## Akaike Inf. Crit.           28.434           
## =============================================
## Note:             *p<0.1; **p<0.05; ***p<0.01
```

```r
stargazer2(m1, odd.ratio = T, type = "text") 
```

```
## 
## =============================================
##                       Dependent variable:    
##                   ---------------------------
##                               vs             
## ---------------------------------------------
## hp                          0.996**          
##                             (0.001)          
##                                              
## mpg                          1.022           
##                             (0.017)          
##                                              
## Constant                     1.762           
##                             (0.915)          
##                                              
## ---------------------------------------------
## Observations                  32             
## Log Likelihood              -11.217          
## Akaike Inf. Crit.           28.434           
## =============================================
## Note:             *p<0.1; **p<0.05; ***p<0.01
```

```r
# Now the coefficients and significance is correct!
```


```r
# You can also use lists
m1 <- glm(mtcars$vs ~ mtcars$mpg)
m2 <- glm(mtcars$vs ~ mtcars$mpg + mtcars$hp)
m3 <- glm(mtcars$vs ~ mtcars$mpg + mtcars$hp + mtcars$am)

models <- list(m1, m2, m3)

stargazer(models, type = "text")
```

```
## 
## ===============================================
##                        Dependent variable:     
##                   -----------------------------
##                                vs              
##                      (1)        (2)      (3)   
## -----------------------------------------------
## mpg                0.056***    0.022    0.041* 
##                    (0.011)    (0.017)  (0.022) 
##                                                
## hp                           -0.004**  -0.003* 
##                               (0.001)  (0.002) 
##                                                
## am                                      -0.223 
##                                        (0.173) 
##                                                
## Constant          -0.678***    0.566    0.141  
##                    (0.239)    (0.519)  (0.611) 
##                                                
## -----------------------------------------------
## Observations          32        32        32   
## Log Likelihood     -14.669    -11.217  -10.299 
## Akaike Inf. Crit.   33.338    28.434    28.599 
## ===============================================
## Note:               *p<0.1; **p<0.05; ***p<0.01
```

```r
stargazer2(models, odd.ratio = T, type = "text")
```

```
## 
## ===============================================
##                        Dependent variable:     
##                   -----------------------------
##                                vs              
##                      (1)        (2)      (3)   
## -----------------------------------------------
## mpg                1.057***    1.022    1.042* 
##                    (0.012)    (0.017)  (0.023) 
##                                                
## hp                            0.996**   0.997* 
##                               (0.001)  (0.002) 
##                                                
## am                                      0.800  
##                                        (0.139) 
##                                                
## Constant           0.508***    1.762    1.151  
##                    (0.121)    (0.915)  (0.703) 
##                                                
## -----------------------------------------------
## Observations          32        32        32   
## Log Likelihood     -14.669    -11.217  -10.299 
## Akaike Inf. Crit.   33.338    28.434    28.599 
## ===============================================
## Note:               *p<0.1; **p<0.05; ***p<0.01
```

```r
# Same significance but different coefficients and SE's
```

Caveats:

* It only accepts one model or one list containing several models. I did this because I didn't want to get into distinguishing between several separate models. If you want to improve it, [here's](https://github.com/cimentadaj/cimentadaj/blob/master/R/stargazer2.R) the Github website, submit a pull request!

* It doesn't calculate confidence intervals as the formula is more complicated and I didn't need them for now.

---
Update: I included this function in my personal package which you can install like this:

```r
# install.packages("devtools")
devtools::install_github("cimentadaj/cimentadaj")
```
