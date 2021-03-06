---
layout: post
title:  "T-tests, regression (and ANOVA): They're all the same!"
comments: true
---

Upon reading "How Not To Lie with Statistics: Avoiding Common Mistakes in Quantitative Political Science" from Gary King, I stumbled into a section which proved that t-tests, ANOVA and Linear Regression are intimately related, both conceptually and algebraically. As a late-comer in statistics, one usually does not pay attention to these nuances. I decided to make a short simulation in R just to make sure my intuition was right.


```r
set.seed(1)

income <- sample(1000:5000, 100, replace = T)
gender <- rep(c(1, 0), 50)
```


```r
t <- t.test(income ~ gender)
unname(t$estimate[2] - t$estimate[1])
```

```
## [1] 201.46
```


```r
coef(model <- lm(income ~ gender))
```

```
## (Intercept)      gender 
##     2970.64      201.46
```


```r
# The ANOVA model is actually computed through the lm
# call but we can use the anova() function to check if
# the differences are significant as well.

anova(model)
```

```
## Analysis of Variance Table
## 
## Response: income
##           Df    Sum Sq Mean Sq F value Pr(>F)
## gender     1   1014653 1014653  0.8842 0.3494
## Residuals 98 112463658 1147588
```

It's fun to find out about these things and prove that they make sense. However, the strength of linear models is that you can ‘adjust' for other important variables and get an adjusted estimated difference. Let's add another variable called kids with the number of children per person and see how the difference changes.


```r
kids <- sample(1:4, 100, replace = T)
lm(income ~ gender + kids)
```

```
## 
## Call:
## lm(formula = income ~ gender + kids)
## 
## Coefficients:
## (Intercept)       gender         kids  
##    2945.911      200.471        9.891
```

Well, we can now say that the difference in income between male and females is of about 200 adjusted for the number of children per person.
