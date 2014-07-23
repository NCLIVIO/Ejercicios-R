---
title: "Relationship between transmission and miles per gallon"
author: 'By: Natalia clivio'
date: "Monday, July 21, 2014"
output: html_document
---

##Executive Summary
This report sets use the work for Motor Trend, about the automobile industry. This analysis explore the relationship between the transmission and miles per gallon (MPG). They are particularly interested the following two questions:

* Is an automatic or manual transmission better for MPG?
* Quantify the MPG difference between automatic and manual transmissions?

##Analysis
The dataset is mtcars, taken from datasets library from R. It has 32 observations and 11 variables.

**1. Preliminary Exploration of the Data**  
There are 11 variables, since we are interested in the relationshp between mpg and other variables. 

```{r}
data(mtcars)
names(mtcars)
```

we first check the correlation between mpg and other variables, by means of the *cor()* function.Where mtcars[,-1] is the dataframe without mpg. 

```{r}
cor(mtcars$mpg,mtcars[,-1])
```

From the correlation data, we could see cyl, hp, wt and carb are negatively correlated with mpg.


**2. Exploratory Analysis**  
For this analysis we will use only the **mpg** and **am** variable so the data will be prepped accordingly. To assess whether the mpg is better for automatic or manual transmission, we will first perform an exploratory data analysis. Specifically, we will produce a box-whisker plot to see the pattern of the distribution and also to get an idea about the summary statistics.

```{r}
datacar<-mtcars[c("am","mpg")]
```

The box-whisker plot of mpg over transmission mechanism shows that the median mpg is lower in automatic transmission compared to its manual counterpart. Also from the plot we see that the variation in mpg in manual transmission is larger than automatic transmission. 

*See Figure 1 in Appendix

The first quartile of manual transmission is higher than the 3rd quartile of automatic transmission. The result clearly indicates both groups have a very different mpg average. This could be supported by the *t.test()* function.

```{r}
t.test(datacar$mpg~datacar$am,conf.level=0.95)
```


**3. Linear Regression**  
A linear regression is performed to assess whether there is any difference in the average mpg between groups. It´s with mpg as dependent variable and the am as independent variable.
The estimated difference is represented by the coefficient of am variable in the model along with p-value.

```{r}
lmcar<-lm(mpg~am,datacar)
coef(summary(lmcar))
```

95% confidence interval of the difference is represented by the confidence interval of the am variable

```{r}
confint(lmcar)
```

The regression coefficient indicates that the average mpg for manual transmission is 7.24 units higher compared to automatic transmissions. The confidence interval and the p-value indicate that the difference in mpg between groups is statistically different from zero.

To Quantifying how different is the MPG between automatic and manual transmissions, is usefull the residual analysis.
Here we adopt a stepwise algorithm to choose the best model by using *step()* function

```{r}
stepmodel = step(lm(data = mtcars, mpg ~ .),trace=0,steps=10000)
summary(stepmodel)
```

We got a model which includes 3 variables **wt**, **qsec** and **am**. This model captured 0.85 of total variance. 

To further optimize the model, we examine **mpg ~ wt + qsec** and controled by **am**

```{r}
carmodel <- lm(mpg~ factor(am):wt + factor(am):qsec,data=mtcars)
summary(carmodel)
```

To interpret the results, we could see this model captured 89.5% of total variance and adjusted variance is 0.879 - which is quite good. In the coefficents section, we could see when the weight increased 1000 lbs, the mpg decreased, -3.176 miles for autocars and -6.09 miles for manual cars - implicating when the weight of car increased - it might be better to choose manual cars. 

For the qsec part, when the accelaration speed droped and ¼ mile time increased 1 secs, the mpg increased 0.834 miles for automatic cars and 1.446 miles for manual cars - this implys that if the car has low accelation speed at same weight, manual cars is better for mpg. 

Finally we plot the residue and diagnostic plot for this linear model.

*See Figure 2 in Appendix


##Conclusion
Based on the analysis and statistical evidence we could say that the average mpg is higher in the manual transmission group. But this conclusion might change if we consider other variables in the analysis, because the mpg is largely determined by the interplay between weight, accelaration and tramsmission.

Therefore given the above analysis to get complete understanding, further analysis is needed to consider other variables such as weight, number of gears, etc.


##Apendix

###Figure 1

Boxplot is the compare summary statistics between two group (automatic vs manual) transmission

```{r, echo=FALSE}
boxplot(datacar$mpg~factor(datacar$am,label=c("Automatic","Manual")),
        col= "pink", main="Boxplot of MPG over transmission method",xlab="Transmission",ylab="MPG")
```


###Figure 2

The residual check and diagnostics plot

```{r, echo=FALSE}
par(mfrow=c(2, 2))
plot(carmodel)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

