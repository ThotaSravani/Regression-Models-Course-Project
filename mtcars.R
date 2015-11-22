## Exploratory Data Analysis

library(ggplot2)
data(mtcars)
mtcars[1:3, ] # Sample Data

dim(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- factor(mtcars$am)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
attach(mtcars)

## Inference

result <- t.test(mpg ~ am)
result$p.Value
result$estimate

## Refression Analysis

mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)

##Transmission 
boxplot(mpg ~ am,data = mtcars,xlab="Transmission (0 = Automatic ,1 = Manual)")
title(main="Boxplot Of Transmission Against MPG")

## Pair Graph
pairs(mtcars,panel=panel.smooth,main="pair Graph Of Motor Trend Car Road Tests")

## Analysis of variance Model
analysis <- aov(mpg ~ ., data = mtcars)

summary(analysis)
## Predictor variables
lm <- lm(mpg ~ cyl + disp + wt + drat + am, data = mtcars)
summary(lm)
## Coefficient of draft 
ts <-lm(mpg~ cyl + disp + wt + am, data = mtcars)
summary(lm)
## Remove Variable disp
ts <- lm(mpg ~ cyl + wt + am, data = mtcars)
summary(lm)
## Diagnostic Model
par(mfrow = c(2, 2))
plot(lm)