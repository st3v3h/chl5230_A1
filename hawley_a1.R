# Course: CHL5230H - Applied Machine Learning for Health Data
# Term: Fall 2019
# Assignment 1
# Author: Steve Hawley

library(tidyverse)

#import the data
bc_data <- read.csv("bc_data-1.csv", header = F)

#look at the structure
str(bc_data)

#rename the columns to something useful
colnames(bc_data) <- c("ID","outcome","time","radius_m","texture_m","perimeter_m","area_m","smoothness_m","compactness_m","concavity_m","conc_points_m","symmetry_m","fractal_m","radius_se","texture_se","perimeter_se","area_se","smoothness_se","compactness_se","concavity_se","conc_points_se","symmetry_se","fractal_se","radius_w","texture_w","perimeter_w","area_w","smoothness_w","compactness_w","concavity_w","conc_points_w","symmetry_w","fractal_w","size","ln_status")

#clean missing data
bc_data[bc_data == "?"] <- NA

#clean up ln_status
bc_data$ln_status <- factor(ifelse(bc_data$ln_status %in% 0, "0",
                                   ifelse(bc_data$ln_status %in% 1:3, "1-3", "4 or more")),
                                   levels=c("0","1-3","4 or more"))

#subset the data for only those with recurrence. Also dropping the standard error and worst measurement columns
bc_data.rec <- bc_data %>% filter(outcome=="R") %>% select(-matches("_se|_w"))

#summarize new data set
summary(bc_data.rec)

#loop through the predictor columns and create boxplots for each mean
invisible(lapply(colnames(bc_data.rec[3:14]),function(x){
  boxplot(bc_data.rec[,x],main=x,type="l")
}))

plot(bc_data.rec$ln_status, xlab="Lymph node status")

#define predictors
predictors <- colnames(bc_data.rec[4:15])

library(glmnet)

x <- model.matrix(time ~.,bc_data.rec)[,-1] #need to recode factors to dummy variables "~." = take everything after weight
# we excluded the first column as it corresponds to the intercept and it is always 1

y <- bc_data.rec$time


#########PICK UP HERE############

rr.mod <- glmnet(x,y,family="gaussian",alpha=0) # gaussian for linear regression, alpha = 0 == ridge, alpha = 1 == lasso

plot(rr.mod)
# notice the x-axis in this plot

plot(rr.mod,xvar = "lambda")
# this might be more intuitive

# for different values of lambda we are getting differnet coefficient estimates.
# we need to decide on an optimal value for lambda

# We will do it by performing cross-validation

cv.rr <- cv.glmnet(x,y,alpha=0)
plot(cv.rr) #plot shows minimum lambda and acceptable range
cv.rr$lambda.min

coef.min <- coef(cv.rr, s = "lambda.min")
coef.min #gives you the coefficients using the minimum value of lambda
# We really do not care about these values though...

# making some predictions using the optimal lambda, and for lambda value = 1*se from optimal lambda

predict(rr.mod, newx=x[1:10,],s= c(cv.rr$lambda.min, cv.rr$lambda.1se)) # newx ~= newdata from before. s= lambda
# or we can use the output from cv.glmnet function
predict(cv.rr, newx=x[1:10,],s= c(cv.rr$lambda.min, cv.rr$lambda.1se))

# calculating the MSE of the whole data set using these two choices for lambda

mean((y-predict(rr.mod, newx=x, s= cv.rr$lambda.min))^2)
mean((y-predict(rr.mod, newx=x, s= cv.rr$lambda.1se))^2)
# higher as expected
