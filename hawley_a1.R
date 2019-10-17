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

#subset the data for only those with recurrence. Also dropping the standard error and worst measurement columns; will only use means for model
#bc_data.rec <- bc_data %>% filter(outcome=="R") %>% select(,-matches("_se|_w"))
bc_data.rec <- bc_data %>% select(-matches("_se|_w|ID|outcome"))

#summarize new data set
summary(bc_data.rec)

#loop through the predictor columns and create boxplots for each mean (exclude the factor column)
invisible(lapply(colnames(bc_data.rec[1:12]),function(x){
  boxplot(bc_data.rec[,x],main=x,type="l")
}))

plot(bc_data.rec$ln_status, xlab="Lymph node status")

#define predictors
#predictors <- colnames(bc_data.rec[4:15])

library(glmnet)

#create model matrix of predictors (everything except time response)
x <- model.matrix(time ~.,bc_data.rec)[,-1] #remove intercept, ID, and outcome

# response variable 
y <- bc_data.rec$time

##RIDGE REGRESSION##
# Train the ridge regression model using x and y; alpha= 0 for RR
rr.mod <- glmnet(x,y,family="gaussian",alpha=0)

# plot the L2 Norm against the coefficients 
plot(rr.mod)

# perform cross-validation to determine optimal value of lambda 
cv.rr <- cv.glmnet(x,y,alpha=0, nfolds = 5)

# plot shows minimum lambda and acceptable range
plot(cv.rr) 

# minimum lamdba value
cv.rr$lambda.min

# report the coefficients using the optimum lambda value
coef.min <- coef(cv.rr, s = "lambda.min")
coef.min 

# calculating the MSE of the whole data set using the optimal lambda value
mean((y-predict(rr.mod, newx=x, s= cv.rr$lambda.min))^2)


##LASSO##
# Train the LASSO model using x and y; alpha = 1 for lasso
l.mod <- glmnet(x,y,family="gaussian",alpha=1)

# plot the L2 Norm against the coefficients 
plot(l.mod)

# perform cross-validation to determine optimal value of lambda 
cv.l <- cv.glmnet(x,y,alpha=1, nfolds = 5)

# plot shows minimum lambda and acceptable range
plot(cv.l) 

# minimum lamdba value
cv.l$lambda.min

# report the coefficients using the optimum lambda value
coef.min.l <- coef(cv.l, s = "lambda.min")
coef.min.l 

# Using the optimal lambda value, the selected features are: perimeter, smoothness, and symmetry
rownames(coef.min.l)[coef.min.l[,1]!=0][-1]

# calculating the MSE of the whole data set using the optimal lambda value
mean((y-predict(l.mod, newx=x, s= cv.l$lambda.min))^2)
