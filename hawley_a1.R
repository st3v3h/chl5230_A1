#' ---
#' title: "CHL5230H | Assignment 1"
#' author: "Steve Hawley"
#' date: "October 20, 2019"
#' output: word_document
#' editor_options: 
#'   chunk_output_type: console
#' ---
#' 
#' ##Task 1: Load Data
#' 
#' First, the data are loaded in R. The structure of the data set is examined and columns are given header names because none were provided. Next, missing data are converted to NA.
#' 
## ----task1, warning=FALSE------------------------------------------------
#importing tidyverse for data wrangling
library(tidyverse)

#import the data
bc_data <- read.csv("bc_data-1.csv", header = F)

#look at the structure
str(bc_data)

#rename the columns to something useful
colnames(bc_data) <- c("ID","outcome","time","radius_m","texture_m","perimeter_m","area_m","smoothness_m","compactness_m","concavity_m","conc_points_m","symmetry_m","fractal_m","radius_se","texture_se","perimeter_se","area_se","smoothness_se","compactness_se","concavity_se","conc_points_se","symmetry_se","fractal_se","radius_w","texture_w","perimeter_w","area_w","smoothness_w","compactness_w","concavity_w","conc_points_w","symmetry_w","fractal_w","size","ln_status")

#clean missing data
bc_data[bc_data == "?"] <- NA


#' 
#' ##Task 2: Data Wrangling and Descriptive Statistics
#' 
#' The data are now prepared for model generation. First, lymph node status is organized into 3 levels (down from 23). The data are then subset to include only the "mean value" columns as well as tumor size and lymph node status. Further, only "recurrent" outcomes are retained for model generation. The data are then summarized with basic descriptive statisitcs (min, q1, median, mean, q3, max) for all continuous predictors and frequency counts for the factor predictor. Corrosponding boxplots and a column plot for the continuous and factor predictors, respectfully, are generated. Y-axis labels are not provided for the boxplots as they are not available in the data set. 
#' 
## ----task2, warning=FALSE, results='asis'--------------------------------

#for descriptive stats (provides nice R Markdown output)
library(summarytools)

#clean up ln_status into 3 factors
bc_data$ln_status <- factor(ifelse(bc_data$ln_status %in% 0, "0",
                                   ifelse(bc_data$ln_status %in% 1:3, "1-3", "4 or more")),
                            levels=c("0","1-3","4 or more"))

#subset the data for only those with recurrence. Also dropping the 'standard error' and 'worst' measurement columns; will only use means for model
bc_data.rec <- bc_data %>% filter(outcome=="R") %>% select(-matches("_se|_w|ID|outcome"))

#summarize new data set
descr(bc_data.rec[2:12], stats= c("min","q1","med","mean","q3","max"), transpose = T, headings=F, style = "rmarkdown")
freq(bc_data.rec$ln_status)

#loop through the predictor columns and create boxplots for each mean (exclude the factor column)
invisible(lapply(colnames(bc_data.rec[1:12]),function(x){
  boxplot(bc_data.rec[,x],main=x,type="l")
}))

plot(bc_data.rec$ln_status, xlab="Lymph node status", ylab="count", ylim=c(0,25))


#' 
#' ##Task 3: Ridge Regression Model
#' 
#' The riddge regression model is now generated using this glmnet package. "Time" is set as the response variable and all other variables are set as predictors. A plot is then generated which shows the coefficients of the predictors for different levels of regularization (log lambda in this case). The plot shows that when lambda ~0 (no shrinkage), the coefficients are at a maximum (i.e., the are equal to their least-squares estimates) but as lambda increases, the coefficient estimates essentially shrink to 0.
#' 
## ----task3, warning=FALSE------------------------------------------------
library(glmnet)

#create model matrix of predictors (everything except time response)
x <- model.matrix(time ~.,bc_data.rec)[,-1] #remove intercept

# response variable 
y <- bc_data.rec$time

##RIDGE REGRESSION##
# Train the ridge regression model using x and y; alpha= 0 for RR
rr.mod <- glmnet(x,y,family="gaussian",alpha=0)

# plot the the coefficients against the level of regularization 
plot(rr.mod, xvar = "lambda", label = T)


#' 
#' ##Task 4: Lambda Optimization 
#' 
#' Here, the optimal value of lambda is determined using 5-fold cross validation. A plot of MSE v. log(lambda) shows at what lambda value the MSE is minimized. This value is then used to generate a list of coefficients at the minimum lambda value (coef.min).
#' 
## ----task4---------------------------------------------------------------
# perform 5-fold cross-validation to determine optimal value of lambda 
cv.rr <- cv.glmnet(x,y,alpha=0, nfolds = 5)

# plot shows minimum lambda and acceptable range
plot(cv.rr) 

# minimum lamdba value
cv.rr$lambda.min

# report the coefficients using the optimum lambda value
coef.min <- coef(cv.rr, s = "lambda.min")
coef.min 


#' 
#' ##Task 5: Mean Squared Error Using Optimal Lambda Value
#' 
#' Finally, the MSE for the model is calclated using the optimal lambda value determined in the previous task. 
#' 
## ----task5---------------------------------------------------------------
# calculating the MSE of the whole data set using the optimal lambda value
t5_mse <- mean((y-predict(rr.mod, newx=x, s= cv.rr$lambda.min))^2)
t5_mse


#' 
#' The MSE is: **`r t5_mse`**
#' 
#' ##Tase 6: LASSO Regression
#' 
#' Tasks 3-5 are now repeated using LASSO regression. Again, the glmnet package is used to generate the model, and a plot of coefficients v. log lambda is generated. The optimal value of lambda is then determined using 5-fold cross validation and plotted to show the value of lambda that gives the minimum MSE. Finally, the minimum value of lambda is used to calculate the minimum MSE and coefficients. Because LASSO shrinks some predictor coefficients down to 0, only the non-0 coeffiencts are reported. 
#' 
## ----task6---------------------------------------------------------------
##LASSO##
# Train the LASSO model using x and y; alpha = 1 for lasso
l.mod <- glmnet(x,y,family="gaussian",alpha=1)

# plot the the coefficients against the level of regularization
plot(l.mod, xvar = "lambda")

# perform cross-validation to determine optimal value of lambda 
cv.l <- cv.glmnet(x,y,alpha=1, nfolds = 5)

# plot shows minimum lambda and acceptable range
plot(cv.l) 

# minimum lamdba value
cv.l$lambda.min

# report the coefficients using the optimum lambda value
coef.min.l <- coef(cv.l, s = "lambda.min")
coef.min.l 

# Using the optimal lambda value, the selected features are:
l.features <- rownames(coef.min.l)[coef.min.l[,1]!=0][-1]
l.features

# calculating the MSE of the whole data set using the optimal lambda value
t6_mse <- mean((y-predict(l.mod, newx=x, s= cv.l$lambda.min))^2)
t6_mse


#' 
#' ##Task 7: Comparison of two methods 
#' 
#' The calculated MSE using ridge regression was `r t5_mse` and the MSE calculated for LASSO regression was `r t6_mse`. Given that the ridge regression produced a smaller MSE, it may be hypothesized that the method produces a better model for this data set, which may suggest that most of the features in this data set are associated with the response. However, in order to properly evaluate the models, model performance should be measured on (ideally) a new data set. If these data are not available, the models may be tested by splitting the data into training a test sets, and resampling methods such as cross-validation, leave-one-out, or validation set may be used.
#' 
#' ##Task 8: Appropriateness of design
#' 
#' The objective of this model is to predict time to recurrance based on data generated from the fine needle aspirate of a breast mass. Irrespective of the model performance, the practical design of the is not suitable for this objective. This is because all of the feature data is collected *after* the recurrance has already occured. In other words, this model cannot predict time to recurrance until it has already happened, thus rendering the predictive power of this model moot. Therefore, to create a more useful model to predict time to recurrance, features available at the time of the initial surgery (e.g., basic demographics, initial tumor characteristics, etc.) should be selected. 
