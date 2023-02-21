#---------------------- Week_4_Module_4 R Script ----------------------#

print("Author : Nikshita Ranganathan")
print("Module 4 Assignment - Regularization")
print("Course Name -  ALY6015: Intermediate Analytics")

# Installing and loading the packages
library(ISLR)
library(glmnet)
library(Metrics)
library(corrgram)
library(hrbrthemes)
library(caret)
library(dplyr)
library(skimr)
library(MASS)
library(psych)

# Load the data
attach(College)
College
glimpse(College)
skim(College)
describe(College)
describeBy(College,Private)

# Scatterplot 
qplot(x=Outstate,y=Grad.Rate,color=Private,shape=Private)+scale_shape_manual(values = c(16, 17))+scale_color_brewer(palette="Set1")

# Histogram
ggplot(College,aes(x=Grad.Rate,fill=Private)) +geom_histogram(color="#e9ecef", alpha=0.5, position = 'identity')+scale_fill_manual(values=c("#69b3a2", "#404080")) +ggtitle("Histogram for Graduation Rate")+theme_ipsum()

# Correlation
corr <- select_if(College, is.numeric) 
cormatrix<-round(cor(corr,method = "pearson"),digits=2)
corrgram(corr, order = TRUE, upper.panel = panel.pie,lower.panel=panel.shade,
         main = "Correlogram",)
corrgram(corr, lower.panel=panel.pts, upper.panel=panel.conf,
         diag.panel=panel.density)

# Split data into train and test sets
set.seed(123)
datasplit <- createDataPartition(College$Grad.Rate, p = 0.7, list = FALSE)
train <- College[datasplit,]
test <- College[-datasplit,]

# Create a model matrix
train_x <- model.matrix(Grad.Rate ~ ., train)[, -1]
test_x <- model.matrix(Grad.Rate ~ ., test)[, -1]

train_y <- train$Grad.Rate
test_y <-test$Grad.Rate

# Find best values of Lambda
# Find the best lambda using cross-validation
set.seed(123)
cv_ridge <- cv.glmnet(train_x, train_y, alpha = 0, nfolds = 10)
cv_ridge

# Optimal Value of Lambda; Minimizes the Prediction Error
# Lambda Min - Minimizes out of sample loss
# Lambda 1se - Largest value of Lambda within 1 Standard Error of Lambda Min
log(cv_ridge$lambda.min)
log(cv_ridge$lambda.1se)

# Plot
plot(cv_ridge)

# Fit models based on lambda
# Fit the final model on Training Set using lambda.min
# alpha = 1 for Lasso (L2)
# alpha = 0 for Ridge (L1)

# Ridge Regression 
ridge_model.min <- glmnet(train_x,train_y,alpha = 0,lambda = cv_ridge$lambda.min)
ridge_model.min

# Display Regression Coefficients
coef(ridge_model.min)
plot(coef(ridge_model.min))

# Fit the model on training set using lambda.1se
ridge_model.1se <- glmnet(train_x,train_y,alpha = 0,lambda = cv_ridge$lambda.1se)
ridge_model.1se

# Display Regression Coefficients
coef(ridge_model.1se)
plot(coef(ridge_model.1se))

# Display coefficients of OLS model with no regularization
ols <- lm(Grad.Rate ~ ., data = train)
coef(ols)

# View RMSE of the full model
ridge_preds.ols <- predict(ols, new = test)
rmse(test$Grad.Rate, ridge_preds.ols)

# Train set predictions
ridge_pred.train <- predict(ridge_model.1se, newx = train_x)                      
ridge_train.rmse <- rmse(train_y, ridge_pred.train)

# Test set predictions
ridge_pred.test <- predict(ridge_model.1se, newx = test_x)
ridge_test.rmse <- rmse(test_y, ridge_pred.test)

# Comparing rmse values of train and test set predictions
ridge_train.rmse
ridge_test.rmse

# Find best values of Lambda
# Find the best lambda using cross-validation
set.seed(123)
cv_lasso <- cv.glmnet(train_x, train_y, alpha = 1, nfolds = 10)
cv_lasso

# Optimal Value of Lambda; Minimizes the Prediction Error
# Lambda Min - Minimizes out of sample loss
# Lambda 1se - Largest value of Lambda within 1 Standard Error of Lambda Min
log(cv_lasso$lambda.min)
log(cv_lasso$lambda.1se)

# Plot
plot(cv_lasso)

# Lasso Regression 
lasso_model.min <- glmnet(train_x,train_y,alpha=1,lambda = cv_lasso$lambda.min)
lasso_model.min

# Display Regression Coefficients
coef(lasso_model.min)
plot(coef(lasso_model.min))

# Fit the model on training set using lambda.1se
lasso_model.1se <- glmnet(train_x,train_y, alpha = 1, lambda = cv_lasso$lambda.1se)
lasso_model.1se

# Display Regression Coefficients
coef(lasso_model.1se)
plot(coef(lasso_model.1se))

# Display coefficients of OLS model with no regularization
ols_train <- lm(Grad.Rate ~ ., data = train)
coef(ols_train)

# View RMSE of the full model
lasso_preds.ols <- predict(ols_train, new = test)
rmse(test$Grad.Rate, lasso_preds.ols)

# Train set predictions
lasso_pred.train <- predict(lasso_model.1se, newx = train_x)                     
lasso_train.rmse <- rmse(train_y, lasso_pred.train)

# Test set predictions
lasso_pred.test <- predict(lasso_model.1se, newx = test_x)                     
lasso_test.rmse <- rmse(test_y, lasso_pred.test)

# Comparing rmse values of train and test set predictions
lasso_train.rmse
lasso_test.rmse

# Stepwise Selection (Train set)
model1<-lm(formula = Grad.Rate ~ ., data = train)
summary(model1)

# Stepwise Stepwise Selection
stepAIC(model1, direction = "both")
model.final=step(model1)

# Regression Model with optimal features of stepwise selection (Train data)
model2 <- lm(formula = Grad.Rate ~ Private + Apps + Top25perc + P.Undergrad + Outstate + Room.Board + Personal + perc.alumni + Expend, data = train)
summary1<-summary(model2)
summary1
rmse_stepwise_train<-sqrt(mean(summary1$residuals^2))
rmse_stepwise_train

# Stepwise Selection (Train set)
model3<-lm(formula = Grad.Rate ~ ., data = test)
summary(model3)

# Stepwise Stepwise Selection
stepAIC(model3, direction = "both")
model.final=step(model3)

# Regression Model with optimal features of stepwise selection (Train data)
model4 <- lm(formula = Grad.Rate ~ Apps + Top25perc + P.Undergrad + Outstate + Room.Board + PhD+ perc.alumni + Terminal, data = test)
summary2<-summary(model4)
summary2
rmse_stepwise_test<-sqrt(mean(summary2$residuals^2))
rmse_stepwise_test

