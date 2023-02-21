#---------------------- Week_3_Module_3 R Script ----------------------#

print("Author : Nikshita Ranganathan")
print("Module 3 Assignment - GLM and Logistic Regression")
print("Course Name -  ALY6015: Intermediate Analytics")

# Installing and loading the packages
library(ISLR)
library(ellipse)
library(corrgram)
library(RColorBrewer)
library(dplyr)
library(psych)
library(skimr)
library(corrplot)
library(gridExtra)
library(ggpubr)
library(MASS)
library(caret)
library(hrbrthemes)
library(grid)
library(pROC)
library(ochRe)
library(Amelia)
library(mlbench)
library(wesanderson)

# Load the data
attach(College)
College

# Descriptive statistics
str(College)
summary(College)
glimpse(College)
headTail(College)
dim(College)
skim(College)
describe(College)
describeBy(College,Private)

# Checking Missing data
missmap(College, col=c("blue", "red"), legend=FALSE)
sum(is.na(College))
sum(is.null(College))

# Checking for duplicated rows and removing them
duplicated(College)
anyDuplicated(College)

# Scatterplot
qplot(x=Apps,y=Accept,color=Private,shape=Private)+scale_shape_manual(values = c(16, 17)) +scale_color_manual(values = c("#F6AAC9", "#88BDE6"))
qplot(x=S.F.Ratio,y=Expend,color=Private,shape=Private)+scale_shape_manual(values = c(16, 17)) +scale_color_manual(values = c("#F6AAC9", "#88BDE6"))

# Boxplots
y<-qplot(x=Private,y=Outstate,fill=Private,geom='boxplot')+guides(fill=FALSE)+scale_fill_manual(values= wes_palette("Chevalier1", n = 2))
z<-qplot(x=Private,y=Grad.Rate,fill=Private,geom='boxplot')+guides(fill=FALSE)+scale_fill_manual(values= wes_palette("Chevalier1", n = 2))
grid.arrange(y,z,nrow=1)

# Barplot
df1<-College %>% arrange(desc(Enroll))
df1<-head(df1,10)
University_name<-row.names(df1)
ggplot(df1, aes(x=University_name, y=Enroll,fill=University_name)) + geom_bar(stat = "identity")+coord_flip()+scale_fill_brewer(palette="Set3")+ggtitle("Universities with Top 10 Enrollments")+labs(x = "University Name", y = "Number of Enrollments",fill = "University Name")

# Histograms
hist1<-ggplot(College,aes(x=Apps)) +geom_histogram(fill= "#00A9FF",colour = "black")
hist2<-ggplot(College,aes(x=Accept)) +geom_histogram(fill= "#A54657",colour = "black")
hist3<-ggplot(College,aes(x=Enroll)) +geom_histogram(fill= "#97ce4c",colour = "black")
hist4<-ggplot(College,aes(x=Top10perc)) +geom_histogram(fill= "#00C19A",colour = "black")
hist5<-ggplot(College,aes(x=Top25perc)) +geom_histogram(fill= "#F39B7FFF",colour = "black")
hist6<-ggplot(College,aes(x=Outstate)) +geom_histogram(fill= "#938dd2",colour = "black")
hist7<-ggplot(College,aes(x=Expend)) +geom_histogram(fill= "#F8766D",colour = "black")
hist8<-ggplot(College,aes(x=Grad.Rate)) +geom_histogram(fill= "#00BFC4",colour = "black")
grid.arrange(hist1,hist2,hist3,hist4,hist5,hist6,hist7,hist8,top=textGrob("Histograms of Variables"))

# Correlation Matrix
corr <- select_if(College, is.numeric) 
cormatrix<-round(cor(corr,method = "pearson"),digits=2)

corrgram(corr, order = TRUE, upper.panel = panel.pie,lower.panel=panel.shade,
          main = "Correlogram",)
corrgram(corr, lower.panel=panel.pts, upper.panel=panel.conf,
         diag.panel=panel.density)

corr<-round(cor(corr,method = "pearson"),digits=2)
plotcorr(corr, col = colorRampPalette(c("#E08214", "white", "#8073AC"))(10), mar=c(0,0.1,0,0))

# Split data into train and test sets
datasplit <-createDataPartition(College$Private,p=0.7,list=FALSE)
train<-College[datasplit,]
test<-College[-datasplit,]
head(train)
head(test)

# Fit a logistic regression model
model1<-glm(Private~.,data=train,family=binomial(link="logit"))
summary(model1)

# Display regression coefficients (log-odds)
coef(model1)

# Display regression coefficients (odds)
exp(coef(model1))

# Model 2 after variable selection
model2<-glm(Private~Apps+Enroll+F.Undergrad+Outstate+Terminal+perc.alumni,data=train,family=binomial(link="logit"))
summary(model2)

# Display regression coefficients (log-odds)
coef(model2)

# Display regression coefficients (odds)
exp(coef(model2))

# Train set predictions
probabilities.train <- predict(model2, newdata = train, type = "response")
predicted.classes.train <- as.factor(ifelse(probabilities.train >= 0.5, "Yes", "No"))

# Model Accuracy
cm_train<-confusionMatrix(predicted.classes.train,train$Private,positive='Yes')
cm_train

# Test set predictions
probabilities.test <- predict(model2, newdata = test, type = "response")
predicted.classes.test <- as.factor(ifelse(probabilities.test >= 0.5, "Yes", "No"))

# Model Accuracy
cm_test<-confusionMatrix(predicted.classes.test,test$Private,positive='Yes')
cm_test

# Plot the Receiver operator characteristic curve
ROC1<- roc(test$Private, probabilities.test)
plot(ROC1, col = "blue", ylab = "Sensitivity - TP Rate", xlab = "Sensitivity - FP Rate")

# Plot the Area Under the ROC Curve
auc <- auc(ROC1)
auc
