#---------------------- Week_1_Module_1 R Script ----------------------#

print("Author : Nikshita Ranganathan")
print("Module 1 Assignment - Regression Diagnostics with R")
print("Course Name -  ALY6015: Intermediate Analytics")

# Installing and loading the libraries
install.packages("psych")
library(car)
library(ggcorrplot)
library(psych)
library(GGally)
library(corrgram)
library(corrplot)
library(ggplot2)
library(leaps)
library(ochRe)
library(dplyr)
library(skimr)
library(visdat)
library(gridExtra)
library(naniar)
library(hrbrthemes)
library(magrittr)
install.packages("ggridges")
library(ggridges)
install.packages("olsrr")
library("olsrr")

# Importing the dataset
getwd()
housing<-read.csv("AmesHousing.csv")

# Descriptive statistics & EDA
str(housing)
summary(housing)
headTail(housing)
dim(housing)
skim(housing)
describe(housing,quant = c(0.25, 0.75),IQR = T)
glimpse(housing)

# Density Plots
d1<-ggplot(housing,aes(x=`SalePrice`))+geom_density(fill = "#925E9F99")+xlab("Sale Price of Houses")+ggtitle("Density plot A")
d2<-ggplot(housing,aes(x=`Lot.Frontage`))+geom_density(fill = "#79AF97FF")+xlab("Lot Frontage")+ggtitle("Density plot B")
d3<-ggplot(housing,aes(x=`Lot.Area`))+geom_density(fill = "#FFDC91FF")+xlab("Lot Area")+ggtitle("Density plot C")
d4<-ggplot(housing,aes(x=`Year.Built`))+geom_density(fill = "cornflowerblue")+xlab("Year Built")+ggtitle("Density plot D")
d5<-ggplot(housing,aes(x=`Yr.Sold`))+geom_density(fill = "tomato3")+xlab("Year Sold")+ggtitle("Density plot E")
d6<-ggplot(housing,aes(x=`Gr.Liv.Area`))+geom_density(fill = "#FF9DA7")+xlab("Living Area")+ggtitle("Density plot F")
grid.arrange(d1,d2,d3,d4,d5,d6)

# Boxplots
options(scipen=5)
ggplot(housing, aes(x=House.Style, y=SalePrice, fill=House.Style)) + 
  geom_boxplot(outlier.size = 1) +theme_ipsum()+scale_fill_ochre("qalah") +ggtitle("Boxplots")+xlab("House Style")+ylab("Sale Price of Houses")+labs(fill = "House Style")

# Ridge Chart
ggplot(housing, aes(x=SalePrice, y=Neighborhood, fill=Neighborhood)) + 
  geom_density_ridges() +theme(legend.position="none")+theme_ipsum() +ggtitle("Ridge Chart")+xlab("Sale Price of Houses")+ylab("Neighbourhood")+labs(fill = "Neighbourhood")

# Grouped Bar Chart
options(scipen=5)
ggplot(housing, aes(Yr.Sold,SalePrice,fill=Bldg.Type)) +
  geom_col(position = "dodge")+
  scale_fill_ochre(palette="tasmania")+ggtitle("Grouped Bar chart")+xlab("Year Sold")+ylab("Sale Price of Houses")+labs(fill = "Building Type")+theme(axis.text.x = element_text( hjust = 1))

# Changing the datatypes
housing$Garage.Yr.Blt<-as.numeric(housing$Garage.Yr.Blt)

# Visualization and Checking NA values
vis_miss(housing)
gg_miss_var(housing)
gg_miss_which(housing)
sum(is.na(housing))
sum(is.null(housing))
colSums(is.na(housing))

# Checking for duplicated rows and removing them
duplicated(housing)
anyDuplicated(housing)

# Imputation Model (Mean value and other values)
housing$Lot.Frontage[which(is.na(housing$Lot.Frontage))]<-mean(housing$Lot.Frontage,na.rm=TRUE)
housing <-housing %>% mutate(Alley = ifelse(is.na(Alley), "No Alley",Alley)) %>% 
  mutate(Mas.Vnr.Area = ifelse(is.na(Mas.Vnr.Area), 0,Mas.Vnr.Area)) %>%
  mutate(Bsmt.Qual = ifelse(is.na(Bsmt.Qual), "No Bsmt",Bsmt.Qual)) %>%
  mutate(Bsmt.Cond = ifelse(is.na(Bsmt.Cond), "No Bsmt",Bsmt.Cond)) %>%
  mutate(Bsmt.Exposure = ifelse(is.na(Bsmt.Exposure), "No Bsmt",Bsmt.Exposure)) %>%
  mutate(BsmtFin.Type.1 = ifelse(is.na(BsmtFin.Type.1), "No Bsmt",BsmtFin.Type.1)) %>%
  mutate(BsmtFin.SF.1 = ifelse(is.na(BsmtFin.SF.1), 0,BsmtFin.SF.1)) %>%
  mutate(BsmtFin.Type.2 = ifelse(is.na(BsmtFin.Type.2), "No Bsmt",BsmtFin.Type.2)) %>%
  mutate(BsmtFin.SF.2 = ifelse(is.na(BsmtFin.SF.2), 0,BsmtFin.SF.2)) %>%
  mutate(Bsmt.Unf.SF = ifelse(is.na(Bsmt.Unf.SF), 0,Bsmt.Unf.SF)) %>%
  mutate(Total.Bsmt.SF = ifelse(is.na(Total.Bsmt.SF), 0,Total.Bsmt.SF)) %>%
  mutate(Bsmt.Half.Bath = ifelse(is.na(Bsmt.Half.Bath), 0,Bsmt.Half.Bath)) %>%
  mutate(Bsmt.Full.Bath = ifelse(is.na(Bsmt.Full.Bath), 0,Bsmt.Full.Bath)) %>%
  mutate(Fireplace.Qu = ifelse(is.na(Fireplace.Qu), "No Fireplace",Fireplace.Qu)) %>%
  mutate(Garage.Type = ifelse(is.na(Garage.Type), "No Garage",Garage.Type)) %>%
  mutate(Garage.Yr.Blt = ifelse(is.na(Garage.Yr.Blt),0, Garage.Yr.Blt)) %>%
  mutate(Garage.Finish = ifelse(is.na(Garage.Finish), "No Garage",Garage.Finish)) %>%
  mutate(Garage.Cars = ifelse(is.na(Garage.Cars), 0,Garage.Cars)) %>%
  mutate(Garage.Area = ifelse(is.na(Garage.Area), 0,Garage.Area)) %>%
  mutate(Garage.Qual = ifelse(is.na(Garage.Qual), "No Garage",Garage.Qual)) %>%
  mutate(Garage.Cond = ifelse(is.na(Garage.Cond), "No Garage",Garage.Cond)) %>%
  mutate(Pool.QC = ifelse(is.na(Pool.QC), "No Pool",Pool.QC)) %>%
  mutate(Fence = ifelse(is.na(Fence), "No Fence",Fence)) %>%
  mutate(Misc.Feature=ifelse(is.na(Misc.Feature),"None",Misc.Feature))

# cor() function - Correlation Matrix
corr <- select_if(housing, is.numeric)  
drop<-c("Order","PID")
corr<-corr[,!(names(corr) %in% drop)]
cormatrix<-round(cor(corr,method = "pearson"),digits=2)
View(cormatrix)

# Correlation plot
ggcorrplot(cormatrix, 
           outline.color = "white",
           ggtheme = theme_bw(),
           colors = c("#D53E4F","#FFFFBF","#3288BD"),tl.cex=7,title = "Correlation Plot",legend.title = "Correlation")

# Scatterplots
options(scipen=5)
ggplot(housing, aes(x=Overall.Qual, y=SalePrice)) +
  geom_point(alpha = 0.7,color="#00A9FF") +
  geom_smooth(color="#F8766D", fill="darkgrey", se=TRUE) +
  theme_ipsum_rc(grid="XY") +labs(title="Scatterplot A",x="Overall Quality", y="House Sale Price")

ggplot(housing, aes(x=BsmtFin.SF.2, y=SalePrice)) +
  geom_point(alpha = 0.7,color="#00BF7d") +
  geom_smooth(color="#F8766D", fill="darkgrey", se=TRUE) +
  theme_ipsum_rc(grid="XY") +labs(title="Scatterplot B",x=" Rating of basement finished area (if multiple types)", y="House Sale Price")

ggplot(housing, aes(x=Mas.Vnr.Area, y=SalePrice)) +
  geom_point(alpha = 0.7,color="#925e9fff") +
  geom_smooth(color="#F8766D", fill="darkgrey", se=TRUE) +
  theme_ipsum_rc(grid="XY") +labs(title="Scatterplot C",x=" Masonry veneer area (sqft)", y="House Sale Price")

# Regression Model
model1<-lm(formula = SalePrice ~ Total.Bsmt.SF+X1st.Flr.SF+Gr.Liv.Area+Garage.Area+BsmtFin.SF.1, data = corr)
summary(model1)
summary(model1)$adj.r.squared
AIC(model1)
BIC(model1)

# Equation = Y=-25511.583+(45.068)Total.Bsmt.SF+(-5.111)X1st.Flr.SF+(71.178)Gr.Liv.Area+(101.264)Garage.Area+(23.159)BsmtFin.SF.1

# Diagnostic Plots
plot(model1)

qqPlot(model1, labels = rownames(cormatrix), simulate = TRUE, main = "Q-Q Plot")
crPlots(model=model1)
spreadLevelPlot(model1)
vif(model1)

# Multicollinearity
ols_vif_tol(model1)

# Outlier Detection & Removal
outlierTest(model = model1)
corr <- corr[!(row.names(corr) %in% c("1499","2181","2182","1768","45","434","1064","1761","2333","433")),]
model1<-lm(formula = SalePrice ~ Total.Bsmt.SF+X1st.Flr.SF+Gr.Liv.Area+Garage.Area+BsmtFin.SF.1, data = corr)
outlierTest(model = model1)
corr <- corr[!(row.names(corr) %in% c("2593","1638","2331","2446","2451","2335","1641","1183")),]
model1<-lm(formula = SalePrice ~ Total.Bsmt.SF+X1st.Flr.SF+Gr.Liv.Area+Garage.Area+BsmtFin.SF.1, data = corr)
outlierTest(model = model1)
corr <- corr[!(row.names(corr) %in% c("2342")),]
model1<-lm(formula = SalePrice ~ Total.Bsmt.SF+X1st.Flr.SF+Gr.Liv.Area+Garage.Area+BsmtFin.SF.1, data = corr)
outlierTest(model = model1)
corr <- corr[!(row.names(corr) %in% c("1643")),]
model1<-lm(formula = SalePrice ~ Total.Bsmt.SF+X1st.Flr.SF+Gr.Liv.Area+Garage.Area+BsmtFin.SF.1, data = corr)
outlierTest(model = model1)
corr <- corr[!(row.names(corr) %in% c("424")),]
model1<-lm(formula = SalePrice ~ Total.Bsmt.SF+X1st.Flr.SF+Gr.Liv.Area+Garage.Area+BsmtFin.SF.1, data = corr)
summary(model1)
summary(model1)$adj.r.squared
AIC(model1)
BIC(model1)

cutoff<-4/(nrow(corr)-length(model1$coefficients)-2)
plot(model1,which=4,cook.levels=cutoff)
abline(h=cutoff,lty=2,col="red")

# Feature Selection
library(MASS)
model2<-lm(formula = SalePrice ~ ., data = corr)
summary(model2)
# Forward Stepwise Selection
stepAIC(model2, direction = "forward")

# Backward Stepwise Selection
stepAIC(model2, direction = "backward")

# Stepwise Stepwise Selection
stepAIC(model2, direction = "both")

# Best Subset Selection
leaps <- regsubsets(SalePrice ~ ., data = corr, nbest = 4)
summary(leaps)
with(summary(leaps),data.frame(rsq,adjr2,cp,rss,outmat))
plot(leaps, scale = "adjr2",main = "Adjusted R^2")
final <- lm(formula = SalePrice ~ MS.SubClass +
            Overall.Qual + Year.Built +
            BsmtFin.SF.1 + Gr.Liv.Area + Bedroom.AbvGr + Garage.Area,
          data = corr)
summary(final)
