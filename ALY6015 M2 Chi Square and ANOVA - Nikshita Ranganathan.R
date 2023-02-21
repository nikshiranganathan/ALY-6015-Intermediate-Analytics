#---------------------- Week_2_Module_2 R Script ----------------------#

print("Author : Nikshita Ranganathan")
print("Module 2 Assignment - Chi Square and ANOVA")
print("Course Name -  ALY6015: Intermediate Analytics")

# Installing and loading the packages
library(dplyr)
library(psych)
library(tibble)
library(skimr)
library(corrgram)
library(GGally)
library(ggplot2)
library(hrbrthemes)
library(wesanderson)

# Section 11-1 
# Q6. Blood Type

# State the hypotheses
# H0: Type A=0.20, Type B=0.28, Type O=0.36, Type AB= 0.16
# H1: The blood type distribution of hospital patients is not the same as the general population

# Creating matrix
Data_bloodtype <- matrix(c(0.20, 0.28, 0.36, 0.16,12, 8, 24, 6),ncol=2,byrow=FALSE)
colnames(Data_bloodtype) <- c("Expected","Observed")
rownames(Data_bloodtype) <- c("Type A", "Type B","Type O","Type AB")
Data_bloodtype

# Set significance level
alpha<-0.1

# Create a vector of the values
observed <- c(12, 8, 24, 6)

# Create a vector of the probabilities
p <- c(0.20, 0.28, 0.36, 0.16)

# Run the test and save the results in result variable
result <- chisq.test(x = observed, p = p)

# View the test statistic and p-value
result$statistic
result$p.value
result$parameter
result

# Critical Value
qchisq(alpha,result$parameter, lower.tail=FALSE)

# Compare the p-value to alpha and make a decision
ifelse(result$p.value > alpha,"Fail to reject the null hypothesis","Reject the null hypothesis")

# Q8. On-Time Performance by Airlines

# State the hypotheses
# H0: On-Time = 0.708, National Aviation System Delay = 0.082,Aircraft Arriving Late = 0.09, Other (because of weather and other conditions) = 0.12
# H1: The on-time performance distribution of airlines is not the same as government's statistics

# Creating matrix
Data_airlines <- matrix(c(0.708, 0.082, 0.09, 0.12,125, 10, 25, 40),ncol=2,byrow=FALSE)
colnames(Data_airlines) <- c("Expected","Observed")
rownames(Data_airlines) <- c("On time", "National Aviation System delay","Aircraft arriving late","Other (because of weather and other conditions)")
Data_airlines

# Set significance level
alpha2 = 0.05

# Create a vector of the values
observed2 <- c(125, 10, 25, 40)

# Create a vector of the probabilities
p2 <- c(0.708, 0.082, 0.09, 0.12)

# Run the test and save the results to result variable
result2 <- chisq.test(x = observed2, p = p2)

# View the test statistic and p-value
result2$statistic
result2$p.value
result2$parameter
result2

# Critical Value
qchisq(alpha2,result2$parameter, lower.tail=FALSE)

# Compare the p-value to alpha and make a decision
ifelse(result2$p.value > alpha2,"Fail to reject the null hypothesis","Reject the null hypothesis")

# Section 11-2
# Q8. Ethnicity and Movie Admissions

# State the hypotheses
# H0: Movie admissions are independent to ethnicity
# H1: Movie admissions are dependent to ethnicity

# Set significance level
alpha3 = 0.05

# Create a vector for each row
year2013<-c(724, 335, 174, 107)
year2014<-c(370, 292, 152, 140)

# State the number of rows for the matrix
rows1=2

# Create a matrix from the rows
Movies=matrix(c(year2013,year2014),nrow=rows1,byrow=TRUE)

# Name the rows and columns of the matrix
rownames(Movies) <- c("2013", "2014")
colnames(Movies) <- c("Caucasian", "Hispanic", "African American", "Other")

# View the matrix
Movies

# Run the test and save the results to result variable
result3 <- chisq.test(Movies)

# View the test statistic and p-value
result3$statistic
result3$p.value
result3$parameter
result3

# Critical Value
qchisq(alpha3,result3$parameter, lower.tail=FALSE)

# Compare the p-value to alpha and make a decision
ifelse(result3$p.value > alpha3,"Fail to reject the null hypothesis","Reject the null hypothesis")

# Q10. Women in the Military

# State the hypotheses
# H0: Ranks and branches of Armed Forces are independent
# H1: Ranks and branches of Armed Forces are dependent

# Set significance level
alpha4 = 0.05

# Create one vector for each row
army <- c(10791, 62491)
navy <- c(7816, 42750)
marine_corps <- c(932, 9525)
air_force <- c(11819, 54344)

# State the number of rows for the matrix
rows2=4

# Create a matrix from the rows
Military=matrix(c(army,navy,marine_corps,air_force),nrow=rows2,byrow=TRUE)

# Name the rows and columns of the matrix
rownames(Military) <- c("Army", "Navy", "Marine Corps", "Air Corps")
colnames(Military) <- c("Officers", "Enlisted")

# View the matrix
Military

# Run the test and save the results to result variable
result4 <- chisq.test(Military)

# View the test statistic and p-value
result4$statistic
result4$p.value
result4$parameter
result4

# Critical Value
qchisq(alpha4,result4$parameter, lower.tail=FALSE)

# Compare the p-value to alpha and make a decision
ifelse(result4$p.value > alpha4,"Fail to reject the null hypothesis","Reject the null hypothesis")

# Section 12-1
# Q8. Sodium Contents of Foods

# State the hypotheses
# H0: μ_Condiments = μ_Cereals = μ_Desserts
# H1: At least one mean is different from the others

# Set significance level
alpha5 = 0.05

# Create a dataframe for Condiments
Condiments <- data.frame('Sodium' = c(270, 130, 230, 180, 80, 70, 200), 'Food' = rep('condiments', 7),stringsAsFactors = FALSE)

# Create a data frame for the Cereals
Cereals <- data.frame('Sodium' = c(260, 220, 290, 290, 200, 320, 140), 'Food' = rep('cereals', 7), stringsAsFactors= FALSE)

# Create a data frame for the Desserts
Desserts <- data.frame('Sodium' = c(100, 180, 250, 250, 300, 360, 300, 160), 'Food' = rep('desserts', 8),stringsAsFactors = FALSE)

# Combine the data frames into one
Sodium <- rbind(Condiments, Cereals, Desserts)
Sodium$Food <- as.factor(Sodium$Food)

# Run the ANOVA test
anova <- aov(Sodium ~ Food, data = Sodium)

# View the model summary
summary(anova)

# Save the summary to an object
a.summary <- summary(anova)
a.summary

# Degrees of Freedom
# k - 1: between group variance - numerator
df.numerator <- a.summary[[1]][1, "Df"]
df.numerator

# N - k: within group variance - denominator
df.denominator <- a.summary[[1]][2, "Df"]
df.denominator

# Extract the F test value from the summary
f.value <- a.summary[[1]][[1, "F value"]]
f.value

# Extract the P-value value from the summary
p.value <- a.summary[[1]][[1, "Pr(>F)"]]
p.value

# Critical Value
qf(p=alpha5, df1=df.numerator, df2 = df.denominator, lower.tail=FALSE)

# Determine if we should reject the null hypothesis
ifelse(p.value>alpha5,"Fail to reject the null hypothesis","Reject the null hypothesis")

# Section 12-2
# Q10. Sales for Leading Companies

# State the hypotheses
# H0: μ_Cereals = μ_Chocolatecandy = μ_Coffee
# H1: At least one mean is different from the others

# Set Significance Level
alpha6 = 0.01

# Create a data frame for the cereals
Cereal <- data.frame('Sales' = c(578, 320, 264, 249, 237), 'Food' = rep('cereal', 5), stringsAsFactors = FALSE)

# Create a data frame for the chocolate Candy
Chocolatecandy <- data.frame('Sales' = c(311, 106, 109, 125, 173), 'Food' = rep('chocolate candy', 5),stringsAsFactors = FALSE)

# Create a data frame for the coffee
Coffee <- data.frame('Sales' = c(261, 185, 302, 689), 'Food' = rep('coffee', 4), stringsAsFactors = FALSE)

# Combine the data frames into one
Sales <- rbind(Cereal, Chocolatecandy, Coffee)
Sales$Food <- as.factor(Sales$Food)

# Run the ANOVA test
anova2 <- aov(Sales ~ Food, data = Sales)

# View the model summary and save it
a.summary2 <- summary(anova2)
a.summary2

# Degrees of Freedom
# k - 1: Between Group Variance - Numerator
df.numerator2 <- a.summary2[[1]][1, "Df"]
df.numerator2

# N - k: Within Group Variance - Denominator
df.denominator2 <- a.summary2[[1]][2, "Df"]
df.denominator2

# Extract the F test value from the summary
f.value2 <- a.summary2[[1]][[1, "F value"]]
f.value2

# Extract the P-value from the summary
p.value2 <- a.summary2[[1]][[1, "Pr(>F)"]]
p.value2

# Critical Value
qf(p=alpha6, df1=df.numerator2, df2 = df.denominator2, lower.tail=FALSE)

# Determine if we should reject the null hypothesis
ifelse(p.value2>alpha6,"Fail to reject the null hypothesis","Reject the null hypothesis")

# See differences
TukeyHSD(anova2)

# Q12. Per-Pupil Expenditures

# State the hypotheses
# H0: μ_Eastern Third = μ_Middle Third = μ_Western Third
# H1: At least one mean is different from the others

# Set Significance Level
alpha7 = 0.05

# Create a data frame for the Eastern Third
Easternthird <- data.frame('Expenditure' = c(4946, 5953, 6202, 7243, 6113), 'States' = rep('Eastern Third', 5),stringsAsFactors = FALSE)

# Create a data frame for the Middle Third
Middlethird <- data.frame('Expenditure' = c(6149, 7451, 6000, 6479), 'States' = rep('Middle Third', 4),stringsAsFactors = FALSE)

# Create a data frame for the Western Third
Westernthird <- data.frame('Expenditure' = c(5282, 8605, 6528, 6911), 'States' = rep('Western Third', 4),stringsAsFactors = FALSE)

# Combine the data frames into one
Expenditure <- rbind(Easternthird, Middlethird, Westernthird)
Expenditure$States <- as.factor(Expenditure$States)

# Run the ANOVA test
anova3 <- aov(Expenditure ~ States, data = Expenditure)

# View the model summary and save it
a.summary3 <- summary(anova3)
a.summary3

# Degrees of Freedom
# k - 1: Between Group Variance - Numerator
df.numerator3 <- a.summary3[[1]][1, "Df"]
df.numerator3

# N - k: Within Group Variance - Denominator
df.denominator3 <- a.summary3[[1]][2, "Df"]
df.denominator3

# Extract the F test value from the summary
f.value3 <- a.summary3[[1]][[1, "F value"]]
f.value3

# Extract the P-value from the summary
p.value3 <- a.summary3[[1]][[1, "Pr(>F)"]]
p.value3

# Critical Value
qf(p=alpha7, df1=df.numerator3, df2 = df.denominator3, lower.tail=FALSE)

# Determine if we should reject the null hypothesis
ifelse(p.value3>alpha7,"Fail to reject the null hypothesis","Reject the null hypothesis")

# See differences
TukeyHSD(anova3)

# Section 12-3
# Q10. Increasing Plant Growth

# State the hypotheses

# 1. Hypotheses for Grow-light (Factor-1)
# H0: μ_Grow-light 1 = μ_Grow-light 2
# H1: There is a difference in means of Grow-light 1 and Grow-light 2

# 2. Hypotheses for Plant-Food (Factor-2)
# H0: μ_Plant-Food A = μ_Plant-Food B
# H1: There is a difference in means of Plant-Food A and Plant-Food B

# 3. Hypotheses for interaction (Factor1:Factor2-Grow-light:Plant-Food)
# H0: There is no interaction between the Growth-Light and Plant-Food
# H1: There is interaction between the Growth-Light and Plant-Food

# Set Significance Level
alpha8 = 0.05

# Create a data frame
Data_plants <- data.frame('plant_growth' = c(9.2, 9.4, 8.9, 8.5, 9.2, 8.9, 7.1, 7.2, 8.5, 5.5, 5.8, 7.6),'grow_light' = c('1', '1', '1', '2', '2', '2', '1', '1', '1', '2', '2', '2'),'plant_food' = c('A', 'A', 'A', 'A', 'A', 'A', 'B', 'B', 'B', 'B', 'B', 'B'),stringsAsFactors = TRUE)

# Run the ANOVA test
anova4 <- aov(plant_growth ~ grow_light + plant_food + grow_light:plant_food, data = Data_plants)

# View the model summary
summary(anova4)

# Save the summary to an object
a.summary4 <- summary(anova4)
a.summary4

# Degrees of Freedom
# k - 1: Between Group Variance - Numerator (Grow-light (Factor-1))
df.numerator_growlight <- a.summary4[[1]][1, "Df"]
df.numerator_growlight

# k - 1: Between Group Variance - Numerator (Plant-Food (Factor-2))
df.numerator_plantfood <- a.summary4[[1]][2, "Df"]
df.numerator_plantfood

# k - 1: Between Group Variance - Numerator (Factor1:Factor2-Grow-light:Plant-Food)
df.numerator_growlight_plantfood <- a.summary4[[1]][3, "Df"]
df.numerator_growlight_plantfood

# N - k: Within Group Variance - Denominator
df.denominator4 <- a.summary4[[1]][4, "Df"]
df.denominator4

# Extract the F test value (Grow-light (Factor-1))
f.value_growlight <- a.summary4[[1]][[1, "F value"]]
f.value_growlight

# Extract the F test value (Plant-Food (Factor-2))
f.value_plantfood <- a.summary4[[1]][[2, "F value"]]
f.value_plantfood

# Extract the F test value (Factor1:Factor2-Grow-light:Plant-Food)
f.value_growlight_plantfood <- a.summary4[[1]][[3, "F value"]]
f.value_growlight_plantfood

# Extract the P-value (Grow-light (Factor-1))
p.value_growlight <- a.summary4[[1]][[1, "Pr(>F)"]]
p.value_growlight

# Extract the P-value (Plant-Food (Factor-2))
p.value_plantfood <- a.summary4[[1]][[2, "Pr(>F)"]]
p.value_plantfood

# Extract the P-value (Factor1:Factor2-Grow-light:Plant-Food)
p.value_growlight_plantfood <- a.summary4[[1]][[3, "Pr(>F)"]]
p.value_growlight_plantfood

# Critical Value (Grow-light (Factor-1))
qf(p=alpha8, df1=df.numerator_growlight, df2 = df.denominator4, lower.tail=FALSE)

# Critical Value (Plant-Food (Factor-2))
qf(p=alpha8, df1=df.numerator_plantfood, df2 = df.denominator4, lower.tail=FALSE)

# Critical Value (Factor1:Factor2-Grow-light:Plant-Food)
qf(p=alpha8, df1=df.numerator_growlight_plantfood, df2 = df.denominator4, lower.tail=FALSE)

# Determine if we should reject the null hypothesis (Grow-light (Factor-1))
ifelse(p.value_growlight>alpha7,"There is no difference in means of Grow-light 1 and Grow-light 2","There is a difference in means of Grow-light 1 and Grow-light 2")

# Determine if we should reject the null hypothesis (Plant-Food (Factor-2))
ifelse(p.value_plantfood>alpha7,"There is no difference in means of Plant-Food A and Plant-Food B","There is a difference in means of Plant-Food A and Plant-Food B")

# Determine if we should reject the null hypothesis (Factor1:Factor2-Grow-light:Plant-Food)
ifelse(p.value_growlight_plantfood>alpha7,"There is no interaction between the Growth-Light and Plant-Food","There is interaction between the Growth-Light and Plant-Food")

# Baseball Question
# Importing the dataset
getwd()
baseball<-read.csv("baseball.csv")

# Descriptive statistics & EDA
str(baseball)
summary(baseball)
headTail(baseball)
dim(baseball)
skim(baseball)
describe(baseball,quant = c(0.25, 0.75),IQR = T)
glimpse(baseball)

baseball <- baseball %>% mutate(RankSeason = ifelse(is.na(RankSeason), 0,RankSeason)) %>%
  mutate(RankPlayoffs = ifelse(is.na(RankPlayoffs), 0,RankPlayoffs))

# Correlation Matrix
corr <- select_if(baseball, is.numeric)  
ggcorr(corr,nbreaks = 6,label = TRUE,label_size = 3,hjust = 0.75,size=3,color = "black")
corrgram(corr, lower.panel=panel.pts, upper.panel=panel.conf,diag.panel=panel.density)

# Graph 1 -  Scatterplot
ggplot(baseball, aes(x=W, y=RS))+geom_point(aes(color=factor(Playoffs)))+theme_ipsum()+labs(colour = "Playoffs",y = "Runs Scored", x = "Wins")+ggtitle("Scatterplot between Runs scored and Wins")+ scale_color_manual(values= wes_palette("Darjeeling1", n = 2))

baseball$decade <- baseball$Year - (baseball$Year %% 10)
Wins_decade_league<-baseball %>% group_by(decade,League) %>% summarize(wins=sum(W))  
Wins_byyear<-baseball %>% group_by(Year,League) %>% summarize(wins=sum(W))  
Wins_decade_league %>% ggplot( aes(x=decade, y=wins,fill=League)) + geom_bar(position="stack", stat="identity")+ scale_fill_brewer(palette="Accent")+ggtitle("Stacked Bar chart")+labs(x = "Decade", y = "Wins")
Wins_byyear %>% ggplot( aes(x=Year, y=wins,colour=League)) + geom_line(size=1) +scale_color_manual(values= wes_palette("GrandBudapest1", n = 2))+ggtitle("Line Chart")+labs(x = "Year", y = "Wins")

Wins_decade<-baseball %>% group_by(decade) %>% summarize(wins=sum(W))%>% as.tibble()  

# State the hypotheses
# H0: There is no difference in number of wins by decade
# H1: There is difference in number of wins by decade

# Set Significance Level
alpha9 = 0.05

# Run the test and save the results
result_baseball <- chisq.test(x = Wins_decade)

# View the test statistic and p-value
result_baseball$statistic
result_baseball$p.value
result_baseball$parameter
result_baseball

# Critical Value
qchisq(alpha9,result_baseball$parameter, lower.tail=FALSE)

# Compare the p-value to alpha and make a decision
ifelse(result_baseball$p.value > alpha9,"Fail to reject the null hypothesis","Reject the null hypothesis")

# Crop Data Question
# Importing the dataset
getwd()
Data_crop<-read.csv("crop_data.csv")

# Changing datatypes to factors
Data_crop$density=as.factor(Data_crop$density)
Data_crop$fertilizer=as.factor(Data_crop$fertilizer)
Data_crop$block=as.factor(Data_crop$block)

# Descriptive statistics
str(Data_crop)
summary(Data_crop)
headTail(Data_crop)
dim(Data_crop)
skim(Data_crop)
describe(Data_crop,quant = c(0.25, 0.75),IQR = T)
glimpse(Data_crop)

# State the hypotheses

# 1. Hypotheses for Fertilizer (Factor-1)
# H0: μ_Fertilizer 1 = μ_Fertilizer 2 = μ_Fertilizer 3
# H1: There is a difference in means of Fertilizer 1,Fertilizer 2 and Fertilizer 3

# 2. Hypotheses for Density (Factor-2)
# H0: μ_Density 1 = μ_Density 2
# H1: There is a difference in means of Density 1 and Density 2

# 3. Hypotheses for interaction (Factor1:Factor2-Fertilizer:Density)
# H0: There is no interaction between the Fertilizer and Density
# H1: There is interaction between the Fertilizer and Density

# Set Significance Level
alpha10 = 0.05

# Run the ANOVA test
anova5 <- aov(yield ~ fertilizer + density + fertilizer:density, data = Data_crop)

# View the model summary
summary(anova5)

# Save the summary to an object
a.summary5 <- summary(anova5)
a.summary5

# Degrees of Freedom
# k - 1: Between Group Variance - Numerator (Fertilizer (Factor-1))
df.numerator_fertilizer <- a.summary5[[1]][1, "Df"]
df.numerator_fertilizer

# k - 1: Between Group Variance - Numerator (Density (Factor-2))
df.numerator_density <- a.summary5[[1]][2, "Df"]
df.numerator_density

# k - 1: Between Group Variance - Numerator (Factor1:Factor2-Fertilizer:Density)
df.numerator_fertilizer_density <- a.summary5[[1]][3, "Df"]
df.numerator_fertilizer_density

# N - k: Within Group Variance - Denominator
df.denominator5 <- a.summary5[[1]][4, "Df"]
df.denominator5

# Extract the F test value (Fertilizer (Factor-1))
f.value_fertilizer <- a.summary5[[1]][[1, "F value"]]
f.value_fertilizer

# Extract the F test value (Density (Factor-2))
f.value_density <- a.summary5[[1]][[2, "F value"]]
f.value_density

# Extract the F test value (Factor1:Factor2-Fertilizer:Density)
f.value_fertilizer_density <- a.summary5[[1]][[3, "F value"]]
f.value_fertilizer_density

# Extract the P-value (Fertilizer (Factor-1))
p.value_fertilizer <- a.summary5[[1]][[1, "Pr(>F)"]]
p.value_fertilizer

# Extract the P-value (Density (Factor-2))
p.value_density <- a.summary5[[1]][[2, "Pr(>F)"]]
p.value_density

# Extract the P-value (Factor1:Factor2-Fertilizer:Density)
p.value_fertilizer_density <- a.summary5[[1]][[3, "Pr(>F)"]]
p.value_fertilizer_density

# Critical Value (Fertilizer (Factor-1))
qf(p=alpha10, df1=df.numerator_fertilizer, df2 = df.denominator5, lower.tail=FALSE)

# Critical Value (Density (Factor-2))
qf(p=alpha10, df1=df.numerator_density, df2 = df.denominator5, lower.tail=FALSE)

# Critical Value (Factor1:Factor2-Fertilizer:Density)
qf(p=alpha10, df1=df.numerator_fertilizer_density, df2 = df.denominator5, lower.tail=FALSE)

# Determine if we should reject the null hypothesis (Fertilizer (Factor-1))
ifelse(p.value_fertilizer>alpha10,"There is a no difference in means of Fertilizer 1,Fertilizer 2 and Fertilizer 3","There is a difference in mean yields for Fertilizer 1,Fertilizer 2 and Fertilizer 3")

# Determine if we should reject the null hypothesis (Density (Factor-2))
ifelse(p.value_density>alpha10,"There is a no difference in means of Density 1 and Density 2","There is a difference in mean yields for Density 1 and Density 2")

# Determine if we should reject the null hypothesis (Factor1:Factor2-Fertilizer:Density)
ifelse(p.value_fertilizer_density>alpha10,"There is no interaction between the Fertilizer and Density","There is interaction between the Fertilizer and Density")

