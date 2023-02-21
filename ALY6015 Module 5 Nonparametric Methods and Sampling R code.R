#---------------------- Week_5_Module_5 R Script ----------------------#

print("Author : Nikshita Ranganathan")
print("Module 5 Assignment - Nonparametric Statistical Methods and Sampling")
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

# Section 13-2 
# Q6. Game Attendance

# State the hypotheses
# H0: Median for the paid attendance at 20 local football games is 3000
# H1: Median for the paid attendance at 20 local football games is not equal to 3000

# Set significance level
alpha1 <- 0.05

# Claim
median1 <- 3000

# Paid attendance for these 20 local football games
attendance <- c(6210, 3150, 2700, 3012, 4875, 3540, 6127, 2581, 2642, 2573, 2792, 2800, 2500, 3700, 6030, 5437, 2758, 3490, 2851, 2720)

difference <- attendance - median1

# Determine the games with attendance more than 3000 
# exclude 0 values; + sign if value is greater than median, - sign is less
pos1 <- length(difference[difference > 0])

# Determine the games with attendance less than 3000
neg1 <- length(difference[difference < 0])

# Run the test and save the results to the result variable
result1 <- binom.test(x = c(pos1, neg1), alternative = 'two.sided')
result1

# View the p-value
result1$p.value

# Determine if we should reject the null hypothesis
ifelse(result1$p.value > alpha1,"Fail to reject the null hypothesis","Reject the null hypothesis")

# Q10. Lottery Ticket Sales

# State the hypotheses
# H0: Median of lottery tickets sales is greater than equal to 200
# H1: Median of lottery tickets sales is less than 200

# Set significance level
alpha2 <- 0.05

# Claim
median2 <- 200

# Determine when the lottery tickets are greater than 200 
# exclude 0 values; + sign if value is greater than median, - sign is less
pos2 <- 25

# Determine the games with attendance less than 3000
neg2 <- 15

# Run the test and save the results to the result variable
result2 <- binom.test(x = c(pos2, neg2), alternative = 'less')
result2

# View the p-value
result2$p.value

# Determine if we should reject the null hypothesis
ifelse(result2$p.value > alpha2,"Fail to reject the null hypothesis","Reject the null hypothesis")

# Section 13-3 
# Q4. Lengths of Prison Sentences

# State the Hypothesis
# H0: There is no difference in the sentence received by each gender
# H1: There is a difference in the sentence received by each gender

# Set Significance Level
alpha3 <- 0.05

# Create vectors of Gender-based Values
Male <- c(8, 12, 6, 14, 22, 27, 32, 24, 26, 19, 15, 13)
Female <- c(7, 5, 2, 3, 21, 26, 30, 9, 4, 17, 23, 12, 11, 16)

# Run the Wilcoxon Rank Sum Test
result3 <- wilcox.test(x = Male, y = Female, alternative = 'two.sided', correct = FALSE)
result3

# View the p-value
result3$p.value

# Compare the p-value to alpha to decide the result
ifelse(result3$p.value > alpha3,"Fail to reject the null hypothesis","Reject the null hypothesis")

# Q8. Winning Baseball Games

# H0: There is no difference in the number of wins by each league’s Eastern Division (American League and National League)
# H1: There is a difference in the number of wins by each league’s Eastern Division (American League and National League)

# Set Significance Level
alpha4 <- 0.05

# Create vectors of League-based Values
NL <- c(89, 96, 88, 101, 90, 91, 92, 96, 108, 100, 95)
AL <- c(108, 86, 91, 97, 100, 102, 95, 104, 95, 89, 88, 101)

# Wilcox Rank Test
result4 <- wilcox.test(x = NL, y = AL, alternative = 'two.sided', correct = FALSE)
result4

# View the p-value
result4$p.value

# Compare the p-value to alpha to decide the result
ifelse(result4$p.value > alpha4,"Fail to reject the null hypothesis","Reject the null hypothesis")

# Section 13-4
# Q5. ws = 13, n = 15, α = 0.01, two-tailed
ws1 <- 13
CV1 <- qsignrank(0.01/2, 15, lower.tail = TRUE)
CV1

# Compare the critical value to ws to decide the result
ifelse(CV1 <= ws1,"Fail to reject the null hypothesis","Reject the null hypothesis")

# Q6. ws = 32, n = 28, α = 0.025, one-tailed
ws2 <- 32
CV2 <- qsignrank(0.025, 28, lower.tail = TRUE)
CV2

# Compare the critical value to ws to decide the result
ifelse(CV2 <= ws2,"Fail to reject the null hypothesis","Reject the null hypothesis")

# Q7. ws = 65, n = 20, α = 0.05, one-tailed
ws3 <- 65
CV3 <- qsignrank(0.05, 20, lower.tail = TRUE)
CV3

# Compare the critical value to ws to decide the result
ifelse(CV3 <= ws3,"Fail to reject the null hypothesis","Reject the null hypothesis")

# Q8. ws = 22, n = 14, α = 0.10, two-tailed
ws4 <- 22
CV4 <- qsignrank(0.1/2, 14, lower.tail = TRUE)
CV4

# Compare the critical value to ws to decide the result
ifelse(CV4 <= ws4,"Fail to reject the null hypothesis","Reject the null hypothesis")

# Section 13-5
# Q2. Mathematics Literacy Scores

# State the Hypothesis
# H0: There is no difference in the mean mathematics literacy scores in different parts of the world
# H1: There is a difference in the mean mathematics literacy scores in different parts of the world

# Set Significance Level
alpha5 <- 0.05

# Create dataframes for all the Regions
WesternHemisphere <- data.frame(Scores = c(527,406,474,381,411), Region = rep('Western Hemisphere', 5))
Europe <- data.frame(Scores = c(520,510,513,54,496), Region = rep('Europe', 5))
EasternAsia <- data.frame(Scores = c(523,547,547,391,549), Region = rep('Eastern Asia', 5))

# Combine the dataframes in one
data_score <- rbind(WesternHemisphere, Europe, EasternAsia)
data_score

# Run the Kruskal-Wallis Test and save the result to the result variable
result5 <- kruskal.test(Scores ~ Region, data = data_score)
result5

# View the p-value
result5$p.value

# Compare the p-value to alpha to decide the result
ifelse(result5$p.value > alpha5,"Fail to reject the null hypothesis","Reject the null hypothesis")

# Section 13-6

# State the Hypothesis
# H0: There is no relationship among the transport types
# H1: There is a relationship among the transport types

# Set Significance Level
alpha6 <- 0.05

# Create the dataframes for all types of transport 
City <- c(1, 2, 3, 4, 5, 6)
Subway <- c(845, 494, 425, 313, 108, 41)
Rail <- c(39, 291, 142, 103, 33, 38)

# Combining the dataframes into one
data_transport <- data.frame(City = City, Subway = Subway, Rail = Rail)
data_transport

# Spearman Rank Correlation Coefficient Test
result6 <- cor.test(x = data_transport$Rail, y = data_transport$Subway, method = 'spearman')
result6

# View the test statistic and p-value
result6$p.value
result6$estimate

# Compare the p-value to alpha to decide the result
ifelse(result6$p.value > alpha6,"Fail to reject the null hypothesis","Reject the null hypothesis")

# Section 14-3
# Q16. Prizes in Caramel Corn Boxes

# Number of times to repeat the experiment
n1 <- 40  

# Vector to store results
results1 <- numeric(n1)  

for (i in 1:n1) {
  prizes <- c(1, 2, 3, 4)  # numbers representing the prizes
  boxes <- numeric(0)  # vector to store boxes purchased
  while (length(unique(boxes)) < 4) {
    box <- sample(prizes, 1)  # select a random prize
    boxes <- c(boxes, box)  # add the box to the vector
  }
  results1[i] <- length(boxes)  # store the number of boxes purchased
}

# Calculate the average number of boxes purchased
mean(results1)  

# Q18. Lottery Winner

# Number of times to repeat the experiment
n2 <- 30  

# Vector to store results
results2 <- numeric(n2)  

for (i in 1:n2) {
  letters <- c("b", "i", "g")  # letters needed to spell "big"
  ticket_count <- 0  # counter for number of tickets purchased
  while (TRUE) {
    ticket_count <- ticket_count + 1  # increment ticket counter
    ticket <- sample(letters, 4, replace = TRUE, prob = c(0.6, 0.3, 0.1))  # simulate ticket purchase
    if (all(letters %in% ticket)) {
      results2[i] <- ticket_count  # store number of tickets purchased
      break  # exit while loop if word "big" is spelled
    }
  }
}

# Calculate the average number of tickets purchased
mean(results2)  

