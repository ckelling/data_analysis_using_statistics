###
### Data Analysis Using Statistics
###     from Writing in Biology by Karin Knisely
### Replication Code
###
### Author:        Claire Kelling
### Last modified: 11/22/20
###

# Clear the workspace
rm(list=ls())

# Load libraries
library(ggplot2) #for plotting
library(gridExtra) #for plotting more than one plot together
library(readxl) #for reading in data
library(tidyverse) #for manipulating data

# Set Working Directory
setwd("C:/Users/ckell/Desktop/Google Drive/01_Penn State/2020-2021/biology_stats_chapter/biol_r_code/data_analysis_using_statistics")
setwd("~/data_analysis_using_statistics")

###
### Section 6.4: Data Exploration
###
# Load Data
alcohol_data <- read_xlsx("blackworm_data.xlsx", sheet = "Alcohol") %>% as.data.frame()
caffeine_data <- read_xlsx("blackworm_data.xlsx", sheet = "Caffeine")%>% as.data.frame()

# Exploratory Data Analysis
## How many rows and columns
dim(alcohol_data)
## Summary of dataset
summary(alcohol_data)

## Removing missing data
alcohol_data <- alcohol_data %>% filter(!is.na(pulsation_change))
caffeine_data <- caffeine_data %>% filter(!is.na(pulsation_change))

## Determine type of variables
class(alcohol_data$treatment)
## Changing variable type
alcohol_data$treatment <- as.factor(alcohol_data$treatment)
## Quick view of dataset
head(alcohol_data, n = 5)

####
#### Histograms
####
#Histogram of data
hist(alcohol_data$pulsation_change, breaks = 10, main = "Histogram of Pulsation Change, \nAlcohol Treatment, 10 breaks",
     xlab = "Change in pulsation rate")
hist(alcohol_data$pulsation_change, breaks = 50, main = "Histogram of Pulsation Change, \nAlcohol Treatment, 50 breaks",
     xlab = "Change in pulsation rate")
#saved at 600x500 for chapter

####
#### Boxplots
####
#Boxplots of each concentration
alcohol_plot <- ggplot(alcohol_data, aes(x=as.factor(concentration_perc), y=pulsation_change)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, geom="line", aes(group=1), col = "blue")  + 
  stat_summary(fun=mean, geom="point", col = "blue")+
  labs(x= "Concentration (%)", y = "Change in pulsation rate", title = "Treatment: Alcohol")
caffeine_plot <- ggplot(caffeine_data, aes(x=as.factor(concentration_mm), y=pulsation_change)) + 
  geom_boxplot()+ 
  stat_summary(fun=mean, geom="line", aes(group=1), col = "blue")  + 
  stat_summary(fun=mean, geom="point", col = "blue")+
  labs(x= "Concentration (mM)", y = "Change in pulsation rate", title = "Treatment: Caffeine")

#Plot alcohol and caffeine together
grid.arrange(alcohol_plot, caffeine_plot, nrow=1)
#export at 750x300 for chapter

####
#### Scatterplots
####
#Scatterplot of data
plot(alcohol_data$concentration_perc, alcohol_data$pulsation_change,
     xlab = "Concentration (%)", ylab = "Change in pulsation rate", 
     main = "Scatterplot of Concentration vs Change in Pulsation, \nAlcohol Treatment")
#save at 700x500 for chapter


#clean up before hypothesis testing
rm(alcohol_plot, caffeine_plot)


###
### Section 6.5: Hypothesis Testing
###

###
### A t Test for one sample mean
###

#alcohol, alternative: two-sided
t.test(x=alcohol_data$pulsation_change, alternative = "two.sided", mu = 0, conf.level = 0.99)

###
### A t Test for two sample means
###

#subset the data
one_perc_data <- alcohol_data %>% filter(concentration_perc == 1)
two_perc_data <- alcohol_data %>% filter(concentration_perc == 2)

#run the t-test
t.test(x=one_perc_data$pulsation_change, y = two_perc_data$pulsation_change, 
       alternative = "two.sided", mu = 0)

###
### Chi-square test for tree proportions
### 
# one-way
chisq.test(x = c(53, 74, 3), 
           p = c(0.4, 0.5, 0.1))

# two-way
chisq.test(x = matrix(c(212, 50, 32, 61), nrow = 2), correct = FALSE)


###
### Section 6.5: Is there a relationship between alcohol or caffeine levels and circulation in blackworms?
###
#alcohol, alternative: two-sided
t.test(x=alcohol_data$pulsation_change, alternative = "two.sided", mu = 0, conf.level = 0.99)

#caffeine, alternative: two-sided
t.test(x=caffeine_data$pulsation_change, alternative = "two.sided", mu = 0, conf.level = 0.99)


#alcohol, alternative: two-sided
t.test(x=alcohol_data$pulsation_change, alternative = "greater", mu = 0, conf.level = 0.99)

#caffeine, alternative: two-sided
t.test(x=caffeine_data$pulsation_change, alternative = "greater", mu = 0, conf.level = 0.99)



###
### Correlation
###
cor(as.numeric(as.character(alcohol_data$concentration_perc)), alcohol_data$pulsation_change)
cor(as.numeric(as.character(caffeine_data$concentration_mm)), caffeine_data$pulsation_change)


### Scatterplot with line of best fit

alcohol_data$concentration_perc <- as.numeric(as.character(alcohol_data$concentration_perc))
caffeine_data$concentration_mm <- as.numeric(as.character(caffeine_data$concentration_mm))

plot(alcohol_data$concentration_perc, alcohol_data$pulsation_change,
     xlab = "Concentration (%)", ylab = "Change in pulsation rate", 
     main = "Scatterplot of Concentration vs Change in Pulsation, \nAlcohol Treatment")
abline(lm(alcohol_data$pulsation_change~alcohol_data$concentration_perc), col="red")
#save at 700x500 for chapter

plot(caffeine_data$concentration_mm, caffeine_data$pulsation_change,
     xlab = "Concentration (mM)", ylab = "Change in pulsation rate", 
     main = "Scatterplot of Concentration vs Change in Pulsation, \nCaffeine Treatment")
abline(lm(caffeine_data$concentration_mm~caffeine_data$pulsation_change), col="red")
#save at 700x500 for chapter
