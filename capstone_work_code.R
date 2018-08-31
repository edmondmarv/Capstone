setwd("~/School/Springboard/Capstone")
# Load in project data and dplyr and tidyr
library(tidyr)
library(dplyr)
library(readr)

adult <- read_csv("~/School/Springboard/Capstone/Data/adult.data",
                  col_names = FALSE)
View(adult)

# Add column names to data
?colnames
# Also convert imported dataset to table dataframe
census_data <- tbl_df(adult)

colnames(census_data) <- c("Age", "Work_Class", "FNLWGT", "Education","Education_Number", "Marital_Status", "Occupation", "Relationship", "Race", "Sex", "Capital_Gain", "Capital_Loss", "Hours_Per_Week", "Native_Country")

# Checking for missing values in my dataset
summary(census_data)
sum(is.na(census_data$age))
sum(is.na(census_data$Age))
sum(is.na(census_data$Work_Class))


# I notice that all missing values contain a question mark (?). I will have to convert these values into NA values
census_data[ census_data == "?"] <- NA


# Now that all missing values within the data frame have been converted to an NA value, I can now perform analysis on the missing values.
sum(is.na(census_data$Age))
sum(is.na(census_data$Work_Class))
sum(is.na(census_data$FNLWGT))
sum(is.na(census_data$Education))
sum(is.na(census_data$Education_Number))
sum(is.na(census_data$Marital_Status))
sum(is.na(census_data$Occupation))
sum(is.na(census_data$Relationship))
sum(is.na(census_data$Race))
sum(is.na(census_data$Sex))
sum(is.na(census_data$Capital_Gain))
sum(is.na(census_data$Capital_Loss))
sum(is.na(census_data$Native_Country))


# Using a table to provide a list of all possible values in a chosen category and the number of times it was used, is a great way to determine how to fill in any missing values. Also, this method lets you see if there are a duplicate number of values in a chosen category.

sort(table(census_data$Work_Class, useNA="ifany"))
sort(table(census_data$Occupation, useNA="ifany"))
sort(table(census_data$Native_Country, useNA="ifany"))

# NA values will now be filled with its corresponding most repeated value within its column.

census_data$Work_Class[is.na(census_data$Work_Class)] <- "Private"
census_data$Occupation[is.na(census_data$Occupation)] <- "Prof-specialty"
census_data$Native_Country[is.na(census_data$Native_Country)] <- "United-States"

# Checking the sum of NA values within the entire data set will reveal any remaining missing values

sum(is.na(census_data))

# Add column name to predictor values

colnames(census_data)[15] <- "Income"


library(ggplot2)

gender_income_plot <- ggplot(data=census_data, aes(x=Sex, fill=Income)) + geom_bar(position="dodge", alpha=1) + xlab("Gender") + ylab("Count") + ggtitle("Gender and Income - 1994 U.S. Census Survey")
gender_income_plot + theme(panel.background = element_rect(fill='white', colour='black'))

age_income_plot <- ggplot(data=census_data, aes(x=Age, y=..count..)) + geom_point (aes(colour=Income, shape=Income, size=..count..), stat="count") + xlab("Age") + ylab("Count") + ggtitle("Age and Income - 1994 U.S. Census Survey")
age_income_plot + theme(panel.background = element_rect(fill='white', colour='black'))

income_education_plot <- ggplot(data=census_data, aes(x=Education, fill=Income)) + geom_bar(position="dodge", alpha=1, width=0.9) + scale_x_discrete(limits=c("HS-grad","Assoc-acdm","Assoc-voc","Some-college","Bachelors","Masters","Prof-school","Doctorate")) + xlab("Education Attainment") + ylab("Number of Census Participants") + ggtitle("Income by Educational Attainment - 1994 U.S. Census Survey")
income_education_plot + theme(panel.background = element_rect(fill='white', colour='black'), axis.text.x=element_text(angle= -45, hjust = 0))



# Machine Learning Approach
View(census_data)

# Create training and testing sets

set.seed(144)
index = sample(1:nrow(census_data), 0.7*nrow(census_data))
training = census_data[index, ]
testing =  census_data[-index, ]


# Load needed libraries
library(ggplot2)
library(e1071)
library(caret)

# Build model

## I will convert dependent variable to a binomial, 0 or 1. This will help my
## machine learning algorithm easier to implement.

training$class = ifelse(training$Income == "<=50K", 0, 1)
testing$class = ifelse(testing$Income == "<=50K", 0, 1)
training$gender = ifelse(training$Sex == "Female", 0, 1)
testing$gender = ifelse(testing$Sex == "<=Female", 0, 1)

## Next, I will convert 'Education' to a factor type.

training$Education = as.factor(training$Education)
testing$Education = as.factor(testing$Education)

## Now, I will construct a ML model using Naive Bayes.

nb_model = glm(class~Age+Education_Number+gender, data=training, family=binomial(link = "logit"))

# Build predicted model

nb_pred = predict(nb_model, newdata = testing, type="response")

# Accuracy Test

results_nb_model = ifelse(nb_pred<0.5,0,1)
## 78%
results_nb_model = ifelse(nb_pred<0.4,0,1)
## 75.4%
results_nb_model = ifelse(nb_pred<0.3,0,1)
## 68.4%
table(testing$class, results_nb_model)

# Plot

library(effects)
plot(allEffects(nb_model))