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