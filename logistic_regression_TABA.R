
# libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(psych)
library(corrplot)
library(ResourceSelection)

################################################################################

# Loading dataset 
dia <- read.csv("C:/Users/villain/Downloads/Diabetes Dataset.csv")
head(dia)

################################################################################

#Dataset description
colnames(dia)

# Data structure
#Figure 16
str(dia)
# Dataset summary
#Figure 17
summary(dia)
# Display unique value
unique(dia$CLASS)
table(dia$CLASS)

################################################################################

# Splitting the values of P
df_predict <- subset(dia, CLASS == 'P')
head(df_predict)

# Dropping the P values
df_model <- subset(dia, CLASS != 'P')

################################################################################

#Data validation
# checking for NAs and null values in the data
#Figure 18
sum(is.na(df_model))
na_values <- data.frame(colSums(is.na(df_model)))
head(na_values, 25)


################################################################################

# Data Visualization

# Selecting numeric variables
numeric_vars <- c("AGE", "Urea", "Cr", "HbA1c", "Chol", "TG", "HDL", "LDL", "VLDL", "BMI")


# Creating correlation matrix after selecting and storing the numeric variables
#Figure 19
cor_matrix <- cor(df_model[numeric_vars])
cor_matrix
corrplot(cor_matrix, method = "square", type = "upper", tl.col = "red", tl.srt = 45)

#Scatter plot
pairs(df_model[, c("AGE", "Urea", "Cr", "HbA1c", "Chol", "TG", "HDL", "LDL", "VLDL", "BMI")])

################################################################################

# Data Pre processing
# removing ID and no_of_patient as it make no difference to diabetic
df_model <- df_model %>% select(-c(ID, No_Pation))
head(df_model)

# Converting categorical data into binary data
df_model <- df_model %>% 
  mutate(Gender = case_when(Gender == "M" ~ 1, Gender == "F" ~ 0),
         CLASS = case_when(CLASS == "Y" ~ 1, CLASS == "N" ~ 0))
head(df_model)

################################################################################

#Split the data-set into train and test sets
set.seed(123)
split <- createDataPartition(y = df_model$CLASS, p = 0.7, list = FALSE)
train1 <- df_model[split, ]
test1 <- df_model[-split, ]

################################################################################

#Creating a function to evaluate the model
modelOutput <- function(test,pred){
  # Calculating accuracy and F1 score
  conf_matrix <- table(test$CLASS, pred)
  accuracy <- mean(pred == test$CLASS)
  precision <- sum(pred[test$CLASS == 1] == 1) / sum(pred == 1)
  recall <- conf_matrix[2,2]/sum(conf_matrix[2,])
  f1 <- 2 * precision * recall / (precision + recall)
  
  metrics_df <- data.frame(
    Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
    Value = c(round(accuracy, 3), round(precision, 3), round(recall, 3), round(f1, 3))
  )
  return(head(metrics_df))
}

################################################################################

# MODEL 1
#Figure 21, 22 and 23
model_1 <- glm(CLASS ~ ., data = train1, family = binomial)
summary(model_1)
par(mfrow=c(2,2))
plot(model_1)
pred <- predict(model_1, newdata = test1, type = "response")
pred_class <- ifelse(pred > 0.5, 1, 0)
modelOutput(test1,pred_class)

################################################################################

# MODEL 2
#Figure 24, 25 and 26
model_2 <- glm(CLASS ~ HbA1c + Chol + TG + BMI, family = binomial, data = train1)
summary(model_2)
par(mfrow=c(2,2))
plot(model_2)
pred <- predict(model_2, newdata = test1, type = "response")
pred_class <- ifelse(pred > 0.5, 1, 0)
modelOutput(test1,pred_class)

################################################################################

#visualizing the data 
#Figure 27
histogram <- function(x)
{
  title <- paste(deparse(substitute(x), 500), collapse="\n")
  sdx <- sd(x) 
  mx <- mean(x)
  hist(x, prob=TRUE,main=paste("Histogram of ",title), 
       xlim=c(mx-3*sdx, mx+3*sdx), ylim=c(0, 0.5/sdx)) 
  curve(dnorm(x, mean=mx, sd=sdx), col='green', lwd=3, add=TRUE)
}

par(mfrow=c(2,2))
histogram(log(df_model$HbA1c))
histogram(df_model$HbA1c)
histogram(df_model$HbA1c)

histogram(df_model$Chol)
histogram(sqrt(df_model$Chol))

histogram(log(df_model$TG)) 
histogram(df_model$TG)
histogram(sqrt(df_model$TG))

histogram(log(df_model$BMI))
histogram(df_model$BMI)
histogram(sqrt(df_model$BMI)) 

################################################################################

#MODEL 3
 #Figure 28,29 and 30
model_3 <- glm(CLASS ~ log(HbA1c) + log(Chol) +log(TG) + log(BMI), family = binomial, data = train1)
summary(model_3)
par(mfrow=c(2,2))
plot(model_3)
pred <- predict(model_3, newdata = test1, type = "response")
pred_class <- ifelse(pred > 0.5, 1, 0)
modelOutput(test1,pred_class)

################################################################################

# Performing Hosmer-Lemeshow test
#Figure 31
hl_test <- hoslem.test(train1$CLASS, predict(model_2, type = "response"))
hl_test


################################################################################
# final selected model
#testing the final model on the 'Diabetes==P' cases 
#Figure 32
predicted_values_p <- predict(model_2, newdata = df_predict, type = "response")
prob_diabetic <- sum(predicted_values_p)/length(predicted_values_p)
cat("Probability of diagnosing the 'P' class cases as diabetic: ", prob_diabetic, "\n")

################################################################################




