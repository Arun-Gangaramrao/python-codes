#####################telechurn RF########################################
# Loading necessary libraries
library(randomForest)
library(caret)

# Loading the dataset
df <- read.csv("C:\\Users\\villain\\OneDrive\\Desktop\\Data analytics\\data mining\\tele.csv")
df$Churn_encoded <- as.character(df$Churn_encoded)
df$Churn_encoded <- as.factor(df$Churn_encoded)
# Splitting the dataset into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(df$Churn_encoded, p=0.7, list=FALSE)
train_data <- df[trainIndex, ]
test_data <- df[-trainIndex, ]
#standardizing the values in training data
preProcValues <- preProcess(train_data, method = c("center", "scale"))
train_data <- predict(preProcValues, train_data)

# Creating the random forest classifier
rf_model <- randomForest(Churn_encoded ~ ., data = train_data, ntree = 100, mtry = sqrt(ncol(train_data)), type = "classification")

# Predicting on the testing data
y_pred <- predict(rf_model, test_data)

# Evaluating the model's performance
accuracy_rm <- confusionMatrix(y_pred, test_data$Churn_encoded)$overall["Accuracy"]
print(paste0("Accuracy: ", accuracy_rm))

f1 <- confusionMatrix(y_pred, test_data$Churn_encoded)$byClass["F1"]
print(paste0("F1 score: ", f1))
RF_precision <- confusionMatrix(y_pred, test_data$Churn_encoded)$byClass['Pos Pred Value']
RF_recall <- confusionMatrix(y_pred, test_data$Churn_encoded)$byClass['Sensitivity']

cat("RF precision:", RF_precision, "\n")
cat("RF recall:", RF_recall, "\n")


# load the necessary libraries
library(data.table)

# creating a sample data.frame
Telecus_RF <- data.frame(TeleCusChurn_RFPredictions_R = y_pred)

# save the data.frame to a CSV file
fwrite(Telecus_RF, "TeleCusChurn_RFPredictions_R.csv")


###############################telechurn KNN#########################################
library(class)

dataSet_1 <- read.csv("C:\\Users\\villain\\OneDrive\\Desktop\\Data analytics\\data mining\\tele.csv")

set.seed(123)
dataSet_1$Churn_encoded <- as.character(dataSet_1$Churn_encoded)
dataSet_1$Churn_encoded <- as.factor(dataSet_1$Churn_encoded)
library(caret)

set.seed(123) # for reproducibility
trainIndex <- createDataPartition(dataSet_1$Churn_encoded, p = 0.7, list = FALSE)
train <- dataSet_1[trainIndex,]
test <- dataSet_1[-trainIndex,]

preProcValues <- preProcess(train, method = c("center", "scale"))
train <- predict(preProcValues, train)
# Define the KNN model and parameters
knn_model <- train(Churn_encoded ~ ., data = train, method = "knn",
                   trControl = trainControl(method = "cv", number = 5),
                   tuneGrid = expand.grid(k = 1:20))

# Predict on the test set
knn_pred <- predict(knn_model, newdata = test)

# create a sample data.frame
Telecus_KNN <- data.frame(TeleCusChurn_KNNPredictions_R = knn_pred)

# save the data.frame to a CSV file
fwrite(Telecus_KNN, "TeleCusChurn_KNNPredictions_R.csv")

# Calculate the accuracy and F-score
knn_acc <- confusionMatrix(knn_pred, test$Churn_encoded)$overall['Accuracy']
knn_fscore <- confusionMatrix(knn_pred, test$Churn_encoded)$byClass['F1']
knn_precision <- confusionMatrix(knn_pred, test$Churn_encoded)$byClass['Pos Pred Value']
knn_recall <- confusionMatrix(knn_pred, test$Churn_encoded)$byClass['Sensitivity']

# Print the results
cat("KNN Accuracy:", knn_acc, "\n")
cat("KNN F-score:", knn_fscore, "\n")
cat("KNN precision:", knn_precision, "\n")
cat("KNN recall:", knn_recall, "\n")



