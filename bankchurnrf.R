######################CreditcardChurn RF#####################################
# Loading necessary libraries
library(randomForest)
library(caret)

# Loading the dataset
df <- read.csv("C:\\Users\\villain\\OneDrive\\Desktop\\Data analytics\\data mining\\creditCardchurner.csv")
df$Attrition_Flag_encoded <- as.character(df$Attrition_Flag_encoded)
df$Attrition_Flag_encoded <- as.factor(df$Attrition_Flag_encoded)
# Splitting the dataset into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(df$Attrition_Flag_encoded, p=0.7, list=FALSE)
train_data <- df[trainIndex, ]
test_data <- df[-trainIndex, ]

# Creating the random forest classifier
rf_model <- randomForest(Attrition_Flag_encoded ~ ., data = train_data, ntree = 100, mtry = sqrt(ncol(train_data)), type = "classification")

# Predicting on the testing data
y_pred <- predict(rf_model, test_data)

# Evaluating the model's performance
accuracy_rm <- confusionMatrix(y_pred, test_data$Attrition_Flag_encoded)$overall["Accuracy"]
print(paste0("Accuracy: ", accuracy_rm))

f1 <- confusionMatrix(y_pred, test_data$Attrition_Flag_encoded)$byClass["F1"]
print(paste0("F1 score: ", f1))
RF_precision <- confusionMatrix(y_pred, test_data$Attrition_Flag_encoded)$byClass['Pos Pred Value']
RF_recall <- confusionMatrix(y_pred, test_data$Attrition_Flag_encoded)$byClass['Sensitivity']

cat("RF precision:", RF_precision, "\n")
cat("RF recall:", RF_recall, "\n")
# load the necessary libraries
library(data.table)

# create a sample data.frame
Creditcardcus_RF <- data.frame(CreditcardCusChurn_RFPredictions_R = y_pred)

# save the data.frame to a CSV file
fwrite(Creditcardcus_RF, "CreditcardCusChurn_RFPredictions_R.csv")

#######################creditcardChurn KNN####################################

library(class)

dataSet_1 <- read.csv("C:\\Users\\villain\\OneDrive\\Desktop\\Data analytics\\data mining\\creditCardchurner.csv")
set.seed(123)

dataSet_1$Attrition_Flag_encoded <- as.character(dataSet_1$Attrition_Flag_encoded)
dataSet_1$Attrition_Flag_encoded <- as.factor(dataSet_1$Attrition_Flag_encoded)
library(caret)

set.seed(123) # for reproducibility
trainIndex <- createDataPartition(dataSet_1$Attrition_Flag_encoded, p = 0.7, list = FALSE)
train <- dataSet_1[trainIndex,]
test <- dataSet_1[-trainIndex,]


# Define the KNN model and parameters
knn_model <- train(Attrition_Flag_encoded ~ ., data = train, method = "knn",
                   trControl = trainControl(method = "cv", number = 5),
                   tuneGrid = expand.grid(k = 1:20))

# Predict on the test set
knn_pred <- predict(knn_model, newdata = test)

# Calculate the accuracy and F-score
knn_acc <- confusionMatrix(knn_pred, test$Attrition_Flag_encoded)$overall['Accuracy']
knn_fscore <- confusionMatrix(knn_pred, test$Attrition_Flag_encoded)$byClass['F1']
knn_precision <- confusionMatrix(knn_pred, test$Attrition_Flag_encoded)$byClass['Pos Pred Value']
knn_recall <- confusionMatrix(knn_pred, test$Attrition_Flag_encoded)$byClass['Sensitivity']
# Print the results
cat("KNN Accuracy:", knn_acc, "\n")
cat("KNN F-score:", knn_fscore, "\n")
cat("KNN precision:", knn_precision, "\n")
cat("KNN recall:", knn_recall, "\n")

# create a sample data.frame
Creditcardcus_KNN <- data.frame(CreditcardCusChurn_KNNPredictions_R = knn_pred)

# save the data.frame to a CSV file
fwrite(Creditcardcus_KNN, "CreditcardCusChurn_KNNPredictions_R.csv")



