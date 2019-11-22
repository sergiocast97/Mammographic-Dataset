# Set directory to current
setwd("C:/Users/sergi/Google Drive/Uni/Y-03/CM3111 - Big Data Analytics/mammographic_dataset/Report")

# Getting table of attribute information
attr <- read.csv('data/attributes.csv')
library(xtable)
print(xtable(attr))

# Importing the data
df <- read.table('data/mammographic_masses.data', header=FALSE, sep=",", na.strings="?")

# Reading the data
df


# Dataset dimensions
nrow(df)
ncol(df)
dim(df)


# Column names
names(df)

# Changing the column names
names(df) <- c("Assessment","Age","Shape","Margin","Density", "Severity")
names(df)
df

# Show some rows
df[1:10,]

# Finding how many unique fields
length(unique(df$Severity))

# Showing unique labels
unique(df$Severity)

# Finding out missing values
sum(is.na(df))
sum(is.na(df$Assessment))
sum(is.na(df$Age))
sum(is.na(df$Shape))
sum(is.na(df$Margin))
sum(is.na(df$Density))
sum(is.na(df$Severity))

# Class distribution
severityFreq <- table(df$Severity)
barplot( 
  severityFreq,
  col = c('steelblue4','coral1'),
  main="Severity Frequency",
  names.arg=c("Benign","Malicious"),
  ylab="Number of patients"
)

# Converting into categorical
df$Assessment <- factor(df$Assessment)
df$Shape      <- factor(df$Shape)
df$Margin     <- factor(df$Margin)
df$Density    <- factor(df$Density, order=TRUE, levels=c("1","2","3","4"))
df$Severity   <- factor(df$Severity)

# Summary for every attribute

summary(df$Assessment)
summary(df$Age)
summary(df$Shape)
summary(df$Margin)
summary(df$Density)
summary(df$Severity)

# Finding how attributes compare to goal
plot(
  data = df,
  Age~Severity,
  col = c('steelblue4','coral1'),
  names.arg=c("Benign","Malicious")
  
)

# Fiding how attributes compare to age

library(datasets)
data <- transform(df, Shape)
boxplot(Age~Shape, data,xlab="Shape",ylab="Age")

data <- transform(df, Margin)
boxplot(Age~Margin, data,xlab="Margin",ylab="Age")

data <- transform(df, Density)
boxplot(Age~Density, data,xlab="Density",ylab="Age")

# Histograms
Age <- df$Age
hist(
  Age,
  xlab="Patient Age",
  col="turquoise3",
)

# Filling misssing values with mean
fillMissings <- function(x) { replace( x, is.na(x), mean(x[!is.na(x)])) }
df$Age <- fillMissings(df$Age)
sum(is.na(df$Age))


# Drop useless column
# Backing up the data frame
df_clean <- df

# Dropping the irrelevant column
df_clean$Assessment <- NULL

# Showing the data frame columns without the dropped one
names(df_clean)

# Deleting rows with misssing factor attributes
df_clean <- df_clean[!is.na(df_clean$Shape),]
df_clean <- df_clean[!is.na(df_clean$Margin),]
df_clean <- df_clean[!is.na(df_clean$Density),]

sum(is.na(df_clean))
nrow(df_clean)

# Dividing the dataset into train and test
library(ISLR)
library(dplyr)
train<-sample_frac( df_clean, 0.7)
nrow(train)
sid<-as.numeric(rownames(train))
test <- df_clean[-sid,]
nrow(test)

# Regression Model
regressionModel <- glm(
  Severity ~ Age+Shape+Margin+Density,
  data=train,
  family=binomial
)
summary(regressionModel)

# Using a Confusion Matrix to verify the used model
predictedValues <- as.integer(regressionModel$fitted.values > 0.5)
error <- mean(predictedValues != train$Severity)
( accuracy <- (1 - error) * 100)

# Prediction on test dataset
probabilities <- predict(
  regressionModel,
  newdata=subset(test,select=c(1,2,3,4)),
  type="response"
)
predictions <- ifelse(probabilities>0.5, 1, 0)
test_accuracy <- mean(predictions==test$Severity)
test_accuracy

# ROC Model
library(ROCR)
pr <- prediction(probabilities, test$Severity)
prf <- performance( pr, measure = "tpr", x.measure = "fpr")
plot(prf,frame=FALSE)
# area under the curve
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Improvements

library(caret)
library(mice)

new_df <- df
new_df$Assessment <- NULL
new_df$Age <- fillMissings(new_df$Age)

# Missing values
nrow(new_df)
summary(new_df)
sum(is.na(new_df))

# Impute missing values using MICE
new_df <- mice(new_df, m=1, maxit=10, method='polyreg', seed=500)
new_df <- complete(new_df,1)
nrow(new_df)
summary(new_df)
sum(is.na(new_df))

# Create a partition, to reserve a validation subset for later testing
inTrain <- createDataPartition(
  y=new_df$Severity, # Have Severity as the classifying value
  p=0.8,             # Assign 80% of the data to training
  list=FALSE
)

# Generating the training subset
new_training <- new_df[inTrain,]
nrow(new_training)

# Generating the validating subset
new_validating <- new_df [-inTrain,]
nrow(new_validating)

# Cross Validation
train_control<- trainControl(
  method="cv", # Use the Cross Validation Method
  number=10    # Divide into 10 subsets
)

# Random Forest
randomForestModel<- train(
  Severity~.,
  data=new_training,
  trControl=train_control,
  method="ranger",
  family=binomial()
)

print(randomForestModel)

# Make Predictions
predictions<- predict(
  randomForestModel,
  new_validating[,-ncol(new_validating)]
)

# Append predictions
new_validating<- cbind(
  new_validating,
  predictions
)

# summarize results
results<- confusionMatrix(
  new_validating$predictions,
  new_validating$Severity
)

cat("Accuracy is: ", sum(diag(results$table))/nrow(new_validating))


