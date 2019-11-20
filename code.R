# Set directory to current
setwd("C:/Users/sergi/Google Drive/Uni/Y-03/CM3111 - Big Data Analytics/Coursework/Report")

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
df$Assessment <- as.factor(df$Assessment)
df$Shape      <- as.factor(df$Shape)
df$Margin     <- as.factor(df$Margin)
#df$Density    <- as.factor(df$Density, order=TRUE, levels =c('High', 'Iso', 'Low', 'Fat-containing'))
df$Density    <- as.factor(df$Density)
df$Severity   <- as.factor(df$Severity)

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