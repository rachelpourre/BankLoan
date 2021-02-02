##
## Loan Default Prediction
##

# Libraries
library(tidyverse)
library(DataExplorer)
library(caret)

## Read in the Data
system.time(default <- read_csv("~/Downloads/train_v2.csv"))
system.time(default <- vroom::vroom("~/Downloads/train_v2.csv"))

test <- vroom::vroom("~/Downloads/test_v2.csv")
train <- vroom::vroom("~/Downloads/train_v2.csv")
default <- bind_rows(train, test)

## Misiing values
plot_missing(default)
is.na(default) %>% colMeans() %>%
  round(., 3)
sum(is.na(train) %>% rowMeans()>0)

# We would trow out most of the data
#   Let's predict where the NAs are are fill it in (amputation)
# Do the mean (less correlation), linear regression (higher correlation)
# Use a model to predict, then randomly select a sample with the variance 
# Or K-nearest neighbors amputation/regression/whatever
#   Choose the k-values that look the most similiar to the NA observation in 
#     order to fill it in with their average

# CHOOSE A METHOD, PATHETIC C

default <- deafult[,which(colSums(is.na(default))==0)]

# preProcessing
#   Take the predictors and smash them together (there are 800 column rn)
#   Best possible combination: principal components
#   Eigen vector decomposition, optimize xs into fewer numbers

#preProcess(x=default, method="pca")
# process compenet method

# Juts zeros???
#table(defaul$f33)

# DO AMPUTATIONS FOR COLUMNS THAT JUST HAVE ZEROS!
my.preProcess1 <- preProcess(x=default, method=c("zv","pca"))
default20 <- predict(my.preProcess1, newdata=default)
dim(default20)

# Control how many variables I want to get down to
my.preProcess <- preProcess(x=default, method=c("zv","knnImpute","pca"),
           pcaComp=50)
default2 <- predict(my.preProcess, newdata=default)
dim(default2)

# Some columns (f137 -- it goes from 0 to 20 integers, f138 for example; 391 is even crazier)

cor(train$f391, train$loss, use="complete")
# Basically zero

# Other way to just keep the variables that we want is by seeing the correlation for each variables
#   In pre-process, we can also use "rangeBound" to scale ridiculously huge numbers
#     For most methods, it does not matter

write.csv(file="StopRunning", x=default2)












