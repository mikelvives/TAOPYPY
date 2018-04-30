'This is a script that, given a dataset, will run all the necessary basic feature engineering and will train a glm model using these treated variables'
# Process to be followed:
#   1 - We'll deal first with the numerical variables.
#       1.1 - First, we'll see how correlated are the variables. If several variables are highly correlated, it is only useful to keep one of them and remove the rest.
#             This can be done or via the correlation matrix or via a PCA and analyzing a biplot to check which components go in the same direction. In this way, we select the
#             variables that will be used in our model
#       1.2- Once we have selected the variables that will be used in the model, it usually is a good idea to scale our variables so the model can treat them as best as possible. For 
#            that purpose, we will try to convert each variable to a normal distributed variable using a boxcox transformation (will return the optimal lambda to use when applying a transformation.
#           Once we have a normal variable, we can apply the usual normal transformation (substract the sample mean and divide by the sample std)
#   2 - Then it is time to deal with the categorical variables. 
#       2.1 - The best way to treat categorical variables is to check wether we have independency between our variables or not. This can be done via a chi-squared test or an anova test.

library(aod)
library(ggplot2)
library(data.table)
library(ROCR)
library(caTools)
library(corrplot)
library(ggfortify)
library(nortest)
library(forecast)
library(caret)
library(dplyr)
library(janeaustenr)
library(tidytext)

setwd("/Users/felix/Desktop/Code/Postgrau/Capstone_propio/TAOPYPY/")
source("./R/utility_functions.R")

# Read the data and grab the interesting columns

data <- read.csv("./converted_data.csv", sep = ';')
data$label <- as.factor(data$label)
View(data)

set.seed(101) 
sample <- sample.split(data$label, SplitRatio = 0.75)

# Training Data
train = subset(data, sample == TRUE)
# Testing Data
test = subset(data, sample == FALSE)

# ------------------------------------------------------------------------------ #
# MODEL TRAINING WILL OUR TREATED DATA.

model <- glm(label ~ ., data = train, family = 'binomial')
summary(model)

model <- optimize_model(model, data)

preds <- predict(model, test, type = "response")

saveRDS(model1, file = "model_norm.rds")
saveRDS(model2, file = "model_notnorm.rds")

# VIF is the Variance Inflation Factor and it helps to detect multicolliniarity in our data. Tipically a VIF > 5 is consifered high multicollinearity, but it has to be 
# dealt with when it is > 10.
# car::vif(model1)
# car::vif(model2)

# ------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------ #


# LOAD TEST DATA AND PREDICT OVER IT.
test_data <- read.csv("./Data/data_last_weeks.csv", sep = ';')
summary(test_data)
testfacts <- sapply(test_data, is.factor)

test_num <- test_data[,c('num_applications', 'num_offers_company', 'num_refused', 'num_preselecteds', 'num_chats_success', 'response_rate')]
test_facts <- test_data[testfacts]
test_num[,c(1:6)] <- test_num[,c(1:6)] + 1

lambdas <- get_lambdas(test_num)

# Create the new columns (normalized)
i <- 1
for (col in names(test_num)) {
  test_num[,paste(col, 'normal', sep = '_')] <- BoxCox(test_num[,col], lambdas[[i]])
  i <- i + 1
}
test1 <- test_num[,grep("normal", names(test_num))]
test1[,names(test_facts)[-2]] <- test_facts[-2]

test2 <- test_num[,-grep("normal", names(test_num))]
test2[,names(test_facts)[-2]] <- test_facts[-2]

predictions_norm <- predict(model1, test1, type = 'response')
predictions <- predict(model2, test2, type = 'response')


test_data$Predictions_norm <- predictions_norm
test_data$Predictions <- predictions

fwrite(test_data, "./Data/predictions_last_weeks.csv", sep = ';')

