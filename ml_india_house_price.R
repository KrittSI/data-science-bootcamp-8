## install package 
## install library

install.packages("caret")
install.packages("tidyverse")
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
library(caret)
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)

## 0. prep data/ get data /clean data (No missing value)
## 1. split data
## 2. train model
## 3. score model aka. prediction
## 4. evaluate model

## step 0. prep data
## get and combine data
hp1 <- read_excel("house_price_india.xlsx", sheet = 1)
hp2 <- read_excel("house_price_india.xlsx", sheet = 2)
house_price <- bind_rows(hp1, hp2)
View(house_price)

## check missing value or N/A
house_price %>%
  complete.cases() %>%
  mean()           ## If no missing value, mean = 1

## visualize price distribution
ggplot(data = house_price,
       mapping = aes(x = Price)) +
  geom_histogram()

## check zero or minus in datapoints
## adjust right skewed distribution to normal distribution
mean(house_price$Price <= 0)
house_price$Price <- log(house_price$Price)
clean_df <- house_price

## Then re-check price distribution 
ggplot(data = clean_df,
       mapping = aes(x = Price)) +
  geom_histogram()
 

## 1. split data 80% train, 20% test
split_data <- function(df) {
  set.seed(42)
  n <- nrow(df)
  train_id <- sample(1:n, size = 0.8*n)
  train_df <- df[train_id, ]
  test_df <- df[- train_id, ]
  return(list(training = train_df,
              testing = test_df))
}

prep_data <- split_data(clean_df)
train_df <- prep_data[[1]]
test_df <- prep_data[[2]]

## 2. train model with LM model
set.seed(42)
lm_model <- train(Price ~ . ,
                  data = train_df,
                  method = "lm")
lm_model

## 3. score model
p <- predict(lm_model, newdata = test_df)

## 4. evaluate model
## mean absolute error, root mse
(mae <- mean(abs(p - test_df$Price)))
(rmse <- sqrt(mean((p - test_df$Price)**2)))

