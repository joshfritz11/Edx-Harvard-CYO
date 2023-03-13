# Edx-Harvard-CYO-Project.R
# Author: Josh Fritz
# Date: 13Mar2023


# Dry Bean Data Download ----
if (!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# Dry Bean Dataset
# https://archive-beta.ics.uci.edu/dataset/602/dry+bean+dataset
# https://archive.ics.uci.edu/ml/machine-learning-databases/00602/DryBeanDataset.zip

options(timeout = 120)

dl <- "DryBeanDataset.zip"
if (!file.exists(dl))
    download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00602/DryBeanDataset.zip", dl)

Dry_Bean_Dataset_file <- "DryBeanDataset/Dry_Bean_Dataset.xlsx"
if (!file.exists(Dry_Bean_Dataset_file))
    unzip(dl, Dry_Bean_Dataset_file)

Dry_Bean_Dataset <- readxl::read_xlsx(Dry_Bean_Dataset_file,
                                      col_names = TRUE,
                                      trim_ws = TRUE)


# Data Analysis ----
# * Box-plots of the different variables ----
if (!require(purrr)) install.packages("purrr")
library(purrr)

Dry_Bean_Dataset |> ggplot(aes(Class, Area)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, Perimeter)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, MajorAxisLength)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, MinorAxisLength)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, AspectRation)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, Eccentricity)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, ConvexArea)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, EquivDiameter)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, Extent)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, Solidity)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, roundness)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, Compactness)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, ShapeFactor1)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, ShapeFactor2)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, ShapeFactor3)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, ShapeFactor4)) +
    geom_boxplot()

# * Summary statistics ----
summary(Dry_Bean_Dataset)

Dry_Bean_Dataset %>%
    split(.$Class) %>%
    map(summary)

Dry_Bean_Dataset %>%
    group_by(.$Class) %>%
    summarize(n())


# Dry Bean Training and Test Set Partitioning ----
# Test set will be 10% of the data.
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y = Dry_Bean_Dataset$Class, times = 1, p = 0.1, list = FALSE)
train_set <- Dry_Bean_Dataset[-test_index, ]
test_set <- Dry_Bean_Dataset[test_index, ]


# Control Models ----
# * Random guessing model ----
bean_classes <- unique(train_set$Class)
test_set_length <- length(test_set$Class)

y_hat_random <- tibble(Class = sample(bean_classes, test_set_length, replace = TRUE))
model_accuracy_results <- tibble(Method = "Random Model", Accuracy = caret::confusionMatrix(factor(y_hat_random$Class), factor(test_set$Class))$overall[["Accuracy"]])

# * Proportional guessing model ----
bean_proportions <- map_df(bean_classes, function(bean_class){
    list(Class = bean_class,
         Proportion = mean(train_set$Class == bean_class))
})

y_hat_proportion <- tibble(Class = sample(bean_classes, test_set_length, replace = TRUE, prob = bean_proportions$Proportion))
model_accuracy_results <- model_accuracy_results |> add_row(Method = "Proportion Model", Accuracy = caret::confusionMatrix(factor(y_hat_proportion$Class), factor(test_set$Class))$overall[["Accuracy"]])


# Multi-layer Perceptron Models ----
if (!require(RSNNS)) install.packages("RSNNS")

# * MLP without pre-processing ----
control <- trainControl(method = "cv", number = 10)

train_mlp <- caret::train(Class ~ .,
                          data = train_set,
                          method = "mlp",
                          tuneGrid = data.frame(size = seq(1, 5, 1)),
                          trControl = control)

y_hat_mlp <- predict(train_mlp, test_set)
model_accuracy_results <- model_accuracy_results |> add_row(Method = "Multi-layer Perceptron", Accuracy = caret::confusionMatrix(y_hat_mlp, factor(test_set$Class))$overall[["Accuracy"]])

# * MLP with pre-processing ----
train_mlp_preprocessing <- caret::train(Class ~ .,
                                        data = train_set,
                                        method = "mlp",
                                        tuneGrid = data.frame(size = seq(10, 15, 1)),
                                        trControl = control,
                                        preProcess = c("center", "scale"))

y_hat_mlp_preprocessing <- predict(train_mlp_preprocessing, test_set)
model_accuracy_results <- model_accuracy_results |> add_row(Method = "Multi-layer Perceptron + Pre-processing", Accuracy = caret::confusionMatrix(y_hat_mlp_preprocessing, factor(test_set$Class))$overall[["Accuracy"]])


# Support Vector Machine Model ----
control <- trainControl(method = "cv", number = 10)

train_svm <- caret::train(Class ~ .,
                          data = train_set,
                          method = "svmLinear",
                          tuneGrid = data.frame(C = seq(30, 35, 1)),
                          trControl = control,
                          preProcess = c("center", "scale"))

y_hat_svm <- predict(train_svm, test_set)
model_accuracy_results <- model_accuracy_results |> add_row(Method = "Support Vector Machine", Accuracy = caret::confusionMatrix(y_hat_svm, factor(test_set$Class))$overall[["Accuracy"]])


# k-Nearest Neighbors Model ----
control <- trainControl(method = "cv", number = 10)

train_knn <- caret::train(Class ~ .,
                          data = train_set,
                          method = "knn",
                          tuneGrid = data.frame(k = seq(10, 15, 1)),
                          trControl = control,
                          preProcess = c("center", "scale"))

y_hat_knn <- predict(train_knn, test_set)
model_accuracy_results <- model_accuracy_results |> add_row(Method = "knn", Accuracy = caret::confusionMatrix(y_hat_knn, factor(test_set$Class))$overall[["Accuracy"]])


# Random Forest Model ----
if (!require(randomForest)) install.packages("randomForest")

control <- trainControl(method = "cv", number = 10)
grid <- data.frame(mtry = c(1, 5, 10, 25, 50, 100))

train_rf <- caret::train(Class ~ .,
                         data = train_set,
                         method = "rf",
                         ntree = 150,
                         trControl = control,
                         tuneGrid = grid,
                         nsamp = 5000,
                         preProcess = c("center", "scale"))

y_hat_rf <- predict(train_rf, test_set)
model_accuracy_results <- model_accuracy_results |> add_row(Method = "Random Forest", Accuracy = caret::confusionMatrix(y_hat_rf, factor(test_set$Class))$overall[["Accuracy"]])


# Naive Bayes Classifier Model ----
if (!require(naivebayes)) install.packages("naivebayes")

control <- trainControl(method = "cv", number = 10)

train_naive_bayes_classifier <- caret::train(Class ~ .,
                                             data = train_set,
                                             method = "naive_bayes",
                                             trControl = control,
                                             preProcess = c("center", "scale"))

y_hat_naive_bayes_classifier <- predict(train_naive_bayes_classifier, test_set)
model_accuracy_results <- model_accuracy_results |> add_row(Method = "Naive Bayes", Accuracy = caret::confusionMatrix(y_hat_naive_bayes_classifier, factor(test_set$Class))$overall[["Accuracy"]])


# Output results ----
model_accuracy_results