##########################################################
# Machine Learning Models
##########################################################

if (!require(RSNNS)) install.packages("RSNNS")
if (!require(randomForest)) install.packages("randomForest")
if (!require(naivebayes)) install.packages("naivebayes")
if (!require(evtree)) install.packages("evtree")


# Control Models ----
# Random guessing model
bean_classes <- unique(train_set$Class)
test_set_length <- length(test_set$Class)

y_hat_random <- tibble(Class = sample(bean_classes, test_set_length, replace = TRUE))
model_accuracy_results <- tibble(Method = "Random Model", Accuracy = confusionMatrix(factor(y_hat_random$Class), factor(test_set$Class))$overall[["Accuracy"]])

y_hat_random |> ggplot(aes(Class)) +
    geom_bar()
train_set |> ggplot(aes(Class)) +
    geom_bar()

# Proportional guessing model
bean_proportions <- map_df(bean_classes, function(bean_class){
    list(Class = bean_class,
         Proportion = mean(train_set$Class == bean_class))
})

y_hat_proportion <- tibble(Class = sample(bean_classes, test_set_length, replace = TRUE, prob = bean_proportions$Proportion))
model_accuracy_results <- model_accuracy_results |> add_row(Method = "Proportion Model", Accuracy = confusionMatrix(factor(y_hat_proportion$Class), factor(test_set$Class))$overall[["Accuracy"]])

y_hat_proportion |> ggplot(aes(Class)) +
    geom_bar()
train_set |> ggplot(aes(Class)) +
    geom_bar()


# Multi-layer Perceptron Model ----
control <- trainControl(method = "cv", number = 10)

train_mlp <- train(Class ~ .,
                   data = train_set,
                   method = "mlp",
                   tuneGrid = data.frame(size = seq(1, 5, 1)),
                   trControl = control)

y_hat_mlp <- predict(train_mlp, test_set)
model_accuracy_results <- model_accuracy_results |> add_row(Method = "Multi-layer Perceptron", Accuracy = confusionMatrix(y_hat_mlp, factor(test_set$Class))$overall[["Accuracy"]])

ggplot(train_mlp)

train_mlp_preprocessing <- train(Class ~ .,
                                 data = train_set,
                                 method = "mlp",
                                 tuneGrid = data.frame(size = seq(1, 11, 2)),
                                 trControl = control,
                                 preProcess = c("center", "scale"))

y_hat_mlp_preprocessing <- predict(train_mlp_preprocessing, test_set)
model_accuracy_results <- model_accuracy_results |> add_row(Method = "Multi-layer Perceptron + Pre-processing", Accuracy = confusionMatrix(y_hat_mlp_preprocessing, factor(test_set$Class))$overall[["Accuracy"]])

ggplot(train_mlp_preprocessing)


# Support Vector Machine Model ----
control <- trainControl(method = "cv", number = 10)

train_svm <- train(Class ~ .,
                   data = train_set,
                   method = "svmLinear",
                   tuneGrid = data.frame(C = seq(1, 33, 2)),
                   trControl = control)

y_hat_svm <- predict(train_svm, test_set)
model_accuracy_results <- model_accuracy_results |> add_row(Method = "Support Vector Machine", Accuracy = confusionMatrix(y_hat_svm, factor(test_set$Class))$overall[["Accuracy"]])

ggplot(train_svm)


# knn Model ----
control <- trainControl(method = "cv", number = 10)

train_knn <- train(Class ~ .,
                   data = train_set,
                   method = "knn",
                   tuneGrid = data.frame(k = c(1, 3, 5, 7)),
                   trControl = control)

y_hat_knn <- predict(train_knn, test_set)
model_accuracy_results <- model_accuracy_results |> add_row(Method = "knn", Accuracy = confusionMatrix(y_hat_knn, factor(test_set$Class))$overall[["Accuracy"]])

ggplot(train_knn)


# Random Forest Model ----
control <- trainControl(method = "cv", number = 5)
grid <- data.frame(mtry = c(1, 5, 10, 25, 50, 100))

train_rf <- train(Class ~ .,
                  data = train_set,
                  method = "rf",
                  ntree = 150,
                  trControl = control,
                  tuneGrid = grid,
                  nsamp = 5000)

y_hat_rf <- predict(train_rf, test_set)
model_accuracy_results <- model_accuracy_results |> add_row(Method = "Random Forest", Accuracy = confusionMatrix(y_hat_rf, factor(test_set$Class))$overall[["Accuracy"]])

ggplot(train_rf)


# Naive Bayes Classifier ----
control <- trainControl(method = "cv", number = 10)

train_naive_bayes_classifier <- train(Class ~ .,
                                      data = train_set,
                                      method = "naive_bayes",
                                      trControl = control)

y_hat_naive_bayes_classifier <- predict(train_naive_bayes_classifier, test_set)
model_accuracy_results <- model_accuracy_results |> add_row(Method = "Naive Bayes", Accuracy = confusionMatrix(y_hat_naive_bayes_classifier, factor(test_set$Class))$overall[["Accuracy"]])


# evTree Model ----
control <- trainControl(method = "cv", number = 5)

train_evtree <- train(Class ~ .,
                      data = train_set,
                      method = "evtree",
                      ntrees = 150,
                      trControl = control)

y_hat_evtree <- predict(train_evtree, test_set)
confusionMatrix(y_hat_evtree, factor(test_set$Class))