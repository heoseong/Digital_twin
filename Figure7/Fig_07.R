rm(list=ls())

library(dplyr)
library(ggplot2)
library(tidyverse)
library(h2o)
library(knitr)
library(tidyr)
library(curl)
library(lares)


fruit <- read.csv("for_real_machine_learning.csv", encoding = "UTF-8")
h2o.init()
#h2o.shutdown(prompt=FALSE)
#dev.off()

########sugar content
citrus <- h2o.importFile(path = "for_real_machine_learning.csv")

h2o.dim(citrus)
h2o.ls()
h2o.describe(citrus)


#Partition training and testing splits
citrus_split <- h2o.splitFrame(data = citrus, ratios = 0.8, seed=1)
citrus_train <- citrus_split[[1]]
citrus_test <- citrus_split[[2]]
dim(citrus_train)


#Specify feature and target variables
target <- "brix"
features <- setdiff(names(citrus_train), target)

automl_algorithms <- c(
  "GLM",
  "GBM",
  "DRF", 
  "XGBoost", 
  "DeepLearning",
  "StackedEnsemble"
)

citrus_automl <- h2o.automl(
  y = target,
  x = features,
  training_frame = citrus_train,
  nfolds = 5,
  include_algos = automl_algorithms,
  project_name = "citrus",
  max_runtime_secs = 120,
  seed = 20230322
)

exa <- h2o.explain(citrus_automl, citrus_test)
exa


#Evaluate the leaderboard
citrus_automl_leaderboard <- h2o.get_leaderboard(object=citrus_automl)
head(citrus_automl_leaderboard, 10)


#Select the most accurate model
citrus_automl_best_model <- citrus_automl@leader
model_names <- citrus_automl@leaderboard$model_id
model_names[1]
top_model <- h2o.getModel("StackedEnsemble_BestOfFamily_6_AutoML_1_20230711_150848")


#Report model parameters
citrus_automl_best_model@model$model_summary

#Evaluate accuracy
report_regression_metrics <- function(metrics){
  metrics <- list(
    RMSE = h2o.rmse(metrics), 
    MAE = h2o.mae(metrics), 
    R2 = h2o.r2(metrics)
  )
  return(metrics)
}


#Evaluate accuracy using cross validation
citrus_automl_cv_metrics <- h2o.performance(
  model = citrus_automl_best_model,
  xval = T
)

report_regression_metrics(
  metrics = citrus_automl_cv_metrics
)

#Evaluate accuracy using testing splits
citrus_automl_test_metrics <- h2o.performance(
  model = citrus_automl_best_model,
  newdata = citrus_test
)

report_regression_metrics(
  metrics = citrus_automl_test_metrics
)

#Visualize predictions using residual analysis plot
h2o.residual_analysis_plot(
  model = citrus_automl_best_model,
  newdata = citrus_test
)


#Obtain predicted values
citrus_automl_test_predictions <- h2o.predict(
  object = citrus_automl_best_model,
  newdata = citrus_test
)

head(citrus_automl_test_predictions, 10)

h2o_predictions <- h2o.predict(top_model, newdata = citrus_test) %>%
  as_tibble() %>% bind_cols(as_tibble(citrus_test))

h2o_cf <- h2o_predictions %>% 
  count(brix, pred= predict) %>% 
  mutate(label = "h2o", .before = 1)

kable(h2o_cf)

test_predict <- cbind(as.data.frame(citrus_test$brix),
                      as.data.frame(h2o_predictions$predict))
test_predict
str(test_predict)

real_frame <- as_tibble(test_predict)
real_test <- as_tibble(citrus_test)
farm <- real_test$farm_id
week <- real_test$week

ggplot(data=real_frame, aes(x=brix, y=h2o_predictions$predict)) +
  geom_abline(slope = 1, colour = "black", lty = "dashed", size=0.5) +
  geom_point(data=real_test, aes(color= farm, fill = farm), size=3, color = "gray", shape = 21) +
  theme_bw() +
  labs(x="True sugar content (°brix)", y="Predicted sugar content (°brix)") +
  theme(legend.position = c(0.1, 0.65),
        legend.background = element_rect(fill='transparent'),
        legend.key.size = unit(0.5, 'cm'))

#Shutdown the H2O cluster
h2o.shutdown(prompt = FALSE)



#############fruit size
citrus <- h2o.importFile(path = "for_real_machine_learning.csv")

h2o.dim(citrus)
h2o.ls()
h2o.describe(citrus)


#Partition training and testing splits
citrus_split <- h2o.splitFrame(data = citrus, ratios = 0.8, seed=1)
citrus_train <- citrus_split[[1]]
citrus_test <- citrus_split[[2]]
dim(citrus_train)


#Specify feature and target variables
target <- "size"
features <- setdiff(names(citrus_train), target)

automl_algorithms <- c(
  "GLM",
  "GBM",
  "DRF", 
  "XGBoost", 
  "DeepLearning",
  "StackedEnsemble"
)

citrus_automl <- h2o.automl(
  y = target,
  x = features,
  training_frame = citrus_train,
  nfolds = 5,
  include_algos = automl_algorithms,
  project_name = "citrus",
  max_runtime_secs = 120,
  seed = 20230322
)

exa <- h2o.explain(citrus_automl, citrus_test)
exa


#Evaluate the leaderboard
citrus_automl_leaderboard <- h2o.get_leaderboard(object=citrus_automl)
head(citrus_automl_leaderboard, 10)


#Select the most accurate model
citrus_automl_best_model <- citrus_automl@leader
model_names <- citrus_automl@leaderboard$model_id
model_names[1]
top_model <- h2o.getModel(model_names[1])


#Report model parameters
citrus_automl_best_model@model$model_summary

#Evaluate accuracy
report_regression_metrics <- function(metrics){
  metrics <- list(
    RMSE = h2o.rmse(metrics), 
    MAE = h2o.mae(metrics), 
    R2 = h2o.r2(metrics)
  )
  return(metrics)
}


#Evaluate accuracy using cross validation
citrus_automl_cv_metrics <- h2o.performance(
  model = citrus_automl_best_model,
  xval = T
)

report_regression_metrics(
  metrics = citrus_automl_cv_metrics
)

#Evaluate accuracy using testing splits
citrus_automl_test_metrics <- h2o.performance(
  model = citrus_automl_best_model,
  newdata = citrus_test
)

report_regression_metrics(
  metrics = citrus_automl_test_metrics
)

#Visualize predictions using residual analysis plot
h2o.residual_analysis_plot(
  model = citrus_automl_best_model,
  newdata = citrus_test
)


#Obtain predicted values
citrus_automl_test_predictions <- h2o.predict(
  object = citrus_automl_best_model,
  newdata = citrus_test
)

head(citrus_automl_test_predictions, 10)

h2o_predictions <- h2o.predict(top_model, newdata = citrus_test) %>%
  as_tibble() %>% bind_cols(as_tibble(citrus_test))

h2o_cf <- h2o_predictions %>% 
  count(brix, pred= predict) %>% 
  mutate(label = "h2o", .before = 1)

kable(h2o_cf)

test_predict <- cbind(as.data.frame(citrus_test$size),
                      as.data.frame(h2o_predictions$predict))
test_predict
str(test_predict)

real_frame <- as_tibble(test_predict)
real_test <- as_tibble(citrus_test)
farm <- real_test$farm_id
week <- real_test$week

ggplot(data=real_frame, aes(x=size, y=h2o_predictions$predict)) +
  geom_abline(slope = 1, colour = "black", lty = "dashed", size=0.5) +
  geom_point(data=real_test, aes(color= farm, fill = farm), size=3, color = "gray", shape = 21) +
  theme_bw() +
  labs(x="True fruit size (mm)", y="Predicted fruit size (mm)") +
  theme(legend.position = c(0.1, 0.65),
        legend.background = element_rect(fill='transparent'),
        legend.key.size = unit(0.5, 'cm'))

#Shutdown the H2O cluster
h2o.shutdown(prompt = FALSE)