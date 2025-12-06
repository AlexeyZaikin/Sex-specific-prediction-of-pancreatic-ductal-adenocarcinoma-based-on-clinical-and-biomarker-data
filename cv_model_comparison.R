#### Methods: glm (logistic), rpart, svm, randomForest, glmnet (L1)
#### Author: Alexey Zaikin
rm(list = ls())

# ---- Packages ----
library(readxl)
library(caret)
library(pROC)
library(rpart)
library(e1071)
library(randomForest)
library(glmnet)
library(tibble)

# ---- User settings ----
cols_to_scale <- c(1,3,4,5,6)
folds <- 5
seed_for_folds <- 185

# ---- Load data ----
data1 <- read_excel("data/PDAC_dataset.xlsx")
colnames(data1)[3:7] <- c("Diagnosis", "Creat", "LYVE1", "REG1", "TFF1")
data1 <- subset(data1, Diagnosis %in% c(1,3))
data1$Age <- as.numeric(data1$Age)
data1$Diag <- ifelse(data1$Diagnosis == 3, 1, 0)
data1$Diagnosis <- NULL

m_data <- subset(data1, Sex == "M")
f_data <- subset(data1, Sex == "F")

# ---- Helper functions ----
get_metrics_from_probs <- function(obs, probs) {
  roc_obj <- roc(obs, as.numeric(probs), quiet = TRUE)
  auc_val <- auc(roc_obj)
  xy <- coords(roc_obj, "best", best.method = "youden", ret = c("sensitivity", "specificity"))
  if (is.matrix(xy) && nrow(xy) > 1) xy <- xy[1, ]
  list(AUC = as.numeric(auc_val), Sens = as.numeric(xy[1]), Spec = as.numeric(xy[2]))
}

get_metrics_from_classes <- function(obs, pred_class) {
  roc_obj <- roc(obs, as.numeric(pred_class), quiet = TRUE)
  auc_val <- auc(roc_obj)
  cm <- confusionMatrix(factor(pred_class, levels = c(0,1)), factor(obs, levels = c(0,1)))
  list(AUC = as.numeric(auc_val), Sens = as.numeric(cm$byClass[1]), Spec = as.numeric(cm$byClass[2]))
}

scale_train_apply <- function(train_df, test1_df, test2_df, cols_idx) {
  train_scaled_vals <- scale(train_df[, cols_idx])
  center <- attr(train_scaled_vals, "scaled:center")
  scalev <- attr(train_scaled_vals, "scaled:scale")
  train_df2 <- train_df
  test1_df2 <- test1_df
  test2_df2 <- test2_df
  train_df2[, cols_idx] <- train_scaled_vals
  test1_df2[, cols_idx] <- scale(test1_df[, cols_idx], center = center, scale = scalev)
  test2_df2[, cols_idx] <- scale(test2_df[, cols_idx], center = center, scale = scalev)
  list(train = train_df2, test1 = test1_df2, test2 = test2_df2, center = center, scale = scalev)
}

# ---- Core function ----
run_method <- function(method_name, m_data, f_data, cols_to_scale, folds = 5, seed = 185) {
  acc <- list(
    f_f = list(AUC = numeric(0), Sens = numeric(0), Spec = numeric(0)),
    f_m = list(AUC = numeric(0), Sens = numeric(0), Spec = numeric(0)),
    m_m = list(AUC = numeric(0), Sens = numeric(0), Spec = numeric(0)),
    m_f = list(AUC = numeric(0), Sens = numeric(0), Spec = numeric(0))
  )
  
  set.seed(seed)
  fold_id_m <- sample(rep(1:folds, length.out = nrow(m_data)))
  fold_id_f <- sample(rep(1:folds, length.out = nrow(f_data)))
  
  for (k in 1:folds) {
    train_f <- f_data[which(fold_id_f != k), ]
    test_f  <- f_data[which(fold_id_f == k), ]
    train_m <- m_data[which(fold_id_m != k), ]
    test_m  <- m_data[which(fold_id_m == k), ]
    
    # ---- Female-trained model ----
    scaled_f <- scale_train_apply(train_f, test_f, test_m, cols_to_scale)
    train_f_scaled <- scaled_f$train
    test_f_scaled  <- scaled_f$test1
    test_m_scaled  <- scaled_f$test2
    
    if (method_name == "glm") {
      model_f <- glm(Diag ~ Age + LYVE1 + REG1 + TFF1 + Creat, family = "binomial", data = train_f_scaled)
      pr_f_f <- predict(model_f, test_f_scaled, type = "response")
      pr_f_m <- predict(model_f, test_m_scaled, type = "response")
      m1 <- get_metrics_from_probs(test_f_scaled$Diag, pr_f_f)
      m2 <- get_metrics_from_probs(test_m_scaled$Diag, pr_f_m)
    } else if (method_name == "rpart") {
      model_f <- rpart(Diag ~ Age + LYVE1 + REG1 + TFF1 + Creat, data = train_f_scaled, method = "class", minsplit = 1, minbucket = 1)
      pr_f_f <- predict(model_f, test_f_scaled, type = "class")
      pr_f_m <- predict(model_f, test_m_scaled, type = "class")
      m1 <- get_metrics_from_classes(test_f_scaled$Diag, pr_f_f)
      m2 <- get_metrics_from_classes(test_m_scaled$Diag, pr_f_m)
    } else if (method_name == "svm") {
      model_f <- svm(Diag ~ Age + LYVE1 + REG1 + TFF1 + Creat, data = train_f_scaled, type = "C-classification", kernel = "linear")
      pr_f_f <- predict(model_f, test_f_scaled, type = "response")
      pr_f_m <- predict(model_f, test_m_scaled, type = "response")
      pr_f_f <- factor(pr_f_f, levels = c(0,1))
      pr_f_m <- factor(pr_f_m, levels = c(0,1))
      m1 <- get_metrics_from_classes(test_f_scaled$Diag, pr_f_f)
      m2 <- get_metrics_from_classes(test_m_scaled$Diag, pr_f_m)
    } else if (method_name == "rf") {
      model_f <- randomForest(factor(Diag) ~ Age + LYVE1 + REG1 + TFF1 + Creat, data = train_f_scaled, ntree = 500)
      pr_f_f <- predict(model_f, test_f_scaled, type = "response")
      pr_f_m <- predict(model_f, test_m_scaled, type = "response")
      pr_f_f <- factor(pr_f_f, levels = c(0,1))
      pr_f_m <- factor(pr_f_m, levels = c(0,1))
      m1 <- get_metrics_from_classes(test_f_scaled$Diag, pr_f_f)
      m2 <- get_metrics_from_classes(test_m_scaled$Diag, pr_f_m)
    } else if (method_name == "lasso") {
      x_train <- model.matrix(Diag ~ Age + LYVE1 + REG1 + TFF1 + Creat, train_f_scaled)[, -1, drop = FALSE]
      y_train <- train_f_scaled$Diag
      cvfit <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)
      x_test_m <- model.matrix(Diag ~ Age + LYVE1 + REG1 + TFF1 + Creat, test_m_scaled)[, -1, drop = FALSE]
      x_test_f <- model.matrix(Diag ~ Age + LYVE1 + REG1 + TFF1 + Creat, test_f_scaled)[, -1, drop = FALSE]
      pr_f_m <- predict(cvfit, newx = x_test_m, s = "lambda.min", type = "response")
      pr_f_f <- predict(cvfit, newx = x_test_f, s = "lambda.min", type = "response")
      m1 <- get_metrics_from_probs(test_f_scaled$Diag, pr_f_f)
      m2 <- get_metrics_from_probs(test_m_scaled$Diag, pr_f_m)
    }
    
    acc$f_f$AUC <- c(acc$f_f$AUC, m1$AUC)
    acc$f_f$Sens <- c(acc$f_f$Sens, m1$Sens)
    acc$f_f$Spec <- c(acc$f_f$Spec, m1$Spec)
    acc$f_m$AUC <- c(acc$f_m$AUC, m2$AUC)
    acc$f_m$Sens <- c(acc$f_m$Sens, m2$Sens)
    acc$f_m$Spec <- c(acc$f_m$Spec, m2$Spec)
    
    # ---- Male-trained model ----
    scaled_m <- scale_train_apply(train_m, test_f, test_m, cols_to_scale)
    train_m_scaled <- scaled_m$train
    test_f_scaled_m <- scaled_m$test1
    test_m_scaled_m <- scaled_m$test2
    
    if (method_name == "glm") {
      model_m <- glm(Diag ~ Age + LYVE1 + REG1 + TFF1 + Creat, family = "binomial", data = train_m_scaled)
      pr_m_m <- predict(model_m, test_m_scaled_m, type = "response")
      pr_m_f <- predict(model_m, test_f_scaled_m, type = "response")
      m3 <- get_metrics_from_probs(test_m_scaled_m$Diag, pr_m_m)
      m4 <- get_metrics_from_probs(test_f_scaled_m$Diag, pr_m_f)
    } else if (method_name == "rpart") {
      model_m <- rpart(Diag ~ Age + LYVE1 + REG1 + TFF1 + Creat, data = train_m_scaled, method = "class", minsplit = 1, minbucket = 1)
      pr_m_m <- predict(model_m, test_m_scaled_m, type = "class")
      pr_m_f <- predict(model_m, test_f_scaled_m, type = "class")
      m3 <- get_metrics_from_classes(test_m_scaled_m$Diag, pr_m_m)
      m4 <- get_metrics_from_classes(test_f_scaled_m$Diag, pr_m_f)
    } else if (method_name == "svm") {
      model_m <- svm(Diag ~ Age + LYVE1 + REG1 + TFF1 + Creat, data = train_m_scaled, type = "C-classification", kernel = "linear")
      pr_m_m <- predict(model_m, test_m_scaled_m, type = "response")
      pr_m_f <- predict(model_m, test_f_scaled_m, type = "response")
      pr_m_m <- factor(pr_m_m, levels = c(0,1))
      pr_m_f <- factor(pr_m_f, levels = c(0,1))
      m3 <- get_metrics_from_classes(test_m_scaled_m$Diag, pr_m_m)
      m4 <- get_metrics_from_classes(test_f_scaled_m$Diag, pr_m_f)
    } else if (method_name == "rf") {
      model_m <- randomForest(factor(Diag) ~ Age + LYVE1 + REG1 + TFF1 + Creat, data = train_m_scaled, ntree = 500)
      pr_m_m <- predict(model_m, test_m_scaled_m, type = "response")
      pr_m_f <- predict(model_m, test_f_scaled_m, type = "response")
      pr_m_m <- factor(pr_m_m, levels = c(0,1))
      pr_m_f <- factor(pr_m_f, levels = c(0,1))
      m3 <- get_metrics_from_classes(test_m_scaled_m$Diag, pr_m_m)
      m4 <- get_metrics_from_classes(test_f_scaled_m$Diag, pr_m_f)
    } else if (method_name == "lasso") {
      x_train <- model.matrix(Diag ~ Age + LYVE1 + REG1 + TFF1 + Creat, train_m_scaled)[, -1, drop = FALSE]
      y_train <- train_m_scaled$Diag
      cvfit <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)
      x_test_m <- model.matrix(Diag ~ Age + LYVE1 + REG1 + TFF1 + Creat, test_m_scaled_m)[, -1, drop = FALSE]
      x_test_f <- model.matrix(Diag ~ Age + LYVE1 + REG1 + TFF1 + Creat, test_f_scaled_m)[, -1, drop = FALSE]
      pr_m_m <- predict(cvfit, newx = x_test_m, s = "lambda.min", type = "response")
      pr_m_f <- predict(cvfit, newx = x_test_f, s = "lambda.min", type = "response")
      m3 <- get_metrics_from_probs(test_m_scaled_m$Diag, pr_m_m)
      m4 <- get_metrics_from_probs(test_f_scaled_m$Diag, pr_m_f)
    }
    
    acc$m_m$AUC <- c(acc$m_m$AUC, m3$AUC)
    acc$m_m$Sens <- c(acc$m_m$Sens, m3$Sens)
    acc$m_m$Spec <- c(acc$m_m$Spec, m3$Spec)
    acc$m_f$AUC <- c(acc$m_f$AUC, m4$AUC)
    acc$m_f$Sens <- c(acc$m_f$Sens, m4$Sens)
    acc$m_f$Spec <- c(acc$m_f$Spec, m4$Spec)
  } # end folds
  
  tibble(
    method = method_name,
    f_f_AUC = mean(acc$f_f$AUC), f_f_Sens = mean(acc$f_f$Sens), f_f_Spec = mean(acc$f_f$Spec),
    f_m_AUC = mean(acc$f_m$AUC), f_m_Sens = mean(acc$f_m$Sens), f_m_Spec = mean(acc$f_m$Spec),
    m_m_AUC = mean(acc$m_m$AUC), m_m_Sens = mean(acc$m_m$Sens), m_m_Spec = mean(acc$m_m$Spec),
    m_f_AUC = mean(acc$m_f$AUC), m_f_Sens = mean(acc$m_f$Sens), m_f_Spec = mean(acc$m_f$Spec)
  )
}

# ---- Run all methods ----
methods <- c("glm", "rpart", "svm", "rf", "lasso")
all_results <- lapply(methods, function(m) {
  message("Running method: ", m)
  run_method(m, m_data, f_data, cols_to_scale, folds, seed_for_folds)
})
results_df <- do.call(rbind, all_results)

# ---- Save results ----
print(results_df)
write.csv(results_df, "output/pdac_ml_results_scaled_all_methods_summary.csv", row.names = FALSE)
