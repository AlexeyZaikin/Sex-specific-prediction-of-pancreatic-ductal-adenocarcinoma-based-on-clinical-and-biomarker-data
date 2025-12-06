rm(list = ls())
library(readxl)
library(pROC)

############################################################
# Load & Prepare Data
############################################################

data1 <- read_excel("data/PDAC_dataset.xlsx")
colnames(data1)[3:7] <- c("Diagnosis", "Creat", "LYVE1", "REG1", "TFF1")

# Keep only Diagnosis 1 (benign) and 3 (PDAC)
data1 <- subset(data1, Diagnosis %in% c(1, 3))
data1$Age  <- as.numeric(data1$Age)
data1$Diag <- ifelse(data1$Diagnosis == 3, 1, 0)
data1$Diagnosis <- NULL

# Split by sex
m_data <- subset(data1, Sex == "M")
f_data <- subset(data1, Sex == "F")

# Columns for prediction storage
m_data$m <- NA  # male model → males
m_data$f <- NA  # female model → males
f_data$m <- NA  # male model → females
f_data$f <- NA  # female model → females

############################################################
# 5-Fold CV Setup
############################################################

set.seed(185)
folds <- 5

fold_id_m <- sample(rep(1:folds, length.out = nrow(m_data)))
fold_id_f <- sample(rep(1:folds, length.out = nrow(f_data)))

# original column indices
cols_to_scale <- c(1,3,4,5,6)

############################################################
# 5-fold Cross-Validation
############################################################

for (k in 1:folds) {
  
  # Fold splits
  train_f <- f_data[fold_id_f != k, ]
  test_f  <- f_data[fold_id_f == k, ]
  
  train_m <- m_data[fold_id_m != k, ]
  test_m  <- m_data[fold_id_m == k, ]
  
  ##########################################################
  # FEMALE MODEL
  ##########################################################
  
  f_mean <- attr(scale(train_f[, cols_to_scale]), "scaled:center")
  f_sd   <- attr(scale(train_f[, cols_to_scale]), "scaled:scale")
  
  train_f_scaled <- train_f
  test_f_scaled  <- test_f
  test_m_scaled  <- test_m
  
  train_f_scaled[, cols_to_scale] <- scale(train_f[, cols_to_scale])
  test_f_scaled[, cols_to_scale]  <- scale(test_f[, cols_to_scale], center=f_mean, scale=f_sd)
  test_m_scaled[, cols_to_scale]  <- scale(test_m[, cols_to_scale], center=f_mean, scale=f_sd)
  
  model_f <- glm(Diag ~ Age + LYVE1 + REG1 + TFF1 + Creat,
                 family="binomial", data=train_f_scaled)
  
  pr_f_f <- predict(model_f, test_f_scaled, type="response")  # f→f
  pr_f_m <- predict(model_f, test_m_scaled, type="response")  # f→m
  
  # Save fold predictions
  f_data$f[fold_id_f == k] <- pr_f_f
  m_data$f[fold_id_m == k] <- pr_f_m
  
  ##########################################################
  # MALE MODEL 
  ##########################################################
  
  m_mean <- attr(scale(train_m[, cols_to_scale]), "scaled:center")
  m_sd   <- attr(scale(train_m[, cols_to_scale]), "scaled:scale")
  
  train_m_scaled <- train_m
  test_f_scaled  <- test_f 
  test_m_scaled  <- test_m
  
  train_m_scaled[, cols_to_scale] <- scale(train_m[, cols_to_scale])
  test_f_scaled[, cols_to_scale]  <- scale(test_f[, cols_to_scale], center=m_mean, scale=m_sd)
  test_m_scaled[, cols_to_scale]  <- scale(test_m[, cols_to_scale], center=m_mean, scale=m_sd)
  
  model_m <- glm(Diag ~ Age + LYVE1 + REG1 + TFF1 + Creat,
                 family="binomial", data=train_m_scaled)
  
  pr_m_m <- predict(model_m, test_m_scaled, type="response")  # m→m
  pr_m_f <- predict(model_m, test_f_scaled, type="response")  # m→f
  
  # Save fold predictions
  m_data$m[fold_id_m == k] <- pr_m_m
  f_data$m[fold_id_f == k] <- pr_m_f
  }

############################################################
# Combined ROC plot (all 4 curves)
############################################################

# Compute ROC objects
roc_f_f <- roc(f_data$Diag, f_data$f, quiet=TRUE)   # female model → females
roc_f_m <- roc(m_data$Diag, m_data$f, quiet=TRUE)   # female model → males
roc_m_m <- roc(m_data$Diag, m_data$m, quiet=TRUE)   # male model → males
roc_m_f <- roc(f_data$Diag, f_data$m, quiet=TRUE)   # male model → females

# Plot settings
png("/output/roc_plot.png", width=2000, height=2000, res=300)
plot(roc_f_f, col="#1b9e77", lwd=3, legacy.axes=TRUE,
     main="Sex-Specific Models and Cross-Sex Transfer",
     cex.main=1.2)

lines(roc_f_m, col="#d95f02", lwd=3)
lines(roc_m_m, col="#7570b3", lwd=3)
lines(roc_m_f, col="#e7298a", lwd=3)

abline(0,1,lty=2,col="grey")

# Legend with AUCs
legend("bottomright",
       legend=c(
         paste0("Female model → Females"),
         paste0("Female model → Males"),
         paste0("Male model → Males"),
         paste0("Male model → Females")
       ),
       col=c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"),
       lwd=3, cex=0.8, bty="n"
)
# Close device
dev.off()