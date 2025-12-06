rm(list=ls())
library(readxl)
library(broom)
library(car)
library(dplyr)

# Load & prepare data
data1 <- read_excel("data/PDAC_dataset.xlsx")
colnames(data1)[3:7] <- c("Diagnosis", "Creat", "LYVE1", "REG1", "TFF1")

data1 <- subset(data1, Diagnosis %in% c(1,3))
data1$Age <- as.numeric(data1$Age)
data1$Diag <- ifelse(data1$Diagnosis == 3, 1, 0)
data1$Diagnosis <- NULL

# Split by sex
m_data <- subset(data1, Sex == "M")
f_data <- subset(data1, Sex == "F")

# Fit model for males (example)
m_m <- glm(Diag ~ Age + LYVE1 + REG1 + TFF1 + Creat, 
           family="binomial", data=m_data)

# Extract coefficients
coef_table <- broom::tidy(m_m) %>%
  mutate(
    OR = exp(estimate),
    CI_low = exp(estimate - 1.96 * std.error),
    CI_high = exp(estimate + 1.96 * std.error)
  )

# Compute VIFs
vif_df <- car::vif(m_m) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("term") %>%
  rename(VIF = 2) %>%
  filter(term != "(Intercept)")

# Merge
final_table <- coef_table %>%
  left_join(vif_df, by="term") %>%
  select(
    Predictor = term,
    Coefficient = estimate,
    SE = std.error,
    OR,
    CI_low,
    CI_high,
    VIF
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

write.csv(final_table, "output/logistic_coefficients.csv", row.names = FALSE)
