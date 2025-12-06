{\rtf1\ansi\ansicpg1252\cocoartf2822
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fnil\fcharset0 HelveticaNeue;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\paperw11900\paperh16840\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\deftab560
\pard\pardeftab560\slleading20\pardirnatural\partightenfactor0

\f0\fs26 \cf0 # PDAC Sex-Specific Prediction Models\
\
This repository contains code for training and evaluating sex-specific models to predict PDAC (pancreatic ductal adenocarcinoma) based on clinical and biomarker data. The analyses include 5-fold cross-validation for model comparison, full-data logistic regression with coefficients, and ROC curve visualizations.\
\
---\
\
## Project Structure\
\
The repository is organized into three main R scripts:\
\
1. **`cv_model_comparison.R`**  \
   Performs 5-fold cross-validation to compare multiple machine learning methods:\
   - Logistic Regression (`glm`)\
   - Decision Tree (`rpart`)\
   - Support Vector Machine (`svm`)\
   - Random Forest (`randomForest`)\
   - Lasso Regression (`glmnet` with L1 penalty)  \
\
   This script outputs:\
   - Fold-wise predictions for male and female models\
   - Summary table of AUC, sensitivity, and specificity for all methods\
   - Optional CSV for results export\
\
2. **`logistic_regression_full_data.R`**  \
   Trains the best-performing logistic regression model on the full dataset and records:\
   - Model coefficients\
\
3. **`roc_plot.R`**  \
   Generates ROC curves for male- and female-trained models:\
   - Female model \uc0\u8594  Females\
   - Female model \uc0\u8594  Males\
   - Male model \uc0\u8594  Males\
   - Male model \uc0\u8594  Females  \
\
   Output:\
   - `roc_plot.png` visualizing all four curves on the same figure\
\
---\
\
## Data\
\
\
| Column    | Description                       |\
|-----------|-----------------------------------|\
| Age       | Patient age                        |\
| Sex       | Patient sex (`M` or `F`)           |\
| Diagnosis | 1 = Benign, 3 = PDAC              |\
| Creatinine | Creatinine biomarker               |\
| LYVE1     | Biomarker LYVE1                    |\
| REG1      | Biomarker REG1                     |\
| TFF1      | Biomarker TFF1                     |\
\
---\
\
## Usage\
\
1. Clone the repository:\
\
```bash\
git clone https://github.com/your-username/pdac-prediction.git\
cd pdac-prediction\
\
\pard\pardeftab560\slleading20\partightenfactor0
\cf0 2. Install required packages in R if not already installed: \
\
install.packages(c("readxl", "pROC", "caret", "rpart", "e1071", "randomForest", "glmnet", "tibble"))\
\
3. Run scripts in sequence (or separately):\
source("cv_model_comparison.R")\
source("logistic_regression_full_data.R")\
source("roc_plot.R")\
\
}