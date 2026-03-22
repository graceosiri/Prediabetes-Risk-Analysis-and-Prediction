# 🩺 Prediabetes-Risk-Analysis-and-Prediction

![R](https://img.shields.io/badge/R-Statistical%20Modeling-blue)
![Machine Learning](https://img.shields.io/badge/Machine%20Learning-Random%20Forest-orange)
![Health Analytics](https://img.shields.io/badge/Domain-Health%20Analytics-green)
![Status](https://img.shields.io/badge/Status-Completed-success)

A data science project that analyzes dietary, demographic, and clinical factors to predict prediabetes risk using NHANES survey data.

---

## Overview
This project leverages nationally representative NHANES data to explore the relationship between diet, lifestyle, and prediabetes. It applies both statistical and machine learning techniques while accounting for complex survey design.

---

## Models & Methods
- Survey-Weighted Logistic Regression  
- Principal Component Analysis (PCA)  
- LASSO (Weighted Variable Selection)  
- Random Forest  
- XGBoost  

---

## Tech Stack
- R  
- tidyverse  
- survey  
- glmnet / svyVarSel  
- randomForest  
- xgboost  
- pROC  

---

## Approach
- Merged multiple NHANES datasets (diet, demographics, glucose, BMI) :contentReference[oaicite:0]{index=0}  
- Created a prediabetes classification variable using clinical thresholds  
- Applied survey-weighted modeling to ensure population-level validity  
- Performed feature selection using LASSO and PCA  
- Evaluated models using ROC curves, AUC, accuracy, precision, recall, and F1-score  

---

## Key Takeaways
- Dietary factors such as sodium, fat, and carbohydrate intake are significant predictors  
- Survey-weighted models provide more reliable population insights  
- Machine learning models (Random Forest, XGBoost) improved predictive performance  
- Combining dietary + demographic variables produced stronger predictions  

---

## How to Run
```bash
# Open in R or RStudio
source("Prediabetes Risk Analysis and Prediction.R")
