# 🩺 Prediabetes-Risk-Analysis-and-Prediction

![R](https://img.shields.io/badge/R-Statistical%20Modeling-blue)
![Machine Learning](https://img.shields.io/badge/Machine%20Learning-Random%20Forest-orange)
![Health Analytics](https://img.shields.io/badge/Domain-Health%20Analytics-green)
![Status](https://img.shields.io/badge/Status-Completed-success)

A data science project that analyzes dietary, demographic, and clinical factors to predict prediabetes risk using NHANES survey data.

## Overview
This project leverages 2017 - 2020 National Health and Nutrition Examination Survey data to explore the relationship between diet, lifestyle, and prediabetes. It applies both statistical and machine learning techniques while accounting for complex survey design.

## Models & Methods
- Survey-Weighted Logistic Regression  
- Principal Component Analysis (PCA)  
- LASSO (Weighted Variable Selection)  
- Random Forest  
- XGBoost  

## Tech Stack
- R  
- tidyverse  
- survey  
- glmnet / svyVarSel  
- randomForest  
- xgboost  
- pROC  

## Approach
- Merged multiple NHANES datasets (diet, demographics, glucose, BMI) :contentReference[oaicite:0]{index=0}  
- Created a prediabetes classification variable using clinical thresholds  
- Applied complex survey design to ensure nationally representative results
- Performed feature selection using LASSO and PCA  
- Evaluated models using ROC curves, AUC, accuracy, precision, recall, and F1-score  

## Key Takeaways
- Age, BMI, and waist circumference emerged as the strongest overall predictors 
- Diets higher in sugar, carbohydrates, alcohol, and caffeine lead to elevated prediabetes risk.
- Diets higher in protein, vitamin C, fiber, iron, and water intake lead to a reduced prediabetes risk
- Combining dietary + demographic + body measure variables produced stronger predictions  
- Survey-weighted models provide more reliable population insights  
- Machine learning models (Random Forest, XGBoost) improved predictive performance  

## How to Run
```bash
# Open in R or RStudio
source("Prediabetes Risk Analysis and Prediction.R")
