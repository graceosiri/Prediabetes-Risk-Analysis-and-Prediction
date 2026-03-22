
##load necessary libraries
library(foreign)
library(tidyverse)
library(haven)     
library(survey)
library(pROC)
library(glmnet)
library(svyVarSel)
library(randomForest)
library(xgboost)
library(Matrix)
library(caret)
options(survey.lonely.psu = "adjust")

set.seed(123)


##########################################

##load necessary data sets

##########################################


#dietary intake
diet <- read.xport("C:\\Users\\osiri\\Downloads\\P_DR1TOT.xpt", na.rm = TRUE)

#demographic
demo <- read.xport("C:\\Users\\osiri\\Downloads\\P_DEMO.xpt", na.rm = TRUE)

#Hba1c (Glycohemoglobin)
Hba1c <- read.xport("C:\\Users\\osiri\\Downloads\\P_GHB.xpt", na.rm = TRUE)

#Fasting Glucose
glucose <- read.xport("C:\\Users\\osiri\\Downloads\\P_GLU.xpt", na.rm = TRUE)

#Body Measures - Weight
weight <- read.xport("C:\\Users\\osiri\\Downloads\\P_BMX.xpt", na.rm = TRUE)




#######################################

#select needed variables 

######################################


##Diet

#Sequence number, day one sample weight, Dietary recall status, energy (kcal), protein (gm), 
#carbohydrate (gm), total sugars (gm), dietary fiber (gm),
#total fat (gm), total saturated fatty acids (gm), cholesterol(gm), Vitamic C(mg),
#Calcium(mg), Iron(mg), sodium(mg), potassium(mg), caffeine(mg), alcohol(gm),
#Total plain water drank yesterday (gm)

diet_keep <- diet %>% select(SEQN, WTDRD1PP, DR1DRSTZ, DR1TKCAL, DR1TPROT,
                             DR1TCARB, DR1TSUGR, DR1TFIBE, DR1TTFAT,
                             DR1TSFAT, DR1TCHOL, DR1TVC, DR1TCALC, DR1TIRON,
                             DR1TSODI, DR1TPOTA, DR1TCAFF, DR1TALCO,
                             DR1_320Z)

##Demographic

#Sequence number, gender, age, Race/Hispanic origin w/ NH Asian, 
#Full sample interview weight, Full sample MEC exam weight,
#Masked variance pseudo-PSU, Masked variance pseudo-stratum

demo_keep <- demo %>% select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH3,
                             WTINTPRP, WTMECPRP, SDMVPSU, SDMVSTRA)

##Hba1c (Glycohemoglobin)

#Sequence number, Glycohemoglobin (%)

Hba1c_keep <- Hba1c %>% select(SEQN, LBXGH)

##Fasting Glucose

#Sequence number, Fasting subsample weight, fasting glucose(mg/dl)

glucose_keep <- glucose %>% select(SEQN, WTSAFPRP, LBXGLU)

##Body Measures - Weight

#sequence number, Body Mass Index (kg/m**2), Waist Circumference (cm)

weight_keep <- weight %>% select(SEQN, BMXBMI, BMXWAIST) 



##############################################

#Join all datasets and make final master dataset

##############################################

master <- diet_keep %>%
  left_join(demo_keep, by="SEQN") %>%
  left_join(Hba1c_keep, by="SEQN") %>%
  left_join(glucose_keep, by="SEQN") %>%
  left_join(weight_keep, by="SEQN") %>% 
  mutate(
    prediabetic = case_when(
      # Prediabetes: either HbA1c 5.7–6.4 OR FPG 100–125
      (!is.na(LBXGH) & LBXGH >= 5.7 & LBXGH < 6.5) |
        (!is.na(LBXGLU) & LBXGLU >= 100 & LBXGLU < 126) ~ "Yes",
      
      # Normal: below thresholds
      (!is.na(LBXGH) & LBXGH < 5.7) |
        (!is.na(LBXGLU) & LBXGLU < 100) ~ "No",
      
      # Diabetes: above thresholds → mark as NA (remove from analysis)
      (!is.na(LBXGH) & LBXGH >= 6.5) |
        (!is.na(LBXGLU) & LBXGLU >= 126) ~ NA_character_,
      
      TRUE ~ NA_character_
    ),
    prediabetic = factor(prediabetic, levels = c("No","Yes"))
  ) %>% 
  rename(
    seqn = SEQN,
    sample_weight = WTDRD1PP,
    recall_status = DR1DRSTZ,
    energy_kcal = DR1TKCAL,
    protein_gm = DR1TPROT,
    carbohydrate_gm = DR1TCARB,
    sugar_gm = DR1TSUGR,
    fiber_gm = DR1TFIBE,
    fat_gm = DR1TTFAT,
    saturated_fat_gm = DR1TSFAT,
    cholesterol_gm = DR1TCHOL,
    vitamin_c_mg = DR1TVC,
    calcium_mg = DR1TCALC,
    iron_mg = DR1TIRON,
    sodium_mg = DR1TSODI,
    potassium_mg = DR1TPOTA,
    caffeine_mg = DR1TCAFF,
    alcohol_gm = DR1TALCO,
    water_gm = DR1_320Z,
    gender = RIAGENDR,
    age = RIDAGEYR,
    race = RIDRETH3,
    full_sample_interview_weight = WTINTPRP,
    full_sample_mec_exam_weight = WTMECPRP,
    masked_variance_pseudo_psu = SDMVPSU,
    masked_variance_pseudo_stratum = SDMVSTRA,
    hba1c = LBXGH,
    fasting_subsample_weight = WTSAFPRP,
    fasting_glucose = LBXGLU,
    bmi_kgm = BMXBMI,
    waist_circumfrence = BMXWAIST
  ) %>% 
  mutate(
    gender = factor(gender, levels = c(1,2), 
                    labels = c("Male", "Female")),
    race = factor(race, levels = c(1,2,3,4,6,7), 
                  labels = c("Mexican American", "Hispanic",
                             "White", "Black", "Asian", "Other"))
  )

view(master)
summary(master)
str(master)



###########################################

###Models

############################################



###General logistic regression model without survey design or weights


#all diet variables
model = glm(prediabetic ~ energy_kcal + protein_gm + carbohydrate_gm + sugar_gm +
              fiber_gm + fat_gm + saturated_fat_gm +
              cholesterol_gm + vitamin_c_mg + calcium_mg + iron_mg + potassium_mg +
              sodium_mg + caffeine_mg + alcohol_gm + water_gm, data = master, family = binomial, na.action = na.omit)
summary(model)
exp(coef(model))


#diet variables without multicolinearity
model = glm(prediabetic ~ protein_gm + carbohydrate_gm + 
              fiber_gm + fat_gm +
              cholesterol_gm + vitamin_c_mg + calcium_mg + iron_mg + 
              sodium_mg + caffeine_mg + alcohol_gm + 
              water_gm, data = master, family = binomial, na.action = na.omit)
summary(model)
exp(coef(model))


#diet variables using lasso
model = glm(prediabetic ~ fat_gm + iron_mg + potassium_mg + sodium_mg + caffeine_mg +
              alcohol_gm + water_gm, data = master, family = binomial, na.action = na.omit)
summary(model)
exp(coef(model))


#all variables
model = glm(prediabetic ~ energy_kcal + protein_gm + carbohydrate_gm + sugar_gm +
              fiber_gm + fat_gm + saturated_fat_gm +
              cholesterol_gm + vitamin_c_mg + calcium_mg + iron_mg + potassium_mg +
              sodium_mg + caffeine_mg + alcohol_gm + water_gm + age + race + bmi_kgm + waist_circumfrence,
            data = master, family = binomial, na.action = na.omit)
summary(model)
exp(coef(model))





################################################
# Survey design + analysis subset
################################################ 

desn_all <- svydesign(
  id = ~masked_variance_pseudo_psu,
  strata = ~masked_variance_pseudo_stratum,
  weights = ~sample_weight,
  nest = TRUE,
  data = master
)

## Restrict to adults 18+, valid diet recall, non-missing variables
desn_analysis <- subset(
  desn_all,
  age >= 18 & recall_status == 1 &
    !is.na(prediabetic)
)
desn_analysis <- subset(desn_analysis, !is.na(bmi_kgm) &!is.na(waist_circumfrence))
nrow(desn_analysis)


## weighted prevalence check
svymean(~I(prediabetic == "Yes"), design = desn_analysis, 
        na.rm = TRUE)
prop.table(svytable(~prediabetic, design = desn_analysis))




##################################################
# 5) Prepare stratified train/test split (by masked stratum)
##################################################

set.seed(20)
vars <- desn_analysis$variables
vars$stratum <- desn_analysis$strata
vars$row_id <- seq_len(nrow(vars))

train_ids <- vars %>%
  group_by(stratum) %>%
  sample_frac(0.8) %>%
  pull(row_id)

desn_train <- subset(desn_analysis, seq_len(nrow(vars)) %in% train_ids)
desn_test  <- subset(desn_analysis, !(seq_len(nrow(vars)) %in% train_ids))

# data.frames for ML models
train_df <- as.data.frame(desn_train$variables)
test_df  <- as.data.frame(desn_test$variables)

nrow(desn_train$variables)
ncol(desn_analysis)
nrow(desn_test$variables)
head(desn_train$variables)
view(desn_analysis$variables)




############################################################

#Survey Weighted Logistic regression models

############################################################


# Diet (reduced multicollinearity set)
model1 <- svyglm(
  prediabetic ~ protein_gm + carbohydrate_gm + 
    fiber_gm + fat_gm +
    cholesterol_gm + vitamin_c_mg + calcium_mg + iron_mg + 
    sodium_mg + caffeine_mg + alcohol_gm + 
    water_gm,
  design = desn_train,
  family = quasibinomial()
)

summary(model1)
car::vif(model1)
exp(coef(model1))

logpred <- predict(model1, newdata=test_df,
                   type = "response")
y_true <- ifelse(test_df$prediabetic == "Yes", 1, 0)
y_pred_class <- ifelse(logpred >= 0.416, 1, 0)
w <- test_df$sample_weight
length(logpred)
nrow(test_df)
length(y_true)

# --- Evaluation ---
roc_obj <- roc(y_true, logpred, weights = w)
auc_val <- auc(roc_obj)
plot(roc_obj, col = "blue", main = paste("ROC Curve (AUC =", round(auc_val, 3), ")"))

# Confusion matrix & metrics
cm <- table(Predicted = y_pred_class, Actual = y_true)
cm
accuracy <- mean(y_pred_class == y_true)
precision <- sum(y_pred_class == 1 & y_true == 1) / sum(y_pred_class == 1)
recall <- sum(y_pred_class == 1 & y_true == 1) / sum(y_true == 1)
f1 <- 2 * (precision * recall) / (precision + recall)
metrics <- c(Accuracy = accuracy, Precision = precision, Recall = recall, F1 = f1, AUC = auc_val)
metrics




#All diet variables
model2 <- svyglm(
  prediabetic ~ energy_kcal + protein_gm + carbohydrate_gm + sugar_gm +
    fiber_gm + fat_gm + saturated_fat_gm +
    cholesterol_gm + vitamin_c_mg + calcium_mg + iron_mg + potassium_mg +
    sodium_mg + caffeine_mg + alcohol_gm + water_gm,
  design = desn_train,
  family = quasibinomial()
)

summary(model2)
car::vif(model2)
exp(coef(model2))

logpred <- predict(model2, newdata=test_df,
                   type = "response")
y_true <- ifelse(test_df$prediabetic == "Yes", 1, 0)
y_pred_class <- ifelse(logpred >= 0.41, 1, 0)
w <- test_df$sample_weight
length(logpred)
nrow(test_df)
length(y_true)

# --- Evaluation ---
roc_obj <- roc(y_true, logpred, weights = w)
auc_val <- auc(roc_obj)
plot(roc_obj, col = "blue", main = paste("ROC Curve (AUC =", round(auc_val, 3), ")"))

# Confusion matrix & metrics
cm <- table(Predicted = y_pred_class, Actual = y_true)
cm
accuracy <- mean(y_pred_class == y_true)
precision <- sum(y_pred_class == 1 & y_true == 1) / sum(y_pred_class == 1)
recall <- sum(y_pred_class == 1 & y_true == 1) / sum(y_true == 1)
f1 <- 2 * (precision * recall) / (precision + recall)
metrics <- c(Accuracy = accuracy, Precision = precision, Recall = recall, F1 = f1, AUC = auc_val)
metrics


#All variables
model3 <- svyglm(
  prediabetic ~ energy_kcal + protein_gm + carbohydrate_gm + sugar_gm +
    fiber_gm + fat_gm + saturated_fat_gm +
    cholesterol_gm + vitamin_c_mg + calcium_mg + iron_mg + potassium_mg +
    sodium_mg + caffeine_mg + alcohol_gm + water_gm + 
    gender + age + race  + bmi_kgm  + waist_circumfrence,
  design = desn_train,
  family = quasibinomial()
)

summary(model3)
car::vif(model3)
exp(coef(model3))

logpred <- predict(model3, newdata=test_df,
                   type = "response")
y_true <- ifelse(test_df$prediabetic == "Yes", 1, 0)
y_pred_class <- ifelse(logpred >= 0.41, 1, 0)
w <- test_df$sample_weight
length(logpred)
nrow(test_df)
length(y_true)

# --- Evaluation ---
roc_obj <- roc(y_true, logpred, weights = w)
auc_val <- auc(roc_obj)
plot(roc_obj, col = "blue", main = paste("ROC Curve (AUC =", round(auc_val, 3), ")"))

# Confusion matrix & metrics
cm <- table(Predicted = y_pred_class, Actual = y_true)
cm
accuracy <- mean(y_pred_class == y_true)
precision <- sum(y_pred_class == 1 & y_true == 1) / sum(y_pred_class == 1)
recall <- sum(y_pred_class == 1 & y_true == 1) / sum(y_true == 1)
f1 <- 2 * (precision * recall) / (precision + recall)
metrics <- c(Accuracy = accuracy, Precision = precision, Recall = recall, F1 = f1, AUC = auc_val)
metrics


#Demographic and weight variables only
model4 <- svyglm(
  prediabetic ~ gender + age + race  + bmi_kgm  + waist_circumfrence,
  design = desn_train,
  family = quasibinomial()
)

summary(model4)
logpred <- predict(model4, newdata=test_df,
                   type = "response")
y_true <- ifelse(test_df$prediabetic == "Yes", 1, 0)
y_pred_class <- ifelse(logpred >= 0.41, 1, 0)
w <- test_df$sample_weight
length(logpred)
nrow(test_df)
length(y_true)

# --- Evaluation ---
roc_obj <- roc(y_true, logpred, weights = w)
auc_val <- auc(roc_obj)
plot(roc_obj, col = "blue", main = paste("ROC Curve (AUC =", round(auc_val, 3), ")"))

# Confusion matrix & metrics
cm <- table(Predicted = y_pred_class, Actual = y_true)
cm
accuracy <- mean(y_pred_class == y_true)
precision <- sum(y_pred_class == 1 & y_true == 1) / sum(y_pred_class == 1)
recall <- sum(y_pred_class == 1 & y_true == 1) / sum(y_true == 1)
f1 <- 2 * (precision * recall) / (precision + recall)
metrics <- c(Accuracy = accuracy, Precision = precision, Recall = recall, F1 = f1, AUC = auc_val)
metrics



############################################################

###Sampling weighted Principal Component Analysis - All Diet Variables

############################################################

#using design = desn_analysis for interpretation
svy_pca <- svyprcomp(~ energy_kcal + protein_gm + carbohydrate_gm + sugar_gm +
                         fiber_gm + fat_gm + saturated_fat_gm +
                         cholesterol_gm + vitamin_c_mg + calcium_mg + iron_mg + potassium_mg +
                         sodium_mg + caffeine_mg + alcohol_gm + water_gm,
                       design = desn_analysis,
                       center = TRUE,
                       scale. = TRUE,
                       na.action = na.omit)

summary(svy_pca)
svy_pca$rotation

par(mfrow = c(1,1))
#proportion of variance plot
plot(seq_along(svy_pca$sdev),
     (svy_pca$sdev^2) / sum(svy_pca$sdev^2),
     type = "b", pch = 19, col = "steelblue",
     main = "Scree Plot (Proportion of Variance)",
     xlab = "Principal Component", ylab = "Proportion of Variance Explained")


#cumulative variance plot
var_exp <- (svy_pca$sdev^2) / sum(svy_pca$sdev^2)
cum_var <- cumsum(var_exp)

plot(seq_along(cum_var), cum_var, type = "b", pch = 19, col = "darkorange",
     main = "Cumulative Variance Explained",
     xlab = "Principal Component", ylab = "Cumulative Proportion of Variance")


#Using design = desn_train for prediction
svy_pca <- svyprcomp(~ energy_kcal + protein_gm + carbohydrate_gm + sugar_gm +
                       fiber_gm + fat_gm + saturated_fat_gm +
                       cholesterol_gm + vitamin_c_mg + calcium_mg + iron_mg + potassium_mg +
                       sodium_mg + caffeine_mg + alcohol_gm + water_gm,
                     design = desn_train,
                     center = TRUE,
                     scale. = TRUE,
                     na.action = na.omit)

#extract pc scores
pc_scores_train <- as.data.frame(predict(svy_pca, newdata = model.frame(desn_train)))
pc_scores_train <- pc_scores_train[, 1:3]
colnames(pc_scores_train) <- c("PC1", "PC2", "PC3")

pc_scores_test <- as.data.frame(predict(svy_pca, newdata = model.frame(desn_test)))
pc_scores_test <- pc_scores_test[, 1:3]
colnames(pc_scores_test) <- c("PC1", "PC2", "PC3")

desn_train <- update(desn_train,
                     PC1 = pc_scores_train$PC1,
                     PC2 = pc_scores_train$PC2,
                     PC3 = pc_scores_train$PC3)

desn_test  <- update(desn_test,
                     PC1 = pc_scores_test$PC1,
                     PC2 = pc_scores_test$PC2,
                     PC3 = pc_scores_test$PC3)
head(desn_test$variables)

##PCA's only
model_pca <- svyglm(
  prediabetic ~ PC1 + PC2 + PC3,
  design = desn_train,
  family = quasibinomial()
)
summary(model_pca)
car::vif(model_pca)
exp(coef(model_pca))

test_df <- desn_test$variables
prob_pca <- predict(model_pca, newdata=test_df,
                   type = "response")
y_true <- ifelse(test_df$prediabetic == "Yes", 1, 0)
y_pred_class <- ifelse(prob_pca >= 0.41, 1, 0)
w <- test_df$sample_weight
length(prob_pca)
nrow(test_df)
length(y_true)

# --- Evaluation ---
roc_obj <- roc(y_true, prob_pca, weights = w)
auc_val <- auc(roc_obj)
plot(roc_obj, col = "blue", main = paste("ROC Curve (AUC =", round(auc_val, 3), ")"))

# Confusion matrix & metrics
cm <- table(Predicted = y_pred_class, Actual = y_true)
cm
accuracy <- mean(y_pred_class == y_true)
precision <- sum(y_pred_class == 1 & y_true == 1) / sum(y_pred_class == 1)
recall <- sum(y_pred_class == 1 & y_true == 1) / sum(y_true == 1)
f1 <- 2 * (precision * recall) / (precision + recall)
metrics <- c(Accuracy = accuracy, Precision = precision, Recall = recall, F1 = f1, AUC = auc_val)
metrics


##PCA's plus demographic variables
model_pca_full <- svyglm(
  prediabetic ~ PC1 + PC2 + PC3 + age + gender + race + bmi_kgm + waist_circumfrence,
  design = desn_train,
  family = quasibinomial()
)

summary(model_pca_full)
car::vif(model_pca_full)
exp(coef(model_pca_full))

test_df <- desn_test$variables
prob_pca <- predict(model_pca_full, newdata=test_df,
                    type = "response")
y_true <- ifelse(test_df$prediabetic == "Yes", 1, 0)
y_pred_class <- ifelse(prob_pca >= 0.41, 1, 0)
w <- test_df$sample_weight
length(prob_pca)
nrow(test_df)
length(y_true)

# --- Evaluation ---
roc_obj <- roc(y_true, prob_pca, weights = w)
auc_val <- auc(roc_obj)
plot(roc_obj, col = "blue", main = paste("ROC Curve (AUC =", round(auc_val, 3), ")"))

# Confusion matrix & metrics
cm <- table(Predicted = y_pred_class, Actual = y_true)
cm
accuracy <- mean(y_pred_class == y_true)
precision <- sum(y_pred_class == 1 & y_true == 1) / sum(y_pred_class == 1)
recall <- sum(y_pred_class == 1 & y_true == 1) / sum(y_true == 1)
f1 <- 2 * (precision * recall) / (precision + recall)
metrics <- c(Accuracy = accuracy, Precision = precision, Recall = recall, F1 = f1, AUC = auc_val)
metrics


##################################################

###Lasso variable selection (design-aware with wlasso)

##################################################


##Weighted Lasso

# --- Prepare Training Data ---
train_df <- as.data.frame(desn_train$variables)
train_df$pred <- ifelse(train_df$prediabetic == "Yes", 1, 0)

# --- Fit Survey-Aware Lasso ---
wlasso_fit <- wlasso(
  data = train_df,
  col.y = "pred",
  col.x = c("energy_kcal", "protein_gm", "carbohydrate_gm", "sugar_gm",
            "fiber_gm", "fat_gm", "saturated_fat_gm", "cholesterol_gm",
            "vitamin_c_mg", "calcium_mg", "iron_mg", "potassium_mg",
            "sodium_mg", "caffeine_mg", "alcohol_gm", "water_gm"),
  cluster = "masked_variance_pseudo_psu",
  strata = "masked_variance_pseudo_stratum",
  weights = "sample_weight",
  family = "binomial",
  method = "bootstrap",  
  B = 200                
)
wlasso_fit$lambda
summary(wlasso_fit)
wlasso.plot(wlasso_fit)


# Extract coefficient vector from final model
coef_vec <- as.matrix(wlasso_fit$model$min$beta)

# Get variable names with nonzero coefficients
selected_vars <- rownames(coef_vec)[coef_vec[, 1] != 0]
print(selected_vars)


##Logistic regression model using select variables
model_lasso <- svyglm(
  prediabetic ~ protein_gm + carbohydrate_gm + fiber_gm + 
    saturated_fat_gm + vitamin_c_mg + calcium_mg + iron_mg +
    potassium_mg + sodium_mg + caffeine_mg + alcohol_gm + 
    water_gm,
  design = desn_train,
  family = quasibinomial()
)

summary(model_lasso)
car::vif(model_lasso)
exp(coef(model_lasso))

test_df <- desn_test$variables
prob_lasso <- predict(model_lasso, newdata=test_df,
                      type = "response")
y_true <- ifelse(test_df$prediabetic == "Yes", 1, 0)
y_pred_class <- ifelse(prob_lasso >= 0.41, 1, 0)
w <- test_df$sample_weight
length(prob_lasso)
nrow(test_df)
length(y_true)

# --- Evaluation ---
roc_obj <- roc(y_true, prob_lasso, weights = w)
auc_val <- auc(roc_obj)
plot(roc_obj, col = "blue", main = paste("ROC Curve (AUC =", round(auc_val, 3), ")"))

# Confusion matrix & metrics
cm <- table(Predicted = y_pred_class, Actual = y_true)
cm
accuracy <- mean(y_pred_class == y_true)
precision <- sum(y_pred_class == 1 & y_true == 1) / sum(y_pred_class == 1)
recall <- sum(y_pred_class == 1 & y_true == 1) / sum(y_true == 1)
f1 <- 2 * (precision * recall) / (precision + recall)
metrics <- c(Accuracy = accuracy, Precision = precision, Recall = recall, F1 = f1, AUC = auc_val)
metrics



##Logistic regression model using select variables and all demographic
model_lasso_full <- svyglm(
  prediabetic ~ protein_gm + carbohydrate_gm + fiber_gm + 
    saturated_fat_gm + vitamin_c_mg + calcium_mg + iron_mg +
    potassium_mg + sodium_mg + caffeine_mg + alcohol_gm + 
    water_gm + age + gender + race + bmi_kgm + waist_circumfrence,
  design = desn_train,
  family = quasibinomial()
)


summary(model_lasso_full)
car::vif(model_lasso_full)
exp(coef(model_lasso_full))


test_df <- desn_test$variables
prob_lasso_full <- predict(model_lasso_full, newdata=test_df,
                           type = "response")
y_true <- ifelse(test_df$prediabetic == "Yes", 1, 0)
y_pred_class <- ifelse(prob_lasso_full >= 0.5, 1, 0)
w <- test_df$sample_weight
length(prob_lasso_full)
nrow(test_df)
length(y_true)

# --- Evaluation ---
roc_obj <- roc(y_true, prob_lasso_full, weights = w)
auc_val <- auc(roc_obj)
plot(roc_obj, col = "blue", main = paste("ROC Curve (AUC =", round(auc_val, 3), ")"))

# Confusion matrix & metrics
cm <- table(Predicted = y_pred_class, Actual = y_true)
cm
accuracy <- mean(y_pred_class == y_true)
precision <- sum(y_pred_class == 1 & y_true == 1) / sum(y_pred_class == 1)
recall <- sum(y_pred_class == 1 & y_true == 1) / sum(y_true == 1)
f1 <- 2 * (precision * recall) / (precision + recall)
metrics <- c(Accuracy = accuracy, Precision = precision, Recall = recall, F1 = f1, AUC = auc_val)
metrics




#########################################################

###classification prediction models

########################################################




#######################################

##Random forest 

#######################################

library(randomForest)
library(caret)
library(pROC)


train_df <- as.data.frame(desn_train$variables)
train_df$pred <- factor(ifelse(train_df$prediabetic == "Yes", 1, 0))
test_df  <- as.data.frame(desn_test$variables)
nrow(test_df)
head(train_df)



## all diet variables


rf_model <- randomForest(pred ~ energy_kcal + protein_gm + carbohydrate_gm + sugar_gm +
                           fiber_gm + fat_gm + saturated_fat_gm +
                           cholesterol_gm + vitamin_c_mg + calcium_mg + iron_mg + potassium_mg +
                           sodium_mg + caffeine_mg + alcohol_gm + water_gm,
                         data = train_df, ntree = 500,
                       mtry = floor(sqrt(ncol(train_df) - 1)),
                       importance = TRUE)
print(rf_model)
plot(rf_model)
prob_rf <- predict(rf_model, newdata = test_df, type = "prob")[, "1"]
 

y_true <- ifelse(test_df$prediabetic == "Yes", 1, 0)
y_pred_class <- ifelse(prob_rf >= 0.5, 1, 0)
w <- test_df$sample_weight
length(prob_rf)
nrow(test_df)
length(y_true)

# --- Evaluation ---
roc_obj <- roc(y_true, prob_rf, weights = w)
auc_val <- auc(roc_obj)
plot(roc_obj, col = "blue", main = paste("ROC Curve (AUC =", round(auc_val, 3), ")"))

# Confusion matrix & metrics
cm <- table(Predicted = y_pred_class, Actual = y_true)
cm
accuracy <- mean(y_pred_class == y_true)
precision <- sum(y_pred_class == 1 & y_true == 1) / sum(y_pred_class == 1)
recall <- sum(y_pred_class == 1 & y_true == 1) / sum(y_true == 1)
f1 <- 2 * (precision * recall) / (precision + recall)
metrics <- c(Accuracy = accuracy, Precision = precision, Recall = recall, F1 = f1, AUC = auc_val)
metrics


## Variable importance
importance(rf_model)
varImpPlot(rf_model)



##Random forest - all variables


#Regular train/test split
rf_model <- randomForest(pred ~ energy_kcal + protein_gm + carbohydrate_gm + sugar_gm +
                           fiber_gm + fat_gm + saturated_fat_gm +
                           cholesterol_gm + vitamin_c_mg + calcium_mg + iron_mg + potassium_mg +
                           sodium_mg + caffeine_mg + alcohol_gm + water_gm + age 
                         + gender + race + bmi_kgm + waist_circumfrence, data = train_df, ntree = 1000,
                         mtry = floor(sqrt(ncol(train_df) - 1)),
                         importance = TRUE)
print(rf_model)
plot(rf_model)
prob_rf <- predict(rf_model, newdata=test_df, type = "prob")[, "1"]


y_true <- ifelse(test_df$prediabetic == "Yes", 1, 0)
y_pred_class <- ifelse(prob_rf >= 0.5, 1, 0)
w <- test_df$sample_weight
length(prob_rf)
nrow(test_df)
length(y_true)

# --- Evaluation ---
roc_obj <- roc(y_true, prob_rf, weights = w)
auc_val <- auc(roc_obj)
plot(roc_obj, col = "blue", main = paste("ROC Curve (AUC =", round(auc_val, 3), ")"))

# Confusion matrix & metrics
cm <- table(Predicted = y_pred_class, Actual = y_true)
cm
accuracy <- mean(y_pred_class == y_true)
precision <- sum(y_pred_class == 1 & y_true == 1) / sum(y_pred_class == 1)
recall <- sum(y_pred_class == 1 & y_true == 1) / sum(y_true == 1)
f1 <- 2 * (precision * recall) / (precision + recall)
metrics <- c(Accuracy = accuracy, Precision = precision, Recall = recall, F1 = f1, AUC = auc_val)
metrics


## Variable importance
importance(rf_model)
varImpPlot(rf_model)



########################################

#XGBoost

#######################################


train_df <- as.data.frame(desn_train$variables)
train_df$pred <- ifelse(train_df$prediabetic == "Yes", 1, 0)

test_df <- as.data.frame(desn_test$variables)
test_df$pred <- ifelse(test_df$prediabetic == "Yes", 1, 0)

w <- test_df$sample_weight


#Diet variables only

diet_vars <- c("energy_kcal", "protein_gm", "carbohydrate_gm", "sugar_gm", "fiber_gm",
               "fat_gm", "saturated_fat_gm", "cholesterol_gm", "vitamin_c_mg", "calcium_mg",
               "iron_mg", "potassium_mg", "sodium_mg", "caffeine_mg", "alcohol_gm", "water_gm")

X_train_diet <- as.matrix(train_df[, diet_vars])
X_test_diet  <- as.matrix(test_df[, diet_vars])
y_train <- train_df$pred
y_test  <- test_df$pred

dtrain <- xgb.DMatrix(data = X_train_diet, label = y_train)
dtest  <- xgb.DMatrix(data = X_test_diet, label = y_test, weight = w)

params <- list(objective = "binary:logistic", eval_metric = "auc", max_depth = 6, eta = 0.1)
xgb_diet <- xgb.train(params = params, data = dtrain, nrounds = 100, verbose = 0)

par(mfrow = c(1,1))
library(DiagrammeR)
xgb.plot.tree(model = xgb_diet, trees = 0)
xgb.plot.tree(feature_names = colnames(X_train_diet),
              model = xgb_diet, trees = 0)

prob_xgb_diet <- predict(xgb_diet, newdata = dtest)
y_pred_class <- ifelse(prob_xgb_diet >= 0.5, 1, 0)

roc_obj <- roc(y_test, prob_xgb_diet, weights = w)
auc_val <- auc(roc_obj)
plot(roc_obj, col = "darkgreen", main = paste("ROC Curve (AUC =", round(auc_val, 3), ")"))

cm <- table(Predicted = y_pred_class, Actual = y_test)
cm
accuracy <- mean(y_pred_class == y_test)
precision <- sum(y_pred_class == 1 & y_test == 1) / sum(y_pred_class == 1)
recall <- sum(y_pred_class == 1 & y_test == 1) / sum(y_test == 1)
f1 <- 2 * (precision * recall) / (precision + recall)
metrics <- c(Accuracy = accuracy, Precision = precision, Recall = recall, F1 = f1, AUC = auc_val)
metrics

importance_matrix <- xgb.importance(feature_names = diet_vars, model = xgb_diet)
xgb.plot.importance(importance_matrix)


##All variables
all_vars <- c(diet_vars, "age", "gender", "race", "bmi_kgm", "waist_circumfrence")

X_train_all <- model.matrix(~ . -1, data = train_df[, all_vars])
X_test_all  <- model.matrix(~ . -1, data = test_df[, all_vars])

dtrain_all <- xgb.DMatrix(data = X_train_all, label = y_train)
dtest_all  <- xgb.DMatrix(data = X_test_all, label = y_test, weight = w)

xgb_all <- xgb.train(params = params, data = dtrain_all, nrounds = 200, verbose = 0)

xgb.plot.tree(model = xgb_all, trees = 0)

prob_xgb_all <- predict(xgb_all, newdata = dtest_all)
y_pred_class_all <- ifelse(prob_xgb_all >= 0.5, 1, 0)

roc_obj_all <- roc(y_test, prob_xgb_all, weights = w)
auc_val_all <- auc(roc_obj_all)
plot(roc_obj_all, col = "blue", main = paste("ROC Curve (AUC =", round(auc_val_all, 3), ")"))

cm_all <- table(Predicted = y_pred_class_all, Actual = y_test)
cm_all
accuracy_all <- mean(y_pred_class_all == y_test)
precision_all <- sum(y_pred_class_all == 1 & y_test == 1) / sum(y_pred_class_all == 1)
recall_all <- sum(y_pred_class_all == 1 & y_test == 1) / sum(y_test == 1)
f1_all <- 2 * (precision_all * recall_all) / (precision_all + recall_all)
metrics_all <- c(Accuracy = accuracy_all, Precision = precision_all, Recall = recall_all, F1 = f1_all, AUC = auc_val_all)
metrics_all

importance_matrix_all <- xgb.importance(feature_names = colnames(X_train_all), model = xgb_all)
importance_matrix_all
xgb.plot.importance(importance_matrix_all)



