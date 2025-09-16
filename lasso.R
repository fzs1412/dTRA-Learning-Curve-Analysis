# --- dTRA穿刺成功预测因素的LASSO回归分析 ---
# 作者: 医学统计学专家
# 日期: 2025-09-11
# 目的: 在穿刺失败事件较少的情况下，使用LASSO回归从多个变量中筛选出关键的预测因素。
# 版本: V3 - 同时使用 lambda.1se 和 lambda.min 标准进行分析

# 1. 准备工作：加载所需的R包 --------------------------------------------------------
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(glmnet)) install.packages("glmnet")
if (!require(mice)) install.packages("mice")

library(tidyverse)
library(glmnet)
library(mice)

# 2. 数据加载与准备 -----------------------------------------------------------
file_path <- "D:/OneDrive/R/2025lfx/data/df_clean_EN.rds"
if (!file.exists(file_path)) {
  stop("错误：找不到清洗后的数据文件。请先运行数据清洗脚本。")
}
data_clean <- readRDS(file_path)
imputed_data <- mice(data_clean, m = 1, method = 'pmm', seed = 123) 
data_complete <- complete(imputed_data, 1)

# 3. 为LASSO模型准备数据 -----------------------------------------------------
y <- data_complete$success
predictors_data <- data_complete %>% 
  select(
    -id, -success, -puncture_n, -surgery, -PCItime, -time_sec,
    -first_try, -contrast_dose, -dose_area_product
  )
X <- model.matrix(~ . - 1, data = predictors_data)

# 4. 执行LASSO回归并进行交叉验证 --------------------------------------------
set.seed(42) 
cv_lasso_model <- cv.glmnet(X, y, alpha = 1, family = "binomial")
plot(cv_lasso_model)
title("LASSO交叉验证曲线图", line = 2.5)

# 5. 提取并展示最终结果 (双重标准) --------------------------------------------

# --- 标准一：使用最严格的 lambda.1se ---
lambda_1se <- cv_lasso_model$lambda.1se
cat("最优Lambda值 (lambda.1se, 最简洁模型):", lambda_1se, "\n")
coeffs_1se <- coef(cv_lasso_model, s = lambda_1se)
selected_vars_1se <- as.data.frame(as.matrix(coeffs_1se))
names(selected_vars_1se)[1] <- "系数"
selected_vars_1se <- selected_vars_1se %>%
  rownames_to_column("变量名") %>%
  filter(系数 != 0)
cat("--- 结果 (标准一：lambda.1se) ---\n")
print(selected_vars_1se)
cat("\n\n")

# --- 标准二：使用交叉验证误差最小的 lambda.min ---
lambda_min <- cv_lasso_model$lambda.min
cat("最优Lambda值 (lambda.min, 最佳拟合模型):", lambda_min, "\n")
coeffs_min <- coef(cv_lasso_model, s = lambda_min)
selected_vars_min <- as.data.frame(as.matrix(coeffs_min))
names(selected_vars_min)[1] <- "系数"
selected_vars_min <- selected_vars_min %>%
  rownames_to_column("变量名") %>%
  filter(系数 != 0)
cat("--- 结果 (标准二：lambda.min) ---\n")
print(selected_vars_min)

# --- 如何解读结果 ---
# (Intercept): 这是截距项，代表了基础的成功概率（取对数）。
# 正系数: 说明这个变量的增加，会提升穿刺成功的概率。
# 负系数: 说明这个变量的增加，会降低穿刺成功的概率（即与穿刺失败正相关）。
