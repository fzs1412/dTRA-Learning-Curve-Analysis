# --- dTRA穿刺成功预测因素的LASSO回归分析 ---
# 作者: 医学统计学专家
# 日期: 2025-09-16
# 目的: 在穿刺失败事件较少的情况下，使用LASSO回归从多个变量中筛选出关键的预测因素。
# 版本: V7 - 调整A/B标签位置，进一步左移
setwd('C:/Users/lxqji/OneDrive/R/2025lfx/data')
# 1. 准备工作：加载所需的R包 --------------------------------------------------------
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(glmnet)) install.packages("glmnet")
if (!require(mice)) install.packages("mice")

library(tidyverse)
library(glmnet)
library(mice)

# 2. 数据加载与准备 -----------------------------------------------------------
file_path <- "C:/Users/lxqji/OneDrive/R/2025lfx/data/df_clean_EN.rds"
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

# 5. 绘制两个LASSO分析图表并保存为PDF -----------------------------------------

# 创建PDF图形设备，用于同时展示两个图（PDF格式）
pdf("LASSO_analysis_figures.pdf", width = 10, height = 5)
# 调整边距，为左侧标签留出空间
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)  

# Panel A: 系数轮廓图（左上角标注A，进一步左移）
plot(cv_lasso_model$glmnet.fit, xvar = "lambda", label = TRUE)
# 使用adj = -0.1使标签更靠左，line = 1.5调整垂直位置
mtext("A", side = 3, line = 1.5, adj = -0.1, cex = 1.2, font = 2)
abline(v = log(cv_lasso_model$lambda.min), col = "red", lty = 2)
abline(v = log(cv_lasso_model$lambda.1se), col = "blue", lty = 2)
legend("topleft", legend = c("lambda.min", "lambda.1se"), 
       col = c("red", "blue"), lty = 2, cex = 0.8, bty = "n", inset = 0.05)

# Panel B: 交叉验证误差图（左上角标注B，进一步左移）
plot(cv_lasso_model)
mtext("B", side = 3, line = 1.5, adj = -0.1, cex = 1.2, font = 2)

# 关闭PDF图形设备
dev.off()

# 同时在RStudio中显示图形
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)

# Panel A: 系数轮廓图
plot(cv_lasso_model$glmnet.fit, xvar = "lambda", label = TRUE)
mtext("A", side = 3, line = 1.5, adj = -0.1, cex = 1.2, font = 2)
abline(v = log(cv_lasso_model$lambda.min), col = "red", lty = 2)
abline(v = log(cv_lasso_model$lambda.1se), col = "blue", lty = 2)
legend("topleft", legend = c("lambda.min", "lambda.1se"), 
       col = c("red", "blue"), lty = 2, cex = 0.8, bty = "n", inset = 0.05)

# Panel B: 交叉验证误差图
plot(cv_lasso_model)
mtext("B", side = 3, line = 1.5, adj = -0.1, cex = 1.2, font = 2)

# 恢复默认图形设置
par(mfrow = c(1, 1))

# 6. 提取并展示最终结果 (双重标准) --------------------------------------------

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
    
