# 单因素逻辑回归分析（修正版） ---------------------------------------------------------
library(tidyverse)
library(broom)
library(flextable)

# 加载清洗后的数据
AnalysisData <- readRDS("CleanedData_EN.rds")

# 添加新的分类变量
AnalysisData <- AnalysisData %>%
  mutate(
    # 收缩压分类变量
    SbpLessThan90 = ifelse(SystolicBloodPressure < 90, 1, 0),  # SBP < 90 mmHg
    SbpLessThan100 = ifelse(SystolicBloodPressure < 100, 1, 0), # SBP < 100 mmHg
    # 心率分类变量
    HeartRateOver100 = ifelse(HeartRate > 100, 1, 0),          # 心率 > 100 bpm
  #  HeartRateOver120 = ifelse(HeartRate > 120, 1, 0)           # 心率 > 120 bpm
  )

# 确保结局变量为二分类因子
AnalysisData$PunctureSuccess <- as.factor(AnalysisData$PunctureSuccess)
levels(AnalysisData$PunctureSuccess) <- c("Failure", "Success")  # 设定参考水平

# 定义需要排除的变量（包括您指定删除的变量）
variables_to_exclude <- c(
  "PatientId", "PunctureSuccess", "SurgeryName",
  "PunctureTimeSec", "FirstAttemptSuccess", 
  "ContrastVolumeMl", "RadiationDoseDap", 
  "TotalPunctureAttempts"
)

# 定义需要纳入分析的自变量（排除指定变量并包含新添加的变量）
predictor_vars <- setdiff(names(AnalysisData), variables_to_exclude)

# 数据质量检查函数
check_predictor_quality <- function(data, var) {
  # 检查缺失值
  na_count <- sum(is.na(data[[var]]))
  na_percent <- na_count / nrow(data) * 100
  
  # 检查唯一值数量
  unique_vals <- unique(na.omit(data[[var]]))
  unique_count <- length(unique_vals)
  
  # 生成检查结果
  list(
    variable = var,
    na_count = na_count,
    na_percent = na_percent,
    unique_count = unique_count,
    is_valid = na_percent < 50 & unique_count >= 2  # 缺失值<50%且至少有2个不同值
  )
}

# 检查所有自变量质量
predictor_quality <- map_dfr(predictor_vars, ~check_predictor_quality(AnalysisData, .x))
print("自变量质量检查结果：")
print(predictor_quality)

# 筛选有效的自变量
valid_predictors <- predictor_quality %>% 
  filter(is_valid) %>% 
  pull(variable)

print(paste("有效自变量数量：", length(valid_predictors)))
print("有效自变量列表：")
print(valid_predictors)

# 改进的单因素逻辑回归分析函数（增加错误处理）
run_univariate_logistic <- function(data, outcome, predictors) {
  results <- map_dfr(predictors, function(var) {
    tryCatch({
      # 构建回归公式
      formula_str <- paste(outcome, "~", var)
      
      # 处理变量中的NA值（临时移除含NA的行）
      model_data <- data %>% 
        select(all_of(c(outcome, var))) %>% 
        drop_na()
      
      # 如果样本量太少，跳过分析
      if(nrow(model_data) < 10) {
        return(tibble(
          Variable = var,
          OddsRatio = NA,
          OR_Lower = NA,
          OR_Upper = NA,
          PValue = NA,
          Note = "样本量不足"
        ))
      }
      
      # 检查结局变量是否有足够的变异
      outcome_vals <- unique(model_data[[outcome]])
      if(length(outcome_vals) < 2) {
        return(tibble(
          Variable = var,
          OddsRatio = NA,
          OR_Lower = NA,
          OR_Upper = NA,
          PValue = NA,
          Note = "结局变量变异不足"
        ))
      }
      
      model <- glm(as.formula(formula_str), data = model_data, family = binomial(link = "logit"))
      
      # 提取结果并整理
      tidy_model <- tidy(model, conf.int = TRUE) %>%
        filter(term != "(Intercept)") %>%  # 排除截距项
        mutate(
          Variable = var,
          OddsRatio = exp(estimate),
          OR_Lower = exp(conf.low),
          OR_Upper = exp(conf.high),
          PValue = p.value,
          Note = "分析成功"
        ) %>%
        select(Variable, OddsRatio, OR_Lower, OR_Upper, PValue, Note)
      
      return(tidy_model)
    }, error = function(e) {
      # 捕获并处理错误
      return(tibble(
        Variable = var,
        OddsRatio = NA,
        OR_Lower = NA,
        OR_Upper = NA,
        PValue = NA,
        Note = paste("分析出错:", e$message)
      ))
    })
  })
  
  return(results)
}

# 执行单因素逻辑回归分析（使用经过筛选的有效自变量）
univariate_results <- run_univariate_logistic(
  data = AnalysisData,
  outcome = "PunctureSuccess",
  predictors = valid_predictors
)

# 查看分析出错的变量
error_vars <- univariate_results %>% filter(is.na(OddsRatio))
print("分析出错或无效的变量：")
print(error_vars)

# 结果整理与格式化（仅包含成功分析的变量）
formatted_results <- univariate_results %>%
  filter(!is.na(OddsRatio)) %>%
  # 变量名替换为更易读的标签
  mutate(
    Variable = case_when(
      Variable == "Age" ~ "Age, years",
      Variable == "Female" ~ "Female gender",
      Variable == "SmokingStatus" ~ "Smoking status",
      Variable == "DrinkingStatus" ~ "Drinking status",
      Variable == "HeightCm" ~ "Height, cm",
      Variable == "WeightKg" ~ "Weight, kg",
      Variable == "BodyMassIndex" ~ "Body mass index, kg/m²",
      Variable == "Hypertension" ~ "Hypertension",
      Variable == "DiabetesMellitus" ~ "Diabetes mellitus",
      Variable == "AtrialFibrillation" ~ "Atrial fibrillation",
      Variable == "AcuteCoronarySyndrome" ~ "Acute coronary syndrome",
      Variable == "CerebralInfarction" ~ "Cerebral infarction",
      Variable == "PeripheralArteryDisease" ~ "Peripheral artery disease",
      Variable == "CoronaryArteryCalcification" ~ "Coronary artery calcification",
      Variable == "PriorIpsilateralRadialPuncture" ~ "Prior ipsilateral radial puncture",
      Variable == "AccessSide" ~ "Access side (left vs right)",
      Variable == "AnteriorWallApproach" ~ "Anterior wall approach",
      Variable == "SionWireUsed" ~ "SION wire used",
      Variable == "LeftVentricleEndDiastolicDiameter" ~ "Left ventricle end-diastolic diameter, mm",
      Variable == "LeftVentricleEjectionFraction" ~ "Left ventricle ejection fraction, %",
      Variable == "BrainNatriureticPeptide" ~ "Brain natriuretic peptide, pg/ml",
      Variable == "Hemoglobin" ~ "Hemoglobin, g/dL",
      Variable == "PlateletCount" ~ "Platelet count, ×10⁹/L",
      Variable == "SerumCreatinine" ~ "Serum creatinine, mg/dL",
      Variable == "UricAcid" ~ "Uric acid, mg/dL",
      Variable == "HighDensityLipoprotein" ~ "High density lipoprotein, mg/dL",
      Variable == "LowDensityLipoprotein" ~ "Low density lipoprotein, mg/dL",
      Variable == "AlanineTransaminase" ~ "Alanine transaminase, U/L",
      Variable == "AspartateTransaminase" ~ "Aspartate transaminase, U/L",
      Variable == "ActivatedPartialThromboplastinTime" ~ "Activated partial thromboplastin time, sec",
      Variable == "InternationalNormalizedRatio" ~ "International normalized ratio",
      Variable == "HeartRate" ~ "Heart rate, bpm",
      Variable == "SystolicBloodPressure" ~ "Systolic blood pressure, mmHg",
      Variable == "DiastolicBloodPressure" ~ "Diastolic blood pressure, mmHg",
      Variable == "PciDurationMin" ~ "PCI duration, min",
      Variable == "SbpLessThan90" ~ "Systolic blood pressure < 90 mmHg",
      Variable == "SbpLessThan100" ~ "Systolic blood pressure < 100 mmHg",
      Variable == "HeartRateOver100" ~ "Heart rate > 100 bpm",
      Variable == "HeartRateOver120" ~ "Heart rate > 120 bpm",
      TRUE ~ Variable
    ),
    # 格式化数值
    OddsRatio = sprintf("%.3f", OddsRatio),
    OR_CI = sprintf("(%.3f, %.3f)", OR_Lower, OR_Upper),
    PValue = ifelse(PValue < 0.001, "<0.001", sprintf("%.3f", PValue))
  ) %>%
  select(Variable, OddsRatio, OR_CI, PValue) %>%
  arrange(PValue)

# 创建表格并保存
table2 <- flextable(formatted_results) %>%
  set_caption("Table 2. Univariate Logistic Regression Analysis for Predictors of Puncture Success") %>%
  theme_booktabs() %>%
  autofit() %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  set_header_labels(
    Variable = "Variable",
    OddsRatio = "Odds Ratio",
    OR_CI = "95% Confidence Interval",
    PValue = "P-value"
  )

# 保存为Word文档
save_as_docx(table2, path = "Table2_Univariate_Regression_Revised.docx")

# 输出重要结果（P<0.05的变量）
significant_vars <- formatted_results %>%
  filter(PValue < 0.05 | PValue == "<0.001")

print("Variables with statistically significant association (P < 0.05):")
print(significant_vars)


##多因素 BNP+SBP+cor_calcium+stroke+pad+Scr
mod1<-glm(success~.,data,
          family = "binomial")
logit.step<-step(mod1,direction = c("forward"))   
summary(logit.step)

mod2<-glm(success~cor_calcium,data,
          family = "binomial")
tbl_regression(mod2, exponentiate = TRUE)





names(data)
table1 <-tbl_regression(mod12, exponentiate = TRUE)
table1 <- as_flex_table(table1)
save_as_docx(table1, path = "mimic_logit.docx")
