setwd('C:/Users/lxqji/OneDrive/R/2025lfx/data')

library(dplyr)
library(data.table)
library(readxl)
library(openxlsx) 

# 0. 环境 -------------------------------------------------------------------
library(tidyverse)
library(ggplot2)

df <- read_excel("D:/OneDrive/R/2025lfx/data/首次成功原因分析(1).xlsx")

table(is.na(df$疼痛出汗))

瘀斑	手指麻木	疼痛出汗




parse_puncture <- function(x){
  str_split(x, "\\+") %>%          # 按“+”拆
    map(~as.numeric(.x)) %>%       # 各段转数值
    map_dbl(sum)                   # 每行求和
}

# 1. 清洗关键字段 -------------------------------------------------
df <- df %>% 
  mutate(
    puncture_n = parse_puncture(`穿刺次数`),      # 变成可计算的数值
    time_sec   = as.numeric(`穿刺时间（秒）`),    # 确保是数值
    group20    = ceiling(row_number() / 20),      # 每 20 例一组
    success    = `穿刺成功`                       # 0/1 字段
  )


# 1. 取前 90 例，并按 30 例一组重新分组 ---------------------------
df90 <- df %>% slice(1:150) %>% mutate(group30 = ceiling(row_number() / 30))

# 2. 按 30 例汇总 ------------------------------------------------------------
sum30 <- df90 %>% 
  group_by(group30) %>% 
  summarise(
    n             = n(),
    success_rate  = mean(success, na.rm = TRUE),
    mean_puncture = mean(puncture_n, na.rm = TRUE),
    median_time   = median(time_sec, na.rm = TRUE),
    .groups       = "drop"
  )

# 3. 绘图函数 ---------------------------------------------------------------
save_pdf <- function(plot, file){ pdf(file, width = 8, height = 6); print(plot); dev.off() }

## 图 1  成功率学习曲线
p1 <- ggplot(sum30, aes(group30, success_rate)) +
  geom_line(colour = "steelblue", linewidth = 1.2) +
  geom_point(shape = 21, fill = "white", size = 3) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Learning Curve for Distal Radial Access Success Rate",
       x = "Every 30 Cases", y = "Success Rate") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
save_pdf(p1, "fig1_success_rate.pdf")

## 图 2  平均穿刺次数
p2 <- ggplot(sum30, aes(group30, mean_puncture)) +
  geom_line(colour = "firebrick", linewidth = 1.2) +
  geom_point(shape = 21, fill = "white", size = 3) +
  labs(title = "Learning Curve for Mean Puncture Attempts",
       x = "Every 30 Cases", y = "Mean Puncture Attempts") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
save_pdf(p2, "fig2_puncture_attempts.pdf")

## 图 3  中位穿刺时间
p3 <- ggplot(sum30, aes(group30, median_time)) +
  geom_line(colour = "darkgreen", linewidth = 1.2) +
  geom_point(shape = 21, fill = "white", size = 3) +
  labs(title = "Learning Curve for Median Puncture Time",
       x = "Every 30 Cases", y = "Median Puncture Time (seconds)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
save_pdf(p3, "fig3_median_time.pdf")

