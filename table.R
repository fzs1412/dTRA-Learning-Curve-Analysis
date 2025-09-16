# 0. 环境 ----------------------------------------------------------------------
library(tidyverse)
library(stringr)

# 1. 读入 ---------------------------------------------------------------------
df_raw <- read_excel("D:/OneDrive/R/2025lfx/data/首次成功原因分析4.xlsx")

# 2. 统一清洗函数 -------------------------------------------------------------
df <- df_raw %>% 
  mutate(
    # ---- 基本人口学 ----
    age      = as.numeric(str_extract(`年龄`, "\\d+")),
    female   = ifelse(`性别` == "女", 1, 0),
    height   = as.numeric(`身高`),
    weight   = as.numeric(`体重`),
    bmi      = ifelse(is.na(`BMI`), weight/(height/100)^2, as.numeric(`BMI`)),
    
    # ---- 手术相关 ----
    puncture_n  = map_dbl(str_split(`穿刺次数`, "\\+"), ~ sum(as.numeric(.x))),
    time_sec    = as.numeric(`穿刺时间（秒）`),
    access_side = `穿刺部位（右0，左1）`,   # 0=right, 1=left
    success     = `穿刺成功`,
    first_try   = `首穿成功`,
    anterior    = `前壁法`,
    sion_wire   = `SION导丝`,
    surgery    = `手术名称`,
    # ---- 合并症 / 病史 ----
    hypertension = `HBP`,
    diabetes     = `DM`,
    smoke        = `吸烟`,
    drink        = `饮酒`,
    prev_ra      = `同侧桡动脉穿刺史`,
    af_af        = `Af/AF`,
    acs          = `ACS`,
    stroke       = `CI脑梗`,
    pad          = `外周血管疾病（颈动脉狭窄、下肢硬化闭塞、肾动脉狭窄）`,
    cor_calcium  = `冠脉钙化（轻度1、中度2、重度3）`,
    
    # ---- 实验室 ----
    # ---- 心功能 / 影像 ----
    lvedd = as.numeric(`LVDd（mm）`),
    ef    = as.numeric(`EF`),
    BNP    = as.numeric(`BNP`),
    drink    = as.numeric(`drink`),
       # ---- 其他 ----
    contrast_dose = as.numeric(`碘剂用量`),
    dose_area_product = as.numeric(`放射剂量`)
  ) 

df <- df %>%
  filter( !住院号== 641443  )%>%
  filter( !住院号== 605044  )
summary(df)
names(df)
df <- df %>%
  # 只保留英文、数值列，方便后续
  select(id = 住院号, age, female, smoke, drink, height, weight, bmi,
         hypertension, diabetes, af_af, acs, stroke, pad, cor_calcium,
         success,surgery,PCItime,prev_ra, 
    puncture_n, time_sec, access_side,  first_try, anterior, sion_wire,
    contrast_dose, dose_area_product,lvedd, ef, BNP,
    HB, PLT, Scr, UA, HDL, LDL, ALT, AST, APTT, INR,
    HR,SBP,DBP)

# 3. 保存（可选） --------------------------------------------------------------
saveRDS(df, "df_clean_EN.rds")
df <- readRDS("C:/Users/lxqji/OneDrive/R/2025lfx/data/df_clean_EN.rds")
summary(df$PCItime)
df$time_sec=ifelse( is.na(df$time_sec) ,2000,df$time_sec        )
df$PCItime=ifelse( is.na(df$PCItime) ,69,df$PCItime       )

library(mice)
aa <- mice(dat[,1:26], seed=123)
dat1<-complete(aa, action=3)

 



