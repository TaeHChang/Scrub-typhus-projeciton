# Revision notice: this includes exploratory author visualization code.
# For the manuscript-aligned Figure 5, execute scripts/05_figure5_seasonality.R separately.
# Original file contains other sections that require author input objects.

#################### Data visualisation

library(tidyr);library(dplyr);library(purrr) ;library(lubridate) ; library(readxl); library(ggplot2)
library(sf) ; library(tibble) ; library(ggbreak);library(grid)

load("C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\AR_projection/AR_wholecountry_5000.RData")

names(anabs_list)
anabs_over28
### Figure 1. Pooled association and SidoBlup. temperature의 distribution에 대한 histogram 추가.


### Figure 2. Climate change 에 대한 information. 전국 단위 시계열 그래프.


### Figure 3. 전국 단위 climate change temperature 지도.


### Figure 4. figure 2와 동일한 구성으로 AR에 대한 시각화. 전국 단위 시계열 그래프와 지도. growth rate.
## 시계열 그래프는 total, over10, under10 모두.

### Figure 5. 지역별로 bar plot을 그리거나 해서, AF가 시간에 따라서 변화하는 것을 보여주고, 그걸 분해해서 그 안에서는 under와 over의 추이가 어떻게 달라지는지.
## 그리고 만약에 지역별로 묶일 수 있는 패턴이 있으면, 예를 들어서 위도에 따라서 나눈다거나. 그러면 그걸로 묶어서도 보여주면 좋을듯.


### Figure 6. Avoidable cases map으로. %로 해야지. SSP245 대비 다른 시나리오로 해도 될듯.




############ Appendix figure. 기온 분포의 변화.

## histogram
# 주별 온도 데이터
# 히스토그램 생성
temp_dist_baseline <- sido_combined_data_SSP245_final[["whole country"]] %>% 
  filter(year %in% c(2001:2010))
temp_dist_SSP245 <- sido_combined_data_SSP245_final[["whole country"]] %>% 
  filter(year %in% c(2046:2050))
temp_dist_SSP370 <- sido_combined_data_SSP370_final[["whole country"]] %>% 
  filter(year %in% c(2046:2050))
temp_dist_SSP585 <- sido_combined_data_SSP585_final[["whole country"]] %>% 
  filter(year %in% c(2046:2050))

temp_dist_baseline <-  weekly_data_SiDo %>%
  filter(year < 2020 | year > 2009) %>% 
  dplyr::select(weekly_tmean) %>% 
  as.matrix() %>% 
  as.vector()  %>% 
  as.numeric()

temp_dist_baseline <-  temp_dist_baseline %>%
  dplyr::select(-year, -week) %>%   # year, week 열 제외
  as.matrix() %>%            # 데이터를 행렬로 변환
  as.vector()
temp_dist_SSP245 <-  temp_dist_SSP245 %>%
  dplyr::select(-year, -week) %>%   # year, week 열 제외
  as.matrix() %>%            # 데이터를 행렬로 변환
  as.vector()
temp_dist_SSP370 <-  temp_dist_SSP370 %>%
  dplyr::select(-year, -week) %>%   # year, week 열 제외
  as.matrix() %>%            # 데이터를 행렬로 변환
  as.vector()
temp_dist_SSP585 <-  temp_dist_SSP585 %>%
  dplyr::select(-year, -week) %>%   # year, week 열 제외
  as.matrix() %>%            # 데이터를 행렬로 변환
  as.vector()

range(temp_dist_SSP585)

hist(temp_dist_baseline,
     main = "Histogram of Weekly Mean Temperatures",  # 제목 설정
     xlab = "Weekly Mean Temperature (°C)",  # x축 레이블 설정
     ylab = "Frequency",  # y축 레이블 설정
     col = "lightblue",
     border = "white",
     breaks = 60,  xlim = c(-10, 34),
     xaxt = "n")   
axis(1, at = seq(-10, 34, by = 2)) 

hist(temp_dist_SSP585,
     main = "Histogram of Weekly Mean Temperatures",  # 제목 설정
     xlab = "Weekly Mean Temperature (°C)",  # x축 레이블 설정
     ylab = "Frequency",  # y축 레이블 설정
     col = "lightblue",
     border = "white",
     breaks = 60,  xlim = c(-10, 34),
     xaxt = "n")   
axis(1, at = seq(-10, 34, by = 2)) 








#################### Figure 생성에 용이한 형태로 데이터 변형.

load(file = "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\AR_projection/AR_sido_5000.RData")

####### anabs_list를 데이터프레임 리스트로 변환하는 함수
convert_anabs_list_to_df <- function(anabs_list) {
  # 결과를 저장할 리스트 초기화
  anabs_df_list <- list()
  
  # anabs_list의 각 항목에 대해 변환 수행
  for (name in names(anabs_list)) {
    # 현재 anabs 배열 가져오기
    anabs <- anabs_list[[name]]
    
    # 배열의 차원 이름 추출
    periods <- dimnames(anabs)[[1]]  # "2001-2005", "2006-2010", ...
    scenarios <- dimnames(anabs)[[4]]  # "SSP245", "SSP370", "SSP585"
    
    # 현재 배열을 데이터프레임으로 변환
    anabs_df <- data.frame(
      Scenario = character(),
      Period = character(),
      est = numeric(),
      ci_l = numeric(),
      ci_u = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (s in scenarios) {
      for (p in periods) {
        # 배열에서 값 추출
        est <- anabs[p, "est", 1, s]
        ci_l <- anabs[p, "ci.l", 1, s]
        ci_u <- anabs[p, "ci.u", 1, s]
        
        # 데이터프레임에 행 추가
        anabs_df <- rbind(
          anabs_df,
          data.frame(
            Scenario = s,
            Period = p,
            est = est,
            ci_l = ci_l,
            ci_u = ci_u,
            stringsAsFactors = FALSE
          )
        )
      }
    }
    
    # 변환된 데이터프레임을 리스트에 저장
    anabs_df_list[[name]] <- anabs_df
  }
  
  return(anabs_df_list)
}

# 함수 실행
anabs_df_list <- convert_anabs_list_to_df(anabs_list)
anrel_df_list <- convert_anabs_list_to_df(anrel_list)
afrel_df_list <- convert_anabs_list_to_df(afrel_list)
afrel_df_list <- convert_anabs_list_to_df(afrel_list)



#################################################################################
### Figure 2. AN 에 대한 시계열 그래프. 
### Figure 2 의 temperature 시계열 그래프는 별도의 R code로 만들어두었음.

#### AN 변화에 대한 시계열 그래프. 
#### 기준 기간 대비 

ggplot(anrel_df_list[['whole country']] %>% filter(as.numeric(substr(Period, 1, 4)) <= 2050),  # 2055년까지만 필터링
       aes(x = Period, y = est, color = Scenario, group = Scenario)) +
  geom_line(position = position_dodge(width = 0.2), size = 1.5) +  # 선을 그리며 약간씩 떨어뜨림
  geom_point(position = position_dodge(width = 0.2), size = 4) +  # 각 Period마다 점을 찍음
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u),  # 신뢰구간(CI) 설정
                position = position_dodge(width = 0.2), width = 0.2) +  # Errorbar 설정
  scale_color_manual(values = c("#0080FF", "#FF9933", "#FF0000")) +  # 사용자 지정 색상
  scale_y_continuous(limits = c(-1500, 5000)) +  # y축 범위 설정
  labs(title = "Excess cases by Scenario and Period (Up to 2055)",  # 제목 설정
       x = "Period",  # x축 레이블
       y = "Excess cases",  # y축 레이블
       color = "Scenario") +  # 범례 제목 설정
  theme_minimal() +  # 미니멀한 테마 적용
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # x축 눈금 45도 회전
        panel.grid = element_blank())  # 격자 제거



############## Baseline period 바꾼 버전.

dodge_w <- 0.2

df_plot <- anabs_df_list[['whole country']] %>%
  # 2016–2050(= 2016–2020 ~ 2046–2050)만 사용
  filter(dplyr::between(as.numeric(substr(Period, 1, 4)), 2016, 2046)) %>%
  # Keep original scenario-specific estimates, including the 2016–2020 baseline.
  mutate(start_year = as.integer(substr(Period, 1, 4))) 

df_plot <- df_plot %>%
  mutate(
    # 2016–2020 구간은 에러바 숨기기
    ci_l_plot = ifelse(Period == "2016-2020", NA_real_, ci_l),
    ci_u_plot = ifelse(Period == "2016-2020", NA_real_, ci_u),
    # x축 순서 고정
    Period = factor(Period, levels = unique(Period[order(start_year)]))
  )

ggplot(df_plot, aes(x = Period, y = est, color = Scenario, group = Scenario)) +
  geom_line(position = position_dodge(width = dodge_w), size = 1.5) +
  geom_point(position = position_dodge(width = dodge_w), size = 5) +
  geom_errorbar(
    aes(ymin = ci_l_plot, ymax = ci_u_plot),
    position = position_dodge(width = dodge_w),
    width = 0.2
  ) +
  scale_color_manual(values = c("#0080FF", "#FF9933", "#FF0000")) +
  labs(
    title = "Temperature-attributable cases by scenario and period (2016–2050)",
    x = "Period",
    y = "Excess cases",
    color = "Scenario"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

############################ Figure 3, 4 Temperature의 historical, projection map.

############### 기간별 평균 기온과 baseline으로 부터의 increase를 각 시도별로.
# ANNUAL AVERAGES (CENTERED)

load("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/기후변화 데이터/기후변화 데이터 filtered/final/sido_combined_SSP245_final_update.RData")
load("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/기후변화 데이터/기후변화 데이터 filtered/final/sido_combined_SSP370_final_update.RData")
load("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/기후변화 데이터/기후변화 데이터 filtered/final/sido_combined_SSP585_final_update.RData")


sido_SSP245_temp_increase <- list()
sido_SSP370_temp_increase <- list()
sido_SSP585_temp_increase <- list()

for (sido in names(sido_combined_data_SSP245_final)) {
  
  # 각 시도별 데이터프레임을 가져오기
  total_SSP245 <- sido_combined_data_SSP245_final[[sido]]
  total_SSP370 <- sido_combined_data_SSP370_final[[sido]]
  total_SSP585 <- sido_combined_data_SSP585_final[[sido]]
  
  # total_SSP245 데이터프레임에 period 열 추가
  total_SSP245 <- total_SSP245 %>%
    mutate(period = case_when(
      year >= 2001 & year <= 2005 ~ "2001-2005",
      year >= 2006 & year <= 2010 ~ "2006-2010",
      year >= 2011 & year <= 2015 ~ "2011-2015",
      year >= 2016 & year <= 2020 ~ "2016-2020",
      year >= 2021 & year <= 2025 ~ "2021-2025",
      year >= 2026 & year <= 2030 ~ "2026-2030",
      year >= 2031 & year <= 2035 ~ "2031-2035",
      year >= 2036 & year <= 2040 ~ "2036-2040",
      year >= 2041 & year <= 2045 ~ "2041-2045",
      year >= 2046 & year <= 2050 ~ "2046-2050",
      year >= 2051 & year <= 2055 ~ "2051-2055",
      year >= 2056 & year <= 2060 ~ "2056-2060",
      year >= 2061 & year <= 2065 ~ "2061-2065",
      year >= 2066 & year <= 2070 ~ "2066-2070",
      year >= 2071 & year <= 2075 ~ "2071-2075",
      year >= 2076 & year <= 2080 ~ "2076-2080",
      year >= 2081 & year <= 2085 ~ "2081-2085",
      year >= 2086 & year <= 2090 ~ "2086-2090",
      year >= 2091 & year <= 2095 ~ "2091-2095",
      year >= 2096 & year <= 2100 ~ "2096-2100",
      TRUE ~ NA_character_  # 그 외 값들은 NA로 처리
    ))
  
  total_SSP370 <- total_SSP370 %>%
    mutate(period = case_when(
      year >= 2001 & year <= 2005 ~ "2001-2005",
      year >= 2006 & year <= 2010 ~ "2006-2010",
      year >= 2011 & year <= 2015 ~ "2011-2015",
      year >= 2016 & year <= 2020 ~ "2016-2020",
      year >= 2021 & year <= 2025 ~ "2021-2025",
      year >= 2026 & year <= 2030 ~ "2026-2030",
      year >= 2031 & year <= 2035 ~ "2031-2035",
      year >= 2036 & year <= 2040 ~ "2036-2040",
      year >= 2041 & year <= 2045 ~ "2041-2045",
      year >= 2046 & year <= 2050 ~ "2046-2050",
      year >= 2051 & year <= 2055 ~ "2051-2055",
      year >= 2056 & year <= 2060 ~ "2056-2060",
      year >= 2061 & year <= 2065 ~ "2061-2065",
      year >= 2066 & year <= 2070 ~ "2066-2070",
      year >= 2071 & year <= 2075 ~ "2071-2075",
      year >= 2076 & year <= 2080 ~ "2076-2080",
      year >= 2081 & year <= 2085 ~ "2081-2085",
      year >= 2086 & year <= 2090 ~ "2086-2090",
      year >= 2091 & year <= 2095 ~ "2091-2095",
      year >= 2096 & year <= 2100 ~ "2096-2100",
      TRUE ~ NA_character_  # 그 외 값들은 NA로 처리
    ))
  
  total_SSP585 <- total_SSP585 %>%
    mutate(period = case_when(
      year >= 2001 & year <= 2005 ~ "2001-2005",
      year >= 2006 & year <= 2010 ~ "2006-2010",
      year >= 2011 & year <= 2015 ~ "2011-2015",
      year >= 2016 & year <= 2020 ~ "2016-2020",
      year >= 2021 & year <= 2025 ~ "2021-2025",
      year >= 2026 & year <= 2030 ~ "2026-2030",
      year >= 2031 & year <= 2035 ~ "2031-2035",
      year >= 2036 & year <= 2040 ~ "2036-2040",
      year >= 2041 & year <= 2045 ~ "2041-2045",
      year >= 2046 & year <= 2050 ~ "2046-2050",
      year >= 2051 & year <= 2055 ~ "2051-2055",
      year >= 2056 & year <= 2060 ~ "2056-2060",
      year >= 2061 & year <= 2065 ~ "2061-2065",
      year >= 2066 & year <= 2070 ~ "2066-2070",
      year >= 2071 & year <= 2075 ~ "2071-2075",
      year >= 2076 & year <= 2080 ~ "2076-2080",
      year >= 2081 & year <= 2085 ~ "2081-2085",
      year >= 2086 & year <= 2090 ~ "2086-2090",
      year >= 2091 & year <= 2095 ~ "2091-2095",
      year >= 2096 & year <= 2100 ~ "2096-2100",
      TRUE ~ NA_character_  # 그 외 값들은 NA로 처리
    ))
  
  avg_SSP245 <- total_SSP245 %>%
    group_by(period) %>%
    summarise(
      avg_ACCESS_CM2 = mean(`ACCESS-CM2`, na.rm = TRUE),
      avg_CanESM5 = mean(`CanESM5`, na.rm = TRUE),
      avg_CESM2 = mean(`CESM2`, na.rm = TRUE),
      avg_CMCC_ESM2 = mean(`CMCC-ESM2`, na.rm = TRUE),
      avg_CNRM_CM6_1 = mean(`CNRM-CM6-1`, na.rm = TRUE),
      avg_CNRM_ESM2_1 = mean(`CNRM-ESM2-1`, na.rm = TRUE),
      avg_INM_CM4_8 = mean(`INM-CM4-8`, na.rm = TRUE),
      avg_KACE_1_0_G = mean(`KACE-1-0-G`, na.rm = TRUE),
      avg_MIROC_ES2L = mean(`MIROC-ES2L`, na.rm = TRUE),
      avg_MPI_ESM1_2_LR = mean(`MPI-ESM1-2-LR`, na.rm = TRUE),
      avg_EC_Earth3 = mean(`EC-Earth3`, na.rm = TRUE),
      avg_FGOALS_g3 = mean(`FGOALS-g3`, na.rm = TRUE),
      avg_INM_CM5_0 = mean(`INM-CM5-0`, na.rm = TRUE),
      avg_UKESM1_0_LL = mean(`UKESM1-0-LL`, na.rm = TRUE)
    ) %>%
    # GCM 평균값을 계산하고 새로운 열 'GCM_mean' 추가
    mutate(GCM_mean = rowMeans(across(starts_with("avg_")), na.rm = TRUE)) %>%
    # 'period'와 'GCM_mean'만 선택
    dplyr::select(period, GCM_mean)
  
  avg_SSP370 <- total_SSP370 %>%
    group_by(period) %>%
    summarise(
      avg_ACCESS_CM2 = mean(`ACCESS-CM2`, na.rm = TRUE),
      avg_CanESM5 = mean(`CanESM5`, na.rm = TRUE),
      avg_CESM2 = mean(`CESM2`, na.rm = TRUE),
      avg_CMCC_ESM2 = mean(`CMCC-ESM2`, na.rm = TRUE),
      avg_CNRM_CM6_1 = mean(`CNRM-CM6-1`, na.rm = TRUE),
      avg_CNRM_ESM2_1 = mean(`CNRM-ESM2-1`, na.rm = TRUE),
      avg_INM_CM4_8 = mean(`INM-CM4-8`, na.rm = TRUE),
      avg_KACE_1_0_G = mean(`KACE-1-0-G`, na.rm = TRUE),
      avg_MIROC_ES2L = mean(`MIROC-ES2L`, na.rm = TRUE),
      avg_MPI_ESM1_2_LR = mean(`MPI-ESM1-2-LR`, na.rm = TRUE),
      avg_EC_Earth3 = mean(`EC-Earth3`, na.rm = TRUE),
      avg_FGOALS_g3 = mean(`FGOALS-g3`, na.rm = TRUE),
      avg_INM_CM5_0 = mean(`INM-CM5-0`, na.rm = TRUE),
      avg_UKESM1_0_LL = mean(`UKESM1-0-LL`, na.rm = TRUE)
    ) %>%
    mutate(GCM_mean = rowMeans(across(starts_with("avg_")), na.rm = TRUE)) %>%
    dplyr::select(period, GCM_mean)
  
  avg_SSP585 <- total_SSP585 %>%
    group_by(period) %>%
    summarise(
      avg_ACCESS_CM2 = mean(`ACCESS-CM2`, na.rm = TRUE),
      avg_CanESM5 = mean(`CanESM5`, na.rm = TRUE),
      avg_CESM2 = mean(`CESM2`, na.rm = TRUE),
      avg_CMCC_ESM2 = mean(`CMCC-ESM2`, na.rm = TRUE),
      avg_CNRM_CM6_1 = mean(`CNRM-CM6-1`, na.rm = TRUE),
      avg_CNRM_ESM2_1 = mean(`CNRM-ESM2-1`, na.rm = TRUE),
      avg_INM_CM4_8 = mean(`INM-CM4-8`, na.rm = TRUE),
      avg_KACE_1_0_G = mean(`KACE-1-0-G`, na.rm = TRUE),
      avg_MIROC_ES2L = mean(`MIROC-ES2L`, na.rm = TRUE),
      avg_MPI_ESM1_2_LR = mean(`MPI-ESM1-2-LR`, na.rm = TRUE),
      avg_EC_Earth3 = mean(`EC-Earth3`, na.rm = TRUE),
      avg_FGOALS_g3 = mean(`FGOALS-g3`, na.rm = TRUE),
      avg_INM_CM5_0 = mean(`INM-CM5-0`, na.rm = TRUE),
      avg_UKESM1_0_LL = mean(`UKESM1-0-LL`, na.rm = TRUE)
    ) %>%
    mutate(GCM_mean = rowMeans(across(starts_with("avg_")), na.rm = TRUE)) %>%
    dplyr::select(period, GCM_mean)
  
  
  ## Historical period 대비 변화량.
  avg_SSP245_diff <- avg_SSP245 %>%
    mutate(GCM_mean_diff = GCM_mean - GCM_mean[period == "2016-2020"])
  
  avg_SSP370_diff <- avg_SSP370 %>%
    mutate(GCM_mean_diff = GCM_mean - GCM_mean[period == "2016-2020"])
  
  avg_SSP585_diff <- avg_SSP585 %>%
    mutate(GCM_mean_diff = GCM_mean - GCM_mean[period == "2016-2020"])
  
  sido_SSP245_temp_increase[[sido]] <- avg_SSP245_diff 
  sido_SSP370_temp_increase[[sido]] <- avg_SSP370_diff
  sido_SSP585_temp_increase[[sido]] <- avg_SSP585_diff
}




################################# Historical 대비로 바꾼 버전. 2001-2020 기간 평균값 대비 변화량으로. 


load("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/기후변화 데이터/기후변화 데이터 filtered/final/sido_combined_SSP245_final.RData")
load("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/기후변화 데이터/기후변화 데이터 filtered/final/sido_combined_SSP370_final.RData")
load("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/기후변화 데이터/기후변화 데이터 filtered/final/sido_combined_SSP585_final.RData")

total_SSP245 <- sido_combined_data_SSP245_final[["whole country"]]
total_SSP370 <- sido_combined_data_SSP370_final[["whole country"]]
total_SSP585 <- sido_combined_data_SSP585_final[["whole country"]]


# ANNUAL AVERAGES (CENTERED)
avg_SSP245 <- total_SSP245 %>%
  group_by(year) %>%
  summarise(
    avg_ACCESS_CM2 = mean(`ACCESS-CM2`, na.rm = TRUE),
    avg_CanESM5 = mean(`CanESM5`, na.rm = TRUE),
    avg_CESM2 = mean(`CESM2`, na.rm = TRUE),
    avg_CMCC_ESM2 = mean(`CMCC-ESM2`, na.rm = TRUE),
    avg_CNRM_CM6_1 = mean(`CNRM-CM6-1`, na.rm = TRUE),
    avg_CNRM_ESM2_1 = mean(`CNRM-ESM2-1`, na.rm = TRUE),
    avg_INM_CM4_8 = mean(`INM-CM4-8`, na.rm = TRUE),
    avg_KACE_1_0_G = mean(`KACE-1-0-G`, na.rm = TRUE),
    avg_MIROC_ES2L = mean(`MIROC-ES2L`, na.rm = TRUE),
    avg_MPI_ESM1_2_LR = mean(`MPI-ESM1-2-LR`, na.rm = TRUE),
    avg_EC_Earth3 = mean(`EC-Earth3`, na.rm = TRUE),
    avg_FGOALS_g3 = mean(`FGOALS-g3`, na.rm = TRUE),
    avg_INM_CM5_0 = mean(`INM-CM5-0`, na.rm = TRUE),
    avg_UKESM1_0_LL = mean(`UKESM1-0-LL`, na.rm = TRUE)
  )

avg_SSP370 <- total_SSP370 %>%
  group_by(year) %>%
  summarise(
    avg_ACCESS_CM2 = mean(`ACCESS-CM2`, na.rm = TRUE),
    avg_CanESM5 = mean(`CanESM5`, na.rm = TRUE),
    avg_CESM2 = mean(`CESM2`, na.rm = TRUE),
    avg_CMCC_ESM2 = mean(`CMCC-ESM2`, na.rm = TRUE),
    avg_CNRM_CM6_1 = mean(`CNRM-CM6-1`, na.rm = TRUE),
    avg_CNRM_ESM2_1 = mean(`CNRM-ESM2-1`, na.rm = TRUE),
    avg_INM_CM4_8 = mean(`INM-CM4-8`, na.rm = TRUE),
    avg_KACE_1_0_G = mean(`KACE-1-0-G`, na.rm = TRUE),
    avg_MIROC_ES2L = mean(`MIROC-ES2L`, na.rm = TRUE),
    avg_MPI_ESM1_2_LR = mean(`MPI-ESM1-2-LR`, na.rm = TRUE),
    avg_EC_Earth3 = mean(`EC-Earth3`, na.rm = TRUE),
    avg_FGOALS_g3 = mean(`FGOALS-g3`, na.rm = TRUE),
    avg_INM_CM5_0 = mean(`INM-CM5-0`, na.rm = TRUE),
    avg_UKESM1_0_LL = mean(`UKESM1-0-LL`, na.rm = TRUE)
  )

avg_SSP585 <- total_SSP585 %>%
  group_by(year) %>%
  summarise(
    avg_ACCESS_CM2 = mean(`ACCESS-CM2`, na.rm = TRUE),
    avg_CanESM5 = mean(`CanESM5`, na.rm = TRUE),
    avg_CESM2 = mean(`CESM2`, na.rm = TRUE),
    avg_CMCC_ESM2 = mean(`CMCC-ESM2`, na.rm = TRUE),
    avg_CNRM_CM6_1 = mean(`CNRM-CM6-1`, na.rm = TRUE),
    avg_CNRM_ESM2_1 = mean(`CNRM-ESM2-1`, na.rm = TRUE),
    avg_INM_CM4_8 = mean(`INM-CM4-8`, na.rm = TRUE),
    avg_KACE_1_0_G = mean(`KACE-1-0-G`, na.rm = TRUE),
    avg_MIROC_ES2L = mean(`MIROC-ES2L`, na.rm = TRUE),
    avg_MPI_ESM1_2_LR = mean(`MPI-ESM1-2-LR`, na.rm = TRUE),
    avg_EC_Earth3 = mean(`EC-Earth3`, na.rm = TRUE),
    avg_FGOALS_g3 = mean(`FGOALS-g3`, na.rm = TRUE),
    avg_INM_CM5_0 = mean(`INM-CM5-0`, na.rm = TRUE),
    avg_UKESM1_0_LL = mean(`UKESM1-0-LL`, na.rm = TRUE)
  )


## Historical period 대비 변화량.
avg_SSP245_diff <- avg_SSP245 %>%
  mutate(
    historical_avg_ACCESS_CM2 = mean(`avg_ACCESS_CM2`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_ACCESS_CM2_diff = `avg_ACCESS_CM2` - historical_avg_ACCESS_CM2,
    
    historical_avg_CanESM5 = mean(`avg_CanESM5`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_CanESM5_diff = `avg_CanESM5` - historical_avg_CanESM5,
    
    historical_avg_CESM2 = mean(`avg_CESM2`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_CESM2_diff = `avg_CESM2` - historical_avg_CESM2,
    
    historical_avg_CMCC_ESM2 = mean(`avg_CMCC_ESM2`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_CMCC_ESM2_diff = `avg_CMCC_ESM2` - historical_avg_CMCC_ESM2,
    
    historical_avg_CNRM_CM6_1 = mean(`avg_CNRM_CM6_1`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_CNRM_CM6_1_diff = `avg_CNRM_CM6_1` - historical_avg_CNRM_CM6_1,
    
    historical_avg_CNRM_ESM2_1 = mean(`avg_CNRM_ESM2_1`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_CNRM_ESM2_1_diff = `avg_CNRM_ESM2_1` - historical_avg_CNRM_ESM2_1,
    
    historical_avg_INM_CM4_8 = mean(`avg_INM_CM4_8`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_INM_CM4_8_diff = `avg_INM_CM4_8` - historical_avg_INM_CM4_8,
    
    historical_avg_KACE_1_0_G = mean(`avg_KACE_1_0_G`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_KACE_1_0_G_diff = `avg_KACE_1_0_G` - historical_avg_KACE_1_0_G,
    
    historical_avg_MIROC_ES2L = mean(`avg_MIROC_ES2L`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_MIROC_ES2L_diff = `avg_MIROC_ES2L` - historical_avg_MIROC_ES2L,
    
    historical_avg_MPI_ESM1_2_LR = mean(`avg_MPI_ESM1_2_LR`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_MPI_ESM1_2_LR_diff = `avg_MPI_ESM1_2_LR` - historical_avg_MPI_ESM1_2_LR,
    
    historical_avg_EC_Earth3 = mean(`avg_EC_Earth3`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_EC_Earth3_diff = `avg_EC_Earth3` - historical_avg_EC_Earth3,
    
    historical_avg_FGOALS_g3 = mean(`avg_FGOALS_g3`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_FGOALS_g3_diff = `avg_FGOALS_g3` - historical_avg_FGOALS_g3,
    
    historical_avg_INM_CM5_0 = mean(`avg_INM_CM5_0`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_INM_CM5_0_diff = `avg_INM_CM5_0` - historical_avg_INM_CM5_0,
    
    historical_avg_UKESM1_0_LL = mean(`avg_UKESM1_0_LL`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_UKESM1_0_LL_diff = `avg_UKESM1_0_LL` - historical_avg_UKESM1_0_LL
  ) %>%
  dplyr::select(-starts_with("historical_avg"))


avg_SSP370_diff <- avg_SSP370 %>%
  mutate(
    historical_avg_ACCESS_CM2 = mean(`avg_ACCESS_CM2`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_ACCESS_CM2_diff = `avg_ACCESS_CM2` - historical_avg_ACCESS_CM2,
    
    historical_avg_CanESM5 = mean(`avg_CanESM5`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_CanESM5_diff = `avg_CanESM5` - historical_avg_CanESM5,
    
    historical_avg_CESM2 = mean(`avg_CESM2`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_CESM2_diff = `avg_CESM2` - historical_avg_CESM2,
    
    historical_avg_CMCC_ESM2 = mean(`avg_CMCC_ESM2`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_CMCC_ESM2_diff = `avg_CMCC_ESM2` - historical_avg_CMCC_ESM2,
    
    historical_avg_CNRM_CM6_1 = mean(`avg_CNRM_CM6_1`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_CNRM_CM6_1_diff = `avg_CNRM_CM6_1` - historical_avg_CNRM_CM6_1,
    
    historical_avg_CNRM_ESM2_1 = mean(`avg_CNRM_ESM2_1`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_CNRM_ESM2_1_diff = `avg_CNRM_ESM2_1` - historical_avg_CNRM_ESM2_1,
    
    historical_avg_INM_CM4_8 = mean(`avg_INM_CM4_8`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_INM_CM4_8_diff = `avg_INM_CM4_8` - historical_avg_INM_CM4_8,
    
    historical_avg_KACE_1_0_G = mean(`avg_KACE_1_0_G`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_KACE_1_0_G_diff = `avg_KACE_1_0_G` - historical_avg_KACE_1_0_G,
    
    historical_avg_MIROC_ES2L = mean(`avg_MIROC_ES2L`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_MIROC_ES2L_diff = `avg_MIROC_ES2L` - historical_avg_MIROC_ES2L,
    
    historical_avg_MPI_ESM1_2_LR = mean(`avg_MPI_ESM1_2_LR`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_MPI_ESM1_2_LR_diff = `avg_MPI_ESM1_2_LR` - historical_avg_MPI_ESM1_2_LR,
    
    historical_avg_EC_Earth3 = mean(`avg_EC_Earth3`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_EC_Earth3_diff = `avg_EC_Earth3` - historical_avg_EC_Earth3,
    
    historical_avg_FGOALS_g3 = mean(`avg_FGOALS_g3`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_FGOALS_g3_diff = `avg_FGOALS_g3` - historical_avg_FGOALS_g3,
    
    historical_avg_INM_CM5_0 = mean(`avg_INM_CM5_0`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_INM_CM5_0_diff = `avg_INM_CM5_0` - historical_avg_INM_CM5_0,
    
    historical_avg_UKESM1_0_LL = mean(`avg_UKESM1_0_LL`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_UKESM1_0_LL_diff = `avg_UKESM1_0_LL` - historical_avg_UKESM1_0_LL
  ) %>%
  dplyr::select(-starts_with("historical_avg"))


avg_SSP585_diff <- avg_SSP585 %>%
  mutate(
    historical_avg_ACCESS_CM2 = mean(`avg_ACCESS_CM2`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_ACCESS_CM2_diff = `avg_ACCESS_CM2` - historical_avg_ACCESS_CM2,
    
    historical_avg_CanESM5 = mean(`avg_CanESM5`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_CanESM5_diff = `avg_CanESM5` - historical_avg_CanESM5,
    
    historical_avg_CESM2 = mean(`avg_CESM2`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_CESM2_diff = `avg_CESM2` - historical_avg_CESM2,
    
    historical_avg_CMCC_ESM2 = mean(`avg_CMCC_ESM2`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_CMCC_ESM2_diff = `avg_CMCC_ESM2` - historical_avg_CMCC_ESM2,
    
    historical_avg_CNRM_CM6_1 = mean(`avg_CNRM_CM6_1`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_CNRM_CM6_1_diff = `avg_CNRM_CM6_1` - historical_avg_CNRM_CM6_1,
    
    historical_avg_CNRM_ESM2_1 = mean(`avg_CNRM_ESM2_1`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_CNRM_ESM2_1_diff = `avg_CNRM_ESM2_1` - historical_avg_CNRM_ESM2_1,
    
    historical_avg_INM_CM4_8 = mean(`avg_INM_CM4_8`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_INM_CM4_8_diff = `avg_INM_CM4_8` - historical_avg_INM_CM4_8,
    
    historical_avg_KACE_1_0_G = mean(`avg_KACE_1_0_G`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_KACE_1_0_G_diff = `avg_KACE_1_0_G` - historical_avg_KACE_1_0_G,
    
    historical_avg_MIROC_ES2L = mean(`avg_MIROC_ES2L`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_MIROC_ES2L_diff = `avg_MIROC_ES2L` - historical_avg_MIROC_ES2L,
    
    historical_avg_MPI_ESM1_2_LR = mean(`avg_MPI_ESM1_2_LR`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_MPI_ESM1_2_LR_diff = `avg_MPI_ESM1_2_LR` - historical_avg_MPI_ESM1_2_LR,
    
    historical_avg_EC_Earth3 = mean(`avg_EC_Earth3`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_EC_Earth3_diff = `avg_EC_Earth3` - historical_avg_EC_Earth3,
    
    historical_avg_FGOALS_g3 = mean(`avg_FGOALS_g3`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_FGOALS_g3_diff = `avg_FGOALS_g3` - historical_avg_FGOALS_g3,
    
    historical_avg_INM_CM5_0 = mean(`avg_INM_CM5_0`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_INM_CM5_0_diff = `avg_INM_CM5_0` - historical_avg_INM_CM5_0,
    
    historical_avg_UKESM1_0_LL = mean(`avg_UKESM1_0_LL`[year >= 2001 & year <= 2020], na.rm = TRUE),
    avg_UKESM1_0_LL_diff = `avg_UKESM1_0_LL` - historical_avg_UKESM1_0_LL
  ) %>%
  dplyr::select(-starts_with("historical_avg"))


############## Temperature 추이 그래프.
ylab <- expression(paste("Temperature increase (",degree,"C)"))

# col1을 투명도 조절 가능한 형태로 변환
col1_rgb <- col2rgb("#0080FF") / 255  # 0~255 범위를 0~1 범위로 변환
# 투명도 설정 (예: alpha = 0.5)
col1_with_alpha <- rgb(col1_rgb[1], col1_rgb[2], col1_rgb[3], alpha = 0.1)
col2_rgb <- col2rgb("#FF9933") / 255
col2_with_alpha <- rgb(col2_rgb[1], col2_rgb[2], col2_rgb[3], alpha = 0.1)
col3_rgb <- col2rgb("#FF0000") / 255
col3_with_alpha <- rgb(col3_rgb[1], col3_rgb[2], col3_rgb[3], alpha = 0.1)

avg_SSP245_diff <- avg_SSP245_diff %>% 
  filter(year < 2051)
avg_SSP370_diff <- avg_SSP370_diff %>% 
  filter(year < 2051)
avg_SSP585_diff <- avg_SSP585_diff %>% 
  filter(year < 2051)



par(mar=c(5,4,3,1),mgp=c(2,0.8,0),las=1,cex.axis=0.8,cex.lab=1)
plot(avg_SSP585_diff$year,rowMeans(avg_SSP585_diff[ , !names(avg_SSP585_diff) %in% "year"], na.rm = TRUE),
     type="n",bty="l",ylab=ylab,xlab="Year", ylim=c(-1,4),xlim=c(1999,2055),main="")
abline(h=0)
lines(as.numeric(avg_SSP245_diff$year[avg_SSP245_diff$year < 2021]),  # year를 명확히 숫자로 처리
      rowMeans(avg_SSP245_diff[avg_SSP245_diff$year < 2021, 
                               grepl("_diff$", names(avg_SSP245_diff))], na.rm = TRUE),
      lwd=2)

#### 지금은 GCMs가 하나여서, max, min 값이 같으므로 polygon 생성 안됨.
polygon(
  c(as.numeric(avg_SSP245_diff$year[avg_SSP245_diff$year >= 2020]), 
    rev(as.numeric(avg_SSP245_diff$year[avg_SSP245_diff$year >= 2020]))),  # x축 좌표 (year 범위)
  
  c(apply(avg_SSP245_diff[avg_SSP245_diff$year >= 2020, grepl("_diff$", names(avg_SSP245_diff))], 1, max), 
    rev(apply(avg_SSP245_diff[avg_SSP245_diff$year >= 2020, grepl("_diff$", names(avg_SSP245_diff))], 1, min))),  # y축 좌표 (최댓값과 최솟값)
  
  col = col1_with_alpha,  # 색상 설정
  border = NA  # 경계선을 나타내지 않도록 설정
)

polygon(
  c(as.numeric(avg_SSP370_diff$year[avg_SSP370_diff$year >= 2020]), 
    rev(as.numeric(avg_SSP370_diff$year[avg_SSP370_diff$year >= 2020]))),  # x축 좌표 (year 범위)
  
  c(apply(avg_SSP370_diff[avg_SSP370_diff$year >= 2020, grepl("_diff$", names(avg_SSP370_diff))], 1, max), 
    rev(apply(avg_SSP370_diff[avg_SSP370_diff$year >= 2020, grepl("_diff$", names(avg_SSP370_diff))], 1, min))),  # y축 좌표 (최댓값과 최솟값)
  
  col = col2_with_alpha,  # 색상 설정
  border = NA  # 경계선을 나타내지 않도록 설정
)

polygon(
  c(as.numeric(avg_SSP585_diff$year[avg_SSP585_diff$year >= 2020]), 
    rev(as.numeric(avg_SSP585_diff$year[avg_SSP585_diff$year >= 2020]))),  # x축 좌표 (year 범위)
  
  c(apply(avg_SSP585_diff[avg_SSP585_diff$year >= 2020, grepl("_diff$", names(avg_SSP585_diff))], 1, max), 
    rev(apply(avg_SSP585_diff[avg_SSP585_diff$year >= 2020, grepl("_diff$", names(avg_SSP585_diff))], 1, min))),  # y축 좌표 (최댓값과 최솟값)
  
  col = col3_with_alpha,  # 색상 설정
  border = NA  # 경계선을 나타내지 않도록 설정
)



# lines() 함수 내에서 year 변수를 명확히 숫자로 변환하여 사용
lines(as.numeric(avg_SSP245_diff$year[avg_SSP245_diff$year >= 2020]),  # year 변수를 명확히 숫자로 변환
      rowMeans(avg_SSP245_diff[avg_SSP245_diff$year >= 2020, 
                               grepl("_diff$", names(avg_SSP245_diff))], na.rm = TRUE),
      lwd = 2, col = "#0080FF")
lines(as.numeric(avg_SSP370_diff$year[avg_SSP370_diff$year >= 2020]),  # year 변수를 명확히 숫자로 변환
      rowMeans(avg_SSP370_diff[avg_SSP370_diff$year >= 2020, 
                               grepl("_diff$", names(avg_SSP370_diff))], na.rm = TRUE),
      lwd = 2, col = "#FF9933")
lines(as.numeric(avg_SSP585_diff$year[avg_SSP585_diff$year >= 2020]),  # year 변수를 명확히 숫자로 변환
      rowMeans(avg_SSP585_diff[avg_SSP585_diff$year >= 2020, 
                               grepl("_diff$", names(avg_SSP585_diff))], na.rm = TRUE),
      lwd = 2, col = "#FF0000")


## 2090년부터 2099년까지의 각 행에 대해 최소값을 계산한 후, 그 값들의 평균을 계산하여 y축 최대 최소값으로 사용
rect(
  2052 - 1,  # 시작하는 x 좌표
  mean(apply(avg_SSP245_diff[as.numeric(avg_SSP245_diff$year) %in% 2045:2050, grepl("_diff$", names(avg_SSP245_diff))], 1, min)),  # y축 최소값
  2052 + 0.5,  # 끝나는 x 좌표
  mean(apply(avg_SSP245_diff[as.numeric(avg_SSP245_diff$year) %in% 2045:2050, grepl("_diff$", names(avg_SSP245_diff))], 1, max)),  # y축 최대값
  col = rgb(0, 0.5, 1, alpha = 0.8),   
  border = NA  # 경계선 없음
)

rect(
  2052 + 1.0,  # 시작하는 x 좌표
  mean(apply(avg_SSP370_diff[as.numeric(avg_SSP370_diff$year) %in% 2045:2050, grepl("_diff$", names(avg_SSP370_diff))], 1, min)),  # y축 최소값
  2052 + 2.5,  # 끝나는 x 좌표
  mean(apply(avg_SSP370_diff[as.numeric(avg_SSP370_diff$year) %in% 2045:2050, grepl("_diff$", names(avg_SSP370_diff))], 1, max)),  # y축 최대값
  col = rgb(1, 0.6, 0.2, alpha = 0.8), 
  border = NA  # 경계선 없음
)

rect(
  2052 + 3.0,  # 시작하는 x 좌표
  mean(apply(avg_SSP585_diff[as.numeric(avg_SSP585_diff$year) %in% 2045:2050, grepl("_diff$", names(avg_SSP585_diff))], 1, min)),  # y축 최소값
  2052 + 4.5,  # 끝나는 x 좌표
  mean(apply(avg_SSP585_diff[as.numeric(avg_SSP585_diff$year) %in% 2045:2050, grepl("_diff$", names(avg_SSP585_diff))], 1, max)),  # y축 최대값
  col = rgb(1, 0, 0, alpha = 0.8), 
  border = NA  # 경계선 없음
)

legend("top",c("Historical","SSP585", "SSP370", "SSP245"),lwd=2,col=c("black", "#FF0000", "#FF9933","#0080FF"),
       cex=0.8,bty="n",inset=0.05)

title(main = "Republic of Korea")
mtext("2001-2100", side = 3, line = 0.1)




########### Temperature의 projection map 만들기. 
## Baseline: "2016-2020" 
## Projection: 2026~2030, 2036~2040, 2046~2050 


korea_sido <- read_sf("C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\korea_map/simplified.shp") %>% 
  st_transform(4326)

korea_sido$SIDO_CD


# SIDO_CD 열 이름을 sido_b로 변경하고, 값 변환
korea_sido <- korea_sido %>%
  dplyr::select(-BASE_DATE, -SIDO_NM) %>% 
  rename(sido_b = SIDO_CD) %>%  # SIDO_CD를 sido_b로 이름 변경
  mutate(sido_b = case_when(
    sido_b == "11" ~ "11",  # "11" 변환 추가
    sido_b == "21" ~ "26",
    sido_b == "22" ~ "27",
    sido_b == "23" ~ "28",
    sido_b == "24" ~ "29",
    sido_b == "25" ~ "30",
    sido_b == "26" ~ "31",
    sido_b == "31" ~ "41",
    sido_b == "32" ~ "42",
    sido_b %in% c("29", "33") ~ "43",  # "29"와 "33"을 모두 "43"으로 변환
    sido_b == "34" ~ "44",
    sido_b == "35" ~ "45",
    sido_b == "36" ~ "46",
    sido_b == "37" ~ "47",
    sido_b == "38" ~ "48",
    sido_b == "39" ~ "50",
    TRUE ~ sido_b  # 그 외 값은 그대로 유지
  ))

korea_sido$sido_b

plot(korea_sido)

##################### map에 values 연결.
### baseline period에 대해서 평균 기온값 할당하는 열.
## 2026~2030, 2036~2040, 2046~2050  이 기간들의 difference 할당하는 열들.
## 
sido_SSP245_temp_increase <- sido_SSP245_temp_increase[!names(sido_SSP245_temp_increase) %in% "whole country"]
sido_SSP370_temp_increase <- sido_SSP370_temp_increase[!names(sido_SSP370_temp_increase) %in% "whole country"]
sido_SSP585_temp_increase <- sido_SSP585_temp_increase[!names(sido_SSP585_temp_increase) %in% "whole country"]

# 빈 데이터프레임 초기화
sido_SSP245_df <- data.frame()
sido_SSP370_df <- data.frame()
sido_SSP585_df <- data.frame()

# 리스트의 각 요소에 대해 반복 작업 수행
for (sido_name in names(sido_SSP245_temp_increase)) {
  
  # 해당 시도에 대한 데이터프레임 불러오기
  df <- sido_SSP585_temp_increase[[sido_name]]
  
  # 각 필요한 값을 추출
  GCM_mean_2016 <- df %>% 
    filter(period == "2016-2020") %>%
    pull(GCM_mean)
  
  diff_2031 <- df %>% 
    filter(period == "2031-2035") %>%
    pull(GCM_mean_diff)
  
  diff_2041 <- df %>% 
    filter(period == "2041-2045") %>%
    pull(GCM_mean_diff)
  
  diff_2046 <- df %>% 
    filter(period == "2046-2050") %>%
    pull(GCM_mean_diff)
  
  # 각 값을 하나의 데이터프레임으로 정리
  temp_df <- data.frame(
    sido_b = sido_name,             # 시도 이름
    GCM_mean = GCM_mean_2016,       
    diff_2031 = diff_2031,          
    diff_2041 = diff_2041,          
    diff_2046 = diff_2046           
  )
  
  # 결과 데이터프레임에 추가
  sido_SSP585_df <- bind_rows(sido_SSP585_df, temp_df)
}

# 중복된 행 제거
sido_SSP245_df <- sido_SSP245_df %>% distinct()
sido_SSP370_df <- sido_SSP370_df %>% distinct()
sido_SSP585_df <- sido_SSP585_df %>% distinct()

############################################
korea_sido_SSP245 <- korea_sido %>% 
  left_join(sido_SSP245_df, by = "sido_b")
korea_sido_SSP370 <- korea_sido %>% 
  left_join(sido_SSP370_df, by = "sido_b")
korea_sido_SSP585 <- korea_sido %>% 
  left_join(sido_SSP585_df, by = "sido_b")


# GCM_mean을 기준으로 색상 그라디언트 적용하여 지도 그리기
ggplot(korea_sido_SSP245) +
  geom_sf(aes(fill = GCM_mean)) +  # GCM_mean을 기준으로 색상 채우기
  scale_fill_gradient(low = "#FFFF99", high = "#CC0000",  # 색상의 최소값과 최대값 설정
                      name = "GCM Mean",                    # 색상 범례 제목 설정
                      limits = c(min(korea_sido_SSP245$GCM_mean, na.rm = TRUE), 
                                 max(korea_sido_SSP245$GCM_mean, na.rm = TRUE))) + 
  theme_minimal() +  # 미니멀한 테마 적용
  labs(title = "GCM Mean by Region (SSP245 Scenario)",   # 제목
       fill = "GCM Mean")  # 범례 제목

range(korea_sido_SSP245$diff_2031)

range(korea_sido_SSP585$diff_2046)

ggplot(korea_sido_SSP585) +
  geom_sf(aes(fill = diff_2046)) +  # diff_2040을 기준으로 색상 채우기
  scale_fill_gradient(low = "#FFFF99", high = "#CC0000",  # 색상의 최소값과 최대값 설정
                      name = "GCM Mean",                 # 색상 범례 제목 설정
                      limits = c(0.0, 3.0)) +          # 임의의 최소값과 최대값 설정
  theme_minimal() +  # 미니멀한 테마 적용
  labs(fill = "Temperature increase")  # 범례 제목





###################################################################
#############################Figure 3, 4 AN map
########### 
########### 각 지역별로 기간별 값 할당하기.
## Basline: "2016-2020" AN 절대값 시각화
## Projection: "2031-2035" "2041-2045" "2046-2050" 상승률 시각화.
#####################################

str(anrel_list)

# anabs_list에서 지역코드별 데이터를 추출하여 새로운 리스트 생성
extract_SSP245_tot <- function(anabs_list) {
  # 결과를 저장할 리스트 초기화
  AN_SSP245 <- list()
  
  # anabs_list의 각 지역 코드에 대해 반복
  for (region_code in names(anabs_list)) {
    # 지역 코드에 해당하는 배열 추출
    anabs_region <- anabs_list[[region_code]]
    
    # , , "tot", "SSP245"에 해당하는 데이터 추출
    extracted_data <- anabs_region[, , "tot", "SSP245"]
    
    # 결과를 리스트에 저장
    AN_SSP245[[region_code]] <- extracted_data
  }
  
  return(AN_SSP245)
}

extract_SSP370_tot <- function(anabs_list) {
  # 결과를 저장할 리스트 초기화
  AN_SSP370 <- list()
  
  # anabs_list의 각 지역 코드에 대해 반복
  for (region_code in names(anabs_list)) {
    # 지역 코드에 해당하는 배열 추출
    anabs_region <- anabs_list[[region_code]]
    
    # , , "tot", "SSP370"에 해당하는 데이터 추출
    extracted_data <- anabs_region[, , "tot", "SSP370"]
    
    # 결과를 리스트에 저장
    AN_SSP370[[region_code]] <- extracted_data
  }
  
  return(AN_SSP370)
}

extract_SSP585_tot <- function(anabs_list) {
  # 결과를 저장할 리스트 초기화
  AN_SSP585 <- list()
  
  # anabs_list의 각 지역 코드에 대해 반복
  for (region_code in names(anabs_list)) {
    # 지역 코드에 해당하는 배열 추출
    anabs_region <- anabs_list[[region_code]]
    
    # , , "tot", "SSP585"에 해당하는 데이터 추출
    extracted_data <- anabs_region[, , "tot", "SSP585"]
    
    # 결과를 리스트에 저장
    AN_SSP585[[region_code]] <- extracted_data
  }
  
  return(AN_SSP585)
}

# 함수 실행
AN_SSP245 <- extract_SSP245_tot(anabs_list)
AN_SSP370 <- extract_SSP370_tot(anabs_list)
AN_SSP585 <- extract_SSP585_tot(anabs_list)

AN_SSP245_filtered <- AN_SSP245[setdiff(names(AN_SSP245), "whole country")]
AN_SSP370_filtered <- AN_SSP370[setdiff(names(AN_SSP370), "whole country")]
AN_SSP585_filtered <- AN_SSP585[setdiff(names(AN_SSP585), "whole country")]

sido_SSP245_AN <- data.frame(
  sido_b = character(),  # 시도 이름
  AN_2016 = numeric(),   # 2016–2020 baseline
  AN_2031 = numeric(),   # 2031-2035 데이터
  AN_2041 = numeric(),   # 2041-2045 데이터
  AN_2046 = numeric(),   # 2046-2050 데이터
  stringsAsFactors = FALSE
)

sido_SSP370_AN <- data.frame(
  sido_b = character(),  # 시도 이름
  AN_2016 = numeric(),   # 2016–2020 baseline
  AN_2031 = numeric(),   # 2031-2035 데이터
  AN_2041 = numeric(),   # 2041-2045 데이터
  AN_2046 = numeric(),   # 2046-2050 데이터
  stringsAsFactors = FALSE
)

sido_SSP585_AN <- data.frame(
  sido_b = character(),  # 시도 이름
  AN_2016 = numeric(),   # 2016–2020 baseline
  AN_2031 = numeric(),   # 2031-2035 데이터
  AN_2041 = numeric(),   # 2041-2045 데이터
  AN_2046 = numeric(),   # 2046-2050 데이터
  stringsAsFactors = FALSE
)


for (sido_name in names(AN_SSP245_filtered)) {
  
  # 해당 시도에 대한 데이터프레임 불러오기
  df <- as.data.frame(AN_SSP245_filtered[[sido_name]])  
  df <- rownames_to_column(df, var = "period")
  colnames(df) <- c("period", "AN", "ci_l", "ci_u")  
  
  # 각 필요한 값을 추출
  AN_2016 <- df %>% 
    filter(period == "2016-2020") %>%
    pull(AN)
  
  AN_2031 <- df %>% 
    filter(period == "2031-2035") %>%
    pull(AN)
  
  AN_2041 <- df %>% 
    filter(period == "2041-2045") %>%
    pull(AN)
  
  AN_2046 <- df %>% 
    filter(period == "2046-2050") %>%
    pull(AN)
  
  # 각 값을 하나의 데이터프레임으로 정리
  temp_df <- data.frame(
    sido_b = sido_name,             # 시도 이름
    AN_2016 = AN_2016,      
    AN_2031 = AN_2031,      
    AN_2041 = AN_2041,          
    AN_2046 = AN_2046          
  )
  # 결과 데이터프레임에 추가
  sido_SSP245_AN <- bind_rows(sido_SSP245_AN, temp_df)
}


for (sido_name in names(AN_SSP370_filtered)) {
  
  # 해당 시도에 대한 데이터프레임 불러오기
  df <- as.data.frame(AN_SSP370_filtered[[sido_name]])  
  df <- rownames_to_column(df, var = "period")
  colnames(df) <- c("period", "AN", "ci_l", "ci_u")  
  
  # 각 필요한 값을 추출
  AN_2016 <- df %>% 
    filter(period == "2016-2020") %>%
    pull(AN)
  
  AN_2031 <- df %>% 
    filter(period == "2031-2035") %>%
    pull(AN)
  
  AN_2041 <- df %>% 
    filter(period == "2041-2045") %>%
    pull(AN)
  
  AN_2046 <- df %>% 
    filter(period == "2046-2050") %>%
    pull(AN)
  
  # 각 값을 하나의 데이터프레임으로 정리
  temp_df <- data.frame(
    sido_b = sido_name,             # 시도 이름
    AN_2016 = AN_2016,      
    AN_2031 = AN_2031,      
    AN_2041 = AN_2041,          
    AN_2046 = AN_2046          
  )
  # 결과 데이터프레임에 추가
  sido_SSP370_AN <- bind_rows(sido_SSP370_AN, temp_df)
}


for (sido_name in names(AN_SSP585_filtered)) {

  # 해당 시도에 대한 데이터프레임 불러오기
  df <- as.data.frame(AN_SSP585_filtered[[sido_name]])  
  df <- rownames_to_column(df, var = "period")
  colnames(df) <- c("period", "AN", "ci_l", "ci_u")  
  
  # 각 필요한 값을 추출
  AN_2016 <- df %>% 
    filter(period == "2016-2020") %>%
    pull(AN)
  
  AN_2031 <- df %>% 
    filter(period == "2031-2035") %>%
    pull(AN)
  
  AN_2041 <- df %>% 
    filter(period == "2041-2045") %>%
    pull(AN)
  
  AN_2046 <- df %>% 
    filter(period == "2046-2050") %>%
    pull(AN)
  
  # 각 값을 하나의 데이터프레임으로 정리
  temp_df <- data.frame(
    sido_b = sido_name,             # 시도 이름
    AN_2016 = AN_2016,      
    AN_2031 = AN_2031,      
    AN_2041 = AN_2041,          
    AN_2046 = AN_2046          
  )
  # 결과 데이터프레임에 추가
  sido_SSP585_AN <- bind_rows(sido_SSP585_AN, temp_df)
}




sido_SSP585_AN



# Retain the original model-based baseline and projection estimates for each scenario.
# No cross-scenario averaging or region-specific post-hoc adjustments are applied.


#################### 절대값

sido_SSP245_AN 

sido_SSP370_AN 

sido_SSP585_AN 


#################### 차이값

sido_SSP245_AN_diff <- sido_SSP245_AN %>%
  mutate(
    diff_2031 = (AN_2031 - AN_2016) ,  
    diff_2041 = (AN_2041 - AN_2016) ,  
    diff_2046 = (AN_2046 - AN_2016)    
  )

sido_SSP370_AN_diff <- sido_SSP370_AN %>%
  mutate(
    diff_2031 = (AN_2031 - AN_2016),  
    diff_2041 = (AN_2041 - AN_2016) ,  
    diff_2046 = (AN_2046 - AN_2016)    
  )

sido_SSP585_AN_diff <- sido_SSP585_AN %>%
  mutate(
    diff_2031 = (AN_2031 - AN_2016) ,  
    diff_2041 = (AN_2041 - AN_2016) ,  
    diff_2046 = (AN_2046 - AN_2016)    
  )


#################### percent change
make_diff_cats <- function(x) {
  case_when(
    x <= 0              ~ "<0%",
    x > 0  & x < 5      ~ "<5%",
    x >= 5  & x < 10    ~ "<10%",
    x >= 10 & x < 15    ~ "<15%",
    x >= 15 & x < 20    ~ "<20%",
    x >= 20             ~ ">20%"
  ) |>
    factor(levels = c("<0%", "<5%", "<10%", "<15%", "<20%", ">20%"))
}

# 공통 처리 함수: diff 계산 + 카테고리 생성
add_diff_and_cats <- function(df) {
  df %>%
    mutate(
      diff_2031 = if_else(is.na(AN_2016) | AN_2016 == 0, NA_real_, ((AN_2031 - AN_2016) / AN_2016) * 100),
      diff_2041 = if_else(is.na(AN_2016) | AN_2016 == 0, NA_real_, ((AN_2041 - AN_2016) / AN_2016) * 100),
      diff_2046 = if_else(is.na(AN_2016) | AN_2016 == 0, NA_real_, ((AN_2046 - AN_2016) / AN_2016) * 100),
      
      diff_2031_cat = make_diff_cats(diff_2031),
      diff_2041_cat = make_diff_cats(diff_2041),
      diff_2046_cat = make_diff_cats(diff_2046)
    )
}

# 시나리오별로 적용
sido_SSP245_AN_rate <- add_diff_and_cats(sido_SSP245_AN)
sido_SSP370_AN_rate <- add_diff_and_cats(sido_SSP370_AN)
sido_SSP585_AN_rate <- add_diff_and_cats(sido_SSP585_AN)

write.csv(sido_SSP245_AN, "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\AR_projection/sido_SSP245_AN.csv")
write.csv(sido_SSP370_AN, "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\AR_projection/sido_SSP370_AN.csv")
write.csv(sido_SSP585_AN, "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\AR_projection/sido_SSP585_AN.csv")



## growth rate는 아마 quantile이나 이렇게 구분해야할듯. categorical로 안 하면 차이가 너무 커서 안되고.
## 정확한 수치는 Appendix Table로. 
korea_sido_SSP245_AN <- korea_sido %>% 
  left_join(sido_SSP245_AN, by = "sido_b")
korea_sido_SSP370_AN <- korea_sido %>% 
  left_join(sido_SSP370_AN, by = "sido_b")
korea_sido_SSP585_AN <- korea_sido %>% 
  left_join(sido_SSP585_AN, by = "sido_b")

korea_sido_SSP245_AN_diff <- korea_sido %>% 
  left_join(sido_SSP245_AN_diff, by = "sido_b")
korea_sido_SSP370_AN_diff <- korea_sido %>% 
  left_join(sido_SSP370_AN_diff, by = "sido_b")
korea_sido_SSP585_AN_diff <- korea_sido %>% 
  left_join(sido_SSP585_AN_diff, by = "sido_b")

korea_sido_SSP245_AN_rate <- korea_sido %>% 
  left_join(sido_SSP245_AN_rate, by = "sido_b")
korea_sido_SSP370_AN_rate <- korea_sido %>% 
  left_join(sido_SSP370_AN_rate, by = "sido_b")
korea_sido_SSP585_AN_rate <- korea_sido %>% 
  left_join(sido_SSP585_AN_rate, by = "sido_b")

##################
range(log((korea_sido_SSP245_AN$diff_2031)))
range(korea_sido_SSP370_AN$diff_2046)

ggplot(korea_sido_SSP245_AN) +
  geom_sf(aes(fill = AN_2016)) +  # GCM_mean을 기준으로 색상 채우기
  scale_fill_gradient(low = "#F8F8FF", high = "#FF0000",  # 색상의 최소값과 최대값 설정
                      name = "GCM Mean",                    # 색상 범례 제목 설정
                      limits = c(0,3900)) + 
  theme_minimal() +  # 미니멀한 테마 적용
  labs(  fill = "GCM Mean")  # 범례 제목

ggplot(korea_sido_SSP585_AN_diff) +
  geom_sf(aes(fill = diff_2046)) +  # GCM_mean을 기준으로 색상 채우기
  scale_fill_gradient(low = "#F8F8FF", high = "#FF0000",  # 색상의 최소값과 최대값 설정
                      name = "GCM Mean",                    # 색상 범례 제목 설정
                      limits = c(-80,700)) + 
  theme_minimal() +  # 미니멀한 테마 적용
  labs( fill = "GCM Mean")  # 범례 제목

range(sido_SSP245_AN$AN_2010)
korea_sido_SSP245_AN


# 카테고리별 색 벡터 (낮은 값 = 밝은 색, 높은 값 = 진한 빨강)
col_vec <- c(
  "<0%"  = "#F8F8FF",  # 아주 옅은 색
  "<5%"  = "#FFECEC",  # 아주 연한 코랄 (핑크+웜)
  "<10%" = "#FFC9C2",  # 연한 코랄/살몬
  "<15%" = "#FF9F8A",  # 부드러운 살몬
  "<20%" = "#FF6F61",  # 코랄 레드 (붉은기+주황기 중간)
  ">20%" = "#CC3333"   # 진한 빨강
)

ggplot(korea_sido_SSP245_AN_rate) +
  geom_sf(aes(fill = diff_2046_cat), size = 0.2) +  # 경계선 색/두께는 취향대로
  scale_fill_manual(
    name   = "Percent change\n(2046–2050 vs 2016–2020)",
    values = col_vec,
    drop   = FALSE  # 데이터에 없는 구간도 범례에 유지하고 싶으면 TRUE로
  ) +
  coord_sf() +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title    = element_text(size = 10),
    legend.text     = element_text(size = 9)
  )





#################################### Supplementary table 생성용 코드 1.

AN_SSP245[["whole country"]]

AN_SSP245_df <- as.data.frame(AN_SSP245[["whole country"]]) %>%
  mutate(
    period = rownames(.),                     # 행 이름을 period 열로 이동
    excess_cases = est - est[period == "2016-2020"],             # 2016–2020년 추정치를 기준으로 변화량 계산
    growth_rate = (excess_cases / est[period == "2016-2020"]) * 100 # 2016–2020년 추정치를 분모로 사용
  ) %>%
  select(period, est, ci.l, ci.u, excess_cases, growth_rate) %>% # 열 순서 지정
  rename(
    period = period, 
    est = est, 
    ci_l = ci.l, 
    ci_u = ci.u, 
    excess_cases = excess_cases, 
    growth_rate = growth_rate
  )

AN_SSP370_df <- as.data.frame(AN_SSP370[["whole country"]]) %>%
  mutate(
    period = rownames(.),                     # 행 이름을 period 열로 이동
    excess_cases = est - est[period == "2016-2020"],             # 2016–2020년 추정치를 기준으로 변화량 계산
    growth_rate = (excess_cases / est[period == "2016-2020"]) * 100 # 2016–2020년 추정치를 분모로 사용
  ) %>%
  select(period, est, ci.l, ci.u, excess_cases, growth_rate) %>% # 열 순서 지정
  rename(
    period = period, 
    est = est, 
    ci_l = ci.l, 
    ci_u = ci.u, 
    excess_cases = excess_cases, 
    growth_rate = growth_rate
  )

AN_SSP585_df <- as.data.frame(AN_SSP585[["whole country"]]) %>%
  mutate(
    period = rownames(.),                     # 행 이름을 period 열로 이동
    excess_cases = est - est[period == "2016-2020"],             # 2016–2020년 추정치를 기준으로 변화량 계산
    growth_rate = (excess_cases / est[period == "2016-2020"]) * 100 # 2016–2020년 추정치를 분모로 사용
  ) %>%
  select(period, est, ci.l, ci.u, excess_cases, growth_rate) %>% # 열 순서 지정
  rename(
    period = period, 
    est = est, 
    ci_l = ci.l, 
    ci_u = ci.u, 
    excess_cases = excess_cases, 
    growth_rate = growth_rate
  )

write.csv(AN_SSP245_df, "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\AR_projection/AN_SSP245_df.csv")
write.csv(AN_SSP370_df, "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\AR_projection/AN_SSP370_df.csv")
write.csv(AN_SSP585_df, "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\AR_projection/AN_SSP585_df.csv")







################################# Figure 6. 
########### temperature range 에 따른 contribution. 
########### 일단 각 시나리오 별로 시계열 그래프를 기온 range 에 따라서 분리해서 그려넣고.
########### 그리고 점선이나 그런걸로 total 도 추가.

########### SSP585 2046-2050 에 대해서는 map 을 그려서 보여주기. total, contribution 별 map 이렇게 총 네개. 
########### map 은 이번에는 변화량으로 절대값으로? 



load(file = "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\AR_projection/AR_wholecountry_5000.RData")
load(file = "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\AR_projection/최종/AR_under10.RData")
load(file = "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\AR_projection/최종/AR_over10.RData")
load(file = "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\AR_projection/최종/AR_over28.RData")

anrel
anrel_over10
anrel_over28
anrel_under10

anabs_over10
anabs_over28
anabs_under10

# 함수: 배열을 데이터프레임으로 변환하고 period 및 source 열 추가
extract_df <- function(arr, scenario, source_name) {
  df <- as.data.frame(arr[, , "tot", scenario])
  df$period <- rownames(df)
  df$source <- source_name
  return(df)
}

# 시나리오 목록
scenarios <- c("SSP245", "SSP370", "SSP585")

# 배열 목록 및 이름
array_list <- list(anabs = anabs, anabs_under10 = anabs_under10, anabs_over10 = anabs_over10, anabs_over28 = anabs_over28)

# 결과 저장용 리스트
result_list <- list()

# 각 시나리오에 대해 처리
for (scen in scenarios) {
  temp_list <- lapply(names(array_list), function(name) {
    extract_df(array_list[[name]], scen, name)
  })
  result_list[[scen]] <- bind_rows(temp_list)
}

# 결과: 시나리오별 데이터프레임
SSP245_df <- result_list[["SSP245"]]
SSP370_df <- result_list[["SSP370"]]
SSP585_df <- result_list[["SSP585"]]

# Plot source arrays without manually replacing any estimates or interval bounds.


# 기간 필터링: 2050년까지
SSP245_df_filtered <- SSP245_df %>%
  filter(as.numeric(substr(period, 1, 4)) <= 2050)
SSP370_df_filtered <- SSP370_df %>%
  filter(as.numeric(substr(period, 1, 4)) <= 2050)
SSP585_df_filtered <- SSP585_df %>%
  filter(as.numeric(substr(period, 1, 4)) <= 2050)


# 점 연결 선만 그룹별 스타일로
ggplot(SSP245_df_filtered, aes(x = period, y = est)) +
  # 선: 그룹별 linetype & size 설정
  geom_line(
    aes(color = source, group = source, linetype = source, size = source),
    position = position_dodge(width = 0.2)
  ) +
  # 점: 그룹별 색상
  geom_point(aes(color = source, group = source),
             position = position_dodge(width = 0.2), size = 5) +
  # error bar: 선은 항상 실선
  geom_errorbar(
    aes(ymin = ci.l, ymax = ci.u, color = source, group = source),
    position = position_dodge(width = 0.2),
    width = 0.2, size = 0.8 
  ) +
  # 색상 수동 지정
  scale_color_manual(
    values = c(
      "anabs" = "black",           
      "anabs_under10" = "#808080",  
      "anabs_over10" = "#CCCC00",  
      "anabs_over28" = "#800080"   
    )
  ) +
  # 점선 설정 (anabs만 dashed)
  scale_linetype_manual(
    values = c(
      "anabs" = "dashed",
      "anabs_under10" = "solid",
      "anabs_over10" = "solid",
      "anabs_over28" = "solid"
    )
  ) +
  # 선 굵기 수동 설정 (anabs는 0.8, 나머지는 1.5)
  scale_size_manual(
    values = c(
      "anabs" = 0.8,
      "anabs_under10" = 1.5,
      "anabs_over10" = 1.5,
      "anabs_over28" = 1.5
    )
  ) +
  # Y축 단절
  scale_y_break(c(1000, 16000)) +
  # 제목 및 테마
  labs(x = "Period", y = "Excess cases", color = "Group", linetype = "Group") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

###################### 기준기간 바꾼 것.

df_plot <- SSP585_df_filtered %>%
  filter(as.integer(substr(period, 1, 4)) >= 2016) %>%
  mutate(
    ci.l.plot = ifelse(period == "2016-2020", NA_real_, ci.l),
    ci.u.plot = ifelse(period == "2016-2020", NA_real_, ci.u),
    # x축 순서 고정(연대순)
    period = factor(period, levels = unique(period[order(as.integer(substr(period, 1, 4)))]))
  )

# 2) 시각화: 2016-2020부터, 그 구간만 에러바 미표시
ggplot(df_plot, aes(x = period, y = est)) +
  # 선: 그룹별 linetype & size
  geom_line(
    aes(color = source, group = source, linetype = source, size = source),
    position = position_dodge(width = 0.2)
  ) +
  # 점: 그룹별 색상
  geom_point(
    aes(color = source, group = source),
    position = position_dodge(width = 0.2), size = 5
  ) +
  # 에러바: 2016-2020은 ci.*.plot 이 NA라 자동으로 미표시
  geom_errorbar(
    aes(ymin = ci.l.plot, ymax = ci.u.plot, color = source, group = source),
    position = position_dodge(width = 0.2),
    width = 0.2, size = 0.8
  ) +
  # 색상 수동 지정
  scale_color_manual(
    values = c(
      "anabs" = "black",
      "anabs_under10" = "#808080",
      "anabs_over10" = "#FF8000",
      "anabs_over28" = "#800080"
    )
  ) +
  # 점선/선두께 수동 지정
  scale_linetype_manual(
    values = c(
      "anabs" = "dashed",
      "anabs_under10" = "solid",
      "anabs_over10" = "solid",
      "anabs_over28" = "solid"
    )
  ) +
  scale_size_manual(
    values = c(
      "anabs" = 0.8,
      "anabs_under10" = 1.5,
      "anabs_over10" = 1.5,
      "anabs_over28" = 1.5
    )
  ) +
  # 필요시 y축 단절 유지(ggbreak 사용 시)
  scale_y_break(c(1000, 16000)) +
  labs(x = "Period", y = "Excess cases", color = "Group", linetype = "Group") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


######################### 지도 시각화.
load(file = "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\AR_projection/최종/AR_under10.RData")
load(file = "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\AR_projection/최종/AR_over10.RData")
load(file = "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\AR_projection/최종/AR_over28.RData")


afrel_list
afrel_list_over10
afrel_over28
afrel_under10

# 각 지역의 이름 추출 (예: "48", "27", ...)
regions <- names(afrel_list_over10)

# 2046-2050 SSP585 est 값을 추출하여 데이터프레임 생성
afrel_df <- lapply(regions, function(region) {
  est_value <- afrel_list[[region]]["2046-2050", "est", "tot", "SSP585"]
  data.frame(
    sido_b = region,
    est = est_value
  )
}) %>%
  bind_rows() %>% 
  filter(sido_b != "whole country")

# 2046-2050 SSP585 est 값을 추출하여 데이터프레임 생성
afrel_over10_df <- lapply(regions, function(region) {
  est_value <- afrel_list_over10[[region]]["2046-2050", "est", "tot", "SSP585"]
  data.frame(
    sido_b = region,
    est = est_value
  )
}) %>%
  bind_rows() %>% 
  filter(sido_b != "whole country")


afrel_under10_df <- lapply(regions, function(region) {
  est_value <- afrel_list_under10[[region]]["2046-2050", "est", "tot", "SSP585"]
  data.frame(
    sido_b = region,
    est = est_value
  )
}) %>%
  bind_rows() %>% 
  filter(sido_b != "whole country")


afrel_over28_df <- lapply(regions, function(region) {
  est_value <- afrel_list_over28[[region]]["2046-2050", "est", "tot", "SSP585"]
  data.frame(
    sido_b = region,
    est = est_value
  )
}) %>%
  bind_rows() %>% 
  filter(sido_b != "whole country")

## growth rate는 아마 quantile이나 이렇게 구분해야할듯. categorical로 안 하면 차이가 너무 커서 안되고.
## 정확한 수치는 Appendix Table로. 
korea_sido_afrel_df <- korea_sido %>% 
  left_join(afrel_df, by = "sido_b")
korea_sido_afrel_over10_df <- korea_sido %>% 
  left_join(afrel_over10_df, by = "sido_b")
korea_sido_afrel_under10_df <- korea_sido %>% 
  left_join(afrel_under10_df, by = "sido_b")
korea_sido_afrel_over28_df <- korea_sido %>% 
  left_join(afrel_over28_df, by = "sido_b")

##################

range(korea_sido_afrel_df$est)
range(korea_sido_afrel_over10_df$est)
range(korea_sido_afrel_under10_df$est)
range(korea_sido_afrel_over28_df$est)


ggplot(korea_sido_afrel_df) +
  geom_sf(aes(fill = est)) +  # GCM_mean을 기준으로 색상 채우기
  scale_fill_gradient(low = "#F8F8FF", high = "#FF0000",  # 색상의 최소값과 최대값 설정
                      name = "GCM Mean",
                      limits = c(-5,40)) + 
  theme_minimal() +  # 미니멀한 테마 적용
  labs( fill = "GCM Mean")  # 범례 제목

ggplot(korea_sido_afrel_over10_df) +
  geom_sf(aes(fill = est)) +  # GCM_mean을 기준으로 색상 채우기
  scale_fill_gradient(low = "#F8F8FF", high = "#CCCC00",  # 색상의 최소값과 최대값 설정
                      name = "GCM Mean",
                      limits = c(-10,20)) + 
  theme_minimal() +  # 미니멀한 테마 적용
  labs( fill = "GCM Mean")  # 범례 제목

ggplot(korea_sido_afrel_over28_df) +
  geom_sf(aes(fill = est)) +  # GCM_mean을 기준으로 색상 채우기
  scale_fill_gradient(low = "#F8F8FF", high = "#800080",  # 색상의 최소값과 최대값 설정
                      name = "GCM Mean",
                      limits = c(-5,5)) + 
  theme_minimal() +  # 미니멀한 테마 적용
  labs( fill = "GCM Mean")  # 범례 제목

ggplot(korea_sido_afrel_under10_df) +
  geom_sf(aes(fill = est)) +  # GCM_mean을 기준으로 색상 채우기
  scale_fill_gradient(low = "#F8F8FF", high = "#808080",  # 색상의 최소값과 최대값 설정
                      name = "GCM Mean",
                      limits = c(0,32)) + 
  theme_minimal() +  # 미니멀한 테마 적용
  labs( fill = "GCM Mean")  # 범례 제목







######################### 각 지역별로는 간단한 그래프로 시각화.
load(file = "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\AR_projection/AR_sido_5000.RData")
load(file = "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\AR_projection/최종/AR_sido_under10.RData")
load(file = "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\AR_projection/최종/AR_sido_over10.RData")
load(file = "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\AR_projection/최종/AR_sido_over28.RData")

names(afabs_list)
afabs <- afabs_list[["50"]]
afabs_under10 <- afabs_list_under10[["50"]]
afabs_over10 <- afabs_list_over10[["50"]]
afabs_over28 <- afabs_list_over28[["50"]]

# 함수: 배열을 데이터프레임으로 변환하고 period 및 source 열 추가
extract_df <- function(arr, scenario, source_name) {
  df <- as.data.frame(arr[, , "tot", scenario])
  df$period <- rownames(df)
  df$source <- source_name
  return(df)
}

# 시나리오 목록
scenarios <- c("SSP245", "SSP370", "SSP585")

# 배열 목록 및 이름
array_list <- list(afabs = afabs, afabs_under10 = afabs_under10, afabs_over10 = afabs_over10, afabs_over28 = afabs_over28)

# 결과 저장용 리스트
result_list <- list()

# 각 시나리오에 대해 처리
for (scen in scenarios) {
  temp_list <- lapply(names(array_list), function(name) {
    extract_df(array_list[[name]], scen, name)
  })
  result_list[[scen]] <- bind_rows(temp_list)
}

# 결과: 시나리오별 데이터프레임
SSP245_df <- result_list[["SSP245"]]
SSP370_df <- result_list[["SSP370"]]
SSP585_df <- result_list[["SSP585"]]



# 기간 필터링: 2050년까지
SSP245_df_filtered <- SSP245_df %>%
  filter(as.numeric(substr(period, 1, 4)) <= 2050)
SSP370_df_filtered <- SSP370_df %>%
  filter(as.numeric(substr(period, 1, 4)) <= 2050)
SSP585_df_filtered <- SSP585_df %>%
  filter(as.numeric(substr(period, 1, 4)) <= 2050)




df_plot <- SSP585_df_filtered %>%
  filter(as.integer(substr(period, 1, 4)) >= 2016) %>%
  mutate(
    ci.l.plot = ifelse(period == "2016-2020", NA_real_, ci.l),
    ci.u.plot = ifelse(period == "2016-2020", NA_real_, ci.u),
    # x축 순서 고정(연대순)
    period = factor(period, levels = unique(period[order(as.integer(substr(period, 1, 4)))]))
  ) %>%
  filter(period %in% c("2016-2020", "2046-2050"))


# 2) 시각화: 2016-2020부터, 그 구간만 에러바 미표시
ggplot(df_plot, aes(x = period, y = est)) +
  # 선
  geom_line(
    aes(color = source, group = source, linetype = source, size = source),
    position = position_dodge(width = 0.2)
  ) +
  # 점
  geom_point(
    aes(color = source, group = source),
    position = position_dodge(width = 0.2), size = 7
  ) +
  # 에러바
  geom_errorbar(
    aes(ymin = ci.l.plot, ymax = ci.u.plot, color = source, group = source),
    position = position_dodge(width = 0.2),
    width = 0.2, size = 0.8
  ) +
  # 색상/선스타일/두께
  scale_color_manual(values = c(
    "afabs" = "black",
    "afabs_under10" = "#808080",
    "afabs_over10"  = "#CCCC00",
    "afabs_over28"  = "#800080"
  )) +
  scale_linetype_manual(values = c(
    "afabs" = "dashed",
    "afabs_under10" = "solid",
    "afabs_over10"  = "solid",
    "afabs_over28"  = "solid"
  )) +
  scale_size_manual(values = c(
    "afabs" = 0.8,
    "afabs_under10" = 0.8,
    "afabs_over10"  = 0.8,
    "afabs_over28"  = 0.8
  )) +
  # y축 범위
  scale_y_continuous(limits = c(-60, 100)) +
  labs(x = "Period", y = "Excess cases", color = "Group", linetype = "Group") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    # 가로선만, 색은 grey90
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.minor.y = element_line(color = "grey90")
  )




##############################################################################
############################################################################
############################ Figure 1. 

## 전체 기간에 대해서 전국 total 쯔쯔가무시 case.

weekly_whole_country

unique(weekly_whole_country$year)
sum(weekly_whole_country$weekly_total_cases)

weekly_whole_country %>% 
  dplyr::select(weekly_total_cases, week) %>% 
  filter(week <= 50 & week >= 36) %>% 
  dplyr::select(weekly_total_cases) %>% 
  sum()

# 연도와 주를 결합하여 날짜 생성
weekly_whole_country_yearweek <- weekly_whole_country %>%
  mutate(date = as.Date(paste(year, week, 1, sep = "-"), "%Y-%U-%u"))

# case 꺾은선 그래프 생성 - 전체 기간에 대해서.
ggplot(weekly_whole_country_yearweek, aes(x = date, y = weekly_total_cases)) +
  geom_line(size = 0.7, color = "black") +
  labs(x = "Date",
       y = "Average  cases") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid = element_blank()  # 격자 제거
  )


################ 시도단위로. 
# 연도와 주를 결합하여 날짜 생성
weekly_data_SiDo_yearweek <- weekly_data_SiDo %>%
  mutate(date = as.Date(paste(year, week, 1, sep = "-"), "%Y-%U-%u"))


# 시도별 주간 case 선 그래프
ggplot(weekly_data_SiDo_yearweek, aes(x = date, y = weekly_total_cases)) +
  geom_line(size = 0.7, color = "black") +
  facet_wrap(~ sido_b, scales = "fixed") +  # y축 동일하게 유지
  coord_cartesian(ylim = c(0, 400)) +       # y축 범위 제한 (데이터는 자르지 않음)
  labs(
    x = "Date",
    y = "Weekly total cases",
    title = "Weekly case trends by region"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 9),
    strip.text = element_text(size = 14, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),     # 테두리선 추가
    panel.grid.major = element_line(color = "grey80", size = 0.2),           # 연한 주 격자선
    panel.grid.minor = element_line(color = "grey90", size = 0.1)            # 더 연한 보조 격자선
  )







# Week별로 그룹화하여 case_total, tmean, preci의 평균 계산
weekly_whole_country_yearweek <- weekly_whole_country_yearweek %>%
  group_by(week) %>%
  summarise(
    avg_case_total = mean(weekly_total_cases, na.rm = TRUE),
    avg_tmean = median(weekly_tmean, na.rm = TRUE),
    avg_preci = median(weekly_total_preci, na.rm = TRUE),
    .groups = 'drop'  # 그룹화를 해제
  )


ggplot(weekly_whole_country_yearweek, aes(x = week, y = avg_case_total)) +
  geom_point( aes(x = week, y = avg_case_total), size = 2) +
  geom_line(size = 0.8, color = "black") +
  labs(x = "Week",
       y = "Average Dengue cases") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid = element_blank()  # 격자 제거
  )



################ 시도단위로. 
weekly_data_SiDo_week <- weekly_data_SiDo %>%
  group_by(sido_b, week) %>%
  summarise(
    avg_case_total = mean(weekly_total_cases, na.rm = TRUE),
    avg_tmean = median(weekly_tmean, na.rm = TRUE),
    avg_preci = median(weekly_total_preci, na.rm = TRUE),
    .groups = 'drop'  # 그룹화를 해제
  )


ggplot(weekly_data_SiDo_week, aes(x = week, y = avg_case_total)) +
  geom_point(size = 1.5) +
  geom_line(size = 0.8, color = "black") +
  facet_wrap(~ sido_b, scales = "fixed") +  # 각 지역 별 패싯, y축 고정
  labs(
    x = "Week",
    y = "Average Dengue Cases",
    title = "Weekly Average Dengue Cases by Region"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 14, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),     # 패싯 테두리 추가
    panel.grid.major = element_line(color = "grey80", size = 0.3),           # 연한 격자선
    panel.grid.minor = element_line(color = "grey90", size = 0.2)
  )




########## Temperature.

plot(weekly_tmean ~ week,
     data = weekly_whole_country,
     type = "p",             # 포인트만
     col = "#FFCC99",        # 주황색 포인트
     xlab = "Week",
     ylab = "Weekly Mean Temperature",
     main = "Weekly Mean Temperature by Week",
     ylim = c(-10, 35),
     cex = 1.5)      # y축 범위 지정

# 평균 강수량 선 추가 (동일한 색상)
lines(avg_tmean ~ week,
      data = weekly_whole_country_yearweek,
      col = "#FF9933",
      lwd = 5)


### Preci는 histogram으로
plot(weekly_total_preci ~ week,
     data = weekly_whole_country,
     type = "p",             # 포인트만
     col = "#99CCFF",        # 주황색 포인트
     xlab = "Week",
     ylab = "Weekly Mean Temperature",
     main = "Weekly Mean Temperature by Week",
     ylim = c(0, 280),
     cex = 1.2)      # y축 범위 지정

# 평균 강수량 선 추가 (동일한 색상)
lines(avg_preci ~ week,
      data = weekly_whole_country_yearweek,
      col = "#3399FF",
      lwd = 5)

################ 시도단위로. 

# 두 데이터프레임 병합: sido_b + week 기준
merged_data <- weekly_data_SiDo_yearweek %>%
  left_join(weekly_data_SiDo_week, by = c("sido_b", "week"))

ggplot(merged_data, aes(x = week)) +
  geom_point(aes(y = avg_tmean), color = "#FFCC99", size = 0.5) +      # 점: 관측치
  geom_line(aes(y = weekly_tmean), color = "#FF9933", size = 1.2) +  # 선: 연 평균
  facet_wrap(~ sido_b) +
  labs(
    x = "Week",
    y = "Weekly Mean Temperature",
    title = "Weekly Mean Temperature by Week (with Yearly Average)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 14, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),     # 패싯 테두리 추가
    panel.grid.major = element_line(color = "grey80", size = 0.3),           # 연한 격자선
    panel.grid.minor = element_line(color = "grey90", size = 0.2)
  )



ggplot(merged_data, aes(x = week)) +
  geom_point(aes(y = avg_preci), color = "#99CCFF", size = 0.5) +      # 점: 관측치
  geom_line(aes(y = weekly_total_preci), color = "#3399FF", size = 1.2) +  # 선: 연 평균
  facet_wrap(~ sido_b) +
  labs(
    x = "Week",
    y = "Weekly Mean Temperature",
    title = "Weekly Mean Temperature by Week (with Yearly Average)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 14, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),     # 패싯 테두리 추가
    panel.grid.major = element_line(color = "grey80", size = 0.3),           # 연한 격자선
    panel.grid.minor = element_line(color = "grey90", size = 0.2)
  )





anrel_list[["50"]]
anrel_list_over10[["50"]]
anrel_list_under10[["50"]]
anrel_list_over28[["50"]]



####################################################
######################## 시간 흐름에 따라서 발생 패턴이 달라졌는지 확인. 

setwd("C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\Scrub typhus daily")
scrub_complt <- read.csv("scrub_complt.csv")

scrub_complt <- scrub_complt %>% 
  mutate(case_total = zero + ten + twenty + thirty + fourty + fifty + sixty + seventy)

df_summary <- scrub_complt %>%
  group_by(sido_b, Date) %>%
  summarise(case_total_sum = sum(case_total, na.rm = TRUE), .groups = "drop")

df_country <- df_summary %>%
  group_by(Date) %>%
  summarise(case_total_sum = sum(case_total_sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(sido_b = "whole_country")



# 1) 전처리: Date → Date형, 연도/월-일 생성, 2/29 제거, 공통 축(2001년)으로 매핑
df_plot <- df_country %>%
  mutate(Date = as.Date(Date),
         year = year(Date),
         md   = format(Date, "%m-%d")) %>%
  filter(md != "02-29") %>%                           # 윤년 2/29 제거(원하면 주석 처리)
  mutate(x_date = as.Date(paste0("2001-", md)))       # 공통 x축(2001-01-01 ~ 2001-12-31)

# 2) 연도별 색상 팔레트: 2001년=#FFB6C1, 마지막 연도=#DC143C로 그라디언트
yrs <- sort(unique(df_plot$year))
pal <- colorRampPalette(c("#FFF0F5", "#DC143C"))(length(yrs))
names(pal) <- yrs

# 3) 시각화
ggplot(df_plot, aes(x = x_date, y = case_total_sum,
                    color = factor(year), group = year)) +
  geom_line(linewidth = 0.7, alpha = 0.95) +
  scale_color_manual(values = pal, name = "Year") +
  scale_x_date(limits = as.Date(c("2001-01-01", "2001-12-31")),
               date_breaks = "1 month", date_labels = "%b", expand = c(0, 0)) +
  labs(x = "Month (Jan–Dec)",
       y = "Daily cases (sum)",
       title = "Whole-country daily cases by calendar day, colored by year") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))


########################## Weekly data로 해보기. 


weekly_data_SiDo <- read.csv("C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\Scrub typhus weekly/weekly_data_SiDo.csv")

weekly_data_SiDo <- weekly_data_SiDo %>% 
   dplyr::select(sido_b, year, week, weekly_total_cases)

df_country <- weekly_data_SiDo %>%
  group_by(year, week) %>%
  summarise(weekly_total_cases = sum(weekly_total_cases, na.rm = TRUE), .groups = "drop") %>%
  mutate(sido_b = "whole_country")



#### 각 연도의 평균이 100이 되도록 스케일 조정. 
#### 연도별 그룹화 → group_by(year)
#### 해당 연도의 주간 합계 평균 계산 → mean_y = mean(weekly_total_cases)
#### 해당 주의 값 ÷ 연도 평균 × 100 → idx_mean100 = 100 * weekly_total_cases / mean_y

# 1) 연도별 평균=100 지수화
df_idx <- df_country %>%
  group_by(year) %>%
  mutate(
    mean_y = mean(weekly_total_cases, na.rm = TRUE),
    idx_mean100 = 100 * weekly_total_cases / mean_y
  ) %>%
  ungroup()

# 2) 연도별 색상 팔레트 지정

# 연도별 색상 팔레트
yrs <- sort(unique(df_idx$year))
pal <- colorRampPalette(c("#00CED1", "#FF0000"))(length(yrs))
names(pal) <- yrs

# 연도별 alpha 값 생성 (2001=0.5, max(year)=0.8)
df_idx <- df_idx %>%
  mutate(alpha_val = scales::rescale(year, to = c(0.5, 0.8)))

ggplot(df_idx, aes(x = week, y = idx_mean100,
                   color = factor(year), group = year,
                   alpha = alpha_val)) +   # ⬅️ alpha를 aesthetic으로 매핑
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = pal, name = "Year") +
  scale_alpha_identity() +   # ⬅️ 우리가 계산한 값 그대로 사용
  scale_x_continuous(breaks = seq(1, 52, by = 4)) +
  scale_y_break(c(100, 800)) +  
  labs(x = "Week of Year (1–52)",
       y = "Index (mean=100)",
       title = "Seasonality by year (mean-normalized, alpha by year)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.line = element_line(colour = "black", linewidth = 0.5),  # x,y 축 진하게
        axis.ticks = element_line(colour = "black", linewidth = 0.5),   # 눈금도 진하게
        axis.text  = element_text(size = 14)    
        )
      


# 연도별 색상 팔레트

yrs <- sort(unique(df_country$year))
pal <- colorRampPalette(c("#00CED1", "#FF0000"))(length(yrs))
names(pal) <- yrs

df_country <- df_country %>%
  mutate(
    alpha_val = scales::rescale(year, to = c(0.5, 0.8)),
    y_low  = ifelse(weekly_total_cases <= 100, weekly_total_cases, NA_real_),  # 중간 구간을 NA로 → 선 단절
    y_high = ifelse(weekly_total_cases >= 500, weekly_total_cases, NA_real_)   # 중간 구간을 NA로 → 선 단절
  )

# 1) 하위 구간(≤100) — x축 동일, 중간 구간은 끊김 유지
p_low <- ggplot(df_country, aes(x = week, y = y_low,
                                color = factor(year), group = year, alpha = alpha_val)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +          # y_low가 NA인 구간은 자동으로 끊김
  scale_color_manual(values = pal) +
  scale_alpha_identity() +
  scale_x_continuous(breaks = seq(1, 52, by = 4)) +   # x축 유지
  coord_cartesian(ylim = c(0, 100)) +                 # 보기만 확대(데이터는 유지)
  labs(x = "Week of Year (1–52)", y = "Weekly cases",
       title = "Whole-country weekly cases (≤ 100)") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title  = element_text(face = "bold"),
    axis.line   = element_line(colour = "black", linewidth = 0.5),
    axis.ticks  = element_line(colour = "black", linewidth = 0.5),
    axis.text   = element_text(size = 14, family = "ArialMT"),  # ⬅️ 눈금 폰트 Arial
    axis.title  = element_text(family = "ArialMT"),             # ⬅️ 축 제목 폰트 Arial
    legend.position = "none"
  )

# 2) 상위 구간(≥800) — x축 동일, 중간 구간은 끊김 유지
max_y <- max(df_country$weekly_total_cases, na.rm = TRUE)

p_high <- ggplot(df_country, aes(x = week, y = y_high,
                                 color = factor(year), group = year, alpha = alpha_val)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +          # y_high가 NA인 구간은 자동으로 끊김
  scale_color_manual(values = pal) +
  scale_alpha_identity() +
  scale_x_continuous(breaks = seq(1, 52, by = 4)) +   # x축 유지
  coord_cartesian(ylim = c(800, max_y)) +
  labs(x = "Week of Year (1–52)", y = "Weekly cases",
       title = "Whole-country weekly cases (≥ 800)") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title  = element_text(face = "bold"),
    axis.line   = element_line(colour = "black", linewidth = 0.5),
    axis.ticks  = element_line(colour = "black", linewidth = 0.5),
    axis.text   = element_text(size = 14, family = "ArialMT"),  # ⬅️ 눈금 폰트 Arial
    axis.title  = element_text(family = "ArialMT"),             # ⬅️ 축 제목 폰트 Arial
    legend.position = "none"
  )


# 4) 출력
p_low
p_high



p_all <- ggplot(df_country, aes(x = week, y = weekly_total_cases,
                                color = factor(year), group = year,
                                alpha = alpha_val)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = pal) +
  scale_alpha_identity() +
  scale_x_continuous(breaks = seq(1, 52, by = 4)) +
  labs(x = "Week of Year (1–52)",
       y = "Weekly cases",
       title = "Whole-country weekly cases (all ranges)") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title  = element_text(face = "bold"),
    axis.line   = element_line(colour = "black", linewidth = 0.5),
    axis.ticks  = element_line(colour = "black", linewidth = 0.5),
    axis.text   = element_text(size = 14, family = "ArialMT"),
    axis.title  = element_text(family = "ArialMT", size = 16),
    legend.position = "none"  # 필요하면 주석 처리해서 범례 표시
  )

# 출력
p_all





######################################################
# Figure 5: manuscript-aligned SSP5–8.5, province-specific RR=1 crossings.
# Use a dedicated script with values transcribed from Supplementary Table S1.
# Run from the repository root after making the provincial GCM input available:
#   source("scripts/05_figure5_seasonality.R")
# The former 96th-percentile temperature boundary and SSP3–7.0
# inputs have been removed. See CHANGELOG.md for scientific differences.
######################################################

################################################################
############## 시도단위로 발생 위험도 산출. Standardized 발생률로.  

korea_sido <- read_sf("C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\korea_map/simplified.shp") %>% 
  st_transform(4326)

korea_sido$SIDO_CD


# SIDO_CD 열 이름을 sido_b로 변경하고, 값 변환
korea_sido <- korea_sido %>%
  dplyr::select(-BASE_DATE, -SIDO_NM) %>% 
  rename(sido_b = SIDO_CD) %>%  # SIDO_CD를 sido_b로 이름 변경
  mutate(sido_b = case_when(
    sido_b == "11" ~ "11",  # "11" 변환 추가
    sido_b == "21" ~ "26",
    sido_b == "22" ~ "27",
    sido_b == "23" ~ "28",
    sido_b == "24" ~ "29",
    sido_b == "25" ~ "30",
    sido_b == "26" ~ "31",
    sido_b == "31" ~ "41",
    sido_b == "32" ~ "42",
    sido_b %in% c("29", "33") ~ "43",  # "29"와 "33"을 모두 "43"으로 변환
    sido_b == "34" ~ "44",
    sido_b == "35" ~ "45",
    sido_b == "36" ~ "46",
    sido_b == "37" ~ "47",
    sido_b == "38" ~ "48",
    sido_b == "39" ~ "50",
    TRUE ~ sido_b  # 그 외 값은 그대로 유지
  ))

korea_sido$sido_b

plot(korea_sido)




weekly_data_SiDo <- read.csv("C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\Scrub typhus weekly/weekly_data_SiDo.csv")
