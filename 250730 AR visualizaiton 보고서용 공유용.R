###############  보고서용 visualization


library(tidyr);library(dplyr);library(purrr) ;library(lubridate) ; library(readxl); library(ggplot2)
library(sf) ; library(tibble)

load("C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\AR_projection/AR_final_5000.RData")

names(anabs_list)




#################### Figure 생성에 용이한 형태로 데이터 변형
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


#################################################################


############################ Figure 3, 4 Temperature의 historical, projection map.

############### 기간별 평균 기온과 baseline으로 부터의 increase를 각 시도별로.
# ANNUAL AVERAGES (CENTERED)

load("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/기후변화 데이터/tmean_scn_ssp_cal/updated/sigungu_combined_data_SSP245.RData")
load("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/기후변화 데이터/tmean_scn_ssp_cal/updated/sigungu_combined_data_SSP370.RData")
load("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/기후변화 데이터/tmean_scn_ssp_cal/updated/sigungu_combined_data_SSP585.RData")


sigungu_SSP245_temp_increase <- list()
sigungu_SSP370_temp_increase <- list()
sigungu_SSP585_temp_increase <- list()

for (sigungu in names(sigungu_combined_data_SSP245)) {
  
  # 각 시도별 데이터프레임을 가져오기
  total_SSP245 <- sigungu_combined_data_SSP245[[sigungu]]
  total_SSP370 <- sigungu_combined_data_SSP370[[sigungu]]
  total_SSP585 <- sigungu_combined_data_SSP585[[sigungu]]
  
  # total_SSP245 데이터프레임에 period 열 추가
  total_SSP245 <- total_SSP245 %>%
    mutate(period = case_when(
      year >= 2000 & year <= 2009 ~ "2000-2009",
      year >= 2010 & year <= 2019 ~ "2010-2019",
      year >= 2020 & year <= 2029 ~ "2020-2029",
      year >= 2030 & year <= 2039 ~ "2030-2039",
      year >= 2040 & year <= 2049 ~ "2040-2049",
      year >= 2050 & year <= 2059 ~ "2050-2059",
      year >= 2060 & year <= 2069 ~ "2060-2069",
      year >= 2070 & year <= 2079 ~ "2070-2079",
      year >= 2080 & year <= 2089 ~ "2080-2089",
      year >= 2090 & year <= 2099 ~ "2090-2099",
      TRUE ~ NA_character_  # 그 외 값들은 NA로 처리
    ))
  
  total_SSP370 <- total_SSP370 %>%
    mutate(period = case_when(
      year >= 2000 & year <= 2009 ~ "2000-2009",
      year >= 2010 & year <= 2019 ~ "2010-2019",
      year >= 2020 & year <= 2029 ~ "2020-2029",
      year >= 2030 & year <= 2039 ~ "2030-2039",
      year >= 2040 & year <= 2049 ~ "2040-2049",
      year >= 2050 & year <= 2059 ~ "2050-2059",
      year >= 2060 & year <= 2069 ~ "2060-2069",
      year >= 2070 & year <= 2079 ~ "2070-2079",
      year >= 2080 & year <= 2089 ~ "2080-2089",
      year >= 2090 & year <= 2099 ~ "2090-2099",
      TRUE ~ NA_character_  # 그 외 값들은 NA로 처리
    ))
  
  total_SSP585 <- total_SSP585 %>%
    mutate(period = case_when(
      year >= 2000 & year <= 2009 ~ "2000-2009",
      year >= 2010 & year <= 2019 ~ "2010-2019",
      year >= 2020 & year <= 2029 ~ "2020-2029",
      year >= 2030 & year <= 2039 ~ "2030-2039",
      year >= 2040 & year <= 2049 ~ "2040-2049",
      year >= 2050 & year <= 2059 ~ "2050-2059",
      year >= 2060 & year <= 2069 ~ "2060-2069",
      year >= 2070 & year <= 2079 ~ "2070-2079",
      year >= 2080 & year <= 2089 ~ "2080-2089",
      year >= 2090 & year <= 2099 ~ "2090-2099",
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
    mutate(GCM_mean_diff = GCM_mean - GCM_mean[period == "2000-2009"])
  
  avg_SSP370_diff <- avg_SSP370 %>%
    mutate(GCM_mean_diff = GCM_mean - GCM_mean[period == "2000-2009"])
  
  avg_SSP585_diff <- avg_SSP585 %>%
    mutate(GCM_mean_diff = GCM_mean - GCM_mean[period == "2000-2009"])
  
  sigungu_SSP245_temp_increase[[sigungu]] <- avg_SSP245_diff 
  sigungu_SSP370_temp_increase[[sigungu]] <- avg_SSP370_diff
  sigungu_SSP585_temp_increase[[sigungu]] <- avg_SSP585_diff
}


########### Temperature의 projection map 만들기. 
## Basline: "2001-2005" 
## Projection: 2026~2030, 2036~2040, 2046~2050 
X00_sgg_info_bh_revised <- read_excel("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/00_sgg_info_bh_revised.xlsx")


setwd("C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\보고서용 결과\\sig_20230729")
Korea <- st_read(dsn = "sig.shp") %>% 
  st_set_crs(5179) %>% 
  st_transform(4326)

Korea <- Korea %>%
  rename(sgg_b = SIG_CD)


# 1) 옛→새 코드 매핑 -----------------------------------------------
new2old <- c(
  "27720" = "47720",
  "28177" = "28170",
  "51110" = "42110", "51130" = "42130", "51150" = "42150",
  "51170" = "42170", "51190" = "42190",
  "51210" = "42210", "51230" = "42230",
  "51720" = "42720", "51730" = "42730", "51750" = "42750",
  "51760" = "42760", "51770" = "42770", "51780" = "42780",
  "51790" = "42790", "51800" = "42800", "51810" = "42810",
  "51820" = "42820", "51830" = "42830"
)

# 2) 새 객체 Korea_rev 생성 -----------------------------------------
Korea_rev <- Korea %>% 
  mutate(
    sgg_b = recode(as.character(sgg_b), !!!new2old)
  )

# 3) 교체 결과 확인 --------------------------------------------------
existing_names <- names(SiGunGublup)
all_sgg_in_cases <- unique(Korea_rev$sgg_b)

missing_in_SiGunGublup <- setdiff(all_sgg_in_cases, existing_names)
extra_in_SiGunGublup   <- setdiff(existing_names,  all_sgg_in_cases)







##################### map에 values 연결.
### baseline period에 대해서 평균 기온값 할당하는 열.
##  "2030-2039" "2050-2059" "2070-2079" 이렇게 세 조각으로 value 산출하면 될듯.
## 
sigungu_SSP245_temp_increase 
sigungu_SSP370_temp_increase 
sigungu_SSP585_temp_increase 


# 빈 데이터프레임 초기화
sigungu_SSP245_df <- data.frame()
sigungu_SSP370_df <- data.frame()
sigungu_SSP585_df <- data.frame()

# 리스트의 각 요소에 대해 반복 작업 수행
for (sigungu_name in names(sigungu_SSP245_temp_increase)) {
  
  # 해당 시도에 대한 데이터프레임 불러오기
  df <- sigungu_SSP370_temp_increase[[sigungu_name]]
  
  # 각 필요한 값을 추출
  GCM_mean_2000 <- df %>% 
    filter(period == "2000-2009") %>%
    pull(GCM_mean)
  
  diff_2030 <- df %>% 
    filter(period == "2030-2039") %>%
    pull(GCM_mean_diff)
  
  diff_2050 <- df %>% 
    filter(period == "2050-2059") %>%
    pull(GCM_mean_diff)
  
  diff_2070 <- df %>% 
    filter(period == "2070-2079") %>%
    pull(GCM_mean_diff)
  
  # 각 값을 하나의 데이터프레임으로 정리
  temp_df <- data.frame(
    sgg_b = sigungu_name,             # 시도 이름
    GCM_mean = GCM_mean_2000,       
    diff_2030 = diff_2030,          
    diff_2050 = diff_2050,          
    diff_2070 = diff_2070           
  )
  
  # 결과 데이터프레임에 추가
  sigungu_SSP370_df <- bind_rows(sigungu_SSP370_df, temp_df)
}

# 중복된 행 제거
sigungu_SSP245_df <- sigungu_SSP245_df %>% distinct()
sigungu_SSP370_df <- sigungu_SSP370_df %>% distinct()
sigungu_SSP585_df <- sigungu_SSP585_df %>% distinct()

############################################
korea_sigungu_SSP245 <- Korea_rev %>% 
  left_join(sigungu_SSP245_df, by = "sgg_b")
korea_sigungu_SSP370 <- Korea_rev %>% 
  left_join(sigungu_SSP370_df, by = "sgg_b")
korea_sigungu_SSP585 <- Korea_rev %>% 
  left_join(sigungu_SSP585_df, by = "sgg_b")

bbox <- st_bbox(korea_sigungu_SSP245)
print(bbox)

# GCM_mean을 기준으로 색상 그라디언트 적용하여 지도 그리기
ggplot(korea_sigungu_SSP245) +
  geom_sf(aes(fill = GCM_mean)) +  # GCM_mean을 기준으로 색상 채우기
  scale_fill_gradient(low = "#FFFF99", high = "#CC0000",  # 색상의 최소값과 최대값 설정
                      name = "GCM Mean",                    # 색상 범례 제목 설정
                      limits = c(min(korea_sigungu_SSP245$GCM_mean, na.rm = TRUE), 
                                 max(korea_sigungu_SSP245$GCM_mean, na.rm = TRUE))) + 
  theme_minimal() +  # 미니멀한 테마 적용
  labs(title = "GCM Mean by Region (SSP245 Scenario)",   # 제목
       fill = "GCM Mean")  # 범례 제목

range(korea_sigungu_SSP585$diff_2070)

range(korea_sido_SSP585$diff_2090)

ggplot(korea_sigungu_SSP585) +
  geom_sf(aes(fill = diff_2070)) +  # diff_2040을 기준으로 색상 채우기
  scale_fill_gradient(low = "#FFFF99", high = "#CC0000",  # 색상의 최소값과 최대값 설정
                      name = "GCM Mean",                 # 색상 범례 제목 설정
                      limits = c(0.0, 4.5)) +          # 임의의 최소값과 최대값 설정
  theme_minimal() +  # 미니멀한 테마 적용
  labs(fill = "Temperature increase")  # 범례 제목






########### 각 지역별로 기간별 값 할당하기.
## Basline: "2000-2009" AN 절대값 시각화
## Projection: "2040-2049" "2070-2079" "2090-2099" 상승률 시각화.
#####################################

str(afrel_df_list)

# anabs_list에서 지역코드별 데이터를 추출하여 새로운 리스트 생성
############ AF 로 다시 해보기.
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


sigungu_SSP245_AN <- data.frame(
  sgg_b = character(),  # 시도 이름
  AN_2000 = numeric(),   # 2010년 데이터 
  AN_2030 = numeric(),   # 2031-2035 데이터
  AN_2050 = numeric(),   # 2041-2045 데이터
  AN_2070 = numeric(),   # 2046-2050 데이터
  stringsAsFactors = FALSE
)

sigungu_SSP370_AN <- data.frame(
  sgg_b = character(),  # 시도 이름
  AN_2000 = numeric(),   # 2010년 데이터 
  AN_2030 = numeric(),   # 2031-2035 데이터
  AN_2050 = numeric(),   # 2041-2045 데이터
  AN_2070 = numeric(),   # 2046-2050 데이터
  stringsAsFactors = FALSE
)

sigungu_SSP585_AN <- data.frame(
  sgg_b = character(),  # 시도 이름
  AN_2000 = numeric(),   # 2010년 데이터 
  AN_2030 = numeric(),   # 2031-2035 데이터
  AN_2050 = numeric(),   # 2041-2045 데이터
  AN_2070 = numeric(),   # 2046-2050 데이터
  stringsAsFactors = FALSE
)

for (sigungu_name in names(AN_SSP370)) {
  
  # 해당 시도에 대한 데이터프레임 불러오기
  df <- as.data.frame(AN_SSP370[[sigungu_name]])  
  df <- rownames_to_column(df, var = "period")
  colnames(df) <- c("period", "AN", "ci_l", "ci_u")  
  
  # 각 필요한 값을 추출
  AN_2000 <- df %>% 
    filter(period == "2000-2009") %>%
    pull(AN)
  
  AN_2030 <- df %>% 
    filter(period == "2030-2039") %>%
    pull(AN)
  
  AN_2050 <- df %>% 
    filter(period == "2050-2059") %>%
    pull(AN)
  
  AN_2070 <- df %>% 
    filter(period == "2070-2079") %>%
    pull(AN)
  
  # 각 값을 하나의 데이터프레임으로 정리
  temp_df <- data.frame(
    sgg_b = sigungu_name,             # 시도 이름
    AN_2000 = AN_2000,      
    AN_2030 = AN_2030,      
    AN_2050 = AN_2050,          
    AN_2070 = AN_2070          
  )
  # 결과 데이터프레임에 추가
  sigungu_SSP370_AN <- bind_rows(sigungu_SSP370_AN, temp_df)
}

sigungu_SSP245_AN
sigungu_SSP370_AN
sigungu_SSP585_AN


sigungu_SSP245_AN <- sigungu_SSP245_AN %>%
  mutate(
    diff_2030 = ((AN_2030 - AN_2000)),  
    diff_2050 = ((AN_2050 - AN_2000)) ,  
    diff_2070 = ((AN_2070 - AN_2000))    
  )

sigungu_SSP370_AN <- sigungu_SSP370_AN %>%
  mutate(
    diff_2030 = ((AN_2030 - AN_2000)),  
    diff_2050 = ((AN_2050 - AN_2000)) ,  
    diff_2070 = ((AN_2070 - AN_2000))     
  )

sigungu_SSP585_AN <- sigungu_SSP585_AN %>%
  mutate(
    diff_2030 = ((AN_2030 - AN_2000)),  
    diff_2050 = ((AN_2050 - AN_2000)) ,  
    diff_2070 = ((AN_2070 - AN_2000))      
  )

save(sigungu_SSP245_AN, sigungu_SSP370_AN , sigungu_SSP585_AN, 
     file = "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\보고서용 결과\\AF_sigungu.RData")


korea_sigungu_SSP245_AN <- Korea_rev %>% 
  left_join(sigungu_SSP245_AN, by = "sgg_b")
korea_sigungu_SSP370_AN <- Korea_rev %>% 
  left_join(sigungu_SSP370_AN, by = "sgg_b")
korea_sigungu_SSP585_AN <- Korea_rev %>% 
  left_join(sigungu_SSP585_AN, by = "sgg_b")


##################
range(log((korea_sigungu_SSP245_AN$diff_2070)))
range((korea_sigungu_SSP245_AN$diff_2070))
range((korea_sigungu_SSP245_AN$AN_2000))
table(sigungu_SSP585_AN$diff_2090)

ggplot(korea_sigungu_SSP245_AN) +
  geom_sf(aes(fill = AN_2000)) +  # GCM_mean을 기준으로 색상 채우기
  scale_fill_gradient(low = "#F8F8FF", high = "#FF0000",  # 색상의 최소값과 최대값 설정
                      name = "GCM Mean",                    # 색상 범례 제목 설정
                      limits = c(-300, 90)) + 
  theme_minimal() +  # 미니멀한 테마 적용
  labs(  fill = "GCM Mean")  # 범례 제목

ggplot(korea_sigungu_SSP585_AN) +
  geom_sf(aes(fill = diff_2070)) +  # GCM_mean을 기준으로 색상 채우기
  scale_fill_gradient(low = "#F8F8FF", high = "#FF0000",  # 색상의 최소값과 최대값 설정
                      name = "GCM Mean",                    # 색상 범례 제목 설정
                      limits = c(-60,500)) + 
  theme_minimal() +  # 미니멀한 테마 적용
  labs( fill = "GCM Mean")  # 범례 제목





########################################################################
#############

#### AN 변화에 대한 시계열 그래프. 


load(file = "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\보고서용 결과\\Ansim_wholecountry_1000.RData")
anabs
# dimnames 추출
periods <- dimnames(anabs)[[1]]
vars <- dimnames(anabs)[[2]]
scenarios <- dimnames(anabs)[[4]]

# 빈 리스트 준비
df_list <- list()

# 시나리오별 반복
for (scen in scenarios) {
  # 시나리오별로 추출 (3차원 배열: 기간 x 변수)
  submat <- anabs[, , 1, scen]
  
  # 데이터프레임으로 변환
  df <- as.data.frame(submat)
  df$Period <- periods
  df$Scenario <- scen
  
  # 열 순서 정리
  df <- df %>% dplyr::select(Scenario, Period, est, ci.l, ci.u)
  
  # 리스트에 저장
  df_list[[scen]] <- df
}

# 하나의 데이터프레임으로 결합
final_df <- bind_rows(df_list)


#### AN 변화에 대한 시계열 그래프. 
#### 기준 기간 대비 

ggplot(final_df, aes(x = Period, y = est, color = Scenario, group = Scenario)) +
  geom_line(position = position_dodge(width = 0.2), size = 1.5) +  # 선을 그리며 약간씩 떨어뜨림
  geom_point(position = position_dodge(width = 0.2), size = 4) +  # 각 Period마다 점을 찍음
  geom_errorbar(aes(ymin = ci.l, ymax = ci.u),  # 신뢰구간(CI) 설정
                position = position_dodge(width = 0.2), width = 0.2) +  # Errorbar 설정
  scale_color_manual(values = c("#0080FF", "#FF9933", "#FF0000")) +  # 사용자 지정 색상
  scale_y_continuous(limits = c(18000, 50000)) +  # y축 범위 설정
  labs(title = "Excess cases by Scenario and Period (Up to 2055)",  # 제목 설정
       x = "Period",  # x축 레이블
       y = "Excess cases",  # y축 레이블
       color = "Scenario") +  # 범례 제목 설정
  theme_minimal() +  # 미니멀한 테마 적용
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # x축 눈금 45도 회전
        panel.grid = element_blank())  # 격자 제거




