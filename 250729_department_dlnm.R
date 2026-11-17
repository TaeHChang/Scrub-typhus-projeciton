################### Department level 쯔쯔가무시 DLNM
############# sido 단위로 계산된 blup을 sigungu 단위로 down-scaling. 

library(data.table) # HANDLE LARGE DATASETS
library(readr)
library(dlnm) ; library(gnm) ; library(splines) # MODELLING TOOLS
library(dplyr) ; library(tidyr) # DATA MANAGEMENT TOOLS
library(ggplot2) ; library(patchwork) # PLOTTING TOOLS
library(zoo); library(readxl); library(mixmeta)


## 240819_two stage 파일에서 생산한 객체 사용.
## model_0 <- mixmeta(coef_temp ~ 1,vcov_temp, data=SiDo_landcover_final)
## SiDoblup <- blup(model_0, vcov=T)

model_0$coefficients
model_0$vcov
SiDoblup

X00_sgg_info_bh_revised <- read_excel("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/00_sgg_info_bh_revised.xlsx")

########################## SiDoblup -> SiGunGublup

# 새로운 리스트 생성
SiGunGublup <- list()

for (i in 1:nrow(X00_sgg_info_bh_revised)) {
  sgg <- as.character(X00_sgg_info_bh_revised$sgg_b[i])
  
  # sgg가 "36110"인 경우 sido를 "43"으로 강제 지정
  if (sgg == "36110") {
    sido <- "43"
  } else {
    sido <- as.character(X00_sgg_info_bh_revised$sido_b[i])
  }
  
  # 해당 sido가 SiDoblup에 존재하는 경우만 복사
  if (sido %in% names(SiDoblup)) {
    SiGunGublup[[sgg]] <- SiDoblup[[sido]]
  } else {
    warning(paste("sido", sido, "not found in SiDoblup for sgg", sgg))
  }
}

SiGunGublup[["43750"]]
SiDoblup[["43"]]

names(SiGunGublup)

# 1. 기준값: sgg_b 벡터 (문자형)
all_sgg_b <- as.character(X00_sgg_info_bh_revised$sgg_b)

# 2. 현재 리스트의 이름
existing_names <- names(SiGunGublup)

# 3. 누락된 sgg_b (기준에는 있는데 리스트에는 없는 것)
missing_in_list <- setdiff(all_sgg_b, existing_names)

# 4. 리스트에는 있는데 기준에는 없는 것 (있을 필요는 없지만 확인용)
extra_in_list <- setdiff(existing_names, all_sgg_b)

############################################################################

weekly_data_SiGun <- read_csv("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/data/Scrub typhus weekly/weekly_data_SiGun.csv", 
                              col_types = cols(...1 = col_skip()))

weekly_data_SiGun_cases <- weekly_data_SiGun %>% 
  dplyr::select(sgg_b, sido_b, year, week, weekly_total_cases)


# 1. 분할 매핑 정의 (missing -> 분할 대상 sgg_b 벡터)
sgg_mapping <- list(
  "4111_1" = c("41111", "41113", "41115", "41117"),
  "4113_1" = c("41131", "41133", "41135"),
  "4117_1" = c("41171", "41173"),
  "4127_1" = c("41271", "41273"),
  "4128_1" = c("41281", "41285", "41287"),
  "4146_1" = c("41461", "41463", "41465"),
  "4311_1" = c("43111", "43112", "43113", "43114"),
  "4413_1" = c("44131", "44133"),
  "4511_1" = c("45111", "45113"),
  "4711_1" = c("47111", "47113"),
  "4812_1" = c("48121", "48123", "48125", "48127", "48129"),
  "11_1"   = c("11110", "11140", "11170", "11200", "11215", "11230", "11260", "11290",
               "11305", "11320", "11350", "11380", "11410", "11440", "11470", "11500",
               "11530", "11545", "11560", "11590", "11620", "11650", "11680", "11710",
               "11740"),
  "26_1"   = c("26110", "26140", "26170", "26200", "26230", "26260", "26290",
               "26320", "26350", "26380", "26410", "26440", "26470", "26500", "26530",
               "26710"),
  "27_1"   = c("27110", "27140", "27170", "27200", "27230", "27260", "27290", "27710"),
  "28_1"   = c("28110", "28140", "28170", "28185", "28200", "28237", "28245", "28260", "28710", "28720"),
  "29_1"   = c("29110", "29140", "29155", "29170", "29200"),
  "30_1"   = c("30110", "30140", "30170", "30200", "30230"),
  "31_1"   = c("31110", "31140", "31170", "31200", "31710")
)

# 2. 변환된 데이터 저장용 리스트
expanded_rows <- list()

# 3. 매핑된 값마다 반복 처리
for (original in names(sgg_mapping)) {
  targets <- sgg_mapping[[original]]
  n <- length(targets)
  
  # 원본 행 필터링
  original_rows <- weekly_data_SiGun_cases %>% 
    filter(sgg_b == original)
  
  # 분할 대상이 없으면 skip
  if (nrow(original_rows) == 0) next
  
  # 각 대상 sgg_b로 복제
  for (new_sgg in targets) {
    expanded <- original_rows %>%
      mutate(
        sgg_b = new_sgg,
        weekly_total_cases = weekly_total_cases / n
      )
    expanded_rows[[length(expanded_rows) + 1]] <- expanded
  }
}

# 4. 하나의 데이터프레임으로 병합
expanded_data <- bind_rows(expanded_rows)

# 5. 기존 데이터에서 "_1"이 들어간 원본 제거
cleaned_data <- weekly_data_SiGun_cases %>%
  filter(!sgg_b %in% names(sgg_mapping))

# 6. 합치기
weekly_data_SiGun_cases_updated <- bind_rows(cleaned_data, expanded_data)

# 1. "47730"의 행 필터링
source_rows <- weekly_data_SiGun_cases_updated %>%
  filter(sgg_b == "47730")

# 2. sgg_b만 "47720"으로 변경
new_rows <- source_rows %>%
  mutate(sgg_b = "47720")

weekly_data_SiGun_cases_updated <- bind_rows(weekly_data_SiGun_cases_updated, new_rows)


weekly_data_SiGun_cases %>% 
  filter(sgg_b == "11_1")

weekly_data_SiGun_cases_updated %>% 
  filter(sgg_b == "11110")


######################

# 1. 기준: SiGunGublup 리스트 이름
existing_names <- names(SiGunGublup)

# 2. 주간 데이터에 있는 고유 sgg_b 추출 (문자형으로)
all_sgg_in_cases <- unique(as.character(weekly_data_SiGun_cases_updated$sgg_b))

# 3. 누락된 sgg_b: 주간 데이터에는 있지만 리스트에는 없는 것
missing_in_SiGunGublup <- setdiff(all_sgg_in_cases, existing_names)

# 4. 리스트에는 있는데 주간 데이터에는 없는 sgg_b
extra_in_SiGunGublup <- setdiff(existing_names, all_sgg_in_cases)


save(SiGunGublup, model_0, weekly_data_SiGun_cases_updated,
     file = "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\보고서용 결과\\blup_cases.RData")


###################################
load("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/기후변화 데이터/tmean_scn_ssp_cal/tmean_scn_245_cal.RData")
load("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/기후변화 데이터/tmean_scn_ssp_cal/tmean_scn_370_cal.RData")
load("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/기후변화 데이터/tmean_scn_ssp_cal/tmean_scn_585_cal.RData")



for (gcm in names(tmean_scn_585_cal)) {
  # 현재 GCM의 하위 리스트 이름 확인
  sgg_names <- names(tmean_scn_585_cal[[gcm]])
  
  # "28177"이 있으면 이름을 "28170"으로 변경
  if ("28177" %in% sgg_names) {
    tmean_scn_585_cal[[gcm]][["28170"]] <- tmean_scn_585_cal[[gcm]][["28177"]]
    tmean_scn_585_cal[[gcm]][["28177"]] <- NULL
  }
}


# 1. 기준: SiGunGublup 리스트 이름
existing_names <- names(SiGunGublup)

ssp <- names(tmean_scn_585_cal$CanESM5)

# 3. 누락된 sgg_b: 주간 데이터에는 있지만 리스트에는 없는 것
missing_in_SiGunGublup <- setdiff(ssp, existing_names)

# 4. 리스트에는 있는데 주간 데이터에는 없는 sgg_b
extra_in_SiGunGublup <- setdiff(existing_names, ssp)

#####################################################
######## weekly 변환하고 52주만 남기기.
tmean_scn_585_cal$CanESM5$`11110`

tmean_scn_370_weekly <- list()

# 모든 GCM 모델에 대해 반복
for (gcm in names(tmean_scn_370_cal)) {
  
  # 하위 리스트 저장소 초기화
  gcm_list <- list()
  
  for (sgg in names(tmean_scn_370_cal[[gcm]])) {
    
    df <- tmean_scn_370_cal[[gcm]][[sgg]]
    
    # 날짜 파싱, 연도 및 주차 계산
    df <- df %>%
      mutate(
        date = as.Date(date),
        year = year(date),
        week = isoweek(date)  # ISO 기준 주차
      ) %>%
      # 주별 평균 기온 계산
      group_by(sgg_b, year, week) %>%
      summarise(weekly_tmean = mean(tmean, na.rm = TRUE), .groups = "drop") %>%
      # 주 53 제외
      filter(week <= 52)
    
    gcm_list[[sgg]] <- df
  }
  
  tmean_scn_370_weekly[[gcm]] <- gcm_list
}

####################################################################
######################### 데이터 구조 변환
######################## list[GCM][sido] -> list[sido] → data.frame(year, week, GCM1, GCM2, ..., GCMn)


##### sigungu 별 데이터로 변환.
combine_sigungu_data <- function(data_list) {
  # 결과를 저장할 리스트 생성
  result_list <- list()
  
  # 모든 GCM에서 첫 번째 시도의 이름을 가져옴 (모든 GCM에 동일한 시도들이 있다고 가정)
  gcm_names <- names(data_list)
  sigungu_names <- names(data_list[[1]])
  
  # 시도별로 반복 처리
  for (sigungu_name in sigungu_names) {
    # 시도별로 데이터를 저장할 빈 리스트 생성
    sigungu_data_list <- list()
    
    # 각 GCM별로 해당 시도 데이터를 추출하여 리스트에 저장
    for (gcm in gcm_names) {
      gcm_data <- data_list[[gcm]][[sigungu_name]]
      
      # GCM 이름을 열 이름으로 추가
      if (!is.null(gcm_data)) {
        gcm_data <- gcm_data %>%
          dplyr::select(year, week, weekly_tmean) %>%
          rename(!!gcm := weekly_tmean)
        sigungu_data_list[[gcm]] <- gcm_data
      }
    }
    
    # 시도별 데이터프레임을 결합
    combined_sigungu_data <- reduce(sigungu_data_list, full_join, by = c("year", "week"))
    
    # 결과 리스트에 저장
    result_list[[sigungu_name]] <- combined_sigungu_data
  }
  
  return(result_list)
}


# 데이터 처리 실행

sigungu_combined_data_SSP245 <- combine_sigungu_data(tmean_scn_245_weekly)
sigungu_combined_data_SSP370 <- combine_sigungu_data(tmean_scn_370_weekly)
sigungu_combined_data_SSP585 <- combine_sigungu_data(tmean_scn_585_weekly)

b <- sigungu_combined_data_SSP245[["41135"]]





save(sigungu_combined_data_SSP585,
     file = "C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/기후변화 데이터/tmean_scn_ssp_cal/updated/sigungu_combined_data_SSP585.RData")


