################ 250730 AR and projeciton 보고서용


library(data.table) # HANDLE LARGE DATASETS
library(readr) ; library(purrr)
library(dlnm) ; library(gnm) ; library(splines) # MODELLING TOOLS
library(sf) ; library(terra) # HANDLE SPATIAL DATA
library(exactextractr) # FAST EXTRACTION OF AREA-WEIGHTED RASTER CELLS
library(dplyr) ; library(tidyr) # DATA MANAGEMENT TOOLS
library(ggplot2) ; library(patchwork) # PLOTTING TOOLS
library(tidyterra) ; library(zoo)
library(EpiEstim); library(tidyverse) ;library(readxl) ; library(MASS) ; library(mixmeta);
library(tsModel)


# LOAD THE FUNCTION attrdl
# (READ THE ACCOMPANYING PDF FOR DOCUMENTATION)
source("C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\R-code\\attrdl.R")

load("C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\보고서용 결과\\blup_cases.RData")

load("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/기후변화 데이터/tmean_scn_ssp_cal/updated/sigungu_combined_data_SSP245.RData")
load("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/기후변화 데이터/tmean_scn_ssp_cal/updated/sigungu_combined_data_SSP370.RData")
load("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/기후변화 데이터/tmean_scn_ssp_cal/updated/sigungu_combined_data_SSP585.RData")

tmean_scn_245_weekly


########################## AR projection 하는 코드. 
############# 기본적인 객체들 먼저 생성해두기.
########## 시도별 계산을 먼저 진행. 
weekly_data_SiDo <- read.csv("C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\Scrub typhus weekly/weekly_data_SiDo.csv")

# weekly_data_SiDO 데이터프레임을 sido_b 컬럼을 기준으로 분할
list_of_dfs <- split(weekly_data_SiDo, weekly_data_SiDo$sido_b)

# 리스트의 이름 확인
names(list_of_dfs)

list_of_cbtmean <- lapply(list_of_dfs, function(df) { 
  pct_temp <- quantile(df$weekly_tmean, prob=c(.01, .10, .33, .66, .90, .99), na.rm=TRUE)
  varknot_temp <- pct_temp[c(3, 4)]  # knots at 33rd and 66th
  
  pct_temp_lag_2_9 <- quantile(2:9, prob=c(.01,.10,.33,.50,.66,.90,.99),na.rm=T)
  varknot_temp_lag_2_9 <- pct_temp_lag_2_9[c(3, 4)] # knots at 33rd and 66th
  
  argvar_temp <- list(fun="ns", knots=varknot_temp)
  arglag_temp_2_9 <- list(fun="ns", knots=varknot_temp_lag_2_9)
  
  cbtmean_2_9 <- crossbasis(df$weekly_tmean, lag= c(2,9), argvar=argvar_temp, arglag=arglag_temp_2_9)
  
  return(cbtmean_2_9)
})

names(list_of_cbtmean) <- names(list_of_dfs)

############# 전국 단위 CI 때문에 전국 단위는 별도로 계산해야 함.
weekly_whole_country <- weekly_data_SiDo %>%
  group_by( year, week) %>%
  # 요약 계산
  summarise(
    weekly_total_zero = sum(weekly_total_zero, na.rm = TRUE),
    weekly_total_ten = sum(weekly_total_ten, na.rm = TRUE),
    weekly_total_twenty = sum(weekly_total_twenty, na.rm = TRUE),
    weekly_total_thirty = sum(weekly_total_thirty, na.rm = TRUE),
    weekly_total_fourty = sum(weekly_total_fourty, na.rm = TRUE),
    weekly_total_fifty = sum(weekly_total_fifty, na.rm = TRUE),
    weekly_total_sixty = sum(weekly_total_sixty, na.rm = TRUE),
    weekly_total_seventy = sum(weekly_total_seventy, na.rm = TRUE),
    weekly_total_cases = sum(weekly_total_cases, na.rm = TRUE),
    across(!c(weekly_total_zero, weekly_total_ten, weekly_total_twenty,
              weekly_total_thirty, weekly_total_fourty, weekly_total_fifty,
              weekly_total_sixty, weekly_total_seventy, weekly_total_cases),
           mean, na.rm = TRUE), # 합을 계산한 변수들을 제외하고 나머지 숫자형 변수에 대해 평균 계산
    .groups = "drop"  # 그룹화 해제
  ) %>%
  dplyr::select(-sido_b)




############ For loop로 모든 지역들에 대해서 계산.


# (1) DIMENSION - 10-YEAR PERIOD 
#  *LABEL THE HISTORICAL PERIOD
histperiod <- "2000-2009"

# 2010년부터 2099년까지 10년 단위로 생성
projperiod <- paste(seq(2010, 2090, by = 10), "-", seq(2019, 2099, by = 10), sep = "")

##  "2040-2049" "2070-2079" "2090-2099" 이렇게 세 조각으로 value 산출하면 될듯.

#  *DEFINE SEQUENCE OF PERIODS FOR THE PREDICTIONS (HISTORICAL & PROJECTED)
histseqperiod <- factor(rep(histperiod,length.out=52*length(seq(2000,2009))))
projseqperiod <- factor(rep(projperiod,each=52*10))
seqperiod <- factor(c(as.numeric(histseqperiod)-1,as.numeric(projseqperiod)))

# 기존 seqperiod에 추가하고 다시 factor로 설정
seqperiod <- factor(c(as.character(seqperiod)),
                    levels = levels(seqperiod))

levels(seqperiod) <- c(histperiod,projperiod)

length(seqperiod)

# (2) DIMENSION - RANGE OF TEMPERATURES
temprange <- c("tot")

# (3) DIMENSION - ABSOLUTE AN/DIFFERENCE IN AN
absrel <- c("abs","rel")

# (4) DIMENSION - GENERAL CIRCULATION MODELS
# *LIST OF GCMs 

gcm <- c("ACCESS-CM2" = "ACCESS-CM2",
         "CanESM5"  = "CanESM5", 
         "CESM2" = "CESM2", 
         "CMCC-ESM2"  = "CMCC-ESM2",
         "CNRM-CM6-1" = "CNRM-CM6-1",
         "CNRM-ESM2-1"  = "CNRM-ESM2-1",
         "INM-CM4-8"  = "INM-CM4-8",
         "KACE-1-0-G" = "KACE-1-0-G",
         "MIROC-ES2L" = "MIROC-ES2L",
         "MPI-ESM1-2-LR" = "MPI-ESM1-2-LR",
         "EC-Earth3" = "EC-Earth3",
         "FGOALS-g3" = "FGOALS-g3",
         "INM-CM5-0"  = "INM-CM5-0",
         "UKESM1-0-LL"  = "UKESM1-0-LL")

SSP <- c(SSP245="sigungu_combined_data_SSP245", SSP370="sigungu_combined_data_SSP370", SSP585="sigungu_combined_data_SSP585")


# (6) DIMENSION - NUMBER OF ITERATION IN THE MONTE-CARLO SIMULATION 
nsim <- 2

# DEFINE THE ARRAY
ansim <- array(NA,dim=c(length(levels(seqperiod)),length(temprange),length(absrel),
                        length(gcm),length(SSP),nsim+1), 
               dimnames=list(levels(seqperiod),temprange ,absrel,
                             names(gcm),names(SSP), c("est",paste0("sim",seq(nsim)))))

length(ansim)
dim(ansim)

# (7) centering value 고정.
cen <- 10

# (8) Cases projection

list_of_dfs <- split(weekly_data_SiGun_cases_updated, weekly_data_SiGun_cases_updated$sgg_b)

# 새로운 리스트 생성
cases_by_week_total_proj_list <- list()

# list_of_dfs의 각 요소에 대해 반복 실행
for (sgg_b in names(list_of_dfs)) {
  
  df <- list_of_dfs[[sgg_b]]
  
  # 1. 53주는 제외하고, 연도에 관계없이 week별 평균 계산
  weekly_avg <- df %>%
    filter(week <= 52) %>%
    group_by(week) %>%
    summarise(mean_cases = mean(weekly_total_cases, na.rm = TRUE)) %>%
    arrange(week)
  
  # 2. 주차별 평균 벡터 추출 (길이 52)
  avg_weekly_vector <- weekly_avg$mean_cases
  
  # 3. 100년간 반복 (52주 × 100년 = 5200)
  repeated_vector <- rep(avg_weekly_vector, times = 100)
  
  # 4. 리스트에 저장
  cases_by_week_total_proj_list[[sgg_b]] <- repeated_vector
}
names(cases_by_week_total_proj_list)
cases_by_week_total_proj_list[["48740"]]



# 결과를 저장할 리스트 생성
anabs_list <- list()
anrel_list <- list()
afabs_list <- list()
afrel_list <- list()


#### sigungu_combined_data_SSP245 리스트의 각 요소에 대해 반복 실행
for (administrative in names(sigungu_combined_data_SSP245)) {
  cat("\n\n",names(sigungu_combined_data_SSP245[administrative]),"\n")
  
  # 각 시도의 데이터를 가져오기
  total_SSP245_tas <- sigungu_combined_data_SSP245[[administrative]]
  total_SSP370_tas <- sigungu_combined_data_SSP370[[administrative]]
  total_SSP585_tas <- sigungu_combined_data_SSP585[[administrative]]
  cases_by_week_total_proj <- cases_by_week_total_proj_list[[sgg_b]]
  blup <-  SiGunGublup[[administrative]]
  casesperiod_1 <- sum(cases_by_week_total_proj[1:520])
  cen <- 10
  
  # (5) DIMENSION - SCENARIO DIMENSION
  #  *LIST OF REPRESENTATIVE CONCENTRATION PATHWAYS SCENARIOS 
  SSP <- c(SSP245="total_SSP245_tas", SSP370="total_SSP370_tas", SSP585="total_SSP585_tas")
  
  # DEFINE THE ARRAY
  ansim <- array(NA,dim=c(length(levels(seqperiod)),length(temprange),length(absrel),
                          length(gcm),length(SSP),nsim+1), 
                 dimnames=list(levels(seqperiod),temprange ,absrel,
                               names(gcm),names(SSP), c("est",paste0("sim",seq(nsim)))))
  
  # RUN LOOP PER SSP
  for (i in seq(SSP)) {
    
    # PRINT
    cat("\n\n",names(SSP)[i],"\n")
    
    # SELECTION OF THE PROJECTED TEMPERATURE SERIES FOR A SPECIFIC SSP SCENARIO
    tmeanproj <- as.data.frame(get(SSP[[i]]))
    # RUN LOOP PER GCM
    for(j in seq(gcm)) {
      
      # PRINT
      cat(gcm[j],"")
      
      # (4) EXTRAPOLATION OF THE CURVE: 
      # - DERIVE THE CENTERED BASIS USING THE PROJECTED TEMPERATURE SERIES
      #   AND EXTRACT PARAMETERS
      pct_temp <- quantile(weekly_whole_country$weekly_tmean, prob=c(.01, .10, .33, .66, .90, .99), na.rm=TRUE)
      varknot_temp <- pct_temp[c(3, 4)]  # knots at 33rd and 66th
      
      pct_temp_lag_2_9 <- quantile(2:9, prob=c(.01,.10,.33,.50,.66,.90,.99),na.rm=T)
      varknot_temp_lag_2_9 <- pct_temp_lag_2_9[c(3, 4)] # knots at 33rd and 66th
      
      ### Bound 설정을 안 해줘서 error 났었음.
      argvar_temp <- list(fun="ns", knots=varknot_temp, Bound=range(weekly_whole_country$weekly_tmean,na.rm=T))
      arglag_temp_2_9 <- list(fun="ns", knots=varknot_temp_lag_2_9)
      
      cbtmean_2_9 <- crossbasis(tmeanproj[,j+2], lag= c(2,9), argvar=argvar_temp, arglag=arglag_temp_2_9)
      
      
      # (5) IMPACT PROJECTIONS:
      # - COMPUTE THE weekly CONTRIBUTIONS OF ATTRIBUTABLE DEATHS
      an <- attrdl(tmeanproj[,j+2], cbtmean_2_9, cases_by_week_total_proj, 
                   coef = blup$blup, vcov = blup$vcov, cen = 10 , dir="forw", type="an", tot = FALSE)
      
      # - SUM AN (ABS) BY TEMPERATURE RANGE AND PERIOD, STORE IN ARRAY BEFORE THE ITERATIONS
      # NB: ACCOUNT FOR NO TEMPERATURE BELOW/ABOVE CEN FOR GIVEN PERIODS
      ansim[,"tot","abs",j,i,1] <- tapply(an, seqperiod, sum, na.rm = TRUE)  # 전체 합
      
      # (6) ESTIMATE UNCERTAINTY OF THE PROJECTED AN:
      # - SAMPLE COEF ASSUMING A MULTIVARIATE NORMAL DISTRIBUTION
      set.seed(13041975+j)
      coefsim <- mvrnorm(nsim,blup$blup, blup$vcov)
      
      # - LOOP ACROSS ITERATIONS
      for(s in seq(nsim)) {
        
        # COMPUTE THE DAILY CONTRIBUTIONS OF ATTRIBUTABLE DEATHS
        an <- attrdl(tmeanproj[,j+2], cbtmean_2_9, cases_by_week_total_proj, 
                     coef = coefsim[s,], vcov = blup$vcov, cen = 10 , dir="forw", type="an", tot = FALSE)
        
        # STORE THE ATTRIBUTABLE MORTALITY
        ansim[,"tot","abs",j,i,s+1] <- tapply(an,seqperiod,sum,na.rm = TRUE)
      }
    }
  }
  ####################
  ansim[,,"rel",,,] <- ansim[,,"abs",,,] - ansim[rep("2000-2009",length(levels(seqperiod))),,"abs",,,]
  
  ########## COMPUTE AN/AF (95%CI) IN THE ENSEMBLE, BY RANGE & PERIOD & RCP
  
  estci <- c("est","ci.l","ci.u")
  
  anabs <- afabs <- anrel <- afrel <- array(NA,dim=c(length(levels(seqperiod)),
                                                     length(estci),length(temprange),length(SSP)), 
                                            dimnames=list(levels(seqperiod),estci,temprange,names(SSP)))
  
  # ATTRIBUTABLE NUMBERS 
  # ABSOLUTE
  anabs[,"est",,"SSP245"] <- apply(ansim[,,"abs",,"SSP245",1],c(1),mean)
  anabs[,"ci.l",,"SSP245"] <- apply(ansim[,,"abs",,"SSP245",-1],c(1),quantile,0.025, na.rm = TRUE)
  anabs[,"ci.u",,"SSP245"] <- apply(ansim[,,"abs",,"SSP245",-1],c(1),quantile,0.975, na.rm = TRUE)
  
  anabs[,"est",,"SSP370"] <- apply(ansim[,,"abs",,"SSP370",1],c(1),mean)
  anabs[,"ci.l",,"SSP370"] <- apply(ansim[,,"abs",,"SSP370",-1],c(1),quantile,0.025, na.rm = TRUE)
  anabs[,"ci.u",,"SSP370"] <- apply(ansim[,,"abs",,"SSP370",-1],c(1),quantile,0.975, na.rm = TRUE)
  
  anabs[,"est",,"SSP585"] <- apply(ansim[,,"abs",,"SSP585",1],c(1),mean)
  anabs[,"ci.l",,"SSP585"] <- apply(ansim[,,"abs",,"SSP585",-1],c(1),quantile,0.025, na.rm = TRUE)
  anabs[,"ci.u",,"SSP585"] <- apply(ansim[,,"abs",,"SSP585",-1],c(1),quantile,0.975, na.rm = TRUE)
  
  # RELATIVE
  anrel[,"est",,"SSP245"] <- apply(ansim[,,"rel",,"SSP245",1],c(1),mean)
  anrel[,"ci.l",,"SSP245"] <- apply(ansim[,,"rel",,"SSP245",-1],c(1),quantile,0.025, na.rm = TRUE)
  anrel[,"ci.u",,"SSP245"] <- apply(ansim[,,"rel",,"SSP245",-1],c(1),quantile,0.975, na.rm = TRUE)
  
  anrel[,"est",,"SSP370"] <- apply(ansim[,,"rel",,"SSP370",1],c(1),mean)
  anrel[,"ci.l",,"SSP370"] <- apply(ansim[,,"rel",,"SSP370",-1],c(1),quantile,0.025, na.rm = TRUE)
  anrel[,"ci.u",,"SSP370"] <- apply(ansim[,,"rel",,"SSP370",-1],c(1),quantile,0.975, na.rm = TRUE)
  
  anrel[,"est",,"SSP585"] <- apply(ansim[,,"rel",,"SSP585",1],c(1),mean)
  anrel[,"ci.l",,"SSP585"] <- apply(ansim[,,"rel",,"SSP585",-1],c(1),quantile,0.025, na.rm = TRUE)
  anrel[,"ci.u",,"SSP585"] <- apply(ansim[,,"rel",,"SSP585",-1],c(1),quantile,0.975, na.rm = TRUE)
  
  # ATTRIBUTABLE FRACTION
  afabs[,,,] <- anabs[,,,]/casesperiod_1*100
  afrel[,,,] <- anrel[,,,]/casesperiod_1*100
  
  # 결과를 리스트에 저장 (각 시도별로)
  anabs_list[[administrative]] <- anabs
  anrel_list[[administrative]] <- anrel
  afabs_list[[administrative]] <- afabs
  afrel_list[[administrative]] <- afrel
  
}

casesperiod_1

sum(cases_by_week_list[[2]][1:260])

names(anabs_list)
anrel_list[[5]]
anabs_list[[9]] 

anabs_list
save(anabs_list, anrel_list, afabs_list, afrel_list,
     file = "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\보고서용 결과\\Ansim_1000.RData")



















load("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/기후변화 데이터/tmean_scn_ssp_cal/updated/sigungu_combined_data_SSP245.RData")
load("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/기후변화 데이터/tmean_scn_ssp_cal/updated/sigungu_combined_data_SSP370.RData")
load("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/기후변화 데이터/tmean_scn_ssp_cal/updated/sigungu_combined_data_SSP585.RData")


# 먼저 리스트를 데이터프레임으로 결합하면서 id 컬럼 추가
combined_df <- bind_rows(sigungu_combined_data_SSP245, .id = "region")
unique(combined_df$region)
# 열 이름 확인
cols_to_average <- setdiff(colnames(combined_df), c("region", "year", "week"))

# year, week를 기준으로 그룹화하고 나머지 열 평균 계산
total_SSP245 <- combined_df %>%
  group_by(year, week) %>%
  summarise(
    across(all_of(cols_to_average), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )


# 먼저 리스트를 데이터프레임으로 결합하면서 id 컬럼 추가
combined_df <- bind_rows(sigungu_combined_data_SSP370, .id = "region")

# 열 이름 확인
cols_to_average <- setdiff(colnames(combined_df), c("region", "year", "week"))

# year, week를 기준으로 그룹화하고 나머지 열 평균 계산
total_SSP370 <- combined_df %>%
  group_by(year, week) %>%
  summarise(
    across(all_of(cols_to_average), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )


# 먼저 리스트를 데이터프레임으로 결합하면서 id 컬럼 추가
combined_df <- bind_rows(sigungu_combined_data_SSP585, .id = "region")

# 열 이름 확인
cols_to_average <- setdiff(colnames(combined_df), c("region", "year", "week"))

# year, week를 기준으로 그룹화하고 나머지 열 평균 계산
total_SSP585 <- combined_df %>%
  group_by(year, week) %>%
  summarise(
    across(all_of(cols_to_average), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

total_SSP585_mean_2000_2020 <- total_SSP585 %>%
  filter(year >= 2000, year <= 2020) %>%
  mutate(
    tmean_ens = rowMeans(
      as.matrix(select(., -year, -week)),
      na.rm = TRUE
    )
  ) %>%
  select(year, week, tmean_ens)

total_SSP585_mean_2000_2020

########################## AR projection 하는 코드. 
############# 기본적인 객체들 먼저 생성해두기.
########## 시도별 계산을 먼저 진행. 
weekly_data_SiDo <- read.csv("C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\Scrub typhus weekly/weekly_data_SiDo.csv")

# weekly_data_SiDO 데이터프레임을 sido_b 컬럼을 기준으로 분할
list_of_dfs <- split(weekly_data_SiDo, weekly_data_SiDo$sido_b)

# 리스트의 이름 확인
names(list_of_dfs)

list_of_cbtmean <- lapply(list_of_dfs, function(df) { 
  pct_temp <- quantile(df$weekly_tmean, prob=c(.01, .10, .33, .66, .90, .99), na.rm=TRUE)
  varknot_temp <- pct_temp[c(3, 4)]  # knots at 33rd and 66th
  
  pct_temp_lag_2_9 <- quantile(2:9, prob=c(.01,.10,.33,.50,.66,.90,.99),na.rm=T)
  varknot_temp_lag_2_9 <- pct_temp_lag_2_9[c(3, 4)] # knots at 33rd and 66th
  
  argvar_temp <- list(fun="ns", knots=varknot_temp)
  arglag_temp_2_9 <- list(fun="ns", knots=varknot_temp_lag_2_9)
  
  cbtmean_2_9 <- crossbasis(df$weekly_tmean, lag= c(2,9), argvar=argvar_temp, arglag=arglag_temp_2_9)
  
  return(cbtmean_2_9)
})

names(list_of_cbtmean) <- names(list_of_dfs)

############# 전국 단위 CI 때문에 전국 단위는 별도로 계산해야 함.
weekly_whole_country <- weekly_data_SiDo %>%
  group_by( year, week) %>%
  # 요약 계산
  summarise(
    weekly_total_zero = sum(weekly_total_zero, na.rm = TRUE),
    weekly_total_ten = sum(weekly_total_ten, na.rm = TRUE),
    weekly_total_twenty = sum(weekly_total_twenty, na.rm = TRUE),
    weekly_total_thirty = sum(weekly_total_thirty, na.rm = TRUE),
    weekly_total_fourty = sum(weekly_total_fourty, na.rm = TRUE),
    weekly_total_fifty = sum(weekly_total_fifty, na.rm = TRUE),
    weekly_total_sixty = sum(weekly_total_sixty, na.rm = TRUE),
    weekly_total_seventy = sum(weekly_total_seventy, na.rm = TRUE),
    weekly_total_cases = sum(weekly_total_cases, na.rm = TRUE),
    across(!c(weekly_total_zero, weekly_total_ten, weekly_total_twenty,
              weekly_total_thirty, weekly_total_fourty, weekly_total_fifty,
              weekly_total_sixty, weekly_total_seventy, weekly_total_cases),
           mean, na.rm = TRUE), # 합을 계산한 변수들을 제외하고 나머지 숫자형 변수에 대해 평균 계산
    .groups = "drop"  # 그룹화 해제
  ) %>%
  dplyr::select(-sido_b)

################################################

#### Historical cases의 trend가 미래에도 유지될 것을 가정하고, case series를 동일하게 projection.
weekly_whole_country

weekly_data_SiDo 

cases_by_week_total <- tapply(weekly_whole_country$weekly_total_cases, 
                              weekly_whole_country$week, 
                              mean, na.rm = TRUE)
cases_by_week_total_proj <- rep(cases_by_week_total,length=nrow(total_SSP245))




################################################################################
######################
##### Projection, quantification of ARs / 기존 코드 수정해서 attrdl 사용하는 코드로 변환하여 사용.
##### 기본 객체 셋업.

# (1) DIMENSION - 10-YEAR PERIOD 
#  *LABEL THE HISTORICAL PERIOD
histperiod <- "2000-2009"

# 2010년부터 2099년까지 10년 단위로 생성
projperiod <- paste(seq(2010, 2090, by = 10), "-", seq(2010, 2090, by = 10) + 9, sep = "")



#  *DEFINE SEQUENCE OF PERIODS FOR THE PREDICTIONS (HISTORICAL & PROJECTED)
histseqperiod <- factor(rep(histperiod,length.out=52*length(seq(2000,2009))))
projseqperiod <- factor(rep(projperiod,each=52*10))
seqperiod <- factor(c(as.numeric(histseqperiod)-1,as.numeric(projseqperiod)))

levels(seqperiod) <- c(histperiod,projperiod)

length(seqperiod)

# (2) DIMENSION - RANGE OF TEMPERATURES
temprange <- c("tot")

# (3) DIMENSION - ABSOLUTE AN/DIFFERENCE IN AN
absrel <- c("abs","rel")

# (4) DIMENSION - GENERAL CIRCULATION MODELS
# *LIST OF GCMs 

gcm <- c("ACCESS-CM2" = "ACCESS-CM2",
         "CanESM5"  = "CanESM5", 
         "CESM2" = "CESM2", 
         "CMCC-ESM2"  = "CMCC-ESM2",
         "CNRM-CM6-1" = "CNRM-CM6-1",
         "CNRM-ESM2-1"  = "CNRM-ESM2-1",
         "INM-CM4-8"  = "INM-CM4-8",
         "KACE-1-0-G" = "KACE-1-0-G",
         "MIROC-ES2L" = "MIROC-ES2L",
         "MPI-ESM1-2-LR" = "MPI-ESM1-2-LR",
         "EC-Earth3" = "EC-Earth3",
         "FGOALS-g3" = "FGOALS-g3",
         "INM-CM5-0"  = "INM-CM5-0",
         "UKESM1-0-LL"  = "UKESM1-0-LL")

# (5) DIMENSION - SCENARIO DIMENSION
#  *LIST OF REPRESENTATIVE CONCENTRATION PATHWAYS SCENARIOS 
SSP <- c(SSP245="total_SSP245", SSP370="total_SSP370", SSP585="total_SSP585")

# (6) DIMENSION - NUMBER OF ITERATION IN THE MONTE-CARLO SIMULATION 
nsim <- 1000

# DEFINE THE ARRAY
ansim <- array(NA,dim=c(length(levels(seqperiod)),length(temprange),length(absrel),
                        length(gcm),length(SSP),nsim+1), 
               dimnames=list(levels(seqperiod),temprange ,absrel,
                             names(gcm),names(SSP), c("est",paste0("sim",seq(nsim)))))

length(ansim)
dim(ansim)

# (7) centering value 고정.
cen <- 10

## (8) whole country 기본값은 앞서 계산한 데이터프레임에서 다시 계산함.
## 
pct_temp <- quantile(weekly_whole_country$weekly_tmean, prob=c(.01, .10, .33, .66, .90, .99), na.rm=TRUE)
varknot_temp <- pct_temp[c(3, 4)]  # knots at 33rd and 66th

pct_temp_lag_2_9 <- quantile(2:9, prob=c(.01,.10,.33,.50,.66,.90,.99),na.rm=T)
varknot_temp_lag_2_9 <- pct_temp_lag_2_9[c(3, 4)] # knots at 33rd and 66th

### Bound 설정을 안 해줘서 error 났었음.
argvar_temp <- list(fun="ns", knots=varknot_temp, Bound=range(weekly_whole_country$weekly_tmean,na.rm=T))
arglag_temp_2_9 <- list(fun="ns", knots=varknot_temp_lag_2_9)

cbtmean_2_9 <- crossbasis(weekly_whole_country$weekly_tmean, lag= c(2,9), argvar=argvar_temp, arglag=arglag_temp_2_9)


## model_0 <- mixmeta(coef_temp ~ 1,vcov_temp, data=SiDo_landcover_final)
coef <- model_0$coefficients
vcov <- model_0$vcov


#########################################
# RUN LOOP PER SSP
for (i in seq(SSP)) {
  
  # PRINT
  cat("\n\n",names(SSP)[i],"\n")
  
  # SELECTION OF THE PROJECTED TEMPERATURE SERIES FOR A SPECIFIC SSP SCENARIO
  tmeanproj <- get(SSP[[i]])
  
  # RUN LOOP PER GCM
  for(j in seq(gcm)) {
    
    # PRINT
    cat(gcm[j],"")
    
    # (4) EXTRAPOLATION OF THE CURVE: 
    # - DERIVE THE CENTERED BASIS USING THE PROJECTED TEMPERATURE SERIES
    #   AND EXTRACT PARAMETERS
    pct_temp <- quantile(weekly_whole_country$weekly_tmean, prob=c(.01, .10, .33, .66, .90, .99), na.rm=TRUE)
    varknot_temp <- pct_temp[c(3, 4)]  # knots at 33rd and 66th
    
    pct_temp_lag_2_9 <- quantile(2:9, prob=c(.01,.10,.33,.50,.66,.90,.99),na.rm=T)
    varknot_temp_lag_2_9 <- pct_temp_lag_2_9[c(3, 4)] # knots at 33rd and 66th
    
    ### Bound 설정을 안 해줘서 error 났었음.
    argvar_temp <- list(fun="ns", knots=varknot_temp, Bound=range(weekly_whole_country$weekly_tmean,na.rm=T))
    arglag_temp_2_9 <- list(fun="ns", knots=varknot_temp_lag_2_9)
    
    cbtmean_2_9 <- crossbasis(tmeanproj[,j+2], lag= c(2,9), argvar=argvar_temp, arglag=arglag_temp_2_9)
    

    # (5) IMPACT PROJECTIONS:
    # - COMPUTE THE weekly CONTRIBUTIONS OF ATTRIBUTABLE DEATHS
    an <- attrdl(tmeanproj[[j + 2]], cbtmean_2_9, cases_by_week_total_proj, 
                 coef = model_0$coefficients, vcov = model_0$vcov, cen = 10 , dir="forw", type="an", tot = FALSE)
    
    # - SUM AN (ABS) BY TEMPERATURE RANGE AND PERIOD, STORE IN ARRAY BEFORE THE ITERATIONS
    # NB: ACCOUNT FOR NO TEMPERATURE BELOW/ABOVE CEN FOR GIVEN PERIODS
    ansim[,"tot","abs",j,i,1] <- tapply(an, seqperiod, sum, na.rm = TRUE)  # 전체 합
    
    # (6) ESTIMATE UNCERTAINTY OF THE PROJECTED AN:
    # - SAMPLE COEF ASSUMING A MULTIVARIATE NORMAL DISTRIBUTION
    set.seed(13041975+j)
    coefsim <- mvrnorm(nsim,model_0$coefficients,model_0$vcov)
    
    # - LOOP ACROSS ITERATIONS
    for(s in seq(nsim)) {
      
      # COMPUTE THE DAILY CONTRIBUTIONS OF ATTRIBUTABLE DEATHS
      an <- attrdl(tmeanproj[[j + 2]], cbtmean_2_9, cases_by_week_total_proj, 
                   coef = coefsim[s,], vcov = model_0$vcov, cen = 10 , dir="forw", type="an", tot = FALSE)
      
      # STORE THE ATTRIBUTABLE MORTALITY
      ansim[,"tot","abs",j,i,s+1] <- tapply(an, seqperiod, sum, na.rm = TRUE)  # 전체 합
      
    }
  }
}

#### 2005년까지는 temperature 가 동일하게 붙어 있는데, 2006년부터 GCM model에 따라서 다르게 붙어 있으므로 2001-2005 합산값이 model마다 미세하게 다름.
ansim[, , , , "SSP245", "est"]
str(ansim)

ansim[,,"rel",,,] <- ansim[,,"abs",,,] - ansim[rep("2000-2009",length(levels(seqperiod))),,"abs",,,]


########## COMPUTE AN/AF (95%CI) IN THE ENSEMBLE, BY RANGE & PERIOD & RCP
estci <- c("est","ci.l","ci.u")

anabs <- afabs <- anrel <- afrel <- array(NA,dim=c(length(levels(seqperiod)),
                                                   length(estci),length(temprange),length(SSP)), 
                                          dimnames=list(levels(seqperiod),estci,temprange,names(SSP)))

# ATTRIBUTABLE NUMBERS 
# ABSOLUTE
anabs[,"est",,"SSP245"] <- apply(ansim[,,"abs",,"SSP245",1],c(1),mean)
anabs[,"ci.l",,"SSP245"] <-  apply(ansim[,,"abs",,"SSP245",-1], c(1),quantile,0.025)
anabs[,"ci.u",,"SSP245"] <- apply(ansim[,,"abs",,"SSP245",-1], c(1),quantile,0.975)

anabs[,"est",,"SSP370"] <- apply(ansim[,,"abs",,"SSP370",1],c(1),mean)
anabs[,"ci.l",,"SSP370"] <- apply(ansim[,,"abs",,"SSP370",-1],c(1),quantile,0.025)
anabs[,"ci.u",,"SSP370"] <- apply(ansim[,,"abs",,"SSP370",-1],c(1),quantile,0.975)

anabs[,"est",,"SSP585"] <- apply(ansim[,,"abs",,"SSP585",1],c(1),mean)
anabs[,"ci.l",,"SSP585"] <- apply(ansim[,,"abs",,"SSP585",-1],c(1),quantile,0.025)
anabs[,"ci.u",,"SSP585"] <- apply(ansim[,,"abs",,"SSP585",-1],c(1),quantile,0.975)

# RELATIVE
anrel[,"est",,"SSP245"] <- apply(ansim[,,"rel",,"SSP245",1],c(1),mean)
anrel[,"ci.l",,"SSP245"] <- apply(ansim[,,"rel",,"SSP245",-1],c(1),quantile,0.025)
anrel[,"ci.u",,"SSP245"] <- apply(ansim[,,"rel",,"SSP245",-1],c(1),quantile,0.975)

anrel[,"est",,"SSP370"] <- apply(ansim[,,"rel",,"SSP370",1],c(1),mean)
anrel[,"ci.l",,"SSP370"] <- apply(ansim[,,"rel",,"SSP370",-1],c(1),quantile,0.025)
anrel[,"ci.u",,"SSP370"] <- apply(ansim[,,"rel",,"SSP370",-1],c(1),quantile,0.975)

anrel[,"est",,"SSP585"] <- apply(ansim[,,"rel",,"SSP585",1],c(1),mean)
anrel[,"ci.l",,"SSP585"] <- apply(ansim[,,"rel",,"SSP585",-1],c(1),quantile,0.025)
anrel[,"ci.u",,"SSP585"] <- apply(ansim[,,"rel",,"SSP585",-1],c(1),quantile,0.975)

casesperiod <- sum(cases_by_week_total)*5

# ATTRIBUTABLE FRACTION
afabs[,,,] <- anabs[,,,]/casesperiod*100
afrel[,,,] <- anrel[,,,]/casesperiod*100

save(anabs, anrel, afabs, afrel, file = "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\보고서용 결과\\Ansim_wholecountry_1000.RData")








