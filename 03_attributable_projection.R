# Manuscript-aligned revision draft (2026-11-17) — NOT an independently validated run.
# Changes from supplied source: 1–9-week log-knotted lags, 5,000 Monte Carlo
# draws, matched 2016–2020 contrasts. Do not report output as original-run results
# until all study inputs, methods and numerical estimates have been reconciled.
# The full data are NOT included here and must be obtained from lawful custodians.

##################################################
########### AR projection updated
##################################################

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

## 240819_two stage 파일에서 생산한 객체 사용.
## model_0 <- mixmeta(coef_temp ~ 1,vcov_temp, data=SiDo_landcover_final)
## SiDoblup <- blup(model_0, vcov=T)

model_0$coefficients
model_0$vcov
SiDoblup



#############################################################
load("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/기후변화 데이터/기후변화 데이터 filtered/final/sido_combined_SSP245_final_update.RData")
load("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/기후변화 데이터/기후변화 데이터 filtered/final/sido_combined_SSP370_final_update.RData")
load("C:/Users/redwo/OneDrive/바탕 화면/Direction 과제/Scrub typhus/기후변화 데이터/기후변화 데이터 filtered/final/sido_combined_SSP585_final_update.RData")

total_SSP245 <- sido_combined_data_SSP245_final[["whole country"]]
total_SSP370 <- sido_combined_data_SSP370_final[["whole country"]]
total_SSP585 <- sido_combined_data_SSP585_final[["whole country"]]

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

  # 1–9 week lag range with log-spaced knot; same lag basis as stage 1.
  
  argvar_temp <- list(fun="ns", knots=varknot_temp)
  arglag_temp_1_9 <- list(fun="ns", knots=dlnm::logknots(c(1,9), nk=1))
  
  cbtmean_1_9 <- crossbasis(df$weekly_tmean, lag= c(1,9), argvar=argvar_temp, arglag=arglag_temp_1_9)
  
  return(cbtmean_1_9)
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

national_cases_by_week_total <- tapply(weekly_whole_country$weekly_total_cases, 
                              weekly_whole_country$week, 
                              mean, na.rm = TRUE)
national_cases_by_week_total_proj <- rep(national_cases_by_week_total,length=nrow(total_SSP245))


# Nationwide projections use only the nationwide weekly case series defined above.
# Regional series are prepared separately in the regional projection section below.


################################################################################
######################
##### Projection, quantification of ARs / 기존 코드 수정해서 attrdl 사용하는 코드로 변환하여 사용.
##### 기본 객체 셋업.

# (1) DIMENSION - 10-YEAR PERIOD 
#  *LABEL THE HISTORICAL PERIOD
# First five-year time block (do not confuse with the comparison reference).
histperiod <- "2001-2005"

# 2021년부터 2060년까지 5년 단위로 생성
projperiod <- paste(seq(2006, 2096, by = 5), "-", seq(2006, 2096, by = 5) + 4, sep = "")

##  "2031-2035" "2041-2045" "2051-2055" 이렇게 세 조각으로 value 산출하면 될듯.

#  *DEFINE SEQUENCE OF PERIODS FOR THE PREDICTIONS (HISTORICAL & PROJECTED)
histseqperiod <- factor(rep(histperiod,length.out=52*length(seq(2001,2005))))
projseqperiod <- factor(rep(projperiod,each=52*5))
seqperiod <- factor(c(as.numeric(histseqperiod)-1,as.numeric(projseqperiod)))
## projection data 는 2099년까지 있고, seqperiod 는 2100까지 있으므로 맨 뒤의 52개 weeks 를 제외함.
seqperiod <- head(seqperiod, -52)
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
nsim <- 5000

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

  # 1–9 week lag range with log-spaced knot; same lag basis as stage 1.

### Bound 설정을 안 해줘서 error 났었음.
argvar_temp <- list(fun="ns", knots=varknot_temp, Bound=range(weekly_whole_country$weekly_tmean,na.rm=T))
arglag_temp_1_9 <- list(fun="ns", knots=dlnm::logknots(c(1,9), nk=1))

cbtmean_1_9 <- crossbasis(weekly_whole_country$weekly_tmean, lag= c(1,9), argvar=argvar_temp, arglag=arglag_temp_1_9)


## model_0 <- mixmeta(coef_temp ~ 1,vcov_temp, data=SiDo_landcover_final)
coef <- model_0$coefficients
vcov <- model_0$vcov


# Reference for differences in temperature-attributable cases (manuscript baseline).
# Each GCM/scenario/draw will be differenced against its matched 2016–2020 value.
comparison_period <- "2016-2020"

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

  # 1–9 week lag range with log-spaced knot; same lag basis as stage 1.
    
    ### Bound 설정을 안 해줘서 error 났었음.
    argvar_temp <- list(fun="ns", knots=varknot_temp, Bound=range(weekly_whole_country$weekly_tmean,na.rm=T))
    arglag_temp_1_9 <- list(fun="ns", knots=dlnm::logknots(c(1,9), nk=1))
    
    cbtmean_1_9 <- crossbasis(tmeanproj[,j+2], lag= c(1,9), argvar=argvar_temp, arglag=arglag_temp_1_9)
    
    
    # (5) IMPACT PROJECTIONS:
    # - COMPUTE THE weekly CONTRIBUTIONS OF ATTRIBUTABLE DEATHS
    an <- attrdl(tmeanproj[,j+2], cbtmean_1_9, national_cases_by_week_total_proj, 
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
      an <- attrdl(tmeanproj[,j+2], cbtmean_1_9, national_cases_by_week_total_proj, 
                   coef = coefsim[s,], vcov = model_0$vcov, cen = 10 , dir="forw", type="an", tot = FALSE)
      
      # STORE THE ATTRIBUTABLE MORTALITY
      ansim[,"tot","abs",j,i,s+1] <- tapply(an, seqperiod, sum, na.rm = TRUE)  # 전체 합
      
    }
  }
}

#### Different GCMs may yield different values in the early time blocks.
#### The manuscript comparison/reference for anrel and afrel is 2016–2020.
ansim[, , , , "SSP245", "est"]
str(ansim)

# Paired changes: subtract the SAME GCM/scenario/simulation draw at 2016–2020.
# 2001–2005 is only the first chronological block, not the reference for anrel.
stopifnot(comparison_period %in% dimnames(ansim)[[1]])
ansim[,"tot","rel",,,] <- ansim[,"tot","abs",,,] -
  ansim[rep(comparison_period, length(levels(seqperiod))), "tot", "abs",,,]


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

# PAIRED DIFFERENCE relative to 2016–2020. The 2.5th/97.5th percentiles
# below are taken from the simulated differences within GCM/scenario/draw.
# They are not calculated by subtracting two separate confidence limits.
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

casesperiod <- sum(national_cases_by_week_total)*5

# ATTRIBUTABLE FRACTION
afabs[,,,] <- anabs[,,,]/casesperiod*100
afrel[,,,] <- anrel[,,,]/casesperiod*100

save(anabs, anrel, afabs, afrel, comparison_period, file = "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\AR_projection/AR_wholecountry_5000.RData")




################################################################################################################################
################################################################################################################################
################################################################################################################################

############ For loop로 모든 지역들에 대해서 계산.



# (1) DIMENSION - 10-YEAR PERIOD 
#  *LABEL THE HISTORICAL PERIOD
# First five-year time block (do not confuse with the comparison reference).
histperiod <- "2001-2005"

# 2021년부터 2060년까지 5년 단위로 생성
projperiod <- paste(seq(2006, 2096, by = 5), "-", seq(2006, 2096, by = 5) + 4, sep = "")

##  "2031-2035" "2041-2045" "2051-2055" 이렇게 세 조각으로 value 산출하면 될듯.

#  *DEFINE SEQUENCE OF PERIODS FOR THE PREDICTIONS (HISTORICAL & PROJECTED)
histseqperiod <- factor(rep(histperiod,length.out=52*length(seq(2001,2005))))
projseqperiod <- factor(rep(projperiod,each=52*5))
seqperiod <- factor(c(as.numeric(histseqperiod)-1,as.numeric(projseqperiod)))
## projection data 는 2099년까지 있고, seqperiod 는 2100까지 있으므로 맨 뒤의 52개 weeks 를 제외함.
seqperiod <- head(seqperiod, -52)
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
nsim <- 5000

# DEFINE THE ARRAY
ansim <- array(NA,dim=c(length(levels(seqperiod)),length(temprange),length(absrel),
                        length(gcm),length(SSP),nsim+1), 
               dimnames=list(levels(seqperiod),temprange ,absrel,
                             names(gcm),names(SSP), c("est",paste0("sim",seq(nsim)))))

length(ansim)
dim(ansim)

# (7) centering value 고정.
cen <- 10

### 이 객체들은 위쪽에서 계산.
list_of_dfs <- split(weekly_data_SiDo, weekly_data_SiDo$sido_b)
list_of_dfs <- append(list_of_dfs, list(`whole country` = weekly_whole_country))
names(list_of_dfs)

# 새로운 리스트 생성
cases_by_week_total_proj_list <- list()

# list_of_dfs의 각 요소에 대해 반복 실행
for (sido in names(list_of_dfs)) {
  
  # 현재 시도에 해당하는 데이터프레임 가져오기
  total_SSP245 <- list_of_dfs[[sido]]
  
  # weekly_whole_country 데이터에서 week별 평균을 계산
  cases_by_week_total <- tapply(total_SSP245$weekly_total_cases, 
                                total_SSP245$week, 
                                mean, na.rm = TRUE)
  
  # 평균 값을 total_SSP245의 행 수만큼 반복하여 확장
  cases_by_week_total_proj <- rep(cases_by_week_total, length = 5148)
  
  # 계산된 값을 새로운 리스트에 저장
  cases_by_week_total_proj_list[[sido]] <- cases_by_week_total_proj
}

cases_by_week_total_proj_list[["whole country"]]
cases_by_week_total_proj

# 'whole country'라는 이름의 요소 추가
SiDoblup[["whole country"]] <- list(
  blup = model_0$coefficients,  # model_0의 coefficients를 blup으로 추가
  vcov = model_0$vcov           # model_0의 vcov를 vcov으로 추가
)

names(SiDoblup)

# 결과를 저장할 리스트 생성
anabs_list <- list()
anrel_list <- list()
afabs_list <- list()
afrel_list <- list()


#### sido_combined_data_SSP245_final 리스트의 각 요소에 대해 반복 실행
for (sido in names(sido_combined_data_SSP245_final)) {
  cat("\n\n",names(sido_combined_data_SSP245_final[sido]),"\n")
  
  # 각 시도의 데이터를 가져오기
  total_SSP245 <- sido_combined_data_SSP245_final[[sido]]
  total_SSP370 <- sido_combined_data_SSP370_final[[sido]]
  total_SSP585 <- sido_combined_data_SSP585_final[[sido]]
  cases_by_week_total_proj <- cases_by_week_total_proj_list[[sido]]
  blup <- SiDoblup[[sido]]
  casesperiod <- sum(cases_by_week_total_proj[1:260])
  
  # (5) DIMENSION - SCENARIO DIMENSION
  #  *LIST OF REPRESENTATIVE CONCENTRATION PATHWAYS SCENARIOS 
  SSP <- c(SSP245="total_SSP245", SSP370="total_SSP370", SSP585="total_SSP585")
  
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

  # 1–9 week lag range with log-spaced knot; same lag basis as stage 1.
      
      ### Bound 설정을 안 해줘서 error 났었음.
      argvar_temp <- list(fun="ns", knots=varknot_temp, Bound=range(weekly_whole_country$weekly_tmean,na.rm=T))
      arglag_temp_1_9 <- list(fun="ns", knots=dlnm::logknots(c(1,9), nk=1))
      
      cbtmean_1_9 <- crossbasis(tmeanproj[,j+2], lag= c(1,9), argvar=argvar_temp, arglag=arglag_temp_1_9)
      
      
      # (5) IMPACT PROJECTIONS:
      # - COMPUTE THE weekly CONTRIBUTIONS OF ATTRIBUTABLE DEATHS
      an <- attrdl(tmeanproj[,j+2], cbtmean_1_9, cases_by_week_total_proj, 
                   coef = blup$blup, vcov = blup$vcov, cen = 10 , dir="forw", type="an", tot = FALSE)
      
      # - SUM AN (ABS) BY TEMPERATURE RANGE AND PERIOD, STORE IN ARRAY BEFORE THE ITERATIONS
      # NB: ACCOUNT FOR NO TEMPERATURE BELOW/ABOVE CEN FOR GIVEN PERIODS
      ansim[,"tot","abs",j,i,1] <- tapply(an,seqperiod,sum,na.rm = TRUE)
      
      # (6) ESTIMATE UNCERTAINTY OF THE PROJECTED AN:
      # - SAMPLE COEF ASSUMING A MULTIVARIATE NORMAL DISTRIBUTION
      set.seed(13041975+j)
      coefsim <- mvrnorm(nsim,blup$blup, blup$vcov)
      
      # - LOOP ACROSS ITERATIONS
      for(s in seq(nsim)) {
        
        # COMPUTE THE DAILY CONTRIBUTIONS OF ATTRIBUTABLE DEATHS
        an <- attrdl(tmeanproj[,j+2], cbtmean_1_9, cases_by_week_total_proj, 
                     coef = coefsim[s,], vcov = blup$vcov, cen = 10 , dir="forw", type="an", tot = FALSE)
        
        # STORE THE ATTRIBUTABLE MORTALITY
        ansim[,"tot","abs",j,i,s+1] <- tapply(an,seqperiod,sum,na.rm = TRUE)
        
      }
    }
  }
  ####################
  # Paired changes: subtract the matched GCM/scenario/simulation draw at 2016–2020.
  stopifnot(comparison_period %in% dimnames(ansim)[[1]])
  ansim[,"tot","rel",,,] <- ansim[,"tot","abs",,,] -
    ansim[rep(comparison_period, length(levels(seqperiod))), "tot", "abs",,,]
  
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
  
  # PAIRED DIFFERENCE relative to 2016–2020. Quantiles are computed on
  # paired simulation differences, not by subtracting marginal CI limits.
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
  afabs[,,,] <- anabs[,,,]/casesperiod*100
  afrel[,,,] <- anrel[,,,]/casesperiod*100
  
  # 결과를 리스트에 저장 (각 시도별로)
  anabs_list[[sido]] <- anabs
  anrel_list[[sido]] <- anrel
  afabs_list[[sido]] <- afabs
  afrel_list[[sido]] <- afrel
  
}
anabs
anabs_list[["whole country"]]
anabs

anabs_list[["50"]]


save(anabs_list, anrel_list, afabs_list, afrel_list, comparison_period, file = "C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\AR_projection/AR_sido_5000.RData")
