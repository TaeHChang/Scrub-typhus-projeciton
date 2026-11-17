# Revision notice: manuscript-aligned analysis draft (2026-11-17).
# These commands were edited AFTER the original scripts; they do not document
# which code was originally executed for the manuscript. Verify results before release.

#### TS design for scrub typhus in Korea (weekly data) 
### 시도 단위 분석으로 진행하기로 결정 되었음.
# Main-model lag window: 1–9 weeks; 1 log-spaced interior lag knot
# preserves the original lag-basis df=3 (requires numerical validation).

## 최종 모형 코드 정리.

library(data.table) # HANDLE LARGE DATASETS
library(readr)
library(dlnm) ; library(gnm) ; library(splines) # MODELLING TOOLS
library(sf) ; library(terra) # HANDLE SPATIAL DATA
library(exactextractr) # FAST EXTRACTION OF AREA-WEIGHTED RASTER CELLS
library(dplyr) ; library(tidyr) # DATA MANAGEMENT TOOLS
library(ggplot2) ; library(patchwork) # PLOTTING TOOLS
library(tidyterra) ; library(zoo)
library(EpiEstim); library(tidyverse) ;library(readxl) ;library(mixmeta)


weekly_data_SiDo <- read.csv("C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\Scrub typhus weekly/weekly_data_SiDo.csv")


# weekly_data_SiDO 데이터프레임을 sido_b 컬럼을 기준으로 분할
list_of_dfs <- split(weekly_data_SiDo, weekly_data_SiDo$sido_b)

# 리스트의 이름 확인
names(list_of_dfs)




process_data_model <- function(df) {
  # Calculate percentiles and knots
  pct_temp <- quantile(df$weekly_tmean, prob=c(.01, .10, .33, .66, .90, .99), na.rm=TRUE)
  varknot_temp <- pct_temp[c(3, 4)]  # knots at 33rd and 66th
  
  argvar_temp <- list(fun="ns", knots=varknot_temp)
  
  pct_preci <- quantile(df$weekly_total_preci, prob=c(.01,.10,.33,.66,.90,.99),na.rm=T)
  varknot_preci <- pct_preci[c(3,4)] # knots at 33rd and 66th
  
  argvar_preci <- list(fun="ns", knots=varknot_preci)
  
  
  # Crossbasis
  cbtmean_1_9 <- crossbasis(df$weekly_tmean, lag= c(1,9), argvar=argvar_temp, arglag=list(fun="ns", knots=dlnm::logknots(c(1,9), nk=1)))
  cbpreci_1_9 <- crossbasis(df$weekly_total_preci, lag= c(1,9), argvar=argvar_preci, arglag=list(fun="ns", knots=dlnm::logknots(c(1,9), nk=1)))
  
  
  # Model fitting
  df$splweek_4 <- onebasis(df$week, "ns", df=4)
  df$splyear_3 <- onebasis(df$year, "ns", df=3)
  
  model <- glm(weekly_total_cases ~ cbtmean_1_9 + cbpreci_1_9  + splweek_4 + splyear_3 + factor(COVID) + auto_14 
               + holi_thanksgiving_1, data = df, family=quasipoisson)
  
  # Cross-prediction
  cpfull <- crosspred(cbtmean_1_9, model, cen= 10)
  cr_temp <- crossreduce(cbtmean_1_9, model, cen = 10)
  return(list(model = model, crosspred = cpfull, coef = coef(cr_temp),  vcov = vcov(cr_temp)) )
}

# 결과 계산 및 결과 변수 업데이트
results_list <- vector("list", length(list_of_dfs))

pct_temp <- quantile(weekly_data_SiDo$weekly_tmean, prob=c(.01, .10, .33, .66, .90, .99), na.rm=TRUE)
varknot_temp <- pct_temp[c(3, 4)]

coef_temp <- matrix(NA, nrow=length(list_of_dfs), ncol=length(varknot_temp) + 1)  # 적절한 열의 수로 설정
vcov_temp <-  vector("list", length(list_of_dfs))

pct_preci <- quantile(weekly_data_SiDo$weekly_preci, prob=c(.01, .10, .33, .66, .90, .99), na.rm=TRUE)
varknot_preci <- pct_preci[c(3, 4)]

coef_preci <- matrix(NA, nrow=length(list_of_dfs), ncol=length(varknot_preci) + 1)  # 적절한 열의 수로 설정
vcov_preci <-  vector("list", length(list_of_dfs))



# 각 데이터 프레임에 대해 process_data_model 함수 적용
for (i in seq_along(list_of_dfs)) {
  df <- list_of_dfs[[i]]
  result <- process_data_model(df)
  results_list[[i]] <- result
  coef_temp[i, ] <- result$coef  # 계수 저장
  vcov_temp[[i]] <- result$vcov  # 공분산 행렬 저장
  coef_temp[i, ] <- result$coef  # 계수 저장
  vcov_temp[[i]] <- result$vcov  # 공분산 행렬 저장
}



rownames(coef_temp) <- names(list_of_dfs)
names(vcov_temp) <- names(list_of_dfs)
coef_temp
vcov_temp



################### Si-do Second-stage
SiDo_landcover_final <- read.csv("C:\\Users\\redwo\\OneDrive\\바탕 화면\\Direction 과제\\Scrub typhus\\data\\Scrub typhus weekly/SiDo_landcover_final.csv")

## forest, wetland만.
SiDo_landcover_final <- SiDo_landcover_final %>% 
  rename(urban = X1,
         agriculture_1 = X2,
         forest = X3,
         grassland = X4,
         wetland = X5,
         Barren = X6,
         Water = X7,
         agriculture_2 = X8)



### 가장 간단한 model_0 (random effect only model) 기반으로 projection 진행.
model_0 <- mixmeta(coef_temp ~ 1, 
                   vcov_temp, data=SiDo_landcover_final)
model_1 <- mixmeta(coef_temp ~ forest, 
                   vcov_temp, data=SiDo_landcover_final)
model_2 <- mixmeta(coef_temp ~ wetland, 
                   vcov_temp, data=SiDo_landcover_final)
model_3 <- mixmeta(coef_temp ~ forest + wetland, 
                   vcov_temp, data=SiDo_landcover_final)

summary(model_3)

SiDoblup <- blup(model_0, vcov=T)
names(SiDoblup) <- names(list_of_dfs)



model_0$coefficients
summary(model_0)

temps_all <- unlist(lapply(list_of_dfs, \(d) d$weekly_tmean))
bound_temp <- quantile(temps_all,  c(.00,1.00), na.rm=TRUE)
xvar_temp <- seq(bound_temp[1],bound_temp[2],by=0.1)
argvar_temp=list(fun="ns", knots=quantile(xvar_temp,prob=c(.33,.66)))
bvar_temp <- do.call(onebasis, c(list(x=xvar_temp), argvar_temp))

regions <- names(list_of_dfs)

pred.pool <- crosspred(bvar_temp, coef=model_0$coefficients[str_detect(names(model_0$coefficients), "Intercept")], 
                       vcov=model_0$vcov[grep("Intercept", rownames(model_0$vcov)), grep("Intercept", rownames(model_0$vcov))], model.link="log", 
                       by=0.1, cen=10)

pred.pool <- crosspred(bvar_temp, coef=model_0$coefficients, 
                       vcov=model_0$vcov, model.link="log", 
                       by=0.1, cen=10)

pred.reg <- lapply(seq(SiDoblup), 
                   function(i) crosspred(bvar_temp, coef=SiDoblup[[i]]$blup, vcov=SiDoblup[[i]]$vcov, by=0.1, 
                                         model.link="log", cen=10))
par(mfrow=c(1,1))
plot(pred.pool, type="l", ci="n", ylab="RR", ylim=c(0,30), lwd=2, col = "blue", 
     xlab="Temperature (C)", main="model 3")
for(i in seq(regions)) lines(pred.reg[[i]], col="grey")
lines(pred.pool, lwd=3)


col <- c("aquamarine3")
parold <- par(no.readonly=T)
# 마진 설정 변경: 위쪽 마진을 넓게 설정
par(mar = c(4, 4, 2, 0.5), las = 1, mgp = c(2.5, 1, 0))
plot(pred.pool , "overall", ylim=c(0,30), ylab="RR", col=col[1], lwd=1.5,
     xlab=expression(paste("Temperature ("*degree,"C)")), 
     ci.arg=list(col=alpha(col[1], 0.2)))
title(main = "SiDo weekly TS analysis, two-stage design, cen = 10", cex.main = 1.5)
par(parold)
