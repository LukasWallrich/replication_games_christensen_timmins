#Clearing old workspace
rm(list = ls()) 

#Load Packages
## \Roaming\Microsoft\Windows\Network Shortcuts\share (141.142.208.117 (netshare-backup server (Samba, Ubuntu)))
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}
## These lines load the required packages
packages <- c("readxl", "readstata13", "lfe", "Synth","data.table", "plm", "ggplot2", "MatchIt", "experiment", "stargazer")
lapply(packages, pkgTest)

#Set WD local
setwd("C:/Users/genin/OneDrive/Documents/Git/Discrimination/data")


## Housing Search  ######################################################################################################

#Preamble

# Unless Otherwise Specified, Model Sequence is as follows:
# Model 1: treatment effect conditional on advertisement price
# Model 2: treatment effect also conditional on level of treatment outcome in advertised listing
# Model 3: treatment effect also conditional on racial composition of block group of advertised listing
# Model 4: treatment effect also conditional on racial composition of block group of recommended listing
# Model 5: treatment effect also conditional on price of recommended home

# All models cluster standard errors by trial 

recs_trial_final <- readRDS("HUDprocessed_JPE_names_042021.rds")

#construct separate indicators for market and control
recs_trial_final$market <- as.factor(sapply(strsplit(recs_trial_final$CONTROL, "-"), `[`, 1))
recs_trial_final$CONTROL <- as.factor(recs_trial_final$CONTROL)


#construct indicators for race groups
recs_trial_final$ofcolor <- 0
recs_trial_final$ofcolor[recs_trial_final$APRACE.x==2] <- 1
recs_trial_final$ofcolor[recs_trial_final$APRACE.x==3] <- 1
recs_trial_final$ofcolor[recs_trial_final$APRACE.x==4] <- 1
recs_trial_final$ofcolor <- as.factor(recs_trial_final$ofcolor)

#construct indicators for race groups
recs_trial_final$whitetester <- 0
recs_trial_final$whitetester[recs_trial_final$APRACE.x==1] <- 1

recs_trial_final$blacktester <- 0
recs_trial_final$blacktester[recs_trial_final$APRACE.x==2] <- 1

recs_trial_final$hisptester <- 0
recs_trial_final$hisptester[recs_trial_final$APRACE.x==3] <- 1

recs_trial_final$asiantester <- 0
recs_trial_final$asiantester[recs_trial_final$APRACE.x==4] <- 1


#construct same race for buyer and tester
recs_trial_final$buyer_samerace_Rec <- 0
recs_trial_final$buyer_samerace_Rec[recs_trial_final$buyer_pred_race_Rec=="white" & recs_trial_final$APRACE.x==1] <- 1
recs_trial_final$buyer_samerace_Rec[recs_trial_final$buyer_pred_race_Rec=="black" & recs_trial_final$APRACE.x==2] <- 1
recs_trial_final$buyer_samerace_Rec[recs_trial_final$buyer_pred_race_Rec=="hispanic" & recs_trial_final$APRACE.x==3] <- 1
recs_trial_final$buyer_samerace_Rec[recs_trial_final$buyer_pred_race_Rec=="asian" & recs_trial_final$APRACE.x==4] <- 1
recs_trial_final$buyer_samerace_Rec[is.na(recs_trial_final$buyer_pred_race_Rec)] <- NA

#construct race of buyer
recs_trial_final$buyer_white_Rec <- 0
recs_trial_final$buyer_white_Rec[recs_trial_final$buyer_pred_race_Rec=="white"] <- 1
recs_trial_final$buyer_white_Rec[is.na(recs_trial_final$buyer_pred_race_Rec)] <- NA

recs_trial_final$buyer_black_Rec <- 0
recs_trial_final$buyer_black_Rec[recs_trial_final$buyer_pred_race_Rec=="black"] <- 1
recs_trial_final$buyer_black_Rec[is.na(recs_trial_final$buyer_pred_race_Rec)] <- NA

recs_trial_final$buyer_hisp_Rec <- 0
recs_trial_final$buyer_hisp_Rec[recs_trial_final$buyer_pred_race_Rec=="hispanic"] <- 1
recs_trial_final$buyer_hisp_Rec[is.na(recs_trial_final$buyer_pred_race_Rec)] <- NA

recs_trial_final$buyer_asian_Rec <- 0
recs_trial_final$buyer_asian_Rec[recs_trial_final$buyer_pred_race_Rec=="asian"] <- 1
recs_trial_final$buyer_asian_Rec[is.na(recs_trial_final$buyer_pred_race_Rec)] <- NA


### Construct Indicators of Confidence for Name Classifier
recs_trial_final$prob50 <- 0
recs_trial_final$prob50[recs_trial_final$buyer_pred_race_Rec=="white" & recs_trial_final$buyer_whi_Rec > .5] <- 1
recs_trial_final$prob50[recs_trial_final$buyer_pred_race_Rec=="black" & recs_trial_final$buyer_bla_Rec > .5] <- 1
recs_trial_final$prob50[recs_trial_final$buyer_pred_race_Rec=="hispanic" & recs_trial_final$buyer_his_Rec > .5] <- 1
recs_trial_final$prob50[recs_trial_final$buyer_pred_race_Rec=="asian" & recs_trial_final$buyer_asi_Rec > .5] <- 1
recs_trial_final$prob50[recs_trial_final$buyer_pred_race_Rec=="other" & recs_trial_final$buyer_oth_Rec > .5] <- 1
recs_trial_final$prob50[is.na(recs_trial_final$buyer_pred_race_Rec)] <- NA

recs_trial_final$prob70 <- 0
recs_trial_final$prob70[recs_trial_final$buyer_pred_race_Rec=="white" & recs_trial_final$buyer_whi_Rec > .7] <- 1
recs_trial_final$prob70[recs_trial_final$buyer_pred_race_Rec=="black" & recs_trial_final$buyer_bla_Rec > .7] <- 1
recs_trial_final$prob70[recs_trial_final$buyer_pred_race_Rec=="hispanic" & recs_trial_final$buyer_his_Rec > .7] <- 1
recs_trial_final$prob70[recs_trial_final$buyer_pred_race_Rec=="asian" & recs_trial_final$buyer_asi_Rec > .7] <- 1
recs_trial_final$prob70[recs_trial_final$buyer_pred_race_Rec=="other" & recs_trial_final$buyer_oth_Rec > .7] <- 1
recs_trial_final$prob70[is.na(recs_trial_final$buyer_pred_race_Rec)] <- NA


SR4_ <- felm(buyer_samerace_Rec ~ 1 | w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, prob70==1))
summary(SR4_)

#Buyer Race
## Buyer Race is based on buyer names from ZTRAX transaction data

WT4_ <- felm(buyer_white_Rec ~ whitetester + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
summary(WT4_)

BT4_ <- felm(buyer_black_Rec ~ blacktester + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
summary(BT4_)

HT4_ <- felm(buyer_hisp_Rec ~ hisptester + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
summary(HT4_)

AT4_ <- felm(buyer_asian_Rec ~ asiantester + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
summary(AT4_)

#summarize race prediction probabilities
summary(recs_trial_final$buyer_whi_Rec[recs_trial_final$buyer_pred_race_Rec=="white"])
summary(recs_trial_final$buyer_pred_race_Rec)
summary(recs_trial_final$buyer_bla_Rec[recs_trial_final$buyer_pred_race_Rec=="black"])
summary(recs_trial_final$buyer_his_Rec[recs_trial_final$buyer_pred_race_Rec=="hispanic"])
summary(recs_trial_final$buyer_asi_Rec[recs_trial_final$buyer_pred_race_Rec=="asian"])

summary(recs_trial_final$buyer_bla_Rec[recs_trial_final$buyer_pred_race_Rec=="white"])
summary(recs_trial_final$buyer_his_Rec[recs_trial_final$buyer_pred_race_Rec=="white"])
summary(recs_trial_final$buyer_asi_Rec[recs_trial_final$buyer_pred_race_Rec=="white"])

summary(recs_trial_final$buyer_whi_Rec[recs_trial_final$buyer_pred_race_Rec=="black"])
summary(recs_trial_final$buyer_his_Rec[recs_trial_final$buyer_pred_race_Rec=="black"])
summary(recs_trial_final$buyer_asi_Rec[recs_trial_final$buyer_pred_race_Rec=="black"])

summary(recs_trial_final$buyer_whi_Rec[recs_trial_final$buyer_pred_race_Rec=="hispanic"])
summary(recs_trial_final$buyer_bla_Rec[recs_trial_final$buyer_pred_race_Rec=="hispanic"])
summary(recs_trial_final$buyer_asi_Rec[recs_trial_final$buyer_pred_race_Rec=="hispanic"])

summary(recs_trial_final$buyer_whi_Rec[recs_trial_final$buyer_pred_race_Rec=="asian"])
summary(recs_trial_final$buyer_bla_Rec[recs_trial_final$buyer_pred_race_Rec=="asian"])
summary(recs_trial_final$buyer_his_Rec[recs_trial_final$buyer_pred_race_Rec=="asian"])


white = mean(recs_trial_final$buyer_whi_Rec[recs_trial_final$buyer_pred_race_Rec=="white"],na.rm =TRUE) - mean(recs_trial_final$buyer_bla_Rec[recs_trial_final$buyer_pred_race_Rec=="white"],na.rm =TRUE)
black = mean(recs_trial_final$buyer_bla_Rec[recs_trial_final$buyer_pred_race_Rec=="black"],na.rm =TRUE) - mean(recs_trial_final$buyer_whi_Rec[recs_trial_final$buyer_pred_race_Rec=="black"],na.rm =TRUE)
hisp  = mean(recs_trial_final$buyer_his_Rec[recs_trial_final$buyer_pred_race_Rec=="hispanic"],na.rm =TRUE) - mean(recs_trial_final$buyer_bla_Rec[recs_trial_final$buyer_pred_race_Rec=="white"],na.rm =TRUE)
Asian = mean(recs_trial_final$buyer_whi_Rec[recs_trial_final$buyer_pred_race_Rec=="white"],na.rm =TRUE) - mean(recs_trial_final$buyer_bla_Rec[recs_trial_final$buyer_pred_race_Rec=="white"],na.rm =TRUE)



#Buyer Race
## Buyer Race is based on buyer names from ZTRAX transaction data

WT50_ <- felm(buyer_white_Rec ~ whitetester + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, prob50==1))
summary(WT50_)

BT50_ <- felm(buyer_black_Rec ~ blacktester + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, prob50==1))
summary(BT50_)

HT50_ <- felm(buyer_hisp_Rec ~ hisptester + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, prob50==1))
summary(HT50_)

AT50_ <- felm(buyer_asian_Rec ~ asiantester + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, prob50==1))
summary(AT50_)

summary(recs_trial_final$buyer_whi_Rec[recs_trial_final$buyer_pred_race_Rec=="white" & recs_trial_final$prob50==1])
summary(recs_trial_final$buyer_pred_race_Rec)
summary(recs_trial_final$buyer_bla_Rec[recs_trial_final$buyer_pred_race_Rec=="black" & recs_trial_final$prob50==1])
summary(recs_trial_final$buyer_his_Rec[recs_trial_final$buyer_pred_race_Rec=="hispanic" & recs_trial_final$prob50==1])
summary(recs_trial_final$buyer_asi_Rec[recs_trial_final$buyer_pred_race_Rec=="asian" & recs_trial_final$prob50==1])


#Buyer Race
## Buyer Race is based on buyer names from ZTRAX transaction data

WT70_ <- felm(buyer_white_Rec ~ whitetester + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, prob70==1))
summary(WT70_)

BT70_ <- felm(buyer_black_Rec ~ blacktester + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, prob70==1))
summary(BT70_)

HT70_ <- felm(buyer_hisp_Rec ~ hisptester + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, prob70==1))
summary(HT70_)

AT70_ <- felm(buyer_asian_Rec ~ asiantester + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, prob70==1))
summary(AT70_)




#Buyer Race
## Buyer Race is based on buyer names from ZTRAX transaction data

WT50_r <- felm(buyer_white_Rec ~ whitetester + w2012pc_Rec + b2012pc_Rec + a2012pc_Rec + hisp2012pc_Rec + logAdPrice  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, prob50==1))
summary(WT50_r)

BT50_r <- felm(buyer_black_Rec ~ blacktester + w2012pc_Rec + b2012pc_Rec + a2012pc_Rec + hisp2012pc_Rec + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, prob50==1))
summary(BT50_r)

HT50_r <- felm(buyer_hisp_Rec ~ hisptester + w2012pc_Rec + b2012pc_Rec + a2012pc_Rec + hisp2012pc_Rec + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, prob50==1))
summary(HT50_r)

AT50_r <- felm(buyer_asian_Rec ~ asiantester + w2012pc_Rec + b2012pc_Rec + a2012pc_Rec + hisp2012pc_Rec + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, prob50==1))
summary(AT50_r)


#Buyer Race
## Buyer Race is based on buyer names from ZTRAX transaction data

WT70_r <- felm(buyer_white_Rec ~ whitetester + w2012pc_Rec + b2012pc_Rec + a2012pc_Rec + hisp2012pc_Rec + logAdPrice  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, prob70==1))
summary(WT70_r)

BT70_r <- felm(buyer_black_Rec ~ blacktester + w2012pc_Rec + b2012pc_Rec + a2012pc_Rec + hisp2012pc_Rec + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, prob70==1))
summary(BT70_r)

HT70_r <- felm(buyer_hisp_Rec ~ hisptester + w2012pc_Rec + b2012pc_Rec + a2012pc_Rec + hisp2012pc_Rec + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, prob70==1))
summary(HT70_r)

AT70_r <- felm(buyer_asian_Rec ~ asiantester + w2012pc_Rec + b2012pc_Rec + a2012pc_Rec + hisp2012pc_Rec + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, prob70==1))
summary(AT70_r)


# Sales Price
## Sales Price is based on buyer names from ZTRAX transaction data

summary(recs_trial_final$SalesPriceAmount_Rec)
summary(recs_trial_final$RecordingDate_Rec)

recs_trial_final$TransMonth <- as.factor(format(recs_trial_final$RecordingDate_Rec, '%B'))
recs_trial_final$TransYear <- as.factor(format(recs_trial_final$RecordingDate_Rec, '%Y'))
summary(recs_trial_final$TransYear)

recs_trial_final$TransMid11 <- 0
recs_trial_final$TransMid11[recs_trial_final$RecordingDate_Rec > "2011-01-06"] <- 1
recs_trial_final$TransMid11[is.na(recs_trial_final$RecordingDate_Rec)] <- NA

# Constrain to After 06/01/2011 (price > 10,000 and price < 10 Million)
logPrice11_ <- felm(log(SalesPriceAmount_Rec) ~ ofcolor + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + TransMonth + TransYear  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, SalesPriceAmount_Rec>10000 & SalesPriceAmount_Rec<10000000 & TransMid11 ==1 ))
summary(logPrice11_)

logPrice11_ <- felm(log(SalesPriceAmount_Rec) ~ APRACE.x + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + TransMonth + TransYear  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, SalesPriceAmount_Rec>10000 & SalesPriceAmount_Rec<10000000 & TransMid11 ==1 ))
summary(logPrice11_)



### GENERATE TABLES
out <- "C:/Users/genin/OneDrive/Documents/Git/Discrimination/output/"



stargazer(WT4_, BT4_, HT4_, AT4_,
          type = "latex",
          out = paste0(out, "HUM_Cen1_J.tex"),
          title="Steering and Neighborhood Effects",
          model.numbers = F,
          covariate.labels = c("Racial Minority"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list( c("ln(Price) Advert Home","Y","Y","Y","Y"),
                           c("Racial Comp Advert Home","Y","Y","Y","Y"),
                           c("Outcome Advertised Home","Y","Y","Y","Y")))

stargazer(AS4, SP4, RSEI4, PM4,
          type = "latex",
          out = paste0(out, "HUM_Cen2_JPE.tex"),
          title="Steering and Neighborhood Effects",
          model.numbers = F,
          keep = c("ofcolor"),
          covariate.labels = c("Racial Minority"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("p-values", signif(p5, digits = 2), signif(p_2, digits = 2)),
                           c("q-values", signif(p5, digits = 2),signif(q2, digits = 2)),
                           c("ln(Price) Advert Home","Y","Y","Y","Y"),
                           c("Racial Comp Advert Home","Y","Y","Y","Y"),
                           c("Outcome Advertised Home","Y","Y","Y","Y")))


stargazer(PR4_, SK4_, COL4_, ES4_,
          type = "latex",
          out = paste0(out, "HUM_Cen1_JPE_.tex"),
          title="Steering and Neighborhood Effects",
          dep.var.labels.include = F,
          column.labels = c("Poverty Rate","High Skill", "College", "Elem School"),
          model.numbers = F,
          keep = c("APRACE.x"),
          covariate.labels = c("African American",
                               "Hispanic",
                               "Asian",
                               "Other"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("ln(Price) Advert Home","Y","Y","Y","Y"),
                           c("Racial Comp Advert Home","Y","Y","Y","Y"),
                           c("Outcome Advertised Home","Y","Y","Y","Y")))


stargazer(AS4_, SP4_, RSEI4_, PM4_,
          type = "latex",
          out = paste0(out, "HUM_Cen2_JPE_.tex"),
          title="Steering and Neighborhood Effects",
          dep.var.labels.include = F,
          column.labels = c("Assaults", "Superfund", "Toxics", "PM"),
          model.numbers = F,
          keep = c("APRACE.x"),
          covariate.labels = c("African American",
                               "Hispanic",
                               "Asian",
                               "Other"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("ln(Price) Advert Home","Y","Y","Y","Y"),
                           c("Racial Comp Advert Home","Y","Y","Y","Y"),
                           c("Outcome Advertised Home","Y","Y","Y","Y")))

