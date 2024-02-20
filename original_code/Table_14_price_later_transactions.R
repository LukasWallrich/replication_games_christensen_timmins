# ---------------------------------------------------------------------------------------------- #
#   Generate Table 14. Discriminatory Steering and Later Transactions                            #
#                                                                                                # 
#   R-Version: 4.04                                                                              #                                                             #
#   Date Last Modification: 12/01/2021                                                           #
# -----------------------------------------------------------------------------------------------#

# Clear workspace
rm(list = ls()) 

# Set working directory
setwd("~/")

# Define function for loading packages
rm(list = ls()) 

#Load Packages
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}
# Load packages
packages <- c("readxl", "readstata13", "lfe", "Synth","data.table", "plm", "ggplot2", "MatchIt", "experiment", "stargazer")
lapply(packages, pkgTest)

# Output directory
out <- "~/"


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

## Means

w = recs_trial_final %>% filter(buyer_pred_race_Rec=="white") %>%
  summarise(mean_as = mean(buyer_asi_Rec, na.rm=TRUE),
            mean_his= mean(buyer_his_Rec, na.rm=TRUE),
            mean_black= mean(buyer_bla_Rec, na.rm=TRUE),
            mean_whi= mean(buyer_whi_Rec, na.rm=TRUE))

b = recs_trial_final %>% filter(seller_pred_race_Rec=="black") %>%
  summarise(mean_as = mean(buyer_asi_Rec, na.rm=TRUE),
            mean_his= mean(buyer_his_Rec, na.rm=TRUE),
            mean_black= mean(buyer_bla_Rec, na.rm=TRUE),
            mean_whi= mean(buyer_whi_Rec, na.rm=TRUE))

a = recs_trial_final %>% filter(buyer_pred_race_Rec=="asian" & buyer_asi_Rec>0.7) %>% 
  summarise(mean_as = mean(buyer_asi_Rec, na.rm=TRUE),
            mean_his= mean(buyer_his_Rec, na.rm=TRUE),
            mean_black= mean(buyer_bla_Rec, na.rm=TRUE),
            mean_whi= mean(buyer_whi_Rec, na.rm=TRUE))

h = recs_trial_final %>% filter(buyer_pred_race_Rec=="hispanic") %>% 
  summarise(mean_as = mean(buyer_asi_Rec, na.rm=TRUE),
            mean_his= mean(buyer_his_Rec, na.rm=TRUE),
            mean_black= mean(buyer_bla_Rec, na.rm=TRUE),
            mean_whi= mean(buyer_whi_Rec, na.rm=TRUE))

w_dif = w$mean_whi-w$mean_black
a_dif = a$mean_as - a$mean_his
b_dif = b$mean_black -a$mean_black
h_dif = h$mean_his-h$mean_as

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

##### Difference of Score ###############

dif_score = matrix(ncol = 1,nrow =4)
dif_score[1,] = w_dif
dif_score[2,] = b_dif
dif_score[3,] = h_dif 
dif_score[4,] = a_dif
dif_score= t(dif_score)

# Same Race Tester 

same1 <- felm(buyer_white_Rec ~ whitetester + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
summary(same1)

same2 <- felm(buyer_black_Rec ~ blacktester + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
summary(same2)

same3 <- felm(buyer_hisp_Rec ~ hisptester + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
summary(same3)

same4 <- felm(buyer_asian_Rec ~ asiantester + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
summary(same4)


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

logPrice11 <- felm(log(SalesPriceAmount_Rec) ~ ofcolor + TransMonth + TransYear | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, SalesPriceAmount_Rec>10000 & SalesPriceAmount_Rec<10000000 & TransMid11 ==1 ))
logPrice12 <- felm(log(SalesPriceAmount_Rec) ~ ofcolor + w2012pc_Ad + TransMonth + TransYear | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, SalesPriceAmount_Rec>10000 & SalesPriceAmount_Rec<10000000 & TransMid11 ==1 ))
logPrice13 <- felm(log(SalesPriceAmount_Rec) ~ ofcolor + w2012pc_Ad + logAdPrice + TransMonth + TransYear | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, SalesPriceAmount_Rec>10000 & SalesPriceAmount_Rec<10000000 & TransMid11 ==1 ))
logPrice14 <- felm(log(SalesPriceAmount_Rec) ~ ofcolor + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + TransMonth + TransYear  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, SalesPriceAmount_Rec>10000 & SalesPriceAmount_Rec<10000000 & TransMid11 ==1 ))
logPrice15 <- felm(log(SalesPriceAmount_Rec) ~ ofcolor + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + TransMonth + TransYear  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, SalesPriceAmount_Rec>10000 & SalesPriceAmount_Rec<10000000 & TransMid11 ==1 ))


logPrice11_ <- felm(log(SalesPriceAmount_Rec) ~ APRACE.x + TransMonth + TransYear  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, SalesPriceAmount_Rec>10000 & SalesPriceAmount_Rec<10000000 & TransMid11 ==1 ))
logPrice12_ <- felm(log(SalesPriceAmount_Rec) ~ APRACE.x + w2012pc_Ad + TransMonth + TransYear  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, SalesPriceAmount_Rec>10000 & SalesPriceAmount_Rec<10000000 & TransMid11 ==1 ))
logPrice13_ <- felm(log(SalesPriceAmount_Rec) ~ APRACE.x + w2012pc_Ad + logAdPrice + TransMonth + TransYear  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, SalesPriceAmount_Rec>10000 & SalesPriceAmount_Rec<10000000 & TransMid11 ==1 ))
logPrice14_ <- felm(log(SalesPriceAmount_Rec) ~ APRACE.x + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice + TransMonth + TransYear   | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, SalesPriceAmount_Rec>10000 & SalesPriceAmount_Rec<10000000 & TransMid11 ==1 ))
logPrice15_ <- felm(log(SalesPriceAmount_Rec) ~ APRACE.x + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice  + TransMonth + TransYear  | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = subset(recs_trial_final, SalesPriceAmount_Rec>10000 & SalesPriceAmount_Rec<10000000 & TransMid11 ==1 ))


###########Control Group Mean####################

a=subset(recs_trial_final, select=c(w2012pc_Rec,ofcolor,w2012pc_Ad, b2012pc_Ad,
                                    a2012pc_Ad, hisp2012pc_Ad, logAdPrice, CONTROL,
                                    SEQUENCE.x.x, month.x ,HCITY.x ,ARELATE2.x ,
                                    HHMTYPE.x , SAVLBAD.x ,STOTUNIT_Rec ,SAPPTAM.x , 
                                    TSEX.x.x , THHEGAI.x , TPEGAI.x , THIGHEDU.x ,
                                    TCURTENR.x ,  ALGNCUR.x , AELNG1.x , DPMTEXP.x ,
                                    AMOVERS.x , age.x , ALEASETP.x , ACAROWN.x , white.x))
a=a[complete.cases(a),]

mean1=as.vector(c(mean(a[a$white.x==1,"w2012pc_Rec"]),
                  mean(a[a$white.x==1,"w2012pc_Rec"]),
                  mean(a[a$white.x==1,"w2012pc_Rec"]),
                  mean(a[a$white.x==1,"w2012pc_Rec"]),
                  mean(a[a$white.x==1,"w2012pc_Rec"]))) 

### GENERATE TABLES

##Panel A

stargazer(dif_score,
          type = "latex",
          out = paste0(out, "HUM_tabdifscore_JPE.tex"),
          title="Discriminatory Steering and Later Transactions",
          model.numbers = F,
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F)

stargazer(same1, same2, same3, same4,
          type = "latex",
          out = paste0(out, "HUM_samerace_JPE.tex"),
          title="Discriminatory Steering and Later Transactions",
          model.numbers = F,
          keep = c("whitetester","blacktester","hisptester","asiantester"),
          covariate.labels = c("Same Race Tester"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F)

## Panel B

stargazer(logPrice11, logPrice12, logPrice13, logPrice14, logPrice15,
          type = "latex",
          out = paste0(out, "HUM_tabprice_JPE.tex"),
          title="Steering and Neighborhood Effects",
          model.numbers = F,
          keep = c("ofcolor"),
          covariate.labels = c("Racial Minority"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("ln(Price) Advert Home","N","Y","N","Y"),
                           c("Racial Comp Advert Home","N","Y","N","Y")))


stargazer(logPrice11_ , logPrice12_ , logPrice13_ , logPrice14_ , logPrice15_ ,
          type = "latex",
          out = paste0(out, "HUM_tabprice_JPE_.tex"),
          title="Steering and Neighborhood Effects",
          dep.var.labels.include = F,
          column.labels = c("I","II","III","IV","V"),
          model.numbers = F,
          keep = c("APRACE.x"),
          covariate.labels = c("African American",
                               "Hispanic",
                               "Asian",
                               "Other"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("Comparison Mean (White)",signif(mean1, digits = 2)),
                           c("Share White Advert Home","N","Y","Y","Y","Y"),
                           c("ln(Price) Advert Home","N","N","Y","Y","Y"),
                           c("Racial Comp Advert Home","N","N","N","Y","Y"),
                           c("Poverty Share Advert Home","N","N","N","N","Y"),
                           c("Year","Y","Y","Y","Y","Y"),
                           c("Month of Year","Y","Y","Y","Y","Y")
          ))
