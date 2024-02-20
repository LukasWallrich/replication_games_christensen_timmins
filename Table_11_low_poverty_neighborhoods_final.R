# ---------------------------------------------------------------------------------------------- #
#   Generate Table 11. Discriminatory Steering: Low Poverty Neighborhoods                        #
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

recs_trial_final <- readRDS("HUDprocessed_JPE_census_042021.rds")

#construct separate indicators for market and control
recs_trial_final$market <- as.factor(sapply(strsplit(recs_trial_final$CONTROL, "-"), `[`, 1))
recs_trial_final$CONTROL <- as.factor(recs_trial_final$CONTROL)


#construct indicators for race groups
recs_trial_final$ofcolor <- 0
recs_trial_final$ofcolor[recs_trial_final$APRACE.x==2] <- 1
recs_trial_final$ofcolor[recs_trial_final$APRACE.x==3] <- 1
recs_trial_final$ofcolor[recs_trial_final$APRACE.x==4] <- 1
recs_trial_final$ofcolor <- as.factor(recs_trial_final$ofcolor)


#subset experimental data to mothers
recs_trial_final_mothers <- subset(recs_trial_final, kids.x==1 & TSEX.x.x==0)
recs_trial_final_mothers <- within(recs_trial_final_mothers, { count <- ave(as.character(TESTERID), CONTROL, FUN=function(x) length(unique(x)))})


#NEIGHBORHOOD POVERTY RATES
## Poverty rates are based on American Community Survey definitions by Census block group

# STEERING AND NEIGHBORHOOD POVERTY RATES
# STEERING INTO LOW POVERTY NEIGHBORHOODS -- MTO moves residents from 40% poverty rate to 10% poverty rate 

recs_trial_final$MTOpovrateHighDad <- 0
recs_trial_final$MTOpovrateHighDad[recs_trial_final$povrate_Rec < .1 & recs_trial_final$nodad_Rec < .5] <- 1
recs_trial_final$MTOpovrateHighDad <- as.factor(recs_trial_final$MTOpovrateHighDad)

recs_trial_final$MTOpovrate <- 0
recs_trial_final$MTOpovrate[recs_trial_final$povrate_Rec < .1] <- 1
recs_trial_final$MTOpovrate <- as.factor(recs_trial_final$MTOpovrate)


recs_trial_final_MTO <- subset(recs_trial_final, povrate_Ad<.1)
recs_trial_final_MTO <- within(recs_trial_final_MTO, { count <- ave(as.character(TESTERID), CONTROL, FUN=function(x) length(unique(x)))})
recs_trial_final_MTOsub <- subset(recs_trial_final_MTO, count>1)

recs_trial_final_MTOmom <- subset(recs_trial_final, povrate_Ad<.1 & kids.x==1 & TSEX.x.x==0)
recs_trial_final_MTOmom <- within(recs_trial_final_MTOmom, { count <- ave(as.character(TESTERID), CONTROL, FUN=function(x) length(unique(x)))})
recs_trial_final_MTOmomsub <- subset(recs_trial_final_MTOmom, count>1)

recs_trial_final_MTOfam <- subset(recs_trial_final, povrate_Ad<.1 & kids.x==1)
recs_trial_final_MTOfam <- within(recs_trial_final_MTOfam, { count <- ave(as.character(TESTERID), CONTROL, FUN=function(x) length(unique(x)))})
recs_trial_final_MTOfamsub <- subset(recs_trial_final_MTOfam, count>1)

# Subset tester pairs who are trying to move to a low poverty neighborhood with high father presence

recs_trial_final_MTOhighdads <- subset(recs_trial_final, povrate_Ad<.1 & recs_trial_final$nodad_Ad < .5)
recs_trial_final_MTOhighdads <- within(recs_trial_final_MTOhighdads, { count <- ave(as.character(TESTERID), CONTROL, FUN=function(x) length(unique(x)))})
recs_trial_final_MTOhighdadsub <- subset(recs_trial_final_MTOhighdads, count>1)

recs_trial_final_MTOhighdadsfam <- subset(recs_trial_final, povrate_Ad<.1 & kids.x==1 & recs_trial_final$nodad_Ad < .5)
recs_trial_final_MTOhighdadsfam <- within(recs_trial_final_MTOhighdadsfam, { count <- ave(as.character(TESTERID), CONTROL, FUN=function(x) length(unique(x)))})
recs_trial_final_MTOhighdadsfamsub <- subset(recs_trial_final_MTOhighdadsfam, count>1)

recs_trial_final_MTOhighdadsmom <- subset(recs_trial_final, povrate_Ad<.1 & kids.x==1 & TSEX.x.x==0 & recs_trial_final$nodad_Ad < .5)
recs_trial_final_MTOhighdadsmom <- within(recs_trial_final_MTOhighdadsmom, { count <- ave(as.character(TESTERID), CONTROL, FUN=function(x) length(unique(x)))})
recs_trial_final_MTOhighdadsmomsub <- subset(recs_trial_final_MTOhighdadsmom, count>1)





#Test Steering into Low Poverty Neighborhoods (based on MTO definition)
#Test MTO Low Poverty Rate

MTOpovrate_MTOsub_ <- felm(MTOpovrate ~ APRACE.x + povrate_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final_MTOsub)
summary(MTOpovrate_MTOsub_)


MTOpovrate_MTOfamsub_ <- felm(MTOpovrate ~ APRACE.x + povrate_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final_MTOfamsub)
summary(MTOpovrate_MTOfamsub_)


MTOpovrate_MTOmomssub_ <- felm(MTOpovrate ~ APRACE.x + povrate_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final_MTOmomsub)
summary(MTOpovrate_MTOmomssub_)



#Test MTO Low Poverty Rate with High Dad Rate

MTOpovrateHighDad_ <- felm(MTOpovrateHighDad ~ APRACE.x + povrate_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final_MTOhighdadsub)
summary(MTOpovrateHighDad_)


MTOpovrateHighDad_famsub_ <- felm(MTOpovrateHighDad ~ APRACE.x + povrate_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final_MTOhighdadsfamsub)
summary(MTOpovrateHighDad_famsub_)

MTOpovrateHighDad_momsub_ <- felm(MTOpovrateHighDad ~ APRACE.x + povrate_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final_MTOhighdadsmomsub)
summary(MTOpovrateHighDad_momsub_)



### GENERATE TABLES


### Effects in Low Poverty Neighborhoods (based on MTO definition)
stargazer(MTOpovrate_MTOsub_, MTOpovrate_MTOfamsub_, MTOpovrate_MTOmomssub_, MTOpovrateHighDad_, MTOpovrateHighDad_famsub_, MTOpovrateHighDad_momsub_,
          type = "latex",
          out = paste0(out, "HUM_Cen_MTO1withdads_JPE_.tex"),
          title="Steering and Neighborhood Effects",
          dep.var.labels.include = F,
          column.labels = c("Low Poverty","Low Poverty: Families" ,"Low Poverty: Moms", "Low Pov/High Dad","Low Pov/High Dad: Families" ,"Low Pov/High Dad: Moms"),
          model.numbers = F,
          keep = c("APRACE.x"),
          covariate.labels = c("African American",
                               "Hispanic",
                               "Asian",
                               "Other"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F)

