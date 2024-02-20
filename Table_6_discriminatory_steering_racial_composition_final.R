# ---------------------------------------------------------------------------------------------- #
#   Generate Table 6. Discriminatory Steering and Neighborhood Racial Composition                #
#                                                                                                # 
#   R-Version: 4.04                                                                              #                                                             #
#   Date Last Modification: 12/01/2021                                                           #
# -----------------------------------------------------------------------------------------------#

# Clear workspace

rm(list = ls()) 

# Set working directory

setwd("~/")

# Define function for loading packages

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

# Load data   

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


# STEERING and Neighborhood Composition
# STEERING AND white NEIGHBORHOOD 

W1 <- felm(w2012pc_Rec ~ ofcolor | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
W2 <- felm(w2012pc_Rec ~ ofcolor + w2012pc_Ad | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
W3 <- felm(w2012pc_Rec ~ ofcolor + logAdPrice + w2012pc_Ad | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
W4 <- felm(w2012pc_Rec ~ ofcolor + logAdPrice + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
W5 <- felm(w2012pc_Rec ~ ofcolor + logAdPrice + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad  + povrate_Ad | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)


# STEERING AND white NEIGHBORHOOD

W1_ <- felm(w2012pc_Rec ~ APRACE.x | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
W2_ <- felm(w2012pc_Rec ~ APRACE.x + w2012pc_Ad | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
W3_ <- felm(w2012pc_Rec ~ APRACE.x + logAdPrice + w2012pc_Ad | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
W4_ <- felm(w2012pc_Rec ~ APRACE.x + logAdPrice + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
W5_ <- felm(w2012pc_Rec ~ APRACE.x + logAdPrice + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad  + povrate_Ad | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

### GENERATE TABLES


###########Control Group Mean####################

a=subset(recs_trial_final, select=c(w2012pc_Rec,ofcolor,povrate_Ad,w2012pc_Ad, b2012pc_Ad,
                                    a2012pc_Ad, hisp2012pc_Ad, logAdPrice, CONTROL,
                                    SEQUENCE.x.x, month.x ,HCITY.x ,market ,ARELATE2.x ,
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

stargazer(W1, W2, W3, W4, W5,
          type = "latex",
          out = paste0(out, "HUM_tab6_JPE.tex"),
          title="Steering and Neighborhood Effects",
          model.numbers = F,
          keep = c("ofcolor"),
          covariate.labels = c("Racial Minority"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("ln(Price) Advert Home","N","Y","N","Y"),
                           c("Racial Comp Advert Home","N","Y","N","Y")))


stargazer(W1_ , W2_ , W3_ , W4_ , W5_ ,
          type = "latex",
          out = paste0(out, "HUM_tab6_JPE_.tex"),
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
                           c("Poverty Share Advert Home","N","N","N","N","Y")
                           ))



