# ---------------------------------------------------------------------------------------------- #
#   Generate Table 5. Differences in Recommendations and Availability of Advertised Properties   #
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
packages <- c("readxl", "readstata13", "lfe", "Synth","data.table", "plm", "ggplot2", "MatchIt", "experiment", "stargazer","dplyr")
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

recs_trial_final <- readRDS("adsprocessed_JPE.rds")

#construct separate indicators for market and control

recs_trial_final$market <- as.factor(sapply(strsplit(as.character(recs_trial_final$CONTROL), "-"), `[`, 1))
recs_trial_final$CONTROL <- as.factor(recs_trial_final$CONTROL)

recs_trial_final$show <- recs_trial_final$STOTUNIT
recs_trial_final$show[recs_trial_final$STOTUNIT<0] <- NA

#construct indicators for race groups
recs_trial_final$ofcolor <- 0
recs_trial_final$ofcolor[recs_trial_final$APRACE==2] <- 1
recs_trial_final$ofcolor[recs_trial_final$APRACE==3] <- 1
recs_trial_final$ofcolor[recs_trial_final$APRACE==4] <- 1
recs_trial_final$ofcolor <- as.factor(recs_trial_final$ofcolor)

recs_trial_final$count=1


# Dicrimination and Showings
# STEERING AND white NEIGHBORHOOD 

SHOW1 <- felm(show ~ ofcolor | CONTROL + SEQUENCE.x + month + HCITY + market + ARELATE2 + HHMTYPE + SAPPTAM + TSEX.x + THHEGAI + TPEGAI + THIGHEDU + TCURTENR +  ALGNCUR + AELNG1 + DPMTEXP + AMOVERS + age + ALEASETP + ACAROWN| 0 | CONTROL, data  = recs_trial_final)
SHOW3 <- felm(show ~ ofcolor + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x + month + HCITY + market + ARELATE2 + HHMTYPE + SAPPTAM + TSEX.x + THHEGAI + TPEGAI + THIGHEDU + TCURTENR +  ALGNCUR + AELNG1 + DPMTEXP + AMOVERS + age + ALEASETP + ACAROWN| 0 |CONTROL, data  = recs_trial_final)

SHOW1_ <- felm(show ~ APRACE | CONTROL + SEQUENCE.x + month + HCITY + market + ARELATE2 + HHMTYPE + SAPPTAM + TSEX.x + THHEGAI + TPEGAI + THIGHEDU + TCURTENR +  ALGNCUR + AELNG1 + DPMTEXP + AMOVERS + age + ALEASETP + ACAROWN| 0 | CONTROL, data  = recs_trial_final)
SHOW3_ <- felm(show ~ APRACE + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x + month + HCITY + market + ARELATE2 + HHMTYPE + SAPPTAM + TSEX.x + THHEGAI + TPEGAI + THIGHEDU + TCURTENR +  ALGNCUR + AELNG1 + DPMTEXP + AMOVERS + age + ALEASETP + ACAROWN| 0 |CONTROL, data  = recs_trial_final)


##########################################################################################################################################################################

# STEERING 

recs_trial_final$home_av <- recs_trial_final$SAVLBAD
recs_trial_final$home_av[recs_trial_final$home_av<0] <- NA
recs_trial_final$home_av[recs_trial_final$home_av>1] <- 0

SHOWad1 <- felm(home_av ~ ofcolor | CONTROL + SEQUENCE.x + month + HCITY + market + ARELATE2 + HHMTYPE + SAPPTAM + TSEX.x + THHEGAI + TPEGAI + THIGHEDU + TCURTENR +  ALGNCUR + AELNG1 + DPMTEXP + AMOVERS + age + ALEASETP + ACAROWN| 0 | CONTROL, data  = recs_trial_final)
SHOWad3 <- felm(home_av ~ ofcolor + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x + month + HCITY + market + ARELATE2 + HHMTYPE + SAPPTAM + TSEX.x + THHEGAI + TPEGAI + THIGHEDU + TCURTENR +  ALGNCUR + AELNG1 + DPMTEXP + AMOVERS + age + ALEASETP + ACAROWN| 0 |CONTROL, data  = recs_trial_final)

SHOWad1_ <- felm(home_av ~ APRACE | CONTROL + SEQUENCE.x + month + HCITY + market + ARELATE2 + HHMTYPE + SAPPTAM + TSEX.x + THHEGAI + TPEGAI + THIGHEDU + TCURTENR +  ALGNCUR + AELNG1 + DPMTEXP + AMOVERS + age + ALEASETP + ACAROWN| 0 | CONTROL, data  = recs_trial_final)
SHOWad3_ <- felm(home_av ~ APRACE + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x + month + HCITY + market + ARELATE2 + HHMTYPE + SAPPTAM + TSEX.x + THHEGAI + TPEGAI + THIGHEDU + TCURTENR +  ALGNCUR + AELNG1 + DPMTEXP + AMOVERS + age + ALEASETP + ACAROWN| 0 |CONTROL, data  = recs_trial_final)

### GENERATE TABLES
out <- "C:/Users/genin/OneDrive/Documents/Git/Discrimination/output/"

###########Control Group Mean####################

stargazer(SHOW1, SHOW3, SHOWad1, SHOWad3,
          type = "latex",
          out = paste0(out, "HUM_tab5_JPE.tex"),
          title="Steering and Neighborhood Effects",
          model.numbers = F,
          keep = c("ofcolor"),
          covariate.labels = c("Racial Minority"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("ln(Price) Advert Home","N","Y","N","Y"),
                           c("Racial Comp Advert Home","N","Y","N","Y")))


stargazer(SHOW1_, SHOW3_, SHOWad1_, SHOWad3_,
          type = "latex",
          out = paste0(out, "HUM_tab5_JPE_.tex"),
          title="Steering and Neighborhood Effects",
          dep.var.labels.include = F,
          column.labels = c("Number of Recommendations","Home Availability"),
          model.numbers = F,
          keep = c("APRACE"),
          covariate.labels = c("African American",
                               "Hispanic",
                               "Asian",
                               "Other"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("ln(Price) Advert Home","N","Y","N","Y"),
                           c("Racial Comp Advert Home","N","Y","N","Y")))


