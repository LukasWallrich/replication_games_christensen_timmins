# ------------------------------------------#
#   Robustness checks for Table 5           #
#   R-Version: 4.3.2                        #
# ------------------------------------------#

setwd(here::here())

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

# Load data
recs_trial_final <- readRDS("Final Data Sets/adsprocessed_JPE.rds")

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

##
# Included robustness check: windsorize outcome variable
# Has major outliers
##

stem(recs_trial_final$show)
#Windsorize recs_trial_final$show
qntl <- quantile( recs_trial_final$show, c(0.05, 0.95), na.rm = TRUE)
recs_trial_final$show_winds <- recs_trial_final$show
recs_trial_final$show_winds[ recs_trial_final$show < qntl[1]] <- qntl[1]
recs_trial_final$show_winds[ recs_trial_final$show > qntl[2]] <- qntl[2]


SHOW3 <- felm(show ~ ofcolor + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x + month + HCITY + market + ARELATE2 + HHMTYPE + SAPPTAM + TSEX.x + THHEGAI + TPEGAI + THIGHEDU + TCURTENR +  ALGNCUR + AELNG1 + DPMTEXP + AMOVERS + age + ALEASETP + ACAROWN| 0 |CONTROL, data  = recs_trial_final)
SHOW3winds <- felm(show_winds ~ ofcolor + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x + month + HCITY + market + ARELATE2 + HHMTYPE + SAPPTAM + TSEX.x + THHEGAI + TPEGAI + THIGHEDU + TCURTENR +  ALGNCUR + AELNG1 + DPMTEXP + AMOVERS + age + ALEASETP + ACAROWN| 0 |CONTROL, data  = recs_trial_final)

SHOW3_ <- felm(show ~ APRACE + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x + month + HCITY + market + ARELATE2 + HHMTYPE + SAPPTAM + TSEX.x + THHEGAI + TPEGAI + THIGHEDU + TCURTENR +  ALGNCUR + AELNG1 + DPMTEXP + AMOVERS + age + ALEASETP + ACAROWN| 0 |CONTROL, data  = recs_trial_final)
SHOW3_winds <- felm(show_winds ~ APRACE + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x + month + HCITY + market + ARELATE2 + HHMTYPE + SAPPTAM + TSEX.x + THHEGAI + TPEGAI + THIGHEDU + TCURTENR +  ALGNCUR + AELNG1 + DPMTEXP + AMOVERS + age + ALEASETP + ACAROWN| 0 |CONTROL, data  = recs_trial_final)

##
# Excluded robustness check: cluster errors among testers, not trials
# (They are not nested)
##

SHOW3t <- felm(show ~ ofcolor + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x + month + HCITY + market + ARELATE2 + HHMTYPE + SAPPTAM + TSEX.x + THHEGAI + TPEGAI + THIGHEDU + TCURTENR +  ALGNCUR + AELNG1 + DPMTEXP + AMOVERS + age + ALEASETP + ACAROWN| 0 | TESTERID, data  = recs_trial_final)
SHOW3_t <- felm(show ~ APRACE + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x + month + HCITY + market + ARELATE2 + HHMTYPE + SAPPTAM + TSEX.x + THHEGAI + TPEGAI + THIGHEDU + TCURTENR +  ALGNCUR + AELNG1 + DPMTEXP + AMOVERS + age + ALEASETP + ACAROWN| 0 |TESTERID, data  = recs_trial_final)



