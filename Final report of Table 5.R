#' ---
#' title: '**Replicating Christensen & Timmins (2022)**'
#' author: "Lukas Wallrich, Giulio Giacomo Cantone, Rémi Thériault"
#' subtitle: Emphasis on Table 5 (Column 2), p. 2135, as part of the Institute for Replication *AI Games* 
#' date: "`r format(Sys.Date())`"
#' output: 
#'   pdf_document:
#'      keep_tex: true
#'      latex_engine: xelatex
#' header-includes:
#'   - \usepackage{dcolumn} 
#' ---

#' Reference: Christensen, P., & Timmins, C. (2022). Sorting or steering: The effects of housing discrimination on neighborhood choice. *Journal of Political Economy*, *130*(8), 2110-2163. https://doi.org/10.1086/720140

#+ setup, echo=FALSE, include=FALSE
# Clear workspace
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)
rm(list = ls())

# Set working directory

# setwd("~/")

# Define function for loading packages
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

# Load packages
#+ echo=FALSE, include=FALSE
packages <- c("readxl", "readstata13", "lfe", "Synth","data.table", "plm", "ggplot2", "MatchIt", "experiment", "stargazer","dplyr", "visdat", "missForest", "doParallel")
lapply(packages, pkgTest)


# Output directory

# out <- "~/"

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
# out <- "C:/Users/genin/OneDrive/Documents/Git/Discrimination/output/"

###########Control Group Mean####################

#+ results="asis"
stargazer(SHOW1, SHOW3, SHOWad1, SHOWad3,
          type = "latex",
          # out = paste0(out, "HUM_tab5_JPE.tex"),
          header=FALSE,
          report = "vc*sp",
          title="Steering and Neighborhood Effects",
          model.numbers = F,
          keep = c("ofcolor"),
          covariate.labels = c("Racial Minority"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("ln(Price) Advert Home","N","Y","N","Y"),
                           c("Racial Comp Advert Home","N","Y","N","Y")))


#+ results="asis"
stargazer(SHOW1_, SHOW3_, SHOWad1_, SHOWad3_,
          type = "latex",
          # out = paste0(out, "HUM_tab5_JPE_.tex"),
          header=FALSE,
          report = "vc*sp",
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

#' We note that in *Table 5*, the authors merge tables from two different models, but only report the R2 for the second table. While these differences do not alter the direction of the results, it would have been more transparent to report the R2 separately for the two models.
#' 

#' # Robustness Check 1: Imputing Missing Data

#' First check the missing data

# Smaller subset of data for easier inspection
# recs_trial_final %>%
  # select(manualworkerId:att_check2_raw, 
  #        condition:condition_dum) %>%
  # vis_miss

recs_trial_final <- readRDS("adsprocessed_JPE.rds")

list.var <- c(
  # "home_av", "ofcolor", "market", 
  "CONTROL", "SEQUENCE.x", "month", "HCITY", "ARELATE2", "HHMTYPE", "SAPPTAM", "TSEX.x", "THHEGAI", "TPEGAI", "THIGHEDU", "TCURTENR", "ALGNCUR", "AELNG1", "DPMTEXP", "AMOVERS", "age", "ALEASETP", "ACAROWN", "w2012pc_Ad", "b2012pc_Ad", "a2012pc_Ad", "hisp2012pc_Ad", "logAdPrice", "APRACE", "STOTUNIT", "SAVLBAD")

# Need logical and character variables as factors for missForest
# "Error: Can not handle categorical predictors with more than 53 categories."
new.data <- recs_trial_final %>% 
  # select(-c(date.x, datepos.x, date.y, datepos.y), -where(is.factor)) %>% 
  # Dates not allowed
  # Just too many variables to be done at 4pm, so need to only select variables part of the analyses
  select(all_of(list.var)) %>%
  mutate(across(c(where(is.character), where(is.logical)), as.factor)) %>%
  # Too many categories (> 53), so remove corresponding factors
  select(-c(HCITY, CONTROL)) %>%
  as.data.frame()

new.data %>%
  select(where(is.factor)) %>%
  lapply(levels)

# Check how many missing values per variable
#+ echo=FALSE, include=FALSE
lapply(new.data, \(x) sum(is.na(x)))
  
# Parallel processing
registerDoParallel(cores = 18)

# Variables
#+ echo=FALSE, include=FALSE
x <- Sys.time()
x
set.seed(100)
data.imp <- missForest(new.data, verbose = TRUE, parallelize = "variables")
y <- Sys.time()
y

y - x

# Extract imputed dataset
recs_trial_final_imputed <- data.imp$ximp

#+ echo=FALSE, include=FALSE
lapply(recs_trial_final_imputed, \(x) sum(is.na(x)))

# Add back HCITY, CONTROL
recs_trial_final <- recs_trial_final_imputed %>%
  mutate(CONTROL = recs_trial_final$CONTROL,
         HCITY = recs_trial_final$HCITY)

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

recs_trial_final$home_av <- recs_trial_final$SAVLBAD
recs_trial_final$home_av[recs_trial_final$home_av<0] <- NA
recs_trial_final$home_av[recs_trial_final$home_av>1] <- 0

# Dicrimination and Showings
# STEERING AND white NEIGHBORHOOD 

SHOW1 <- felm(show ~ ofcolor | CONTROL + SEQUENCE.x + month + HCITY + market + ARELATE2 + HHMTYPE + SAPPTAM + TSEX.x + THHEGAI + TPEGAI + THIGHEDU + TCURTENR +  ALGNCUR + AELNG1 + DPMTEXP + AMOVERS + age + ALEASETP + ACAROWN| 0 | CONTROL, data  = recs_trial_final)
SHOW3 <- felm(show ~ ofcolor + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x + month + HCITY + market + ARELATE2 + HHMTYPE + SAPPTAM + TSEX.x + THHEGAI + TPEGAI + THIGHEDU + TCURTENR +  ALGNCUR + AELNG1 + DPMTEXP + AMOVERS + age + ALEASETP + ACAROWN| 0 |CONTROL, data  = recs_trial_final)

SHOW1_ <- felm(show ~ APRACE | CONTROL + SEQUENCE.x + month + HCITY + market + ARELATE2 + HHMTYPE + SAPPTAM + TSEX.x + THHEGAI + TPEGAI + THIGHEDU + TCURTENR +  ALGNCUR + AELNG1 + DPMTEXP + AMOVERS + age + ALEASETP + ACAROWN| 0 | CONTROL, data  = recs_trial_final)
SHOW3_ <- felm(show ~ APRACE + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x + month + HCITY + market + ARELATE2 + HHMTYPE + SAPPTAM + TSEX.x + THHEGAI + TPEGAI + THIGHEDU + TCURTENR +  ALGNCUR + AELNG1 + DPMTEXP + AMOVERS + age + ALEASETP + ACAROWN| 0 |CONTROL, data  = recs_trial_final)


##########################################################################################################################################################################

# STEERING 

SHOWad1 <- felm(home_av ~ ofcolor | CONTROL + SEQUENCE.x + month + HCITY + market + ARELATE2 + HHMTYPE + SAPPTAM + TSEX.x + THHEGAI + TPEGAI + THIGHEDU + TCURTENR +  ALGNCUR + AELNG1 + DPMTEXP + AMOVERS + age + ALEASETP + ACAROWN| 0 | CONTROL, data  = recs_trial_final)
SHOWad3 <- felm(home_av ~ ofcolor + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x + month + HCITY + market + ARELATE2 + HHMTYPE + SAPPTAM + TSEX.x + THHEGAI + TPEGAI + THIGHEDU + TCURTENR +  ALGNCUR + AELNG1 + DPMTEXP + AMOVERS + age + ALEASETP + ACAROWN| 0 |CONTROL, data  = recs_trial_final)

SHOWad1_ <- felm(home_av ~ APRACE | CONTROL + SEQUENCE.x + month + HCITY + market + ARELATE2 + HHMTYPE + SAPPTAM + TSEX.x + THHEGAI + TPEGAI + THIGHEDU + TCURTENR +  ALGNCUR + AELNG1 + DPMTEXP + AMOVERS + age + ALEASETP + ACAROWN| 0 | CONTROL, data  = recs_trial_final)
SHOWad3_ <- felm(home_av ~ APRACE + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x + month + HCITY + market + ARELATE2 + HHMTYPE + SAPPTAM + TSEX.x + THHEGAI + TPEGAI + THIGHEDU + TCURTENR +  ALGNCUR + AELNG1 + DPMTEXP + AMOVERS + age + ALEASETP + ACAROWN| 0 |CONTROL, data  = recs_trial_final)

### GENERATE TABLES
# out <- "C:/Users/genin/OneDrive/Documents/Git/Discrimination/output/"

###########Control Group Mean####################

#+ results="asis"
stargazer(SHOW1, SHOW3, SHOWad1, SHOWad3,
          type = "latex",
          # out = paste0(out, "HUM_tab5_JPE.tex"),
          header=FALSE,
          report = "vc*sp",
          title="Steering and Neighborhood Effects",
          model.numbers = F,
          keep = c("ofcolor"),
          covariate.labels = c("Racial Minority"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("ln(Price) Advert Home","N","Y","N","Y"),
                           c("Racial Comp Advert Home","N","Y","N","Y")))


#+ results="asis"
stargazer(SHOW1_, SHOW3_, SHOWad1_, SHOWad3_,
          type = "latex",
          # out = paste0(out, "HUM_tab5_JPE_.tex"),
          header=FALSE,
          report = "vc*sp",
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

