# ---------------------------------------------------------------------------------------------- #
#   Generate balance tables for HUD Discrimination paper                                         #
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

packages <- c("caret", "dplyr", "lfe", "stargazer")
lapply(packages, pkgTest)

# Input

testers.path <- "tester_assignment.csv"

# Load data 

testers <- read.csv(testers.path, header = TRUE)

# Output directory

out <- "~/"

# Balance statistics advertised homes by race --------------------------------------------------

# ***This is using self identified racial categories***
# APRACE: 
#   1 = White 
#   2 = Black/African-American 
#   3 = Hispanic 
#   4 = Asian/Pacific Islander 
#   5 = Other (Specify)

# APRACE is the variable of interest

testers$APRACE <- as.factor(testers$APRACE)

# Tester Characteristics -------------------------------------------------------------------

# Age ------------------------------------------------------------------------------------------

t6 <- felm(age ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t6.coef <- as.data.frame(coef(summary(t6)))
names(t6.coef)[which(names(t6.coef) == "Estimate")] <- "Age"
t6.coef <- as.data.frame(t(t6.coef))

rm(t6)

# Gender ---------------------------------------------------------------------------------------

testers$TSEX.x <- as.factor(testers$TSEX.x)

gender.dummies <- as.data.frame(predict(dummyVars(~ TSEX.x, data = testers), newdata = testers))
testers <- cbind(testers, gender.dummies)

t5 <- felm(TSEX.x.1 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t5.coef <- as.data.frame(coef(summary(t5)))
names(t5.coef)[which(names(t5.coef) == "Estimate")] <- "Percent Male"
t5.coef <- as.data.frame(t(t5.coef))

rm(t5)

# Homeowner ------------------------------------------------------------------------------------

# Do you presently rent or own your home? 
# 1 = rent
# 2 = own
# 3 = other (specify)

testers$TCURTENR <- as.factor(testers$TCURTENR)

own.dummies <- as.data.frame(predict(dummyVars(~ TCURTENR, data = testers), newdata = testers))
testers <- cbind(testers, own.dummies)

# rent home

t10.1 <- felm(TCURTENR.1 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t10.1.coef <- as.data.frame(coef(summary(t10.1)))
names(t10.1.coef)[which(names(t10.1.coef) == "Estimate")] <- "Percent Rented Home"
t10.1.coef <- as.data.frame(t(t10.1.coef))

rm(t10.1)

# own home

t10.2 <- felm(TCURTENR.2 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t10.2.coef <- as.data.frame(coef(summary(t10.2)))
names(t10.2.coef)[which(names(t10.2.coef) == "Estimate")] <- "Percent Owned Home"
t10.2.coef <- as.data.frame(t(t10.2.coef))

rm(t10.2)


# Combine into one dataframe of the first panel

t10.coef <- rbind(t6.coef, t5.coef, t10.1.coef, t10.2.coef)

# Drop race category 'Other'

t10.coef$APRACE5 <- NULL

# Drop (Intercept)

t10.coef$`(Intercept)` <- NULL 

# Panel B - Income ---------------------------------------------------------------------------------------

# Run model separately for each income bin

testers$TPEGAI <- as.factor(testers$TPEGAI)

inc.dummies <- as.data.frame(predict(dummyVars(~ TPEGAI, data = testers), newdata = testers))
testers <- cbind(testers, inc.dummies)

# 1 = Under $10,000

t7.1 <- felm(TPEGAI.1 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t7.1.coef <- as.data.frame(coef(summary(t7.1)))
names(t7.1.coef)[which(names(t7.1.coef) == "Estimate")] <- "Under $10,000"
t7.1.coef <- as.data.frame(t(t7.1.coef))

rm(t7.1)

# 2 = $10,000 - $19,999

t7.2 <- felm(TPEGAI.2 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t7.2.coef <- as.data.frame(coef(summary(t7.2)))
names(t7.2.coef)[which(names(t7.2.coef) == "Estimate")] <- "$10,000 - $19,999"
t7.2.coef <- as.data.frame(t(t7.2.coef))

rm(t7.2)

# 3 = $20,000 - $29,999

t7.3 <- felm(TPEGAI.3 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t7.3.coef <- as.data.frame(coef(summary(t7.3)))
names(t7.3.coef)[which(names(t7.3.coef) == "Estimate")] <- "$20,000 - $29,999"
t7.3.coef <- as.data.frame(t(t7.3.coef))

rm(t7.3)

# 4 = $30,000 - $39,999

t7.4 <- felm(TPEGAI.4 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t7.4.coef <- as.data.frame(coef(summary(t7.4)))
names(t7.4.coef)[which(names(t7.4.coef) == "Estimate")] <- "$30,000 - $39,999"
t7.4.coef <- as.data.frame(t(t7.4.coef))

rm(t7.4)

# 5 = $40,000 - $49,999

t7.5 <- felm(TPEGAI.5 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t7.5.coef <- as.data.frame(coef(summary(t7.5)))
names(t7.5.coef)[which(names(t7.5.coef) == "Estimate")] <- "$40,000 - $49,999"
t7.5.coef <- as.data.frame(t(t7.5.coef))

rm(t7.5)

# 6 = $50,000 - $74,999

t7.6 <- felm(TPEGAI.6 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t7.6.coef <- as.data.frame(coef(summary(t7.6)))
names(t7.6.coef)[which(names(t7.6.coef) == "Estimate")] <- "$50,000 - $74,999"
t7.6.coef <- as.data.frame(t(t7.6.coef))

rm(t7.6)

# 7 = $75,000 - 99,999

t7.7 <- felm(TPEGAI.7 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t7.7.coef <- as.data.frame(coef(summary(t7.7)))
names(t7.7.coef)[which(names(t7.7.coef) == "Estimate")] <- "$75,000 - $99,999"
t7.7.coef <- as.data.frame(t(t7.7.coef))

rm(t7.7)

# 8 = $100,000 or more

t7.8 <- felm(TPEGAI.8 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t7.8.coef <- as.data.frame(coef(summary(t7.8)))
names(t7.8.coef)[which(names(t7.8.coef) == "Estimate")] <- "$100,000 or more"
t7.8.coef <- as.data.frame(t(t7.8.coef))

rm(t7.8)


# Generate table

# Combine into one data frame

t7.coef <- rbind(t7.1.coef,
                 t7.2.coef,
                 t7.3.coef,
                 t7.4.coef,
                 t7.5.coef,
                 t7.6.coef,
                 t7.7.coef,
                 t7.8.coef)

# Drop race category 'Other'

t7.coef$APRACE5 <- NULL

# Drop (Intercept)

t7.coef$`(Intercept)` <- NULL

# Panel C - Education ------------------------------------------------------------------------------------

testers$THIGHEDU <- as.factor(testers$THIGHEDU)

edu.dummies <- as.data.frame(predict(dummyVars(~ THIGHEDU, data = testers), newdata = testers))
testers <- cbind(testers, edu.dummies)

# 1 = Attended High School

t9.2 <- felm(THIGHEDU.2 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t9.2.coef <- as.data.frame(coef(summary(t9.2)))
names(t9.2.coef)[which(names(t9.2.coef) == "Estimate")] <- "Attended High School"
t9.2.coef <- as.data.frame(t(t9.2.coef))

rm(t9.2)

# 2 = GED

t9.3 <- felm(THIGHEDU.3 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t9.3.coef <- as.data.frame(coef(summary(t9.3)))
names(t9.3.coef)[which(names(t9.3.coef) == "Estimate")] <- "GED"
t9.3.coef <- as.data.frame(t(t9.3.coef))

rm(t9.3)

# 3 = High School Diploma

t9.4 <- felm(THIGHEDU.4 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t9.4.coef <- as.data.frame(coef(summary(t9.4)))
names(t9.4.coef)[which(names(t9.4.coef) == "Estimate")] <- "High School Diploma"
t9.4.coef <- as.data.frame(t(t9.4.coef))

rm(t9.4)

# 4 = Attended Vocational / Technical School

t9.5 <- felm(THIGHEDU.5 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t9.5.coef <- as.data.frame(coef(summary(t9.5)))
names(t9.5.coef)[which(names(t9.5.coef) == "Estimate")] <- "Attended Vocational / Technical School"
t9.5.coef <- as.data.frame(t(t9.5.coef))

rm(t9.5)

# 5 = Vocational / Technical School Diploma

t9.6 <- felm(THIGHEDU.6 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t9.6.coef <- as.data.frame(coef(summary(t9.6)))
names(t9.6.coef)[which(names(t9.6.coef) == "Estimate")] <- "Vocational / Technical School Diploma"
t9.6.coef <- as.data.frame(t(t9.6.coef))

rm(t9.6)

# 6 = Attended College

t9.7 <- felm(THIGHEDU.7 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t9.7.coef <- as.data.frame(coef(summary(t9.7)))
names(t9.7.coef)[which(names(t9.7.coef) == "Estimate")] <- "Attended College"
t9.7.coef <- as.data.frame(t(t9.7.coef))

rm(t9.7)


# 7 = Associate's Degree

t9.8 <- felm(THIGHEDU.8 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t9.8.coef <- as.data.frame(coef(summary(t9.8)))
names(t9.8.coef)[which(names(t9.8.coef) == "Estimate")] <- "Associate's Degree"
t9.8.coef <- as.data.frame(t(t9.8.coef))

rm(t9.8)

# 8 = Bachelor's Degree 

t9.9 <- felm(THIGHEDU.9 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t9.9.coef <- as.data.frame(coef(summary(t9.9)))
names(t9.9.coef)[which(names(t9.9.coef) == "Estimate")] <- "Bachelor's Degree"
t9.9.coef <- as.data.frame(t(t9.9.coef))

rm(t9.9)

# 9 = Attended Graduate / Professional School

t9.10 <- felm(THIGHEDU.10 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t9.10.coef <- as.data.frame(coef(summary(t9.10)))
names(t9.10.coef)[which(names(t9.10.coef) == "Estimate")] <- "Attended Graduate / Professional School"
t9.10.coef <- as.data.frame(t(t9.10.coef))

rm(t9.10)

# 10 = Graduate / Professional Degree

t9.11 <- felm(THIGHEDU.11 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t9.11.coef <- as.data.frame(coef(summary(t9.11)))
names(t9.11.coef)[which(names(t9.11.coef) == "Estimate")] <- "Graduate / Professional Degree"
t9.11.coef <- as.data.frame(t(t9.11.coef))

rm(t9.11)

# Generate table

# Combine into one data frame

t9.coef <- rbind(t9.2.coef,
                 t9.3.coef,
                 t9.4.coef,
                 t9.5.coef,
                 t9.6.coef,
                 t9.7.coef,
                 t9.8.coef,
                 t9.9.coef,
                 t9.10.coef,
                 t9.11.coef)

# Drop race category 'Other'

t9.coef$APRACE5 <- NULL

# Drop (Intercept)

t9.coef$`(Intercept)` <- NULL


# Panel D - Assigned Characteristics----------------------------------------------------------

# Test Sequence --------------------------------------------------------------------------------

testers$SEQUENCE <- as.factor(testers$SEQUENCE)

seq.dummies <- as.data.frame(predict(dummyVars(~ SEQUENCE, data = testers), newdata = testers))
testers <- cbind(testers, seq.dummies)


t1 <- felm(SEQUENCE.1 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t1.coef <- as.data.frame(coef(summary(t1)))
names(t1.coef)[which(names(t1.coef) == "Estimate")] <- "Percent Tester Went First"
t1.coef <- as.data.frame(t(t1.coef))

rm(t1)

# Car Owner ------------------------------------------------------------------------------------

# Tester owns a car?
# 1 = Yes 
# 0 = No

testers$ACAROWN <- as.factor(testers$ACAROWN)

t15 <- felm(ACAROWN ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t15.coef <- as.data.frame(coef(summary(t15)))
names(t15.coef)[which(names(t15.coef) == "Estimate")] <- "Percent Car Owner"
t15.coef <- as.data.frame(t(t15.coef))

rm(t15)

# Length of Employment (Assigned) -------------------------------------------------------------------------

t11 <- felm(AELNG1 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t11.coef <- as.data.frame(coef(summary(t11)))
names(t11.coef)[which(names(t11.coef) == "Estimate")] <- "Length of Employment (Years)"
t11.coef <- as.data.frame(t(t11.coef))

rm(t11)


# Years at Residence (Assigned) ---------------------------------------------------------------------------

t13 <- felm(ALGNCUR ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t13.coef <- as.data.frame(coef(summary(t13)))
names(t13.coef)[which(names(t13.coef) == "Estimate")] <- "Years at Residence"
t13.coef <- as.data.frame(t(t13.coef))

rm(t13)

# Generate table

tassig.coef <- rbind(t1.coef,
                 t15.coef,
                 t11.coef,
                 t13.coef)

# Drop race category 'Other'

tassig.coef$APRACE5 <- NULL

# without CONTROL
# Drop (Intercept)

tassig.coef$`(Intercept)` <- NULL


# Panel D - Lease Type -----------------------------------------------------------------------------------

testers$ALEASETP <- as.factor(testers$ALEASETP)

leasetp.dummies <- as.data.frame(predict(dummyVars(~ ALEASETP, data = testers), newdata = testers))
testers <- cbind(testers, leasetp.dummies)

# 1 = Month - to - Month

t14.1 <- felm(ALEASETP.1 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t14.1.coef <- as.data.frame(coef(summary(t14.1)))
names(t14.1.coef)[which(names(t14.1.coef) == "Estimate")] <- "Month-to-Month"
t14.1.coef <- as.data.frame(t(t14.1.coef))

rm(t14.1)

# 2 = Lease 

t14.2 <- felm(ALEASETP.2 ~ APRACE | CONTROL | 0 | CONTROL, data = testers)

t14.2.coef <- as.data.frame(coef(summary(t14.2)))
names(t14.2.coef)[which(names(t14.2.coef) == "Estimate")] <- "Lease"
t14.2.coef <- as.data.frame(t(t14.2.coef))

rm(t14.2)

# Generate table

# Combine into one dataframe

t14.coef <- rbind(t14.1.coef, t14.2.coef)

# Drop race category 'Other'

t14.coef$APRACE5 <- NULL

# without CONTROL
# Drop (Intercept)

t14.coef$`(Intercept)` <- NULL

# Convert into LaTeX using stargazer

# Convert into LaTeX using stargazer

stargazer(t10.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "panel_A.tex"))


stargazer(t7.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "Panel_b_income.tex"))


stargazer(t9.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "Panel_C_edu.tex"))


stargazer(tassig.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "panel_d_assigned_ch.tex"))

stargazer(t14.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "Panel_E_blease.tex"))



