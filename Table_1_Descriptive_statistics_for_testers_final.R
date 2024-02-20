# -------------------------------------------------------------------------------------------------- #
#   Generate descriptive statistics tables for HUD Discrimination paper                              #
#   R-Version: 4.04                                                                                  #                                                             #
#   Date Last Modification: 12/01/2021                                                               #
# -------------------------------------------------------------------------------------------------- #

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

packages <- c("caret","dplyr", "stargazer")
lapply(packages, pkgTest)

# Input

testers.path <- "tester_assignment.csv"

# Load data 

testers <- read.csv(testers.path)

# Output

out <- "~/"


# Descriptive statistics by race ---------------------------------------------------------------

# Testers Dataset 

testers = testers[,c( "age","APRACE",
                                    "TSEX.y",
                                    "SEQUENCE",
                                    "TCURTENR",
                                    "TPEGAI",
                                    "THIGHEDU",
                                    "ARENTNOW",
                                    "ACAROWN",
                                    "ALEASETP",
                                    "AELNG1",
                                    "ALGNCUR")]

# Convert factors variables into separate indicators -------------------------------------------

# Tester Characteristics (Not Assigned) --------------------------------------------------------

# Personal Annual Income

testers$TPEGAI <- as.factor(as.character(testers$TPEGAI))

inc.dummies <- as.data.frame(predict(dummyVars(~ TPEGAI, data = testers), newdata = testers))
testers <- cbind(testers, inc.dummies)

testers$TPEGAI <- NULL
testers$`TPEGAI.-1` <- NULL

# Highest Level of Education

testers$THIGHEDU <- as.factor(as.character(testers$THIGHEDU))

edu.dummies <- as.data.frame(predict(dummyVars(~ THIGHEDU, data = testers), newdata = testers))
testers <- cbind(testers, edu.dummies)

testers$THIGHEDU <- NULL
testers$`THIGHEDU.-1` <- NULL

# Sequence (First or Second)

testers$SEQUENCE <- as.factor(as.character(testers$SEQUENCE))

seq.dummies <- as.data.frame(predict(dummyVars(~ SEQUENCE, data = testers), newdata = testers))
testers <- cbind(testers, seq.dummies)

testers$SEQUENCE <- NULL
testers$SEQUENCE.2 <- NULL

# Homeowner: Do you presently rent or own your home?
# 1 = rent
# 2 = own
# 3 = other

testers$TCURTENR <- as.factor(as.character(testers$TCURTENR))

own.dummies <- as.data.frame(predict(dummyVars(~ TCURTENR, data = testers), newdata = testers))
testers <- cbind(testers, own.dummies)

testers$TCURTENR <- NULL
testers$`TCURTENR.-1` <- NULL
testers$TCURTENR.3 <- NULL

# Assigned Characteristics ---------------------------------------------------------------------

# Lease Type
# 1 = Month-to-Month
# 2 = Lease

testers$ALEASETP <- as.factor(as.character(testers$ALEASETP))

lease.dummies <- as.data.frame(predict(dummyVars(~ ALEASETP, data = testers), newdata = testers))
testers <- cbind(testers, lease.dummies)

testers$ALEASETP <- NULL

# Generate table -------------------------------------------------------------------------------
testers_all.summary = summarise_all(testers,funs(mean),na.rm = TRUE)
testers_all.summary = subset(testers_all.summary, select = -c(2))

testers_group <- group_by(testers, APRACE)
testers.summary <- summarise_all(testers_group, funs(mean), na.rm = TRUE)

# Generate LaTeX output ------------------------------------------------------------------------

# Transpose table

testers.summary <- as.data.frame(t(testers.summary))
testers_all.summary <- as.data.frame(t(testers_all.summary))
# Remove first row

testers.summary <- testers.summary[-1,]
testers.summary = cbind(testers_all.summary,testers.summary)
# Digits

for (i in 1:ncol(testers.summary)){
  testers.summary[,i] <- as.numeric(as.character(testers.summary[,i]))
  testers.summary[,i] <- round(testers.summary[,i], digits = 3)
}

# Format row names

# Tester Characteristics (Not Assigned)

rownames(testers.summary)[which(rownames(testers.summary) == "age")] <- "Age"
rownames(testers.summary)[which(rownames(testers.summary) == "TSEX.y")] <- "Percent Male"
rownames(testers.summary)[which(rownames(testers.summary) == "SEQUENCE.1")] <- "Percent Tester Went First"
rownames(testers.summary)[which(rownames(testers.summary) == "TCURTENR.1")] <- "Percent Rented Home"
rownames(testers.summary)[which(rownames(testers.summary) == "TCURTENR.2")] <- "Percent Owned Home"

rownames(testers.summary)[which(rownames(testers.summary) == "TPEGAI.1")] <- "Under $10,000"
rownames(testers.summary)[which(rownames(testers.summary) == "TPEGAI.2")] <- "$10,000 - $19,999"
rownames(testers.summary)[which(rownames(testers.summary) == "TPEGAI.3")] <- "$20,000 - $29,999"
rownames(testers.summary)[which(rownames(testers.summary) == "TPEGAI.4")] <- "$30,000 - $39,999"
rownames(testers.summary)[which(rownames(testers.summary) == "TPEGAI.5")] <- "$40,000 - $49,999"
rownames(testers.summary)[which(rownames(testers.summary) == "TPEGAI.6")] <- "$50,000 - $74,999"
rownames(testers.summary)[which(rownames(testers.summary) == "TPEGAI.7")] <- "$75,000 - $99,999"
rownames(testers.summary)[which(rownames(testers.summary) == "TPEGAI.8")] <- "$100,000 or more"

rownames(testers.summary)[which(rownames(testers.summary) == "THIGHEDU.1")] <- "Grade school or less"
rownames(testers.summary)[which(rownames(testers.summary) == "THIGHEDU.2")] <- "Attended high school"
rownames(testers.summary)[which(rownames(testers.summary) == "THIGHEDU.3")] <- "GED"
rownames(testers.summary)[which(rownames(testers.summary) == "THIGHEDU.4")] <- "High School diploma"
rownames(testers.summary)[which(rownames(testers.summary) == "THIGHEDU.5")] <- "Attended vocational / technical school"
rownames(testers.summary)[which(rownames(testers.summary) == "THIGHEDU.6")] <- "Vocational / technical school diploma"
rownames(testers.summary)[which(rownames(testers.summary) == "THIGHEDU.7")] <- "Attended college"
rownames(testers.summary)[which(rownames(testers.summary) == "THIGHEDU.8")] <- "Associate's Degree"
rownames(testers.summary)[which(rownames(testers.summary) == "THIGHEDU.9")] <- "Bachelor's Degree"
rownames(testers.summary)[which(rownames(testers.summary) == "THIGHEDU.10")] <- "Attended graduate / professional school"
rownames(testers.summary)[which(rownames(testers.summary) == "THIGHEDU.11")] <- "Graduate / professional degree"

# Tester Characteristics (Assigned)

rownames(testers.summary)[which(rownames(testers.summary) == "ARENTNOW")] <- "Rent"
rownames(testers.summary)[which(rownames(testers.summary) == "ACAROWN")] <- "Percent Car Owner"
rownames(testers.summary)[which(rownames(testers.summary) == "ALEASETP.1")] <- "Month-to-Month"
rownames(testers.summary)[which(rownames(testers.summary) == "ALEASETP.2")] <- "Lease"
rownames(testers.summary)[which(rownames(testers.summary) == "AELNG1")] <- "Length of Employment (Years)"
rownames(testers.summary)[which(rownames(testers.summary) == "ALGNCUR")] <- "Years at Residence"


# ----------------------------------------------------------------------------------------------
# For APRACE variable: 
#   1 = White
#   2 = Black / African American
#   3 = Hispanic
#   4 = Asian
#   5 = Other (specify which)

# Drop descriptive statistics for race category 'Other'

homes.summary$V5 <- NULL
testers.summary$V5 <- NULL

# Output ---------------------------------------------------------------------------------------

# Save summary

testers.summary = testers.summary[c(1,2,26,27,7,8,9,10,11,12,13,14,17,18,19,20,21,22,23,24,15,16,3,25,4,5,6,28,29),]

saveRDS(testers.summary, paste0(out, "testers-summary.rds"))

stargazer(testers.summary,
          type = "latex",
          title = "Tester Characteristics",
          out = paste0(out, "testers-desc.tex"),
          summary = FALSE,
          covariate.labels = c("Variable",
                               "All Groups",
                               "White",
                               "African American",
                               "Hispanic",
                               "Asian"))
