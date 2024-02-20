# -------------------------------------------------------------------------------------------------- #
#   Generate descriptive statistics tables for HUD Discrimination paper                              #
#                                                                                                    #
#   R-Version: 4.04                                                                                  #                                                             #
#   Date Last Modification: 12/01/2021                                                               #
# -------------------------------------------------------------------------------------------------- #

# Clear workspace

rm(list = ls())

# Set working directory

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

packages <- c("caret","dplyr", "stargazer")
lapply(packages, pkgTest)

# Input

homes.path <- "Final Data Sets/adsprocessed_JPE.rds"
test.path = "Final Data Sets/HUDprocessed_JPE_testscores_042021.rds"

# Load data

homes <- readRDS(homes.path)
scores = readRDS(test.path)

# Output

out <- "C:/Users/genin/OneDrive/Documents/Git/Discrimination/output/"



# Descriptive statistics by race ---------------------------------------------------------------

# Homes dataset

# Subset dataset to desired variables

homes <- subset(homes,select = c(APRACE,
                                 DPMTEXP,
                                 Assault_Ad,
                                 Elementary_School_Score_Ad,
                                 povrate_Ad,
                                 skill_Ad,
                                 college_Ad,
                                 singlefamily_Ad,
                                 ownerocc_Ad,
                                 SFcount_Ad,
                                 RSEI_Ad,
                                 PM25_Ad,
                                 HHMTYPE,
                                 Price,
                                 w2012pc_Ad,
                                 b2012pc_Ad,
                                 a2012pc_Ad,
                                 hisp2012pc_Ad))

scores = subset(scores, select =c(APRACE.x,mn_avg_ol_elem_Ad,mn_avg_ol_middle_Ad))

# Convert factors variables into separate indicators -------------------------------------------

# Home Characteristics -------------------------------------------------------------------------

# Reason for Downpayment

homes$DPMTEXP <- as.factor(as.character(homes$DPMTEXP))

dpreason.dummies <- as.data.frame(predict(dummyVars(~ DPMTEXP, data = homes), newdata = homes))
homes <- cbind(homes, dpreason.dummies)

homes$DPMTEXP <- NULL

# House Type

homes$HHMTYPE <- as.factor(as.character(homes$HHMTYPE))

htype.dummies <- as.data.frame(predict(dummyVars(~ HHMTYPE, data = homes), newdata = homes))
homes <- cbind(homes, htype.dummies)

homes$HHMTYPE <- NULL
homes$`HHMTYPE.-1` <- NULL

# Order for table ------------------------------------------------------------------------------

col_order <- c("APRACE","Price", "HHMTYPE.1", "HHMTYPE.2","HHMTYPE.3" , "HHMTYPE.4" ,"HHMTYPE.5",
               "SFcount_Ad", "PM25_Ad","RSEI_Ad",
               "Assault_Ad","Elementary_School_Score_Ad","povrate_Ad","college_Ad","skill_Ad",
               "w2012pc_Ad","b2012pc_Ad","hisp2012pc_Ad","a2012pc_Ad",
               "singlefamily_Ad","ownerocc_Ad")
homes <- homes[, col_order]


# Generate table -------------------------------------------------------------------------------

homes_race <- group_by(homes, APRACE)
homes.summary <- summarise_all(homes_race, funs(mean), na.rm = TRUE)
homes.summary_all=summarise_all(homes,funs(mean),na.rm=TRUE)

scores_race=group_by(scores,APRACE.x)
scores.summary <- summarise_all(scores_race, funs(mean), na.rm = TRUE)
scores.summary_all=summarise_all(scores,funs(mean),na.rm=TRUE)

# Generate LaTeX output ------------------------------------------------------------------------

# Transpose table

homes.summary <- as.data.frame(t(homes.summary))
homes.summary_all <- as.data.frame(t(homes.summary_all))

scores.summary <- as.data.frame(t(scores.summary))
scores.summary_all <- as.data.frame(t(scores.summary_all))
# Remove first row

homes.summary <- homes.summary[-1,]
homes.summary_all <- as.data.frame(homes.summary_all[-1,])

scores.summary <- scores.summary[-1,]
scores.summary_all <- as.data.frame(scores.summary_all[-1,])

# Digits

for (i in 1:ncol(homes.summary)){
  homes.summary[,i] <- as.numeric(as.character(homes.summary[,i]))
  homes.summary[,i] <- round(homes.summary[,i], digits = 3)}

for (i in 1:ncol(homes.summary_all)){
  homes.summary_all[,i] <- as.numeric(as.character(homes.summary_all[,i]))
  homes.summary_all[,i] <- round(homes.summary_all[,i], digits = 3)}

for (i in 1:ncol(scores.summary)){
  scores.summary[,i] <- as.numeric(as.character(scores.summary[,i]))
  scores.summary[,i] <- round(scores.summary[,i], digits = 3)}

for (i in 1:ncol(scores.summary_all)){
  scores.summary_all[,i] <- as.numeric(as.character(scores.summary_all[,i]))
  scores.summary_all[,i] <- round(scores.summary_all[,i], digits = 3)}

# Format row names

# Home Characteristics

rownames(homes.summary)[which(rownames(homes.summary) == "Price")] <- "Listing Price"
rownames(homes.summary)[which(rownames(homes.summary) == "HHMTYPE.1")] <- "Single family, detached"
rownames(homes.summary)[which(rownames(homes.summary) == "HHMTYPE.2")] <- "Duplex"
rownames(homes.summary)[which(rownames(homes.summary) == "HHMTYPE.3")] <- "Rowhouse or Townhouse"
rownames(homes.summary)[which(rownames(homes.summary) == "HHMTYPE.4")] <- "Multi-family structure"
rownames(homes.summary)[which(rownames(homes.summary) == "HHMTYPE.5")] <- "Mobile home"
rownames(homes.summary)[which(rownames(homes.summary) == "SFcount_Ad")] <- "Superfund Sites"
rownames(homes.summary)[which(rownames(homes.summary) == "PM25_Ad")] <- "PM 25"
rownames(homes.summary)[which(rownames(homes.summary) == "RSEI_Ad")] <- "Risk-Screening Env. Indicators"

rownames(homes.summary_all)[which(rownames(homes.summary_all) == "Price")] <- "Listing Price"
rownames(homes.summary_all)[which(rownames(homes.summary_all) == "HHMTYPE.1")] <- "Single family, detached"
rownames(homes.summary_all)[which(rownames(homes.summary_all) == "HHMTYPE.2")] <- "Duplex"
rownames(homes.summary_all)[which(rownames(homes.summary_all) == "HHMTYPE.3")] <- "Rowhouse or Townhouse"
rownames(homes.summary_all)[which(rownames(homes.summary_all) == "HHMTYPE.4")] <- "Multi-family structure"
rownames(homes.summary_all)[which(rownames(homes.summary_all) == "HHMTYPE.5")] <- "Mobile home"
rownames(homes.summary_all)[which(rownames(homes.summary_all) == "SFcount_Ad")] <- "Superfund Sites"
rownames(homes.summary_all)[which(rownames(homes.summary_all) == "PM25_Ad")] <- "PM 25"
rownames(homes.summary_all)[which(rownames(homes.summary_all) == "RSEI_Ad")] <- "Risk-Screening Env. Indicators"

# Neighborhood Characteristics

rownames(homes.summary)[which(rownames(homes.summary) == "w2012pc_Ad")] <- "Percent White"
rownames(homes.summary)[which(rownames(homes.summary) == "b2012pc_Ad")] <- "Percent African American"
rownames(homes.summary)[which(rownames(homes.summary) == "hisp2012pc_Ad")] <- "Percent Hispanic"
rownames(homes.summary)[which(rownames(homes.summary) == "a2012pc_Ad")] <- "Percent Asian"
rownames(homes.summary)[which(rownames(homes.summary) == "college_Ad")] <- "Percent College Graduate"
rownames(homes.summary)[which(rownames(homes.summary) == "skill_Ad")] <- "Percent High Skill"
rownames(homes.summary)[which(rownames(homes.summary) == "povrate_Ad")] <- "Poverty Rate"
rownames(homes.summary)[which(rownames(homes.summary) == "Assault_Ad")] <- "Assaults"
rownames(homes.summary)[which(rownames(homes.summary) == "Elementary_School_Score_Ad")] <- "Elem. School Quality"
rownames(homes.summary)[which(rownames(homes.summary) == "singlefamily_Ad")] <- "Single-Parent HH"
rownames(homes.summary)[which(rownames(homes.summary) == "ownerocc_Ad")] <- "Ownership Rate"

rownames(homes.summary_all)[which(rownames(homes.summary_all) == "w2012pc_Ad")] <- "Percent White"
rownames(homes.summary_all)[which(rownames(homes.summary_all) == "b2012pc_Ad")] <- "Percent African American"
rownames(homes.summary_all)[which(rownames(homes.summary_all) == "hisp2012pc_Ad")] <- "Percent Hispanic"
rownames(homes.summary_all)[which(rownames(homes.summary_all) == "a2012pc_Ad")] <- "Percent Asian"
rownames(homes.summary_all)[which(rownames(homes.summary_all) == "college_Ad")] <- "Percent College Graduate"
rownames(homes.summary_all)[which(rownames(homes.summary_all) == "skill_Ad")] <- "Percent High Skill"
rownames(homes.summary_all)[which(rownames(homes.summary_all) == "povrate_Ad")] <- "Poverty Rate"
rownames(homes.summary_all)[which(rownames(homes.summary_all) == "Assault_Ad")] <- "Assaults"
rownames(homes.summary_all)[which(rownames(homes.summary_all) == "Elementary_School_Score_Ad")] <- "Elem. School Quality"
rownames(homes.summary_all)[which(rownames(homes.summary_all) == "singlefamily_Ad")] <- "Single-Parent HH"
rownames(homes.summary_all)[which(rownames(homes.summary_all) == "ownerocc_Ad")] <- "Ownership Rate"

rownames(scores.summary)[which(rownames(scores.summary) == "mn_avg_ol_elem_Ad")] <- "Elem. School Test Score"
rownames(scores.summary)[which(rownames(scores.summaryl) == "mn_avg_ol_middle_Ad")] <- "Midd. School Test Score"

# ----------------------------------------------------------------------------------------------
# For APRACE variable:
#   1 = White
#   2 = Black / African American
#   3 = Hispanic
#   4 = Asian
#   5 = Other (specify which)

# Drop descriptive statistics for race category 'Other'

homes.summary$`5` <- NULL
homes.summary$all=homes.summary_all$`homes.summary_all[-1, ]`

scores.summary$`5` <- NULL
scores.summary$all=scores.summary_all$`scores.summary_all[-1, ]`

# Output ---------------------------------------------------------------------------------------

# Save summary

homes.summary = homes.summary[c(1,2,3,4,5,6,10,11,12,14,13,19,20,15,16,17,18,7,9,8),]
homes.summary = homes.summary[,c(6,1,2,3,4)]

scores.summary = scores.summary[,c(6,1,2,3,4)]

saveRDS(homes.summary, paste0(out, "homes-summary.rds"))

# Convert to LaTeX using stargazer

stargazer(homes.summary,
          type = "latex",
          title = "Home and Neighborhood Characteristics",
          out = paste0(out, "homes-desc_all.tex"),
          summary = FALSE,
          model.numbers = F, digits=3,
          keep.stat=c("n"),
          covariate.labels = c("Variable",
                               "White",
                               "African American",
                               "Hispanic",
                               "Asian",
                               "Other", "All Groups"))

# First Column of Neighborhood Characteristics

stargazer(scores.summary,
          type = "latex",
          title = "Home and Neighborhood Characteristics",
          out = paste0(out, "homes-desc_scores.tex"),
          summary = FALSE,
          model.numbers = F, digits=3,
          covariate.labels = c("Variable",
                               "White",
                               "African American",
                               "Hispanic",
                               "Asian",
                               "Other", "All Groups"))
