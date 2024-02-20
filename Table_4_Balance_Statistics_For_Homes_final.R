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

testers.path <- "HUDprocessed_JPE_census_042021.rds"
homes.path <- "adsprocessed_JPE.rds"
test.path = "HUDprocessed_JPE_testscores_042021.rds"

# Load data 

testers <- readRDS(testers.path)
homes <- readRDS(homes.path)
scores = readRDS(test.path)

# Output directory

out <- "~/"



# Home and Neighborhood Characteristics --------------------------------------------------------

# without CONTROL to demonstrate the effect of house characteristics without assigning tester pairs

h1 <- felm(Price ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

h1.coef <- as.data.frame(coef(summary(h1)))
names(h1.coef)[which(names(h1.coef) == "Estimate")] <- "Listing Price"
h1.coef <- as.data.frame(t(h1.coef))


# Home Type ------------------------------------------------------------------------------------

# What type of building is it? 

homes$HHMTYPE <- as.factor(homes$HHMTYPE)

htype.dummies <- as.data.frame(predict(dummyVars(~ HHMTYPE, data = homes), newdata = homes))
homes <- cbind(homes, htype.dummies)


# 1 = Single-family Detached

h2.1 <- felm(HHMTYPE.1 ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

h2.1.coef <- as.data.frame(coef(summary(h2.1)))
names(h2.1.coef)[which(names(h2.1.coef) == "Estimate")] <- "Single-family Detached"
h2.1.coef <- as.data.frame(t(h2.1.coef))


# 2 = Duplex

h2.2 <- felm(HHMTYPE.2 ~ APRACE| CONTROL | 0 | CONTROL, data = homes)

h2.2.coef <- as.data.frame(coef(summary(h2.2)))
names(h2.2.coef)[which(names(h2.2.coef) == "Estimate")] <- "Duplex"
h2.2.coef <- as.data.frame(t(h2.2.coef))


# 3 = Rowhouse or Townhouse

h2.3 <- felm(HHMTYPE.3 ~ APRACE| CONTROL | 0 | CONTROL, data = homes)

h2.3.coef <- as.data.frame(coef(summary(h2.3)))
names(h2.3.coef)[which(names(h2.3.coef) == "Estimate")] <- "Rowhouse or Townhouse"
h2.3.coef <- as.data.frame(t(h2.3.coef))

# 4 = Multi-family Structure

h2.4 <- felm(HHMTYPE.4 ~ APRACE| CONTROL | 0 | CONTROL, data = homes)

h2.4.coef <- as.data.frame(coef(summary(h2.4)))
names(h2.4.coef)[which(names(h2.4.coef) == "Estimate")] <- "Multi-family Structure"
h2.4.coef <- as.data.frame(t(h2.4.coef))


# 5 = Mobile Home 

h2.5 <- felm(HHMTYPE.5 ~ APRACE| CONTROL | 0 | CONTROL, data = homes)

h2.5.coef <- as.data.frame(coef(summary(h2.5)))
names(h2.5.coef)[which(names(h2.5.coef) == "Estimate")] <- "Mobile Home"
h2.5.coef <- as.data.frame(t(h2.5.coef))


# Generate table

# Combine into one data frame 

h2.coef <- rbind(h1.coef,
                 h2.1.coef,
                 h2.2.coef,
                 h2.3.coef,
                 h2.4.coef,
                 h2.5.coef)

# Drop race category 'Other'

h2.coef$APRACE5 <- NULL

# without CONTROL
# Exclude (Intercept)

h2.coef$`(Intercept)` <- NULL


# Pollution Measurements in Advertised Homes ---------------------------------------------------

# Superfund Sites 

p1 <- felm(SFcount_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

p1.coef <- as.data.frame(coef(summary(p1)))
names(p1.coef)[which(names(p1.coef) == "Estimate")] <- "Superfund Sites"
p1.coef <- as.data.frame(t(p1.coef))

rm(p1)

# Risk-Screening Environmental Indicators

p3 <- felm(RSEI_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

p3.coef <- as.data.frame(coef(summary(p3)))
names(p3.coef)[which(names(p3.coef) == "Estimate")] <- "Risk-Screening Env. Indicator"
p3.coef <- as.data.frame(t(p3.coef))

rm(p3)

# Respiratory Hazard Index

p2 <- felm(PM25_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

p2.coef <- as.data.frame(coef(summary(p2)))
names(p2.coef)[which(names(p2.coef) == "Estimate")] <- "Respiratory Hazard Index"
p2.coef <- as.data.frame(t(p2.coef))

rm(p2)


# Generate table -------------------------------------------------------------------------------

p.coef <- rbind(p1.coef,
                p2.coef,
                p3.coef)

# Drop race category 'Other'

p.coef$APRACE5 <- NULL


# Neighborhood Block Group Characteristics -----------------------------------------------------------

# Elementary School Test Score

scores$APRACE=scores$APRACE.x

es <- felm(mn_avg_ol_elem_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = scores)

es.coef <- as.data.frame(coef(summary(es)))
names(es.coef)[which(names(es.coef) == "Estimate")] <- "Elem. School Test Score"
es.coef <- as.data.frame(t(es.coef))

rm(es)

# Middle School Test Score

mi <- felm(mn_avg_ol_middle_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = scores)

mi.coef <- as.data.frame(coef(summary(mi)))
names(mi.coef)[which(names(mi.coef) == "Estimate")] <- "Midd. School Test Score"
mi.coef <- as.data.frame(t(mi.coef))

rm(mi)

# Assaults

cbg1 <- felm(Assault_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

cbg1.coef <- as.data.frame(coef(summary(cbg1)))
names(cbg1.coef)[which(names(cbg1.coef) == "Estimate")] <- "Assault"
cbg1.coef <- as.data.frame(t(cbg1.coef))

rm(cbg1)

# Elementary School

cbg2 <- felm(Elementary_School_Score_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

cbg2.coef <- as.data.frame(coef(summary(cbg2)))
names(cbg2.coef)[which(names(cbg2.coef) == "Estimate")] <- "Elem. School Quality"
cbg2.coef <- as.data.frame(t(cbg2.coef))

rm(cbg2)

# Poverty Rate

cbg3 <- felm(povrate_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

cbg3.coef <- as.data.frame(coef(summary(cbg3)))
names(cbg3.coef)[which(names(cbg3.coef) == "Estimate")] <- "Poverty Rate"
cbg3.coef <- as.data.frame(t(cbg3.coef))

rm(cbg3)

# Percent High Skill

cbg4 <- felm(skill_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

cbg4.coef <- as.data.frame(coef(summary(cbg4)))
names(cbg4.coef)[which(names(cbg4.coef) == "Estimate")] <- "Percent High Skill"
cbg4.coef <- as.data.frame(t(cbg4.coef))

rm(cbg4)

# Percent College Graduates

cbg5 <- felm(college_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

cbg5.coef <- as.data.frame(coef(summary(cbg5)))
names(cbg5.coef)[which(names(cbg5.coef) == "Estimate")] <- "Percent College Educated"
cbg5.coef <- as.data.frame(t(cbg5.coef))

rm(cbg5)

# Single Parent Family

cbg6 <- felm(singlefamily_Ad~ APRACE | CONTROL | 0 | CONTROL, data = homes)

cbg6.coef <- as.data.frame(coef(summary(cbg6)))
names(cbg6.coef)[which(names(cbg6.coef) == "Estimate")] <- "Single-Parent HH"
cbg6.coef <- as.data.frame(t(cbg6.coef))

rm(cbg6)

# Ownership Rate

cbg7 <- felm(ownerocc_Ad~ APRACE | CONTROL | 0 | CONTROL, data = homes)

cbg7.coef <- as.data.frame(coef(summary(cbg7)))
names(cbg7.coef)[which(names(cbg7.coef) == "Estimate")] <- "Single-Parent HH"
cbg7.coef <- as.data.frame(t(cbg7.coef))

rm(cbg7)



# Percent White

cbg8 <- felm(w2012pc_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

cbg8.coef <- as.data.frame(coef(summary(cbg8)))
names(cbg8.coef)[which(names(cbg8.coef) == "Estimate")] <- "Percent White"
cbg8.coef <- as.data.frame(t(cbg8.coef))

rm(cbg8)

# Percent African American

cbg9 <- felm(b2012pc_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

cbg9.coef <- as.data.frame(coef(summary(cbg9)))
names(cbg9.coef)[which(names(cbg9.coef) == "Estimate")] <- "Percent African American"
cbg9.coef <- as.data.frame(t(cbg9.coef))

rm(cbg9)


# Percent Hispanic

cbg10 <- felm(hisp2012pc_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

cbg10.coef <- as.data.frame(coef(summary(cbg10)))
names(cbg10.coef)[which(names(cbg10.coef) == "Estimate")] <- "Percent Hispanic"
cbg10.coef <- as.data.frame(t(cbg10.coef))

rm(cbg10)

# Percent Asian

cbg11 <- felm(a2012pc_Ad ~ APRACE | CONTROL | 0 | CONTROL, data = homes)

cbg11.coef <- as.data.frame(coef(summary(cbg11)))
names(cbg11.coef)[which(names(cbg11.coef) == "Estimate")] <- "Percent Asian"
cbg11.coef <- as.data.frame(t(cbg11.coef))

rm(cbg11)







# Generate table -------------------------------------------------------------------------------

cbg.coef <- rbind(mi.coef,
                  es.coef,
                cbg1.coef,
                cbg2.coef,
                cbg3.coef,
                cbg4.coef,
                cbg5.coef,
                cbg6.coef,
                cbg7.coef,
                cbg8.coef,
                cbg9.coef,
                cbg10.coef,
                cbg11.coef)

# Drocbg race category 'Other'

cbg.coef$APRACE5 <- NULL

# Convert into LaTeX using stargazer


stargazer(h2.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          df = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "Panel_A_home.tex"))

stargazer(cbg.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "Panel_B_Neighborhoos_Characteristics.tex"))


stargazer(p.coef,
          type = "latex",
          title = "Balance Statistics for Advertised Homes by Race",
          summary = FALSE,
          covariate.labels = c("Variable",
                               "African American",
                               "Hispanic",
                               "Asian"),
          out = paste0(out, "Panel_C_Pollution_Measurements.tex"))



