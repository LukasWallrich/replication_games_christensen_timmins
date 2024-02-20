# ---------------------------------------------------------------------------------------------- #
#   Generate Table 12. Discriminatory Steering: Median Income in Neighborhood                    #
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
recs_trial_final$lnmincome_rec= log(recs_trial_final$medincome_Rec)


#Construct samples for testers who are mothers and who are families

recs_trial_final_mothers <- subset(recs_trial_final, kids.x==1 & TSEX.x.x==0)
recs_trial_final_mothers <- within(recs_trial_final_mothers, { count <- ave(as.character(TESTERID), CONTROL, FUN=function(x) length(unique(x)))})
recs_trial_final_motherssub <- subset(recs_trial_final_mothers, count>1)

recs_trial_final_fam <- subset(recs_trial_final, kids.x==1)
recs_trial_final_fam <- within(recs_trial_final_fam, { count <- ave(as.character(TESTERID), CONTROL, FUN=function(x) length(unique(x)))})
recs_trial_final_famsub <- subset(recs_trial_final_fam, count>1)


#Test Steering into High/Low Income Neighborhoods (block group median income from ACS)
#Test MTO Low Poverty Rate



medincome_ <- felm(lnmincome_rec ~ APRACE.x + medincome_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
summary(medincome_)

medincomefams_ <- felm(lnmincome_rec ~ APRACE.x + medincome_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final_famsub)
summary(medincomefams_)

medincomemoms_ <- felm(lnmincome_rec ~ APRACE.x + medincome_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + HHMTYPE.x + SAVLBAD.x + STOTUNIT_Rec + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final_motherssub)
summary(medincomemoms_)

mean1=as.vector(c(mean(recs_trial_final[recs_trial_final$white.x==1,"lnmincome_rec"],na.rm=TRUE),
                  mean(recs_trial_final_famsub[recs_trial_final_famsub$white.x==1,"lnmincome_rec"],na.rm=TRUE),
                  mean(recs_trial_final_motherssub[recs_trial_final_motherssub$white.x==1,"lnmincome_rec"],na.rm=TRUE))) 


### GENERATE TABLES


### Median Income of Neighborhood 
stargazer(medincome_, medincomefams_, medincomemoms_,
          type = "latex",
          out = paste0(out, "HUM_Cen_medincome_.tex"),
          title="Steering and Neighborhood Effects: Household Income",
          dep.var.labels.include = F,
          column.labels = c("Med Income","Med Income: Families", "Med Income: Moms"),
          model.numbers = F,
          keep = c("APRACE.x"),
          covariate.labels = c("African American",
                               "Hispanic",
                               "Asian",
                               "Other"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("Comparison Mean (White)",signif(mean1, digits = 2))))



