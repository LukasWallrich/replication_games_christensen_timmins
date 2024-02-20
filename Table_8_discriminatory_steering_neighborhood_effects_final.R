# ---------------------------------------------------------------------------------------------- #
#   Generate Table 8. Discriminatory Steering and Neighborhood Effects                           #
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

#subset experimental data to families
recs_trial_final_families <- subset(recs_trial_final, kids.x==1)
recs_trial_final_families <- within(recs_trial_final_families, { count <- ave(as.character(TESTERID), CONTROL, FUN=function(x) length(unique(x)))})



####################################################
#################Panel A############################
####################################################

###Test Scores
#############################################################################################################################################################################
recs_trial_final_2 <- readRDS("HUDprocessed_JPE_testscores_042021.rds")

#construct separate indicators for market and control
recs_trial_final_2$market <- as.factor(sapply(strsplit(as.character(recs_trial_final_2$CONTROL), "-"), `[`, 1))
recs_trial_final_2$CONTROL <- as.factor(recs_trial_final_2$CONTROL)


#construct indicators for race groups
recs_trial_final_2$ofcolor <- 0
recs_trial_final_2$ofcolor[recs_trial_final_2$APRACE.x==2] <- 1
recs_trial_final_2$ofcolor[recs_trial_final_2$APRACE.x==3] <- 1
recs_trial_final_2$ofcolor[recs_trial_final_2$APRACE.x==4] <- 1
recs_trial_final_2$ofcolor <- as.factor(recs_trial_final_2$ofcolor)

#subset experimental data to mothers
recs_trial_final_mothers_2 <- subset(recs_trial_final_2, kids.x==1 & TSEX.x.x==0)
#recs_trial_final_mothers <- within(recs_trial_final_mothers, { count <- ave(as.character(TESTERID), CONTROL, FUN=function(x) length(unique(x)))})


#############################################################################################################################################################################
# STEERING and ELEMENTARY School Test Scores
## Stanford School Data

### Temporary (Switch to full models below when new data come in)
# Of Color vs. White
ED_elem4 <- felm(mn_avg_ol_elem_Rec ~ ofcolor +  mn_avg_ol_elem_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x + age.x | 0 | CONTROL, data = recs_trial_final_2)
# Group Specific vs. White
ED_elem4_ <- felm(mn_avg_ol_elem_Rec ~ APRACE.x + mn_avg_ol_elem_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x +ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x + age.x | 0 | CONTROL, data = recs_trial_final_2)

summary(ED_elem4)
summary(ED_elem4_)

#########################################################################################################################################################################

# STEERING and MIDDLE School Test Scores
## Stanford School Data

#Temporary
# Of Color vs. White
ED_middle4 <- felm(mn_avg_ol_middle_Rec ~ ofcolor + mn_avg_ol_middle_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x + age.x | 0 | CONTROL, data = recs_trial_final_2)
# Group Specific vs. White
ED_middle4_ <- felm(mn_avg_ol_middle_Rec ~ APRACE.x + mn_avg_ol_middle_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x +ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x + age.x | 0 | CONTROL, data = recs_trial_final_2)

summary(ED_middle4)
summary(ED_middle4_)


#############################################################################################################################################################################
# STEERING INTO HIGH Assault NEIGHBORHOODS 
## Number of nearby Assaults in 2017

# Of Color vs. White
AS4 <- felm(Assault_Rec ~ ofcolor + Assault_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
# Group Specific vs. White
AS4_ <- felm(Assault_Rec ~ APRACE.x + Assault_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(AS4)
summary(AS4_)


###SCHOOL QUALITY
#############################################################################################################################################################################
# STEERING and ELEMENTARY School Quality
## Great School Rankings in 2017

# Of Color vs. White
ES4 <- felm(Elementary_School_Score_Rec ~ ofcolor + Elementary_School_Score_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
# Group Specific vs. White
ES4_ <- felm(Elementary_School_Score_Rec ~ APRACE.x + Elementary_School_Score_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(ES4)
summary(ES4_)


###########################################
############Panel B#######################
#########################################

#NEIGHBORHOOD POVERTY RATES
## Poverty rates are based on American Community Survey definitions by Census block group

# Of Color vs. White
PR4 <- felm(povrate_Rec ~ ofcolor + povrate_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
# Group Specific vs. White
PR4_ <- felm(povrate_Rec ~ APRACE.x + povrate_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(PR4)
summary(PR4_)


##########################################################################################################################################################################

# STEERING AND NEIGHBORHOOD HIGH Skill  
## Skill is share of census block group employed in ACS defined Management, business, science, and arts occupations

# Of Color vs. White
SK4 <- felm(skill_Rec ~ ofcolor + skill_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
# Group Specific vs. White
SK4_ <- felm(skill_Rec ~ APRACE.x + skill_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(SK4)
summary(SK4_)


#############################################################################################################################################################################

# STEERING INTO HIGHLY EDUCATED NEIGHBORHOODS 
## College is share of census block group with at least a college education

# Of Color vs. White
COL4 <- felm(college_Rec ~ ofcolor + college_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
# Group Specific vs. White
COL4_ <- felm(college_Rec ~ APRACE.x + college_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(COL4)
summary(COL4_)


#############################################################################################################################################################################

#NEIGHBORHOOD Single Family Households
## Household Composition is based on American Community Survey definitions by Census block group

# Of Color vs. White
SF4 <- felm(singlefamily_Rec ~ ofcolor + singlefamily_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
# Group Specific vs. White
SF4_ <- felm(singlefamily_Rec ~ APRACE.x + singlefamily_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(SF4)
summary(SF4_)


#############################################################################################################################################################################
#NEIGHBORHOOD Home Ownership
## Home Ownership by Race is based on American Community Survey definitions by Census block group

# Of Color vs. White
own4 <- felm(ownerocc_Rec ~ ofcolor + ownerocc_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)
# Group Specific vs. White
own4_ <- felm(ownerocc_Rec ~ APRACE.x + ownerocc_Ad + w2012pc_Ad + b2012pc_Ad + a2012pc_Ad + hisp2012pc_Ad + logAdPrice | CONTROL + SEQUENCE.x.x + month.x + HCITY.x + market + ARELATE2.x + SAPPTAM.x + TSEX.x.x + THHEGAI.x + TPEGAI.x + THIGHEDU.x + TCURTENR.x +  ALGNCUR.x + AELNG1.x + DPMTEXP.x + AMOVERS.x + age.x + ALEASETP.x + ACAROWN.x | 0 | CONTROL, data = recs_trial_final)

summary(own4)
summary(own4_)





### GENERATE TABLES


p1 <- as.vector(PR4$cpval[1])
p2 <- as.vector(SK4$cpval[1])
p3 <- as.vector(COL4$cpval[1])
p4 <- as.vector(SF4$cpval[1])
p5 =  as.vector(own4$cpval[1])


p_1 <- sort( c(p1, p2, p3, p4 , p5), decreasing = FALSE)
q1 <- p.adjust(p_1, method = "hochberg", n = length(p_1))

p6 <- as.vector(AS4$cpval[1])
p7 <- as.vector(ES4$cpval[1])
p8 <- as.vector( ED_elem4$cpval[1])
p9 <- as.vector( ED_middle4$cpval[1])

p_2 <- sort( c(p6, p7, p8), decreasing = FALSE)
q2 <- p.adjust(p_2, method = "hochberg", n = length(p_2))

###########Control Group Mean####################

a=subset(recs_trial_final, select=c(ofcolor,povrate_Rec,w2012pc_Ad, b2012pc_Ad,
                                    a2012pc_Ad, hisp2012pc_Ad, logAdPrice, CONTROL,
                                    SEQUENCE.x.x, month.x ,HCITY.x ,market ,ARELATE2.x ,
                                    HHMTYPE.x , SAVLBAD.x ,STOTUNIT_Rec ,SAPPTAM.x , 
                                    TSEX.x.x , THHEGAI.x , TPEGAI.x , THIGHEDU.x ,
                                    TCURTENR.x ,  ALGNCUR.x , AELNG1.x , DPMTEXP.x ,
                                    AMOVERS.x , age.x , ALEASETP.x , ACAROWN.x , white.x))
a=a[complete.cases(a),]

b=subset(recs_trial_final, select=c(ofcolor,skill_Rec,w2012pc_Ad, b2012pc_Ad,
                                    a2012pc_Ad, hisp2012pc_Ad, logAdPrice, CONTROL,
                                    SEQUENCE.x.x, month.x ,HCITY.x ,market ,ARELATE2.x ,
                                    HHMTYPE.x , SAVLBAD.x ,STOTUNIT_Rec ,SAPPTAM.x , 
                                    TSEX.x.x , THHEGAI.x , TPEGAI.x , THIGHEDU.x ,
                                    TCURTENR.x ,  ALGNCUR.x , AELNG1.x , DPMTEXP.x ,
                                    AMOVERS.x , age.x , ALEASETP.x , ACAROWN.x , white.x ))
b=b[complete.cases(b),]

c=subset(recs_trial_final, select=c(ofcolor,college_Rec,w2012pc_Ad, b2012pc_Ad,
                                    a2012pc_Ad, hisp2012pc_Ad, logAdPrice, CONTROL,
                                    SEQUENCE.x.x, month.x ,HCITY.x ,market ,ARELATE2.x ,
                                    HHMTYPE.x , SAVLBAD.x ,STOTUNIT_Rec ,SAPPTAM.x , 
                                    TSEX.x.x , THHEGAI.x , TPEGAI.x , THIGHEDU.x ,
                                    TCURTENR.x ,  ALGNCUR.x , AELNG1.x , DPMTEXP.x ,
                                    AMOVERS.x , age.x , ALEASETP.x , ACAROWN.x , white.x ))
c=c[complete.cases(c),]

d=subset(recs_trial_final, select=c(ofcolor,singlefamily_Rec,w2012pc_Ad, b2012pc_Ad,
                                    a2012pc_Ad, hisp2012pc_Ad, logAdPrice, CONTROL,
                                    SEQUENCE.x.x, month.x ,HCITY.x ,market ,ARELATE2.x ,
                                    HHMTYPE.x , SAVLBAD.x ,STOTUNIT_Rec ,SAPPTAM.x , 
                                    TSEX.x.x , THHEGAI.x , TPEGAI.x , THIGHEDU.x ,
                                    TCURTENR.x ,  ALGNCUR.x , AELNG1.x , DPMTEXP.x ,
                                    AMOVERS.x , age.x , ALEASETP.x , ACAROWN.x , white.x ))
d=d[complete.cases(d),]

e=subset(recs_trial_final, select=c(ofcolor,ownerocc_Rec,w2012pc_Ad, b2012pc_Ad,
                                    a2012pc_Ad, hisp2012pc_Ad, logAdPrice, CONTROL,
                                    SEQUENCE.x.x, month.x ,HCITY.x ,market ,ARELATE2.x ,
                                    HHMTYPE.x , SAVLBAD.x ,STOTUNIT_Rec ,SAPPTAM.x , 
                                    TSEX.x.x , THHEGAI.x , TPEGAI.x , THIGHEDU.x ,
                                    TCURTENR.x ,  ALGNCUR.x , AELNG1.x , DPMTEXP.x ,
                                    AMOVERS.x , age.x , ALEASETP.x , ACAROWN.x , white.x ))
e=e[complete.cases(e),]

f=subset(recs_trial_final, select=c(ofcolor,Assault_Rec,w2012pc_Ad, b2012pc_Ad,
                                    a2012pc_Ad, hisp2012pc_Ad, logAdPrice, CONTROL,
                                    SEQUENCE.x.x, month.x ,HCITY.x ,market ,ARELATE2.x ,
                                    HHMTYPE.x , SAVLBAD.x ,STOTUNIT_Rec ,SAPPTAM.x , 
                                    TSEX.x.x , THHEGAI.x , TPEGAI.x , THIGHEDU.x ,
                                    TCURTENR.x ,  ALGNCUR.x , AELNG1.x , DPMTEXP.x ,
                                    AMOVERS.x , age.x , ALEASETP.x , ACAROWN.x , white.x ))
f=f[complete.cases(f),]

g=subset(recs_trial_final, select=c(ofcolor,Elementary_School_Score_Rec ,w2012pc_Ad, b2012pc_Ad,
                                    a2012pc_Ad, hisp2012pc_Ad, logAdPrice, CONTROL,
                                    SEQUENCE.x.x, month.x ,HCITY.x ,market ,ARELATE2.x ,
                                    HHMTYPE.x , SAVLBAD.x ,STOTUNIT_Rec ,SAPPTAM.x , 
                                    TSEX.x.x , THHEGAI.x , TPEGAI.x , THIGHEDU.x ,
                                    TCURTENR.x ,  ALGNCUR.x , AELNG1.x , DPMTEXP.x ,
                                    AMOVERS.x , age.x , ALEASETP.x , ACAROWN.x , white.x ))
g=g[complete.cases(g),]


h=subset(recs_trial_final_2, select=c(ofcolor,mn_avg_ol_elem_Rec, logAdPrice, CONTROL,
                                    SEQUENCE.x.x, month.x ,HCITY.x ,ARELATE2.x ,SAPPTAM.x,
                                    TSEX.x.x , THHEGAI.x , TPEGAI.x , THIGHEDU.x , 
                                    TCURTENR.x , age.x ,white.x ))
h=h[complete.cases(h),]

i=subset(recs_trial_final_2, select=c(ofcolor,mn_avg_ol_middle_Rec, logAdPrice, CONTROL,
                                      SEQUENCE.x.x, month.x ,HCITY.x ,ARELATE2.x ,SAPPTAM.x,
                                      TSEX.x.x , THHEGAI.x , TPEGAI.x , THIGHEDU.x , 
                                      TCURTENR.x , age.x ,white.x ))
i=i[complete.cases(i),]




mean1=as.vector(c(mean(h[h$white.x==1,"mn_avg_ol_elem_Rec"],na.rm=TRUE),
                  mean(i[i$white.x==1,"mn_avg_ol_middle_Rec"],na.rm=TRUE),
                  mean(f[f$white.x==1,"Assault_Rec"],na.rm=TRUE),
                  mean(g[g$white.x==1,"Elementary_School_Score_Rec"],na.rm=TRUE)))

mean2=as.vector(c(mean(a[a$white.x==1,"povrate_Rec"], na.rm=TRUE),
                  mean(b[b$white.x==1,"skill_Rec"], na.rm=TRUE),
                  mean(c[c$white.x==1,"college_Rec"], na.rm=TRUE),
                  mean(d[d$white.x==1,"singlefamily_Rec"], na.rm=TRUE),
                  mean(e[e$white.x==1,"ownerocc_Rec"], na.rm=TRUE)))  



## Table 8

### Panel A Racial Minority

stargazer( ED_elem4, ED_middle4, AS4, ES4,
          type = "latex",
          out = paste0(out, "HUM_tab_8_A_JPE.tex"),
          title="Steering and Neighborhood Effects",
          model.numbers = F,
          keep = c("ofcolor"),
          covariate.labels = c("Racial Minority"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("ln(Price) Advert Home","Y","Y","Y","Y"),
                           c("Racial Comp Advert Home","Y","Y","Y","Y"),
                           c("Outcome Advertised Home","Y","Y","Y","Y")))

### Panel B Racial Minority

stargazer(PR4, SK4, COL4, SF4, own4,
          type = "latex",
          out = paste0(out, "HUM_tab8_B_JPE.tex"),
          title="Steering and Neighborhood Effects",
          model.numbers = F,
          keep = c("ofcolor"),
          covariate.labels = c("Racial Minority"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("p-values",signif(p_1, digits = 2)),
                           c("q-values",signif(q1, digits = 2)),
                           c("ln(Price) Advert Home","Y","Y","Y","Y"),
                           c("Racial Comp Advert Home","Y","Y","Y","Y"),
                           c("Outcome Advertised Home","Y","Y","Y","Y")))

### Panel A by Race

stargazer( ED_elem4_, ED_middle4_,AS4_, ES4_,
          type = "latex",
          out = paste0(out, "HUM_tab8_A_JPE_.tex"),
          title="Steering and Neighborhood Effects",
          dep.var.labels.include = F,
          column.labels = c("Assaults", "Elem School",  "Elem School", "Middle School"),
          model.numbers = F,
          keep = c("APRACE.x"),
          covariate.labels = c("African American",
                               "Hispanic",
                               "Asian",
                               "Other"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("Comparison Mean (White)",signif(mean1, digits = 2)),
                           c("ln(Price) Advert Home","Y","Y","Y","Y"),
                           c("Racial Comp Advert Home","Y","Y","Y","Y"),
                           c("Outcome Advertised Home","Y","Y","Y","Y")))

### Panel B by Race

stargazer(PR4_, SK4_, COL4_, SF4_, own4_,
          type = "latex",
          out = paste0(out, "HUM_tab8_B_JPE_.tex"),
          title="Steering and Neighborhood Effects",
          dep.var.labels.include = F,
          column.labels = c("Poverty Rate","High Skill", "College", "Sing Family", "Owner Occ"),
          model.numbers = F,
          keep = c("APRACE.x"),
          covariate.labels = c("African American",
                               "Hispanic",
                               "Asian",
                               "Other"),
          keep.stat=c("n","adj.rsq"),
          digits=4,digits.extra=0,no.space=T,align=T,model.names=F,notes.append=T,object.names=F,
          add.lines = list(c("Comparison Mean (White)",signif(mean2, digits = 2)),
                           c("ln(Price) Advert Home","Y","Y","Y","Y"),
                           c("Racial Comp Advert Home","Y","Y","Y","Y"),
                           c("Outcome Advertised Home","Y","Y","Y","Y")))


