# Structural Equation Model to Test Differences in Correlations
# This is the script for the main analysis in the manuscript: "Parent and child gender effects in the relationship between attachment 
# and both internalizing and externalizing problems of children between 2 and 5 years old: A dyadic perspective"
# Authors: Magda Matetovici, Mauricio Garnier Villarreal
# Last Modified: 31.08.2024

library(tidyverse)
library(dplyr)
library(mice)


# Load datafiles
data_attachment_recoded<- read.csv(file = "data_attachment_recoded_parent_age.csv")
data_problem_recoded <- read.csv(file = "data_sdq_original_recoded.csv")
#-------------------------------------------------------------------------------
names(data_attachment_recoded)[names(data_attachment_recoded) == "data$A_parentage"] <- "parent_age"
# Make sure they are all ordered variables
# Convert all columns to factors 

data_attachment_recoded <- data_attachment_recoded %>% 
  mutate_all(factor)
data_problem_recoded <- data_problem_recoded %>% 
  mutate_all(factor)

# Convert all factors to ordered factors
data_attachment_recoded <- data_attachment_recoded %>% 
  mutate_all(ordered)
data_problem_recoded <- data_problem_recoded %>% 
  mutate_all(ordered)


#Perform imputation for attachment data (4 missing values)
set.seed(12345)
imp_attachment <- mice(data_attachment_recoded[, 50:98], m = 1, method = "polr", maxit=20)
imp_attachment <- complete(imp_attachment, 1)

imp_attachment <- imp_attachment %>%
  mutate(parents = case_when(
    dyad %in% c("mother_daughter", "mother_son") ~ "mother",
    dyad %in% c("father_daughter", "father_son") ~ "father",
  ))
imp_attachment$parents

imp_attachment <- imp_attachment %>%
  mutate(children = case_when(
    dyad %in% c("mother_daughter", "father_daughter") ~ "daughter",
    dyad %in% c("father_son", "mother_son") ~ "son",
  ))
imp_attachment$children

imp_attachment <- imp_attachment %>%
  mutate(similar = case_when(
    dyad %in% c("mother_daughter", "father_son") ~ "same",
    dyad %in% c("father_daughter", "mother_son") ~ "different",
  ))
imp_attachment$similar
imp_attachment$dyad


#multiple imputation for problem data (2 missing values)
imp_problems <- mice(data_problem_recoded[, 22:42], m = 1, method = "polr")
imp_problems <- complete(imp_problems, 1)

imp_problems$dyad <- factor(imp_problems$dyad , ordered = FALSE)
imp_attachment$dyad <- factor(imp_attachment$dyad , ordered = FALSE)

#Put datasets together, remove first variable because it appears twice
imp_attachment = cbind(imp_problems[, -1], imp_attachment)

# add parent age and make sure it is not factored
imp_attachment <- cbind(imp_attachment, data_attachment_recoded$data.A_parentage)
names(imp_attachment)[names(imp_attachment) == "data_attachment_recoded$data.A_parentage"] <- "parent_age"
imp_attachment$parent_age <- as.integer(imp_attachment$parent_age)

#one missing datapoint, imputation:
imp_attachment <- mice(imp_attachment, m = 1, method = "norm")
imp_attachment <- complete(imp_attachment, 1)

#-------------------------------------------------------------------------------
### WEAK INVARIANCE MODELS - Internalizing Problems
#-------------------------------------------------------------------------------

weak_mods <- measEq.syntax(configural.model = c(model_secure_attachment,
                                                model_avoidant_attachment2,
                                                model_ambivalent_attachment,
                                                model_disorganised_attachment,
                                                model_internalizing2),
                           data = imp_attachment,
                           ordered = TRUE,
                           parameterization = "theta",
                           ID.fac = "std.lv",
                           ID.cat = "Wu.Estabrook.2016",
                           group = "dyad",
                           group.equal = c("thresholds", "loadings"),
                           group.partial = c("secure_attachment =~C_ari61_recoded", 
                                             "secure_attachment =~C_ari30_recoded", 
                                             "secure_attachment =~C_ari46_recoded",
                                             "avoidant_attachment =~C_ari57_recoded", 
                                             "avoidant_attachment =~C_ari66_recoded",
                                             "ambivalent_attachment =~C_ari19_recoded", 
                                             "ambivalent_attachment =~C_ari13_recoded",
                                             "ambivalent_attachment =~C_ari15_recoded",
                                             "ambivalent_attachment =~C_ari58_recoded",
                                             "ambivalent_attachment =~C_ari16_recoded",
                                             "internalising  =~ C_SDQ8.2_11_recoded ", 
                                             "internalising  =~ C_SDQ8.2_16_recoded", 
                                             "internalising  =~ C_SDQ8.2_19_recoded"))



## Phantom factors
cat(as.character(weak_mods))

mm <- as.character(weak_mods)

mm2 <- sub('secure_attachment ~~ c(1, NA, NA, NA)*secure_attachment + c(psi.1_1.g1, psi.1_1.g2, psi.1_1.g3, psi.1_1.g4)*secure_attachment
avoidant_attachment ~~ c(1, NA, NA, NA)*avoidant_attachment + c(psi.2_2.g1, psi.2_2.g2, psi.2_2.g3, psi.2_2.g4)*avoidant_attachment
ambivalent_attachment ~~ c(1, NA, NA, NA)*ambivalent_attachment + c(psi.3_3.g1, psi.3_3.g2, psi.3_3.g3, psi.3_3.g4)*ambivalent_attachment
disorganised_attachment ~~ c(1, NA, NA, NA)*disorganised_attachment + c(psi.4_4.g1, psi.4_4.g2, psi.4_4.g3, psi.4_4.g4)*disorganised_attachment
internalising ~~ c(1, NA, NA, NA)*internalising + c(psi.5_5.g1, psi.5_5.g2, psi.5_5.g3, psi.5_5.g4)*internalising', 
           
           'secure_attachment ~~ c(0, 0, 0, 0)*secure_attachment + c(psi.1_1.g1, psi.1_1.g2, psi.1_1.g3, psi.1_1.g4)*secure_attachment
avoidant_attachment ~~ c(0, 0, 0, 0)*avoidant_attachment + c(psi.2_2.g1, psi.2_2.g2, psi.2_2.g3, psi.2_2.g4)*avoidant_attachment
ambivalent_attachment ~~ c(0, 0, 0, 0)*ambivalent_attachment + c(psi.3_3.g1, psi.3_3.g2, psi.3_3.g3, psi.3_3.g4)*ambivalent_attachment
disorganised_attachment ~~ c(0, 0, 0, 0)*disorganised_attachment + c(psi.4_4.g1, psi.4_4.g2, psi.4_4.g3, psi.4_4.g4)*disorganised_attachment
internalising ~~ c(0, 0, 0, 0)*internalising + c(psi.5_5.g1, psi.5_5.g2, psi.5_5.g3, psi.5_5.g4)*internalising', 
           
           mm, fixed=T)

mm2 <- sub('secure_attachment ~~ c(NA, NA, NA, NA)*avoidant_attachment + c(psi.2_1.g1, psi.2_1.g2, psi.2_1.g3, psi.2_1.g4)*avoidant_attachment
secure_attachment ~~ c(NA, NA, NA, NA)*ambivalent_attachment + c(psi.3_1.g1, psi.3_1.g2, psi.3_1.g3, psi.3_1.g4)*ambivalent_attachment
secure_attachment ~~ c(NA, NA, NA, NA)*disorganised_attachment + c(psi.4_1.g1, psi.4_1.g2, psi.4_1.g3, psi.4_1.g4)*disorganised_attachment
secure_attachment ~~ c(NA, NA, NA, NA)*internalising + c(psi.5_1.g1, psi.5_1.g2, psi.5_1.g3, psi.5_1.g4)*internalising
avoidant_attachment ~~ c(NA, NA, NA, NA)*ambivalent_attachment + c(psi.3_2.g1, psi.3_2.g2, psi.3_2.g3, psi.3_2.g4)*ambivalent_attachment
avoidant_attachment ~~ c(NA, NA, NA, NA)*disorganised_attachment + c(psi.4_2.g1, psi.4_2.g2, psi.4_2.g3, psi.4_2.g4)*disorganised_attachment
avoidant_attachment ~~ c(NA, NA, NA, NA)*internalising + c(psi.5_2.g1, psi.5_2.g2, psi.5_2.g3, psi.5_2.g4)*internalising
ambivalent_attachment ~~ c(NA, NA, NA, NA)*disorganised_attachment + c(psi.4_3.g1, psi.4_3.g2, psi.4_3.g3, psi.4_3.g4)*disorganised_attachment
ambivalent_attachment ~~ c(NA, NA, NA, NA)*internalising + c(psi.5_3.g1, psi.5_3.g2, psi.5_3.g3, psi.5_3.g4)*internalising
disorganised_attachment ~~ c(NA, NA, NA, NA)*internalising + c(psi.5_4.g1, psi.5_4.g2, psi.5_4.g3, psi.5_4.g4)*internalising',
           
           'secure_attachment ~~ c(0, 0, 0, 0)*avoidant_attachment + c(psi.2_1.g1, psi.2_1.g2, psi.2_1.g3, psi.2_1.g4)*avoidant_attachment
secure_attachment ~~ c(0, 0, 0, 0)*ambivalent_attachment + c(psi.3_1.g1, psi.3_1.g2, psi.3_1.g3, psi.3_1.g4)*ambivalent_attachment
secure_attachment ~~ c(0, 0, 0, 0)*disorganised_attachment + c(psi.4_1.g1, psi.4_1.g2, psi.4_1.g3, psi.4_1.g4)*disorganised_attachment
secure_attachment ~~ c(0, 0, 0, 0)*internalising + c(psi.5_1.g1, psi.5_1.g2, psi.5_1.g3, psi.5_1.g4)*internalising
avoidant_attachment ~~ c(0, 0, 0, 0)*ambivalent_attachment + c(psi.3_2.g1, psi.3_2.g2, psi.3_2.g3, psi.3_2.g4)*ambivalent_attachment
avoidant_attachment ~~ c(0, 0, 0, 0)*disorganised_attachment + c(psi.4_2.g1, psi.4_2.g2, psi.4_2.g3, psi.4_2.g4)*disorganised_attachment
avoidant_attachment ~~ c(0, 0, 0, 0)*internalising + c(psi.5_2.g1, psi.5_2.g2, psi.5_2.g3, psi.5_2.g4)*internalising
ambivalent_attachment ~~ c(0, 0, 0, 0)*disorganised_attachment + c(psi.4_3.g1, psi.4_3.g2, psi.4_3.g3, psi.4_3.g4)*disorganised_attachment
ambivalent_attachment ~~ c(0, 0, 0, 0)*internalising + c(psi.5_3.g1, psi.5_3.g2, psi.5_3.g3, psi.5_3.g4)*internalising
disorganised_attachment ~~ c(0, 0, 0, 0)*internalising + c(psi.5_4.g1, psi.5_4.g2, psi.5_4.g3, psi.5_4.g4)*internalising',
           
           mm2, fixed=T)

# 

phs <- '
phsa =~ c(1, NA, NA, NA)*secure_attachment
phav =~ c(1, NA, NA, NA)*avoidant_attachment
pham =~ c(1, NA, NA, NA)*ambivalent_attachment
phds =~ c(1, NA, NA, NA)*disorganised_attachment
phin =~ c(1, NA, NA, NA)*internalising

phsa ~~ c(1, 1, 1, 1)*phsa
phav ~~ c(1, 1, 1, 1)*phav
pham ~~ c(1, 1, 1, 1)*pham
phds ~~ c(1, 1, 1, 1)*phds
phin ~~ c(1, 1, 1, 1)*phin

phsa ~c(0,0,0,0)*1
phav ~c(0,0,0,0)*1
pham ~c(0,0,0,0)*1
phds ~c(0,0,0,0)*1
phin ~c(0,0,0,0)*1
'

ph_cors <- '
phsa ~~ c(NA, NA, NA, NA)*phav + c(rho.2_1.g1, rho.2_1.g2, rho.2_1.g3, rho.2_1.g4)*phav
phsa ~~ c(NA, NA, NA, NA)*pham + c(rho.3_1.g1, rho.3_1.g2, rho.3_1.g3, rho.3_1.g4)*pham
phsa ~~ c(NA, NA, NA, NA)*phds + c(rho.4_1.g1, rho.4_1.g2, rho.4_1.g3, rho.4_1.g4)*phds
phsa ~~ c(NA, NA, NA, NA)*phin + c(rho.5_1.g1, rho.5_1.g2, rho.5_1.g3, rho.5_1.g4)*phin
phav ~~ c(NA, NA, NA, NA)*pham + c(rho.3_2.g1, rho.3_2.g2, rho.3_2.g3, rho.3_2.g4)*pham
phav ~~ c(NA, NA, NA, NA)*phds + c(rho.4_2.g1, rho.4_2.g2, rho.4_2.g3, rho.4_2.g4)*phds
phav ~~ c(NA, NA, NA, NA)*phin + c(rho.5_2.g1, rho.5_2.g2, rho.5_2.g3, rho.5_2.g4)*phin
pham ~~ c(NA, NA, NA, NA)*phds + c(rho.4_3.g1, rho.4_3.g2, rho.4_3.g3, rho.4_3.g4)*phds
pham ~~ c(NA, NA, NA, NA)*phin + c(rho.5_3.g1, rho.5_3.g2, rho.5_3.g3, rho.5_3.g4)*phin
phds ~~ c(NA, NA, NA, NA)*phin + c(rho.5_4.g1, rho.5_4.g2, rho.5_4.g3, rho.5_4.g4)*phin

#drho1 := rho.2_1.g1 - rho.2_1.g2
#drho2 := rho.2_1.g1 - rho.2_1.g3
#drho3 := rho.2_1.g1 - rho.2_1.g4
#drho4 := rho.2_1.g2 - rho.2_1.g3
#drho5 := rho.2_1.g2 - rho.2_1.g4
#drho6 := rho.2_1.g3 - rho.2_1.g4

#drho7 := rho.3_1.g1 - rho.3_1.g2
#drho8 := rho.3_1.g1 - rho.3_1.g3
#drho9 := rho.3_1.g1 - rho.3_1.g4
#drho10 := rho.3_1.g2 - rho.3_1.g3
#drho11 := rho.3_1.g2 - rho.3_1.g4
#drho12 := rho.3_1.g3 - rho.3_1.g4

#drho13 := rho.4_1.g1 - rho.4_1.g2
#drho14 := rho.4_1.g1 - rho.4_1.g3
#drho15 := rho.4_1.g1 - rho.4_1.g4
#drho16 := rho.4_1.g2 - rho.4_1.g3
#drho17 := rho.4_1.g2 - rho.4_1.g4
#drho18 := rho.4_1.g3 - rho.4_1.g4

#comparing dyads regarding secure attachment and internalizing problems
drho18 := rho.5_1.g1 - rho.5_1.g2
drho19 := rho.5_1.g1 - rho.5_1.g3
drho20 := rho.5_1.g1 - rho.5_1.g4
drho21 := rho.5_1.g2 - rho.5_1.g3
drho22 := rho.5_1.g2 - rho.5_1.g4
drho23 := rho.5_1.g3 - rho.5_1.g4

#drho24 := rho.3_2.g1 - rho.3_2.g2
#drho25 := rho.3_2.g1 - rho.3_2.g3
#drho26 := rho.3_2.g1 - rho.3_2.g4
#drho27 := rho.3_2.g2 - rho.3_2.g3
#drho28 := rho.3_2.g2 - rho.3_2.g4
#drho29 := rho.3_2.g3 - rho.3_2.g4

#drho30 := rho.4_2.g1 - rho.4_2.g2
#drho31 := rho.4_2.g1 - rho.4_2.g3
#drho32 := rho.4_2.g1 - rho.4_2.g4
#drho33 := rho.4_2.g2 - rho.4_2.g3
#drho34 := rho.4_2.g2 - rho.4_2.g4
#drho35 := rho.4_2.g3 - rho.4_2.g4

#comparing dyads regarding avoidant attachment and internalizing problems
drho35 := rho.5_2.g1 - rho.5_2.g2
drho36 := rho.5_2.g1 - rho.5_2.g3
drho37 := rho.5_2.g1 - rho.5_2.g4
drho38 := rho.5_2.g2 - rho.5_2.g3
drho39 := rho.5_2.g2 - rho.5_2.g4
drho40 := rho.5_2.g3 - rho.5_2.g4

#drho40 := rho.4_3.g1 - rho.4_3.g2
#drho41 := rho.4_3.g1 - rho.4_3.g3
#drho42 := rho.4_3.g1 - rho.4_3.g4
#drho43 := rho.4_3.g2 - rho.4_3.g3
#drho44 := rho.4_3.g2 - rho.4_3.g4
#drho45 := rho.4_3.g3 - rho.4_3.g4

#comparing dyads regarding ambivalent attachment and internalizing problems
drho46 := rho.5_3.g1 - rho.5_3.g2
drho47 := rho.5_3.g1 - rho.5_3.g3
drho48 := rho.5_3.g1 - rho.5_3.g4
drho49 := rho.5_3.g2 - rho.5_3.g3
drho50 := rho.5_3.g2 - rho.5_3.g4
drho51 := rho.5_3.g3 - rho.5_3.g4

#comparing dyads regarding disorganized attachment and internalizing problems
drho52 := rho.5_4.g1 - rho.5_4.g2
drho53 := rho.5_4.g1 - rho.5_4.g3
drho54 := rho.5_4.g1 - rho.5_4.g4
drho55 := rho.5_4.g2 - rho.5_4.g3
drho56 := rho.5_4.g2 - rho.5_4.g4
drho57 := rho.5_4.g3 - rho.5_4.g4

#comparing similar vs different dyads for each attachment type and internalizing problems
drho58 := (rho.5_1.g2+rho.5_1.g4)/2 - (rho.5_1.g1+rho.5_1.g3)/2
drho59 := (rho.5_2.g2+rho.5_2.g4)/2 - (rho.5_2.g1+rho.5_2.g3)/2
drho60 := (rho.5_3.g2+rho.5_3.g4)/2 - (rho.5_3.g1+rho.5_3.g3)/2
drho61 := (rho.5_4.g2+rho.5_4.g4)/2 - (rho.5_4.g1+rho.5_4.g3)/2

#comparing mothers vs fathers for each attachment type and internalizing problems
drho62 := (rho.5_1.g2+rho.5_1.g1)/2 - (rho.5_1.g4+rho.5_1.g3)/2
drho63 := (rho.5_2.g2+rho.5_2.g1)/2 - (rho.5_2.g4+rho.5_2.g3)/2
drho64 := (rho.5_3.g2+rho.5_3.g1)/2 - (rho.5_3.g4+rho.5_3.g3)/2
drho65 := (rho.5_4.g2+rho.5_4.g1)/2 - (rho.5_4.g4+rho.5_4.g3)/2


#comparing sons vs daugthers for each attachment type and internalizing problems
drho66 := (rho.5_1.g2+rho.5_1.g3)/2 - (rho.5_1.g4+rho.5_1.g1)/2
drho67 := (rho.5_2.g2+rho.5_2.g3)/2 - (rho.5_2.g4+rho.5_2.g1)/2
drho68 := (rho.5_3.g2+rho.5_3.g3)/2 - (rho.5_3.g4+rho.5_3.g1)/2
drho69 := (rho.5_4.g2+rho.5_4.g3)/2 - (rho.5_4.g4+rho.5_4.g1)/2
'
control_age_of_parent <- 'phsa ~ parent_age
                          phav ~ parent_age
                          pham ~ parent_age
                          phds ~ parent_age
                          phin ~ parent_age'
  
mm3 <- c(mm2, phs)
mm4 <- c(mm3, ph_cors)
mm5 <- c(mm4, control_age_of_parent)
cat(mm5)

sam1 <- cfa(model = mm5, 
            data = imp_attachment,
            group= "dyad",
            estimator = "WLSMV")

# save to text file to visualize all the output
sink(file = "sam2_summary_internalizing_problems_age.txt")
summary(sam1,standardized=T, ci=T)
sink(file = NULL)

#Reliability of items and factors

#check the item reliability
cfa_internalising <- cfa(model = mm, 
                         data = imp_attachment,
                         group= "dyad",
                        estimator = "WLSMV")
r_squared_values_internalizing <- inspect(cfa_internalising, "r2")
save(r_squared_values_internalizing, file ="item_reliability_internalizing.RData")

#check the factor reliability
compRelSEM(cfa_internalising)

## Correlation differences

phs <- c("phsa","phav","pham","phds","phin")
pars_cor <- parameterestimates(sam1)

## factor correlations
pars_cor[pars_cor$op == "~~" & pars_cor$lhs != pars_cor$rhs & pars_cor$lhs %in% phs,]

# define parameters (differences)
df_comparisons <- pars_cor[pars_cor$op == ":=",]
pars_cor[pars_cor$op == ":=" & pars_cor$pvalue < .05,]
pars_cor[pars_cor$op == ":=" & pars_cor$pvalue < .01,]

#Adjust p-values for false discovery rate
pvals <- pars_cor[pars_cor$op == ":=" ,]$pvalue
df_comparisons$adjusted_p_values <- p.adjust(pvals, method = "fdr")
print(df_comparisons$adjusted_p_values)

#these are the comparisons that are significant at 0.05 after adjustment for false-discovery rate
df_comparisons[df_comparisons[,"adjusted_p_values"]<0.05, ]


#-------------------------------------------------------------------------------
# Weak invariance model for Externalizing Problems
#-------------------------------------------------------------------------------
weak_mods_externalizing <- measEq.syntax(configural.model = c(model_secure_attachment,
                                                model_avoidant_attachment2,
                                                model_ambivalent_attachment,
                                                model_disorganised_attachment,
                                                model_externalizing),
                           data = imp_attachment,
                           ordered = TRUE,
                           parameterization = "theta",
                           ID.fac = "std.lv",
                           ID.cat = "Wu.Estabrook.2016",
                           group = "dyad",
                           group.equal = c("thresholds", "loadings"),
                           group.partial = c("secure_attachment =~C_ari61_recoded", 
                                             "secure_attachment =~C_ari30_recoded", 
                                             "secure_attachment =~C_ari46_recoded",
                                             "avoidant_attachment =~C_ari57_recoded", 
                                             "avoidant_attachment =~C_ari66_recoded",
                                             "ambivalent_attachment =~C_ari19_recoded", 
                                             "ambivalent_attachment =~C_ari13_recoded",
                                             "ambivalent_attachment =~C_ari15_recoded",
                                             "ambivalent_attachment =~C_ari58_recoded",
                                             "ambivalent_attachment =~C_ari16_recoded"))


## phantom factors
cat(as.character(weak_mods_externalizing))

mm <- as.character(weak_mods_externalizing)

mm2 <- sub('secure_attachment ~~ c(1, NA, NA, NA)*secure_attachment + c(psi.1_1.g1, psi.1_1.g2, psi.1_1.g3, psi.1_1.g4)*secure_attachment
avoidant_attachment ~~ c(1, NA, NA, NA)*avoidant_attachment + c(psi.2_2.g1, psi.2_2.g2, psi.2_2.g3, psi.2_2.g4)*avoidant_attachment
ambivalent_attachment ~~ c(1, NA, NA, NA)*ambivalent_attachment + c(psi.3_3.g1, psi.3_3.g2, psi.3_3.g3, psi.3_3.g4)*ambivalent_attachment
disorganised_attachment ~~ c(1, NA, NA, NA)*disorganised_attachment + c(psi.4_4.g1, psi.4_4.g2, psi.4_4.g3, psi.4_4.g4)*disorganised_attachment
externalising ~~ c(1, NA, NA, NA)*externalising + c(psi.5_5.g1, psi.5_5.g2, psi.5_5.g3, psi.5_5.g4)*externalising', 
           
           'secure_attachment ~~ c(0, 0, 0, 0)*secure_attachment + c(psi.1_1.g1, psi.1_1.g2, psi.1_1.g3, psi.1_1.g4)*secure_attachment
avoidant_attachment ~~ c(0, 0, 0, 0)*avoidant_attachment + c(psi.2_2.g1, psi.2_2.g2, psi.2_2.g3, psi.2_2.g4)*avoidant_attachment
ambivalent_attachment ~~ c(0, 0, 0, 0)*ambivalent_attachment + c(psi.3_3.g1, psi.3_3.g2, psi.3_3.g3, psi.3_3.g4)*ambivalent_attachment
disorganised_attachment ~~ c(0, 0, 0, 0)*disorganised_attachment + c(psi.4_4.g1, psi.4_4.g2, psi.4_4.g3, psi.4_4.g4)*disorganised_attachment
externalising ~~ c(0, 0, 0, 0)*externalising + c(psi.5_5.g1, psi.5_5.g2, psi.5_5.g3, psi.5_5.g4)*externalising
', 
           
           mm, fixed=T)

mm2 <- sub('secure_attachment ~~ c(NA, NA, NA, NA)*avoidant_attachment + c(psi.2_1.g1, psi.2_1.g2, psi.2_1.g3, psi.2_1.g4)*avoidant_attachment
secure_attachment ~~ c(NA, NA, NA, NA)*ambivalent_attachment + c(psi.3_1.g1, psi.3_1.g2, psi.3_1.g3, psi.3_1.g4)*ambivalent_attachment
secure_attachment ~~ c(NA, NA, NA, NA)*disorganised_attachment + c(psi.4_1.g1, psi.4_1.g2, psi.4_1.g3, psi.4_1.g4)*disorganised_attachment
secure_attachment ~~ c(NA, NA, NA, NA)*externalising + c(psi.5_1.g1, psi.5_1.g2, psi.5_1.g3, psi.5_1.g4)*externalising
avoidant_attachment ~~ c(NA, NA, NA, NA)*ambivalent_attachment + c(psi.3_2.g1, psi.3_2.g2, psi.3_2.g3, psi.3_2.g4)*ambivalent_attachment
avoidant_attachment ~~ c(NA, NA, NA, NA)*disorganised_attachment + c(psi.4_2.g1, psi.4_2.g2, psi.4_2.g3, psi.4_2.g4)*disorganised_attachment
avoidant_attachment ~~ c(NA, NA, NA, NA)*externalising + c(psi.5_2.g1, psi.5_2.g2, psi.5_2.g3, psi.5_2.g4)*externalising
ambivalent_attachment ~~ c(NA, NA, NA, NA)*disorganised_attachment + c(psi.4_3.g1, psi.4_3.g2, psi.4_3.g3, psi.4_3.g4)*disorganised_attachment
ambivalent_attachment ~~ c(NA, NA, NA, NA)*externalising + c(psi.5_3.g1, psi.5_3.g2, psi.5_3.g3, psi.5_3.g4)*externalising
disorganised_attachment ~~ c(NA, NA, NA, NA)*externalising + c(psi.5_4.g1, psi.5_4.g2, psi.5_4.g3, psi.5_4.g4)*externalising',
           
           'secure_attachment ~~ c(0, 0, 0, 0)*avoidant_attachment + c(psi.2_1.g1, psi.2_1.g2, psi.2_1.g3, psi.2_1.g4)*avoidant_attachment
secure_attachment ~~ c(0, 0, 0, 0)*ambivalent_attachment + c(psi.3_1.g1, psi.3_1.g2, psi.3_1.g3, psi.3_1.g4)*ambivalent_attachment
secure_attachment ~~ c(0, 0, 0, 0)*disorganised_attachment + c(psi.4_1.g1, psi.4_1.g2, psi.4_1.g3, psi.4_1.g4)*disorganised_attachment
secure_attachment ~~ c(0, 0, 0, 0)*externalising + c(psi.5_1.g1, psi.5_1.g2, psi.5_1.g3, psi.5_1.g4)*externalising
avoidant_attachment ~~ c(0, 0, 0, 0)*ambivalent_attachment + c(psi.3_2.g1, psi.3_2.g2, psi.3_2.g3, psi.3_2.g4)*ambivalent_attachment
avoidant_attachment ~~ c(0, 0, 0, 0)*disorganised_attachment + c(psi.4_2.g1, psi.4_2.g2, psi.4_2.g3, psi.4_2.g4)*disorganised_attachment
avoidant_attachment ~~ c(0, 0, 0, 0)*externalising + c(psi.5_2.g1, psi.5_2.g2, psi.5_2.g3, psi.5_2.g4)*externalising
ambivalent_attachment ~~ c(0, 0, 0, 0)*disorganised_attachment + c(psi.4_3.g1, psi.4_3.g2, psi.4_3.g3, psi.4_3.g4)*disorganised_attachment
ambivalent_attachment ~~ c(0, 0, 0, 0)*externalising + c(psi.5_3.g1, psi.5_3.g2, psi.5_3.g3, psi.5_3.g4)*externalising
disorganised_attachment ~~ c(0, 0, 0, 0)*externalising + c(psi.5_4.g1, psi.5_4.g2, psi.5_4.g3, psi.5_4.g4)*externalising',
           
           mm2, fixed=T)

# 

phs <- '
phsa =~ c(1, NA, NA, NA)*secure_attachment
phav =~ c(1, NA, NA, NA)*avoidant_attachment
pham =~ c(1, NA, NA, NA)*ambivalent_attachment
phds =~ c(1, NA, NA, NA)*disorganised_attachment
phex =~ c(1, NA, NA, NA)*externalising 

phsa ~~ c(1, 1, 1, 1)*phsa
phav ~~ c(1, 1, 1, 1)*phav
pham ~~ c(1, 1, 1, 1)*pham
phds ~~ c(1, 1, 1, 1)*phds
phex ~~ c(1, 1, 1, 1)*phex

phsa ~c(0,0,0,0)*1
phav ~c(0,0,0,0)*1
pham ~c(0,0,0,0)*1
phds ~c(0,0,0,0)*1
phex ~c(0,0,0,0)*1
'

ph_cors <- '
phsa ~~ c(NA, NA, NA, NA)*phav + c(rho.2_1.g1, rho.2_1.g2, rho.2_1.g3, rho.2_1.g4)*phav
phsa ~~ c(NA, NA, NA, NA)*pham + c(rho.3_1.g1, rho.3_1.g2, rho.3_1.g3, rho.3_1.g4)*pham
phsa ~~ c(NA, NA, NA, NA)*phds + c(rho.4_1.g1, rho.4_1.g2, rho.4_1.g3, rho.4_1.g4)*phds
phsa ~~ c(NA, NA, NA, NA)*phex + c(rho.5_1.g1, rho.5_1.g2, rho.5_1.g3, rho.5_1.g4)*phex
phav ~~ c(NA, NA, NA, NA)*pham + c(rho.3_2.g1, rho.3_2.g2, rho.3_2.g3, rho.3_2.g4)*pham
phav ~~ c(NA, NA, NA, NA)*phds + c(rho.4_2.g1, rho.4_2.g2, rho.4_2.g3, rho.4_2.g4)*phds
phav ~~ c(NA, NA, NA, NA)*phex + c(rho.5_2.g1, rho.5_2.g2, rho.5_2.g3, rho.5_2.g4)*phex
pham ~~ c(NA, NA, NA, NA)*phds + c(rho.4_3.g1, rho.4_3.g2, rho.4_3.g3, rho.4_3.g4)*phds
pham ~~ c(NA, NA, NA, NA)*phex + c(rho.5_3.g1, rho.5_3.g2, rho.5_3.g3, rho.5_3.g4)*phex
phds ~~ c(NA, NA, NA, NA)*phex + c(rho.5_4.g1, rho.5_4.g2, rho.5_4.g3, rho.5_4.g4)*phex

#drho1 := rho.2_1.g1 - rho.2_1.g2
#drho2 := rho.2_1.g1 - rho.2_1.g3
#drho3 := rho.2_1.g1 - rho.2_1.g4
#drho4 := rho.2_1.g2 - rho.2_1.g3
#drho5 := rho.2_1.g2 - rho.2_1.g4
#drho6 := rho.2_1.g3 - rho.2_1.g4

#drho7 := rho.3_1.g1 - rho.3_1.g2
#drho8 := rho.3_1.g1 - rho.3_1.g3
#drho9 := rho.3_1.g1 - rho.3_1.g4
#drho10 := rho.3_1.g2 - rho.3_1.g3
#drho11 := rho.3_1.g2 - rho.3_1.g4
#drho12 := rho.3_1.g3 - rho.3_1.g4

#drho13 := rho.4_1.g1 - rho.4_1.g2
#drho14 := rho.4_1.g1 - rho.4_1.g3
#drho15 := rho.4_1.g1 - rho.4_1.g4
#drho16 := rho.4_1.g2 - rho.4_1.g3
#drho17 := rho.4_1.g2 - rho.4_1.g4
#drho18 := rho.4_1.g3 - rho.4_1.g4

#comparing dyads regarding secure attachment and internalizing problems
drho18 := rho.5_1.g1 - rho.5_1.g2
drho19 := rho.5_1.g1 - rho.5_1.g3
drho20 := rho.5_1.g1 - rho.5_1.g4
drho21 := rho.5_1.g2 - rho.5_1.g3
drho22 := rho.5_1.g2 - rho.5_1.g4
drho23 := rho.5_1.g3 - rho.5_1.g4

#drho24 := rho.3_2.g1 - rho.3_2.g2
#drho25 := rho.3_2.g1 - rho.3_2.g3
#drho26 := rho.3_2.g1 - rho.3_2.g4
#drho27 := rho.3_2.g2 - rho.3_2.g3
#drho28 := rho.3_2.g2 - rho.3_2.g4
#drho29 := rho.3_2.g3 - rho.3_2.g4

#drho30 := rho.4_2.g1 - rho.4_2.g2
#drho31 := rho.4_2.g1 - rho.4_2.g3
#drho32 := rho.4_2.g1 - rho.4_2.g4
#drho33 := rho.4_2.g2 - rho.4_2.g3
#drho34 := rho.4_2.g2 - rho.4_2.g4
#drho35 := rho.4_2.g3 - rho.4_2.g4

#comparing dyads regarding avoidant attachment and internalizing problems
drho35 := rho.5_2.g1 - rho.5_2.g2
drho36 := rho.5_2.g1 - rho.5_2.g3
drho37 := rho.5_2.g1 - rho.5_2.g4
drho38 := rho.5_2.g2 - rho.5_2.g3
drho39 := rho.5_2.g2 - rho.5_2.g4
drho40 := rho.5_2.g3 - rho.5_2.g4

#drho40 := rho.4_3.g1 - rho.4_3.g2
#drho41 := rho.4_3.g1 - rho.4_3.g3
#drho42 := rho.4_3.g1 - rho.4_3.g4
#drho43 := rho.4_3.g2 - rho.4_3.g3
#drho44 := rho.4_3.g2 - rho.4_3.g4
#drho45 := rho.4_3.g3 - rho.4_3.g4

#comparing dyads regarding ambivalent attachment and internalizing problems
drho46 := rho.5_3.g1 - rho.5_3.g2
drho47 := rho.5_3.g1 - rho.5_3.g3
drho48 := rho.5_3.g1 - rho.5_3.g4
drho49 := rho.5_3.g2 - rho.5_3.g3
drho50 := rho.5_3.g2 - rho.5_3.g4
drho51 := rho.5_3.g3 - rho.5_3.g4

#comparing dyads regarding disorganized attachment and internalizing problems
drho52 := rho.5_4.g1 - rho.5_4.g2
drho53 := rho.5_4.g1 - rho.5_4.g3
drho54 := rho.5_4.g1 - rho.5_4.g4
drho55 := rho.5_4.g2 - rho.5_4.g3
drho56 := rho.5_4.g2 - rho.5_4.g4
drho57 := rho.5_4.g3 - rho.5_4.g4

#comparing similar vs different dyads for each attachment type and internalizing problems
drho58 := (rho.5_1.g2+rho.5_1.g4)/2 - (rho.5_1.g1+rho.5_1.g3)/2
drho59 := (rho.5_2.g2+rho.5_2.g4)/2 - (rho.5_2.g1+rho.5_2.g3)/2
drho60 := (rho.5_3.g2+rho.5_3.g4)/2 - (rho.5_3.g1+rho.5_3.g3)/2
drho61 := (rho.5_4.g2+rho.5_4.g4)/2 - (rho.5_4.g1+rho.5_4.g3)/2

#comparing mothers vs fathers for each attachment type and internalizing problems
drho62 := (rho.5_1.g2+rho.5_1.g1)/2 - (rho.5_1.g4+rho.5_1.g3)/2
drho63 := (rho.5_2.g2+rho.5_2.g1)/2 - (rho.5_2.g4+rho.5_2.g3)/2
drho64 := (rho.5_3.g2+rho.5_3.g1)/2 - (rho.5_3.g4+rho.5_3.g3)/2
drho65 := (rho.5_4.g2+rho.5_4.g1)/2 - (rho.5_4.g4+rho.5_4.g3)/2


#comparing songs vs daugthers for each attachment type and internalizing problems
drho66 := (rho.5_1.g2+rho.5_1.g3)/2 - (rho.5_1.g4+rho.5_1.g1)/2
drho67 := (rho.5_2.g2+rho.5_2.g3)/2 - (rho.5_2.g4+rho.5_2.g1)/2
drho68 := (rho.5_3.g2+rho.5_3.g3)/2 - (rho.5_3.g4+rho.5_3.g1)/2
drho69 := (rho.5_4.g2+rho.5_4.g3)/2 - (rho.5_4.g4+rho.5_4.g1)/2
'
control_age_of_parent <- 'phsa ~ parent_age
                          phav ~ parent_age
                          pham ~ parent_age
                          phds ~ parent_age
                          phex ~ parent_age'
mm3 <- c(mm2, phs)
mm4 <- c(mm3, ph_cors)
mm5 <- c(mm4, control_age_of_parent)

sink(file = "mm5_externalizing_problems_age.txt")
cat(mm5)
sink(file = NULL)


sam2 <- cfa(model = mm5, 
            data = imp_attachment,
            group= "dyad",
            estimator = "WLSMV")

# save to text file to visualize all the output
sink(file = "sam2_summary_externalizing_problems_age.txt")
summary(sam2,standardized=T, ci=T)
sink(file = NULL)

#check the item reliability
cfa_externalizing <- cfa(model = mm, 
                         data = imp_attachment,
                         group= "dyad",
                         estimator = "WLSMV")

# check for factor reliability
r_squared_values_externalizing <- inspect(cfa_externalizing, "r2")
save(r_squared_values_externalizing, file ="r_squared_values_externalizing.Rdata")

#check the factor reliability
compRelSEM(cfa_externalizing)

## Correlation differences

phs <- c("phsa","phav","pham","phds","phin")
pars_cor <- parameterestimates(sam2)

## factor correlations
pars_cor[pars_cor$op == "~~" & pars_cor$lhs != pars_cor$rhs & pars_cor$lhs %in% phs,]

# define parameters (differences)
df_comparisons <- pars_cor[pars_cor$op == ":=",]
pars_cor[pars_cor$op == ":=" & pars_cor$pvalue < .05,]
pars_cor[pars_cor$op == ":=" & pars_cor$pvalue < .01,]

#Adjust p-values for false discovery rate
pvals <- pars_cor[pars_cor$op == ":=" ,]$pvalue
df_comparisons$adjusted_p_values <- p.adjust(pvals, method = "fdr")
print(df_comparisons$adjusted_p_values)

#these are the comparisons that are significant at 0.05 after adjustment for false-discovery rate
df_comparisons[df_comparisons[,"adjusted_p_values"]<0.05, ]


#Save this workspace
save.image(file = "R_workspace_comparisons_problembehavior_with_age_18.08.RData")

#Load workspace
load("R_workspace_comparisons_problembehavior_with_age_18.08.RData")

#Check correlations at the level of the entire sample

group <- measEq.syntax(configural.model = c(model_secure_attachment,
                                            model_avoidant_attachment,
                                            model_ambivalent_attachment,
                                            model_disorganised_attachment,
                                            model_externalizing, 
                                            model_internalizing),
                       data = imp_attachment,
                       ordered = TRUE,
                       parameterization = "theta",
                       ID.fac = "std.lv",
                       ID.cat = "Wu.Estabrook.2016")

sam10<- cfa(model = as.character(group), 
            data = imp_attachment,
            estimator = "WLSMV")
summary(sam10)
