# Establishing Measurement Invariance with Lavaan - Attachment and Prosocial Behavior
# Authors: Magda Matetovici, Mauricio Garnier Villarreal 
# Last Modified: 31.08.2024

#-------------------------------------------------------------------------------
#Install Packages

#-------------------------------------------------------------------------------
#Load packages
library(lavaan)
library(mice)
library(dplyr)
library(semTools)
#-------------------------------------------------------------------------------
# Load datafiles
data_attachment_recoded<- read.csv(file = "data_attachment_original_recoded.csv")
data_problem_recoded <- read.csv(file = "data_sdq_original_recoded.csv")
#-------------------------------------------------------------------------------
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
imp_attachment <- mice(data_attachment_recoded[, 49:97], m = 1, method = "polr", maxit=20)
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


colnames(data_problem_recoded)[7] <- "dyad"
data_problem_recoded$dyad <- imp_attachment$dyad
#multiple imputation for problem data (2 missing values)

imp_problems <- mice(data_problem_recoded[, 22:42], m = 1, method = "polr")
imp_problems <- complete(imp_problems, 1)

data_attachment_recoded$dyad <- factor(data_attachment_recoded$dyad , ordered = FALSE)

data_problem_recoded$dyad <- factor(data_problem_recoded$dyad , ordered = FALSE)
################################################################################
#ATTACHMENT
################################################################################
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------                   
### Configural- invariance

model_secure_attachment <- '
        secure_attachment =~ C_ari6_recoded + C_ari11_recoded + C_ari17_recoded + C_ari30_recoded + C_ari37_recoded + 
                              C_ari42_recoded + C_ari46_recoded + C_ari47_recoded + C_ari50_recoded + C_ari54_recoded + 
                                C_ari59_recoded + C_ari61_recoded + C_ari64_recoded
                    '
model_avoidant_attachment <-'
        avoidant_attachment =~ C_ari3_recoded + C_ari14_recoded + C_ari23_recoded + C_ari25_recoded + C_ari26_recoded + 
                                C_ari31_recoded + C_ari33_recoded + C_ari34_recoded + C_ari36_recoded + C_ari57_recoded + 
                                  C_ari66_recoded
                            '
model_ambivalent_attachment <- '
        ambivalent_attachment =~ C_ari12_recoded + C_ari15_recoded + C_ari16_recoded + C_ari19_recoded + C_ari38_recoded + 
                                  C_ari39_recoded + C_ari51_recoded + C_ari53_recoded + C_ari56_recoded + C_ari58_recoded + 
                                    C_ari63_recoded'
model_disorganised_attachment <-'
                                
        disorganised_attachment =~ C_ari4_recoded + C_ari8_recoded + C_ari13_recoded + C_ari18_recoded + C_ari22_recoded + 
                                    C_ari28_recoded + C_ari29_recoded + C_ari35_recoded + C_ari40_recoded + C_ari43_recoded + 
                                      C_ari49_recoded + C_ari62_recoded + C_ari65_recoded'

################################################################################
## Configural models
################################################################################

#Configural model Secure Attachment
fit_configural_secure <-  cfa(model = model_secure_attachment, 
                              data = imp_attachment,
                              group = "dyad",
                              ordered = TRUE,
                              estimator = 'WLSMV',
                              parameterization = "theta",
                              std.lv = TRUE)

summary(fit_configural_secure)
fitMeasures(fit_configural_secure)
moreFitIndices(fit_configural_secure)

#for some items the responses for some categories is really low, for example item 59:
group_tables_item61 <- lapply(split(imp_attachment$C_ari61_recoded, imp_attachment$dyad), table)
group_tables_item59 <- lapply(split(imp_attachment$C_ari59_recoded, imp_attachment$dyad), table)
#-------------------------------------------------------------------------------

#Configural model Ambivalent Attachment
fit_configural_ambivalent <-  cfa(model = model_ambivalent_attachment, 
                              data = imp_attachment,
                              group = "dyad",
                              ordered = TRUE,
                              estimator = 'WLSMV',
                              parameterization = "theta",
                              std.lv = TRUE)

summary(fit_configural_ambivalent)
fitMeasures(fit_configural_ambivalent)
moreFitIndices(fit_configural_ambivalent)

#Configural model Avoidant Attachment
fit_configural_avoidant <-  cfa(model = model_avoidant_attachment, 
                                  data = imp_attachment,
                                  group = "dyad",
                                  ordered = TRUE,
                                  estimator = 'WLSMV',
                                  parameterization = "theta",
                                  std.lv = TRUE)

summary(fit_configural_avoidant, fit.measures=T)
fitMeasures(fit_configural_avoidant)
moreFitIndices(fit_configural_avoidant)
####
#### check the model fit of this model, maybe modifications?

##items 23 and 25 have in common the fact that the child keeps distance from the parent
## the relation between 57 and 66 is not so clear theoretically -> not introduced in the model

model_avoidant_attachment2 <- paste(model_avoidant_attachment,
                                    "C_ari23_recoded ~~ c(0,0,NA,0)*C_ari25_recoded",
                                    sep= "\n")

fit_configural_avoidant2 <-  cfa(model = model_avoidant_attachment2, 
                                data = imp_attachment,
                                group = "dyad",
                                ordered = TRUE,
                                estimator = 'WLSMV',
                                parameterization = "theta",
                                std.lv = TRUE)

summary(fit_configural_avoidant2, fit.measures=T, standardized=T)
fitMeasures(fit_configural_avoidant2)
moreFitIndices(fit_configural_avoidant2)
modindices(fit_configural_avoidant2, sort=T)[1:10,]
#-------------------------------------------------------------------------------
#Configural model Disorganised Attachment
fit_configural_disorganised <-  cfa(model = model_disorganised_attachment, 
                                data = imp_attachment,
                                group = "dyad",
                                ordered = TRUE,
                                estimator = 'WLSMV',
                                parameterization = "theta",
                                std.lv = TRUE)

summary(fit_configural_disorganised)
fitMeasures(fit_configural_disorganised)
moreFitIndices(fit_configural_disorganised)


################################################################################
### Threshold models Attachment
################################################################################

# Threshold invariance Secure Attachment
threshold_secure_model <- measEq.syntax(configural.model = model_secure_attachment,
                      data = imp_attachment,
                      ordered = TRUE,
                      parameterization = "theta",
                      ID.fac = "std.lv",
                      ID.cat = "Wu.Estabrook.2016",
                      group = "dyad",
                      group.equal = "thresholds")
threshold_secure_model %>% as.character()%>% cat ()
fit_threshold_secure_attachment <- cfa(model = as.character(threshold_secure_model), 
                            data = imp_attachment,
                            group = "dyad",
                            ordered = TRUE,
                            estimator = 'WLSMV')
fitMeasures(fit_threshold_secure_attachment)
moreFitIndices(fit_threshold_secure_attachment)
anova_threshold_secure <- anova(fit_configural_secure,fit_threshold_secure_attachment)
cf1 <- compareFit(fit_configural_secure,fit_threshold_secure_attachment)
summary(cf1)

#### PASSES
#-------------------------------------------------------------------------------
# Threshold invariance Avoidant Attachment
threshold_avoidant_model <- measEq.syntax(configural.model = model_avoidant_attachment2,
                                        data = imp_attachment,
                                        ordered = TRUE,
                                        parameterization = "theta",
                                        ID.fac = "std.lv",
                                        ID.cat = "Wu.Estabrook.2016",
                                        group = "dyad",
                                        group.equal = "thresholds")
threshold_avoidant_model %>% as.character()%>% cat ()

fit_threshold_avoidant_attachment <- cfa(model = as.character(threshold_avoidant_model), 
                                       data = imp_attachment,
                                       group = "dyad",
                                       ordered = TRUE,
                                       estimator = 'WLSMV')
fitMeasures(fit_threshold_avoidant_attachment)
moreFitIndices(fit_threshold_avoidant_attachment)

anova_threshold_avoidant <- anova(fit_configural_avoidant2,fit_threshold_avoidant_attachment)
cf2 <- compareFit(fit_configural_avoidant2,fit_threshold_avoidant_attachment)
summary(cf2)

#### PASSES WITH ALPHA .01



#-------------------------------------------------------------------------------
# Threshold invariance Ambivalent Attachment
threshold_ambivalent_model <- measEq.syntax(configural.model = model_ambivalent_attachment,
                                          data = imp_attachment,
                                          ordered = TRUE,
                                          parameterization = "theta",
                                          ID.fac = "std.lv",
                                          ID.cat = "Wu.Estabrook.2016",
                                          group = "dyad",
                                          group.equal = "thresholds")
threshold_ambivalent_model %>% as.character()%>% cat ()
fit_threshold_ambivalent_attachment <- cfa(model = as.character(threshold_ambivalent_model), 
                                         data = imp_attachment,
                                         group = "dyad",
                                         ordered = TRUE,
                                         estimator = 'WLSMV')
fitMeasures(fit_threshold_ambivalent_attachment)
moreFitIndices(fit_threshold_ambivalent_attachment)

anova_threshold_ambivalent <- anova(fit_configural_ambivalent,fit_threshold_ambivalent_attachment)
cf3 <- compareFit(fit_configural_ambivalent,fit_threshold_ambivalent_attachment)
summary(cf3)
#### PASSES

#-------------------------------------------------------------------------------
# Threshold invariance Disorganised Attachment
threshold_disorganised_model <- measEq.syntax(configural.model = model_disorganised_attachment,
                                            data = imp_attachment,
                                            ordered = TRUE,
                                            parameterization = "theta",
                                            ID.fac = "std.lv",
                                            ID.cat = "Wu.Estabrook.2016",
                                            group = "dyad",
                                            group.equal = "thresholds")
threshold_disorganised_model %>% as.character()%>% cat ()
fit_threshold_disorganised_attachment <- cfa(model = as.character(threshold_disorganised_model), 
                                           data = imp_attachment,
                                           group = "dyad",
                                           ordered = TRUE,
                                           estimator = 'WLSMV')

fitMeasures(fit_threshold_disorganised_attachment)
moreFitIndices(fit_threshold_disorganised_attachment)

anova_threshold_disorganised <- anova(fit_configural_disorganised,fit_threshold_disorganised_attachment)
cf4 <- compareFit(fit_configural_disorganised,fit_threshold_disorganised_attachment)
summary(cf4)

#### PASSES
################################################################################
### Weak invariance
###############################################################################

# Weak Secure Attachment
weak_model_secure <- measEq.syntax(configural.model = model_secure_attachment,
                            data = imp_attachment,
                            ordered = TRUE,
                            parameterization = "theta",
                            ID.fac = "std.lv",
                            ID.cat = "Wu.Estabrook.2016",
                            group = "dyad",
                            group.equal = c("thresholds", "loadings"))

weak_model_secure %>% as.character()%>% cat ()

fit_weak_secure <- cfa(model = as.character(weak_model_secure), 
                           data = imp_attachment,
                           group = "dyad",
                           ordered = TRUE,
                           estimator = 'WLSMV')
fitMeasures(fit_weak_secure)
moreFitIndices(fit_weak_secure)
summary(fit_weak_secure)
anova_weak_secure <- anova(fit_threshold_secure_attachment,fit_weak_secure)
cf5 <- compareFit(fit_threshold_secure_attachment,fit_weak_secure)
summary(cf5)

lavTestScore(fit_weak_secure)
partable(fit_weak_secure)

#partial invariance

weak_model_secure_modified <- measEq.syntax(configural.model = model_secure_attachment,
                                   data = imp_attachment,
                                   ordered = TRUE,
                                   parameterization = "theta",
                                   ID.fac = "std.lv",
                                   ID.cat = "Wu.Estabrook.2016",
                                   group = "dyad",
                                   group.equal = c("thresholds", "loadings"),
                                   group.partial = c("secure_attachment =~C_ari61_recoded", "secure_attachment =~C_ari30_recoded", "secure_attachment =~C_ari46_recoded" ))

fit_weak_secure_modified <- cfa(model = as.character(weak_model_secure_modified), 
                       data = imp_attachment,
                       group = "dyad",
                       ordered = TRUE,
                       estimator = 'WLSMV')
fitMeasures(fit_weak_secure_modified)
moreFitIndices(fit_weak_secure_modified)
anova_weak_secure_modified <- anova(fit_threshold_secure_attachment,fit_weak_secure_modified)
cf6 <- compareFit(fit_threshold_secure_attachment,fit_weak_secure_modified)
summary(cf6)

#MODIFIED PASSES
#-------------------------------------------------------------------------------
# Weak Avoidant Attachment
weak_model_avoidant <- measEq.syntax(configural.model = model_avoidant_attachment2,
                                   data = imp_attachment,
                                   ordered = TRUE,
                                   parameterization = "theta",
                                   ID.fac = "std.lv",
                                   ID.cat = "Wu.Estabrook.2016",
                                   group = "dyad",
                                   group.equal = c("thresholds", "loadings"))

weak_model_avoidant %>% as.character()%>% cat ()

fit_weak_avoidant <- cfa(model = as.character(weak_model_avoidant), 
                           data = imp_attachment,
                           group = "dyad",
                           ordered = TRUE,
                           estimator = 'WLSMV')
fitMeasures(fit_weak_avoidant)
moreFitIndices(fit_weak_avoidant)
anova_weak_avoidant <- anova(fit_threshold_avoidant_attachment,fit_weak_avoidant)
cf7 <- compareFit(fit_threshold_avoidant_attachment,fit_weak_avoidant)
summary(cf7)


lavTestScore(fit_weak_avoidant)
partable(fit_weak_avoidant)


#partial invariance

weak_model_avoidant_modified <- measEq.syntax(configural.model = model_avoidant_attachment2,
                                            data = imp_attachment,
                                            ordered = TRUE,
                                            parameterization = "theta",
                                            ID.fac = "std.lv",
                                            ID.cat = "Wu.Estabrook.2016",
                                            group = "dyad",
                                            group.equal = c("thresholds", "loadings"),
                                            group.partial = c("avoidant_attachment =~C_ari57_recoded", "avoidant_attachment =~C_ari66_recoded"))

fit_weak_avoidant_modified <- cfa(model = as.character(weak_model_avoidant_modified), 
                                data = imp_attachment,
                                group = "dyad",
                                ordered = TRUE,
                                estimator = 'WLSMV')
fitMeasures(fit_weak_avoidant_modified)
moreFitIndices(fit_weak_avoidant_modified)
anova_weak_avoidant_modified <- anova(fit_threshold_avoidant_attachment,fit_weak_avoidant_modified)
cf8 <- compareFit(fit_threshold_avoidant_attachment,fit_weak_avoidant_modified)
summary(cf8)

#MODIFIED PASSES

#-------------------------------------------------------------------------------
# Weak Ambivalent Attachment
weak_model_ambivalent <- measEq.syntax(configural.model = model_ambivalent_attachment,
                                   data = imp_attachment,
                                   ordered = TRUE,
                                   parameterization = "theta",
                                   ID.fac = "std.lv",
                                   ID.cat = "Wu.Estabrook.2016",
                                   group = "dyad",
                                   group.equal = c("thresholds", "loadings"))

weak_model_ambivalent %>% as.character()%>% cat ()

fit_weak_ambivalent <- cfa(model = as.character(weak_model_ambivalent), 
                           data = imp_attachment,
                           group = "dyad",
                           ordered = TRUE,
                           estimator = 'WLSMV')
fitMeasures(fit_weak_ambivalent)
moreFitIndices(fit_weak_ambivalent)
anova_weak_ambivalent <- anova(fit_threshold_ambivalent_attachment,fit_weak_ambivalent)
cf9 <- compareFit(fit_threshold_ambivalent_attachment,fit_weak_ambivalent)
summary(cf9)


lavTestScore(fit_weak_ambivalent)
my_partable <- partable(fit_weak_ambivalent)

#partial invariance

weak_model_ambivalent_modified <- measEq.syntax(configural.model = model_ambivalent_attachment,
                                              data = imp_attachment,
                                              ordered = TRUE,
                                              parameterization = "theta",
                                              ID.fac = "std.lv",
                                              ID.cat = "Wu.Estabrook.2016",
                                              group = "dyad",
                                              group.equal = c("thresholds", "loadings"),
                                              group.partial = c("ambivalent_attachment =~C_ari19_recoded", "ambivalent_attachment =~C_ari13_recoded",
                                                                "ambivalent_attachment =~C_ari15_recoded",
                                                                "ambivalent_attachment =~C_ari58_recoded",
                                                                "ambivalent_attachment =~C_ari16_recoded"))

fit_weak_ambivalent_modified <- cfa(model = as.character(weak_model_ambivalent_modified), 
                                  data = imp_attachment,
                                  group = "dyad",
                                  ordered = TRUE,
                                  estimator = 'WLSMV')
fitMeasures(fit_weak_ambivalent_modified)
moreFitIndices(fit_weak_ambivalent_modified)

anova_weak_ambivalent_modified <- anova(fit_threshold_ambivalent_attachment,fit_weak_ambivalent_modified)
cf10 <- compareFit(fit_threshold_ambivalent_attachment,fit_weak_ambivalent_modified)
summary(cf10)

#PASSES with modifications

#-------------------------------------------------------------------------------
# Weak Disorganised Attachment
weak_model_disorganised <- measEq.syntax(configural.model = model_disorganised_attachment,
                                       data = imp_attachment,
                                       ordered = TRUE,
                                       parameterization = "theta",
                                       ID.fac = "std.lv",
                                       ID.cat = "Wu.Estabrook.2016",
                                       group = "dyad",
                                       group.equal = c("thresholds", "loadings"))

weak_model_disorganised %>% as.character()%>% cat ()

fit_weak_disorganised <- cfa(model = as.character(weak_model_disorganised), 
                           data = imp_attachment,
                           group = "dyad",
                           ordered = TRUE,
                           estimator = 'WLSMV')
fitMeasures(fit_weak_disorganised)
moreFitIndices(fit_weak_disorganised)
anova_weak_disorganised <- anova(fit_threshold_disorganised_attachment,fit_weak_disorganised)
cf11 <- compareFit(fit_threshold_disorganised_attachment,fit_weak_disorganised)
summary(cf11)

####  PASSES WITH ALPHA .01



################################################################################
#PROBLEM BEHAVIOR
################################################################################

#-------------------------------------------------------------------------------
#Internalizing Problems
#-------------------------------------------------------------------------------
### Configural- invariance

model_internalizing<- '
        internalising =~ C_SDQ8.2_3_recoded +  C_SDQ8.2_8_recoded + C_SDQ8.2_13_recoded + 
                                    C_SDQ8.2_16_recoded + C_SDQ8.2_24_recoded + C_SDQ8.2_6_recoded + 
                                    C_SDQ8.2_11_recoded + C_SDQ8.2_14_recoded + C_SDQ8.2_19_recoded + 
                                    C_SDQ8.2_23_recoded
'

#Configural model 
fit_configural_internalizing <-  cfa(model = model_internalizing, 
                              data = imp_problems,
                              group = "dyad",
                              ordered = TRUE,
                              estimator = 'WLSMV',
                              parameterization = "theta",
                              std.lv = TRUE)

summary(fit_configural_internalizing)
fitMeasures(fit_configural_internalizing)
moreFitIndices(fit_configural_internalizing)

modindices(fit_configural_internalizing, sort=T)[1:10,]

#Modifications needed - > correlation between item 11 and 14 in group 3  
model_internalizing2 <- paste(model_internalizing,
                                    "C_SDQ8.2_11_recoded ~~ c(0,0,NA,0)*C_SDQ8.2_14_recoded",
                                    sep= "\n")

fit_configural_internalizing2 <-  cfa(model = model_internalizing2, 
                                     data = imp_problems,
                                     group = "dyad",
                                     ordered = TRUE,
                                     estimator = 'WLSMV',
                                     parameterization = "theta",
                                     std.lv = TRUE)

summary(fit_configural_internalizing2)
fitMeasures(fit_configural_internalizing2)
moreFitIndices(fit_configural_internalizing2)
#--------------------------------------------------------------------------------
### Treshold invariance
#Directly testing loadings because we have only 3 response options
# 
# threshold_internalizing <- measEq.syntax(configural.model = model_internalizing2,
#                                  data = imp_problems,
#                                  ordered = TRUE,
#                                  parameterization = "theta",
#                                  ID.fac = "std.lv",
#                                  ID.cat = "Wu.Estabrook.2016",
#                                  group = "dyad",
#                                  group.equal = "thresholds")
# 
# threshold_internalizing %>% as.character()%>% cat ()
# 
# fit_threshold_internalizing <- cfa(model = as.character(threshold_internalizing), 
#                                        data = imp_problems,
#                                        group = "dyad",
#                                        ordered = TRUE,
#                                        estimator = 'WLSMV')
# fitMeasures(fit_threshold_internalizing)
# anova_threshold_internalizing <- anova(fit_configural_internalizing2,fit_threshold_internalizing)

#-------------------------------------------------------------------------------
### Weak invariance
weak_model_internalizing <- measEq.syntax(configural.model = model_internalizing2,
                                           data = imp_problems,
                                           ordered = TRUE,
                                           parameterization = "theta",
                                           ID.fac = "std.lv",
                                           ID.cat = "Wu.Estabrook.2016",
                                           group = "dyad",
                                           group.equal = c("thresholds", "loadings"))

weak_model_internalizing  %>% as.character()%>% cat ()

fit_weak_model_internalizing <- cfa(model = as.character(weak_model_internalizing), 
                               data = imp_problems,
                               group = "dyad",
                               ordered = TRUE,
                               estimator = 'WLSMV')
fitMeasures(fit_weak_model_internalizing)
moreFitIndices(fit_weak_model_internalizing)
anova_threshold_internalizing <- anova(fit_configural_internalizing2,fit_weak_model_internalizing)

cf12 <- compareFit(fit_configural_internalizing2,fit_weak_model_internalizing)
summary(cf12)
#Modifications needed
lavTestScore(fit_weak_model_internalizing)
my_partable <- partable(fit_weak_model_internalizing)

weak_model_internalizing_modified <- measEq.syntax(configural.model = model_internalizing2,
                                          data = imp_problems,
                                          ordered = TRUE,
                                          parameterization = "theta",
                                          ID.fac = "std.lv",
                                          ID.cat = "Wu.Estabrook.2016",
                                          group = "dyad",
                                          group.equal = c("thresholds", "loadings"),
                                          group.partial = c("internalising  =~ C_SDQ8.2_11_recoded ", "internalising  =~ C_SDQ8.2_16_recoded", "internalising  =~ C_SDQ8.2_19_recoded"))

weak_model_internalizing_modified  %>% as.character()%>% cat ()

fit_weak_model_internalizing_modified <- cfa(model = as.character(weak_model_internalizing_modified), 
                                    data = imp_problems,
                                    group = "dyad",
                                    ordered = TRUE,
                                    estimator = 'WLSMV')
fitMeasures(fit_weak_model_internalizing_modified)
moreFitIndices(fit_weak_model_internalizing_modified)
anova_threshold_internalizing_modified <- anova(fit_configural_internalizing2,fit_weak_model_internalizing_modified)
#PASSES With Modifications 

#-------------------------------------------------------------------------------
#Externalizing Problems
#-------------------------------------------------------------------------------
#Configural Invariance
model_externalizing <- '
         externalising =~ C_SDQ8.2_5_recoded + C_SDQ8.2_7_recoded + C_SDQ8.2_12_recoded + C_SDQ8.2_18_recoded + 
                                    C_SDQ8.2_22_recoded + C_SDQ8.2_2_recoded + C_SDQ8.2_10_recoded + C_SDQ8.2_15_recoded + 
                                    C_SDQ8.2_21_recoded + C_SDQ8.2_25_recoded
'

#Configural model 
fit_configural_model_externalizing <-  cfa(model = model_externalizing, 
                                     data = imp_problems,
                                     group = "dyad",
                                     ordered = TRUE,
                                     estimator = 'WLSMV',
                                     parameterization = "theta",
                                     std.lv = TRUE)

summary(fit_configural_model_externalizing)
fitMeasures(fit_configural_model_externalizing)
moreFitIndices(fit_configural_model_externalizing)



#Weak Invariance
### Weak invariance
weak_model_externalizing <- measEq.syntax(configural.model = model_externalizing,
                                          data = imp_problems,
                                          ordered = TRUE,
                                          parameterization = "theta",
                                          ID.fac = "std.lv",
                                          ID.cat = "Wu.Estabrook.2016",
                                          group = "dyad",
                                          group.equal = c("thresholds", "loadings"))

weak_model_externalizing  %>% as.character()%>% cat ()

fit_weak_model_externalizing <- cfa(model = as.character(weak_model_externalizing), 
                                    data = imp_problems,
                                    group = "dyad",
                                    ordered = TRUE,
                                    estimator = 'WLSMV')
fitMeasures(fit_weak_model_externalizing)
moreFitIndices(fit_weak_model_externalizing)
anova_threshold_externalizing <- anova(fit_configural_model_externalizing,fit_weak_model_externalizing)

#Passes