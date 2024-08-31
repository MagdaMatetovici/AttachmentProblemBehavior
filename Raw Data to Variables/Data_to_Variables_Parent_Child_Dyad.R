# Data Parent and Child Dyad - Extracts variables of interest from the raw data file
# Author: Magda Matetovici

#Packages
#install.packages("foreign")
#install.packages("dplyr")

# Data Cleaning: This code extracts the variables of interest from the raw data file
#Load Raw Dataset
library(foreign)
data <- read.spss("C:/Users/matet/Desktop/Parent and Child Dyad/data_pcd.sav", to.data.frame=TRUE)

#Gender of Children
child_gender <- data$A_childgender

#Gender of Parent
parent_gender<- data$A_parentgender

# Create a separate dataframe with ARI-CP items (Attachment measure) and convert them to numeric
ari_data <- data[, 110:175]
item_number <- c("25", "46","3", "4", "6", "8", "11", "12", "13", "14", "15", "16", "17", "18",
                "19", "22", "23", "24", "26", "28", "29", "30", "31", "33", "34",
                "35", "36", "37", "38", "39", "40", "42", "43",
                "47", "49", "50", "51", "53", "54", "56", "57", "58", "59",
                "61", "62", "63", "64", "65", "66")
item_name <- "C_ari"
column_names <- paste0(item_name, item_number)
ari_data <- ari_data[, column_names]

# make it all characters from factors
ari_data[] <- lapply(ari_data, function(x) {
  if(is.factor(x)) as.character(x) else x
})

#recode the variables from text to numbers
ari_data[] <- lapply(ari_data, function(x) replace(x, x=="Helemaal niet van toepassing", "1"))

ari_data[] <- lapply(ari_data, function(x) replace(x, x=="Nauwelijks van toepassing", "2"))

ari_data[] <- lapply(ari_data, function(x) replace(x, x=="Soms wel, soms niet van toepassing", "3"))

ari_data[] <- lapply(ari_data, function(x) replace(x, x=="Redelijk van toepassing", "4"))

ari_data[] <- lapply(ari_data, function(x) replace(x, x=="Helemaal van toepassing", "5"))

#turn everything into numeric from character
ari_data[] <- lapply(ari_data, function(x) {
  if(is.character(x)) as.numeric(x) else x
})


#Secure Attachment, items (from the official questionnaire): 1,4,6,12,21,27,31,33,35,42,43,46
#variables name from the raw data: ari6 + ari11 + ari17 + ari30 + ari37 + ari42 + ari46 + ari47 + ari50 + ari54 + ari59 + ari61 + ari64

ari_data$secure_attachment <-ari_data[,"C_ari6"] + ari_data[,"C_ari11"] + ari_data[,"C_ari17"] + ari_data[,"C_ari30"] 
                    + ari_data[,"C_ari37"] + ari_data[,"C_ari42"] + ari_data[,"C_46"] + ari_data[,"C_ari47"]
                    + ari_data[,"C_ari50"] + ari_data[,"C_ari54"] + ari_data[,"C_ari59"]+ ari_data[,"C_ari61"]+ ari_data[,"C_ari64"] 


#Avoidant attachment, items (from the official questionnaire): 2,9,16,17,18,22,23,24,26,40,48
# variables name from the raw data: ari3 + ari14 + ari23 + ari25 + ari26 + ari31 + ari33 + ari34 + ari36 + ari57 + ari66

ari_data$avoidant_attachment <- ari_data[,"C_ari3"] + ari_data[,"C_ari14"] + ari_data[,"C_ari23"] + ari_data[,"C_ari25"] 
                            + ari_data[,"C_ari26"] + ari_data[,"C_ari31"] + ari_data[,"C_ari33"] + ari_data[,"C_ari34"]
                            + ari_data[,"C_ari36"] + ari_data[,"C_ari66"] + ari_data[,"C_ari57"] 


#Ambivalent attachment, items (from the official questionnaire): 7, 10, 14, 28,29,36,37,39,41,45
# variables name from the raw data: ari12 + ari15 + ari16 + ari19 + ari38 + ari39 + ari51 + ari53 + ari56 + ari58 + ari63

ari_data$ambivalent_attachment <- ari_data[,"C_ari12"] + ari_data[,"C_ari15"] + ari_data[,"C_ari16"]+ ari_data[,"C_ari19"] + ari_data[,"C_ari38"] 
                                + ari_data[,"C_ari39"] + ari_data[,"C_ari51"] + ari_data[,"C_ari53"] + ari_data[,"C_ari56"]
                                + ari_data[,"C_ari58"] + ari_data[,"C_ari63"] 



#Disorganized attachment, items (from the official questionnaire): 3,5,8,13,15, 19, 20, 25, 30, 32, 34, 44, 47
# variables name from the raw data: ari4 + ari8 + ari13 + ari18 + ari22 + ari28 + ari29 + ari35 + ari40 + ari43 + ari49 + ari62 + ari65

ari_data$disorganized_attachment <- ari_data[,"C_ari4"] + ari_data[,"C_ari8"] + ari_data[,"C_ari13"] + ari_data[,"C_ari18"] 
                                + ari_data[,"C_ari22"] + ari_data[,"C_ari28"] + ari_data[,"C_ari29"] + ari_data[,"C_ari35"]
                                + ari_data[,"C_ari40"] + ari_data[,"C_ari43"]+ ari_data[,"C_ari49"] + ari_data[,"C_ari62"] + ari_data[,"C_ari65"] 

#create a separate dataframe without the sum scores on the scales, only the items
ari_items_only <- cbind(ari_data[,"C_ari46"], ari_data[,"C_ari6"], ari_data[,"C_ari11"], ari_data[,"C_ari17"], 
                        ari_data[,"C_ari30"], ari_data[,"C_ari37"], ari_data[,"C_ari42"], ari_data[,"C_ari47"],
                        ari_data[,"C_ari50"], ari_data[,"C_ari59"], ari_data[,"C_ari6"], ari_data[,"C_ari64"],
                        ari_data[,"C_ari3"], ari_data[,"C_ari14"],ari_data[,"C_ari23"],ari_data[,"C_ari25"],
                        ari_data[,"C_ari26"], ari_data[,"C_ari31"], ari_data[,"C_ari33"],ari_data[,"C_ari34"],
                        ari_data[,"C_ari36"], ari_data[,"C_ari66"], ari_data[,"C_ari57"], 
                        ari_data[,"C_ari12"], ari_data[,"C_ari15"], ari_data[,"C_ari16"], ari_data[,"C_ari19"], ari_data[,"C_ari38"], 
                        ari_data[,"C_ari39"], ari_data[,"C_ari51"], ari_data[,"C_ari53"], ari_data[,"C_ari56"], 
                        ari_data[,"C_ari58"], ari_data[,"C_ari63"], 
                        ari_data[,"C_ari4"], ari_data[,"C_ari8"], ari_data[,"C_ari13"], ari_data[,"C_ari18"],
                        ari_data[,"C_ari22"], ari_data[,"C_ari28"], ari_data[,"C_ari29"], ari_data[,"C_ari35"],
                        ari_data[,"C_ari40"], ari_data[,"C_ari43"], ari_data[,"C_ari49"], ari_data[,"C_ari62"], ari_data[,"C_ari65"])

#recode male (0) and female (1) for both parent and child 

ari_data$parent_gender <- parent_gender
ari_data$child_gender <- child_gender

ari_data[] <- lapply(ari_data, function(x) {
  if(is.factor(x)) as.character(x) else x
})


ari_data[]<- lapply(ari_data, function(x) replace(x, x=="male", "0"))
ari_data[]<- lapply(ari_data, function(x) replace(x, x=="jongen", "0"))

ari_data[]<- lapply(ari_data, function(x) replace(x, x=="female", "1"))
ari_data[]<- lapply(ari_data, function(x) replace(x, x=="meisje", "1"))

# Data SDQ (only items from externalising and internalising scales)

#Externalising problems
#represented by the conduct and hyperactivity scales
#some of the items need to be reversed

item_number_sdq_ex <- c("5", "7", "12", "18","22", "2", "10", "15", "21", "25")
item_name_sdq_ex <- "C_SDQ8.2_"
column_names_sdq_ex <- paste0(item_name_sdq_ex, item_number_sdq_ex)
sdq_data_ex <- data[, column_names_sdq_ex]

#recoding the scores for reverse items
#reverse items: 7, 21, 25

sdq_data_ex$C_SDQ8.2_7 <- ifelse(sdq_data_ex$C_SDQ8.2_7 == "Niet waar", 2,
                   ifelse(sdq_data_ex$C_SDQ8.2_7 == "Een beetje waar", 1,
                          ifelse(sdq_data_ex$C_SDQ8.2_7 == "Zeker waar", 0, sdq_data_ex$C_SDQ8.2_7)))

sdq_data_ex$C_SDQ8.2_21 <- ifelse(sdq_data_ex$C_SDQ8.2_21 == "Niet waar", 2,
                                 ifelse(sdq_data_ex$C_SDQ8.2_21 == "Een beetje waar", 1,
                                        ifelse(sdq_data_ex$C_SDQ8.2_21 == "Zeker waar", 0, sdq_data_ex$C_SDQ8.2_21)))

sdq_data_ex$C_SDQ8.2_25 <- ifelse(sdq_data_ex$C_SDQ8.2_25 == "Niet waar", 2,
                                  ifelse(sdq_data_ex$C_SDQ8.2_25 == "Een beetje waar", 1,
                                         ifelse(sdq_data_ex$C_SDQ8.2_25 == "Zeker waar", 0, sdq_data_ex$C_SDQ8.2_25)))
library(dplyr)
sdq_data_ex <- sdq_data_ex %>%   mutate_all(~ ifelse(. == "Niet waar", 0,
                                                     ifelse(. == "Een beetje waar", 1,
                                                            ifelse(. == "Zeker waar", 2, .))))

#Internalising problems
# represented by the emotional and peer problems scales

item_number_sdq_int <- c("3", "8", "13", "16", "24","6", "11", "14", "19", "23")
item_name_sdq_int <- "C_SDQ8.2_"
column_names_sdq_int <- paste0(item_name_sdq_int, item_number_sdq_int)
sdq_data_int <- data[, column_names_sdq_int]

#recoding the scores fo reverse items
#reverse items: 11, 14 
sdq_data_int$C_SDQ8.2_11 <- ifelse(sdq_data_int$C_SDQ8.2_11 == "Niet waar", 2,
                                   ifelse(sdq_data_int$C_SDQ8.2_11 == "Een beetje waar", 1,
                                          ifelse(sdq_data_int$C_SDQ8.2_11 == "Zeker waar", 0, sdq_data_int$C_SDQ8.2_11)))
sdq_data_int$C_SDQ8.2_14 <- ifelse(sdq_data_int$C_SDQ8.2_14 == "Niet waar", 2,
                                   ifelse(sdq_data_int$C_SDQ8.2_14 == "Een beetje waar", 1,
                                          ifelse(sdq_data_int$C_SDQ8.2_14 == "Zeker waar", 0, sdq_data_int$C_SDQ8.2_14)))

sdq_data_int <- sdq_data_int %>%   mutate_all(~ ifelse(. == "Niet waar", 0,
                                                     ifelse(. == "Een beetje waar", 1,
                                                            ifelse(. == "Zeker waar", 2, .))))

# Make the variable dyad from the gender of the child and the gender of the parents
# Conding for the dyads: female - meisje (1), female-jongen (2), male - meisje (3), male-jongen (4)

data$A_parentgender<- as.character(data$A_parentgender)
data$A_childgender <- as.character(data$A_childgender)

dyad <- c()

for (i in 1: length(data$A_parentgender)){
  if (data$A_parentgender[i] == "female" & data$A_childgender[i] == "meisje"){
    dyad <- c(dyad, 1)
  }
  else if (data$A_parentgender[i] == "female" & data$A_childgender[i] == "jongen"){
    dyad <- c(dyad, 2)
  } 
  else if (data$A_parentgender[i] == "male" & data$A_childgender[i] == "meisje"){
    dyad <- c(dyad, 3)
  }
  else {
    dyad <- c(dyad, 4)
  }
}

data$dyad <- dyad
data$dyad

# Write the sdq data

data_sdq <- cbind(sdq_data_ex, sdq_data_int)
data_sdq$dyad <- factor(data$dyad, labels = c("mother_daughter", "mother_son", "father_daughter", "father_son"))

write.csv(data_sdq, "data_sdq.csv")

# Save the attachment data 
data_attachment <- select(ari_data, 
                          C_ari6, C_ari11, C_ari17, C_ari30, C_ari37, C_ari42, C_ari46, C_ari47, C_ari50, C_ari54, 
                          C_ari59, C_ari61, C_ari64, C_ari3, C_ari14, C_ari23, C_ari25, C_ari26, C_ari31, C_ari33, C_ari34, 
                          C_ari36, C_ari57, C_ari66, C_ari12, C_ari15, C_ari16, C_ari19, C_ari38, C_ari39, C_ari51, 
                          C_ari53, C_ari56, C_ari58, C_ari63, C_ari4, C_ari8, C_ari13, C_ari18, C_ari22, C_ari28, 
                          C_ari29, C_ari35, C_ari40, C_ari43, C_ari49, C_ari62, C_ari65)

data_attachment$dyad <- data$dyad
data_attachment$dyad <- factor(data_attachment$dyad, labels = c("mother_daughter", "mother_son", "father_daughter", "father_son"))
# Save the dataframe to a CSV file
write.csv(data_attachment, file = "data_attachment.csv", row.names = FALSE)

#After running this code, we have obtained two csv files, one with the attachment data and another one with the problem behavior data
