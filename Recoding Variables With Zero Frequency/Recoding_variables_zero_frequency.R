#Recoding variables with 0 frequency
#Author: Magda Matetovici
# We use the two csv files obtained from the script Data_to_Variables

#Packages
#install.packages("dplyr")
#install.packages("purr")
#install.packages("foreign")
#install.packages("tidyverse")
################################################################################
# ARI-CP
################################################################################

# ARI-CP categories:
# 1 - not at all applicable/ helemaal niet van toepassing
# 2 - hardly applicable/ nauwelijks van toepassing 
# 3 - sometimes applicable/ soms wel, soms niet van toepassing
# 4 - somewhat applicable/ redelijk van toepassing
# 5 - helemaal van toepassing/ fully applicable 

# Rule: if it is possible to merge upper or lower category, merge lower
library(dplyr)
library(purrr)
library(foreign)

# Find the categories with zero frequency
# Load data
data_attachment <- read.csv(file = "data_attachment.csv")
data_sdq <- read.csv(file = "data_sdq.csv")

# Convert all columns to factors for both datasets
data_attachment <- data_attachment %>% 
  mutate_all(factor)

data_sdq <- data_sdq %>% 
  mutate_all(factor)

# Convert all factors to ordered factors for both datasets
data_attachment <- data_attachment %>% 
  mutate_all(ordered)

data_sdq <- data_sdq %>% 
  mutate_all(ordered)

#-------------------------------------------------------------------------------
# Check the frequency of each response per item and group
#-------------------------------------------------------------------------------

# First, make sure all your item columns and group column are in a list
columns <- names(data_attachment[, 1:48])

# Use map function to apply table function to each of the item columns by group
result <- map(columns, ~data_attachment %>% 
                group_by(.data[[..1]], dyad) %>% 
                summarise(freq = n()))

#------------------------------------------------------------------------------
# This does not work as it should - redo the code

# Function to identify items with zero frequency in any category for each group
find_items_with_zero_frequency <- function(df) {
  # Filter the dataframe to find items with zero frequency
  zero_frequency_items <- filter(df, freq == 0)
  
  return(zero_frequency_items)
}

# Apply the function to each dataframe in the result list
zero_frequency_items_list <- map(result, find_items_with_zero_frequency)

# Output the list of dataframes containing items with zero frequency by group
print(zero_frequency_items_list)

# How many have this problem -> each item has this problem
length(zero_frequency_items_list)
#-------------------------------------------------------------------------------

#Recoding items one by one

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari6
# Check categories to be merged, by looking which dyad is missing for each of the category
# Categories to be merged: 1, 2, 3
result[[1]]

# recoding

data_attachment <- data_attachment %>%
  mutate(C_ari6_recoded = case_when(
    C_ari6 %in% 1:3 ~ 1,
    C_ari6 == 4 ~ 2,
    C_ari6 == 5 ~ 3
  ))
table(data_attachment$C_ari6_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari11 frequency 0 for father-son and mother_daughter 
# for response option 2; and also for mother_daugher for response option 1
# categories to be merged: 1, 2, 3
result[[2]]

#recoding
data_attachment <- data_attachment %>%
  mutate(C_ari11_recoded = case_when(
    C_ari11 %in% 1:3 ~ 1,
    C_ari11 == 4 ~ 2,
    C_ari11 == 5 ~ 3
  ))
table(data_attachment$C_ari11_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari17
# categories to be merged: 1 and 2

result[[3]]

# recoding
data_attachment <- data_attachment %>%
  mutate(C_ari17_recoded = case_when(
    C_ari17 %in% 1:2 ~ 1,
    C_ari17 == 3 ~ 2,
    C_ari17 == 4 ~ 3,
    C_ari17 == 5 ~ 4,
  ))
table(data_attachment$C_ari17_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari30
# categories to be merged: 1 and 2

result[[4]]

#recoding
data_attachment <- data_attachment %>%
  mutate(C_ari30_recoded = case_when(
    C_ari30 %in% 1:2 ~ 1,
    C_ari30 == 3 ~ 2,
    C_ari30 == 4 ~ 3,
    C_ari30 == 5 ~ 4,
  ))
table(data_attachment$C_ari30_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari37
# has an NA that needs to be dealt with
# categories to be merged: 1, 2, 3

result[[5]]

# recoding
data_attachment <- data_attachment %>%
  mutate(C_ari37_recoded = case_when(
    C_ari37 %in% 1:3 ~ 1,
    C_ari37 == 4 ~ 2,
    C_ari37 == 5 ~ 3
  ))
table(data_attachment$C_ari37_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari42
# categories to be merged: 1 and 2

result[[6]]

# recoding
data_attachment <- data_attachment %>%
  mutate(C_ari42_recoded = case_when(
    C_ari42 %in% 1:2 ~ 1,
    C_ari42 == 3 ~ 2,
    C_ari42 == 4 ~ 3,
    C_ari42 == 5 ~ 4,
  ))
table(data_attachment$C_ari42_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari46
# categories to be merged: 2 and 3

result[[7]]

# recoding

data_attachment <- data_attachment %>%
  mutate(C_ari46_recoded = case_when(
    C_ari46 %in% 2:3 ~ 1,
    C_ari46 == 4 ~ 2,
    C_ari46 == 5 ~ 3,
  ))
table(data_attachment$C_ari46_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item_Cari47
# categories to be merged: 2 and 3

result[[8]]

#recoding
data_attachment <- data_attachment %>%
  mutate(C_ari47_recoded = case_when(
    C_ari47 %in% 2:3 ~ 1,
    C_ari47 == 4 ~ 2,
    C_ari47 == 5 ~ 3,
  ))
table(data_attachment$C_ari47_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item_Cari50
# categories to be merged 1 and 2

result[[9]]

#recoding

data_attachment <- data_attachment %>%
  mutate(C_ari50_recoded = case_when(
    C_ari50 %in% 1:2 ~ 1,
    C_ari50 == 3 ~ 2,
    C_ari50 == 4 ~ 3,
    C_ari50 == 5 ~ 4,
  ))
table(data_attachment$C_ari50_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item_Cari54
# categories to be merged 1 and 2

result[[10]]

#recoding
data_attachment <- data_attachment %>%
  mutate(C_ari54_recoded = case_when(
    C_ari54 %in% 1:2 ~ 1,
    C_ari54 == 3 ~ 2,
    C_ari54 == 4 ~ 3,
    C_ari54 == 5 ~ 4,
  ))
table(data_attachment$C_ari54_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari59
# categories to be merged 1 and 2
result[[11]]

# recoding
data_attachment <- data_attachment %>%
  mutate(C_ari59_recoded = case_when(
    C_ari59 %in% 1:2 ~ 1,
    C_ari59 == 3 ~ 2,
    C_ari59 == 4 ~ 3,
    C_ari59 == 5 ~ 4,
  ))
table(data_attachment$C_ari59_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari61

# categories to be merged 1, 2 and 3

result[[12]]

# recoding

data_attachment <- data_attachment %>%
  mutate(C_ari61_recoded = case_when(
    C_ari61 %in% 1:3 ~ 1,
    C_ari61 == 4 ~ 2,
    C_ari61 == 5 ~ 3
  ))
table(data_attachment$C_ari61_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari64

# categories to be merged 1, 2 3

result[[13]]

# recoding
data_attachment <- data_attachment %>%
  mutate(C_ari64_recoded = case_when(
    C_ari64 %in% 1:3 ~ 1,
    C_ari64 == 4 ~ 2,
    C_ari64 == 5 ~ 3
  ))
table(data_attachment$C_ari64_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Item C_ari3
# categories to be merge 4 and 5
result[[14]]

#recoding

data_attachment <- data_attachment %>%
  mutate(C_ari3_recoded = case_when(
    C_ari3 %in% 1 ~ 1,
    C_ari3 == 2 ~ 2,
    C_ari3 == 3 ~ 3,
    C_ari3 %in% 4:5 ~ 4,
    
  ))
table(data_attachment$C_ari3_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari14
# categories to be merged - none
result[[15]]

data_attachment$C_ari14_recoded <- data_attachment$C_ari14
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari23
# categories to be merged 3 and 4
result[[16]]

# recoding 
data_attachment <- data_attachment %>%
  mutate(C_ari23_recoded = case_when(
    C_ari23 %in% 1 ~ 1,
    C_ari23 == 2 ~ 2,
    C_ari23 %in% 3:4 ~ 3,
    C_ari23 == 5 ~ 4,
    
  ))
table(data_attachment$C_ari23_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari25
# categories to be merged 3, 4, 5
result[[17]]

# recoding 
data_attachment <- data_attachment %>%
  mutate(C_ari25_recoded = case_when(
    C_ari25 %in% 1 ~ 1,
    C_ari25 == 2 ~ 2,
    C_ari25 %in%  3:5 ~ 3
  ))
table(data_attachment$C_ari25_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item_Cari26
# categories to be merged 3, 4, 5
result[[18]]
# recoding 
data_attachment <- data_attachment %>%
  mutate(C_ari26_recoded = case_when(
    C_ari26 %in% 1 ~ 1,
    C_ari26 == 2 ~ 2,
    C_ari26 %in%  3:5 ~ 3
  ))
table(data_attachment$C_ari26_recoded)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item_Cari31
# categories to be merged 3, 4, 5
result[[19]]
# recoding 
data_attachment <- data_attachment %>%
  mutate(C_ari31_recoded = case_when(
    C_ari31 %in% 1 ~ 1,
    C_ari31 == 2 ~ 2,
    C_ari31 %in%  3:5 ~ 3
  ))
table(data_attachment$C_ari31_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari33
# categories to be merged 4 and 5
result[[20]]

# recoding 
data_attachment <- data_attachment %>%
  mutate(C_ari33_recoded = case_when(
    C_ari33 %in% 1 ~ 1,
    C_ari33 == 2 ~ 2,
    C_ari33 ==  3 ~ 3,
    C_ari33 %in%  4:5 ~ 4,
  ))
table(data_attachment$C_ari33_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari34
# categories to be merged 4 and 5
result[[21]]

# recoding 
data_attachment <- data_attachment %>%
  mutate(C_ari34_recoded = case_when(
    C_ari34 %in% 1 ~ 1,
    C_ari34 == 2 ~ 2,
    C_ari34 ==  3 ~ 3,
    C_ari34 %in%  4:5 ~ 4,
  ))
table(data_attachment$C_ari34_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari36
# categories to be merged 4 and 5
result[[22]]

# recoding 
data_attachment <- data_attachment %>%
  mutate(C_ari36_recoded = case_when(
    C_ari36 %in% 1 ~ 1,
    C_ari36 == 2 ~ 2,
    C_ari36 ==  3 ~ 3,
    C_ari36 %in%  4:5 ~ 4,
  ))
table(data_attachment$C_ari36_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari57
# categories to be recoded: none

result[[23]]

data_attachment$C_ari57_recoded <- data_attachment$C_ari57
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari66
# categories to be recoded: none

result[[24]]

data_attachment$C_ari66_recoded <- data_attachment$C_ari66
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari12
# categories to be recoded 4 and 5 

result[[25]]

# recoding 
data_attachment <- data_attachment %>%
  mutate(C_ari12_recoded = case_when(
    C_ari12 %in% 1 ~ 1,
    C_ari12 == 2 ~ 2,
    C_ari12 ==  3 ~ 3,
    C_ari12 %in%  4:5 ~ 4,
  ))
table(data_attachment$C_ari12_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari15 
# category 5 is missing, declare 4 levels 

result[[26]]

# recoding 
data_attachment <- data_attachment %>%
  mutate(C_ari15_recoded = case_when(
    C_ari15 %in% 1 ~ 1,
    C_ari15 == 2 ~ 2,
    C_ari15 ==  3 ~ 3,
    C_ari15 %in%  4:5 ~ 4,
  ))
table(data_attachment$C_ari15_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari16 
# categories to be recoded 4 and 5

result[[27]]

# recoding 
data_attachment <- data_attachment %>%
  mutate(C_ari16_recoded = case_when(
    C_ari16 %in% 1 ~ 1,
    C_ari16 == 2 ~ 2,
    C_ari16 ==  3 ~ 3,
    C_ari16 %in%  4:5 ~ 4,
  ))
table(data_attachment$C_ari16_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari19
# categories to be recoded 4 and 5

result[[28]]
# recoding 
data_attachment <- data_attachment %>%
  mutate(C_ari19_recoded = case_when(
    C_ari19 %in% 1 ~ 1,
    C_ari19 == 2 ~ 2,
    C_ari19 ==  3 ~ 3,
    C_ari19 %in%  4:5 ~ 4,
  ))
table(data_attachment$C_ari19_recoded)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari38
# categories to be recoded 4 and 5

result[[29]]
# recoding 
data_attachment <- data_attachment %>%
  mutate(C_ari38_recoded = case_when(
    C_ari38 %in% 1 ~ 1,
    C_ari38 == 2 ~ 2,
    C_ari38 ==  3 ~ 3,
    C_ari38 %in%  4:5 ~ 4,
  ))
table(data_attachment$C_ari38_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari39
# categories to be recoded 4 and 5

result[[30]]
# recoding 
data_attachment <- data_attachment %>%
  mutate(C_ari39_recoded = case_when(
    C_ari39 %in% 1 ~ 1,
    C_ari39 == 2 ~ 2,
    C_ari39 ==  3 ~ 3,
    C_ari39 %in%  4:5 ~ 4,
  ))
table(data_attachment$C_ari39_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari51
# categories to be recoded 4 and 5

result[[31]]
# recoding 
data_attachment <- data_attachment %>%
  mutate(C_ari51_recoded = case_when(
    C_ari51 %in% 1 ~ 1,
    C_ari51 == 2 ~ 2,
    C_ari51 ==  3 ~ 3,
    C_ari51 %in%  4:5 ~ 4,
  ))
table(data_attachment$C_ari51_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari53
# categories to be recoded 4 and 5

result[[32]]

# recoding 
data_attachment <- data_attachment %>%
  mutate(C_ari53_recoded = case_when(
    C_ari53 %in% 1 ~ 1,
    C_ari53 == 2 ~ 2,
    C_ari53 ==  3 ~ 3,
    C_ari53 %in%  4:5 ~ 4,
  ))
table(data_attachment$C_ari53_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari56
# categories to be recoded 3 and 4, no category 5

result[[33]]

#recoding
data_attachment <- data_attachment %>%
  mutate(C_ari56_recoded = case_when(
    C_ari56 %in% 1 ~ 1,
    C_ari56 == 2 ~ 2,
    C_ari56 ==  3 ~ 3,
    C_ari56 %in%  4:5 ~ 4,
  ))
table(data_attachment$C_ari56_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari58
# categories to be recoded 4 and 5 

result[[34]]
#recoding
data_attachment <- data_attachment %>%
  mutate(C_ari58_recoded = case_when(
    C_ari58 %in% 1 ~ 1,
    C_ari58 == 2 ~ 2,
    C_ari58 ==  3 ~ 3,
    C_ari58 %in%  4:5 ~ 4,
  ))
table(data_attachment$C_ari58_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari63
# there is one NA
# categories to be recoded 4 and 5
result[[35]]

#recoding
data_attachment <- data_attachment %>%
  mutate(C_ari63_recoded = case_when(
    C_ari63 %in% 1 ~ 1,
    C_ari63 == 2 ~ 2,
    C_ari63 ==  3 ~ 3,
    C_ari63 %in%  4:5 ~ 4,
  ))
table(data_attachment$C_ari63_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari4
# no categories to be recorded
result[[36]]

data_attachment$C_ari4_recoded <- data_attachment$C_ari4

# Item C_ari8
# there is one NA
# categories to be recorded 3, 4 and 5 
result[[37]]

#recoding
data_attachment <- data_attachment %>%
  mutate(C_ari8_recoded = case_when(
    C_ari8 %in% 1 ~ 1,
    C_ari8 == 2 ~ 2,
    C_ari8 %in%  3:5 ~ 3,
  ))
table(data_attachment$C_ari8_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari13
# categories to be recorded 4 and 3
result[[38]]

#recoding
data_attachment <- data_attachment %>%
  mutate(C_ari13_recoded = case_when(
    C_ari13 %in% 1 ~ 1,
    C_ari13 == 2 ~ 2,
    C_ari13 %in%   3:4 ~ 3,
    C_ari13 %in%  5 ~ 4,
  ))
table(data_attachment$C_ari13_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari18
# categories to be recorded 4 and 3
result[[39]]

#recoding
data_attachment <- data_attachment %>%
  mutate(C_ari18_recoded = case_when(
    C_ari18 %in% 1 ~ 1,
    C_ari18 == 2 ~ 2,
    C_ari18 %in%   3 ~ 3,
    C_ari18 %in%  4:5 ~ 4,
  ))
table(data_attachment$C_ari18_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari22
# one NA
# categories to be recorded 4 and 5
result[[40]]

#recoding
data_attachment <- data_attachment %>%
  mutate(C_ari22_recoded = case_when(
    C_ari22 %in% 1 ~ 1,
    C_ari22 == 2 ~ 2,
    C_ari22 ==  3 ~ 3,
    C_ari22 %in%  4:5 ~ 4,
  ))
table(data_attachment$C_ari22_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari28
# categories to be recorded 3, 4 and 5
result[[41]]

#recoding
data_attachment <- data_attachment %>%
  mutate(C_ari28_recoded = case_when(
    C_ari28 %in% 1 ~ 1,
    C_ari28 == 2 ~ 2,
    C_ari28 %in%  3:5 ~ 3,
  ))
table(data_attachment$C_ari28_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari29
# no categories to be recoded
result[[42]]

data_attachment$C_ari29_recoded <- data_attachment$C_ari29
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari35
# categories to be recoded 4 and 5
result[[43]]

#recoding
data_attachment <- data_attachment %>%
  mutate(C_ari35_recoded = case_when(
    C_ari35 %in% 1 ~ 1,
    C_ari35 == 2 ~ 2,
    C_ari35 ==  3 ~ 3,
    C_ari35 %in%  4:5 ~ 4,
  ))
table(data_attachment$C_ari35_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari40
# categories to be recoded 3 and 4
result[[44]]

#recoding
data_attachment <- data_attachment %>%
  mutate(C_ari40_recoded = case_when(
    C_ari40 %in% 1 ~ 1,
    C_ari40 == 2 ~ 2,
    C_ari40  %in%  3:4 ~ 3,
    C_ari40 ==  5 ~ 4,
  ))
table(data_attachment$C_ari40_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari43
# categories to be recoded 4 and 5
result[[45]]

#recoding
data_attachment <- data_attachment %>%
  mutate(C_ari43_recoded = case_when(
    C_ari43 %in% 1 ~ 1,
    C_ari43 == 2 ~ 2,
    C_ari43 ==  3 ~ 3,
    C_ari43 %in%  4:5 ~ 4,
  ))
table(data_attachment$C_ari43_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari49
# categories to be recoded 4 and 5
result[[46]]
#recoding
data_attachment <- data_attachment %>%
  mutate(C_ari49_recoded = case_when(
    C_ari49 %in% 1 ~ 1,
    C_ari49 == 2 ~ 2,
    C_ari49 ==  3 ~ 3,
    C_ari49 %in%  4:5 ~ 4,
  ))
table(data_attachment$C_ari49_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari62
# categories to be recoded 4 and 5
result[[47]]

#recoding
data_attachment <- data_attachment %>%
  mutate(C_ari62_recoded = case_when(
    C_ari62 %in% 1 ~ 1,
    C_ari62 == 2 ~ 2,
    C_ari62 ==  3 ~ 3,
    C_ari62 %in%  4:5 ~ 4,
  ))
table(data_attachment$C_ari62_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_ari65
# no categories to be recoded
result[[48]]

data_attachment$C_ari65_recoded <- data_attachment$C_ari65

# Save the dataset containing original and recoded variables

write.csv(data_attachment, "data_attachment_original_recoded.csv", row.names = FALSE)

#-------------------------------------------------------------------------------
# SDQ
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Check the frequency of each response per item and group
#-------------------------------------------------------------------------------

# First, make sure all your item columns and group column are in a list
columns <- names(data_sdq[, 2:21])

# Use map function to apply table function to each of the item columns by group
result2 <- map(columns, ~data_sdq[, 2:22] %>% 
                group_by(.data[[..1]], dyad) %>% 
                summarise(freq = n()))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_SDQ8.2_5
# categories to be merged none
result2[[1]]

#recoding
data_sdq$C_SDQ8.2_5_recoded <- data_sdq$C_SDQ8.2_5
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_SDQ8.2_7
# categories to be merged none
result2[[2]]

#recoding
data_sdq$C_SDQ8.2_7_recoded <- data_sdq$C_SDQ8.2_7
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_SDQ8.2_12
# categories to be merged 2 and 3
result2[[3]]
#recoding

data_sdq <- data_sdq %>%
  mutate(C_SDQ8.2_12_recoded = case_when(
    C_SDQ8.2_12 %in% 0 ~ 0,
    C_SDQ8.2_12 %in% 1:2 ~ 1
  ))
table(data_sdq$C_SDQ8.2_12_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_SDQ8.2_18
# categories to be merged 2 and 3
result2[[4]]

#recoding

data_sdq <- data_sdq %>%
  mutate(C_SDQ8.2_18_recoded = case_when(
    C_SDQ8.2_18 %in% 0 ~ 0,
    C_SDQ8.2_18 %in% 1:2 ~ 1
  ))
table(data_sdq$C_SDQ8.2_18_recoded)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_SDQ8.2_22
# categories to be merged none
result2[[5]]
#recoding
data_sdq$C_SDQ8.2_22_recoded <- data_sdq$C_SDQ8.2_22
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_SDQ8.2_2
# categories to be merged none
result2[[6]]
#recoding
data_sdq$C_SDQ8.2_2_recoded <- data_sdq$C_SDQ8.2_2
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_SDQ8.2_10
# one NA
# categories to be merged none
result2[[7]]
#recoding
data_sdq$C_SDQ8.2_10_recoded <- data_sdq$C_SDQ8.2_10
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_SDQ8.2_15
# one NA
# categories to be merged none
result2[[8]]
#recoding
data_sdq$C_SDQ8.2_15_recoded <- data_sdq$C_SDQ8.2_15
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_SDQ8.2_21
# one NA
# categories to be merged none
result2[[9]]
#recoding
data_sdq$C_SDQ8.2_21_recoded <- data_sdq$C_SDQ8.2_21
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_SDQ8.2_25
# categories to be merged none
result2[[10]]
#recoding
data_sdq$C_SDQ8.2_25_recoded <- data_sdq$C_SDQ8.2_25
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_SDQ8.2_3
# categories to be merged none
result2[[11]]
#recoding
data_sdq$C_SDQ8.2_3_recoded <- data_sdq$C_SDQ8.2_3
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_SDQ8.2_8
# categories to be merged none
result2[[12]]
#recoding
data_sdq$C_SDQ8.2_8_recoded <- data_sdq$C_SDQ8.2_8
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_SDQ8.2_13
# categories to be merged none
result2[[13]]
#recoding
data_sdq$C_SDQ8.2_13_recoded <- data_sdq$C_SDQ8.2_13
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_SDQ8.2_16
# categories to be merged none
result2[[14]]
#recoding
data_sdq$C_SDQ8.2_16_recoded <- data_sdq$C_SDQ8.2_16
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_SDQ8.2_24
# categories to be merged none
result2[[15]]
#recoding
data_sdq$C_SDQ8.2_24_recoded <- data_sdq$C_SDQ8.2_24
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_SDQ8.2_6
# categories to be merged none
result2[[16]]
#recoding
data_sdq$C_SDQ8.2_6_recoded <- data_sdq$C_SDQ8.2_6
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_SDQ8.2_11
# categories to be merged none
result2[[17]]
#recoding
data_sdq$C_SDQ8.2_11_recoded <- data_sdq$C_SDQ8.2_11
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_SDQ8.2_14
# categories to be merged 2 and 3
result2[[18]]

#recoding

data_sdq <- data_sdq %>%
  mutate(C_SDQ8.2_14_recoded = case_when(
    C_SDQ8.2_14 %in% 0 ~ 0,
    C_SDQ8.2_14 %in% 1:2 ~ 1
  ))
table(data_sdq$C_SDQ8.2_14_recoded)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_SDQ8.2_19
# categories to be merged 1 and 2
result2[[19]]

#recoding

data_sdq <- data_sdq %>%
  mutate(C_SDQ8.2_19_recoded = case_when(
    C_SDQ8.2_19 %in% 0 ~ 0,
    C_SDQ8.2_19 %in% 1:2 ~ 1
  ))
table(data_sdq$C_SDQ8.2_19_recoded)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Item C_SDQ8.2_23
# categories to be merged none
result2[[20]]
#recoding
data_sdq$C_SDQ8.2_23_recoded <- data_sdq$C_SDQ8.2_23
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Save dataset with original and recoded variables for sdq
write.csv(data_sdq, "data_sdq_original_recoded.csv", row.names = FALSE)

################################################################################
# Load data attachment and check if the variables are stored correctly
data_attachment_recoded <- read.csv(file = "data_attachment_original_recoded.csv")
library(dplyr)
# Make sure they are all ordered variables
# Convert all columns to factors 

data_attachment_recoded <- data_attachment_recoded %>% 
  mutate_all(factor)

# Convert all factors to ordered factors

data_attachment_recoded <- data_attachment_recoded %>% 
  mutate_all(ordered)


# Count the number of levels for each variable in your data
num_levels <- sapply(data_attachment_recoded[50:97], function(x) if (is.ordered(x)) length(levels(x)) else NA)

# Check how many variables have 3, 4, and 5 levels
count_3 <- sum(num_levels == 3, na.rm = TRUE)
count_4 <- sum(num_levels == 4, na.rm = TRUE)
count_5 <- sum(num_levels == 5, na.rm = TRUE)

list(three_levels = count_3, four_levels = count_4, five_levels = count_5)
# For the attachment questionnaire, there are:
# 12 items with three response levels
# 30 items with fours response levels
# 6 intems with 5 response levels



# Load data problems and check

data_problems_recoded <- read.csv(file = "data_sdq_original_recoded.csv")


# Make sure they are all ordered variables
# Convert all columns to factors for both datasets

data_problems_recoded <- data_problems_recoded %>% 
  mutate_all(factor)

# Convert all factors to ordered factors for both datasets


data_problems_recoded <- data_problems_recoded %>% 
  mutate_all(ordered)


# Count the number of levels for each variable in your data
num_levels <- sapply(data_problems_recoded[23:42], function(x) if (is.ordered(x)) length(levels(x)) else NA)

# Check how many variables have 3, 4, and 5 levels
count_2 <- sum(num_levels == 2, na.rm = TRUE)
count_3 <- sum(num_levels == 3, na.rm = TRUE)

list(two_levels = count_2, three_levels = count_3)