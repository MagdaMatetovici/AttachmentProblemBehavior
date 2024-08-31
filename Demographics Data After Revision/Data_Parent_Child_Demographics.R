#Demographics Data
#Author: Magda Matetovici
#install.packages("foreign")
library(foreign)

#Load the data

data <- read.spss("C:/Users/matet/Desktop/Parent and Child Dyad/data_pcd.sav", to.data.frame=TRUE)

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
data$dyad_name <- factor(data$dyad, labels = c("mother_daughter", "mother_son", "father_daughter", "father_son"))

# Obtain the size of each group

table(data$A_parentgender)
table(data$dyad_name)

# Obtain the mean and standard deviation of parents and children
mean(as.numeric(data$A_parentage), na.rm = TRUE)
sd(as.numeric(data$A_parentage), na.rm = TRUE)

mean(as.numeric(data$A_childage), na.rm = TRUE)
sd(as.numeric(data$A_childage), na.rm = TRUE)


# Check if there are differences among the four dyads
# A_parenteducation

unique(data$A_parenteducation)

# contingency table with dyads and education
education_table_original <- table(data$dyad_name, data$A_parenteducation)

# collapse primary to highschool categories
highschool <- rowSums(education_table_original[, 1:6])
education_table <- cbind(highschool, education_table_original[, 7], education_table_original[, 8], education_table_original[, 9])
colnames(education_table) <- c("highschool", "mbo", "hbo", "wo")

# add two phds to wo category and AD to hbo category
education_table[4, 3] <- education_table[4, 3] + 1
education_table[3, 4] <- education_table[3, 4] + 1
education_table[4, 4] <- education_table[4, 4] + 1

chi_square_test <- chisq.test(education_table)
#groups do not differ in terms of education
# A_parentage
data$dyad_name
anova_parent_age <- aov(data$A_parentage ~ data$dyad_name, data = data)
summary(anova_parent_age)
# mother-son is younger than father-son and father-daughter


tukey_result <- TukeyHSD(anova_parent_age)

library(dplyr)
summary_stats <- data %>%
  group_by(dyad_name) %>%
  summarise(
    Mean = mean(A_parentage, na.rm = TRUE),
    SD = sd(A_parentage, na.rm = TRUE)
  )

# A_child_age
anova_child_age <- aov(data$A_childage ~ data$dyad_name, data = data)
summary(anova_child_age)

#no difference in child age
tukey_result <- TukeyHSD(anova_child_age)


# A_familiychildren
#no difference between the dyads in the amount of children in the family
unique(data$A_familychildren)

siblings_table_original <- table(data$dyad_name, data$A_familychildren)

more_than_4 <- rowSums(siblings_table_original[, 4:7])
siblings_table <- cbind(siblings_table_original[,1],siblings_table_original[,2], siblings_table_original[,3], more_than_4)

colnames(siblings_table) <- c("one", "two", "three", "4 or more")
chi_square_test <- chisq.test(siblings_table)


read.csv("data_attachment_original_recoded.csv")
data_attachment_recoded<- cbind(data_attachment_recoded, data$A_parentage)
write.csv(data_attachment_recoded, file = "data_attachment_recoded_parent_age.csv")
