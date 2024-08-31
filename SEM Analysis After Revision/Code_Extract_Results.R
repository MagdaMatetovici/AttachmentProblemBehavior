#Code to extract results from the sam2_summary_externalizing_problems_age and sam2_summary_externalizing_problems_age output files
#Author: Magda Matetovici

# Function to calculate the common standard dev
combine <- function(SE_1, SE_2) {
  # Calculate squared standard errors
  SE_1_sq <- SE_1^2
  SE_2_sq <- SE_2^2
  
  # Sum of squared standard errors
  SE_combined_sq <- SE_1_sq + SE_2_sq
  
  # Compute combined standard error
  SE_combined <- sqrt(SE_combined_sq)
  
  return(SE_combined)
}
#examples
combine (0.092, 0.104)
mean(0.512, 0.226)
mean(0.704, 0.745)
combine(0.092, 0.063)
combine(0.063, 0.104)
mean(0.512, 0.745)
mean(0.704, 0.226)

pick_dyads <- function (type, corelations, sds){
  if (type == "parent"){
    mothers_sd <- combine(sds[1],sds[2])
    mothers_cor <- mean(c(corelations[1], corelations[2]))
    fathers_sd <- combine(sds[3],sds[4])
    fathers_cor <- mean(c(corelations[3], corelations[4]))
    
    result <- list(
      mothers_cor = mothers_cor,
      fathers_cor = fathers_cor,
      mothers_sd = mothers_sd,
      fathers_sd = fathers_sd)
    return(result)
  } else if (type == "same"){
    same_sd <- combine(sds[2],sds[4])
    same_cor <- mean(c(corelations[2], corelations[4]))
    diff_sd <- combine(sds[1],sds[3])
    diff_cor <- mean (c(corelations[1], corelations[3]))
    
    result <- list(
      same_cor = same_cor,
      diff_cor = diff_cor,
      same_sd = same_sd,
      diff_sd = diff_sd)
    return(result)
  }else if (type == "child"){
    daughter_sd <- combine(sds[2],sds[3])
    daughter_cor <- mean(c(corelations[2], corelations[3]))
    son_sd <- combine(sds[1],sds[4])
    son_cor <- mean(c(corelations[1], corelations[4]))
    
    result <- list(
      daughter_cor = daughter_cor,
      son_cor = son_cor,
      daughter_sd = daughter_sd,
      son_sd = son_sd)
    return(result)
  }
  
}

#Internalizing Problems

secure_correlations <- c(-0.669, -0.490, -0.349, -0.763)
secure_sd <- c(0.070, 0.095, 0.099, 0.070)
pick_dyads(type = "parent", corelations = secure_correlations, sds = secure_sd)
pick_dyads(type = "child", corelations = secure_correlations, sds = secure_sd)
pick_dyads(type = "same", corelations = secure_correlations, sds = secure_sd)

avoidant_correlations <- c(0.399, 0.456, 0.239, 0.720)
avoidant_sd <- c(0.092, 0.085, 0.127, 0.088)

pick_dyads(type = "parent", corelations = avoidant_correlations, sds = avoidant_sd)
pick_dyads(type = "child", corelations = avoidant_correlations, sds = avoidant_sd)
pick_dyads(type = "same", corelations = avoidant_correlations, sds = avoidant_sd)

ambivalent_correlations <- c(0.665, 0.772, 0.591, 0.830)
ambivalent_sd <- c(0.061, 0.076, 0.100, 0.062)

pick_dyads(type = "parent", corelations = ambivalent_correlations, sds = ambivalent_sd)
pick_dyads(type = "child", corelations = ambivalent_correlations, sds = ambivalent_sd)
pick_dyads(type = "same", corelations = ambivalent_correlations, sds = ambivalent_sd)

disorganised_correlations <-c(0.590, 0.640, 0.498, 0.776)
disorganised_sd <- c(0.067, 0.086, 0.113, 0.048)

pick_dyads(type = "parent", corelations = disorganised_correlations, sds = disorganised_sd)
pick_dyads(type = "child", corelations = disorganised_correlations, sds = disorganised_sd)
pick_dyads(type = "same", corelations = disorganised_correlations, sds = disorganised_sd)

#------------------------------------------------------------------------------
#Externalizing Problems
secure_correlations <- c(-0.756, -0.450, -0.254, -0.605)
secure_sd <- c(0.045, 0.068, 0.118, 0.071)
pick_dyads(type = "parent", corelations = secure_correlations, sds = secure_sd)
pick_dyads(type = "child", corelations = secure_correlations, sds = secure_sd)
pick_dyads(type = "same", corelations = secure_correlations, sds = secure_sd)

avoidant_correlations <- c(0.697, 0.242, 0.357, 0.496)
avoidant_sd <- c(0.049, 0.079, 0.108, 0.097)

pick_dyads(type = "parent", corelations = avoidant_correlations, sds = avoidant_sd)
pick_dyads(type = "child", corelations = avoidant_correlations, sds = avoidant_sd)
pick_dyads(type = "same", corelations = avoidant_correlations, sds = avoidant_sd)

ambivalent_correlations <- c(0.754, 0.686, 0.626, 0.716)
ambivalent_sd <- c(0.041, 0.059, 0.078, 0.052)

pick_dyads(type = "parent", corelations = ambivalent_correlations, sds = ambivalent_sd)
pick_dyads(type = "child", corelations = ambivalent_correlations, sds = ambivalent_sd)
pick_dyads(type = "same", corelations = ambivalent_correlations, sds = ambivalent_sd)

disorganised_correlations <-c(0.860, 0.790, 0.871, 0.811)
disorganised_sd <- c(0.029, 0.045, 0.054, 0.048)

pick_dyads(type = "parent", corelations = disorganised_correlations, sds = disorganised_sd)
pick_dyads(type = "child", corelations = disorganised_correlations, sds = disorganised_sd)
pick_dyads(type = "same", corelations = disorganised_correlations, sds = disorganised_sd)