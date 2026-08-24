source("scripts/00_settings.R")

        #### VG2 ####

obs_grades <- read.csv("data.temp/gradedf2_wide.csv")
obs_grades <- obs_grades[,-1]
#removing all rows with only NAs in grade columns
n_subjectgrades <- sum(!grepl("_c$", names(obs_grades)))

ex_two <- readRDS("results/expected_grades/expectedobs2_2d.rds")
ex_two <- ex_two[,1:n_subjectgrades]
ex_joint <- readRDS("results/expected_grades/expectedobs2_joint.rds")
ex_joint <- ex_joint[,1:n_subjectgrades]


#function that computes the average grade on subject i for dns = 0 and dns = 1

s1_range <- 1:ncol(obs_grades)
s0_range <- (ncol(obs_grades)+1):(ncol(obs_grades)*2)

model_fit <- function(grades){
  
  fit_frame <- as.data.frame(matrix(NA, nrow = ncol(grades), ncol = ncol(grades)*2))
  names(fit_frame)[s1_range] <- paste0(names(grades),".1") # s_1
  names(fit_frame)[s0_range] <- paste0(names(grades),".0") # s_0
  row.names(fit_frame) <- names(grades)
  
  for(s in s1_range) {
    s_1 <- which(!is.na(grades[,s])) # which students recieved a grade in subject s?
    s_0 <- which(is.na(grades[,s]))  # which students did not recieve a grade in subject s?
    
    for(i in s1_range){
      # mean grade in subject i, for students who recieved a grade in subject s
      fit_frame[s, i] <- mean(grades[s_1, i], na.rm = TRUE)
      
      # mean grade in subject i, for students who did not recieve a grade in subject s
      fit_frame[s, s0_range[i]] <- mean(grades[s_0, i], na.rm = TRUE)
    }
  }
  
  fit_frame[is.na(fit_frame)] <- NA # turn any NaN (from empty groups) into NA too
  
  return(fit_frame)
}


# Observed subject means and Expected subject means for Models 2 and 4
means_obs <- model_fit(obs_grades)
means_two <- model_fit(ex_two) 
means_joint <- model_fit(ex_joint)

s1_two <- rowSums((means_two[,s1_range] - means_obs[,s1_range])^2, na.rm = T)
s0_two <- rowSums((means_two[,s0_range] - means_obs[,s0_range])^2, na.rm = T)
fit_two <- s1_two + s0_two

s1_joint <- rowSums((means_joint[,s1_range] - means_obs[,s1_range])^2, na.rm = T)
s0_joint <- rowSums((means_joint[,s0_range] - means_obs[,s0_range])^2, na.rm = T)
fit_joint <- s1_joint + s0_joint

model_fit_table <- rbind(fit_joint, fit_two)
model_fit_table <- round(model_fit_table, 3)
model_fit_table2 <- t(model_fit_table)

sum(model_fit_table2[,1] < model_fit_table2[,2], na.rm = TRUE)

write.csv(model_fit_table2, file = "results/modelfit2.csv", row.names = F)

        #### VG3 ####

obs_grades <- read.csv("data.temp/gradedf3_wide.csv")
obs_grades <- obs_grades[,-1]

n_subjectgrades <- sum(!grepl("_c$", names(obs_grades)))

ex_two <- readRDS("results/expected_grades/expectedobs3_2d.rds")
ex_two <- ex_two[,1:n_subjectgrades]
ex_joint <- readRDS("results/expected_grades/expectedobs3_joint.rds")
ex_joint <- ex_joint[,1:n_subjectgrades]

s1_range <- 1:ncol(obs_grades)
s0_range <- (ncol(obs_grades)+1):(ncol(obs_grades)*2)

##function that computes the average grade on subject i for dns = 0 and dns = 1
# Observed subject means and Expected subject means for Models 2 and 4
means_obs <- model_fit(obs_grades)
means_two <- model_fit(ex_two) 
means_joint <- model_fit(ex_joint)

s1_two <- rowSums((means_two[,s1_range] - means_obs[,s1_range])^2, na.rm = T)
s0_two <- rowSums((means_two[,s0_range] - means_obs[,s0_range])^2, na.rm = T)
fit_two <- s1_two + s0_two

s1_joint <- rowSums((means_joint[,s1_range] - means_obs[,s1_range])^2, na.rm = T)
s0_joint <- rowSums((means_joint[,s0_range] - means_obs[,s0_range])^2, na.rm = T)
fit_joint <- s1_joint + s0_joint

model_fit_table <- rbind(fit_joint, fit_two)
model_fit_table <- round(model_fit_table, 3)
model_fit_table3 <- t(model_fit_table)

sum(model_fit_table3[,1] < model_fit_table3[,2], na.rm = TRUE)
nrow(model_fit_table3)

write.csv(model_fit_table3, file = "results/modelfit3.csv", row.names = F)


