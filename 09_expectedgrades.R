source("scripts/00_settings.R")

##### Compute probabilities from mirtobject
mirtprobs <- function(mirtmod, nquad, ndim, Mu, Sigma){
  GHgrid <- gaussHermiteData(nquad)
  GHxXD <- as.matrix(expand.grid(rep(list(GHgrid$x), ndim)))
  GHwXD <- as.matrix(expand.grid(rep(list(GHgrid$w), ndim)))
  
  #Adjust grid
  GHxXDN <- t(t(chol(Sigma)) %*% t(GHxXD) + Mu)
  
  J <- length(extract.mirt(mirtmod, "itemtype"))
  itemprobs <- vector("list", J)
  #Compute probabilities for all categories for all latent variable values, from mirt object
  for(i in 1:J) itemprobs[[i]] <- probtrace(extract.item(mirtmod, i), GHxXDN)
  return(list(itemprobs = itemprobs, GHxXDN = GHxXDN, GHwXD = GHwXD, GHxXD = GHxXD))
}




        #### EXPECTED GRADES: MISSING ####

#mirtobj - mirt object
#obsdata - dataframe with grades
#nquad - number of quadrature points per dimension
#ndim - number of latent variables
#Mu - mean vector of latent variables
#Sigma - covariance matrix of latent variables
#Jelect - number of electives
#Jcat - vector with number of response categories for choice and grades
computeExpected <- function(mirtobj, obsdata, nquad, ndim, Mu, Sigma, Jelect, Jcat, ncores = detectCores() - 4){
  ##### compute conditional probabilities at quadrature points
  myprobs <- mirtprobs(mirtobj, nquad = nquad, ndim = ndim, Mu = Mu, Sigma = Sigma)
  
  ##### we have N number of respondents
  ##### Jall is the number of elective grades plus the number of total grades
  N <- nrow(obsdata)
  Jall <- ncol(obsdata)
  Jcat <- c(Jcat, rep(2, Jelect)) 
  
  ##### function that computes the observed/expected grade row for one respondent k
  computeRow <- function(k){
    newgrade_k <- rep(NA, Jall)
    
    ##### compute normalizing constant for the posterior distribution
    ##### conditional probabilities for selection (binary) and for grade (ordinal), given a combination of latent variables
    conditionalprob <- rep(1.0, nrow(myprobs$GHxXDN))
    for(i in 1:Jall){
      if(is.na(obsdata[k,i])) next
      conditionalprob <- conditionalprob * myprobs$itemprobs[[i]][,obsdata[k,i] + 1]
    }
    
    marginalprob <- numeric(1)
    ##### weight each quadrature point and accumulate
    ##### adjust weights to standard normal distribution instead of exp(-x^2)
    for(i in 1:nrow(myprobs$GHxXDN)) marginalprob <- conditionalprob[i] * prod(myprobs$GHwD[i,] / sqrt(2.0 * pi) * exp(myprobs$GHxD[i,]^2 / 2.0)) + marginalprob
    
    ##### loop through each course
    for(l in 1:Jall){
      if(!is.na(obsdata[k,l])){
        newgrade_k[l] <- obsdata[k, l]
        next
      }
      
      ##### compute expected value for missing grade
      conditionalexp <- matrix(0, nrow(myprobs$GHxXDN), ncol = Jcat[l])
      for(j in 1:Jcat[l]) conditionalexp[, j] <- as.numeric((j - 1)) * myprobs$itemprobs[[l]][,j] * conditionalprob / marginalprob
      
      myexpgrade <- numeric(1)
      for(j in 1:Jcat[l]) for(i in 1:nrow(myprobs$GHxXDN)) myexpgrade <- conditionalexp[i,j] * prod(myprobs$GHwD[i,] / sqrt(2.0 * pi) * exp(myprobs$GHxD[i,]^2 / 2.0)) + myexpgrade
      
      newgrade_k[l] <- myexpgrade
    }
    
    newgrade_k
  }
  
  ##### set up a cluster of worker processes (works on windows, mac, and linux, unlike mclapply's forking)
  cl <- makeCluster(ncores)
  
  ##### loop through each respondent, in parallel across ncores, with a progress bar
  results <- pblapply(1:N, computeRow, cl = cl)
  stopCluster(cl)
  
  ##### output is a matrix of same size as the data matrix, but with expected values instead of missing values
  newgrade <- do.call(rbind, results)
  return(newgrade)
}


          #### VG1: UNI-DIM MODEL ####

gradedf1_wide <- read.csv("data.temp/gradedf1_wide.csv")

#removing ID columns
gradedf1_wide$w19_0634_lnr <- gradedf1_wide$w19_0634_lnr <- NULL

#reading in model
mod1 <- read_rds("results/models/mod1.rds")

#validating that gradedf1_wide corresponds to data used in the model
validating <- extract.mirt(mod1, what = 'data')
validating<- as.data.frame(validating)
waldo::compare(validating, gradedf1_wide)

one_dim_mean_vecc <- c(0)
one_dim_theta_cov <- data.frame(1)

car_vec <- rep(6, ncol(gradedf1_wide))

#changing grades to 0-5, a requirement for the function
gradedf1_wide[] <- gradedf1_wide[]-1


#compute expected grades for missing cells for Model 4
expected1 <- computeExpected(mod1,
                                 obsdata = gradedf1_wide, 
                                 nquad = 15,
                                 ndim = 1,
                                 Mu = one_dim_mean_vecc,
                                 Sigma = one_dim_theta_cov,
                                 Jelect = 0,
                                 Jcat = car_vec)

#changing grades back to 1-6 and matching names with the grade df.
expected1 <- as.data.frame(expected1)
expected1[] <- expected1[]+1
names(expected1) <- names(gradedf1_wide)

#save output
saveRDS(expected1, file = "results/expected_grades/expected1.rds")


        #### VG2: JOINT MODEL ####

items2 <- read.csv("data.temp/items_all2.csv")

#reading in model
mod2_joint <- read_rds("results/models/mod_joint2.rds")

#validating that items2 corresponds to data used in the model
validating <- extract.mirt(mod2_joint, what = 'data')
validating<- as.data.frame(validating)
waldo::compare(validating, items2)


summary(mod2_joint)
mean_vec_3d <- c(0,0,0)

f_cov2_3d <- data.frame(c(1, .820, .408),
                        c(.820, 1, 0.355),
                        c(.408, 0.355 , 1))

n_subjectgrades <- sum(!grepl("_c$", names(items2)))
car_vec <- rep(6, n_subjectgrades)

#changing grades to 0-5, a requirement for the function
items2[1:n_subjectgrades][] <- items2[1:n_subjectgrades][]-1


expected2_joint <- computeExpected(mod2_joint,
                                obsdata = items2, 
                                nquad = 15,
                                ndim = 3,
                                Mu = mean_vec_3d,
                                Sigma = f_cov2_3d,
                                Jelect = 8,
                                Jcat = car_vec)

#changing grades back to 1-6 and matching names with the grade df.
expected2_joint <- as.data.frame(expected2_joint)
expected2_joint[1:n_subjectgrades][] <- expected2_joint[1:n_subjectgrades][]+1
names(expected2_joint) <- names(items2)


saveRDS(expected2_joint, file = "results/expected_grades/expected2_joint.rds")


        #### VG3: JOINT MODEL ####

items3 <- read.csv("data.temp/items_all3.csv")

#reading in model
mod3_joint <- read_rds("results/mod_joint3")

#validating that items2 corresponds to data used in the model
validating <- extract.mirt(mod3_joint, what = 'data')
validating<- as.data.frame(validating)
waldo::compare(validating, items3)

n_subjectgrades <- sum(!grepl("_c$", names(items3)))

summary(mod3_joint)
mean_vec_3d <- c(0,0,0)

f_cov3_3d <- data.frame(c(1, .806, -.619),
                        c(.806, 1, -.364 ),
                        c(-.619, -.364 , 1))

car_vec <- rep(6, n_subjectgrades)

#changing grades to 0-5, a requirement for the function
items3[1:n_subjectgrades][] <- items3[1:n_subjectgrades][]-1


#compute expected grades for missing cells for Model 4
expected3_joint <- computeExpected(mod3_joint,
                                   obsdata = items3, 
                                   nquad = 15,
                                   ndim = 3,
                                   Mu = mean_vec_3d,
                                   Sigma = f_cov3_3d,
                                   Jelect = 11,
                                   Jcat = car_vec)

#changing grades back to 1-6 and matching names with the grade df.
expected3_joint <- as.data.frame(expected3_joint)
expected3_joint[1:n_subjectgrades][] <- expected3_joint[1:n_subjectgrades][]+1
names(expected3_joint) <- names(items3)

saveRDS(expected3_joint, file = "results/expected_grades/expected3_joint.rds")




        #### EXPECTED GRADES: OBSERVED ####

computeExpectedObs <- function(mirtobj, obsdata, nquad, ndim, Mu, Sigma, Jelect, Jcat, ncores = detectCores() - 4){
  ##### compute conditional probabilities at quadrature points
  myprobs <- mirtprobs(mirtobj, nquad = nquad, ndim = ndim, Mu = Mu, Sigma = Sigma)
  
  ##### we have N number of respondents
  ##### Jall is the number of elective grades plus the number of total grades
  N <- nrow(obsdata)
  Jall <- ncol(obsdata)
  Jcat <- c(Jcat, rep(2, Jelect)) # changed this to let electives be last because thats how i set up the dfs 
  
  ##### function that computes the observed/expected grade row for one respondent k
  computeRow <- function(k){
    newgrade_k <- rep(NA, Jall)
    
    ##### compute normalizing constant for the posterior distribution
    ##### conditional probabilities for selection (binary) and for grade (ordinal), given a combination of latent variables
    conditionalprob <- rep(1.0, nrow(myprobs$GHxXDN))
    for(i in 1:Jall){
      if(is.na(obsdata[k,i])) next
      conditionalprob <- conditionalprob * myprobs$itemprobs[[i]][,obsdata[k,i] + 1]
    }
    
    marginalprob <- numeric(1)
    ##### weight each quadrature point and accumulate
    ##### adjust weights to standard normal distribution instead of exp(-x^2)
    for(i in 1:nrow(myprobs$GHxXDN)) marginalprob <- conditionalprob[i] * prod(myprobs$GHwD[i,] / sqrt(2.0 * pi) * exp(myprobs$GHxD[i,]^2 / 2.0)) + marginalprob
    
    ##### loop through each course
    for(l in 1:Jall){
      if(is.na(obsdata[k,l])){
        newgrade_k[l] <- obsdata[k, l]
        next
      }
      
      ##### compute expected value for missing grade
      conditionalexp <- matrix(0, nrow(myprobs$GHxXDN), ncol = Jcat[l])
      for(j in 1:Jcat[l]) conditionalexp[, j] <- as.numeric((j - 1)) * myprobs$itemprobs[[l]][,j] * conditionalprob / marginalprob
      
      myexpgrade <- numeric(1)
      for(j in 1:Jcat[l]) for(i in 1:nrow(myprobs$GHxXDN)) myexpgrade <- conditionalexp[i,j] * prod(myprobs$GHwD[i,] / sqrt(2.0 * pi) * exp(myprobs$GHxD[i,]^2 / 2.0)) + myexpgrade
      
      newgrade_k[l] <- myexpgrade
    }
    
    newgrade_k
  }
  
  ##### set up a cluster of worker processes (works on windows, mac, and linux, unlike mclapply's forking)
  cl <- makeCluster(ncores)
  
  ##### loop through each respondent, in parallel across ncores, with a progress bar
  results <- pblapply(1:N, computeRow, cl = cl)
  stopCluster(cl)
  
  ##### output is a matrix of same size as the data matrix, but with expected values instead of missing values
  newgrade <- do.call(rbind, results)
  return(newgrade)
}


          ##### VG2 #####

items2 <- read.csv("data.temp/items_all2.csv")

n_subjectgrades <- sum(!grepl("_c$", names(items2)))


            ######  mod 2d ######


subject_grades2 <- items2[,1:n_subjectgrades]

mod2_2d <- read_rds("results/models/mod2_2d.rds")
summary(mod2_2d)
mean_vec_2d <- c(0,0)
f_cov2_2d <- data.frame(c(1,.815),
                        c(.815,1))

car_vec <- rep(6, ncol(subject_grades2))

#changing grades to 0-5, a requirement for the function
subject_grades2[] <- subject_grades2[]-1


#compute expected grades for missing cells for Model 4
expectedobs2_2d <- computeExpectedObs(mod2_2d,
                                obsdata = subject_grades2, 
                                nquad = 15,
                                ndim = 2,
                                Mu = mean_vec_2d,
                                Sigma = f_cov2_2d,
                                Jelect = 0,
                                Jcat = car_vec)

#changing grades back to 1-6 and matching names with the grade df.
expectedobs2_2d <- as.data.frame(expectedobs2_2d)
expectedobs2_2d[] <- expectedobs2_2d[]+1
names(expectedobs2_2d) <- names(subject_grades2)

saveRDS(expectedobs2_2d, file = "results/expected_grades/expectedobs2_2d.rds")


            ###### mod joint ######

mod2_joint <- read_rds("results/models/mod_joint2.rds")
summary(mod2_joint)

mean_vec_3d <- c(0,0,0)

f_cov2_3d <- data.frame(c(1, .820, .408),
                        c(.820, 1, 0.355),
                        c(.408, 0.355 , 1))

car_vec <- rep(6, n_subjectgrades)

#changing grades to 0-5, a requirement for the function
items2[1:n_subjectgrades][] <- items2[1:n_subjectgrades][]-1


#compute expected grades for missing cells for Model 4
expectedobs2_joint <- computeExpectedObs(mod2_joint,
                                   obsdata = items2, 
                                   nquad = 15,
                                   ndim = 3,
                                   Mu = mean_vec_3d,
                                   Sigma = f_cov2_3d,
                                   Jelect = 8,
                                   Jcat = car_vec)

#changing grades back to 1-6 and matching names with the grade df.
expectedobs2_joint <- as.data.frame(expectedobs2_joint)
expectedobs2_joint[1:n_subjectgrades][] <- expectedobs2_joint[1:n_subjectgrades][]+1
names(expectedobs2_joint) <- names(items2)


saveRDS(expectedobs2_joint, file = "results/expected_grades/expectedobs2_joint.rds")


          ##### VG3 #####

items3 <- read.csv("data.temp/items_all3.csv")

n_subjectgrades <- sum(!grepl("_c$", names(items3)))

            ######  mod 2d ######

subject_grades3 <- items3[,1:n_subjectgrades]

mod3_2d <- read_rds("results/models/mod3_2d.rds")
summary(mod3_2d)
mean_vec_2d <- c(0,0)
f_cov3_2d <- data.frame(c(1,.821),
                        c(.821,1))

car_vec <- rep(6, ncol(subject_grades3))

#changing grades to 0-5, a requirement for the function
subject_grades3[] <- subject_grades3[]-1


#compute expected grades for missing cells for Model 4
expectedobs3_2d <- computeExpectedObs(mod3_2d,
                                obsdata = subject_grades3, 
                                nquad = 15,
                                ndim = 2,
                                Mu = mean_vec_2d,
                                Sigma = f_cov3_2d,
                                Jelect = 0,
                                Jcat = car_vec)

#changing grades back to 1-6 and matching names with the grade df.
expectedobs3_2d <- as.data.frame(expectedobs3_2d)
expectedobs3_2d[] <- expectedobs3_2d[]+1
names(expectedobs3_2d) <- names(subject_grades3)

saveRDS(expectedobs3_2d, file = "results/expected_grades/expectedobs3_2d.rds")


##### mod joint #####

mod3_joint <- read_rds("results/models/mod_joint3")
summary(mod3_joint)
mean_vec_3d <- c(0,0,0)

f_cov3_3d <- data.frame(c(1, .806, -.619),
                        c(.806, 1, -.364 ),
                        c(-.619, -.364 , 1))

car_vec <- rep(6, n_subjectgrades)

#changing grades to 0-5, a requirement for the function
items3[1:n_subjectgrades][] <- items3[1:n_subjectgrades][]-1


#compute expected grades for missing cells for Model 4
expectedobs3_joint <- computeExpectedObs(mod3_joint,
                                   obsdata = items3, 
                                   nquad = 15,
                                   ndim = 3,
                                   Mu = mean_vec_3d,
                                   Sigma = f_cov3_3d,
                                   Jelect = 11,
                                   Jcat = car_vec)

#changing grades back to 1-6 and matching names with the grade df.
expectedobs3_joint <- as.data.frame(expectedobs3_joint)
expectedobs3_joint[1:n_subjectgrades][] <- expectedobs3_joint[1:n_subjectgrades][]+1
names(expectedobs3_joint) <- names(items3)

saveRDS(expectedobs3_joint, file = "results/expected_grades/expectedobs3_joint.rds")

