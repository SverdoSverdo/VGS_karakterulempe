setwd("") # set a path to your working directory

###the working directory needs to have folders:
#scripts
#data.temp
#results/expected grades
#results/models
#results/plots

if (!require("pacman")) install.packages("pacman")

pacman::p_load(dplyr,httr2,purrr, tidyr,tidyverse, ggplot2, data.table,readxl,
               lavaan,psych,mirt, corrplot,reshape2,fastGHQuad,parallel,
               pbapply,waldo,patchwork, gridExtra)

