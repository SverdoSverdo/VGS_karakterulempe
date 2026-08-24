source("scripts/00_settings.R")

subject_desc <- read.csv("data.temp/dir_fagkoder_ST.csv")

gradedf <- read.csv("data.temp/gradedf.csv")
gradedf <- gradedf[gradedf$year == 1,]


# count rows per fagkode
subjects <- table(gradedf$fagkode)
subjects <- sort(subjects, decreasing = TRUE)[1:30]

#include all mandatory subjects, except for languages
included_subjects <- c(names(subjects)[1:9])

#removing gym
included_subjects <- included_subjects[!included_subjects == "KRO1017"]

#description of the incuded subjects
subject_desc <- subject_desc[subject_desc$code %in% included_subjects,]

#gradedf in wide format
gradedf_wide <- read.csv("data.temp/gradedf1_wide.temp.csv")

#limiting gradedf to included_subjects, dealing with the suffixes in gradedf_wide
subject_names <- sub("_(stp|exam)$", "", names(gradedf_wide))
keep_cols <- names(gradedf_wide)[subject_names == "w19_0634_lnr" | subject_names %in% included_subjects]
gradedf_wide <- gradedf_wide[, keep_cols]

#removing rows with all NAs
gradedf_wide <- gradedf_wide %>%
  filter(if_any(2:ncol(gradedf_wide), ~ !is.na(.)))

write.csv(gradedf_wide, file = "data.temp/gradedf1_wide.csv", row.names = F)

#table with N observed grades in stp and exams
suffixes <- sub(".*_(stp|exam)$", "\\1", names(gradedf_wide))

non_na_counts <- colSums(!is.na(gradedf_wide))

count_suffix <- function(cd, suf) {
  idx <- subject_names == cd & suffixes == suf
  if (!any(idx)) return(NA_integer_)  # subject has no column of this type
  sum(non_na_counts[idx])
}

subject_desc$n_stp <- sapply(subject_desc$code, count_suffix, suf = "stp")
subject_desc$n_exam <- sapply(subject_desc$code, count_suffix, suf = "exam")

subject_desc


##### 3.1 fitting model #####

# item matrix: everything except the id column
items <- gradedf_wide[-1]

# fit both unidimensional models
mod_grm  <- mirt(items, 1, itemtype = "graded")
mod_gpcm <- mirt(items, 1, itemtype = "gpcm")

# compare fit
anova(mod_gpcm, mod_grm) #GRM is the preferred model

saveRDS(mod_grm, file = "results/models/mod1.rds")

itemfit(mod_grm, fit_stats = "infit")
summary(mod_grm)
residuals(mod_grm, type = "LD")

#Only stp grades
items_stp <- items %>%
  select(ends_with("stp"))

mod_stp <- mirt(items_stp, 1, itemtype = "graded")
  itemfit(mod_stp, fit_stats = "infit")
  summary(mod_stp)
  
#Only mandatory stp grades (no maths subjects)
items_mandatory <- items_stp %>%
  select(!starts_with("MAT"))

mod_mandatory <- mirt(items_mandatory, 1, itemtype = "graded")
  M2(mod_mandatory,type = "C2", na.rm = T) # everything great, except a high RMSEA. 

#Residual correlation matrix: RMSEA high due to residual covariance between Norwegian subjects likely.
(M2(mod_mandatory, type = "C2", na.rm = TRUE, residmat = TRUE))

#adding residual covariances between the Norwegian subjects
  spec_resid <- paste0(
    "AA = ", paste(names(items_mandatory), collapse = ", "), "\n",
    "S1 = NOR1260_stp, NOR1261_stp"
  )
cat(spec_resid)

#model
mod_resid <- mirt(data = items_mandatory, model = spec_resid, itemtype = "graded",
                  technical = list(NCYCLES = 10000, MAXQUAD = 40000))
M2(mod_resid,type = "C2", na.rm = T) # amazing
summary(mod_resid)

