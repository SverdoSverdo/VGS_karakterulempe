source("scripts/00_settings.R")


subject_desc <- read.csv("data.temp/dir_fagkoder_ST.csv")

gradedf <- read.csv("data.temp/gradedf.csv")
gradedf <- gradedf[gradedf$year == 3,]

mandatory_subjects <- c("NOR1267", "NOR1268", "NOR1269", "REL1003", "HIS1010", "KRO1019")

#what electives are REA students choosing?
sort(table(gradedf$fagkode[gradedf$program == "STREA3"]))

#what electives are SSC students choosing?
sort(table(gradedf$fagkode[gradedf$program == "STSSA3"]))

# count rows per fagkode, drop mandatory subjects
electives_n <- table(gradedf$fagkode[!gradedf$fagkode %in% mandatory_subjects])
electives_n <- sort(electives_n, decreasing = TRUE)[1:30]

plot(as.integer(electives_n), type = "l",
     xlab = "subjects", ylab = "students enrolled in subject")
abline(v = 12, col = "red")


#Include top 12 electives (to include physics) + mandatory  subjects
included_subjects <- c(names(electives_n)[1:12], mandatory_subjects)

#description of the incuded subjects
subject_desc <- subject_desc[subject_desc$code %in% included_subjects,]

#removing gym
included_subjects <- included_subjects[!included_subjects == "KRO1019"]

#incuded electives
included_electives <- setdiff(included_subjects, mandatory_subjects)

#gradedf in wide format
gradedf_wide <- read.csv("data.temp/gradedf3_wide.temp.csv")

#limiting to included_subjects, dealing with the suffixes in gradedf_wide
subject_names <- sub("_(stp|exam)$", "", names(gradedf_wide))
keep_cols <- names(gradedf_wide)[subject_names == "w19_0634_lnr" | subject_names %in% included_subjects]
gradedf_wide <- gradedf_wide[, keep_cols]


#removing rows with all NAs
gradedf_wide <- gradedf_wide %>%
  filter(if_any(2:ncol(gradedf_wide), ~ !is.na(.)))

#for use later
write.csv(gradedf_wide, file = "data.temp/gradedf3_wide.csv", row.names = F)


subject_names <- sub("_(stp|exam)$", "", names(gradedf_wide))
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

# item matrix: everything except the id column
items <- gradedf_wide[-1]
sapply(items, table)


# fit both unidimensional models
mod_grm <- mirt(items, 1, itemtype = "graded",
                technical = list(NCYCLES = 2000))

mod_gpcm <- mirt(items, 1, itemtype = "gpcm",
                 technical = list(NCYCLES = 2000))

# compare fit
anova(mod_gpcm, mod_grm) #grm provided a better fit

itemfit(mod_grm, fit_stats = "infit")
summary(mod_grm)


#Only stp grades
items_stp <- items %>%
  select(ends_with("stp"))

mod_stp <- mirt(items_stp, 1, itemtype = "graded")
itemfit(mod_stp, fit_stats = "infit")
summary(mod_stp)


# items_stem = items starting with REA or MAT, regardless of suffix (e.g. REA3056_stp, MAT1200_skr)
item_names <- names(items)
items_stem <- item_names[grepl("^(REA|MAT)", item_names)]
items_hum <- setdiff(item_names, items_stem)

# 2d model specification
spec_2d <- paste0(
  "STEM = ", paste(items_stem, collapse = ", "), "\n",
  "HUM = ", paste(items_hum, collapse = ", "), "\n",
  "COV = STEM*HUM"
)
cat(spec_2d)

mod_2d <- mirt(items, spec_2d, itemtype = "graded")
summary(mod_2d)
itemfit(mod_2d, fit_stats = "infit")


anova(mod_grm,mod_2d) # 2 dimensional model fits way better
residuals(mod_2d, type = "LD") # upper triangle aint too bad. 

saveRDS(mod_2d, "results/models/mod3_2d.rds")


#### CHOICE MODEL ####

choicedf <- read.csv("data.temp/choicedf3.csv")
choicedf <- choicedf[, c("w19_0634_lnr", names(choicedf)[names(choicedf) %in% included_electives])]

#they get two grades in English, removing one from the choicedf
choicedf$SPR3032 <- NULL

#giving the variables a suffix to distinguish them from subject grades
names(choicedf)[-1] <- paste0(names(choicedf)[-1], "_c")


items_choice <- choicedf[,-1]


choice_spec_constrained <- paste0(
  "F1 = ", paste(names(items_choice), collapse = ", "), "\n",
  "CONSTRAIN = (1-11", ", t1), (1-11", ", a1)"
)


mod_choice_constraint <- mirt(data = items_choice, model = choice_spec_constrained, itemtype = "ggum",
                              method = "EM",SE = T, quadpts = 20,
                              technical = list(NCYCLES = 2000))

M2(mod_choice_constraint, na.rm = T)
summary(mod_choice_constraint)
coef(mod_choice_constraint, standard = T)

choice_sv <- mod2values(mod_choice_constraint)

#freely estimated t parameters, but constrained discrimination
choice_spec_free <- paste0(
  "F1 = ", paste(names(items_choice), collapse = ", "), "\n",
  "CONSTRAIN =  (1-11", ", a1)"
)

choice_model_free <- mirt(data = items_choice, model = choice_spec_free, itemtype = "ggum",
                          method = "EM",SE = T, 
                          pars = choice_sv,
                          quadpts = 20)
summary(choice_model_free)
coef(choice_model_free)
M2(choice_model_free, na.rm = T)




#### JOINT MODEL ####

full_df <- left_join(gradedf_wide,choicedf, by = "w19_0634_lnr")
items_all <- full_df[,-1]

#for use later
write.csv(items_all, file = "data.temp/items_all3.csv", row.names = F)

spec_3d <- "STEM = REA3036_stp, REA3036_exam, REA3039_stp, REA3039_exam, REA3046_stp, REA3046_exam, REA3058_stp, REA3058_exam, REA3062_stp, REA3062_exam
HUM = HIS1010_stp, HIS1010_exam, NOR1267_stp, NOR1267_exam, NOR1268_stp, NOR1268_exam, NOR1269_stp, NOR1269_exam, REL1003_stp, REL1003_exam, SAM3046_stp, SAM3046_exam, SAM3051_stp, SAM3051_exam, SAM3055_stp, SAM3055_exam, SAM3058_stp, SAM3058_exam, SAM3073_stp, SAM3073_exam, SPR3031_stp, SPR3031_exam, SPR3032_stp, SPR3032_exam
CHOICE = SAM3051_c, SAM3058_c, SAM3073_c, REA3036_c, REA3039_c, REA3058_c, REA3046_c, REA3062_c, SAM3055_c, SAM3046_c, SPR3031_c
CONSTRAIN = (35-45, a3)
COV = STEM*HUM*CHOICE"

cat(spec_3d)


#the model is highly complex, with many items. We help it by providing starting values that will ease convergence. 
sv_joint <- mirt(data = items_all, model = spec_3d,
                 itemtype = c(rep("graded", 34), rep("ggum", 11)),
                 pars = "values")

sv_choice <- mod2values(choice_model_free)

# slope and location are both dimension-indexed in mirt's ggum
sv_choice$name[sv_choice$name == "a1"] <- "a3"
sv_choice$name[sv_choice$name == "b1"] <- "b3"

idx <- match(paste(sv_joint$item,  sv_joint$name),
             paste(sv_choice$item, sv_choice$name))
hit <- !is.na(idx) & sv_joint$item %in% names(items_choice)

sv_joint$value[hit] <- sv_choice$value[idx[hit]]


table(sv_joint$name[hit], sv_joint$est[hit])
subset(sv_joint, item == "SAM3051_c", c("parnum", "name", "value", "est"))


#Starting values that match the final parameter-estimates for quicker estimation
starting_vals3 <- readRDS("data.temp/StartVals_joint3.rds")

#estimating model
mod_joint <- mirt(data = items_all, model = spec_3d,
                  itemtype = c(rep("graded", 34), rep("ggum", 11)),
                  method = "EM", SE = T, quadpts = 20,
                  pars = starting_vals3, # change this with sv_joint to estimate the non-choice parameters from scratch
                  technical = list(NCYCLES = 10000, MAXQUAD = 40000))

saveRDS(mod_joint, "results/models/mod_joint3.rds")

