#In second grade, besides physical education, there are only three subjects taken by all students: HIS1009, NOR1265, and NOR1264
#So, not possible to assess model fit for mandatory subjects.

source("scripts/00_settings.R")

subject_desc <- read.csv("data.temp/dir_fagkoder_ST.csv")

gradedf <- read.csv("data.temp/gradedf.csv")
gradedf <- gradedf[gradedf$year == 2,]

mandatory_subjects <- c("NOR1264", "NOR1265", "HIS1009", "KRO1018","REA3060","REA3056","MAT1023")

#what electives are REA students choosing?
sort(table(gradedf$fagkode[gradedf$program == "STREA2"]))

#what electives are SSC students choosing?
sort(table(gradedf$fagkode[gradedf$program == "STSSA2"]))

# count rows per fagkode, drop mandatory subjects
electives_n <- table(gradedf$fagkode[!gradedf$fagkode %in% mandatory_subjects])
electives_n <- sort(electives_n, decreasing = TRUE)[1:30]

plot(as.integer(electives_n), type = "l",
     xlab = "subjects", ylab = "students enrolled in subject")
    abline(v = 11, col = "red")


#Include top 11 electives + mandatory  subjects
included_subjects <- c(names(electives_n)[1:11], mandatory_subjects)

#description of the incuded subjects
subject_desc <- subject_desc[subject_desc$code %in% included_subjects,]

#removing gym
included_subjects <- included_subjects[!included_subjects == "KRO1018"]

#removing spanish and german
german_spanish <- c("FSP6222","FSP6242")
included_subjects <- included_subjects[!included_subjects %in% german_spanish]

#updating description
subject_desc <- subject_desc[subject_desc$code %in% included_subjects,]

#incuded electives
included_electives <- setdiff(included_subjects, mandatory_subjects)


#gradedf in wide format
gradedf_wide <- read.csv("data.temp/gradedf2_wide.temp.csv")

#limiting to included_subjects, dealing with the suffixes in gradedf_wide
subject_names <- sub("_(stp|exam)$", "", names(gradedf_wide))
  keep_cols <- names(gradedf_wide)[subject_names == "w19_0634_lnr" | subject_names %in% included_subjects]
  gradedf_wide <- gradedf_wide[, keep_cols]

sort(colSums(!is.na(gradedf_wide)))


#removing rows with all NAs
gradedf_wide <- gradedf_wide %>%
  filter(if_any(2:ncol(gradedf_wide), ~ !is.na(.)))

write.csv(gradedf_wide, file = "data.temp/gradedf2_wide.csv", row.names = F)

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

mod_2d <- mirt(items, spec_2d, itemtype = "graded")
summary(mod_2d)
itemfit(mod_2d, fit_stats = "infit")

anova(mod_grm,mod_2d) # 2 dimensional model fits way better

saveRDS(mod_2d, file = "results/models/mod2_2d.rds")


residuals(mod_2d, type = "LD") # upper triangle aint too bad. 


        #### CHOICE MODEL ####

choicedf <- read.csv("data.temp/choicedf2.csv")
choicedf <- choicedf[, c("w19_0634_lnr", names(choicedf)[names(choicedf) %in% included_electives])]

#they get two grades in English, removing one from the choicedf
choicedf$SPR3030 <- NULL

#giving the variables a suffix to distinguish them from subject grades
names(choicedf)[-1] <- paste0(names(choicedf)[-1], "_c")

items_choice <- choicedf[,-1]

choice_spec_constrained <- paste0(
  "AA = ", paste(names(items_choice), collapse = ", "), "\n",
  "CONSTRAIN = (1-8", ", t1), (1-8", ", a1)"
)
cat(choice_spec_constrained)

mod_choice_constraint <- mirt(data = items_choice, model = choice_spec_constrained, itemtype = "ggum",
                                 method = "EM",SE = T, quadpts = 20)

M2(mod_choice_constraint, na.rm = T)
summary(mod_choice_constraint)
coef(mod_choice_constraint, standard = T)

choice_sv <- mod2values(mod_choice_constraint)

#freely estimated t parameters, but constrained discrimination
choice_spec_free <- paste0(
  "F1 = ", paste(names(items_choice), collapse = ", "), "\n",
  "CONSTRAIN =  (1-8", ", a1)"
)
cat(choice_spec_free)

choice_model_free <- mirt(data = items_choice, model = choice_spec_free, itemtype = "ggum",
                          method = "EM",SE = T, 
                          pars = choice_sv,
                          quadpts = 20)
summary(choice_model_free)
M2(choice_model_free, na.rm = T)



        #### JOINT MODEL ####

full_df <- left_join(gradedf_wide,choicedf, by = "w19_0634_lnr")
items_all <- full_df[,-1]

#for use later
write.csv(items_all, file = "data.temp/items_all2.csv", row.names = F)


#model specification
print(nrow(items_all))

spec_3d <- "STEM = MAT1023_stp, MAT1023_exam, REA3035_stp, REA3035_exam, REA3038_stp, REA3038_exam, REA3045_stp, REA3045_exam, REA3056_stp, REA3056_exam, REA3060_stp, REA3060_exam
HUM = HIS1009_stp, NOR1264_stp, NOR1265_stp, SAM3045_stp, SAM3045_exam, SAM3054_stp, SAM3054_exam, SAM3057_stp, SAM3057_exam, SAM3072_stp, SAM3072_exam, SPR3029_stp, SPR3029_exam, SPR3030_stp, SPR3030_exam
CHOICE = SAM3045_c, REA3038_c, REA3045_c, SAM3054_c, REA3035_c, SAM3072_c, SAM3057_c, SPR3029_c
CONSTRAIN = (28-35, a3)
COV = STEM*HUM*CHOICE"

cat(spec_3d)

#Starting values that match the final parameter-estimates for quicker estimation
starting_vals2 <- readRDS("data.temp/StartVals_joint2.rds")

#fit the model
mod_joint <- mirt(data = items_all, model = spec_3d,
                  itemtype = c(rep("graded", 27), rep("ggum", 8)),
                  method = "EM", SE = T, quadpts = 20,
                  pars = starting_vals2, # remove this if estimating without starting values
                  technical = list(NCYCLES = 10000, MAXQUAD = 40000))

saveRDS(mod_joint, "results/models/mod_joint2.rds")
