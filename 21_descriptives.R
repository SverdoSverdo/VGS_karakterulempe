source("scripts/00_settings.R")


#### PREPARING DFs ####

grades_obs1 <- read.csv("data.temp/gradedf1_wide.csv")
grades_obs2 <- read.csv("data.temp/gradedf2_wide.csv")
grades_obs3 <- read.csv("data.temp/gradedf3_wide.csv")


#description of subjects
included_subjects <- unique(sub("_.*", "", names(c(grades_exp1,grades_exp2,grades_exp3))))
full_names <- read.csv("data.temp/dir_fagkoder_ST.csv")
full_names <- full_names[full_names$code %in% included_subjects,]


#everything except T and P maths
drop_subjects1 <-  c("ENG1007","GEO1003", "NAT1007", "NOR1260", "NOR1261", "SAK1001")

#mandatory subjects and S maths
drop_subjects2  <- c("NOR1264", "NOR1265", "HIS1009", "KRO1018","REA3060")

#mandatory subjects and S maths
drop_subjects3 <- c("NOR1267", "NOR1268", "NOR1269", "REL1003", "HIS1010", "KRO1019","REA3062")


#Dropping mandatory subjects, except P and T math year 2
drop_subjects <- function(df, subject_codes) {
  code_prefix <- sub("_.*", "", names(df))
  df[, !(code_prefix %in% subject_codes), drop = FALSE]
}

grades_obs1 <- drop_subjects(grades_obs1, drop_subjects1)
grades_obs2 <- drop_subjects(grades_obs2, drop_subjects2)
grades_obs3 <- drop_subjects(grades_obs3, drop_subjects3)

#defining SSE and REA electives
SSE_subjects2 <- c("SAM3045","SAM3054","SAM3057","SAM3072","SPR3029","SPR3030")
REA_subjects2 <- c("REA3035","REA3038","REA3045","REA3056")

SSE_subjects3 <- c("SAM3046","SAM3051","SAM3055","SAM3058","SAM3073","SPR3031","SPR3032")
REA_subjects3 <- c("REA3036","REA3039","REA3046","REA3058")


#updating subject description to the newly chosen subjects only
included_subjects <- unique(sub("_.*", "", names(c(grades_obs1,grades_obs2,grades_obs3))))
full_names <- read.csv("data.temp/dir_fagkoder_ST.csv")
full_names <- full_names[full_names$code %in% included_subjects,]

full_names$year <- NA
full_names$year[full_names$code %in% sub("_stp", "", names(grades_obs1))] <- "vg1"
full_names$year[full_names$code %in% sub("_stp", "", names(grades_obs2))] <- "vg2"
full_names$year[full_names$code %in% sub("_stp", "", names(grades_obs3))] <- "vg3"

# Reorder columns
full_names <- full_names[, c("year", "title", "code")]


        #### DESCRIPTIVES TABLE ####
descriptives <- full_names

# Get columns for each code
descriptives$n_stp <- sapply(descriptives$code, function(c) {
  cols <- paste0(c, "_stp")
  sum(!is.na(unlist(lapply(list(grades_obs1, grades_obs2, grades_obs3), function(df) df[[cols]]))))
})

#mean stp for each subject
descriptives$mean_stp <- sapply(descriptives$code, function(c) {
  cols <- paste0(c, "_stp")
  mean(unlist(lapply(list(grades_obs1, grades_obs2, grades_obs3), function(df) df[[cols]])), na.rm = TRUE)
})

#mean exam grade for each subject
descriptives$mean_exam <- sapply(descriptives$code, function(c) {
  cols <- paste0(c, "_exam")
  mean(unlist(lapply(list(grades_obs1, grades_obs2, grades_obs3), function(df) df[[cols]])), na.rm = TRUE)
})

desired_order <- c(
  "MAT1019", "MAT1021",
  SSE_subjects2,
  "MAT1023",
  REA_subjects2,
  SSE_subjects3,
  REA_subjects3,
  "REA3058"
)

descriptives$order <- match(descriptives$code, desired_order)
descriptives <- descriptives[order(descriptives$order), ]
descriptives$order <- NULL
descriptives$code <- NULL

descriptives[c("mean_stp","mean_exam")] <- round(descriptives[c("mean_stp","mean_exam")],2) #rounding



#Whats the average grades across the included SSE AND REA subjects?

#exam
avg_grades <- data.frame(
  year = c("vg2", "vg2", "vg3", "vg3"),
  track = c("SSE", "REA", "SSE", "REA"),
  mean_stp = c(
    mean(sapply(paste0(c(SSE_subjects2, "MAT1023"), "_stp"), function(col) mean(grades_obs2[[col]], na.rm = TRUE))),
    mean(sapply(paste0(REA_subjects2, "_stp"), function(col) mean(grades_obs2[[col]], na.rm = TRUE))),
    mean(sapply(paste0(SSE_subjects3, "_stp"), function(col) mean(grades_obs3[[col]], na.rm = TRUE))),
    mean(sapply(paste0(c(REA_subjects3), "_stp"), function(col) mean(grades_obs3[[col]], na.rm = TRUE)))
  ),
  mean_exam = c(
    mean(sapply(paste0(c(SSE_subjects2, "MAT1023"), "_exam"), function(col) mean(grades_obs2[[col]], na.rm = TRUE))),
    mean(sapply(paste0(REA_subjects2, "_exam"), function(col) mean(grades_obs2[[col]], na.rm = TRUE))),
    mean(sapply(paste0(SSE_subjects3, "_exam"), function(col) mean(grades_obs3[[col]], na.rm = TRUE))),
    mean(sapply(paste0(c(REA_subjects3), "_exam"), function(col) mean(grades_obs3[[col]], na.rm = TRUE)))
  )
)

avg_grades[c("mean_stp", "mean_exam")] <- round(avg_grades[c("mean_stp", "mean_exam")], 2)



          ##### how many students in each specialization? ####

gradedf1_wide <- read.csv("data.temp/gradedf1_wide.csv")
gradedf2_wide <- read.csv("data.temp/gradedf2_wide.csv")
gradedf3_wide <- read.csv("data.temp/gradedf3_wide.csv")

included_students <- c(gradedf1_wide$w19_0634_lnr,gradedf2_wide$w19_0634_lnr,gradedf3_wide$w19_0634_lnr)
included_students <- unique(included_students)

gradedf <- gradedf[gradedf$w19_0634_lnr %in% included_students,]

gradedf %>%
  distinct(w19_0634_lnr, program) %>%
  count(program)


# SSE students taking REA subjects
rea_cross <- gradedf |>
  filter(program %in% c("STSSA2", "STSSA3"),
         fagkode %in% c(REA_subjects2, REA_subjects3)) |>
  count(program, fagkode) |>
  pivot_wider(names_from = program, values_from = n, values_fill = 0)

# REA students taking SSE subjects
sse_cross <- gradedf |>
  filter(program %in% c("STREA2", "STREA3"),
         fagkode %in% c(SSE_subjects2, SSE_subjects3)) |>
  count(program, fagkode) |>
  pivot_wider(names_from = program, values_from = n, values_fill = 0)

sse_cross <- merge(sse_cross, full_names[, c("code", "title")], by.x = "fagkode", by.y = "code")
rea_cross <- merge(rea_cross, full_names[, c("code", "title")], by.x = "fagkode", by.y = "code")

sse_cross <- sse_cross[, c("title", "fagkode", "STREA2", "STREA3")]
sse_cross$fagkode <- NULL
rea_cross <- rea_cross[, c("title", "fagkode", "STSSA2", "STSSA3")]
rea_cross$fagkode <- NULL


print(rea_cross)

print(sse_cross)

rea_cross$students <- rea_cross$STSSA2 + rea_cross$STSSA3
rea_cross <- rea_cross[, c("title", "students")]

sse_cross$students <- sse_cross$STREA2 + sse_cross$STREA3
sse_cross <- sse_cross[, c("title", "students")]


