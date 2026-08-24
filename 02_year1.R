source("scripts/00_settings.R")


gradedf <- read.csv("data.temp/gradedf.csv")
  full_names <- read.csv("data.temp/dir_fagkoder_ST.csv")

gradedf1 <- gradedf[gradedf$year == 1,]
  length(unique(gradedf1$w19_0634_lnr))
  sort(table(gradedf1$fagkode))


#Include all "common" subjects
included_subjects_1 <- names(sort(table(gradedf1$fagkode), decreasing = TRUE))[1:9]
included_subjects_1 <- included_subjects_1[!included_subjects_1 == "KRO1017"]


length(unique(gradedf1$w19_0634_lnr)) #26,653 students before
gradedf1 <- gradedf1[gradedf1$fagkode %in% included_subjects_1,] # only include the included_subjects
length(unique(gradedf1$w19_0634_lnr)) #26261 students after

gradedf1$exam <- dplyr::coalesce(
  ifelse(gradedf1$skr != "" & !is.na(gradedf1$skr), gradedf1$skr, NA),
  ifelse(gradedf1$mun != "" & !is.na(gradedf1$mun), gradedf1$mun, NA),
  ifelse(gradedf1$kar_annen != "" & !is.na(gradedf1$kar_annen), gradedf1$kar_annen, NA)
)


gradedf1 <- gradedf1 %>%
  mutate(across(c(stp, exam), ~ ifelse(.x %in% c("IV", "IM"), "1", .x)))

# coerce all  grade columns to numeric; every letter code -> NA
for (v in c("stp", "exam")) {
  gradedf1[[v]] <- as.numeric(gradedf1[[v]])
}

# keep only id, timevar, and the three grade columns
gradedf1_wide <- gradedf1[c("w19_0634_lnr", "fagkode", "stp","exam")]


# reshape wide; default names come out as stp.fagkode etc.
gradedf1_wide <- reshape(gradedf1_wide, dir = "wide",
                         idvar = "w19_0634_lnr",
                         timevar = "fagkode",
                         v.names = c("stp", "exam"))

# rename columns from <var>.<fagkode> to <fagkode>_<var>
vars <- c("stp", "exam")
old <- grep(paste0("^(", paste(vars, collapse = "|"), ")\\."),
            names(gradedf1_wide))
names(gradedf1_wide)[old] <- sub("^(.*)\\.(.*)$", "\\2_\\1",
                                 names(gradedf1_wide)[old])


# order: by fagkode alphabetically, then stp -> skr -> mun within each
id <- "w19_0634_lnr"
cols <- setdiff(names(gradedf1_wide), id)
fag <- sub("_(stp|exam)$", "", cols)
suf <- sub("^.*_(stp|exam)$", "\\1", cols)
suf_rank <- match(suf, c("stp", "exam"))
ord <- order(fag, suf_rank)

gradedf1_wide <- gradedf1_wide[, c(id, cols[ord])]

#many subjects with 5 or fewer exam grades
(counts <- colSums(!is.na(gradedf1_wide[-1])))

#removing those
keep <- c(TRUE, counts >= 10)
gradedf1_wide <- gradedf1_wide[, keep]

write.csv(gradedf1_wide, "data.temp/gradedf1_wide.temp.csv", row.names = F)


        ####  DESCRIPTIVES ####


#students per specialization
table(gradedf1$program)


#getting subject names
total_responses <- rbind(sapply(gradedf1_wide[-1], table),sapply(gradedf1_wide[-1], function(x) sum(!is.na(x))))
row.names(total_responses)[nrow(total_responses)] <- "total"

#getting subject names
cols <- colnames(total_responses)
codes <- str_remove(cols, "_(stp|exam)$")   # MAT1021_skr -> MAT1021

# look each up in full_names (adjust column names to match your str() output)
titles <- full_names$title[match(codes, full_names$code)]

grade1_desc <- as.data.frame(rbind(titles,total_responses))


#Getting exam correlations for each subject
resp_cols <- colnames(grade1_desc)
resp_code <- str_remove(resp_cols, "_(stp|exam)$")
resp_suf  <- str_extract(resp_cols, "(stp|exam)$")

exam_cor <- vapply(seq_along(resp_cols), function(i) {
  if (resp_suf[i] != "stp") return(NA_real_)        # only _stp gets a value
  
  stp_col <- paste0(resp_code[i], "_stp")
  exam_col <- paste0(resp_code[i], "_exam")
  
  # which exam column(s) exist in gradedf1_wide
  exam_col <- intersect(c(exam_col), colnames(gradedf1_wide))
  
  # need the stp column present, and exactly one exam column
  if (!(stp_col %in% colnames(gradedf1_wide)) || length(exam_col) == 0)
    return(NA_real_)
  
  cor(gradedf1_wide[[stp_col]], gradedf1_wide[[exam_col]],
      use = "pairwise.complete.obs")
}, numeric(1))

names(exam_cor) <- resp_cols

grade1_desc["exam_cor", ] <- round(exam_cor[colnames(grade1_desc)],2)

openxlsx::write.xlsx(grade1_desc, "results/grade1_desc.xlsx", rowNames = T)




