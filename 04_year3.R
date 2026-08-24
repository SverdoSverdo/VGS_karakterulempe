source("scripts/00_settings.R")

gradedf <- read.csv("data.temp/gradedf.csv")
full_names <- read.csv("data.temp/dir_fagkoder_ST.csv")

gradedf3 <- gradedf[gradedf$year == 3,]
length(unique(gradedf3$w19_0634_lnr))
sort(table(gradedf3$fagkode))


mandatory_subjects <- c("NOR1267", "NOR1268", "NOR1269", "REL1003", "HIS1010", "KRO1019")

# count rows per fagkode, drop mandatory subjects, keep top 30
n_per_fag <- table(gradedf3$fagkode)
n_per_fag <- n_per_fag[!names(n_per_fag) %in% mandatory_subjects]
n_per_fag <- sort(n_per_fag, decreasing = TRUE)[1:30]

plot(as.integer(n_per_fag), type = "l",
     xlab = "subjects", ylab = "students enrolled in subject")

#what electives are REA students choosing?
sort(table(gradedf3$fagkode[gradedf3$program == "STREA3"]))

#what electives are SSC students choosing?
sort(table(gradedf3$fagkode[gradedf3$program == "STSSA3"]))

#including top 20 most common subjects
included_subjects_3 <- names(sort(table(gradedf3$fagkode), decreasing = TRUE))[1:20]
included_subjects_3 <- included_subjects_3[!included_subjects_3 == "KRO1019"] # removing gym


length(unique(gradedf3$w19_0634_lnr)) #25,094 students before
gradedf3 <- gradedf3[gradedf3$fagkode %in% included_subjects_3,] # only include the included_subjects
length(unique(gradedf3$w19_0634_lnr)) #24,810 students after


# check if any row has more than one non-empty value across skr, mun, kar_annen
exam_cols <- c("skr", "mun", "kar_annen")

non_empty <- sapply(gradedf3[exam_cols], function(x) !is.na(x) & x != "")
n_nonempty <- rowSums(non_empty)

# any rows with more than 1 non-empty exam value? nope
table(n_nonempty)

gradedf3$exam <- dplyr::coalesce(
  ifelse(gradedf3$skr != "" & !is.na(gradedf3$skr), gradedf3$skr, NA),
  ifelse(gradedf3$mun != "" & !is.na(gradedf3$mun), gradedf3$mun, NA),
  ifelse(gradedf3$kar_annen != "" & !is.na(gradedf3$kar_annen), gradedf3$kar_annen, NA)
)

gradedf3 <- gradedf3 %>%
  mutate(across(c(stp, exam), ~ ifelse(.x %in% c("IV", "IM"), "1", .x)))


# coerce all three grade columns to numeric; every letter code -> NA
for (v in c("stp", "exam")) {
  gradedf3[[v]] <- as.numeric(gradedf3[[v]])
}

# keep only id, timevar, and the three grade columns
gradedf3_wide <- gradedf3[c("w19_0634_lnr", "fagkode", "stp", "exam")]

# reshape wide; default names come out as stp.fagkode etc.
gradedf3_wide <- reshape(gradedf3_wide, dir = "wide",
                         idvar = "w19_0634_lnr",
                         timevar = "fagkode",
                         v.names = c("stp", "exam"))

# rename columns from <var>.<fagkode> to <fagkode>_<var>
vars <- c("stp", "exam")
old <- grep(paste0("^(", paste(vars, collapse = "|"), ")\\."),
            names(gradedf3_wide))
names(gradedf3_wide)[old] <- sub("^(.*)\\.(.*)$", "\\2_\\1",
                                 names(gradedf3_wide)[old])


# order: by fagkode alphabetically, then stp -> skr -> mun within each
id <- "w19_0634_lnr"
  cols <- setdiff(names(gradedf3_wide), id)
  fag <- sub("_(stp|exam)$", "", cols)
  suf <- sub("^.*_(stp|exam)$", "\\1", cols)
  suf_rank <- match(suf, c("stp", "exam"))
  ord <- order(fag, suf_rank)

gradedf3_wide <- gradedf3_wide[, c(id, cols[ord])]

#many subjects with virtually 0 exam grades
(counts <- colSums(!is.na(gradedf3_wide[-1])))

#removing those
keep <- c(TRUE, counts >= 10)
gradedf3_wide <- gradedf3_wide[, keep]

write.csv(gradedf3_wide, "data.temp/gradedf3_wide.temp.csv", row.names = F)


        #### 1. DESCRIPTIVES ####


#students per specialization
table(unique(gradedf3[, c("w19_0634_lnr", "program")])$program)

#N grades and total number of grades per subject
total_responses <- rbind(sapply(gradedf3_wide[-1], table),sapply(gradedf3_wide[-1], function(x) sum(!is.na(x))))
row.names(total_responses)[nrow(total_responses)] <- "total"

#getting subject names
cols <- colnames(total_responses)
codes <- str_remove(cols, "_(stp|exam)$")

# look each up in full_names (adjust column names to match your str() output)
titles <- full_names$title[match(codes, full_names$code)]

grade3_desc <- as.data.frame(rbind(titles,total_responses))


#Getting exam correlations for each subject
resp_cols <- colnames(grade3_desc)
resp_code <- str_remove(resp_cols, "_(stp|exam)$")
resp_suf  <- str_extract(resp_cols, "(stp|exam)$")

exam_cor <- vapply(seq_along(resp_cols), function(i) {
  if (resp_suf[i] != "stp") return(NA_real_)        # only _stp gets a value
  
  stp_col <- paste0(resp_code[i], "_stp")
  exam_col <- paste0(resp_code[i], "_exam")
  
  # which exam column(s) exist in gradedf1_wide
  exam_col <- intersect(c(exam_col), colnames(gradedf3_wide))
  
  # need the stp column present, and exactly one exam column
  if (!(stp_col %in% colnames(gradedf3_wide)) || length(exam_col) == 0)
    return(NA_real_)
  
  cor(gradedf3_wide[[stp_col]], gradedf3_wide[[exam_col]],
      use = "pairwise.complete.obs")
}, numeric(1))

names(exam_cor) <- resp_cols

grade3_desc["exam_cor", ] <- round(exam_cor[colnames(grade3_desc)],2)

openxlsx::write.xlsx(grade3_desc, "results/grade3_desc.xlsx", rowNames = T)
