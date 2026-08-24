source("scripts/00_settings.R")

gradedf <- read.csv("data.temp/gradedf.csv")
  full_names <- read.csv("data.temp/dir_fagkoder_ST.csv")

gradedf2 <- gradedf[gradedf$year == 2,]
length(unique(gradedf2$w19_0634_lnr))
sort(table(gradedf2$fagkode))

mandatory_subjects <- c("NOR1264", "NOR1265", "HIS1009", "KRO1018","REA3060","REA3056","MAT1023")
# REA3060 = S matte, REA3056 = R matte, MAT1023


# count rows per fagkode, drop mandatory subjects, keep top 30
n_per_fag <- table(gradedf2$fagkode)
n_per_fag <- n_per_fag[!names(n_per_fag) %in% mandatory_subjects]
n_per_fag <- sort(n_per_fag, decreasing = TRUE)[1:30]

plot(as.integer(n_per_fag), type = "l",
     xlab = "subjects", ylab = "students enrolled in subject")+
  abline(v = 11, col = "red")


#what electives are REA students choosing?
sort(table(gradedf2$fagkode[gradedf2$program == "STREA2"]))

#what electives are SSC students choosing?
sort(table(gradedf2$fagkode[gradedf2$program == "STSSA2"]))

#Include 20 top subjects
included_subjects_2 <- names(sort(table(gradedf2$fagkode), decreasing = TRUE))[1:20]
included_subjects_2 <- included_subjects_2[!included_subjects_2 == "KRO1018"]


length(unique(gradedf2$w19_0634_lnr)) #24,109 students before
gradedf2 <- gradedf2[gradedf2$fagkode %in% included_subjects_2,] # only include the included_subjects
length(unique(gradedf2$w19_0634_lnr)) #23,986 students after


# check if any row has more than one non-empty value across skr, mun, kar_annen
exam_cols <- c("skr", "mun", "kar_annen")

non_empty <- sapply(gradedf2[exam_cols], function(x) !is.na(x) & x != "")
n_nonempty <- rowSums(non_empty)

# any rows with more than 1 non-empty exam value? nope
table(n_nonempty)

gradedf2$exam <- dplyr::coalesce(
  ifelse(gradedf2$skr != "" & !is.na(gradedf2$skr), gradedf2$skr, NA),
  ifelse(gradedf2$mun != "" & !is.na(gradedf2$mun), gradedf2$mun, NA),
  ifelse(gradedf2$kar_annen != "" & !is.na(gradedf2$kar_annen), gradedf2$kar_annen, NA)
)

gradedf2 <- gradedf2 %>%
  mutate(across(c(stp, exam), ~ ifelse(.x %in% c("IV", "IM"), "1", .x)))

# coerce all three grade columns to numeric; every letter code -> NA
for (v in c("stp", "exam")) {
  gradedf2[[v]] <- as.numeric(gradedf2[[v]])
}

# keep only id, timevar, and the three grade columns
gradedf2_wide <- gradedf2[c("w19_0634_lnr", "fagkode", "stp", "exam")]


# reshape wide; default names come out as stp.fagkode etc.
gradedf2_wide <- reshape(gradedf2_wide, dir = "wide",
                         idvar = "w19_0634_lnr",
                         timevar = "fagkode",
                         v.names = c("stp", "exam"))

# rename columns from <var>.<fagkode> to <fagkode>_<var>
vars <- c("stp", "exam")
old <- grep(paste0("^(", paste(vars, collapse = "|"), ")\\."),
            names(gradedf2_wide))
names(gradedf2_wide)[old] <- sub("^(.*)\\.(.*)$", "\\2_\\1",
                                 names(gradedf2_wide)[old])


# order: by fagkode alphabetically, then stp -> skr -> mun within each
id <- "w19_0634_lnr"
cols <- setdiff(names(gradedf2_wide), id)
fag <- sub("_(stp|exam)$", "", cols)
suf <- sub("^.*_(stp|exam)$", "\\1", cols)
suf_rank <- match(suf, c("stp", "exam"))
ord <- order(fag, suf_rank)

gradedf2_wide <- gradedf2_wide[, c(id, cols[ord])]


#many subjects with 5 or fewer exam grades
(counts <- colSums(!is.na(gradedf2_wide[-1])))

#removing those
keep <- c(TRUE, counts >= 10)
gradedf2_wide <- gradedf2_wide[, keep]

gradedf2_wide <- gradedf2_wide %>%
  filter(if_any(everything(), ~ !is.na(.)))

write.csv(gradedf2_wide, "data.temp/gradedf2_wide.temp.csv", row.names = F)


        #### 1. DESCRIPTIVES ####


#students per specialization
table(unique(gradedf2[, c("w19_0634_lnr", "program")])$program)

#N grades and total number of grades per subject
total_responses <- rbind(sapply(gradedf2_wide[-1], table),sapply(gradedf2_wide[-1], function(x) sum(!is.na(x))))
row.names(total_responses)[nrow(total_responses)] <- "total"


#getting subject names
cols <- colnames(total_responses)
codes <- str_remove(cols, "_(stp|exam)$")   # MAT1021_skr -> MAT1021

# look each up in full_names (adjust column names to match your str() output)
titles <- full_names$title[match(codes, full_names$code)]

grade2_desc <- as.data.frame(rbind(titles,total_responses))


#Getting exam correlations for each subject
resp_cols <- colnames(grade2_desc)
resp_code <- str_remove(resp_cols, "_(stp|exam)$")
resp_suf  <- str_extract(resp_cols, "(stp|exam)$")

exam_cor <- vapply(seq_along(resp_cols), function(i) {
  if (resp_suf[i] != "stp") return(NA_real_)        # only _stp gets a value
  
  stp_col <- paste0(resp_code[i], "_stp")
  exam_col <- paste0(resp_code[i], "_exam")
  
  # which exam column(s) exist in gradedf1_wide
  exam_col <- intersect(c(exam_col), colnames(gradedf2_wide))
  
  # need the stp column present, and exactly one exam column
  if (!(stp_col %in% colnames(gradedf2_wide)) || length(exam_col) == 0)
    return(NA_real_)
  
  cor(gradedf2_wide[[stp_col]], gradedf2_wide[[exam_col]],
      use = "pairwise.complete.obs")
}, numeric(1))

names(exam_cor) <- resp_cols

grade2_desc["exam_cor", ] <- round(exam_cor[colnames(grade2_desc)],2)


openxlsx::write.xlsx(grade2_desc, "results/grade2_desc.xlsx", rowNames = T)




