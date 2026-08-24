source("scripts/00_settings.R")

#### 1. PREPARING DF WITH GRADES ####

#dataset with grades, indexed as EDUCATION_TAB_KAR_VG in NUDB, needs to contain columns:
# w19_0634_lnr, stp, fagkode, fagstatus, termin2, mun, skr, kar_annen, lnr_org
data <- fread("directory_to_data/EDUCATION_TAB_KAR_VG.csv", data.table = F)
names(data) <- tolower(names(data))

data <- data[data$skolear == 20232024,] # only selecting year 20232024
data <- data[!duplicated(data),] #removing duplicate rows

table(data$fagstatus)

#we include those with elevstatus (E) and those exemped from grading(F)
legit_fagstatus <- c("E","F")

data <- data[data$fagstatus %in% legit_fagstatus,] 

# removing unnecessary columns, keeping termin2 for the post-dedup fill
gradedf <- data[c("w19_0634_lnr","stp","fagkode","fagstatus","termin2","mun","skr","kar_annen","lnr_org")]

# 50950 duplicated rows to start
key <- paste(gradedf$w19_0634_lnr, gradedf$fagkode)
sum(duplicated(key) | duplicated(key, fromLast = TRUE))

# first select fagkode/ID rows where fagstatus == E
key <- paste(gradedf$w19_0634_lnr, gradedf$fagkode)
is_dup <- duplicated(key) | duplicated(key, fromLast = TRUE)
has_e <- ave(gradedf$fagstatus == "E", key, FUN = any)
gradedf <- gradedf[!(is_dup & has_e & gradedf$fagstatus != "E"), ]

# 6971 duplicates left
key <- paste(gradedf$w19_0634_lnr, gradedf$fagkode)
sum(duplicated(key) | duplicated(key, fromLast = TRUE))

# we then select fagkode/ID rows where stp is non-NA
key <- paste(gradedf$w19_0634_lnr, gradedf$fagkode)
is_dup <- duplicated(key) | duplicated(key, fromLast = TRUE)
has_val <- ave(gradedf$stp != "", key, FUN = any)
gradedf <- gradedf[!(is_dup & has_val & gradedf$stp == ""), ]

# 5129 duplicated rows left. virtually all of these are NA values with fagstatus_kode == S, meaning they have a double registration of them quitting the subject
key <- paste(gradedf$w19_0634_lnr, gradedf$fagkode)
sum(duplicated(key) | duplicated(key, fromLast = TRUE))

# we prioritize rows with the highest grade
has_orig <- gradedf$stp != ""
stp_num <- suppressWarnings(as.numeric(gradedf$stp))
gradedf <- gradedf[order(gradedf$w19_0634_lnr, gradedf$fagkode,
                         !has_orig, -stp_num, na.last = TRUE), ]
gradedf <- gradedf[!duplicated(gradedf[c("w19_0634_lnr","fagkode")]), ]

# 0 duplicates left
key <- paste(gradedf$w19_0634_lnr, gradedf$fagkode)
sum(duplicated(key) | duplicated(key, fromLast = TRUE))

# Some times, end of the term grades were listed as 2nd term grades. Hence, 2nd term grades are moved to STP 
gradedf$stp[gradedf$stp == ""] <- gradedf$termin2[gradedf$stp == ""]


#### 2. IDENTIIFYING STUDY PROGRAM ####


#dataset with course codes, indexed  F_UTD_KURS in NUDB, needs to contain columns:
# w19_0634_lnr, KTRINNDATO, FKURSKOD, KTRINN, GYLDIG_TV_FOM
kurs <- fread("directory_to_data/F_UTD_KURS.csv", data.table = F)
kurs <- kurs[kurs$w19_0634_lnr %in% gradedf$w19_0634_lnr,]

# we want to know what class they were in during the period 202308 to 202312. we recode the fall semester of 2023 to 2023
kurs$KTRINNDATO[kurs$KTRINNDATO >= 202308 & kurs$KTRINNDATO <= 202312] <- 2023
kurs <- kurs[kurs$KTRINNDATO == 2023, ] # only need 2023

# base programomrC%dekode (first 6 chars) and the trailing year-digit
kurs$fkurs6 <- substr(kurs$FKURSKOD, 1, 6)

# code sets per year
codes_1 <- c("STREA1", "STSSA1", "STUSP1")
codes_2 <- c("STREA2", "STSSA2", "STUSP2")
codes_3 <- c("STREA3", "STSSA3", "STUSP3")
all_codes <- unique(c(codes_1, codes_2, codes_3))

# rows in 2023 carrying one of the relevant codes
kurs <- kurs[kurs$KTRINNDATO == 2023 & kurs$fkurs6 %in% all_codes, ]
kurs$ydigit <- substr(kurs$fkurs6, 6, 6)   # last char = trinn number

#Does anyone have conflicting years in their codes?

# per person, how many distinct year-digits do they carry?
ndig <- tapply(kurs$ydigit, kurs$w19_0634_lnr, function(x) length(unique(x)))

# the ones that actually matter: registered across different grade years
bad_year <- names(ndig)[ndig > 1]
cat("ids with conflicting year-digits (registered in 2 grades at once):",
    length(bad_year), "\n")


# remove these ids entirely from kurs
kurs <- kurs[!(kurs$w19_0634_lnr %in% bad_year), ]

## Which people have conflictin codes??
ncode <- tapply(kurs$fkurs6, kurs$w19_0634_lnr, function(x) length(unique(x)))

# anyone with more than one code (any conflict, not just cross-year)
bad_code <- names(ncode)[ncode > 1]
cat("ids with any conflicting codes:", length(bad_code), "\n")

# look at them, same columns as conflicts, sorted by id
conflicts_code <- kurs[kurs$w19_0634_lnr %in% bad_code,
                       c("w19_0634_lnr", "fkurs6", "ydigit", "KTRINN",
                         "GYLDIG_TV_FOM", "FKURSKOD")]
conflicts_code <- conflicts_code[order(conflicts_code$w19_0634_lnr), ]

# per id: the program code if unique, else "unknown<year-digit>"
# (cross-year conflicts already removed, so ydigit is constant within id)
prog <- tapply(seq_len(nrow(kurs)), kurs$w19_0634_lnr, function(i) {
  codes <- kurs$fkurs6[i]
  if (length(unique(codes)) == 1) {
    codes[1]                                  # single program -> keep it
  } else {
    paste0("unknown", kurs$ydigit[i][1])      # conflicting codes -> unknown<year>
  }
})

kurs <- data.frame(
  w19_0634_lnr = names(prog),
  program = unname(prog),
  stringsAsFactors = FALSE
)

gradedf <- left_join(gradedf, kurs, by = "w19_0634_lnr")


# Students with these unknown program status take subjects similar to general student population
unknown2 <- gradedf[gradedf$program == "unknown2",]
  sort(table(unknown2$fagkode))
  
unknown2 <- gradedf[gradedf$program == "unknown2",]
  sort(table(unknown2$fagkode))
  
  
#People with STUSP2 and 3 are international students I believe. A lot of "IBA...." codes
STUSP2 <- gradedf[gradedf$program == "STUSP2",]
sort(table(STUSP2$fagkode))

STUSP3 <- gradedf[gradedf$program == "STUSP3",]
sort(table(STUSP3$fagkode))

#we remove these students from the gradedf
gradedf <- gradedf[!(gradedf$program == "STUSP2" | gradedf$program == "STUSP3"), ]

##There are a few unknowns, inferring from subejct choice what specialization they are in
table(gradedf$program)

# #In 2nd grade, students may take these two courses as part of fellesfag, so we ignore them
ignore_rea <- c("REA3060", "REA3056")

# per id: count distinct REA fagkoder, excluding S and R math
rea_count <- tapply(gradedf$fagkode, gradedf$w19_0634_lnr, function(x) {
  rea <- unique(x[startsWith(x, "REA") & !x %in% ignore_rea])
  length(rea)
})


# ids that have 2+ qualifying REA subjects -> realfag track
many_rea <- names(rea_count)[rea_count >= 2]
hit <- gradedf$w19_0634_lnr %in% many_rea

# unknown2 -> STREA2 if 2+ REA, else STSSA2
gradedf$program[gradedf$program == "unknown2" &  hit] <- "STREA2"
gradedf$program[gradedf$program == "unknown2" & !hit] <- "STSSA2"

# unknown3 -> STREA3 if 2+ REA, else STSSA3
gradedf$program[gradedf$program == "unknown3" &  hit] <- "STREA3"
gradedf$program[gradedf$program == "unknown3" & !hit] <- "STSSA3"

#just change unknown1 to STUSP1
gradedf$program[gradedf$program == "unknown1"] <- "STUSP1"

table(gradedf$program)

#make year column
gradedf$year <- as.numeric(substr(gradedf$program, nchar(gradedf$program), nchar(gradedf$program)))

gradedf <- gradedf[!is.na(gradedf$w19_0634_lnr),]

write.csv(gradedf, "data.temp/gradedf.csv", row.names = F)

