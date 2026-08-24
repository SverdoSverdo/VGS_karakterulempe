source("scripts/00_settings.R")


gradedf <- fread("data.temp/gradedf.csv", data.table = F)

        #### VG2 ####

gradedf2 <- gradedf[gradedf$year == 2,]

# keep only id, timevar, and the grade column
gradedf2_wide <- gradedf2[c("w19_0634_lnr", "fagkode", "stp")]


# reshape wide; default names come out as stp.fagkode etc.
gradedf2_wide <- reshape(gradedf2_wide, dir = "wide",
                         idvar = "w19_0634_lnr",
                         timevar = "fagkode")


colnames(gradedf2_wide) <- gsub("^stp\\.", "", colnames(gradedf2_wide))


xtable_student.temp <- gradedf2_wide

#Did student i enroll in subject n? cell gets a 1 if it is not NA, and a 0 if it is NA
for(i in 2:ncol(xtable_student.temp))xtable_student.temp[i][!is.na(xtable_student.temp[i])] <- 1
for(i in 2:ncol(xtable_student.temp))xtable_student.temp[i][is.na(xtable_student.temp[i])] <- 0

#did the school offer the subject? extracting school-level data
xtable_school.temp <- gradedf2[,c("lnr_org","fagkode","stp")]
xtable_school.temp <- xtable_school.temp[!xtable_school.temp$lnr_org =="",] #removing NA School IDs

# A school gets a 1 if the course is offered in the offered column
xtable_school.temp <- xtable_school.temp %>%
  group_by(lnr_org, fagkode) %>%
  summarise(offered = as.integer(any(stp != "")), .groups = "drop")

xtable_school.temp <- as.data.frame(xtable_school.temp)
xtable_school.temp$fagkode <- as.character(xtable_school.temp$fagkode)

#reshaping
xtable_school.wide <- reshape(xtable_school.temp, dir = "wide", sep = "",
                               idvar = "lnr_org", timevar = "fagkode",
                               v.names = "offered")
colnames(xtable_school.wide) <- gsub("^offered", "", colnames(xtable_school.wide))


#100 if the school offered subjects, 10 if it does not
for(i in 2:ncol(xtable_school.wide))xtable_school.wide[i][!is.na(xtable_school.wide[i])] <- 100
for(i in 2:ncol(xtable_school.wide))xtable_school.wide[i][is.na(xtable_school.wide[i])] <- 10

#creating DF with students and school IDs
student_school <- gradedf2
student_school <- student_school[,c("lnr_org","w19_0634_lnr")]
student_school$lnr_org[student_school$lnr_org == ""] <- NA
student_school <- student_school[order(is.na(student_school$lnr_org)), ]
student_school <- student_school[!duplicated(student_school$w19_0634_lnr), ] # keep non-na lnr_org when available
length(unique(student_school$lnr_org))# 306 schools are included in the study


student_df <- xtable_student.temp
school_df <- xtable_school.wide

#connecting student IDs and school IDs for the school DF
school_df<- left_join(student_school,school_df, by = "lnr_org")
school_df <- school_df[!is.na(school_df$lnr_org),]
#connecting student IDs and school IDs for the student DF
student_df <- left_join(student_school,student_df,by = "w19_0634_lnr")
student_df <- student_df[!is.na(student_df$lnr_org),]

for(i in 3:ncol(student_df))student_df[,i] <- as.numeric(student_df[,i])
for(i in 3:ncol(school_df))school_df[,i] <- as.numeric(school_df[,i])
school_df <- school_df[names(student_df)]# reordering so columns match


#adding the DFs together. ensuring the IDs are in the same row
student_df <- student_df[order(student_df$w19_0634_lnr), ]
school_df <- school_df[order(school_df$w19_0634_lnr), ]

# check ids match exactly before adding
identical(student_df$w19_0634_lnr, school_df$w19_0634_lnr)

choicedf <- cbind(student_df$w19_0634_lnr, student_df[,3:ncol(student_df)] + school_df[,3:ncol(school_df)])
names(choicedf)[1] <- "w19_0634_lnr"

table(unlist(choicedf[ , -1]), useNA = "ifany")
# 100 = course offered, not taken
# 101 = course offered, and taken
# 10 = course not offered, course not taken
# 11 = course not offered, course taken; students with grades from multiple high schools. only a few, we set those to 1.
for(i in 2:ncol(choicedf))choicedf[i][choicedf[i] == 101] <- 1
for(i in 2:ncol(choicedf))choicedf[i][choicedf[i] == 11] <- 1 
for(i in 2:ncol(choicedf))choicedf[i][choicedf[i] == 100] <- 0
for(i in 2:ncol(choicedf))choicedf[i][choicedf[i] == 10] <- NA

write.csv(choicedf, file = "data.temp/choicedf2.csv", row.names = F)



        #### VG3 ####

gradedf3 <- gradedf[gradedf$year == 3,]

# keep only id, timevar, and the grade column
gradedf3_wide <- gradedf3[c("w19_0634_lnr", "fagkode", "stp")]


# reshape wide; default names come out as stp.fagkode etc.
gradedf3_wide <- reshape(gradedf3_wide, dir = "wide",
                         idvar = "w19_0634_lnr",
                         timevar = "fagkode")

colnames(gradedf3_wide) <- gsub("^stp\\.", "", colnames(gradedf3_wide))


xtable_student.temp <- gradedf3_wide

#Did student i enroll in subject n? cell gets a 1 if it is not NA, and a 0 if it is NA
for(i in 2:ncol(xtable_student.temp))xtable_student.temp[i][!is.na(xtable_student.temp[i])] <- 1
for(i in 2:ncol(xtable_student.temp))xtable_student.temp[i][is.na(xtable_student.temp[i])] <- 0

#did the school offer the subject? extracting school-level data
xtable_school.temp <- gradedf3[,c("lnr_org","fagkode","stp")]
xtable_school.temp <- xtable_school.temp[!xtable_school.temp$lnr_org =="",] #removing NA School IDs

# A school gets a 1 if the course is offered in the offered column
xtable_school.temp <- xtable_school.temp %>%
  group_by(lnr_org, fagkode) %>%
  summarise(offered = as.integer(any(stp != "")), .groups = "drop")

xtable_school.temp <- as.data.frame(xtable_school.temp)
xtable_school.temp$fagkode <- as.character(xtable_school.temp$fagkode)

#reshaping
xtable_school.wide <- reshape(xtable_school.temp, dir = "wide", sep = "",
                              idvar = "lnr_org", timevar = "fagkode",
                              v.names = "offered")
colnames(xtable_school.wide) <- gsub("^offered", "", colnames(xtable_school.wide))


#100 if the school offered subjects, 10 if it does not
for(i in 2:ncol(xtable_school.wide))xtable_school.wide[i][!is.na(xtable_school.wide[i])] <- 100
for(i in 2:ncol(xtable_school.wide))xtable_school.wide[i][is.na(xtable_school.wide[i])] <- 10

#creating DF with students and school IDs
student_school <- gradedf3
student_school <- student_school[,c("lnr_org","w19_0634_lnr")]
student_school$lnr_org[student_school$lnr_org == ""] <- NA
student_school <- student_school[order(is.na(student_school$lnr_org)), ]
student_school <- student_school[!duplicated(student_school$w19_0634_lnr), ] # keep non-na lnr_org when available
length(unique(student_school$lnr_org))# 303 schools are included in the study

student_df <- xtable_student.temp
school_df <- xtable_school.wide

#connecting student IDs and school IDs for the school DF
school_df<- left_join(student_school,school_df, by = "lnr_org")
school_df <- school_df[!is.na(school_df$lnr_org),]
#connecting student IDs and school IDs for the student DF
student_df <- left_join(student_school,student_df,by = "w19_0634_lnr")
student_df <- student_df[!is.na(student_df$lnr_org),]

for(i in 3:ncol(student_df))student_df[,i] <- as.numeric(student_df[,i])
for(i in 3:ncol(school_df))school_df[,i] <- as.numeric(school_df[,i])
school_df <- school_df[names(student_df)]# reordering so columns match


#adding the DFs together. ensuring the IDs are in the same row
student_df <- student_df[order(student_df$w19_0634_lnr), ]
school_df <- school_df[order(school_df$w19_0634_lnr), ]

# check ids match exactly before adding
identical(student_df$w19_0634_lnr, school_df$w19_0634_lnr)

choicedf <- cbind(student_df$w19_0634_lnr, student_df[,3:ncol(student_df)] + school_df[,3:ncol(school_df)])
names(choicedf)[1] <- "w19_0634_lnr"

table(unlist(choicedf[ , -1]), useNA = "ifany")
# 100 = course offered, not taken
# 101 = course offered, and taken
# 10 = course not offered, course not taken
# 11 = course not offered, course taken; students with grades from multiple high schools. only a few, we set those to 1.
for(i in 2:ncol(choicedf))choicedf[i][choicedf[i] == 101] <- 1
for(i in 2:ncol(choicedf))choicedf[i][choicedf[i] == 11] <- 1 
for(i in 2:ncol(choicedf))choicedf[i][choicedf[i] == 100] <- 0
for(i in 2:ncol(choicedf))choicedf[i][choicedf[i] == 10] <- NA

write.csv(choicedf, file = "data.temp/choicedf3.csv", row.names = F)

