source("scripts/00_settings.R")
Sys.setlocale("LC_ALL", "en_US.UTF-8") # for plots with norwegian characters


#loading in DFs
grades_exp1 <- readRDS("results/expected_grades/expected1.rds")
grades_exp2 <- readRDS("results/expected_grades/expected2_joint.rds")
grades_exp3 <- readRDS("results/expected_grades/expected3_joint.rds")

gradedf <- read.csv("data.temp/gradedf.csv")


#getting the names of subjects
included_subjects <- unique(sub("_.*", "", names(c(grades_exp1,grades_exp2,grades_exp3))))
fagkoder <- read.csv("data.temp/dir_fagkoder_ST.csv")
full_names <- fagkoder[fagkoder$code %in% included_subjects,]

#dropping subjects from full_name that we dont need
drop_subjects1 <-  c("ENG1007","GEO1003", "NAT1007", "NOR1260", "NOR1261", "SAK1001")
drop_subjects2  <- c("NOR1264", "NOR1265", "HIS1009", "KRO1018","REA3060")
drop_subjects3 <- c("NOR1267", "NOR1268", "NOR1269", "REL1003", "HIS1010", "KRO1019","REA3062")
drop_subjects <- c(drop_subjects1,drop_subjects2,drop_subjects3)

full_names <- full_names[!full_names$code %in% drop_subjects,]

#quick cleaning of gradedf, using it to calculate probability of being drawn to specific exams
gradedf <- gradedf[gradedf$fagkode %in% full_names$code,]

#making one exam column
gradedf$exam <- dplyr::coalesce(
  ifelse(gradedf$skr != "" & !is.na(gradedf$skr), gradedf$skr, NA),
  ifelse(gradedf$mun != "" & !is.na(gradedf$mun), gradedf$mun, NA),
  ifelse(gradedf$kar_annen != "" & !is.na(gradedf$kar_annen), gradedf$kar_annen, NA)
)



#simulation function
run_simulation <- function(
    n_sims = 10000,
    scenario = "min",               # "min" or "max"
    rea_vg1_subjects, rea_vg2_subjects, rea_vg3_subjects, #list of included subjects
    sse_vg1_subjects, sse_vg2_subjects, sse_vg3_subjects, # list of included subjects
    other_grade = 4, other_exam = 4
) {
  
  # function for computing exam probabilities. getting exam counts
  exam_counts <- gradedf %>% filter(!is.na(exam)) %>% count(year, fagkode)
  
  #the probabilities are normalized in respect to the included subjects so that the sum of them equals 1
  get_exam_probs <- function(year_val, subjects) {
    sub_counts <- exam_counts %>% filter(year == year_val, fagkode %in% subjects) # filter exam counts for the given year and subjects
    prob_vec <- setNames(rep(0, length(subjects)), subjects) # create a named vector with all subjects
    prob_vec[sub_counts$fagkode] <- sub_counts$n # the observed exam count for each subject
    prob_vec / sum(prob_vec) #  Convert counts to probabilities
  }
  
  #compute probabilities for the focal subjects in each year/track
  rea_vg2_probs <- get_exam_probs(2, rea_vg2_subjects)
  rea_vg3_probs <- get_exam_probs(3, rea_vg3_subjects)
  sse_vg2_probs <- get_exam_probs(2, sse_vg2_subjects)
  sse_vg3_probs <- get_exam_probs(3, sse_vg3_subjects)
  
  # STP means for focal subjects
  stp_mean <- function(subjects, grades_exp) {
    vals <- sapply(subjects, function(x) mean(grades_exp[[paste0(x, "_stp")]], na.rm = TRUE))
    mean(vals, na.rm = TRUE)
  }
  rea_vg1_stp <- stp_mean(rea_vg1_subjects, grades_exp1)
  rea_vg2_stp <- stp_mean(rea_vg2_subjects, grades_exp2)
  rea_vg3_stp <- stp_mean(rea_vg3_subjects, grades_exp3)
  sse_vg1_stp <- stp_mean(sse_vg1_subjects, grades_exp1)
  sse_vg2_stp <- stp_mean(sse_vg2_subjects, grades_exp2)
  sse_vg3_stp <- stp_mean(sse_vg3_subjects, grades_exp3)
  
  # Exam means for focal subjects
  exam_mean_vec <- function(subjects, grades_exp) {
    vals <- sapply(subjects, function(x) mean(grades_exp[[paste0(x, "_exam")]], na.rm = TRUE))
    names(vals) <- subjects
    vals
  }
  rea_vg2_exam_means <- exam_mean_vec(rea_vg2_subjects, grades_exp2)
  rea_vg3_exam_means <- exam_mean_vec(rea_vg3_subjects, grades_exp3)
  sse_vg2_exam_means <- exam_mean_vec(sse_vg2_subjects, grades_exp2)
  sse_vg3_exam_means <- exam_mean_vec(sse_vg3_subjects, grades_exp3)
  
  # initialize results df
  results <- data.frame(
    gpa_rea_vg1 = numeric(n_sims), gpa_sse_vg1 = numeric(n_sims),
    gpa_rea_vg2 = numeric(n_sims), gpa_sse_vg2 = numeric(n_sims),
    gpa_rea_vg3 = numeric(n_sims), gpa_sse_vg3 = numeric(n_sims),
    gpa_rea_total = numeric(n_sims), gpa_sse_total = numeric(n_sims),
    rea_exam_subjects = character(n_sims),  
    sse_exam_subjects = character(n_sims)  
  )
  
  #simulation loop
  for(i in 1:n_sims) {
    # sample n focal exams per year per scenario with the computed probabilities, and indexing the mean exam grades
    if (scenario == "min") {
      rea_exam <- sample(names(rea_vg3_probs), 1, prob = rea_vg3_probs)
      sse_exam <- sample(names(sse_vg3_probs), 1, prob = sse_vg3_probs)
      rea_exam_mean <- rea_vg3_exam_means[rea_exam]
      sse_exam_mean <- sse_vg3_exam_means[sse_exam]
      
      # save subject codes
      rea_exam_subjects <- rea_exam
      sse_exam_subjects <- sse_exam
      
    } else { # max
      rea_exam_vg2 <- sample(names(rea_vg2_probs), 1, prob = rea_vg2_probs)
      sse_exam_vg2 <- sample(names(sse_vg2_probs), 1, prob = sse_vg2_probs)
      rea_exams_vg3 <- sample(names(rea_vg3_probs), 2, prob = rea_vg3_probs)
      sse_exams_vg3 <- sample(names(sse_vg3_probs), 2, prob = sse_vg3_probs)
      
      rea_exam_mean_vg2 <- rea_vg2_exam_means[rea_exam_vg2]
      sse_exam_mean_vg2 <- sse_vg2_exam_means[sse_exam_vg2]
      rea_exam_mean_vg3 <- mean(rea_vg3_exam_means[rea_exams_vg3])
      sse_exam_mean_vg3 <- mean(sse_vg3_exam_means[sse_exams_vg3])
      
      # Combine subject codes into a single string
      rea_exam_subjects <- paste(c(rea_exam_vg2, rea_exams_vg3), collapse = ", ")
      sse_exam_subjects <- paste(c(sse_exam_vg2, sse_exams_vg3), collapse = ", ")
    }
    
    # Year 1: 1 focal stp + 4 other stp = 5
    results$gpa_rea_vg1[i] <- (rea_vg1_stp + other_grade * 4) / 5
    results$gpa_sse_vg1[i] <- (sse_vg1_stp + other_grade * 4) / 5
    
    #year 2
    if (scenario == "min") {
      #minimum: 4 focal stp + 1 other stp + 1 other exam = 6
      results$gpa_rea_vg2[i] <- (rea_vg2_stp * 4 + other_grade * 1  + other_exam) / 6
      results$gpa_sse_vg2[i] <- (sse_vg2_stp * 4 + other_grade * 1 + other_exam) / 6
    } else {
      #maximum: 4 focal stp + 1 focal exam + 1 other grade = 6
      results$gpa_rea_vg2[i] <- (rea_vg2_stp * 4 + rea_exam_mean_vg2 + other_grade * 1) / 6
      results$gpa_sse_vg2[i] <- (sse_vg2_stp * 4 + sse_exam_mean_vg2 + other_grade * 1) / 6
    }
    
    # year 3
    if (scenario == "min") {
      #minimum: 3 focal stp + 1 focal exam + 6 other stp + 3 other exam = 13
      results$gpa_rea_vg3[i] <- (rea_vg3_stp * 3 + rea_exam_mean + other_grade * 6 + other_exam * 3) / 13
      results$gpa_sse_vg3[i] <- (sse_vg3_stp * 3 + sse_exam_mean + other_grade * 6 + other_exam * 3) / 13
    } else {
      #maximum: 3 focal stp + 2 focal exam + 6 other stp + 2 other exam = 13
      results$gpa_rea_vg3[i] <- (rea_vg3_stp * 3 + rea_exam_mean_vg3 * 2 + other_grade * 6 + other_exam * 2) / 13
      results$gpa_sse_vg3[i] <- (sse_vg3_stp * 3 + sse_exam_mean_vg3 * 2 + other_grade * 6 + other_exam * 2) / 13
    }
    
    # final GPA based on number of grades, VG1 = 5, VG2 = 6, VG3 = 13
    results$gpa_rea_total[i] <- (results$gpa_rea_vg1[i] * 5 + results$gpa_rea_vg2[i] * 6 + results$gpa_rea_vg3[i] * 13) / 24
    results$gpa_sse_total[i] <- (results$gpa_sse_vg1[i] * 5 + results$gpa_sse_vg2[i] * 6 + results$gpa_sse_vg3[i] * 13) / 24
    
    #the difference between the gpa of rea and sse tracks
    results$gpa_diff[i] <- results$gpa_rea_total[i]-results$gpa_sse_total[i]
    
    # store the exam subject strings
    results$rea_exam_subjects[i] <- rea_exam_subjects
    results$sse_exam_subjects[i] <- sse_exam_subjects
  }
  
  # Return results
  results
}



##### FULL REA TRACK #####

#awards 4 realfagspoeng: R maths x2 = 1.5, Physics x2 = 1.5, chemistry x2 = 1, biology = 0.5. max is 4

rea_vg1 <- c("MAT1021") #1T mat
rea_vg2 <- c("REA3056", "REA3038", "REA3045", "REA3035") #R1 mat, Fys1, Kjem1, Bio1
rea_vg3 <- c("REA3058", "REA3039", "REA3046") # R2 mat, Fys2, Kjem2

sse_vg1 <- c("MAT1019") #1P mat
sse_vg2 <- c("MAT1023", "SAM3054", "SAM3057", "SAM3072") # 2P mat, sos & sos, rett1, Psy1
sse_vg3 <- c("SAM3055", "SAM3073", "SAM3058") #  Pol, Psy2, Rett2

# Run min scenario
full_rea_min <- run_simulation(
  n_sims = 10000,
  scenario = "min",
  rea_vg1_subjects = rea_vg1,
  rea_vg2_subjects = rea_vg2,
  rea_vg3_subjects = rea_vg3,
  sse_vg1_subjects = sse_vg1,
  sse_vg2_subjects = sse_vg2,
  sse_vg3_subjects = sse_vg3
)

# Run max scenario
full_rea_max <- run_simulation(
  n_sims = 10000,
  scenario = "max",
  rea_vg1_subjects = rea_vg1,
  rea_vg2_subjects = rea_vg2,
  rea_vg3_subjects = rea_vg3,
  sse_vg1_subjects = sse_vg1,
  sse_vg2_subjects = sse_vg2,
  sse_vg3_subjects = sse_vg3
)


##### MINIMUM REA TRACK #####
#awards 3 realfagspoeng: R maths x2 = 1.5, Physics x2 = 1.5.


rea_vg1 <- c("MAT1021") #1T mat
rea_vg2 <- c("REA3056", "REA3038", "SAM3072", "SAM3057") #R1 mat, Fys1, psy1, #rett1
rea_vg3 <- c("REA3058", "REA3039", "SAM3073") # R2 mat, Fys2, ps2

sse_vg1 <- c("MAT1019") #1P mat
sse_vg2 <- c("MAT1023", "SAM3054", "SAM3057", "SAM3072") # 2P mat, Sos, rett1, Psy1
sse_vg3 <- c("SAM3055", "SAM3073", "SAM3058") #  Pol, Psy2, Rett2

# Run min scenario
min_rea_min <- run_simulation(
  n_sims = 10000,
  scenario = "min",
  rea_vg1_subjects = rea_vg1,
  rea_vg2_subjects = rea_vg2,
  rea_vg3_subjects = rea_vg3,
  sse_vg1_subjects = sse_vg1,
  sse_vg2_subjects = sse_vg2,
  sse_vg3_subjects = sse_vg3
)

# Run max scenario
min_rea_max <- run_simulation(
  n_sims = 10000,
  scenario = "max",
  rea_vg1_subjects = rea_vg1,
  rea_vg2_subjects = rea_vg2,
  rea_vg3_subjects = rea_vg3,
  sse_vg1_subjects = sse_vg1,
  sse_vg2_subjects = sse_vg2,
  sse_vg3_subjects = sse_vg3
)



##### ADMISSION REQUIREMENTS TRACK #####

#awards 3 realfagspoeng: R maths x2 = 1.5, Physics 1 = 0.5, chemistry x2 = 1,

rea_vg1 <- c("MAT1021") #1T mat
rea_vg2 <- c("REA3056", "REA3038", "REA3045","SAM3072") #R1 mat, Fys1, Kjem1, Psy1
rea_vg3 <- c("REA3058", "REA3046","SAM3073") # R2 mat, Kjem2, psy2

sse_vg1 <- c("MAT1019") #1P mat
sse_vg2 <- c("MAT1023", "SAM3054", "SAM3057", "SAM3072") # 2P mat, sos & sos & sos, rett1, Psy1
sse_vg3 <- c("SAM3055", "SAM3073", "SAM3058") #  Pol, Psy2, Rett2

# Run min scenario
admission_min <- run_simulation(
  n_sims = 10000,
  scenario = "min",
  rea_vg1_subjects = rea_vg1,
  rea_vg2_subjects = rea_vg2,
  rea_vg3_subjects = rea_vg3,
  sse_vg1_subjects = sse_vg1,
  sse_vg2_subjects = sse_vg2,
  sse_vg3_subjects = sse_vg3
)

# Run max scenario
admission_max <- run_simulation(
  n_sims = 10000,
  scenario = "max",
  rea_vg1_subjects = rea_vg1,
  rea_vg2_subjects = rea_vg2,
  rea_vg3_subjects = rea_vg3,
  sse_vg1_subjects = sse_vg1,
  sse_vg2_subjects = sse_vg2,
  sse_vg3_subjects = sse_vg3
)


##### plot #####

make_diff_df <- function(df, course_label, scenario_label) {
  data.frame(
    course = course_label,
    scenario = scenario_label,
    diff = df$gpa_rea_total - df$gpa_sse_total
  )
}


diff_df <- rbind(
  make_diff_df(full_rea_min, "Fullt realfagsløp", "Min"),
  make_diff_df(full_rea_max, "Fullt realfagsløp", "Max"),
  make_diff_df(min_rea_min, "Minimum realfagsløp", "Min"),
  make_diff_df(min_rea_max, "Minimum realfagsløp", "Max"),
  make_diff_df(admission_min, "Opptakskravsløp", "Min"),
  make_diff_df(admission_max, "Opptakskravsløp", "Max")
)

diff_df$course <- factor(diff_df$course,
                         levels = c("Fullt realfagsløp", "Minimum realfagsløp", "Opptakskravsløp"))
diff_df$scenario <- factor(diff_df$scenario, levels = c("Min", "Max"))

summary_df <- diff_df %>%
  group_by(course, scenario) %>%
  summarise(
    mean_diff = mean(diff),
    low = min(diff),
    high = max(diff)
  ) %>%
  ungroup()

summary_df$vjust_adj <- -1.5
summary_df$vjust_adj[summary_df$course == "Fullt realfagsløp" & summary_df$scenario == "Min"] <- -2.5



#Plot

pd <- position_dodge(width = 0.5)

GPA_disadvanatge <- ggplot(diff_df, aes(x = course, y = diff, fill = scenario)) +
  geom_violin(position = pd, width = 0.5, alpha = 0.6, color = NA) +
  geom_linerange(data = summary_df,
                 aes(x = course, y = mean_diff, ymin = low, ymax = high,
                     group = scenario),
                 position = pd, size = 0.3, color = "grey30",
                 inherit.aes = FALSE) +
  geom_point(data = summary_df,
             aes(x = course, y = mean_diff, group = scenario),
             position = pd, size = 3, shape = 21, stroke = 0.3,
             fill = "black", color = "black",
             inherit.aes = FALSE) +
  geom_text(data = summary_df,
            aes(x = course, y = mean_diff, group = scenario,
                label = gsub("\\.", ",", sprintf("%.2f", mean_diff)),
                vjust = vjust_adj),
            position = pd, size = 3.5, family = "serif",
            inherit.aes = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c("Min" = "#2a6f97", "Max" = "#e02941"),
                    name   = "Antall eksamener fra løpet",
                    labels = c("Min" = "Én", "Max" = "Tre")) +
  labs(x = "",
       y = "Karakterulempe i forhold til SSØ-løpet") +
  scale_y_continuous(
    limits = c(-0.5, -0.1),                 # set the displayed range
    breaks = seq(-0.5, -0.1, by = 0.1),
    labels = function(x) gsub("\\.", ",", sprintf("%.1f", x))
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    axis.text.x        = element_text(size = 9, colour = "black"),
    axis.text.y        = element_text(size = 12, colour = "black"),
    axis.title.x       = element_text(size = 11, colour = "black"),
    axis.title.y       = element_text(colour = "black"),
    panel.grid.major.x = element_line(colour = "grey85"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA),
    legend.position    = "bottom",
    legend.key.size    = unit(0.8, "lines"),
    legend.text        = element_text(size = 10, colour = "black"),
    legend.title       = element_text(size = 11, colour = "black")
  )

ggsave("plots/GPA_disadvantage.tiff", 
       plot = GPA_disadvanatge,
       width = 180,
       height = 140,
       units = "mm",
       dpi = 600,
       compression = "lzw")


#### OPPTAKSPOENG ####


#calculating opptakspoeng with STEM points. the full stem run gets 4/2 points, the others get 3/1.5
transformed_df <- diff_df %>%
  mutate(
    opptak_old = diff * 10 + case_when(course == "Fullt realfagsløp" ~ 4, TRUE ~ 3),
    opptak_new = diff * 10 + case_when(course == "Fullt realfagsløp" ~ 2, TRUE ~ 1.5)
  )

summarize_opptak <- function(df, value_col) {
  df %>%
    group_by(course, scenario) %>%
    summarise(
      mean = mean(.data[[value_col]]),
      low = min(.data[[value_col]]),
      high = max(.data[[value_col]])
    ) %>%
    ungroup() %>%
    mutate(vjust_adj = ifelse(course == "Fullt realfagsløp" & scenario == "Min", -2.5, -1.5))
}

summary_old <- summarize_opptak(transformed_df, "opptak_old")
summary_new <- summarize_opptak(transformed_df, "opptak_new")

# equal-width windows for the two panels, each sitting over its own data.
# same span means one point is the same physical distance in both.
range_old <- range(c(summary_old$low, summary_old$high, 0))
range_new <- range(c(summary_new$low, summary_new$high, 0))
span      <- max(diff(range_old), diff(range_new)) + 0.4

window <- function(r, span) {
  mid <- mean(r)
  c(mid - span / 2, mid + span / 2)
}

lim_old <- window(range_old, span)
lim_new <- window(range_new, span)

# plotting function
make_plot <- function(data_df, summary_df, value_col, y_title, show_y_labels, limits) {
  pd <- position_dodge(width = 0.8)
  
  p <- ggplot(data_df, aes(x = course, y = .data[[value_col]], fill = scenario)) +
    geom_violin(position = pd, width = 0.8, alpha = 0.6, color = NA) +
    geom_linerange(data = summary_df,
                   aes(x = course, y = mean, ymin = low, ymax = high, group = scenario),
                   position = pd, linewidth = 0.3, color = "grey30", inherit.aes = FALSE) +
    geom_point(data = summary_df,
               aes(x = course, y = mean, group = scenario),
               position = pd, size = 2.5, shape = 21, stroke = 0.3,
               fill = "black", color = "black", inherit.aes = FALSE) +
    geom_text(data = summary_df,
              aes(x = course, y = mean, group = scenario,
                  label = gsub("\\.", ",", sprintf("%.2f", mean)),
                  vjust = vjust_adj),
              position = pd, size = 3.5, family = "serif", inherit.aes = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey20", linewidth = 0.5) +
    coord_flip(clip = "off") +
    scale_fill_manual(values = c("Min" = "#2a6f97", "Max" = "#e02941"),
                      name = "Antall eksamener fra løpet",
                      labels = c("Min" = "Én", "Max" = "Tre")) +
    scale_y_continuous(
      limits = limits,
      breaks = seq(-3, 3, by = 0.5),
      expand = expansion(mult = c(0.01, 0.01)),
      labels = function(x) gsub("\\.", ",", sprintf("%.1f", x))
    ) +
    labs(x = "", y = y_title) +
    theme_minimal(base_family = "serif") +
    theme(
      axis.text.x = element_text(size = 9, colour = "black"),
      axis.text.y = if (show_y_labels) element_text(size = 11, colour = "black") else element_blank(),
      axis.ticks.y = if (show_y_labels) element_line() else element_blank(),
      axis.title.x = element_text(size = 11, colour = "black"),
      axis.title.y = element_text(colour = "black"),
      panel.grid.major.x = element_line(colour = "grey85"),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(5.5, 10, 5.5, 5.5),
      legend.position = "bottom",
      legend.key.size = unit(0.6, "lines"),
      legend.text = element_text(size = 9, colour = "black"),
      legend.title = element_text(size = 10, colour = "black")
    )
  
  # if show_y_labels is FALSE, remove y-axis text and ticks
  if (!show_y_labels) {
    p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }
  p
}

# build the two plots with headers
plot_old <- make_plot(transformed_df, summary_old, "opptak_old",
                      y_title = "Differanse i skolepoeng", show_y_labels = TRUE,
                      limits = lim_old) +
  ggtitle("Dagens ordning") +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5, family = "serif"),
        plot.margin = margin(5.5, 10, 5.5, 5.5))

plot_new <- make_plot(transformed_df, summary_new, "opptak_new",
                      y_title = "Differanse i skolepoeng", show_y_labels = FALSE,
                      limits = lim_new) +
  ggtitle("Ordningen fra 2028") +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5, family = "serif"),
        plot.margin = margin(5.5, 5.5, 5.5, 10))

combined <- plot_old + plot_new +
  plot_layout(ncol = 2, widths = c(1, 1), guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom"))

ggsave("plots/poeng_disadvantage.tiff",
       plot = combined,
       width = 180,
       height = 120,
       units = "mm",
       dpi = 600,
       compression = "lzw")
