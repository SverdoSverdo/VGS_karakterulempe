source("00_settings.R")
Sys.setlocale("LC_ALL", "en_US.UTF-8") # for plots with norwegian characters 


#### PREPARING DFs ####

grades_exp1 <- readRDS("results/expected_grades/expected1.rds")
grades_obs1 <- read.csv("data.temp/gradedf1_wide.csv")

grades_exp2 <- readRDS("results/expected_grades/expected2_joint.rds")
grades_obs2 <- read.csv("data.temp/gradedf2_wide.csv")

grades_exp3 <- readRDS("results/expected_grades/expected3_joint.rds")
grades_obs3 <- read.csv("data.temp/gradedf3_wide.csv")

#description of subjects
included_subjects <- unique(sub("_.*", "", names(c(grades_exp1,grades_exp2,grades_exp3))))
fagkoder <- read.csv("data.temp/dir_fagkoder_ST.csv")
full_names <- fagkoder[fagkoder$code %in% included_subjects,]

#dropping subjects from full_name that we dont need
#everything except T and P maths
drop_subjects1 <-  c("ENG1007","GEO1003", "NAT1007", "NOR1260", "NOR1261", "SAK1001")

#mandatory subjects and S maths
drop_subjects2  <- c("NOR1264", "NOR1265", "HIS1009", "KRO1018","REA3060")

#mandatory subjects and S maths
drop_subjects3 <- c("NOR1267", "NOR1268", "NOR1269", "REL1003", "HIS1010", "KRO1019","REA3062")

drop_subjects <- c(drop_subjects1,drop_subjects2,drop_subjects3)

full_names <- full_names[!full_names$code %in% drop_subjects,]

#defining SSE and REA electives
SSE_subjects2 <- c("SAM3045","SAM3054","SAM3057","SAM3072","SPR3029","SPR3030")
REA_subjects2 <- c("REA3035","REA3038","REA3045")

SSE_subjects3 <- c("SAM3046","SAM3051","SAM3055","SAM3058","SAM3073","SPR3031","SPR3032")
REA_subjects3 <- c("REA3036","REA3039","REA3046","REA3058")


#### Math T/R vs. Math P ####

subjects1 <- c("MAT1019_stp", "MAT1019_exam", "MAT1021_stp", "MAT1021_exam")

plot_df1 <- data.frame(
  subject  = subjects1,
  observed = sapply(subjects1, function(x) mean(grades_obs1[[x]], na.rm = TRUE)),
  expected = sapply(subjects1, function(x) mean(grades_exp1[[x]], na.rm = TRUE)),
  year = 1
)

subjects2 <- c("MAT1023_stp", "MAT1023_exam", "REA3056_stp", "REA3056_exam")

plot_df2 <- data.frame(
  subject  = subjects2,
  observed = sapply(subjects2, function(x) mean(grades_obs2[[x]], na.rm = TRUE)),
  expected = sapply(subjects2, function(x) mean(grades_exp2[[x]], na.rm = TRUE)),
  year = 2
)

plot_df <- rbind(plot_df1,plot_df2)


##### expected vs. observed #####

# reshape to long format
math_plot <- pivot_longer(plot_df, cols = c(observed, expected),
                          names_to = "index", values_to = "grade")

math_plot$grade <- round(math_plot$grade, 2)

# build subject labels from full_names
subject_code   <- sub("_.*", "", math_plot$subject)
subject_suffix <- sub(".*_", "", math_plot$subject)
title_match    <- full_names$title[match(subject_code, full_names$code)]

math_plot$subject_label <- ifelse(subject_suffix == "stp",
                                  title_match,
                                  paste0(title_match, " Eksamen"))

# order subjects (by label, preserving input order), and recode index for legend labels/order
math_plot$subject_label <- factor(math_plot$subject_label,
                                  levels = rev(unique(math_plot$subject_label)))
math_plot$index <- factor(math_plot$index,
                          levels = c("observed", "expected"),
                          labels = c("Observert", "Beregnet"))

year_labeller <- function(x) {
  paste0("VG", sub("vg", "", x))
}

# precompute bar width — the old ifelse indexed the full data frame,
# which breaks once we split the data by year
math_plot$bar_width <- ifelse(math_plot$index == "Observert", 0.6, 0.6)

# base
levels(math_plot$subject_label) <- sub("Eksamen", "eksamen", levels(math_plot$subject_label))

# everything shared across the panels
common <- list(
  geom_text(aes(label = ifelse(index == "Beregnet",
                               gsub("\\.", ",", sprintf("%.2f", grade)), "")),
            position = position_dodge2(width = 0.9, padding = 0.1),
            hjust = -0.15, vjust = 1.3, size = 3.5,
            family = "serif"),
  scale_y_continuous(
    breaks       = 0:5,
    minor_breaks = seq(0, 5, 0.5),
    limits       = c(0, 5.3)
  ),
  coord_flip(),
  scale_fill_manual(values = c("Observert" = "#ffc78a", "Beregnet" = "#e02941"),
                    name = "Karaktertype"),
  labs(x = "", y = "Gjennomsnittlig karakter"),
  guides(fill = guide_legend(reverse = TRUE)),
  theme(
    text               = element_text(family = "serif", color = "black"),
    axis.title.x       = element_text(size = 13, color = "black"),
    axis.title.y       = element_text(size = 13, color = "black"),
    axis.text.x        = element_text(size = 12, color = "black"),
    axis.text.y        = element_text(size = 12, color = "black"),
    axis.line.x        = element_blank(),
    axis.ticks.x       = element_line(color = "grey20", linewidth = 0.5),
    axis.ticks.y       = element_blank(),
    plot.title         = element_text(size = 13, face = "bold", hjust = 0.5),
    panel.background   = element_rect(fill = NA),
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.key.size    = unit(1, "lines"),
    legend.text        = element_text(size = 12),
    legend.title       = element_text(size = 13)
  )
)

# one panel per year, ggtitle replaces the old facet strip text
panel_plot <- function(d) {
  ggplot(d, aes(x = subject_label, y = grade, fill = index)) +
    geom_col(position = position_dodge2(width = 0.9, padding = 0.1),
             width = d$bar_width) +
    common +
    ggtitle(year_labeller(unique(d$year)))
}

plots <- lapply(split(math_plot, math_plot$year), panel_plot)
plots <- lapply(plots, function(p) p + theme(legend.position = "bottom"))

obs_exp_maths <- wrap_plots(plots, nrow = 1, guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom"))

ggsave("plots/obs_exp_maths.tiff", 
       plot = obs_exp_maths,
       width = 240,
       height = 130,
       units = "mm",
       dpi = 600,
       compression = "lzw")


##### forventet karakterulempe #####

diff_df <- data.frame(
  comparison = c(
    "Matematikk 1T vs 1P",
    "Matematikk R1 vs 2P",
    "Matematikk 1T vs 1P",
    "Matematikk R1 vs 2P"
  ),
  type = c("stp", "stp", "exam", "exam"),
  diff = c(
    plot_df$expected[plot_df$subject == "MAT1021_stp"] - plot_df$expected[plot_df$subject == "MAT1019_stp"],
    plot_df$expected[plot_df$subject == "REA3056_stp"] - plot_df$expected[plot_df$subject == "MAT1023_stp"],
    plot_df$expected[plot_df$subject == "MAT1021_exam"] - plot_df$expected[plot_df$subject == "MAT1019_exam"],
    plot_df$expected[plot_df$subject == "REA3056_exam"] - plot_df$expected[plot_df$subject == "MAT1023_exam"]
  )
)


diff_df$diff <- round(diff_df$diff, 2)


diff_df <- data.frame(
  comparison = c("Matematikk 1T vs 1P", "Matematikk R1 vs 2P",
                 "Matematikk 1T vs 1P", "Matematikk R1 vs 2P"),
  type       = c("stp", "stp", "exam", "exam"),
  diff       = c(-0.42, -0.56, -0.48, -0.03)
)

diff_df$comparison <- factor(diff_df$comparison, levels = rev(unique(diff_df$comparison)))
diff_df$type <- factor(diff_df$type, levels = c("exam", "stp"),
                       labels = c("Eksamen", "Standpunkt"))

pd <- position_dodge(width = 0.6)

diff_maths <- ggplot(diff_df, aes(x = comparison, y = diff, colour = type)) +
  geom_linerange(aes(ymin = 0, ymax = diff), position = pd, linewidth = 0.7,
                 show.legend = FALSE) +
  geom_point(size = 4, position = pd) +
  geom_text(aes(label = gsub("\\.", ",", sprintf("%.2f", diff))), position = pd,
            hjust = 1.7, size = 3.5, show.legend = FALSE, family = "serif") +
  coord_flip(clip = "off") +
  scale_y_continuous(
    breaks = c(-0.6, -0.4, -0.2, 0),
    limits = c(-0.65, 0),
    expand = expansion(mult = c(0, 0.15)),
    labels = function(x) gsub("\\.", ",", sprintf("%.1f", x))
  ) +
  scale_colour_manual(values = c("#7b3f8d", "#518D3F"), name = "Karaktertype") +
  guides(colour = guide_legend(override.aes = list(size = 2.5))) +
  labs(x = "", y = "Karakterulempe") +
  theme_minimal(base_family = "serif") +
  theme(
    axis.text.x        = element_text(size = 9, colour = "black"),
    axis.text.y        = element_text(size = 10, colour = "black"),
    axis.title.x       = element_text(size = 10, colour = "black"),
    axis.title.y       = element_text(colour = "black"),
    panel.grid.major.x = element_line(colour = "grey85"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA),
    legend.position    = "bottom",
    legend.key.size    = unit(0.6, "lines"),
    legend.text        = element_text(size = 9, colour = "black"),
    legend.title       = element_text(size = 10, colour = "black")
  )


ggsave("plots/diff_maths.tiff", 
       plot = diff_maths,
       width = 180,
       height = 90,
       units = "mm",
       dpi = 600,
       compression = "lzw")



#### VG2 ####


##### expected vs. observed #####

# Create  vector with both _stp and _exam suffixes
subject_codes2 <- c(
  paste0(SSE_subjects2, "_stp"),
  paste0(SSE_subjects2, "_exam"),
  paste0(REA_subjects2, "_stp"),
  paste0(REA_subjects2, "_exam")
)

# compute means for observed and expected per subject
means_df <- data.frame(
  subject  = subject_codes2,
  observed = sapply(subject_codes2, function(x) mean(grades_obs2[[x]], na.rm = TRUE)),
  expected = sapply(subject_codes2, function(x) mean(grades_exp2[[x]], na.rm = TRUE))
)

# reshape to long format
subj_plot <- pivot_longer(means_df, cols = c(observed, expected),
                          names_to = "index", values_to = "grade")

subj_plot$grade <- round(subj_plot$grade, 2)

# build subject labels from full_names
subject_code   <- sub("_.*", "", subj_plot$subject)
subject_suffix <- sub(".*_", "", subj_plot$subject)
subj_plot$subject_label <- full_names$title[match(subject_code, full_names$code)]
subj_plot$type <- ifelse(subject_suffix == "stp", "Standpunktkarakterer", "Eksamenskarakterer")

# Custom order for subjects
desired_order <- c(
  full_names$title[match(SSE_subjects2, full_names$code)],
  full_names$title[match(REA_subjects2, full_names$code)]
)
# Add "Eksamen" suffix for exam subjects
desired_order_exam <- paste0(desired_order, " Eksamen")
desired_order_stp <- desired_order
desired_order <- c(desired_order_exam, desired_order_stp)

# order subjects and recode index
subj_plot$subject_label <- factor(subj_plot$subject_label, levels = rev(desired_order))
subj_plot$index <- factor(subj_plot$index,
                          levels = c("observed", "expected"),
                          labels = c("Observert", "Beregnet"))
subj_plot$type <- factor(subj_plot$type, levels = c("Standpunktkarakterer", "Eksamenskarakterer"))

subj_plot <- subj_plot[!is.na(subj_plot$subject_label),]


# precompute bar width — the ifelse indexes the full data frame,
# which breaks once we split the data by type
subj_plot$bar_width <- ifelse(subj_plot$index == "Observert", 0.6, 0.6)


# everything shared across the panels
common_vg2 <- list(
  geom_text(aes(label = ifelse(index == "Beregnet",
                               gsub("\\.", ",", sprintf("%.2f", grade)), "")),
            position = position_dodge2(width = 0.9, padding = 0.1),
            hjust = -0.15, vjust = 0.9, size = 3.5,
            family = "serif"),
  scale_y_continuous(
    breaks       = 0:5,
    minor_breaks = seq(0, 5, 0.5),
    limits       = c(0, 5.3)
  ),
  coord_flip(),
  scale_fill_manual(values = c("Observert" = "#ffc78a", "Beregnet" = "#e02941"),
                    name = "Karaktertype"),
  labs(x = "", y = "Gjennomsnittlig karakter"),
  guides(fill = guide_legend(reverse = TRUE)),
  theme(
    text               = element_text(family = "serif", color = "black"),
    axis.title.x       = element_text(size = 13, color = "black"),
    axis.title.y       = element_text(size = 13, color = "black"),
    axis.text.x        = element_text(size = 12, color = "black"),
    axis.text.y        = element_text(size = 12, color = "black"),
    axis.line.x        = element_blank(),
    axis.ticks.x       = element_line(color = "grey20", linewidth = 0.5),
    axis.ticks.y       = element_blank(),
    plot.title         = element_text(size = 13, face = "bold", hjust = 0.5,
                                      color = "black"),
    panel.background   = element_rect(fill = NA),
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.key.size    = unit(1, "lines"),
    legend.text        = element_text(size = 12, color = "black"),
    legend.title       = element_text(size = 13, color = "black"),
    # set here rather than with `&` on the patchwork: keeps the legend keys
    # horizontal, and plot.margin is per-panel anyway
    legend.position    = "bottom",
    plot.margin        = margin(t = 5.5, r = 0, b = 5.5, l = 0)
  )
)

# one panel per type, ggtitle replaces the old facet strip text
panel_plot_vg2 <- function(d) {
  ggplot(d, aes(x = subject_label, y = grade, fill = index)) +
    geom_col(position = position_dodge2(width = 0.9, padding = 0.1),
             width = d$bar_width) +
    common_vg2 +
    ggtitle(unique(d$type))
}

plots_vg2 <- lapply(split(subj_plot, subj_plot$type), panel_plot_vg2)

# drop subject labels from every panel except the leftmost
plots_vg2[-1] <- lapply(plots_vg2[-1],
                        function(p) p + theme(axis.text.y = element_blank()))

# combine side by side; guides collects the two identical legends into one,
# plot_annotation places the collected legend at the bottom
obs_exp_vg2 <- wrap_plots(plots_vg2, nrow = 1, guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom"))

obs_exp_vg2

ggsave("results/plots/obs_exp_vg2.tiff", 
       plot = obs_exp_vg2,
       width = 240,
       height = 180,
       units = "mm",
       dpi = 600,
       compression = "lzw")


###### means of subjects means ######

avg_grades <- data.frame(
  track = c("SSE", "Realfag"),
  mean_stp = c(
    mean(sapply(paste0(SSE_subjects2, "_stp"), function(col) mean(grades_obs2[[col]], na.rm = TRUE))),
    mean(sapply(paste0(REA_subjects2, "_stp"), function(col) mean(grades_obs2[[col]], na.rm = TRUE)))
  ),
  mean_stp_exp = c(
    mean(sapply(paste0(SSE_subjects2, "_stp"), function(col) mean(grades_exp2[[col]], na.rm = TRUE))),
    mean(sapply(paste0(REA_subjects2, "_stp"), function(col) mean(grades_exp2[[col]], na.rm = TRUE)))
  ),
  mean_exam = c(
    mean(sapply(paste0(SSE_subjects2, "_exam"), function(col) mean(grades_obs2[[col]], na.rm = TRUE))),
    mean(sapply(paste0(REA_subjects2, "_exam"), function(col) mean(grades_obs2[[col]], na.rm = TRUE)))
  ),
  mean_exam_exp = c(
    mean(sapply(paste0(SSE_subjects2, "_exam"), function(col) mean(grades_exp2[[col]], na.rm = TRUE))),
    mean(sapply(paste0(REA_subjects2, "_exam"), function(col) mean(grades_exp2[[col]], na.rm = TRUE)))
  )
)

avg_grades[c("mean_stp", "mean_stp_exp", "mean_exam", "mean_exam_exp")] <- 
  round(avg_grades[c("mean_stp", "mean_stp_exp", "mean_exam", "mean_exam_exp")], 2)



##### forventet karakterulempe #####


# calculate the expected-grade penalty of choosing a given subject instead of an SSE subject
#this function calculates the stp and exam (if an exam grade exists) pentalty for taking specific courses compared to averages across SSE subjects
subject_penalty <- function(grades_exp, sse_subjects, subject_code){
  
  get_cols <- function(codes, suffix, df) {
    cols <- paste0(codes, suffix)
    cols[cols %in% names(df)]
  }
  
  # Standpunkt (stp)
  sse_stp_cols <- get_cols(sse_subjects, "_stp", grades_exp) # retrieves the columns with SSE stp grades
  sse_mean_stp <- mean(unlist(grades_exp[sse_stp_cols]), na.rm = TRUE) # takes the mean of all grades in these columns
  subject_stp  <- mean(grades_exp[[paste0(subject_code, "_stp")]], na.rm = TRUE) #mean expected grade in the focal subject
  stp_penalty  <- subject_stp - sse_mean_stp
  
  # exam 
  sse_exam_cols <- c(get_cols(sse_subjects, "_exam", grades_exp)) # finds the exam columns for the SSE subjects
  sse_mean_exam <- mean(unlist(grades_exp[sse_exam_cols]), na.rm = TRUE) # takes the mean of all exam grades in these columns
  
  # finds the exam column for the focal subject
  subject_exam_col <- paste0(subject_code, c("_exam")) 
  subject_exam_col <- subject_exam_col[subject_exam_col %in% names(grades_exp)]
  
  if(length(subject_exam_col) == 0){
    subject_exam <- NA
    exam_penalty <- NA
  } else {
    #calculates the difference between expected exam grade in focal subject and the mean of exam grades in SSE subjects
    subject_exam <- mean(grades_exp[[subject_exam_col]], na.rm = TRUE) 
    exam_penalty <- subject_exam - sse_mean_exam
  }
  
  list(stp_penalty = stp_penalty, exam_penalty = exam_penalty)
}


diff_fysikk1 <- subject_penalty(grades_exp2, SSE_subjects2, "REA3038")
diff_kjemi1 <- subject_penalty(grades_exp2, SSE_subjects2, "REA3045")
diff_biologi1 <- subject_penalty(grades_exp2, SSE_subjects2, "REA3035")


###### plot ######

diff_df <- data.frame(
  comparison = c("Fysikk 1", "Kjemi 1", "Biologi 1",
                 "Fysikk 1", "Kjemi 1", "Biologi 1"),
  type       = c("stp", "stp", "stp", "exam", "exam", "exam"),
  diff       = c(-0.93, -0.75, -0.27, -0.88, -0.74, -0.42)
)


diff_df$comparison <- factor(diff_df$comparison, levels = c("Fysikk 1", "Kjemi 1", "Biologi 1"))
diff_df$type <- factor(diff_df$type, levels = c("exam", "stp"),
                       labels = c("Eksamen", "Standpunkt"))

pd <- position_dodge(width = 0.6)

diff_rea2 <- ggplot(diff_df, aes(x = comparison, y = diff, colour = type)) +
  geom_linerange(aes(ymin = 0, ymax = diff), position = pd, linewidth = 0.7,
                 show.legend = FALSE) +
  geom_point(size = 4, position = pd) +
  geom_text(aes(label = gsub("\\.", ",", sprintf("%.2f", diff))), position = pd,
            hjust = 1.7, size = 3.5, show.legend = FALSE, family = "serif") +
  coord_flip(clip = "off") +
  scale_y_continuous(
    breaks = c(-1, -0.8, -0.6, -0.4, -0.2, 0),
    limits = c(-1.05, 0),
    expand = expansion(mult = c(0, 0.15)),
    labels = function(x) gsub("\\.", ",", sprintf("%.1f", x))
  ) +
  scale_colour_manual(values = c("#7b3f8d", "#518D3F"), name = "Karaktertype") +
  guides(colour = guide_legend(override.aes = list(size = 2.5))) +
  labs(x = "", y = "Karakterulempe i forhold til SSØ-gjennomsnittet") +
  theme_minimal(base_family = "serif") +
  theme(
    axis.text.x        = element_text(size = 9, colour = "black"),
    axis.text.y        = element_text(size = 11, colour = "black"),
    axis.title.x       = element_text(size = 10, colour = "black"),
    axis.title.y       = element_text(colour = "black"),
    panel.grid.major.x = element_line(colour = "grey85"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA),
    legend.position    = "bottom",
    legend.key.size    = unit(0.6, "lines"),
    legend.text        = element_text(size = 9, colour = "black"),
    legend.title       = element_text(size = 10, colour = "black")
  )


ggsave("plots/diff_rea2.tiff", 
       plot = diff_rea2,
       width = 180,
       height = 90,
       units = "mm",
       dpi = 600,
       compression = "lzw")



#### VG3 ####


##### expected vs. observed #####


# Create  vector with both _stp and _exam suffixes
subject_codes3 <- c(
  paste0(SSE_subjects3, "_stp"),
  paste0(SSE_subjects3, "_exam"),
  paste0(REA_subjects3, "_stp"),
  paste0(REA_subjects3, "_exam")
)

# compute means for observed and expected per subject
means_df <- data.frame(
  subject  = subject_codes3,
  observed = sapply(subject_codes3, function(x) mean(grades_obs3[[x]], na.rm = TRUE)),
  expected = sapply(subject_codes3, function(x) mean(grades_exp3[[x]], na.rm = TRUE))
)

# reshape to long format
subj_plot <- pivot_longer(means_df, cols = c(observed, expected),
                          names_to = "index", values_to = "grade")

subj_plot$grade <- round(subj_plot$grade, 2)

# build subject labels from full_names
subject_code   <- sub("_.*", "", subj_plot$subject)
subject_suffix <- sub(".*_", "", subj_plot$subject)
subj_plot$subject_label <- full_names$title[match(subject_code, full_names$code)]
subj_plot$type <- ifelse(subject_suffix == "stp", "Standpunktkarakterer", "Eksamenskarakterer")

# Custom order for subjects
desired_order <- c(
  full_names$title[match(SSE_subjects3, full_names$code)],
  full_names$title[match(REA_subjects3, full_names$code)]
)
# Add "Eksamen" suffix for exam subjects
desired_order_exam <- paste0(desired_order, " Eksamen")
desired_order_stp <- desired_order
desired_order <- c(desired_order_exam, desired_order_stp)

# order subjects and recode index
subj_plot$subject_label <- factor(subj_plot$subject_label, levels = rev(desired_order))
subj_plot$index <- factor(subj_plot$index,
                          levels = c("observed", "expected"),
                          labels = c("Observert", "Beregnet"))
subj_plot$type <- factor(subj_plot$type, levels = c("Standpunktkarakterer", "Eksamenskarakterer"))

# precompute bar width, the ifelse indexes the full data frame, which breaks once we split the data by type
subj_plot$bar_width <- ifelse(subj_plot$index == "Observert", 0.6, 0.6)

# everything shared across the panels
common_vg3 <- list(
  geom_text(aes(label = ifelse(index == "Beregnet",
                               gsub("\\.", ",", sprintf("%.2f", grade)), "")),
            position = position_dodge2(width = 0.9, padding = 0.1),
            hjust = -0.15, vjust = 0.9, size = 3.5,
            family = "serif"),
  scale_y_continuous(
    breaks       = 0:5,
    minor_breaks = seq(0, 5, 0.5),
    limits       = c(0, 5.3)
  ),
  coord_flip(),
  scale_fill_manual(values = c("Observert" = "#ffc78a", "Beregnet" = "#e02941"),
                    name = "Karaktertype"),
  labs(x = "", y = "Gjennomsnittlig karakter"),
  guides(fill = guide_legend(reverse = TRUE)),
  theme(
    text               = element_text(family = "serif", color = "black"),
    axis.title.x       = element_text(size = 13, color = "black"),
    axis.title.y       = element_text(size = 13, color = "black"),
    axis.text.x        = element_text(size = 12, color = "black"),
    axis.text.y        = element_text(size = 12, color = "black"),
    axis.line.x        = element_blank(),
    axis.ticks.x       = element_line(color = "grey20", linewidth = 0.5),
    axis.ticks.y       = element_blank(),
    plot.title         = element_text(size = 13, face = "bold", hjust = 0.5,
                                      color = "black"),
    panel.background   = element_rect(fill = NA),
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.key.size    = unit(1, "lines"),
    legend.text        = element_text(size = 12, color = "black"),
    legend.title       = element_text(size = 13, color = "black")
  )
)

# one panel per type — ggtitle replaces the old facet strip text
panel_plot_vg3 <- function(d) {
  ggplot(d, aes(x = subject_label, y = grade, fill = index)) +
    geom_col(position = position_dodge2(width = 0.9, padding = 0.1),
             width = d$bar_width) +
    common_vg3 +
    ggtitle(unique(d$type))
}

plots_vg3 <- lapply(split(subj_plot, subj_plot$type), panel_plot_vg3)

# drop subject labels from every panel except the leftmost
plots_vg3[-1] <- lapply(plots_vg3[-1],
                        function(p) p + theme(axis.text.y = element_blank()))

# pull the panels closer together by trimming the facing margins
plots_vg3[[1]] <- plots_vg3[[1]] + theme(plot.margin = margin(5.5, -5, 5.5, 5.5))
plots_vg3[[2]] <- plots_vg3[[2]] + theme(plot.margin = margin(5.5, 5.5, 5.5, -5))

# combine side by side, collect the two identical legends into one at the bottom
obs_exp_vg3 <- wrap_plots(plots_vg3, nrow = 1, guides = "collect") &
  theme(legend.position = "bottom")


ggsave("results/plots/obs_exp_vg3.tiff", 
       plot = obs_exp_vg3,
       width = 240,
       height = 180,
       units = "mm",
       dpi = 600,
       compression = "lzw")


###### means of subjects means ######

avg_grades <- data.frame(
  track = c("SSE", "Realfag"),
  mean_stp = c(
    mean(sapply(paste0(SSE_subjects3, "_stp"), function(col) mean(grades_obs3[[col]], na.rm = TRUE))),
    mean(sapply(paste0(REA_subjects3, "_stp"), function(col) mean(grades_obs3[[col]], na.rm = TRUE)))
  ),
  mean_stp_exp = c(
    mean(sapply(paste0(SSE_subjects3, "_stp"), function(col) mean(grades_exp3[[col]], na.rm = TRUE))),
    mean(sapply(paste0(REA_subjects3, "_stp"), function(col) mean(grades_exp3[[col]], na.rm = TRUE)))
  ),
  mean_exam = c(
    mean(sapply(paste0(SSE_subjects3, "_exam"), function(col) mean(grades_obs3[[col]], na.rm = TRUE))),
    mean(sapply(paste0(REA_subjects3, "_exam"), function(col) mean(grades_obs3[[col]], na.rm = TRUE)))
  ),
  mean_exam_exp = c(
    mean(sapply(paste0(SSE_subjects3, "_exam"), function(col) mean(grades_exp3[[col]], na.rm = TRUE))),
    mean(sapply(paste0(REA_subjects3, "_exam"), function(col) mean(grades_exp3[[col]], na.rm = TRUE)))
  )
)

avg_grades[c("mean_stp", "mean_stp_exp", "mean_exam", "mean_exam_exp")] <- 
  round(avg_grades[c("mean_stp", "mean_stp_exp", "mean_exam", "mean_exam_exp")], 2)


##### forventet karakterulempe #####

diff_fysikk2 <- subject_penalty(grades_exp3, SSE_subjects3, "REA3039")
diff_kjemi2 <- subject_penalty(grades_exp3, SSE_subjects3, "REA3046")
diff_biologi2 <- subject_penalty(grades_exp3, SSE_subjects3, "REA3036")
diff_mathsr2 <- subject_penalty(grades_exp3, SSE_subjects3, "REA3058")


diff_df <- data.frame(
  comparison = c(
    "Biologi 2",
    "Kjemi 2",
    "Fysikk 2",
    "Matematikk R2"
  ),
  type = c("stp", "stp", "stp", "stp", "exam", "exam", "exam", "exam"),
  diff = c(
    diff_biologi2$stp_penalty,
    diff_kjemi2$stp_penalty,
    diff_fysikk2$stp_penalty,
    diff_mathsr2$stp_penalty,
    diff_biologi2$exam_penalty,
    diff_kjemi2$exam_penalty,
    diff_fysikk2$exam_penalty,
    diff_mathsr2$exam_penalty
  )
)

diff_df$diff <- round(diff_df$diff, 2)
diff_df$comparison <- factor(diff_df$comparison, levels = c("Matematikk R2", "Fysikk 2", "Kjemi 2", "Biologi 2"))
diff_df$type <- factor(diff_df$type, levels = c("exam", "stp"),
                       labels = c("Eksamen", "Standpunkt"))

pd <- position_dodge(width = 0.6)

diff_rea3 <- ggplot(diff_df, aes(x = comparison, y = diff, colour = type)) +
  geom_linerange(aes(ymin = 0, ymax = diff), position = pd, linewidth = 0.7,
                 show.legend = FALSE) +
  geom_point(size = 4, position = pd) +
  geom_text(aes(label = gsub("\\.", ",", sprintf("%.2f", diff))), position = pd,
            hjust = 1.7, size = 3.5, show.legend = FALSE, family = "serif") +
  coord_flip(clip = "off") +
  scale_y_continuous(
    breaks = c(-1.4, -1.2, -1, -0.8, -0.6, -0.4, -0.2, 0),
    limits = c(-1.55, 0),
    expand = expansion(mult = c(0, 0.15)),
    labels = function(x) gsub("\\.", ",", sprintf("%.1f", x))
  ) +
  scale_colour_manual(values = c("#7b3f8d", "#518D3F"), name = "Karaktertype") +
  guides(colour = guide_legend(override.aes = list(size = 2.5))) +
  labs(x = "", y = "Karakterulempe i forhold til SSØ-gjennomsnittet") +
  theme_minimal(base_family = "serif") +
  theme(
    axis.text.x        = element_text(size = 9, colour = "black"),
    axis.text.y        = element_text(size = 11, colour = "black"),
    axis.title.x       = element_text(size = 10, colour = "black"),
    axis.title.y       = element_text(colour = "black"),
    panel.grid.major.x = element_line(colour = "grey85"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA),
    legend.position    = "bottom",
    legend.key.size    = unit(0.6, "lines"),
    legend.text        = element_text(size = 9, colour = "black"),
    legend.title       = element_text(size = 10, colour = "black")
  )

ggsave("results/plots/diff_rea3.tiff", 
       plot = diff_rea3,
       width = 180,
       height = 90,
       units = "mm",
       dpi = 600,
       compression = "lzw")






