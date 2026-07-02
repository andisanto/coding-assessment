library(logr)



# Start the log (creates a .log file automatically)
log_open("question_3_tlg/02_create_visualization.R.log")
log_print("Starting Execution data visualization")

# ==============================================================================
# Program Name: 02_create_visualization.R
# Purpose     : create visual for AEs
# Inputs      : pharmaverseadam::adae 
# Outputs     : ae_severity_heatmap.png | ae_severity_bar_chart.png | 
#               ae_top_10_incidence_ci.png
# Author      : Antonio Di Santo Date : 02-Jul-2026
# ==============================================================================

# Load required visualization and clinical packages
library(ggplot2)
library(pharmaverseadam)

# 1. Clean data using base R (Removing screen failures from ADSL)
adsl_raw <- as.data.frame(pharmaverseadam::adsl)
adae_raw <- as.data.frame(pharmaverseadam::adae)

adsl_clean <- adsl_raw[
  !grepl("Screen Failure", adsl_raw$TRT01A, ignore.case = TRUE) & !is.na(adsl_raw$TRT01A), 
]

# 2. Filter for treatment-emergent AEs and keep patients present in safety population
adae_subset <- adae_raw[
  adae_raw$TRTEMFL == "Y" & adae_raw$USUBJID %in% adsl_clean$USUBJID & !is.na(adae_raw$TRTEMFL), 
]

# 3. Create a clean severity data frame (Drop missing severity rows)
ae_sev_data <- adae_subset[!is.na(adae_subset$AESEV) & adae_subset$AESEV != "", c("TRT01A", "AESEV")]

# Convert variables to factors for ordered visualization
ae_sev_data$TRT01A <- factor(ae_sev_data$TRT01A)
ae_sev_data$AESEV <- factor(ae_sev_data$AESEV, levels = c("MILD", "MODERATE", "SEVERE"))

# 4. Generate summary counts for the heatmap calculation
sev_counts <- as.data.frame(table(ae_sev_data$TRT01A, ae_sev_data$AESEV))
names(sev_counts) <- c("Treatment", "Severity", "Count")

# =========================================================================
# OPTION A: 100% Stacked Bar Chart (Best for viewing relative proportions)
# =========================================================================
log_print("Generating Option A: Stacked Bar Chart")

plot_bar <- ggplot(ae_sev_data, aes(x = TRT01A, fill = AESEV)) +
  geom_bar(position = "fill", width = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "YlOrRd") + 
  labs(
    title = "Adverse Event Severity Distribution by Treatment Arm",
    subtitle = "Safety Population",
    x = "Treatment Group",
    y = "Percentage of Total Events",
    fill = "Severity Level"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave("question_3_tlg/ae_severity_bar_chart.png", plot = plot_bar, width = 8, height = 6, dpi = 300)

# =========================================================================
# OPTION B: Heatmap Matrix (Best for clean, non-overlapping grid layouts)
# =========================================================================
log_print("Generating Option B: Heatmap Matrix")

plot_heatmap <- ggplot(sev_counts, aes(x = Severity, y = Treatment, fill = Count)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = Count), fontface = "bold", size = 5) + 
  scale_fill_gradient(low = "#fee8c8", high = "#e34a33") +    
  labs(
    title = "Heatmap Matrix of AE Severity Counts by Treatment",
    subtitle = "Safety Population",
    x = "Adverse Event Severity",
    y = "Treatment Group",
    fill = "Event Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid = element_blank() 
  )

ggsave("question_3_tlg/ae_severity_heatmap.png", plot = plot_heatmap, width = 8, height = 5, dpi = 300)

# =========================================================================
####### Plot 2: Top 10 most frequent AEs (with 95% CI for incidence rates). 
# =========================================================================
log_print("Generating Plot 2: Top 10 AE Incidence CIs")

arm_totals <- table(adsl_clean$TRT01A)

unique_ae <- unique(adae_subset[, c("USUBJID", "TRT01A", "AETERM")])

global_counts <- table(unique_ae$AETERM)
top_10_terms <- names(sort(global_counts, decreasing = TRUE))[1:10]

ae_top_10_data <- unique_ae[unique_ae$AETERM %in% top_10_terms, ]

ae_counts_df <- as.data.frame(table(ae_top_10_data$AETERM, ae_top_10_data$TRT01A))
names(ae_counts_df) <- c("AETERM", "TRT01A", "n")

ae_counts_df$N <- as.numeric(arm_totals[as.character(ae_counts_df$TRT01A)])
ae_counts_df$Incidence <- ae_counts_df$n / ae_counts_df$N
ae_counts_df$CI_Lower <- NA
ae_counts_df$CI_Upper <- NA

for (i in 1:nrow(ae_counts_df)) {
  x <- ae_counts_df$n[i]
  total <- ae_counts_df$N[i]
  
  if (total > 0) {
    b_test <- binom.test(x, total, conf.level = 0.95)
    ae_counts_df$CI_Lower[i] <- b_test$conf.int[1]
    ae_counts_df$CI_Upper[i] <- b_test$conf.int[2]
  }
}

ae_counts_df$AETERM <- factor(ae_counts_df$AETERM, levels = rev(top_10_terms))

plot_incidence <- ggplot(ae_counts_df, aes(x = Incidence, y = AETERM, color = TRT01A)) +
  geom_errorbarh(
    aes(xmin = CI_Lower, xmax = CI_Upper), 
    position = position_dodge(width = 0.5), 
    height = 0.3, 
    linewidth = 0.8
  ) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = "Patient-Level Incidence Rates with Exact 95% Confidence Intervals (Safety Population)",
    x = "Incidence Rate (% of Patients in Treatment Arm)",
    y = "Adverse Event Term (AETERM)",
    color = "Treatment Arm"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(hjust = 0),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "bold")
  )

ggsave("question_3_tlg/ae_top_10_incidence_ci.png", plot = plot_incidence, width = 10, height = 7, dpi = 300)

# Correctly log final status and close the log environment safely
log_print("Execution successfully completed. Closing log.")
log_close()
