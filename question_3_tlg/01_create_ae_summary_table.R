

library(logr)

# Start the log (creates a .log file automatically)
log_open("question_3_tlg/01_create_ae_summary_table.log")
log_print("Starting Execution AE smmary table")

# ==============================================================================
# Program Name: 01_create_ae_summary_table.R
# Purpose     : create_ae_summary_table 
# Inputs      : pharmaverseadam::adae 
# Outputs     : Table_10_ae_summary_table.html
# Author.     : Antonio Di Santo Date : 02-Jul-2026
# ==============================================================================




# Load required clinical packages
library(gtsummary)
library(gt)
library(pharmaverseadam)

# Convert package datasets to standard data frames
adsl_raw <- as.data.frame(pharmaverseadam::adsl)
adae_raw <- as.data.frame(pharmaverseadam::adae)

# 1. Clean data, removing screen failures safely from ADSL
adsl_clean <- adsl_raw[
  !grepl("Screen Failure", adsl_raw$TRT01A, ignore.case = TRUE) & !is.na(adsl_raw$TRT01A), 
]

# 2. Extract unique subject-level AE rows to meet regulatory count rules
adae_subset <- adae_raw[
  adae_raw$TRTEMFL == "Y" & adae_raw$USUBJID %in% adsl_clean$USUBJID & !is.na(adae_raw$TRTEMFL), 
]

# Keep only columns needed for the hierarchical structure
ae_table_data <- unique(adae_subset[, c("USUBJID", "TRT01A", "AESOC", "AEDECOD")])

# 3. Generate the Nested Hierarchical Table
ae_table <- tbl_hierarchical(
  data = ae_table_data,
  variables = c(AESOC, AEDECOD),      # Defines the row nesting hierarchy
  by = TRT01A,                        # Stratifies treatment arms horizontally
  id = USUBJID,                       # Ensures subject-level counts (not event counts)
  denominator = adsl_clean,           # Uses the entire clinical population for % calculations
  overall_row = TRUE,                 # Injects standalone "Any AE" row at the top
  label = list(
    `..ard_hierarchical_overall..` = "Subjects with at least one TEAE"
  )
) %>%
  # 4. Add the total column (calculated dynamically across arms)
  add_overall(last = TRUE, col_label = "**Total**") %>%
  # 5. Sort the finalized table (SOC descending first, then PT within SOC)
  sort_hierarchical(sort_type = "descending") %>%
  # 6. Formats and final touch-ups
  modify_header(
    # UPDATE THE ROW LABEL COLUMN HEADER TO SPECIFIC MEDDRA DICTIONARY LABELS
    label = "**Primary System Organ Class  \nDictionary-Derived Term**",
    all_stat_cols() ~ "**{level}**  \nN = {n}" 
  ) %>%
  as_gt() %>%
  tab_header(
    title = "Table 10: Summary of Treatment-Emergent Adverse Events",
    subtitle = "Safety Population"
  )


# 7. Save the nested HTML table to a file
gtsave(data = ae_table, filename = "question_3_tlg/Table_10_ae_summary_table.html")
log_print("Execution successfully completed. Closing log.")

# Close the log at the end of the script
log_close()
