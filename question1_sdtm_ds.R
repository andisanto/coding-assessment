library(logr)

# Start the log (creates a .log file automatically)
log_open("question1_sdtm_ds.log")
log_print("Starting Execution DS mapping")

# ==============================================================================
# Program Name: sdtm_ds.R
# Purpose     : Derive and map SDTM DS domain from raw data using {sdtm.oak}
# Inputs      : pharmaverseraw::ds_raw
# Outputs     : sdtm_ds (SDTM standard data frame containing requested variables)
# Author.     : Antonio Di Santo Date : 02-Jul-2026
# ==============================================================================

if (!requireNamespace("pharmaverseraw", quietly = TRUE)) install.packages("pharmaverseraw")
if (!requireNamespace("sdtm.oak", quietly = TRUE)) install.packages("sdtm.oak")

# ------------------------------------------------------------------------------
# 1. Environment Setup & Data Infrastructure
# ------------------------------------------------------------------------------
library(pharmaverseraw)
library(sdtm.oak)
library(dplyr)
library(lubridate)

# Updated Controlled Terminology Spec
study_ct <- data.frame(
  stringsAsFactors = FALSE,
  codelist_code = c(
    "C66727", "C66727", "C66727", "C66727", "C66727", "C66727", 
    "C66727", "C66727", "C66727", "C66727", "C66727", "C66727", "C66727"
  ),
  term_code = c(
    "C41331", "C25250", "C28554", "C48226", "C48227", "C48250", 
    "C142185", "C49628", "C49632", "C49634", "C_RAND", "C_OTHER", "C_OTHER"
  ),
  term_value = c(
    "ADVERSE EVENT", "COMPLETED", "DEATH", "LACK OF EFFICACY", 
    "LOST TO FOLLOW-UP", "PHYSICIAN DECISION", "PROTOCOL VIOLATION", 
    "SCREEN FAILURE", "STUDY TERMINATED BY SPONSOR", "WITHDRAWAL BY SUBJECT",
    "RANDOMIZED", "OTHER", "OTHER"
  ),
  collected_value = c(
    "Adverse Event", "Complete", "Dead", "Lack of Efficacy", 
    "Lost To Follow-Up", "Physician Decision", "Protocol Violation", 
    "Trial Screen Failure", "Study Terminated By Sponsor", "Withdrawal by Subject",
    "Randomized", "Final Lab Visit", "Final Retrieval Visit"
  ),
  term_preferred_term = c(
    "AE", "Completed", "Died", NA, NA, NA, "Violation", 
    "Failure to Meet Inclusion/Exclusion Criteria", NA, "Dropout",
    "Randomized", "Other", "Other"
  ),
  term_synonyms = c(
    "ADVERSE EVENT", "COMPLETE", "Death", NA, NA, NA, NA, NA, NA, 
    "Discontinued Participation", "RANDOMIZED", "FINAL LAB VISIT", "FINAL RETRIEVAL VISIT"
  )
)

# Initialize Raw Dataset and calculate the target USUBJID value string
raw_ds_prep <- pharmaverseraw::ds_raw %>%
  filter(!is.na(PATNUM)) %>%
  mutate(
    DERIVED_USUBJID = paste(STUDY, PATNUM, sep = "-")
  ) %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

# Initialize target skeleton using oak's native ID framework columns (oak_id)
ds_target <- raw_ds_prep %>% 
  select(all_of(oak_id_vars()))

# ------------------------------------------------------------------------------
# 2. Variable Mapping and Transformations
# ------------------------------------------------------------------------------

# Map Date/Time
ds_target <- assign_datetime(
  raw_dat = raw_ds_prep, 
  tgt_dat = ds_target,
  raw_var = c("IT.DSSTDAT", "DSTMCOL"), 
  tgt_var = "DSSTDTC",
  raw_fmt = c("m-d-y", "H:M")
)

# Apply Conditional Logic for DSTERM, DSDECOD, and DSCAT
raw_ds_mapped <- raw_ds_prep %>%
  mutate(
    TMP_DSDECOD = if_else(!is.na(OTHERSP), toupper(OTHERSP), toupper(IT.DSDECOD)),
    TMP_DSTERM  = if_else(!is.na(OTHERSP), OTHERSP, IT.DSTERM),
    TMP_DSCAT   = case_when(
      !is.na(OTHERSP)                     ~ "OTHER EVENT",
      toupper(IT.DSDECOD) == "RANDOMIZED" ~ "PROTOCOL MILESTONE",
      TRUE                                ~ "DISPOSITION EVENT"
    )
  )

# Map Controlled Terminology for DSDECOD
ds_target <- assign_ct(
  raw_dat  = raw_ds_mapped,
  tgt_dat  = ds_target,
  raw_var  = "TMP_DSDECOD", 
  tgt_var  = "DSDECOD",
  ct_spec  = study_ct,    
  ct_clst  = "C66727" 
)

# Map DSTERM via assign_no_ct
ds_target <- assign_no_ct(
  raw_dat = raw_ds_mapped,
  tgt_dat = ds_target,
  raw_var = "TMP_DSTERM",
  tgt_var = "DSTERM"
)

# Map Hardcoded Clinical Constants (DOMAIN)
ds_target <- hardcode_no_ct(
  raw_dat = raw_ds_prep, 
  tgt_dat = ds_target,
  raw_var = "STUDY", 
  tgt_var = "DOMAIN", 
  tgt_val = "DS"
)

# Map DSCAT dynamically using our conditional logic variable
ds_target <- assign_no_ct(
  raw_dat = raw_ds_mapped, 
  tgt_dat = ds_target,
  raw_var = "TMP_DSCAT", 
  tgt_var = "DSCAT"
)

# Map your custom concatenated string to the target USUBJID field 
ds_target <- assign_no_ct(
  raw_dat = raw_ds_prep,
  tgt_dat = ds_target,
  raw_var = "DERIVED_USUBJID",
  tgt_var = "USUBJID"
)

# Map STUDYID explicitly into ds_target so derive_seq() can group by it
ds_target <- assign_no_ct(
  raw_dat = raw_ds_prep,
  tgt_dat = ds_target,
  raw_var = "STUDY",
  tgt_var = "STUDYID"
)



# Map VISIT directly from raw records via assign_no_ct
ds_target <- assign_no_ct(
  raw_dat = raw_ds_prep,
  tgt_dat = ds_target,
  raw_var = "INSTANCE",
  tgt_var = "VISIT"
)


# VISITNUM: Create a Study-Level Global Visit Lookup Table
# This ensures that a given VISIT name always resolves to the exact same VISITNUM study-wide!
visit_lookup <- ds_target %>%
  select(VISIT) %>%
  distinct() %>%
  filter(!is.na(VISIT)) %>%
  # Chronological sort tip: order visits logically if you have a predefined master order.
  # Otherwise, an alphabetical/appearance match establishes the global numbering rules.
  arrange(VISIT) %>% 
  mutate(VISITNUM = row_number() * 10) # 10, 20, 30... formatting sequence

# Step 3.3: Safely Apply Global VISITNUM Across All Trial Subjects
ds_target <- ds_target %>%
  left_join(visit_lookup, by = "VISIT")
# ------------------------------------------------------------------------------
# 3. Sequence (DSSEQ) & Study Day (DSSTDY) Derivations
# ------------------------------------------------------------------------------

# Sequence Number Generation
sdtm_ds_seq <- ds_target %>%
  arrange(USUBJID, DSSTDTC, DSTERM) %>%
  derive_seq(
    tgt_var  = "DSSEQ",
    rec_vars = oak_id_vars()
  )

# Study Day Calculation (DSSTDY)
sdtm_ds_final <- sdtm_ds_seq %>%
  group_by(USUBJID) %>%
  mutate(rfstdtc_mock = min(ymd(substr(DSSTDTC, 1, 10)), na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(
    days_diff = as.integer(ymd(substr(DSSTDTC, 1, 10)) - rfstdtc_mock),
    DSSTDY    = if_else(days_diff >= 0, days_diff + 1L, days_diff)
  )

# ------------------------------------------------------------------------------
# 4. Final Subsetting & Variable Sorting
# ------------------------------------------------------------------------------

# Structure output and duplicate timestamp for DSDTC target specification
question1_sdtm_ds <- sdtm_ds_final %>%
  mutate(DSDTC = DSSTDTC) %>% 
  select(
    STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT, 
    VISITNUM, VISIT, DSDTC, DSSTDTC, DSSTDY
  )

# Output verification
glimpse(question1_sdtm_ds)
log_print(glimpse(question1_sdtm_ds))

# Close the log at the end of the script
log_close()
