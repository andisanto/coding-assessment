
library(logr)

# Start the log (creates a .log file automatically)
log_open("question_2_adam/create_adsl.log")
log_print("Starting Execution ADSL mapping")

# ==============================================================================
# Program Name: create_adsl.R
# Purpose     : Derive and map ADaM ADSL 
# Inputs      : pharmaversesdtm::dm, pharmaversesdtm::vs, pharmaversesdtm::ex,
#               pharmaversesdtm::ds, pharmaversesdtm::ae
# Outputs     : adsl
# Author.     : Antonio Di Santo Date : 02-Jul-2026
# ==============================================================================



library(tidyverse)
library(xportr)

# 1. Core Population Flag & Age Groups (DM) -------------------------------
demo <- pharmaversesdtm::dm %>%
  select(STUDYID, USUBJID, AGE, ARM) %>%
  mutate(
    # Age Group 9 variables
    AGEGR9 = case_when(
      AGE < 18 ~ "<18",
      AGE >= 18 & AGE <= 50 ~ "18-50",
      AGE > 50 ~ ">50",
      TRUE ~ NA_character_
    ),
  
    AGEGR9N = recode_values(
      AGEGR9,
      "<18"   ~ 1,
      "18-50" ~ 2,
      ">50"   ~ 3,
      default = NA_real_
    ),
    #Evaluates strictly to "Y" for all randomized participants in DM
    ITTFL = if_else(!is.na(ARM) & ARM != "", "Y", "N")
  ) %>%
  select(-ARM) # Retain AGE in the demo dataframe


# 2. First Exposure Datetime (EX) -----------------------------------------
# Filter for valid doses: EXDOSE > 0 OR (EXDOSE == 0 and EXTRT contains 'PLACEBO')
ex_valid <- pharmaversesdtm::ex %>%
  # Explicitly cast to character to bypass implicit Date vector behaviors
  mutate(exstdtc_raw = str_trim(as.character(EXSTDTC))) %>%
  filter(EXDOSE > 0 | (EXDOSE == 0 & str_detect(str_to_upper(EXTRT), "PLACEBO"))) %>%
  filter(str_length(str_sub(exstdtc_raw, 1, 10)) == 10)

# Derive TRTSDTM and TRTSTMF based strictly on the raw text patterns
treat_dtm <- ex_valid %>%
  select(USUBJID, exstdtc_raw) %>%
  mutate(
    # 1. Determine Imputation Flag based strictly on raw string composition
    TRTSTMF = case_when(
      !str_detect(exstdtc_raw, "T") ~ "TM",                                # Time completely missing
      str_detect(exstdtc_raw, "T\\d{2}$") ~ "M",                           # Only hours collected
      str_detect(exstdtc_raw, "T\\d{2}:\\d{2}$") ~ NA_character_,          # Only seconds missing -> no flag
      TRUE ~ NA_character_
    ),
    
    # 2. Impute only 00:00 (Hours and Minutes) as requested
    TRTSDTM_FIXED = case_when(
      !str_detect(exstdtc_raw, "T") ~ paste0(exstdtc_raw, "T00:00"),
      str_detect(exstdtc_raw, "T\\d{2}$") ~ paste0(exstdtc_raw, ":00"),
      TRUE ~ exstdtc_raw
    ),
    
    # Generate a temporary numeric datetime index purely for sorting
    dtm_sort = as.POSIXct(
      if_else(str_length(TRTSDTM_FIXED) == 16, paste0(TRTSDTM_FIXED, ":00"), TRTSDTM_FIXED), 
      format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"
    )
  ) %>%
  group_by(USUBJID) %>%
  # Sort strictly by the verified numeric datetime structure chronologically per patient
  arrange(dtm_sort, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(USUBJID, TRTSDTM = TRTSDTM_FIXED, TRTSTMF)


# 3. Last Known Alive Date Tracking Dataframes ----------------------------
treat_end_dates <- ex_valid %>%
  mutate(
    exendtc_raw = str_trim(as.character(EXENDTC)),
    clean_endtc = case_when(
      !str_detect(exendtc_raw, "T") ~ paste0(exendtc_raw, "T00:00:00"),
      str_detect(exendtc_raw, "T\\d{2}$") ~ paste0(exendtc_raw, ":00:00"),
      str_detect(exendtc_raw, "T\\d{2}:\\d{2}$") ~ paste0(exendtc_raw, ":00"),
      TRUE ~ exendtc_raw
    ),
    exendtm = as.POSIXct(clean_endtc, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  ) %>%
  group_by(USUBJID) %>%
  summarize(TRTEDTM = suppressWarnings(max(exendtm, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(TRTEDTM = replace(TRTEDTM, is.infinite(TRTEDTM), as.POSIXct(NA)))

vs_alive <- pharmaversesdtm::vs %>%
  filter((!is.na(VSSTRESN) | (!is.na(VSSTRESC) & VSSTRESC != "")) & nchar(VSDTC) >= 10) %>%
  mutate(vsd = as_date(str_sub(VSDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarize(max_vs = suppressWarnings(max(vsd, na.rm = TRUE)))

ae_alive <- pharmaversesdtm::ae %>%
  filter(nchar(AESTDTC) >= 10) %>%
  mutate(aed = as_date(str_sub(AESTDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarize(max_ae = suppressWarnings(max(aed, na.rm = TRUE)))

ds_alive <- pharmaversesdtm::ds %>%
  filter(nchar(DSSTDTC) >= 10) %>%
  mutate(dsd = as_date(str_sub(DSSTDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarize(max_ds = suppressWarnings(max(dsd, na.rm = TRUE)))


# 4. Merging and Final Maximized LSTAVLDT Derivation ----------------------
demo_merged <- reduce(list(demo, treat_dtm, treat_end_dates), left_join, by = "USUBJID")

lstavl_df <- demo_merged %>%
  select(USUBJID, TRTEDTM) %>%
  mutate(max_ex = as_date(TRTEDTM)) %>% 
  left_join(vs_alive, by = "USUBJID") %>%
  left_join(ae_alive, by = "USUBJID") %>%
  left_join(ds_alive, by = "USUBJID") %>%
  rowwise() %>%
  mutate(
    LSTAVLDT = suppressWarnings(max(c_across(starts_with("max_")), na.rm = TRUE)),
    LSTAVLDT = replace(LSTAVLDT, is.infinite(LSTAVLDT), NA_Date_)
  ) %>%
  ungroup() %>%
  select(USUBJID, LSTAVLDT)


# 5. Metadata Definitions --------------------------------------------------
dlm <- tribble(
  ~dataset, ~label,
  "adsl",   "Subject-Level Analysis Dataset"
)

vlm <- tribble(
  ~dataset, ~variable, ~label, ~type, ~format,
  'adsl', 'STUDYID',  'Study Identifier',                     'character', NA_character_,
  'adsl', 'USUBJID',  'Unique Subject Identifier',            'character', NA_character_,
  'adsl', 'AGE',      'Age',                                  'numeric',   NA_character_,
  'adsl', 'AGEGR9',   'Pooled Age Group 9',                   'character', NA_character_,
  'adsl', 'AGEGR9N',  'Pooled Age Group 9 (N)',               'numeric',   NA_character_,
  'adsl', 'ITTFL',    'Intent-To-Treat Population Flag',      'character', NA_character_,
  'adsl', 'TRTSDTM',  'Datetime of First Exposure',           'character', NA_character_,
  'adsl', 'TRTSTMF',  'Datetime of First Exposure Imput Flag','character', NA_character_,
  'adsl', 'LSTAVLDT', 'Last Date Known Alive',                'Date',      "DATE9."
)


# 6. Build Final Trimmed ADSL Dataset -------------------------------------
adsl <- left_join(demo_merged, lstavl_df, by = "USUBJID") %>%
  select(STUDYID, USUBJID, AGE, AGEGR9, AGEGR9N, TRTSDTM, TRTSTMF, ITTFL, LSTAVLDT) %>%
  arrange(USUBJID) %>%
  # Apply structural metadata via xportr
  xportr_df_label(dlm, domain = "adsl") %>%
  xportr_label(vlm, domain = "adsl") %>%
  xportr_type(vlm, domain = "adsl") %>%
  xportr_format(vlm, domain = "adsl") %>%
  # Map character NAs to empty strings safely at the very end
  mutate(across(where(is.character), ~ replace_na(., "")))

print(head(adsl))
log_print("Execution successfully completed. Closing log.")

# Close the log at the end of the script
log_close()

