# this is a script that flags fall injuries if a dataset contains both ICD9 and ICD10 codes; e.g. if the dataset spans the time period of October 01 2015 (2015-10-01)

# note: This presumes the column heading is "CLM_FROM_DT" for the date of the claim; amend if necessary

flag_fall_injuries <- function(df) {
  # Convert dataframe to data.table for better performance
  setDT(df)

  # Load fall codes
  icd10_fall_codes <- c("W00", "W01", "W02", "W03", "W04", "W05",
                        "W06", "W07", "W08", "W09", "W10", "W11",
                        "W12", "W13", "W14", "W15", "W16",
                        "W17", "W18", "W19")
  icd9_fall_codes <- c("E880","E881","E882",
                       "E883","E884","E885",
                       "E886","E888")

  # Create regex patterns for fall codes
  icd10_pattern <- paste(icd10_fall_codes, collapse = "|")
  icd9_pattern <- paste(icd9_fall_codes, collapse = "|")

  # Flagging falls in separate steps
  df[, icd_code := ifelse(CLM_FROM_DT >= as.Date("2015-10-01"), '10', '09')]

  df[, icd9_fall := as.integer(rowSums(sapply(.SD, function(x) grepl(icd9_pattern, x)))) > 0]

  df[, icd10_fall := as.integer(rowSums(sapply(.SD, function(x) grepl(icd10_pattern, x)))) > 0]

  df[, any_fall := fifelse((icd9_fall == 1 & icd_code == '09') |
                             (icd10_fall == 1 & icd_code == '10'),
                           1L,
                           0L)]

  return(df)
}
