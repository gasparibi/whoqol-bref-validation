library(tidyverse)
library(openxlsx)

# ------------------------------------------------------------------------------
# 1. Define Synthetic Scenarios
# ------------------------------------------------------------------------------

test_scenarios <- tribble(
  ~SUBJID,   ~SCENARIO_DESC,
  
  "TEST-01", "Ideal complete baseline (midpoint response vector, all 3s)",
  "TEST-02", "True floor boundary (positive items = 1; reversed items = 5)",
  "TEST-03", "True ceiling boundary (positive items = 5; reversed items = 1)",
  "TEST-04", "Realistic non-uniform mixed response vector",
  
  "TEST-05", "Physical domain: 1 missing item (domain remains valid; mean calculated from available responses)",
  "TEST-06", "Physical domain: 2 missing items (domain invalid; below minimum valid-item threshold)",
  
  "TEST-07", "Global boundary: 5 missing items across all four domains; all domains valid, questionnaire valid",
  "TEST-08", "Global boundary: 5 missing items across two domains; 2 domains invalid, questionnaire valid",
  "TEST-09", "Global boundary: 6 missing items; questionnaire invalid",
  "TEST-10", "Completely blank questionnaire (100% missing)",
  
  "TEST-11", "Out-of-range numeric values (0 and 6; recoded as missing for scoring)",
  "TEST-12", "Non-numeric character input ('X'; recoded as missing for scoring)"
)

# ------------------------------------------------------------------------------
# 2. Initialize Raw Item Response Matrix
# ------------------------------------------------------------------------------

raw_items <- matrix(
  "3",
  nrow = nrow(test_scenarios),
  ncol = 26
)

colnames(raw_items) <- paste0("Q", 1:26)

raw_df <- as.data.frame(
  raw_items,
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# Helper: Find Scenario Row
# ------------------------------------------------------------------------------

scenario_row <- function(id) {
  which(test_scenarios$SUBJID == id)
}

# ------------------------------------------------------------------------------
# 3. Apply Scenario-Specific Values
# ------------------------------------------------------------------------------

# TEST-02: True floor boundary
# Positive items are 1; negatively phrased items are 5 before reversal.
i <- scenario_row("TEST-02")
raw_df[i, 1:26] <- "1"
raw_df[i, c("Q3", "Q4", "Q26")] <- "5"

# TEST-03: True ceiling boundary
# Positive items are 5; negatively phrased items are 1 before reversal.
i <- scenario_row("TEST-03")
raw_df[i, 1:26] <- "5"
raw_df[i, c("Q3", "Q4", "Q26")] <- "1"

# TEST-04: Realistic non-uniform mixed response vector.
# The resulting fractional domain means also exercise non-integer calculations.
i <- scenario_row("TEST-04")
test04_values <- c(
  4, 3, 2, 4, 5, 3, 2, 4, 1, 3, 4, 2, 5,
  3, 2, 4, 3, 2, 4, 5, 3, 2, 4, 3, 2, 1
)
raw_df[i, paste0("Q", 1:26)] <- as.list(as.character(test04_values))

# TEST-05: 1 missing item in Physical domain; domain remains valid (6/7 valid).
i <- scenario_row("TEST-05")
raw_df[i, "Q10"] <- NA_character_

# TEST-06: 2 missing items in Physical domain; domain fails completeness (5/7 valid).
i <- scenario_row("TEST-06")
raw_df[i, c("Q10", "Q15")] <- NA_character_

# TEST-07: Exactly 5 missing items, spread across all four domains.
# Physical: 6/7; Psychological: 5/6; Social: 2/3; Environment: 6/8.
# The questionnaire has 21/26 valid responses and each domain is at its threshold.
i <- scenario_row("TEST-07")
raw_df[i, c("Q10", "Q5", "Q20", "Q8", "Q9")] <- NA_character_

# TEST-08: Exactly 5 missing items concentrated in two domains.
# Physical: 5/7 invalid; Psychological: 4/6 invalid; Social: 3/3 valid; Environment: 8/8 valid.
# Overall: 21/26 valid, so global validity passes despite two invalid domains.
i <- scenario_row("TEST-08")
raw_df[i, c("Q10", "Q15", "Q18", "Q5", "Q6")] <- NA_character_

# TEST-09: Add one more missing item to TEST-07's pattern (6 missing total).
# This is the first globally invalid case: only 20/26 responses remain valid.
i <- scenario_row("TEST-09")
raw_df[i, c("Q10", "Q5", "Q20", "Q8", "Q9", "Q18")] <- NA_character_

# TEST-10: Completely blank questionnaire.
i <- scenario_row("TEST-10")
raw_df[i, paste0("Q", 1:26)] <- as.list(rep(NA_character_, 26))

# TEST-11: Out-of-range numeric values.
i <- scenario_row("TEST-11")
raw_df[i, "Q5"] <- "0"
raw_df[i, "Q12"] <- "6"

# TEST-12: Non-numeric character input.
i <- scenario_row("TEST-12")
raw_df[i, "Q8"] <- "X"

# ------------------------------------------------------------------------------
# 4. Preserve Raw Inputs and Define the Cleaning / Scoring Rules
# ------------------------------------------------------------------------------

# Preserve original inputs as character fields so invalid entries (e.g., "X")
# remain visible for audit and test verification. Raw fields receive a _RAW suffix.
raw_df_char <- raw_df %>%
  mutate(across(everything(), as.character)) %>%
  rename_with(~ paste0(.x, "_RAW"))

dataset_raw <- bind_cols(
  test_scenarios,
  raw_df_char
)

raw_item_cols <- paste0("Q", 1:26, "_RAW")
clean_item_names <- paste0("Q", 1:26)

# WHOQOL-BREF domain item mappings. Q3, Q4, and Q26 are reversed before scoring.
phys_items  <- c("Q3", "Q4", "Q10", "Q15", "Q16", "Q17", "Q18")
psych_items <- c("Q5", "Q6", "Q7", "Q11", "Q19", "Q26")
soc_items   <- c("Q20", "Q21", "Q22")
env_items   <- c("Q8", "Q9", "Q12", "Q13", "Q14", "Q23", "Q24", "Q25")

clean_item <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  ifelse(!is.na(x_num) & x_num %in% 1:5, x_num, NA_real_)
}

score_domain <- function(x, min_complete) {
  n_valid <- sum(!is.na(x))

  if (n_valid < min_complete) {
    return(
      list(
        MISSCNT = length(x) - n_valid,
        VALFL = "N",
        MEAN = NA_real_,
        SCORE_4_20 = NA_real_,
        SCORE_100 = NA_real_
      )
    )
  }

  # Calculate the mean using available valid responses. The raw item values
  # are not overwritten; for a mean score, this yields the same result as
  # substituting the missing item(s) with that mean.
  mean_score <- mean(x, na.rm = TRUE)
  score_4_20 <- mean_score * 4
  score_100 <- (score_4_20 - 4) * 6.25

  list(
    MISSCNT = length(x) - n_valid,
    VALFL = "Y",
    MEAN = mean_score,
    SCORE_4_20 = score_4_20,
    SCORE_100 = score_100
  )
}

# ------------------------------------------------------------------------------
# 5. Clean Items, Reverse Score, and Calculate Domain Scores
# ------------------------------------------------------------------------------

processed_rows <- vector("list", nrow(dataset_raw))

for (i in seq_len(nrow(dataset_raw))) {

  # Clean each raw item and return a named numeric vector (one value per item).
  raw_num <- vapply(
    dataset_raw[i, raw_item_cols],
    clean_item,
    numeric(1)
  )
  names(raw_num) <- clean_item_names

  # Reverse score negatively phrased items; keep cleaned raw responses unchanged.
  q3_r  <- ifelse(!is.na(raw_num["Q3"]),  6 - raw_num["Q3"],  NA_real_)
  q4_r  <- ifelse(!is.na(raw_num["Q4"]),  6 - raw_num["Q4"],  NA_real_)
  q26_r <- ifelse(!is.na(raw_num["Q26"]), 6 - raw_num["Q26"], NA_real_)

  # Use a separate vector for domain scoring so the cleaned item layer remains
  # faithful to the cleaned (pre-reversal) response values.
  domain_items <- raw_num
  domain_items["Q3"]  <- q3_r
  domain_items["Q4"]  <- q4_r
  domain_items["Q26"] <- q26_r

  phys  <- domain_items[phys_items]
  psych <- domain_items[psych_items]
  soc   <- domain_items[soc_items]
  env   <- domain_items[env_items]

  # Missing/invalid entries have already become NA in raw_num.
  total_misscnt <- sum(is.na(raw_num))
  overall_valfl <- ifelse((26 - total_misscnt) >= 21, "Y", "N")

  phys_score  <- score_domain(phys, min_complete = 6)
  psych_score <- score_domain(psych, min_complete = 5)
  soc_score   <- score_domain(soc, min_complete = 2)
  env_score   <- score_domain(env, min_complete = 6)

  # If global validity fails, suppress all domain score derivations.
  if (overall_valfl == "N") {
    phys_score$VALFL  <- psych_score$VALFL  <- soc_score$VALFL  <- env_score$VALFL  <- "N"
    phys_score$MEAN   <- psych_score$MEAN   <- soc_score$MEAN   <- env_score$MEAN   <- NA_real_
    phys_score$SCORE_4_20 <- psych_score$SCORE_4_20 <- soc_score$SCORE_4_20 <- env_score$SCORE_4_20 <- NA_real_
    phys_score$SCORE_100  <- psych_score$SCORE_100  <- soc_score$SCORE_100  <- env_score$SCORE_100  <- NA_real_
  }

  # Store the cleaned numeric item layer together with reverse scores and outputs.
  cleaned_items_row <- as_tibble_row(as.list(raw_num))

  processed_rows[[i]] <- bind_cols(
    tibble(
      SUBJID = dataset_raw$SUBJID[i],
      SCENARIO_DESC = dataset_raw$SCENARIO_DESC[i]
    ),
    cleaned_items_row,
    tibble(
      Q3_R = q3_r,
      Q4_R = q4_r,
      Q26_R = q26_r,

      TOTAL_MISSCNT = total_misscnt,
      OVERALL_VALFL = overall_valfl,

      PHYS_MISSCNT = phys_score$MISSCNT,
      PHYS_VALFL = phys_score$VALFL,
      PHYS_MEAN = phys_score$MEAN,
      PHYS_4_20 = phys_score$SCORE_4_20,
      PHYS_100 = phys_score$SCORE_100,

      PSYCH_MISSCNT = psych_score$MISSCNT,
      PSYCH_VALFL = psych_score$VALFL,
      PSYCH_MEAN = psych_score$MEAN,
      PSYCH_4_20 = psych_score$SCORE_4_20,
      PSYCH_100 = psych_score$SCORE_100,

      SOC_MISSCNT = soc_score$MISSCNT,
      SOC_VALFL = soc_score$VALFL,
      SOC_MEAN = soc_score$MEAN,
      SOC_4_20 = soc_score$SCORE_4_20,
      SOC_100 = soc_score$SCORE_100,

      ENV_MISSCNT = env_score$MISSCNT,
      ENV_VALFL = env_score$VALFL,
      ENV_MEAN = env_score$MEAN,
      ENV_4_20 = env_score$SCORE_4_20,
      ENV_100 = env_score$SCORE_100
    )
  )
}

processed_dataset <- bind_cols(
  dataset_raw,
  bind_rows(processed_rows) %>% select(-SUBJID, -SCENARIO_DESC)
)

# ------------------------------------------------------------------------------
# 6. Fixed Reference / Expected Domain Scores
# ------------------------------------------------------------------------------

expected_reference <- tribble(
  ~SUBJID, ~EXPECTED_PHYS_100, ~EXPECTED_PSYCH_100, ~EXPECTED_SOC_100, ~EXPECTED_ENV_100,
  "TEST-01", 50.00,  50.00,  50.00,  50.00,
  "TEST-02",  0.00,   0.00,   0.00,   0.00,
  "TEST-03",100.00, 100.00, 100.00, 100.00,
  "TEST-04", 46.43,  70.83,  58.33,  50.00,
  "TEST-05", 50.00,  50.00,  50.00,  50.00,
  "TEST-06",    NA,  50.00,  50.00,  50.00,
  "TEST-07", 50.00,  50.00,  50.00,  50.00,
  "TEST-08",    NA,     NA,  50.00,  50.00,
  "TEST-09",    NA,     NA,     NA,     NA,
  "TEST-10",    NA,     NA,     NA,     NA,
  "TEST-11", 50.00,  50.00,  50.00,  50.00,
  "TEST-12", 50.00,  50.00,  50.00,  50.00
)

compare_score <- function(actual, expected) {
  if (is.na(actual) && is.na(expected)) return("PASS")
  if (is.na(actual) || is.na(expected)) return("FAIL")
  if (round(actual, 2) == round(expected, 2)) return("PASS")
  "FAIL"
}

# ------------------------------------------------------------------------------
# 7. Build Final Export Dataset
# ------------------------------------------------------------------------------

final_export_df <- processed_dataset %>%
  left_join(expected_reference, by = "SUBJID") %>%
  mutate(
    QC_PHYS  = map2_chr(PHYS_100,  EXPECTED_PHYS_100,  compare_score),
    QC_PSYCH = map2_chr(PSYCH_100, EXPECTED_PSYCH_100, compare_score),
    QC_SOC   = map2_chr(SOC_100,   EXPECTED_SOC_100,   compare_score),
    QC_ENV   = map2_chr(ENV_100,   EXPECTED_ENV_100,   compare_score),
    QC_ALL   = ifelse(
      QC_PHYS == "PASS" & QC_PSYCH == "PASS" & QC_SOC == "PASS" & QC_ENV == "PASS",
      "PASS", "FAIL"
    )
  )

# Display the raw input layer first, followed by the cleaned numeric layer.
# Keep reverse-scored items immediately after their corresponding cleaned items.
cleaned_item_display_cols <- c(
  "Q1", "Q2",
  "Q3", "Q3_R",
  "Q4", "Q4_R",
  paste0("Q", 5:25),
  "Q26", "Q26_R"
)

final_export_df <- final_export_df %>%
  select(
    SUBJID,
    SCENARIO_DESC,
    all_of(raw_item_cols),
    all_of(cleaned_item_display_cols),
    TOTAL_MISSCNT,
    OVERALL_VALFL,
    PHYS_MISSCNT, PHYS_VALFL, PHYS_MEAN, PHYS_4_20, PHYS_100,
    PSYCH_MISSCNT, PSYCH_VALFL, PSYCH_MEAN, PSYCH_4_20, PSYCH_100,
    SOC_MISSCNT, SOC_VALFL, SOC_MEAN, SOC_4_20, SOC_100,
    ENV_MISSCNT, ENV_VALFL, ENV_MEAN, ENV_4_20, ENV_100,
    EXPECTED_PHYS_100, EXPECTED_PSYCH_100, EXPECTED_SOC_100, EXPECTED_ENV_100,
    QC_PHYS, QC_PSYCH, QC_SOC, QC_ENV, QC_ALL
  )

# ------------------------------------------------------------------------------
# 8. Basic Internal QC
# ------------------------------------------------------------------------------

if (any(final_export_df$QC_ALL == "FAIL")) {
  failed_scenarios <- final_export_df %>%
    filter(QC_ALL == "FAIL") %>%
    select(SUBJID, SCENARIO_DESC, QC_PHYS, QC_PSYCH, QC_SOC, QC_ENV)
  print(failed_scenarios)
  stop("QC failed: one or more synthetic validation scenarios did not match expected values.")
}

# Explicitly assert the key global boundary cases.
stopifnot(
  # TEST-07: five missing; globally valid
  final_export_df$TOTAL_MISSCNT[final_export_df$SUBJID == "TEST-07"] == 5,
  final_export_df$OVERALL_VALFL[final_export_df$SUBJID == "TEST-07"] == "Y",
  
  # TEST-08: five missing; globally valid, two invalid domains
  final_export_df$TOTAL_MISSCNT[final_export_df$SUBJID == "TEST-08"] == 5,
  final_export_df$OVERALL_VALFL[final_export_df$SUBJID == "TEST-08"] == "Y",
  final_export_df$PHYS_VALFL[final_export_df$SUBJID == "TEST-08"] == "N",
  final_export_df$PSYCH_VALFL[final_export_df$SUBJID == "TEST-08"] == "N",
  final_export_df$SOC_VALFL[final_export_df$SUBJID == "TEST-08"] == "Y",
  final_export_df$ENV_VALFL[final_export_df$SUBJID == "TEST-08"] == "Y",
  is.na(final_export_df$PHYS_100[final_export_df$SUBJID == "TEST-08"]),
  is.na(final_export_df$PSYCH_100[final_export_df$SUBJID == "TEST-08"]),
  final_export_df$SOC_100[final_export_df$SUBJID == "TEST-08"] == 50,
  final_export_df$ENV_100[final_export_df$SUBJID == "TEST-08"] == 50,
  
  # TEST-09: six missing; globally invalid
  final_export_df$TOTAL_MISSCNT[final_export_df$SUBJID == "TEST-09"] == 6,
  final_export_df$OVERALL_VALFL[final_export_df$SUBJID == "TEST-09"] == "N"
)

# ------------------------------------------------------------------------------
# 9. Create Excel Workbook, Scoring Notes, and Formatting
# ------------------------------------------------------------------------------

wb <- createWorkbook()
data_sheet <- "WHOQOL-BREF Validation Data"
notes_sheet <- "Scoring Notes"
addWorksheet(wb, data_sheet)
addWorksheet(wb, notes_sheet)

header_style <- createStyle(
  fontName = "Arial", fontSize = 11, fontColour = "#FFFFFF",
  fgFill = "#1F4E78", halign = "center", valign = "center",
  textDecoration = "bold", wrapText = TRUE
)
data_style <- createStyle(fontName = "Arial", fontSize = 10, halign = "center", valign = "center")
text_style <- createStyle(fontName = "Arial", fontSize = 10, halign = "left", valign = "center", wrapText = TRUE)
raw_text_style <- createStyle(fontName = "Arial", fontSize = 10, halign = "center", valign = "center", numFmt = "@")
item_number_style <- createStyle(fontName = "Arial", fontSize = 10, halign = "center", valign = "center", numFmt = "0")
decimal_style <- createStyle(fontName = "Arial", fontSize = 10, halign = "center", valign = "center", numFmt = "0.00")
expected_style <- createStyle(fontName = "Arial", fontSize = 10, halign = "center", valign = "center", numFmt = "0.00", fgFill = "#E2F0D9")
qc_style <- createStyle(fontName = "Arial", fontSize = 10, halign = "center", valign = "center", fgFill = "#FFF2CC", textDecoration = "bold")
notes_title_style <- createStyle(fontName = "Arial", fontSize = 16, fontColour = "#FFFFFF", fgFill = "#1F4E78", textDecoration = "bold", valign = "center")
notes_subtitle_style <- createStyle(fontName = "Arial", fontSize = 10, fontColour = "#404040", textDecoration = "italic", wrapText = TRUE, valign = "center")
notes_body_style <- createStyle(fontName = "Arial", fontSize = 10, valign = "top", wrapText = TRUE)

# Main validation-data sheet
writeData(wb, data_sheet, final_export_df, headerStyle = header_style)
num_rows <- nrow(final_export_df)

addStyle(wb, data_sheet, style = data_style, rows = 2:(num_rows + 1), cols = 1:ncol(final_export_df), gridExpand = TRUE)
addStyle(wb, data_sheet, style = text_style, rows = 2:(num_rows + 1), cols = match("SCENARIO_DESC", names(final_export_df)), gridExpand = TRUE, stack = TRUE)

raw_cols <- which(grepl("^Q[0-9]+_RAW$", names(final_export_df)))
clean_item_cols <- which(grepl("^Q[0-9]+$", names(final_export_df)))
decimal_cols <- which(grepl("_MEAN$|_4_20$|_100$|^EXPECTED_", names(final_export_df)))
expected_cols <- which(grepl("^EXPECTED_", names(final_export_df)))
qc_cols <- which(grepl("^QC_", names(final_export_df)))

if (length(raw_cols) > 0) {
  addStyle(wb, data_sheet, style = raw_text_style, rows = 2:(num_rows + 1), cols = raw_cols, gridExpand = TRUE, stack = TRUE)
}
if (length(clean_item_cols) > 0) {
  addStyle(wb, data_sheet, style = item_number_style, rows = 2:(num_rows + 1), cols = clean_item_cols, gridExpand = TRUE, stack = TRUE)
}
if (length(decimal_cols) > 0) {
  addStyle(wb, data_sheet, style = decimal_style, rows = 2:(num_rows + 1), cols = decimal_cols, gridExpand = TRUE, stack = TRUE)
}
if (length(expected_cols) > 0) {
  addStyle(wb, data_sheet, style = expected_style, rows = 1:(num_rows + 1), cols = expected_cols, gridExpand = TRUE, stack = TRUE)
}
if (length(qc_cols) > 0) {
  addStyle(wb, data_sheet, style = qc_style, rows = 1:(num_rows + 1), cols = qc_cols, gridExpand = TRUE, stack = TRUE)
}

setColWidths(wb, data_sheet, cols = match("SUBJID", names(final_export_df)), widths = 14)
setColWidths(wb, data_sheet, cols = match("SCENARIO_DESC", names(final_export_df)), widths = 62)
if (length(raw_cols) > 0) setColWidths(wb, data_sheet, cols = raw_cols, widths = 10)
if (length(clean_item_cols) > 0) setColWidths(wb, data_sheet, cols = clean_item_cols, widths = 7)

reverse_cols <- which(grepl("^Q[0-9]+_R$", names(final_export_df)))
if (length(reverse_cols) > 0) setColWidths(wb, data_sheet, cols = reverse_cols, widths = 7)

derived_names <- c(
  "TOTAL_MISSCNT", "OVERALL_VALFL",
  "PHYS_MISSCNT", "PHYS_VALFL", "PHYS_MEAN", "PHYS_4_20", "PHYS_100",
  "PSYCH_MISSCNT", "PSYCH_VALFL", "PSYCH_MEAN", "PSYCH_4_20", "PSYCH_100",
  "SOC_MISSCNT", "SOC_VALFL", "SOC_MEAN", "SOC_4_20", "SOC_100",
  "ENV_MISSCNT", "ENV_VALFL", "ENV_MEAN", "ENV_4_20", "ENV_100"
)
for (nm in intersect(derived_names, names(final_export_df))) {
  setColWidths(wb, data_sheet, cols = match(nm, names(final_export_df)), widths = 12)
}
if (length(expected_cols) > 0) setColWidths(wb, data_sheet, cols = expected_cols, widths = 16)
if (length(qc_cols) > 0) setColWidths(wb, data_sheet, cols = qc_cols, widths = 11)

freezePane(wb, data_sheet, firstActiveRow = 2, firstActiveCol = 3)
addFilter(wb, data_sheet, rows = 1, cols = 1:ncol(final_export_df))
setRowHeights(wb, data_sheet, rows = 1, heights = 40)
setRowHeights(wb, data_sheet, rows = 2:(num_rows + 1), heights = 22)

# Scoring Notes sheet: concise documentation of the rules implemented above.
scoring_notes <- tribble(
  ~SECTION, ~TOPIC, ~RULE_OR_IMPLEMENTATION,
  "Purpose", "Workbook contents", "The workbook contains the synthetic validation dataset and this notes sheet documenting the implemented cleaning, scoring, and QC rules.",
  "Input layers", "Raw item fields (*_RAW)", "Q1_RAW-Q26_RAW preserve the original synthetic inputs as character values. These fields are not overwritten during cleaning, so invalid inputs such as 0, 6, or X remain visible for audit.",
  "Input layers", "Cleaned item fields (Q1-Q26)", "Q1-Q26 are numeric. Values from 1 to 5 are retained; blank values, out-of-range numeric values, and non-numeric entries are converted to NA for scoring. A cleaned NA is displayed as a blank cell in Excel.",
  "Input layers", "Q1 and Q2", "Q1 (overall quality of life) and Q2 (overall health satisfaction) are retained as questionnaire items but are excluded from the four domain-score calculations. They still count toward the 26-item global completeness rule.",
  "Reverse scoring", "Negatively phrased items", "Q3, Q4, and Q26 are reverse-scored as (6 − cleaned response). Missing/invalid values remain missing after reversal. The cleaned Q3/Q4/Q26 columns retain their pre-reversal values; Q3_R, Q4_R, and Q26_R show the reversed values.",
  "Domain scoring", "Physical", "Items: Q3_R, Q4_R, Q10, Q15, Q16, Q17, Q18. At least 6 of 7 items must be valid.",
  "Domain scoring", "Psychological", "Items: Q5, Q6, Q7, Q11, Q19, Q26_R. At least 5 of 6 items must be valid.",
  "Domain scoring", "Social", "Items: Q20, Q21, Q22. At least 2 of 3 items must be valid.",
  "Domain scoring", "Environment", "Items: Q8, Q9, Q12, Q13, Q14, Q23, Q24, Q25. At least 6 of 8 items must be valid.",
  "Missing data", "Domain mean calculation", "A domain score is calculated only if its minimum valid-item threshold is met. The mean is calculated over valid available responses (na.rm = TRUE); raw item values are not physically imputed or overwritten. For a mean score, this is numerically equivalent to replacing missing item(s) with that same domain mean.",
  "Missing data", "Global validity", "At least 21 of the 26 cleaned item responses must be valid. Six or more missing/invalid items set OVERALL_VALFL = N and suppress all domain means and transformed scores.",
  "Transformations", "4-20 scale", "Domain mean × 4.",
  "Transformations", "0-100 scale", "(4-20 score − 4) × 6.25.",
  "Precision", "Rounding and display", "Scoring calculations retain full numeric precision. The Excel workbook displays domain means and transformed scores to two decimal places; QC comparisons are performed after rounding actual and expected scores to two decimals.",
  "QC", "Expected values and flags", "EXPECTED_* columns contain fixed reference values. Each domain QC flag compares the calculated 0-100 score with its expected value; matching NA/NA pairs pass. QC_ALL passes only when all four domain QC flags pass.",
  "Validation scenarios", "TEST-07", "TEST-07 has five missing items distributed across all four domains: Physical 6/7, Psychological 5/6, Social 2/3, Environment 6/8, and 21/26 globally valid.",
  "Validation scenarios", "TEST-08", "TEST-08 has five missing items across Physical and Psychological only: Physical 4/7 and Psychological 4/6 are invalid; Social 3/3 and Environment 8/8 remain valid. Overall validity passes at 21/26 responses.",
  "Validation scenarios", "TEST-09", "TEST-09 has six missing items (20/26 valid), triggering global invalidity and suppressing all domain scores.",
  "Validation scenarios", "TEST-10 to TEST-12", "TEST-10 is a fully blank questionnaire; TEST-11 checks out-of-range values 0 and 6; TEST-12 checks non-numeric input X.",
  "Scoring principle", "No combined total score", "The workbook reports the four domain scores separately. It does not calculate a single combined WHOQOL-BREF total score."
)

writeData(wb, notes_sheet, "WHOQOL-BREF Synthetic Validation - Scoring Notes", startRow = 1, startCol = 1, colNames = FALSE)
mergeCells(wb, notes_sheet, cols = 1:3, rows = 1)
addStyle(wb, notes_sheet, style = notes_title_style, rows = 1, cols = 1:3, gridExpand = TRUE)
writeData(wb, notes_sheet, "Implementation reference for the companion validation-data worksheet.", startRow = 2, startCol = 1, colNames = FALSE)
mergeCells(wb, notes_sheet, cols = 1:3, rows = 2)
addStyle(wb, notes_sheet, style = notes_subtitle_style, rows = 2, cols = 1:3, gridExpand = TRUE)
writeData(wb, notes_sheet, scoring_notes, startRow = 4, startCol = 1, headerStyle = header_style)
notes_last_row <- nrow(scoring_notes) + 4
addStyle(wb, notes_sheet, style = notes_body_style, rows = 5:notes_last_row, cols = 1:3, gridExpand = TRUE)
addStyle(wb, notes_sheet, style = createStyle(fontName = "Arial", fontSize = 10, textDecoration = "bold", valign = "top", wrapText = TRUE), rows = 5:notes_last_row, cols = 1:2, gridExpand = TRUE, stack = TRUE)
setColWidths(wb, notes_sheet, cols = 1, widths = 22)
setColWidths(wb, notes_sheet, cols = 2, widths = 30)
setColWidths(wb, notes_sheet, cols = 3, widths = 92)
setRowHeights(wb, notes_sheet, rows = 1, heights = 30)
setRowHeights(wb, notes_sheet, rows = 2, heights = 24)
setRowHeights(wb, notes_sheet, rows = 4, heights = 30)
setRowHeights(wb, notes_sheet, rows = 5:notes_last_row, heights = 42)
freezePane(wb, notes_sheet, firstActiveRow = 5, firstActiveCol = 3)
addFilter(wb, notes_sheet, rows = 4, cols = 1:3)

# Save the final workbook in the working directory.
saveWorkbook(wb, "WHOQOL_BREF_Synthetic_Validation_Dataset.xlsx", overwrite = TRUE)

