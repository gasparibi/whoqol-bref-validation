# Clinical data validation and scoring pipeline (WHOQOL-BREF)

This repository provides an independent validation and scoring pipeline built in **R** for the **WHOQOL-BREF** quality-of-life instrument. 
This project simulates central clinical data review processes, translating complex protocol specifications and scoring rules into a fully automated data pipeline that exports a reporting workbook.

---

## About the WHOQOL-BREF

The **WHOQOL-BREF** is a widely utilized, multidimensional quality-of-life assessment instrument developed by the World Health Organization (WHO). 
It provides a standardized cross-cultural profile of subjective well-being across four core domains:

- **Physical Health:** activities of daily living, medical dependence, energy, mobility, pain, sleep, and work capacity.

- **Psychological:** bodily image, positive/negative feelings, self-esteem, spirituality, and cognitive functioning.

- **Social Relationships:** personal relationships, social support, and sexual activity.

- **Environment:** financial resources, freedom, physical safety, health/social care accessibility, home environment, and transport.

The instrument comprises **26 items** (including two global items on overall quality of life and general health) scored on a 5-point Likert scale. 
Because clinical trial and survey data frequently contain edge cases (such as missing items, reverse-scored questions, and strict domain completeness thresholds), this pipeline automates the cleaning, validation, and transformation of raw responses into standard **4–20** and **0–100** score metrics.

---

## Key features

- **Test scenario matrix (`TEST-01` to `TEST-12`):** validates the entire range of potential data conditions, including ideal baselines, floor/ceiling boundaries, partial missingness, domain-specific completeness thresholds, global questionnaire invalidity, and dirty data inputs (out-of-range numerics and non-numeric inputs like `"X"`).

- **Raw data preservation:** retains original inputs in **`_RAW` character fields** to ensure compliance with data integrity and audit-trail best practices, preventing raw data overwrites during cleaning.

- **Automated scoring and domain logic:**

  - handles **reverse scoring** for negatively phrased items (`Q3`, `Q4`, `Q26`).

  - computes domain-specific scores for **Physical**, **Psychological**, **Social**, and **Environment** domains.

  - applies standard transformations to **4–20** and **0–100** score metrics.

- **Built-in Quality Control (QC):** features **programmatic self-assertions** (`stopifnot`) and **automated pass/fail flags** (`QC_ALL`) comparing calculated results against a fixed reference dataset.

- **Excel export:** automatically generates a styled Excel workbook using **`openxlsx`** featuring a dedicated **"Scoring Notes"** documentation tab.

---

## Repository structure

```

whoqol-bref-validation/
│
├── whoqol-bref-validation.Rproj                       # RStudio project configuration
├── README.md                                          # Project documentation
├── LICENSE                                            # MIT License
├── .gitignore                                         # Git ignore rules
├── R/
│   └── whoqol_bref_pipeline.R                         # Validation and scoring script
│
└── output/
    └── WHOQOL_BREF_Synthetic_Validation_Dataset.xlsx  # Excel report


```

---

## Prerequisites and dependencies

The pipeline was developed and tested using **R 4.5.2** and the following CRAN packages:

```
install.packages(c("tidyverse", "openxlsx"))
```

---

## How to run

1. Clone or download this repository.

2. Open the R script `R/whoqol_bref_pipeline.R` in RStudio or your preferred R environment.

3. Run the script. It will execute the validation workflow, perform internal QC checks, and output the Excel workbook directly into your working directory.

```
source("R/whoqol_bref_pipeline.R")
```

---

## Summary of validation scenarios

| Scenario ID | Description | Expected Outcome |
| :--- | :--- | :--- |
| **TEST-01** | Ideal complete baseline (midpoint response vector) | Pass (all domains = 50.00) |
| **TEST-02** | True floor boundary | Pass (all domains = 0.00) |
| **TEST-03** | True ceiling boundary | Pass (all domains = 100.00) |
| **TEST-04** | Realistic non-uniform mixed response vector | Pass (custom fractional scores) |
| **TEST-05** | Physical domain: 1 missing item | Valid (domain mean calculated from available items) |
| **TEST-06** | Physical domain: 2 missing items | Invalid (fails minimum valid-item threshold) |
| **TEST-07** | Global boundary: 5 missing items across all domains | Questionnaire valid (all domains meet thresholds) |
| **TEST-08** | Global boundary: 5 missing items concentrated in 2 domains | Questionnaire valid, 2 invalid domains |
| **TEST-09** | Global boundary: 6 missing items | Globally invalid (all domain scores suppressed) |
| **TEST-10** | Completely blank questionnaire (100% missing) | Globally invalid |
| **TEST-11** | Out-of-range numeric values (`0` and `6`) | Recoded as missing (`NA`) for scoring |
| **TEST-12** | Non-numeric character input (`"X"`) | Recoded as missing (`NA`) for scoring |

---

## Deliverable highlights

The Excel file (`WHOQOL_BREF_Synthetic_Validation_Dataset.xlsx`) includes two worksheets:

1. **WHOQOL-BREF Validation Data**: contains side-by-side raw inputs, cleaned numeric items, reverse-scored fields, derived metrics, expected references, and automated QC pass/fail flags.

2. **Scoring Notes**: a fully documented reference guide detailing the exact data cleaning, domain mapping, missingness thresholds, and transformation formulas implemented in the script.

---

## License

This project is licensed under the MIT License. 
See the [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE) file for details.

---

## Citation

If you use this repository or its validation logic in your work, please cite:

Gasparini, B. (2026). *Clinical data validation and scoring pipeline (WHOQOL-BREF)*. GitHub repository.



