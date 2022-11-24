## ---------------------------
## Script name: data_preparation.r
##
## Purpose of script:
##  Creates a long data.frame with ratings.
##
## Author: Carsten Sander
##
## Date Created: 2022-11-23
##
## Copyright (c) Carsten Sander, 2022
## Email: carsten.sander@uni-hamburg.de
## ---------------------------
## Notes:
##  Before running this script, make sure your working directory
##  is set to a folder containing the /Processed data/ folder from
##  the github repository
##
##  R Version -- 4.2.1
## ---------------------------

# Load packages
library(tidyverse) # tidyverse_1.3.2
library(magrittr, include.only = "%T>%") # magrittr_2.0.3

# Download raw data from Gitlab
paths <- read.csv("Processed data/raw-data-paths.csv", header = FALSE)[-1, 1]
pav <- lapply(paths, read_csv)
qua <- read.csv("https://gitlab.pavlovia.org/csander/sii1b-direct-1f/raw/master/additional%20data/qualtrics.csv")[-c(1, 2), ] # nolint
download.file("https://gitlab.pavlovia.org/csander/sii1b-direct-1f/raw/master/stimuli/stimuli.xlsx", destfile = "./stimuli.xlsx", cacheOK = TRUE) # nolint
stims <- readxl::read_excel("./stimuli.xlsx")
# ---------------------------

# Process pavlovia data
trials <- data.frame()
ratings <- data.frame()
subjects <- data.frame()

# Loop over all individual data.frames
for (i in seq_len(length(pav))) {
    # Select relevant data for analysis
    trials_i <- pav[[i]] %>%
        filter(trial_type == "test") %>%
        select(
            subject_id = ID, item_id, one_probe, two_probe,
            one_probe_type, two_probe_type,
            one_rating = c_probe_slider_one.response,
            two_rating = c_probe_slider_two.response) %>%
        mutate(
            order = ifelse(one_probe_type == "implied",
                "implied first", "implied other first")
        ) %>%
        pivot_longer(
            cols = matches("one_|two_"),
            names_to = c("position", ".value"),
            names_pattern = "(.{3})_(.+)"
        ) %>%
        select(-position)

    # Build data.frame from individual datasets
    trials <- dplyr::bind_rows(trials, trials_i)
}

# ---------------------------

subjects <- qua %>%
    filter(part == 2) %>%
    # Recode survey items
    mutate(
        consent_given = ifelse(
            grepl("Ja", informed_consent.) |
            grepl("Nein", informed_consent_dc.),
            1, 0),
        compliance = as.numeric(recode(compliance.,
            "überhaupt nicht\1" = "1", "sehr gewissenhaft\n10" = "10")),
        age = as.numeric(str_extract(age, "\\d*")),
        gender = recode(gender,
            "anderes (z.B. nicht-binär)" = "other",
            "männlich" = "male", "weiblich" = "female"),
        pol_orientation = as.numeric(recode(pol_orientation,
            "links" = "1", "rechts" = "10")),
        pol_interest = as.numeric(recode(pol_interest.,
            "sehr stark" = "10", "überhaupt nicht" = "1")),
        pol_justice_freedom_1 = recode(pol_justice_freedom_1,
            "voll übereinstimmen" = 1, "weitgehend übereinstimmen" = 2,
            "weitgehend ablehnen" = 3, "voll und ganz ablehnen" = 4),
        pol_justice_freedom_2 = recode(pol_justice_freedom_2,
            "voll übereinstimmen" = 1, "weitgehend übereinstimmen" = 2,
            "weitgehend ablehnen" = 3, "voll und ganz ablehnen" = 4),
        pol_equal_treatment_1 = recode(pol_equal_treatment_1,
            "voll übereinstimmen" = 4, "weitgehend übereinstimmen" = 3,
            "weitgehend ablehnen" = 2, "voll und ganz ablehnen" = 1),
        pol_equal_treatment_2 = recode(pol_equal_treatment_2,
            "voll übereinstimmen" = 1, "weitgehend übereinstimmen" = 2,
            "weitgehend ablehnen" = 3, "voll und ganz ablehnen" = 4)
    ) %>%
    mutate(
        # Combine political satisfaction items into one scale
        pol_satisfaction = rowMeans(select(.,
            matches("pol_jus|pol_equ")), na.rm = TRUE)) %T>% {
        assign(x = "last_n", value = nrow(.), envir = .GlobalEnv)
        cat("Exclusion according to the pre-registered exclusion criteria\n\n")
        cat("Participants before exclusion:", nrow(.), "\n")
    } %>%
    filter(consent_given == 1) %T>% {
        cat("b. participants who withdrew their consent to\n",
            "  data analysis after full debriefing:", last_n - nrow(.), "\n")
        assign(x = "last_n", value = nrow(.), envir = .GlobalEnv)
    } %>%
    filter(compliance > 5) %T>% {
        cat("d. participants who rate their own data to be\n",
            "  unfit for analyses:", last_n - nrow(.), "\n")
        cat("Participants after exclusion:", nrow(.), "\n")
    } %>%
    filter(data_quality. == "ja") %T>% {
        cat("d. participants who rate their own data to be\n",
            "  unfit for analyses:", last_n - nrow(.), "\n")
        cat("Participants after exclusion:", nrow(.), "\n")
    } %>%
    select(subject_id = ID, age, gender, pol_interest,
                pol_orientation, pol_satisfaction, assumptions)

# ---------------------------

# Merge all data.frames
d_long <- trials %>%
    left_join(subjects) %>%
    left_join(select(stims, item_id, behavior))

# ---------------------------

# Export data
saveRDS(d_long, file = "Processed data/d-long.rds")
