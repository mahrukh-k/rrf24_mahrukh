# Reproducible Research Fundamentals 
# 01. Data processing

### Libraries
# library(haven)
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(labelled)

### Loading data ----

# Load the dataset
#data_path <- "C:/Users/wb572332/OneDrive - WBG/DataWork/DataWork/Data"
data      <- read_dta(file.path(data_path, "Raw/TZA_CCT_baseline.dta"))

View(data)
glimpse(data)

### Remove duplicates based on hhid
## there were two duplicates in hhid in the original data
data_dedup <- data %>%
    distinct(hhid, .keep_all = TRUE)
    
### tidying data: create 3 dataframes (household level, individual level, amenities)

### Household (HH) level data ----

#### Tidying data for HH level
data_tidy_hh <- data_dedup %>%
    select(vid, hhid, enid, floor:n_elder, food_cons:submissionday)

glimpse(data_tidy_hh)

### Data cleaning for Household-member (HH-member) level
data_clean_hh <- data_tidy_hh %>%
    # Convert submissionday to date
    mutate(submissiondate = as.Date(submissionday, format = "%Y-%m-%d %H:%M:%S")) %>%
    # Convert duration to numeric (if it is not already)
    mutate(duration = as.numeric(duration)) %>%
    # Convert ar_farm_unit to factor (categorical data)
    mutate(ar_farm_unit = as.factor(ar_farm_unit)) %>%
    mutate(ar_farm_unit = na_if(ar_farm_unit, "")) %>%
    # Replace values in the crop variable based on crop_other using regex for new crops
    mutate(crop = case_when(
        str_detect(crop_other, "Coconut") ~ 40,
        str_detect(crop_other, "Sesame") ~ 41,
        TRUE ~ crop
    )) %>%
    # Recode negative numeric values (-88) as missing (NA)
    mutate(across(where(is.numeric), ~replace(., .x == -88, NA))) %>%
    # Add variable labels
    set_variable_labels(
        duration = "Duration",
        submissiondate = "Date of Submission",
        ar_farm_unit = "Area Unit"
    )

# Save the household data
write_dta(data_clean_hh, file.path(data_path, "Intermediate/TZA_CCT_HH.dta"))

### Household member (HH-member) level data ----

#### Tidying data for HH-member level
data_tidy_mem <- data_dedup %>%
    select(vid, hhid, enid, gender_1:days_impact_2) %>%
    pivot_longer(cols = -c(vid, hhid, enid),  # Keep IDs static
                 names_to = c(".value","member"),
                 names_pattern = "(.*)_(\\d+)")  # Capture the variable and the suffix


### Data cleaning for HH-member level
data_clean_mem <- data_tidy_mem %>%
    # Drop rows where gender is missing (NA)
    filter(!is.na(gender)) %>%
    # Variable labels
    set_variable_labels(
        member = "Gender")

# Save the tidy household-member data
write_dta(data_clean_mem, file.path(data_path, "Intermediate/TZA_CCT_HH_mem.dta"))

### Secondary data ----

# Load CSV data
secondary_data <- read.csv(file.path(data_path, "Raw/TZA_amenity.csv"))

# Tidying data
secondary_data <- secondary_data %>%
    pivot_wider(names_from = amenity,
                values_from = n,
                names_prefix = "n_")

# Save the final tidy secondary data
write_dta(secondary_data, file.path(data_path, "Intermediate/TZA_amenity_tidy.dta"))
