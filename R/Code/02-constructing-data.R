# Reproducible Research Fundamentals 
# 02. Data construction
# RRF - 2024 - Construction

# Preliminary - Load Data ----
# Load household-level data (HH)
#data_path <- "C:/Users/wb572332/OneDrive - WBG/DataWork/DataWork/Data"

hh_data <- read_dta(file.path(data_path, "Intermediate/TZA_CCT_HH.dta"))

# Load HH-member data
mem_data <- read_dta(file.path(data_path, "Intermediate/TZA_CCT_HH_mem.dta"))

# Load secondary data
secondary_data <- read_dta(file.path(data_path, "Intermediate/TZA_amenity_tidy.dta"))

# Exercise 1: Plan construction outputs ----
# Plan the following outputs:
# 1. Area in acres.(hh_data$ar_farm - standardize to acre)
# 2. Household consumption (food and nonfood) in USD.(hh_data$food_cons - standardize to USD)
# 3. Any HH member sick.(mem_data$days_sick - collapse to hh level)
# 4. Any HH member can read or write.(mem_data$read - collapse to hh level)
# 5. Average sick days.(mem_data$days_sick - collapse to hh level)
# 6. Total treatment cost in USD.(mem_data$treat_cost)
# 7. Total medical facilities.(secondary_data)

# Exercise 2: Standardize conversion values ----

# Data construction: Household (HH) ----
# Instructions:
# 1. Convert farming area to acres where necessary.
# 2. Convert household consumption for food and nonfood into USD.

# Define standardized conversion values:
# 1. Conversion factor for acres.
acre_conv = 2.47


hh_data <- hh_data %>%
    mutate(area_acre = case_when(
        ar_farm_unit == 1 ~ ar_farm,
        ar_farm_unit == 2 ~ ar_farm * acre_conv
    )) %>%
    mutate(area_acre = replace_na(area_acre, 0)) %>%
    set_variable_labels(area_acre = "Area farmed in acres")

    
# 2. USD conversion factor.
usd_conv = 0.00037

hh_data <- hh_data %>%
    mutate(across(c(food_cons, nonfood_cons),
                  ~ .x * usd_conv,
                  .names = "{.col}_usd"))


# Exercise 3: Handle outliers ----
# notes: DescTools can also be used for winsorization

# you can use custom Winsorization function to handle outliers.
winsor_function <- function(dataset, var, min = 0.00, max = 0.95) {
    var_sym <- sym(var)
    
    percentiles <- quantile(
        dataset %>% pull(!!var_sym), probs = c(min, max), na.rm = TRUE
    )
    
    min_percentile <- percentiles[1]
    max_percentile <- percentiles[2]
    
    dataset %>%
        mutate(
            !!paste0(var, "_w") := case_when(
                is.na(!!var_sym) ~ NA_real_,
                !!var_sym <= min_percentile ~ percentiles[1],
                !!var_sym >= max_percentile ~ percentiles[2],
                TRUE ~ !!var_sym
            )
        )
}

# Tips: Apply the Winsorization function to the relevant variables.
# Create a list of variables that require Winsorization and apply the function to each.

win_vars <- c("area_acre","food_cons_usd","nonfood_cons_usd")
for (var in win_vars) {
    hh_data <- winsor_function(hh_data, var)
}

hist(hh_data$food_cons_usd)
hist(hh_data$food_cons_usd_w)

# Exercise 4.1: Create indicators at household level ----
# Instructions:
# Collapse HH-member level data to HH level.
# Plan to create the following indicators:
# 1. Any member was sick.
# 2. Any member can read/write.
# 3. Average sick days.
# 4. Total treatment cost in USD.


mem_data_collapse <- mem_data %>%
    group_by(hhid) %>%
    summarize(is_sick = max(sick, na.rm=TRUE),
              is_read = max(read, na.rm=TRUE),
              avg_sick = mean(days_sick, na.rm=TRUE),
              tot_treat_cost = sum(treat_cost, na.rm=TRUE) * usd_conv) %>%
    ungroup() %>%
    set_variable_labels(is_sick = "Any member is sick",
                        is_read = "Any member can read",
                        avg_sick = "Average number of sick days",
                        tot_treat_cost = "Total treatment cost in USD")

# Exercise 4.2: Data construction: Secondary data ----
# Instructions:
# Calculate the total number of medical facilities by summing relevant columns.
# Apply appropriate labels to the new variables created.

# we use rowSums to account for missing values (simple addition of columns won't take care of corner cases)
secondary_data_collapse <- secondary_data %>%
    mutate(n_medical = rowSums(select(., n_clinic, n_hospital), 
                               na.rm=TRUE)) %>%
    set_variable_labels(n_hospital = "Number of hospitals",
                        n_school = "Number of schools",
                        n_clinic = "Number of clinics",
                        n_medical = "Total medical facilities")

# Exercise 5: Merge HH and HH-member data ----
# Instructions:
# Merge the household-level data with the HH-member level indicators.
# After merging, ensure the treatment status is included in the final dataset.

#merged_hh_mem <- merge(hh_data, mem_data_collapse, by='hhid')

merged_hh_mem <- hh_data %>% left_join(mem_data_collapse, by = "hhid")

treat_status <- read_dta(file.path(data_path, "Raw/treat_status.dta"))

final_hh <- merged_hh_mem %>% left_join(treat_status, by = "vid")

# Exercise 6: Save final dataset ----
# Instructions:
# Only keep the variables you will use for analysis.
# Save the final dataset for further analysis.
# Save both the HH dataset and the secondary data.

# Tip: Ensure all variables are correctly labeled 

write_dta(final_hh, file.path(data_path, "Final/TZA_CCT_analysis_MK.dta"))
write_dta(secondary_data_collapse, file.path(data_path, "Final/TZA_amenity_analysis_MK.dta"))

