# Reproducible Research Fundamentals 
# 03. Data Analysis

# Libraries
library(haven)
library(dplyr)
library(modelsummary)
library(stargazer)
library(ggplot2)
library(tidyr)

# Load data 
#household level data
data_path <- "C:/Users/wb572332/OneDrive - WBG/DataWork/DataWork/Data"
hh_data   <- read_dta(file.path(data_path, "Final/TZA_CCT_analysis.dta"))

# secondary data 
secondary_data <- read_dta(file.path(data_path, "Final/TZA_amenity_analysis.dta")) %>%
    mutate(district = as_factor(district))

# Summary statistics ----

glimpse(hh_data)

hh_data_subset <- hh_data %>%
    select(hh_size, n_child_5, n_child_17, n_adult, n_elder, food_cons_usd_w, 
           nonfood_cons_usd_w, read, sick, days_sick, district, treatment)

# Create summary statistics by district and export to CSV
summary_table <- datasummary(
    All(hh_data_subset) ~ to_factor(district) * (Mean + SD), 
    data = hh_data_subset,
    title = "Summary Statistics by District",
    output = file.path("Outputs", "summary_table_.csv")  # Change to CSV
)

summary_table


# Balance table ----

balance_table <- datasummary_balance(
    hh_size + n_child_5 + n_child_17 + n_adult + n_elder + food_cons_usd_w + nonfood_cons_usd_w +
        read + sick + days_sick ~ treatment,
    data = hh_data_subset,
    stars = TRUE,
    title = "Balance by Treatment Status",
    note = "Includes HHS with observations for baseline and endline",
    output = file.path("Outputs", "balance_table_.csv")  # Change to CSV
)

balance_table 

# Regressions ----

# Model 1: Food consumption regressed on treatment
model1 <- lm(food_cons_usd_w ~ treatment, data = hh_data)

# Model 2: Add controls (crop_damage, drought_flood)
model2 <- lm(food_cons_usd_w ~ treatment + crop_damage, drought_flood, data = hh_data)

# Model 3: Add FE by district
model3 <- lm(food_cons_usd_w ~ treatment + crop_damage, drought_flood + factor(district), data = hh_data)

# Create regression table using stargazer
stargazer(
    model1, model2, model3,
    title = "Food Consumption Effects",
    keep = c("treatment", "crop_damage", "drought_flood"),
    covariate.labels = c("Treatment",
                         "Crop Damage",
                         "Drought/Flood"),
    dep.var.labels = c("Food Consumption (USD)"),
    dep.var.caption = "",
    add.lines = list(c("District Fixed Effects", "No", "No", "Yes")),
    header = FALSE,
    keep.stat = c("n", "adj.rsq"),
    notes = "Standard errors in parentheses",
    out = file.path("Outputs","regression_table_.tex")
)

#use huxtable to export to excel

# Graphs: Area cultivated by treatment assignment across districts ----

# Bar graph by treatment for all districts
# Ensure treatment is a factor for proper labeling
hh_data_plot <- hh_data %>%
    mutate(treatment = factor(treatment, labels = c("Control", "Treatment")), 
           district = as_factor(district))

# Create the bar plot
ggplot(hh_data_plot, aes(x = treatment , y = area_acre_w, fill = treatment)) +
    geom_bar(stat="summary", fun="mean", position="dodge") +
    #geom_text(stat="summary", fun="mean", position="dodge") +  # Add text labels
    facet_wrap(~district) +  # Facet by district
    labs(title = "Area cultivated by treatment assignment across districts",
         x = NULL, y = "Average area cultivated (Acre)") +  # Remove x-axis title
    theme_minimal()
 # Add other customization if needed

ggsave(file.path("Outputs", "fig1_.png"), width = 10, height = 6)


# Graphs: Distribution of non-food consumption by female-headed households ----

# Calculate mean non-food consumption for female and male-headed households
mean_female <- hh_data %>% 
    filter(female_head == 1) %>% 
    summarise(mean = mean(nonfood_cons_usd_w, na.rm = TRUE)) %>% 
    pull(mean)

mean_male <- hh_data %>% 
    filter(female_head == 0) %>% 
    summarise(mean = mean(nonfood_cons_usd_w, na.rm = TRUE)) %>% 
    pull(mean)

# Create the density plot
ggplot(hh_data, 
       aes(x = nonfood_cons_usd_w)) +
    geom_density() +  # Density plot
    geom_vline(xintercept = mean_female, color = "purple", linetype = "dashed", size = 1) +  # Vertical line for female mean
    geom_vline(xintercept = mean_male, color = "grey", linetype = "dashed", size = 1) +  # Vertical line for male mean
    labs(title = "Distribution of Non-Food Consumption",
         x = "Non-food consumption value (USD)", 
         y = "Density",
         color = "Household Head:") +  # Custom labels
    theme_minimal() 
    # Add other customization if needed

ggsave(file.path("Outputs", "fig2_.png"), width = 10, height = 6)

# Graphs: Secondary data ----

long_data <- secondary_data %>%
    ungroup() %>% 
    select(-c(n_hospital, n_clinic)) %>% 
    pivot_longer(cols = c(n_school, n_medical), names_to = "amenity", values_to = "count") %>%
    mutate(amenity = recode(amenity, n_school = "Number of Schools", n_medical = "Number of Medical Facilities"),
           in_sample = if_else(district %in% c("Kibaha", "Chamwino", "Bagamoyo"), "In Sample", "Not in Sample"))

# Create the facet-wrapped bar plot
ggplot(long_data,
       aes(x = reorder(district, count), y = count)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    facet_wrap(~amenity) +  # Create facets for schools and medical facilities
    labs(title = "Access to Amenities: By Districts",
         x = "District", y = NULL, fill = "Districts:") +
    scale_fill_brewer(palette="PuRd") +
    theme_minimal()
    # Add other customization if needed

ggsave(file.path("Outputs", "fig3_.png"), width = 10, height = 6)
