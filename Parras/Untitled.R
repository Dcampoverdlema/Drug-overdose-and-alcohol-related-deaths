library(janitor)
library(ggplot2)
library(tidyverse)
library(sf)

health_data <- read.csv("data/analytic_data2024.csv") %>%  
  clean_names()

selected_data <- health_data %>% 
  select(
    state_fips_code, county_fips_code, state_abbreviation, name,
    drug_overdose_deaths_raw_value, alcohol_impaired_driving_deaths_raw_value,
    unemployment_raw_value, income_inequality_raw_value, high_school_completion_raw_value, 
    adult_smoking_raw_value, excessive_drinking_raw_value, median_household_income_raw_value,
    some_college_raw_value, children_in_poverty_raw_value, children_in_single_parent_households_raw_value,
    social_associations_raw_value, x_below_18_years_of_age_raw_value, 
    x_65_and_older_raw_value, x_non_hispanic_black_raw_value, 
    x_american_indian_or_alaska_native_raw_value, x_asian_raw_value, 
    x_native_hawaiian_or_other_pacific_islander_raw_value, x_hispanic_raw_value, 
    x_non_hispanic_white_raw_value, x_not_proficient_in_english_raw_value, 
    x_female_raw_value, x_rural_raw_value
  )

selected_data %>% 
  mutate(across(everything(), ~ifelse(is.na(.) | . == 0, mean(., na.rm = TRUE), .))) 
  
state_data <- selected_data %>%  
  filter(county_fips_code=="000",state_abbreviation!="US", state_abbreviation!="DC")

state_data <- state_data %>%
  rename(state = name)

plot_usmap(data = state_data, values = "drug_overdose_deaths_raw_value", labels=FALSE)         
         
  


