library(janitor)
library(ggplot2)
library(tidyverse)
library(sf)
library(dplyr)

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

# Convert relevant columns to numeric
selected_data <- selected_data |> 
  mutate(across(
    starts_with("drug_overdose_deaths_raw_value") | starts_with("alcohol_impaired_driving_deaths_raw_value") |
      starts_with("unemployment_raw_value") | starts_with("income_inequality_raw_value") | 
      starts_with("high_school_completion_raw_value") | starts_with("adult_smoking_raw_value") |
      starts_with("excessive_drinking_raw_value") | starts_with("median_household_income_raw_value") |
      starts_with("some_college_raw_value") | starts_with("children_in_poverty_raw_value") | 
      starts_with("children_in_single_parent_households_raw_value") | 
      starts_with("social_associations_raw_value") | starts_with("x_below_18_years_of_age_raw_value") | 
      starts_with("x_65_and_older_raw_value") | starts_with("x_non_hispanic_black_raw_value") | 
      starts_with("x_american_indian_or_alaska_native_raw_value") | starts_with("x_asian_raw_value") | 
      starts_with("x_native_hawaiian_or_other_pacific_islander_raw_value") | starts_with("x_hispanic_raw_value") | 
      starts_with("x_non_hispanic_white_raw_value") | starts_with("x_not_proficient_in_english_raw_value") | 
      starts_with("x_female_raw_value") | starts_with("x_rural_raw_value"),
    as.numeric
  ))

selected_data %>% 
  mutate(across(everything(), ~ifelse(is.na(.) | . == 0, mean(., na.rm = TRUE), .))) 

# summary of variables for drug overdose ---------------------------------------------------------
model_drug <- lm(drug_overdose_deaths_raw_value ~ unemployment_raw_value + income_inequality_raw_value + 
                   high_school_completion_raw_value + adult_smoking_raw_value + excessive_drinking_raw_value + 
                   median_household_income_raw_value + some_college_raw_value + children_in_poverty_raw_value + 
                   children_in_single_parent_households_raw_value + social_associations_raw_value + 
                   x_below_18_years_of_age_raw_value + x_65_and_older_raw_value + 
                   x_non_hispanic_black_raw_value + x_american_indian_or_alaska_native_raw_value + 
                   x_asian_raw_value + x_native_hawaiian_or_other_pacific_islander_raw_value + 
                   x_hispanic_raw_value + x_non_hispanic_white_raw_value + 
                   x_not_proficient_in_english_raw_value + x_female_raw_value + 
                   x_rural_raw_value, data = selected_data)

summary(model_drug)

# summary of variables for alcohol impaired deaths --------------------------------------------------------------------------------
model_alcohol <- lm(alcohol_impaired_driving_deaths_raw_value ~ unemployment_raw_value + income_inequality_raw_value + 
                      high_school_completion_raw_value + adult_smoking_raw_value + excessive_drinking_raw_value + 
                      median_household_income_raw_value + some_college_raw_value + children_in_poverty_raw_value + 
                      children_in_single_parent_households_raw_value + social_associations_raw_value + 
                      x_below_18_years_of_age_raw_value + x_65_and_older_raw_value + 
                      x_non_hispanic_black_raw_value + x_american_indian_or_alaska_native_raw_value + 
                      x_asian_raw_value + x_native_hawaiian_or_other_pacific_islander_raw_value + 
                      x_hispanic_raw_value + x_non_hispanic_white_raw_value + 
                      x_not_proficient_in_english_raw_value + x_female_raw_value + 
                      x_rural_raw_value, data = selected_data)

summary(model_alcohol)


#maps for drug overdose  -------------------------------------------------------------------------
library(maps)
state_data <- selected_data |>
  filter(county_fips_code=='000',state_abbreviation!="US", state_abbreviation !="DC") |>  
  rename(state=name) 
state_data$state <- tolower(state_data$state)

us_states_map <- map_data("state")

merged_data <- us_states_map %>%
  left_join(state_data, by = c("region" = "state"))

ggplot(data = merged_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = drug_overdose_deaths_raw_value), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50") +
  coord_fixed(1.3)+
 theme(axis.title= element_blank(), 
       axis.text=element_blank()) +
  labs(title="Drug Overdose Deaths by State", 
       fill="Drug Overdose Deaths")

ggplot(data = merged_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = adult_smoking_raw_value), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50") +
  coord_fixed(1.3)+
  theme(axis.title= element_blank(), 
        axis.text=element_blank()) +
  labs(title= "Adult Smoking")

# west virginia can be examined more because what in the hell is
# going on in west virginia ?? 



# map for alcohol impaired driving deaths-------------------------- 

ggplot(data = merged_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = alcohol_impaired_driving_deaths_raw_value), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50") +
  coord_fixed(1.3) +
  theme(axis.title= element_blank(), 
        axis.text=element_blank()) +
  labs(title= "Alcohol Impaired Driving Deaths by State", 
       fill="Alcohol Impaired Driving Deaths")

# taking a closer look at montana

ggplot(data = merged_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = x_female_raw_value), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50") +
  coord_fixed(1.3) + 
  labs(title="Females")

# taking a closer look at west virginia for drug overdose -------------------------------------------
west_virginia_data <- selected_data |>  
  filter(state_fips_code==54, name !="West Virginia") 

west_virginia_data$county <- c("barbour","berkeley","boone","braxton","brooke","cabell", "calhoun", "clay",
                               "doddridge","fayette","gilmer","grant","greenbrier","hampshire","hancock","hardy", 
                               "harrison","jackson","jefferson","kanawha","lewis","lincoln","logan","mcdowell", 
                               "marion","marshall","mason","mercer","mineral","mingo","monongalia", "monroe", 
                               "morgan","nicholas","ohio", "pendleton", "pleasants","pocahontas","preston", 
                               "putnam","raleigh","randolph","ritchie","roane ","summers","taylor","tucker","tyler",
                               "upshur", "wayne","webster","wetzel","wirt","wood","wyoming")

# Get map data for counties in West Virginia
counties_map <- map_data("county")
west_virginia_counties <- subset(counties_map, region == "west virginia")
west_virginia_merged <- left_join(west_virginia_counties, west_virginia_data, by = c("subregion" = "county"))

west_virginia_merged <- west_virginia_counties %>%
  left_join(west_virginia_data, by = c("subregion" = "county"))

ggplot(data = west_virginia_merged, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=drug_overdose_deaths_raw_value),color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "black") +
  coord_fixed(1.3) +
  theme(axis.title= element_blank(), 
        axis.text=element_blank()) +
  labs(title= "Drug Overdose Deaths by County", 
       fill="Number of Deaths")










