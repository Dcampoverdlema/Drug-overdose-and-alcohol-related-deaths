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


imputed_data <- read_csv("imputed_data.csv") |> 
  mutate(across(everything(), ~ifelse(is.na(.) | . == 0, mean(., na.rm = TRUE), .))) |> 
  rename(
    ID = ...1,
    Drug_Overdose_Deaths = drug_overdose_deaths_raw_value,
    Alcohol_Impaired_Driving_Deaths = alcohol_impaired_driving_deaths_raw_value,
    Unemployment_Rate = unemployment_raw_value,
    Income_Inequality = income_inequality_raw_value,
    Highschool_Comp_Rate = high_school_completion_raw_value,
    Adult_Smoking_Rate = adult_smoking_raw_value,
    Excessive_Drinking_Rate = excessive_drinking_raw_value,
    Median_House_Income = median_household_income_raw_value,
    College_Education_Rate = some_college_raw_value,
    Children_In_Poverty = children_in_poverty_raw_value,
    Single_Parent_House = children_in_single_parent_households_raw_value,
    Social_Associations = social_associations_raw_value,
    Pop_Under_18 = x_below_18_years_of_age_raw_value,
    Pop_Over_65 = x_65_and_older_raw_value,
    NH_Black = x_non_hispanic_black_raw_value,
    American_Or_Alaska_Native = x_american_indian_or_alaska_native_raw_value,
    Asian_Pop = x_asian_raw_value,
    Pacific_Islander = x_native_hawaiian_or_other_pacific_islander_raw_value,
    Hispanic_Pop = x_hispanic_raw_value,
    NH_White = x_non_hispanic_white_raw_value,
    English_Profic = x_not_proficient_in_english_raw_value,
    Female_Pop = x_female_raw_value,
    Rural_Pop = x_rural_raw_value
  )


# use new_data for alcohol_impaired death rates
# use new_df for drug overdose 


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
  scale_fill_viridis_c(option = "plasma", na.value = "grey50", labels = scales::percent_format(scale = 100)) +
  coord_fixed(1.3) +
  theme_minimal() +  # Apply the minimal theme
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),  # Remove axis ticks
    text = element_text(family = "serif"),  # Set global text family to serif
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Center, bold the title, and increase font size
    plot.subtitle = element_text(hjust = 0.5, face = "plain", size = 8),  # Center the subtitle, normal weight, and reduce font size
    plot.caption = element_text(face = "italic", size = 6, hjust = 0)  # Italicize, reduce font size of the caption, and left-align
  ) +
  labs(
    title = "Alcohol Impaired Driving Deaths by State",
    subtitle = "Data source: County Health Rankings & Roadmaps (https://www.countyhealthrankings.org/)",
    fill = "*Alcohol Driving Deaths",
    caption = "*Percentage of motor vehicle crash deaths with alcohol involvement."
  )

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
  scale_fill_viridis_c(option = "plasma", na.value = "lightgrey") +
  coord_fixed(1.3) +
  theme(axis.title= element_blank(), 
        axis.text=element_blank()) +
  labs(title= "Drug Overdose Deaths by County", 
       fill="Number of Deaths")

ggplot(data = west_virginia_merged, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=median_household_income_raw_value),color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "lightgrey") +
  coord_fixed(1.3) +
  theme(axis.title= element_blank(), 
        axis.text=element_blank()) +
  labs(title= "Median Income by County ", 
       fill="Median Income ")

west_virginia_data |>  
  ggplot(aes(x=median_household_income_raw_value, y=drug_overdose_deaths_raw_value))+
  geom_point() +
  geom_smooth(method = "lm", linewidth = 2, color="red") +
  labs( title="Drug Overdose and Median Income",
    y = "Drug Overdose Deaths", x="Median Household Income")




# correlation  -----------------------------------------------------------------

cor_matrix_drug <- cor(imputed_data)
cor_matrix_alc <- cor(imputed_data)


# DRUG
drug_correlations <- cor_matrix_d["Drug_Overdose_Deaths", ]

# Convert to dataframe for ggplot
drug_cor_df <- data.frame(
  Variable = names(drug_correlations),
  Correlation = as.numeric(drug_correlations)
)

# Plot
ggplot(drug_cor_df, aes(x = reorder(Variable, Correlation), y = Correlation, fill = Correlation > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flips the axes for better readability
  labs(title = "Correlation with Drug Overdose Deaths", x = "Variable", y = "Correlation") +
  scale_fill_manual(values = c("red", "blue"), name = "Correlation Type",
                    labels = c("Negative", "Positive")) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif", size = 12),
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.background = element_rect(fill = "white", colour = "white")
  )


# ALC !!
alcohol_correlations <- cor_matrix_a["Alcohol_Impaired_Driving_Deaths", ]

# Convert to dataframe for ggplot
alcohol_cor_df <- data.frame(
  Variable = names(alcohol_correlations),
  Correlation = as.numeric(alcohol_correlations)
)

# Plot
ggplot(alcohol_cor_df, aes(x = reorder(Variable, Correlation), y = Correlation, fill = Correlation > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flips the axes for better readability
  labs(title = "Correlation with Alcohol-Impaired Driving Deaths", x = "Variable", y = "Correlation") +
  scale_fill_manual(values = c("red", "blue"), name = "Correlation Type",
                    labels = c("Negative", "Positive")) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif", size = 12),
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.background = element_rect(fill = "white", colour = "white")
  )


# ----------------------------------------------------------------------------
health_data_new <- read.csv("data/health_data_selected.csv") |>  
  slice(-1) |> 
  mutate(across(everything(), ~ifelse(is.na(.) | . == 0, mean(., na.rm = TRUE), .))) |>  
  select(alcohol_impaired_driving_deaths, unemployment, median_household_income, 
         children_in_single_parent_households,social_associations, drug_overdose_deaths, 
         disconnected_youth, homicides, suicides,percent_below_18_years_of_age, percent_65_and_older, 
         percent_native_hawaiian_or_other_pacific_islander, percent_non_hispanic_black, 
         percent_hispanic, percent_female, -living_wage)



#----------
health_data_new <- read.csv("data/final_data.csv") |>  
  mutate(across(everything(), ~ifelse(is.na(.) | . == 0, mean(., na.rm = TRUE), .))) |>  
  select(-name,-X,-population)


health_data_new %>%  
  ggplot(aes(x=unemployment, y=drug_overdose_deaths)) +
  geom_point(color="blue",size=3,alpha=0.5) +
  theme_light()





