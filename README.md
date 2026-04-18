# Understanding the Increase in Drug Overdose and Alcohol-Impaired Driving Deaths

**Authors:** Dennis Campoverde-Lema, Samantha Parras, Jodie Chen  
**Date:** July 26, 2024  
**Institution:** Carnegie Mellon University — Summer Research Program

---

## Overview

This project investigates whether demographic and social factors can predict drug overdose deaths and alcohol-impaired driving deaths across U.S. counties. Data was sourced from the **University of Wisconsin Population Health Institute's County Health Rankings** dataset (2024), which originally contained 770 variables across 3,000+ counties.

**Research Question:** Are there demographic and social factors that are predictors of drug overdose and alcohol-related driving incidents?

**Hypothesis:** Drug overdoses and alcohol-impaired driving incidents can be predicted by demographic factors (age, gender, race) and social factors (substance use habits, socioeconomic status).

---

## Key Findings

- **Unemployment** is the top predictor for both drug overdose deaths and alcohol-impaired driving deaths
- **Adult smoking rate** and **children in single-parent households** are strong predictors of drug overdose deaths
- **Excessive drinking** and **American Indian or Alaska Native population percentage** are significant predictors of alcohol-impaired driving deaths
- Social predictors were supported by the data, but demographic predictors alone did not show sufficient evidence — making the hypothesis **half correct**

---

## Data

- **Source:** [County Health Rankings](https://www.countyhealthrankings.org/health-data) — University of Wisconsin Population Health Institute
- **Year:** 2024
- **Unit of Analysis:** U.S. county-level
- **Final dataset size:** ~1,000 observations (reduced from ~3,000 due to missing drug overdose data)

### Key Predictor Variables

| Variable | Rationale |
|---|---|
| Unemployment | Linked to psychological stress and increased substance abuse risk |
| Median Household Income | Lower income associated with worse mental and physical health outcomes |
| Disconnected Youth | Higher likelihood of smoking, drinking, and drug use |
| High School Graduation | Higher education linked to lower smoking rates and better employment |
| Social Associations | Weak social networks linked to higher illness rates and unhealthy behaviors |
| Adult Smoking Rate | Strong correlation with drug overdose deaths |
| Excessive Drinking | Direct predictor of alcohol-impaired driving deaths |

---

## Methods

### Exploratory Data Analysis (EDA)
- Choropleth maps of drug overdose deaths and alcohol-impaired driving deaths by state
- Scatterplots examining relationships between smoking rates, income, and overdose deaths

### Models Used

**For Alcohol-Impaired Driving Deaths:**
- Linear Regression
- Mixed Effects / Random Intercept Linear Model

**For Drug Overdose Deaths:**
- Linear Regression
- Random Forest (with variable importance and partial dependence plots)

---

## Repository Structure

```
├── data/                  # Raw and processed datasets
├── analysis/              # Final modeling scripts
├── DrugOverdoseAndAlcoholRelatedDeaths.qmd   # Final report (Quarto)
├── DrugOverdoseAndAlcoholRelatedDeaths.html  # Rendered report
├── final_data.Rmd         # Final data preparation script
└── README.md
```

---

## Limitations

- County-level aggregation masks within-county variation (e.g., large counties like Los Angeles)
- Significant missing data on drug overdose deaths reduced the dataset from ~3,000 to ~1,000 observations
- State-level drug regulations may confound overdose rate comparisons

---

## Policy Recommendations

To address unemployment as the top risk factor:
- **Career pathways programs** — occupation-specific training for high-growth industries
- **Summer youth employment programs** — targeting disconnected youth ages 14–24
- **Tax credits for hiring** — incentivizing workforce expansion among employers

---

## References

- American Addiction Centers. *Alcohol and Drug Abuse Statistics.* https://americanaddictioncenters.org/addiction-statistics
- County Health Rankings. *Health Data.* https://www.countyhealthrankings.org/health-data
- Generes, W. M. *Recession, Unemployment, and Drug Addiction.* American Addiction Centers, 2024.
- Lerman, R. I. *Five Steps to Cut Unemployment.* Urban Institute, 2011.
- Forbes Advisor. *Worst States For Drunk Driving In 2024.*
