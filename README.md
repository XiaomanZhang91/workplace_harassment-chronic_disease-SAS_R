# Association Between Workplace Harassment and Chronic Disease

## Background

This project uses longitudinal data from employees of a Midwestern university, originally sampled in 1996 and followed through multiple mail and web-based surveys up to 2021, to study the long-term health impacts of workplace harassment.

## Project Aim

To characterize the association between prior exposure to workplace harassment—both **sexual harassment (SEQ)** and **non-sexual/generalized harassment (NSH)**—and long-term health outcomes (**chronic disease**), and to examine the **potential mediating effect of depression (CESD)**.

---

## Study Design & Modeling Framework

This project uses a **mediation triangle design** with three core models:

- **Model 1**:  
  Cox proportional hazards model evaluating the **total effect** of workplace harassment (NSH/SEQ) on time to chronic disease.  
- **Model 2**:  
  Linear **mixed-effects model** assessing the effect of NSH/SEQ on **depression scores (CESD)** over time.  
- **Model 3**:  
  Cox model including NSH/SEQ and CESD to assess the **direct effect** and test **mediation**.

All models were adjusted for baseline characteristics: **age**, **gender**, **race**, and **occupation group**.

---

## Variable Definitions

### Baseline Covariates
| Variable     | Description                             |
|--------------|-----------------------------------------|
| `caseid`     | Study identification number             |
| `age`        | Age at baseline                         |
| `female`     | Gender: 0 = male, 1 = female            |
| `white`      | Race: 1 = White, 0 = Other              |
| `group`      | Occupation group (1 = Admin, 2 = Faculty, 3 = RA/TA/Resident, 4 = Service) |

### Key Time-Varying Variables
- **Harassment**
  - Sexual Harassment (SEQ): `seq_bl`, `seq_yr01`, `...`, `seq_yr23`
  - Non-sexual Harassment (NSH): `nsh_bl`, `nsh_yr01`, `...`, `nsh_yr23`
- **Depression (CESD)**: `cesd_bl`, `cesd_yr01`, `...`, `cesd_yr23`
- **Outcome**:  
  - `anydiag_yrs`: time to chronic disease  
  - `anydiag_status`: disease indicator (1 = event, 0 = censored)

> **Note:** Missing values coded as `999` or `` were handled and imputed using LOCF.

---

## Analytical Tools & Skills Demonstrated

- **Data Wrangling & Cleaning**
  - Wide-to-long transformation using `pivot_longer()` (R) and `ARRAY`/`RENAME` (SAS)
  - Re-coding categorical variables, labeling, and variable engineering (e.g. age groups)
  - Advanced use of `PROC SQL`, `PROC FORMAT`, `dplyr`, `data.table`

- **Missing Data Handling**
  - Defined custom thresholds for exclusion
  - Imputed missing time-varying data using **Last Observation Carried Forward (LOCF)**

- **Longitudinal Modeling**
  - Linear mixed-effects models with random intercepts/slopes
  - Tested multiple covariance structures (AR(1), ARMA(1,1), TOEP)
  - Decomposed time-varying effects (within vs. between subjects)

- **Survival Analysis**
  - Time-varying Cox models using `survSplit()` (R) and `PROC PHREG` (SAS)
  - Proportional hazards assumption checks using interaction terms
  - Mediation analysis through comparison of nested models (likelihood ratio tests)

---

## Files

| File            | Description                                        |
|------------------|----------------------------------------------------|
| `workplace_harassment-chronic_disease.sas`   | Full SAS pipeline: cleaning, modeling, diagnostics |
| `workplace_harassment-chronic_disease.R`       | R code: data wrangling, modeling, plots            |
| `summary.png`    | Visual diagram of mediation model                  |


---

