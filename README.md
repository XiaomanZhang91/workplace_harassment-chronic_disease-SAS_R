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

### Analytical Tools & Skills Demonstrated

This project was initially developed in **SAS**, which handled the bulk of the data cleaning, transformation, and modeling. Later, **R code** was added to demonstrate proficiency with the same analytical workflow in a different environment.

#### Programming Languages & Tools
- **SAS**: `PROC SQL`, `PROC FORMAT`, `PROC MIXED`, `PROC PHREG`, DATA step arrays, regular expression
- **R**: `dplyr`, `tidyr`, `ggplot2`, `data.table`, `nlme`, `survival`, `table1`, regular expression

#### Data Management & Transformation
- Wide-to-long restructuring using `pivot_longer()` (R) and `ARRAY`/`RENAME` (SAS)
- Variable recoding, labeling, and engineering (e.g., age groups)
- Consistent missing data handling using LOCF

#### Longitudinal Modeling
- Fitted mixed-effects models with `nlme` (R) and `PROC MIXED` (SAS)
- Systematic selection of:
  - Random effects (intercepts/slopes)
  - Covariance structures (e.g., AR(1), ARMA(1,1), TOEP)
  - Fixed effects, including decomposition of within vs. between-subject effects

#### Survival Analysis & Mediation
- Time-to-event modeling with time-varying covariates via `survival` (R) and `PROC PHREG` (SAS)
- Proportional hazards checks using interaction terms and visual diagnostics
- Mediation analysis by comparing nested models using likelihood ratio tests

---

## Files

| File            | Description                                        |
|------------------|----------------------------------------------------|
| `workplace_harassment-chronic_disease.sas`   | SAS code: cleaning, plots, modeling |
| `workplace_harassment-chronic_disease.R`       | R code: cleaning, plots, modeling     |


---

