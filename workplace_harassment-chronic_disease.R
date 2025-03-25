
# 1. Raw Data Exploration

# 1.1. Import Data
library(tidyverse)
library(table1)
library(flextable)
library(readxl)
library(survival)


# Import data
# Import data
shn0 <- read_excel("SHNSH.xlsx")


# Factorize categorical variables and add labels
# Factorize categorical variables and add labels
shn0 <- shn0 %>%
  mutate(
    female = factor(female, levels = c(0, 1), labels = c("Male", "Female")),
    white = factor(white, levels = c(0, 1), labels = c("Other Race", "White")),
    group = factor(group, 
                        levels = c(1, 2, 3, 4),
                        labels = c("Secretarial/Admin", "Faculty", "RA/TA/Resident", "Service Maintenance")),
    anydiag_status = factor(anydiag_status, 
                            levels = c(0, 1),
                            labels = c("Censored", "Event"))
  )

label(shn0$caseid) <- "Study identification numbers"
label(shn0$age) <- "Age"
label(shn0$female) <- "Gender"
label(shn0$white) <- "Race"
label(shn0$group) <- "Occupation group"
label(shn0$anydiag_yrs) <- "Length of follow-up"
label(shn0$anydiag_status) <- "Chronic Disease"


# Categorize age based on the histogram
# Categorize age based on the histogram

# hist(shn0$age)
# hist(shn0$age)
shn0 <- shn0 %>%
  mutate(
    age_group = cut(
      age,
      breaks = c(20, 30, 40, 50, Inf),
      labels = c("20-30", "31-40", "41-50", "50-70"),
      right = FALSE  # Include the lower bound, exclude the upper
    )
  )


# Unify the format of missing values
# Unify the format of missing values
shn0 <- shn0 %>%
  mutate(across(starts_with("cesd_"), ~ ifelse(. == 999, NA, .)))


# Rename nsh/seq/cesd_bl to _yr00 (better name after transforming it into the long data)
# Rename nsh/seq/cesd_bl to _yr00 (better name after transforming it into the long data)
shn0 <- shn0 %>%
  rename(nsh_yr00 = nsh_bl,
         seq_yr00 = seq_bl,
         cesd_yr00 = cesd_bl)


# Rename baseline variables: "nsh_bl" → "nsh_0".
# Rename baseline variables: "nsh_bl" → "nsh_0".
names(shn0) <- gsub("_bl$", "_0", names(shn0))


# Rename year variables: e.g. "nsh_yr01" becomes "nsh_1"
# Rename year variables: e.g. "nsh_yr01" becomes "nsh_1"
names(shn0) <- gsub("_yr0*(\\d+)", "_\\1", names(shn0))


# 1.2. Summary Statistics for Raw Data

# Summary statistics table by event
# Summary statistics table by event
shn0_tbl <- shn0 %>% 
  select(-caseid,-anydiag_yrs,-age)
tbl_1 <- table1(~.|anydiag_status, overall=T, data=shn0_tbl)
t1flex(tbl_1)


# Median survival time
# Median survival time
km_age <- survfit( Surv(anydiag_yrs, anydiag_status) ~ age_group, data=shn0 )
summary(km_age, times = 0.5)$quantile

# Note: the median survival time was not automatically calculated because no median survival time is reached for some groups. Use SAS to get the median survival time.
## Note: the median survival time was not automatically calculated because no median survival time is reached for some groups. Use SAS to get the median survival time.



# 1.3. Plot

# 1.3.1 The distributions of continuous variables
hist(shn0$age)
hist(shn0$nsh_bl)
hist(shn0$seq_bl)
hist(shn0$cesd_bl)


# 1.3.2. Time-varying covariates

# Get the long data for each time-varying covariate
# Get the long data for each time-varying covariate
nsh_yr <- shn0 %>%
  select(starts_with("nsh_"), anydiag_status, caseid) %>%
  pivot_longer(
    cols = starts_with("nsh_"),
    names_to = "year",
    values_to = "nsh") %>%
  mutate(year=as.numeric(str_remove(year, "nsh_")))

seq_yr <- shn0 %>%
  select(starts_with("seq_"), anydiag_status, caseid) %>%
  pivot_longer(
    cols = starts_with("seq_"),
    names_to = "year",
    values_to = "seq") %>%
  mutate(year=as.numeric(str_remove(year, "seq_")))

cesd_yr <- shn0 %>%
  select(starts_with("cesd_"), anydiag_status, caseid) %>%
  pivot_longer(
    cols = starts_with("cesd_"),
    names_to = "year",
    values_to = "cesd") %>%
  mutate(year=as.numeric(str_remove(year, "cesd_")))


# plot

## NSH over years by diagnosis
nsh_yr %>%
  mutate(anydiag_status = recode(anydiag_status, 
                                 "Censored" = "No Disease", 
                                 "Event" = "Disease")) %>%
  group_by(year, anydiag_status) %>%
  summarise(nsh_median = median(nsh, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = nsh_median, color = anydiag_status)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_line() +
  labs(
    title = "Median NSH Over Years by Diagnosis Status",
    x = "Year",
    y = "Median NSH",
    color = "Diagnosis Status"
  ) +
  scale_y_continuous(limits = c(25, 35))+
  theme_bw() +
  theme(
    text = element_text(size = 20), 
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 24),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
  )


## seq over years by diagnosis
seq_yr %>%
  mutate(anydiag_status = recode(anydiag_status, 
                                 "Censored" = "No Disease", 
                                 "Event" = "Disease")) %>%
  group_by(year, anydiag_status) %>%
  summarise(seq_median = median(seq, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = seq_median, color = anydiag_status)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_line() +
  labs(
    title = "Median SEQ Over Years by Diagnosis Status",
    x = "Year",
    y = "Median SEQ",
    color = "Diagnosis Status"
  ) +
  theme_bw() +
  scale_y_continuous(limits = c(14, 24))+
  theme(
    text = element_text(size = 20), 
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 24),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
  )

## cesd over years by diagnosis
cesd_yr %>%
  mutate(anydiag_status = recode(anydiag_status, 
                                 "Censored" = "No Disease", 
                                 "Event" = "Disease")) %>%
  group_by(year, anydiag_status) %>%
  summarise(cesd_median = median(cesd, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = cesd_median, color = anydiag_status)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_line() +
  labs(
    title = "Median CESD Over Years by Diagnosis Status",
    x = "Year",
    y = "Median CESD",
    color = "Diagnosis Status"
  ) +
  scale_y_continuous(limits = c(0,3.5))+
  theme_bw() +
  theme(
    text = element_text(size = 20), 
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 24),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
  )



# 1.3.3. Survival Function
library(survival)
library(survminer)


# THM: ggsurvplot only seems to work when anydiag_status is numeric 0/1
shn_raw <- read_excel("SHNSH.xlsx")

# KM estimate
km_fit_0 <- survfit(Surv(anydiag_yrs, anydiag_status) ~ 1, data = shn_raw)


# plot
ggsurvplot(
  km_fit_0, 
  data=shn_raw, 
  conf.int = TRUE, 
  risk.table = TRUE, 
  ggtheme = theme_bw(),
  xlab="Time (year)", 
  ylab="Survival Rate", 
  xlim=c(0,23),
  font.x = c(22),
  font.y = c(22),
  font.tickslab = c(18),
  font.main = c(22)
  )



# 1.3.4. Incidence
(culmulative_incidence <- table(shn0$anydiag_status)[[2]]/nrow(shn0))

# incidence per 1000 person year
(incidence_density <- table(shn0$anydiag_status)[[2]]/sum(shn0$anydiag_yrs)*1000)


# 1.3.5. Mean CESD Change Over Time

## cesd over years
cesd_yr %>%
  group_by(year) %>%
  summarise(cesd_mean = mean(cesd, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = cesd_mean)) +
  geom_point(size = 3, color="#4183c1") +
  geom_line(color="#4183c1") +
  labs(
    title = "Mean CESD Over Years",
    x = "Year",
    y = "Mean CESD"
  ) +
  scale_y_continuous(limits = c(1,4.5))+
  theme_bw() +
  theme(
    text = element_text(size = 20), 
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 24),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
  )

## cesd over years by diagnosis
cesd_yr %>%
  mutate(anydiag_status = recode(anydiag_status, 
                                 "Censored" = "No Disease", 
                                 "Event" = "Disease")) %>%
  group_by(year, anydiag_status) %>%
  summarise(cesd_mean = mean(cesd, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = cesd_mean, color = anydiag_status)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_line() +
  labs(
    title = "Mean CESD Over Years by Diagnosis Status",
    x = "Year",
    y = "Mean CESD",
    color = "Diagnosis Status"
  ) +
  scale_y_continuous(limits = c(0,4.5))+
  theme_bw() +
  theme(
    text = element_text(size = 20), 
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 24),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
  )





# 1.3.6 Correlation Among CESDs
library(corrplot)

cor_cesd <- shn0 %>%
  select(starts_with("cesd_"))%>%
  cor(use="pairwise.complete.obs")

corrplot(cor_cesd, 
         method = "color", 
         col = colorRampPalette(c("#4183c1", "white", "indianred"))(200), 
         tl.col = "black", 
         tl.cex = 2,
         addCoef.col = "black",     
         number.cex = 1.5,
         type = "lower",
         diag = FALSE
         ) 




# 2. Missing Data

# 2.1. Check Missing Data
shn1 <- shn0 %>%
  mutate(missing_nsh=rowSums(is.na(select(., starts_with("nsh")))),
         missing_seq=rowSums(is.na(select(., starts_with("seq")))),
         missing_cesd=rowSums(is.na(select(., starts_with("cesd"))))
         )


# Plot
library(ggplot2)

## SHN
ggplot(shn1, aes(x = missing_nsh)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size=8)+
  labs(title = "Histogram of Missing NSH", x = "Number of Missing Values", y = "Count") +
  scale_x_continuous(breaks = seq(0, 10, by = 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_bw() +
  theme(
    text = element_text(size = 20), 
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 24)
  )

## SEQ
ggplot(shn1, aes(x = missing_seq)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size=8)+
  labs(title = "Histogram of Missing SEQ", x = "Number of Missing Values", y = "Count") +
  scale_x_continuous(breaks = seq(0, 10, by = 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_bw() +
  theme(
    text = element_text(size = 20), 
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 24)
  )

## CESD
ggplot(shn1, aes(x = missing_cesd)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size=8)+
  labs(title = "Histogram of Missing CESD", x = "Number of Missing Values", y = "Count") +
  scale_x_continuous(breaks = seq(0, 10, by = 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_bw() +
  theme(
    text = element_text(size = 20), 
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 24)
  )



# 2.2. Handle Missing Data
# shn2: removed subjects with more than 7 NA nsh/seq, and more than 5 cesd
shn2 <- shn1 %>%
  filter(missing_nsh <= 7 & missing_seq <=7 & missing_cesd <=5)

# Handle baseline missing
shn3 <- shn2 %>%
  mutate(
    nsh_yr00 = coalesce(nsh_yr00, nsh_yr01, nsh_yr03, nsh_yr04, nsh_yr05, nsh_yr06, nsh_yr07, nsh_yr08, nsh_yr09,nsh_yr10, nsh_yr23),
    seq_yr00 = coalesce(seq_yr00, seq_yr01, seq_yr03, seq_yr04, seq_yr05, seq_yr06, seq_yr07, seq_yr08, seq_yr09, seq_yr10, seq_yr23),
    cesd_yr00 = coalesce(cesd_yr00, cesd_yr01, cesd_yr05, cesd_yr06, cesd_yr07, cesd_yr09, cesd_yr10, cesd_yr11, cesd_yr23)
    )


# LOCF
library(zoo)
nsh_cols <- c("nsh_yr00", "nsh_yr01", "nsh_yr03", "nsh_yr04", 
              "nsh_yr05", "nsh_yr06", "nsh_yr07", "nsh_yr08", 
              "nsh_yr09", "nsh_yr10", "nsh_yr23")
seq_cols <- c("seq_yr00", "seq_yr01", "seq_yr03", "seq_yr04",
              "seq_yr05", "seq_yr06", "seq_yr07", "seq_yr08", 
              "seq_yr09", "seq_yr10", "seq_yr23")
cesd_cols <- c("cesd_yr00", "cesd_yr01", "cesd_yr05","cesd_yr06",
               "cesd_yr07", "cesd_yr09", "cesd_yr10","cesd_yr11",
               "cesd_yr23")
shn3[nsh_cols] <- t(apply(shn3[nsh_cols], 1, function(x) {
  zoo::na.locf(x, na.rm = FALSE)
}))
shn3[seq_cols] <- t(apply(shn3[seq_cols], 1, function(x) {
  zoo::na.locf(x, na.rm = FALSE)
}))
shn3[cesd_cols] <- t(apply(shn3[cesd_cols], 1, function(x) {
  zoo::na.locf(x, na.rm = FALSE)
}))


# THM: na.locf() works along a vector (down the rows in a column), so this is wrong
# mutate(across(starts_with("nsh_"), ~ na.locf(., na.rm = FALSE)))




# 2.3. Summary Statistics & Plot after Handling Missing Data

# Summary statistics table by event
tbl_2 <- table1( ~ age_group + group + white + female |anydiag_status, overall=T, data=shn3)
t1flex(tbl_2)


# Plot

# Get the long data for each time-varying covariate
nsh_yr_cleaned <- shn3 %>%
  select(starts_with("nsh_"), anydiag_status, caseid) %>%
  pivot_longer(
    cols = starts_with("nsh_"),
    names_to = "year",
    values_to = "nsh") %>%
  mutate(year=as.numeric(str_remove(year, "nsh_yr")))

seq_yr_cleaned <- shn3 %>%
  select(starts_with("seq_"), anydiag_status, caseid) %>%
  pivot_longer(
    cols = starts_with("seq_"),
    names_to = "year",
    values_to = "seq") %>%
  mutate(year=as.numeric(str_remove(year, "seq_yr")))

cesd_yr_cleaned <- shn3 %>%
  select(starts_with("cesd_"), anydiag_status, caseid) %>%
  pivot_longer(
    cols = starts_with("cesd_"),
    names_to = "year",
    values_to = "cesd") %>%
  mutate(year=as.numeric(str_remove(year, "cesd_yr")))


# plot

## NSH over years by diagnosis
nsh_yr_cleaned %>%
  mutate(anydiag_status = recode(anydiag_status, 
                                 "Censored" = "No Disease", 
                                 "Event" = "Disease")) %>%
  group_by(year, anydiag_status) %>%
  summarise(nsh_median = median(nsh, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = nsh_median, color = anydiag_status)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_line() +
  labs(
    title = "Median NSH Over Years by Diagnosis Status  (Cleaned)",
    x = "Year",
    y = "Median NSH",
    color = "Diagnosis Status"
  ) +
  scale_y_continuous(limits = c(25, 35))+
  theme_bw() +
  theme(
    text = element_text(size = 20), 
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 24),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
  )


## seq over years by diagnosis
seq_yr_cleaned %>%
  mutate(anydiag_status = recode(anydiag_status, 
                                 "Censored" = "No Disease", 
                                 "Event" = "Disease")) %>%
  group_by(year, anydiag_status) %>%
  summarise(seq_median = median(seq, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = seq_median, color = anydiag_status)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_line() +
  labs(
    title = "Median SEQ Over Years by Diagnosis Status (Cleaned)",
    x = "Year",
    y = "Median SEQ",
    color = "Diagnosis Status"
  ) +
  scale_y_continuous(limits = c(14, 24))+
  theme_bw() +
  theme(
    text = element_text(size = 20), 
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 24),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
  )


## cesd over years by diagnosis
cesd_yr_cleaned %>%
  mutate(anydiag_status = recode(anydiag_status, 
                                 "Censored" = "No Disease", 
                                 "Event" = "Disease")) %>%
  group_by(year, anydiag_status) %>%
  summarise(cesd_median = median(cesd, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = cesd_median, color = anydiag_status)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_line() +
  labs(
    title = "Median CESD Over Years by Diagnosis Status  (Cleaned)",
    x = "Year",
    y = "Median CESD",
    color = "Diagnosis Status"
  ) +
  scale_y_continuous(limits = c(0,3.5))+
  theme_bw() +
  theme(
    text = element_text(size = 20), 
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 24),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
  )



# 3. Cox Regression

# 3.1. Data Processing for Cox Regression
# Rename baseline variables: "nsh_bl" → "nsh_0".
shn4 <- shn3
names(shn4) <- gsub("_bl$", "_0", names(shn4))


# Rename year variables: e.g. "nsh_yr01" becomes "nsh_1"
names(shn4) <- gsub("_yr0*(\\d+)", "_\\1", names(shn4))


shn4_backup <- shn4

# Load necessary packages
library(dplyr)
library(tidyr)
library(data.table)

# --- 1. Prepare and split the survival data into intervals ---
shn4$start_times <- rep(0, nrow(shn4))


# Define the cut points
cuts_nsh_seq <- c( 1, 3, 4, 5, 6, 7, 8, 9, 10, 23)
cuts_cesd    <- c( 1, 5, 6, 7, 9, 10, 11, 23)
cuts_all <- sort(unique(c(cuts_nsh_seq, cuts_cesd)))


# Use survSplit to split the follow-up time (from 0 to anydiag_yrs) into intervals.

# survSplit creates intervals in the (start, stop] form (i.e. the risk period does not include the exact start but does include the stop).
shn4_long <- survSplit(Surv(start_times, anydiag_yrs, anydiag_status) ~ .,
                       data = shn4,
                       cut = cuts_all,
                       start = "start",
                       end = "stop",
                       event = "event")

# --- 2. Reshape the time-varying covariates from wide to long format ---

## For nsh:
nsh_long <- shn4 %>%
  select(caseid, starts_with("nsh_")) %>%
  pivot_longer(
    cols = -caseid,
    names_to = "time",
    names_prefix = "nsh_",
    values_to = "nsh"
  ) %>%
  mutate(time = as.numeric(time))

## For seq:
seq_long <- shn4 %>%
  select(caseid, starts_with("seq_")) %>%
  pivot_longer(
    cols = -caseid,
    names_to = "time",
    names_prefix = "seq_",
    values_to = "seq"
  ) %>%
  mutate(time = as.numeric(time))

## For cesd:
cesd_long <- shn4 %>%
  select(caseid, starts_with("cesd_")) %>%
  pivot_longer(
    cols = -caseid,
    names_to = "time",
    names_prefix = "cesd_",
    values_to = "cesd"
  ) %>%
  mutate(time = as.numeric(time))

# --- 3. Merge the covariate measurements with the survival intervals ---
# Convert to data.table objects for efficient merging.
setDT(shn4_long)  # Convert survival intervals to data.table
setDT(nsh_long)
setDT(seq_long)
setDT(cesd_long)


# Set keys for merging on subject id and time.

# For shn4_long, we use the survival interval's start.
setkey(shn4_long, caseid, start)
setkey(nsh_long, caseid, time)
setkey(seq_long, caseid, time)
setkey(cesd_long, caseid, time)


# Merge nsh values:
shn4_long <- nsh_long[shn4_long, on = .(caseid, time = start), roll = TRUE]

# If there is no exact match "roll" the join backwards. That is, it uses the most recent measurement before the interval’s start time.
setnames(shn4_long, "nsh", "nsh_val")
setnames(shn4_long, old ="time" , new = "start")

# Merge seq values:
shn4_long <- seq_long[shn4_long, on = .(caseid, time = start), roll = TRUE]
setnames(shn4_long, "seq", "seq_val")
setnames(shn4_long, old ="time" , new = "start")

# Merge cesd values:
shn4_long <- cesd_long[shn4_long, on = .(caseid, time = start), roll = TRUE]
setnames(shn4_long, "cesd", "cesd_val")
setnames(shn4_long, old ="time" , new = "start")

# order the final dataset by subject and interval start time:
setorder(shn4_long, caseid, start)

# --- 4. Fit a Cox proportional hazards model with time-varying covariates ---
# remove unused cols
shn4_long_backup <- shn4_long

# shn4_long <- shn4_long_backup
# shn4_long <- shn4_long_backup
cols_to_remove <- grep("[0-9]$|(^missing_)", names(shn4_long))
shn4_long <- shn4_long[, -cols_to_remove, with = FALSE]



# 3.2 Cox Regression
library(survival)

# Y = f (X, M)
cox_XM <- coxph(Surv(start, stop, event) ~ nsh_val + seq_val + cesd_val + age_group + white + female + group, data = shn4_long, id=caseid)
summary(cox_XM)

# exclude insignificant var: white & female
cox_XM_1 <- coxph(Surv(start, stop, event) ~ nsh_val + seq_val + cesd_val + age_group + group, data = shn4_long, id=caseid)
summary(cox_XM_1)

# Y = f(X)
cox_X <- coxph(Surv(start, stop, event) ~ nsh_val + seq_val + age_group + white + female + group, data = shn4_long, id=caseid)
summary(cox_X)

# exclude insignificant var: white & female
cox_X_1 <- coxph(Surv(start, stop, event) ~ nsh_val + seq_val  + age_group + group, data = shn4_long, id=caseid)
summary(cox_X_1)



# 4. Mixed-Effects Model

# 4.1 Data Processiong for Mixed-Effects Model

# Get the long format raw data

# Use raw data(shn0) for mixed model
library(tidyr)
library(dplyr)
shn0_long <- shn0 %>%
  pivot_longer(
    cols = starts_with("nsh_") | starts_with("seq_") | starts_with("cesd_"),  # Select time-varying variables
    names_to = c(".value", "time"),  # `.value` retains prefixes, "time" gets extracted
    names_sep = "_"  # Split at `_`
  ) %>%
  mutate(time = as.numeric(time))  # Convert "time" column to numeric
shn0_long_SAS <- shn0_long %>%
  mutate(across(everything(), ~ ifelse(is.na(.), ".", .)))


write.csv(shn0_long_SAS, "shn0_long.csv")



# 4.2 Mean Plots for Interpreting the Mixed-effects Model
library(ggplot2)

# summary(shn0_long1)
# the true first/third quantiles
# mean_nsh_q1 = 30.75
# mean_nsh_q3 = 39.12
# dev_seq_q1 = -0.6000
# dev_seq_q3 = 0.1818

# Becasue the difference between the two line in the first/third quantiles is hard to see from the plot, arbitroy values are picked as:
mean_nsh_q1 = 30
mean_nsh_q3 = 60
dev_seq_q1 = -10
dev_seq_q3 = 10

mean_nsh_mean = 36.32
dev_seq_mean = 0
mean_seq_mean = 20.78
dev_nsh_mean = 0
age_group = 1
time = seq(0,23, length.out=100)

pred_cesd_q1 = -2.314 +
      0.082 * time +
      0.133  * mean_nsh_q1 +
      0.094 * mean_seq_mean +
      (-0.003) * (mean_nsh_q1 * time) +
      0.046 * dev_nsh_mean +
      0.098 * dev_seq_mean +
      (-0.010) * (dev_seq_mean * time)+
      (-0.347) * time
pred_cesd_q3 = -2.314 +
      0.082 * time +
      0.133  * mean_nsh_q3 +
      0.094 * mean_seq_mean +
      (-0.003) * (mean_nsh_q3 * time) +
      0.046 * dev_nsh_mean +
      0.098 * dev_seq_mean +
      (-0.010) * (dev_seq_mean * time)+
      (-0.347) * time
pred_cesd_df_q1 = data.frame(time=time,pred_cesd=pred_cesd_q1,mean_nsh="Low")
pred_cesd_df_q3 = data.frame(time=time,pred_cesd=pred_cesd_q3,mean_nsh="High")
pred_cesd_df <- rbind(pred_cesd_df_q1, pred_cesd_df_q3)

ggplot(pred_cesd_df, aes(x = time, y = pred_cesd, color = mean_nsh)) +
  geom_line() +
  labs(x = "Time", y = "Predicted CESD",
       color = "Mean NSH",
       title = "Trajectories of Predicted CESD Scores: Low vs. High Mean NSH") +
  scale_y_continuous(limits = c(-7.5, 7.5))+
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 24),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20))

# 
pred_cesd_q1 = -2.314 +
      0.082 * time +
      0.133  * mean_nsh_mean +
      0.094 * mean_seq_mean +
      (-0.003) * (mean_nsh_mean * time) +
      0.046 * dev_nsh_mean +
      0.098 * dev_seq_q1 +
      (-0.010) * (dev_seq_q1 * time)+
      (-0.347) * time
pred_cesd_q3 = -2.314 +
      0.082 * time +
      0.133  * mean_nsh_mean +
      0.094 * mean_seq_mean +
      (-0.003) * (mean_nsh_mean * time) +
      0.046 * dev_nsh_mean +
      0.098 * dev_seq_q3 +
      (-0.010) * (dev_seq_q3 * time)+
      (-0.347) * time

pred_cesd_df_q1 = data.frame(time=time,pred_cesd=pred_cesd_q1,mean_nsh="Low")
pred_cesd_df_q3 = data.frame(time=time,pred_cesd=pred_cesd_q3,mean_nsh="High")
pred_cesd_df <- rbind(pred_cesd_df_q1, pred_cesd_df_q3)

ggplot(pred_cesd_df, aes(x = time, y = pred_cesd, color = mean_nsh)) +
  geom_line() +
  labs(x = "Time", y = "Predicted CESD",
       color = "Dev SEQ",
       title = "Trajectories of Predicted CESD Scores: Low vs. High Dev SEQ") +
  scale_y_continuous(limits = c(-7.5, 7.5))+
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 24),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20))



# 5. Likelihood Ratio Test

# MRM var-cov selection (age as ordinal)
LRT <- 16799.7-16798
(1-pchisq(LRT,1))/2

# MRM: decomposed model vs undecomposed model
LRT <- 16799.7-16771.4
1-pchisq(LRT,4)

LRT <- 16780.5-16777.3
1-pchisq(LRT,1)

# Cox model with age_group as multinomial vs as ordinal
LRT <- 3285.183-3279.388
1-pchisq(LRT,2)


