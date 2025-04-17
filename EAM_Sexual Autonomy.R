# Check the working Directory
getwd()

# Load necessary packages
library(haven)
library(dplyr)
library(tidyr)

install.packages(c("survey", "lme4", "gtsummary", "forcats", "labelled", "janitor"))
library(survey)
library(lme4)
library(gtsummary)
library(forcats)
library(tidyverse)
library(labelled)
library(janitor)

# Import Data
dhs_data <- read_dta("C:\\Users\\alawo\\Desktop\\R_Data Analytics\\R_Analytics\\NGIR7BFL.DTA")

head(dhs_data)

# Just to do a quick tab of the needed variables for the analysis
dhs_data %>%
  count(v025)

# A quick frequency of some variables with proportions
dhs_data %>%
  count(v025) %>%
  mutate(prop = n/sum(n))

dhs_data %>%
  count(v005, v021, v023) %>%
  mutate(prop = n/sum(n))

# Survey Weight Setup
dhs_data <- dhs_data %>%
  mutate(
    strata = v023,
    psu = v021,
    wt = v005 / 1e6
  )

survey_design <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~wt,
  data = dhs_data,
  nest = TRUE
)

# Create Study Population
dhs_data <- dhs_data %>%
  mutate(
    mar_stat = case_when(
      v501 %in% c(1, 2) ~ 1,
      v501 %in% c(0, 3, 4, 5) ~ 2
    ),
    pop = if_else(mar_stat == 1 & v044 == 1 & v130 < 4, 1, 0)
  )

# Sexual Autonomy Variables
# Check values frequencies
dhs_data %>%
  count(v822)

dhs_data %>%
  count(v850a)

dhs_data %>%
  count(v850b)

# Recode variables: 0 = 0 "N0", 1 = 1"Yes", 8 = NA
dhs_data <- dhs_data %>%
  mutate(
    Justified = case_when(
      v822 == 0 ~ 0,
      v822 == 1 ~ 1,
      v822 == 8 ~ NA_real_,
      TRUE ~ NA_real_
    ),
    refuse_sex = case_when(
      v850a == 0 ~ 0,
      v850a == 1 ~ 1,
      v850a == 8 ~ NA_real_,
      TRUE ~ NA_real_
    ),
    ask_cd = case_when(
      v850b == 0 ~ 0,
      v850b == 1 ~ 1,
      v850b == 8 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

# Check new variables
dhs_data %>%
  count(Justified)
  
dhs_data %>%
  count(refuse_sex)

dhs_data %>%
  count(ask_cd)

## Create autononmy variables as a sum of the 3 components
dhs_data <- dhs_data %>%
  mutate(
    autonomy = rowSums(across(c(Justified, refuse_sex, ask_cd)), na.rm = TRUE)
  )

dhs_data %>%
  count(autonomy)

# Recode autonomy into binary sexual_autonomy variable
dhs_data <- dhs_data %>%
  mutate(
    sexual_autonomy = case_when(
      autonomy == 3                          ~ 1,
      autonomy >= 0 & autonomy <= 2          ~ 0,
      TRUE                                   ~ NA_real_
    )
  )
    
# Educational Assortative Mating
dhs_data <- dhs_data %>%
  filter(v701 < 8) %>%
  mutate(
    Edu_Mat = case_when(
      v701 < 2 & v106 < 2 ~ 1,
      v701 < 2 & v106 > 1 ~ 2,
      v701 > 1 & v106 < 2 ~ 3,
      v701 > 1 & v106 > 1 ~ 4
    ),
    EAM = factor(Edu_Mat, levels = 1:4, labels = c("Low_educ Homo", "Hypogamy", "Hypergamy", "High_educ Homo"))
  )

dhs_data %>%
  select(sexual_autonomy, EAM, v025, v024, Any_IPV) %>%
  summary()


# Covariates
dhs_data <- dhs_data %>%
  mutate(
    Age = case_when(
      v012 >= 15 & v012 <= 24 ~ 1,
      v012 >= 25 & v012 <= 34 ~ 2,
      v012 >= 35 ~ 3
    ),
    Parity = case_when(
      v201 == 1 ~ 1,
      v201 >= 2 & v201 <= 4 ~ 2,
      v201 >= 5 ~ 3
    ),
    age_marriage = case_when(
      v511 >= 7 & v511 <= 17 ~ 1,
      v511 >= 18 ~ 2
    ),
    Exp = v157 + v158 + v159,
    massmedia_exp = case_when(
      Exp == 0 ~ 0,
      Exp >= 1 & Exp <= 3 ~ 1,
      Exp >= 4 ~ 2
    ),
    work_stat = recode(v714, `0` = 1, `1` = 2),
    Religion = case_when(
      v130 == 1 ~ 1,
      v130 == 2 ~ 2,
      v130 == 3 ~ 3,
      v130 >= 4 & v130 <= 96 ~ NA_real_
    )
  )

# IPV Variables
dhs_data <- dhs_data %>%
  mutate(
    humiliate = if_else(d103a %in% 1:4, 1, 0),
    threaten = if_else(d103b < 9 & d103b %in% 1:4, 1, 0),
    insulted = if_else(d103c %in% 1:4, 1, 0),
    emo = humiliate + threaten + insulted,
    emo_vio = if_else(emo > 0, 1, 0),
    
    pushed = if_else(d105a %in% 1:4, 1, 0),
    slapped = if_else(d105b %in% 1:4, 1, 0),
    punched = if_else(d105c %in% 1:4, 1, 0),
    kicked = if_else(d105d < 9 & d105d %in% 1:4, 1, 0),
    strangled = if_else(d105e < 9 & d105e %in% 1:4, 1, 0),
    knife = if_else(d105f < 9 & d105f %in% 1:4, 1, 0),
    twisted = if_else(d105j < 9 & d105j %in% 1:4, 1, 0),
    phy = pushed + slapped + punched + kicked + strangled + knife + twisted,
    phy_vio = if_else(phy > 0, 1, 0),
    
    phy_sex = if_else(d105h %in% c(1, 2, 4), 1, 0),
    act_sex = if_else(d105i %in% c(1, 2, 4), 1, 0),
    vio_sex = phy_sex + act_sex,
    sex_vio = if_else(vio_sex > 0, 1, 0),
    
    Any_vio = emo_vio + phy_vio + sex_vio,
    Any_IPV = if_else(Any_vio > 0, 1, 0)
  )

dhs_data %>%
  select(sexual_autonomy, EAM, v025, v024, Any_IPV, Age, Parity, age_marriage) %>%
  summary()

# Additional Covariates
# First, create the intermediate variables
dhs_data <- dhs_data %>%
  mutate(
    Partner_alc = recode(d113, `0` = 1, `1` = 2),
    
    jealous = na_if(d101a, 8),
    unfaithful = na_if(d101b, 8),
    permit = na_if(d101c, 8),
    limit_contact = na_if(d101d, 8),
    location = na_if(d101e, 8)
  )

# Then compute control and the rest
dhs_data <- dhs_data %>%
  mutate(
    control = rowSums(select(., jealous, unfaithful, permit, limit_contact, location), na.rm = TRUE),
    
    marital_control = case_when(
      control <= 1 ~ 1,
      control %in% 2:3 ~ 2,
      control >= 4 ~ 3
    ),
    
    healthcare_decisions = case_when(v743a < 9 ~ recode(v743a, `1` = 1, `2` = 2, `4` = 3, `5` = 3, `6` = 3)),
    purchases_decisions = case_when(v743b < 9 ~ recode(v743b, `1` = 1, `2` = 2, `4` = 3, `5` = 3, `6` = 3)),
    visit_decisions = case_when(v743d < 9 ~ recode(v743d, `1` = 1, `2` = 2, `4` = 3, `5` = 3, `6` = 3)),
    earning_decisions = case_when(v743f < 9 ~ recode(v743f, `1` = 1, `2` = 2, `4` = 3, `5` = 3, `6` = 3, `7` = 3)),
    
    maternal_pos = healthcare_decisions + purchases_decisions + visit_decisions + earning_decisions,
    decision = case_when(
      maternal_pos %in% 8:12 ~ 1,
      maternal_pos %in% 6:7 ~ 2,
      maternal_pos %in% 4:5 ~ 3
    ),
    
    lv_arrang = recode(v504, `1` = 1, `2` = 2),
    Ethnicity = case_when(
      v131 %in% c(2, 3) ~ 1,
      v131 == 10 ~ 2,
      v131 == 6 ~ 3,
      v131 %in% c(1, 4, 5, 7, 8, 9, 96, 98) ~ 4
    ),
    HH_Wealth = v190,
    residence = v025,
    region = case_when(v024 %in% 1:3 ~ 1, v024 %in% 4:6 ~ 2),
    
    cpovp = if_else(HH_Wealth == 5, 1, 0)
  )


# Community Level SES and Literacy
dhs_data <- dhs_data %>%
  group_by(v001) %>%
  mutate(
    cpovpl = mean(cpovp, na.rm = TRUE),
    commedu = mean(if_else(v155 == 2, 1, 0), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    cpovw = ntile(cpovpl, 3),
    ccedu = ntile(commedu, 3)
  )



# Multilevel Modeling (example for Model 4)
dhs_model_data <- dhs_data %>% filter(pop == 1) %>% na.omit()

dhs_data %>% filter(pop ==1) %>% nrow()
dhs_data %>% filter(pop == 1) %>% summary()

dhs_model_data <- dhs_data %>%
  filter(pop == 1) %>%
  drop_na(sexual_autonomy, v021)

nrow(dhs_model_data)

dhs_model_data <- dhs_model_data %>%
  mutate(v021 = as.factor(v021))

install.packages("sjPlot")
install.packages("performance")
install.packages("parameters")

library(sjPlot)
library(performance)
library(parameters)


# Empty Model
empty_model <- glmer(sexual_autonomy ~ 1 + (1 | v021),
                     data = dhs_model_data,
                     family = binomial)
empty_model
tab_model(empty_model, transform = "exp", show.ci = TRUE)
model_parameters(empty_model, exponentiate = TRUE)

# Model 1
model1 <- glmer(sexual_autonomy ~ relevel(factor(EAM), ref = "Hypergamy") + (1 | v021),
                data = dhs_model_data, family = binomial)

model1
tab_model(model1, transform = "exp", show.ci = TRUE)
model_parameters(model1, exponentiate = TRUE)

# Model 2
model2 <- glmer(sexual_autonomy ~ factor(EAM) + factor(Age) + factor(Parity) + factor(age_marriage) + 
                  factor(massmedia_exp) + factor(work_stat) + factor(Religion) + (1 | v021),
                data = dhs_model_data, family = binomial)

model2
tab_model(model2, transform = "exp", show.ci = TRUE)
model_parameters(model2, exponentiate = TRUE)

# Model 3
model3 <- glmer(sexual_autonomy ~ factor(EAM) + factor(Age) + factor(Parity) + factor(age_marriage) + 
                  factor(massmedia_exp) + factor(work_stat) + factor(Religion) + Any_IPV + 
                  factor(Partner_alc) + factor(marital_control) + factor(lv_arrang) + 
                  factor(decision) + factor(HH_Wealth) + factor(Ethnicity) + (1 | v021),
                data = dhs_model_data, family = binomial)
model3
tab_model(model3, transform = "exp", show.ci = TRUE)
model_parameters(model3, exponentiate = TRUE)

# Model 4
model4 <- glmer(sexual_autonomy ~ factor(EAM) + factor(Age) + factor(Parity) + factor(age_marriage) + 
                  factor(massmedia_exp) + factor(work_stat) + factor(Religion) + Any_IPV + 
                  factor(Partner_alc) + factor(marital_control) + factor(lv_arrang) + 
                  factor(decision) + factor(HH_Wealth) + factor(Ethnicity) + factor(residence) + 
                  factor(region) + factor(cpovw) + factor(ccedu) + (1 | v021),
                data = dhs_model_data, family = binomial)

model4
tab_model(model4, transform = "exp", show.ci = TRUE)
model_parameters(model4, exponentiate = TRUE)
