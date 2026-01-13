library(haven)
# library(RALSA)
# library(EdSurvey)
library(survey)
library(mitools)
library(svylme)
library(mice)
library(tidyverse)
filter <- dplyr::filter

##### || Read Data -----
sea_plm_folder <- "C:\\Users\\kdbal\\OneDrive\\Desktop\\WORK\\EWDG\\SEAPLM Reading\\SEA_PLM 2024\\SEA_PLM_MS24_DBs_2025.12.04_PHL-20251203T161126Z-1-001\\SEA_PLM_MS24_DBs_(2025.12.04)_PHL\\SPSS Data"

###### > SEAPLM Student -----
file <- "SEA_PLM_MS24_PHL_ST_(2025.12.04).sav"
file_name <- str_c(sea_plm_folder, file, sep = "\\")
sea_student <- read_spss(file_name)

dictionary_student <- data.frame(
  variable_name = names(sea_student),
  label = sapply(sea_student, function(x) attr(x, "label"))
)

###### > SEAPLM School -----
file <- "SEA_PLM_MS24_PHL_SC_(2025.12.04).sav"
file_name <- str_c(sea_plm_folder, file, sep = "\\")

sea_school <- read_spss(file_name) 

dictionary_school <- data.frame(
  variable_name = names(sea_school),
  label = sapply(sea_school, function(x) attr(x, "label"))
)

##### ||----- Analysis with Survey -----|| ------
##### || Data and Survey Design Preparation -----
###### |> Make the urbanity variable -----
facility_vars <- c(
  "SC07B", # cinema 
  "SC07C", # theater
  "SC07D", # foreign lang schools
  "SC07E", # museum
  "SC07J", # shopping center
  "SC07K", # market
  "SC07M"  # hospital
  )

attr(sea_school$SC07B, "labels")
attr(sea_school$SC06, "labels")
str(sea_school$SC06)

sea_school_clean <- sea_school |> 
  # Recode SEA-PLM's 1=Yes and 2=No to 1=Yes/0=No
  mutate(across(all_of(facility_vars), ~ case_when(
    . == "1" ~ 1,
    . == "2" ~ 0,
    TRUE ~ NA_real_ 
  ))) %>%
  # Create the row-wise sum 
  rowwise() %>%
  mutate(urban_facility_index = sum(c_across(all_of(facility_vars)), na.rm = TRUE)) %>%
  ungroup() |> 
  select(-all_of(facility_vars)) |> 
  # Combine the population variable with the facility 
  # Use arbitrary threshold of 3, so if with 3 "urban" facilities, tagged as urbanlite
  # chose 3 since markets are ubiquitious so default present
  # PSA https://www.adb.org/sites/default/files/project-documents/51325/51325-001-tacr-en_5.pdf
  # rural if fails to meet ANY of the following, at least 5k, establishments with 100 employees,
  # five small establishments, at least five facilities from barangay hall
  mutate(urbanity_final = case_when(
    SC06 == "1" ~ "Rural", # less than 3,000 people
    SC06 == "2" & urban_facility_index < 3 ~ "Rural", # bet 3k to 15k
    SC06 == "2" & urban_facility_index >= 3 ~ "Urban-like", # bet 3k to 15k
    SC06 == "3" ~ "Urban",
    SC06 == "4" ~ "Urban",
    SC06 == "5" ~ "Urban",
    T ~ NA
  ))

test <- sea_school_clean |> 
  select(SC06, urban_facility_index, urbanity_final) |> 
  distinct()

###### |> Merge the student and the school data -----
attr(sea_student$PL_PV1_R, "labels")

school_student_merge <- sea_student |> 
  left_join(sea_school_clean) |> 
  as.data.frame() |> 
  # convert band labels into binary for logit, 1 = band 2 and 0 otherwise
  mutate(is_band2_r_1 = case_when(
    PL_PV1_R <= 2 ~ 1,
    T ~ 0
  )) |> 
  mutate(is_band2_r_2 = case_when(
    PL_PV2_R <= 2 ~ 1,
    T ~ 0
  )) |> 
  mutate(is_band2_r_3 = case_when(
    PL_PV3_R <= 2 ~ 1,
    T ~ 0
  )) |> 
  mutate(is_band2_r_4 = case_when(
    PL_PV4_R <= 2 ~ 1,
    T ~ 0
  )) |> 
  mutate(is_band2_r_5 = case_when(
    PL_PV5_R <= 2 ~ 1,
    T ~ 0
  )) |> 
  # Create single grade and multigrade id 
  mutate(school_multigrade = case_when(
    SC09A == "1" & SC09B == "2" ~ "Single-grade only",
    SC09A == "2" & SC09B == "1" ~ "Multigrade only",
    SC09A == "1" & SC09B == "1" ~ "Mixed (Single & Multi)",
    SC09A == "2" & SC09B == "2" ~ "Neither/Other",
    TRUE ~ NA_character_
  )) |> 
  mutate(school_multigrade = factor(school_multigrade,
                levels = c("Single-grade only", "Multigrade only", 
                           "Mixed (Single & Multi)", "Neither/Other"))
  ) |> 
  # relabel the multishift to be clearer
  mutate(school_multishift = case_when(
    SC09C == "1" ~ "multi-shift",
    SC09C == "2" ~ "single-shift",
  )) |> 
  mutate(school_multishift = factor(school_multishift,
                levels = c("single-shift", "multi-shift"))
  )


attr(sea_student$PL_PV1_R, "labels")

###### |> Make survey design object -----
rep_colnames <- grep("rwgt", names(school_student_merge), value = TRUE)

survey_design <- svrepdesign(
  data = school_student_merge,
  type = "JK2", # from manual
  weights = ~FWGT,          
  repweights = school_student_merge[, rep_colnames],
  combined.weights = TRUE, 
  ids = ~SchID, 
  stratum = ~StrtID  
)

##### || Model Setup -----
length(unique(sea_student$SchID)) # only 156 schools
predictors <- c(
    "urbanity_final", # urban lite
    "SchType", # School sector (private public)
    "S_Age", # Age
    "S_Gender", # Student gender
    "S_SES", # Student SES index
    "S_MISCED", # Highest educational level of mother
    "S_FISCED", # Highest educational level of father
    "S_ATTREAD", # Attitudes towards reading
    "ST13", # Attend preschool, note reverse scale with 1 = as highest
    "ST14", # Repeated a grade, note 1 = yes
    "C_RATIO", # Ratio of students to teachers
    # School - Individual classes are organised according to the student’s year level
    # School - Individual classes have students with multiple year levels
    "school_multigrade", 
    "school_multishift", # School - Classes are organised into shifts, where not all students are at school at the same time
    "SC10", # On a typical school day, approximately how many hours do students receive lessons in the school?
    "SC11" # In a typical school year, approximately how many instructional weeks do students receive lessons in the school?
    )

# Check Attributes
for (var in predictors) {
  val_labs <- attr(school_student_merge[[var]], "labels")
  cat("---", var, "---\n")
  if (!is.null(val_labs)) {
    print(val_labs)
  } else {
    cat("No value labels found (likely a continuous variable).\n")
  }
  cat("\n")
}

# Register Formula
model_formula <- reformulate(response = "reading_score", termlabels = predictors)

##### || Model 1: Base Model, no interactions, no other variables -----
# Run logit model
# copy formula code for direct type, does not work if formula = model_formula idk why
model_formula

model_1_pvs <- withPV(
    mapping = list(
                  reading_score ~ is_band2_r_1 + is_band2_r_2 + 
                    is_band2_r_3 + is_band2_r_4 + is_band2_r_5
                ),
    data = survey_design,
    action = quote(
                svyglm(
                      reading_score ~ urbanity_final + SchType + S_Age + S_Gender + 
                        # S_SES + 
                        S_MISCED + S_FISCED + S_ATTREAD + ST13 + ST14 + C_RATIO + 
                        school_multigrade + school_multishift + SC10 + SC11, 
                      family = quasibinomial(), design = survey_design)
              ),
  rewrite = TRUE
)

# Pool the coefficients and se
model_1_pooled <- MIcombine(model_1_pvs)

# Examine the results 
results <- summary(model_1_pooled) |> 
  mutate(across(.cols = -missInfo, 
                ~exp(.))) |> 
  rownames_to_column()

p_vals <- (2 * pt(-abs(coef(model_1_pooled) / SE(model_1_pooled)), df = model_1_pooled$df)) |> 
  as.data.frame() |> 
  rename(p_value = 1) |> 
  rownames_to_column()

model_1_results <- results |> 
  left_join(p_vals) |> 
  mutate(sig = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    p_value < 0.1 ~ ".",
    TRUE ~ "" 
  ))


##### || Model 2: Base model with interactions -----

##### || Model 3: Base model with interactions + other variables -----
# Run logit model
# copy formula code for direct type, does not work if formula = model_formula idk why
model_formula

model_1_pvs <- withPV(
    mapping = list(
                  reading_score ~ is_band2_r_1 + is_band2_r_2 + 
                    is_band2_r_3 + is_band2_r_4 + is_band2_r_5
                ),
    data = survey_design,
    action = quote(
                svyglm(
                      reading_score ~ urbanity_final + SchType + S_Age + S_Gender + 
                        # S_SES + 
                        S_MISCED + S_FISCED + S_ATTREAD + ST13 + ST14 + C_RATIO + 
                        school_multigrade + school_multishift + SC10 + SC11, 
                      family = quasibinomial(), design = survey_design)
              ),
  rewrite = TRUE
)

# Pool the coefficients and se
model_1_pooled <- MIcombine(model_1_pvs)

# Examine the results 
results <- summary(model_1_pooled) |> 
  mutate(across(.cols = -missInfo, 
                ~exp(.))) |> 
  rownames_to_column()

p_vals <- (2 * pt(-abs(coef(model_1_pooled) / SE(model_1_pooled)), df = model_1_pooled$df)) |> 
  as.data.frame() |> 
  rename(p_value = 1) |> 
  rownames_to_column()

model_1_results <- results |> 
  left_join(p_vals) |> 
  mutate(sig = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    p_value < 0.1 ~ ".",
    TRUE ~ "" 
  ))

