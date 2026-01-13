library(haven)
# library(RALSA)
# library(EdSurvey)
library(survey)
library(mitools)
library(svylme)
library(mice)
library(tidyverse)
filter <- dplyr::filter

##### || Folders -----
sea_plm_folder <- "C:\\Users\\kdbal\\OneDrive\\Desktop\\WORK\\EWDG\\SEAPLM Reading\\SEA_PLM 2024\\SEA_PLM_MS24_DBs_2025.12.04_PHL-20251203T161126Z-1-001\\SEA_PLM_MS24_DBs_(2025.12.04)_PHL\\SPSS Data"

##### || Raw Data for Reading -----
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

##### || Analysis with Survey ------
###### |> Make the urbanity variable -----
facility_vars <- c("SC07B", "SC07C", "SC07D", "SC07E", "SC07J", "SC07K", "SC07M")
attr(sea_school$SC07B, "labels")

###### |> Make the urbanity variable -----
attr(sea_student$SchLoc, "labels")

sea_school_clean <- sea_school %>%
  # Recode SEA-PLM's 1=Yes and 2=No to 1=Yes/0=No
  mutate(across(all_of(facility_vars), ~ case_when(
    . == "1" ~ 1,
    . == "2" ~ 0,
    TRUE ~ NA_real_ # Handle missing data safely
  ))) %>%
  # Create the row-wise sum 
  rowwise() %>%
  mutate(urban_facility_index = sum(c_across(all_of(facility_vars)), na.rm = TRUE)) %>%
  ungroup() |> 
  select(-all_of(facility_vars)) |> 
  # Create a synthetic urban/rural classification
  mutate(urbanity_final = case_when(
    SchLoc = 1 
  ))

###### |> Merge the student and the school data -----
attr(sea_student$PL_PV1_R, "labels")

school_student_merge <- sea_student |> 
  left_join(sea_school_clean) |> 
  as.data.frame() |> 
  # mutate(across(where(is.labelled), as_factor)) |>
  mutate(is_band2_1 = case_when(
    PL_PV1_R <= 2 ~ 1,
    T ~ 0
  )) |> 
  mutate(is_band2_2 = case_when(
    PL_PV2_R <= 2 ~ 1,
    T ~ 0
  )) |> 
  mutate(is_band2_3 = case_when(
    PL_PV3_R <= 2 ~ 1,
    T ~ 0
  )) |> 
  mutate(is_band2_4 = case_when(
    PL_PV4_R <= 2 ~ 1,
    T ~ 0
  )) |> 
  mutate(is_band2_5 = case_when(
    PL_PV5_R <= 2 ~ 1,
    T ~ 0
  ))


attr(sea_student$PL_PV1_R, "labels")

###### |> Make survey design object -----
rep_colnames <- grep("rwgt", names(school_student_merge), value = TRUE)

survey_design <- svrepdesign(
  data = school_student_merge,
  type = "JKn",            
  weights = ~FWGT,          
  repweights = school_student_merge[, rep_colnames],
  combined.weights = TRUE, 
  scale = 1,              
  rscales = 1,
  ids = ~SchID, 
  stratum = ~StrtID  
)

##### || Model 1: No interactions -----
length(unique(sea_student$SchID)) # only 156 schools

model_1_pvs <- withPV(
  mapping = list(reading_score ~ is_band2_1 + is_band2_2 + is_band2_3 + is_band2_4 + is_band2_5),
  data = survey_design,
  action = quote(svyglm(reading_score ~ urban_facility_index + SchLoc + SchType + S_Age + S_Gender + S_SES + S_PARED + ST13 + C_RATIO + SC09A + SC09B + SC09C + SC10 + SC11, 
                        family = quasibinomial(),
                        design = survey_design)
                ),
  rewrite = TRUE
)

model_1_pooled <- MIcombine(model_1_pvs)

summary(model_1_pooled)
exp(coef(model_1_pooled))

t_stats <- coef(model_1_pooled) / SE(model_1_pooled)
p_vals <- 2 * pt(-abs(t_stats), df = model_1_pooled$df)
p_vals 




full_mira <- as.mira(model_1_pvs)
predictors <- names(coef(model_1_pvs[[1]]))[-1] 
omnibus_list <- lapply(predictors, function(var) {
  
  # Use lapply to update each model in the list
  model_reduced_list <- lapply(model_1_pvs, function(m) {
    # We use update with a rigorous formula environment
    update(m, as.formula(paste(". ~ . -", var)))
  })
  
  # Convert reduced list to mira object
  reduced_mira <- as.mira(model_reduced_list)
  
  # Perform D1 test - wrapping in try() handles the NA replicate instability
  test <- tryCatch({
    D1(full_mira, reduced_mira)
  }, error = function(e) {
    return(paste("Error in", var, ":", e$message))
  })
  
  return(test)
})

names(omnibus_list) <- predictors
