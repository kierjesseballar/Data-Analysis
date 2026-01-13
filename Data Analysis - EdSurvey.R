library(haven)
library(RALSA)
library(EdSurvey)
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

##### || Analysis with EdSurvey ------
facility_vars <- c("SC07B", "SC07C", "SC07D", "SC07E", "SC07J", "SC07K", "SC07M")

attr(sea_school$SC07B, "labels")

###### |> Make the urbanity variable -----
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
  select(-all_of(facility_vars))

###### |> Merge the student and the school data -----
school_student_merge <- sea_student |> 
  left_join(sea_school_clean) |> 
  as.data.frame()

school_student_edsurvey <- school_student_merge

###### |> Convert to EdSurvey -----
rep_weights <- grep("rwgt", names(school_student_edsurvey), value = TRUE)

# 3. Manually attach the attributes required by EdSurvey functions
attributes(school_student_edsurvey)$pvs <- list(math = list(varnames = c("PV1_M", "PV2_M", "PV3_M", "PV4_M", "PV5_M")))
attributes(school_student_edsurvey)$weights <- list(FWGT = list(jkbase = "FWGT", jkreplicates = rep_weights))
attributes(school_student_edsurvey)$stratumVar <- "StrtID"
attributes(school_student_edsurvey)$psuVar <- "SchID"
attributes(school_student_edsurvey)$jkSumMultiplier <- 1
attributes(school_student_edsurvey)$achievementLevels <- c("Band 3" = 269, "Band 4" = 283, "Band 5" = 297, 
                                                           "Band 6" = 311, "Band 7" = 325, "Band 8" = 339, 
                                                           "Band 9" = 353)
attributes(school_student_edsurvey)$country <- "PHL"
attributes(school_student_edsurvey)$survey <- "SEA-PLM"
attributes(school_student_edsurvey)$dataLevel <- "student"
attributes(school_student_edsurvey)$dataType <- "light.edsurvey.data.frame"

if(is.null(attributes(school_student_edsurvey)$userConditions)) {
  attributes(school_student_edsurvey)$userConditions <- list()
}

class(school_student_edsurvey) <- c("light.edsurvey.data.frame", "data.frame")
class(school_student_edsurvey) 

model_band2 <- logit.sdf(
  formula = I(math < 269) ~ urban_facility_index, 
  data = school_student_edsurvey,
  weightVar = "FWGT"
)

summary(model_band2)
