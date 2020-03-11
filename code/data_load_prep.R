# ==============================================================================
# americans and the arts 
# ==============================================================================

# load packages
library(dplyr)
library(here)
library(prettyR)
library(glue)
library(purrr)
library(stringr)
library(survey)

# 1973 =========================================================================

# load data
data_1973 <- readRDS(here('data', 'aa_1973.rds'))

# provide substantive names to variables of interest
data_1973 <- data_1973 %>% 
  rename(race = F11,
         sex = F12,
         age_group = F8,
         edu = F5,
         income = F9,
         region = S11,
         awf_play = Q24B_1,
         awf_artmuseum = Q24B_2,
         awf_concert = Q24B_3,
         awf_opera = Q24B_4,
         awf_scimuseum = Q24B_5,
         awf_histsite = Q24B_6,
         awf_ballet = Q24B_7,
         occupation = F2B) %>% 
  mutate(occupation = as.character(occupation),
         occupation = case_when(str_detect(occupation, 'Not sure') ~ NA_character_,
                                TRUE ~ occupation))

# change factors to numeric for additive scales

# get vector of variables to recode
vars_to_recode <- c(glue("Q5C_{1:7}"),
                    glue("Q5D_{1:7}"),
                    glue("Q5E_{1:7}"))

# recode
data_1973 <- data_1973 %>% 
  mutate_at(vars_to_recode, ~case_when(is.na(.) ~ NA,
                                       str_detect(., '0') ~ FALSE,
                                       TRUE ~ TRUE)) %>% 
  mutate(gs_hb = rowSums(select(., Q5C_1:Q5C_7), na.rm = TRUE),
         jhs_hb = rowSums(select(., Q5D_1:Q5D_7), na.rm = TRUE),
         col_hb = rowSums(select(., Q5E_1:Q5E_7), na.rm = TRUE))
