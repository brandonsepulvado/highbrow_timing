# ==============================================================================
# analyses
# ==============================================================================

# load packages
library(here)

# load data
source(here('code', 'data_load_prep.R'))

# descriptives =================================================================



# regression models ============================================================

# only control variables
model_c <- lm(as.numeric(income) ~ race + sex + age_group + edu, 
              data = data_1973, weights = data_1973$WEIGHT)

# controls plus elementary highbrow consumption
model_1 <- lm(as.numeric(income) ~ race + sex + age_group + edu + gs_hb, 
              data = data_1973, weights = data_1973$WEIGHT)

# controls plus middle and high school highbrow consumption
model_2 <- lm(as.numeric(income) ~ race + sex + age_group + edu + jhs_hb, 
              data = data_1973, weights = data_1973$WEIGHT)

# controls plus elementary highbrow consumption
model_3 <- lm(as.numeric(income) ~ race + sex + age_group + edu + col_hb, 
              data = data_1973, weights = data_1973$WEIGHT)

# controls plus all consumption periods
model_4 <- lm(as.numeric(income) ~ race + sex + age_group + edu + gs_hb 
              + jhs_hb + col_hb, data = data_1973, weights = data_1973$WEIGHT)



# diagnostics ==================================================================



# output =======================================================================