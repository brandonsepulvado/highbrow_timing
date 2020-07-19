# ==============================================================================
# analyses
# ==============================================================================

# load packages
library(here)
library(texreg)
library(ggplot2)
library(tidyr)
library(ggalluvial)
library(openxlsx)
library(glue)

# load data
source(here('code', 'data_load_prep.R'))

# descriptives =================================================================

# income : unweighted counts
data_1973 %>% 
  filter(!is.na(income)) %>% 
  #count(income) %>% 
  ggplot(aes(x = income)) +
  geom_bar(fill = 'navyblue', alpha = .6) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  labs(x = 'Income',
       y = 'Number of Respondents')

# function to get counts
get_counts <- function(variable, data = data_1973){
  # get counts
  counts <- data %>% 
    filter(!is.na({{variable}})) %>% 
    count({{variable}})
  # return counts
  return(counts)
}

# counts : income
counts_income <- get_counts(income)

# race : unweighted counts
counts_race <- get_counts(race)

# gender : unweighted
counts_gender <- get_counts(sex)

# age
counts_age <- get_counts(age_group)

# education
counts_edu <- get_counts(edu)

# highbrow scales
counts_gs_hb <- get_counts(gs_hb)
counts_jhs_hb <- get_counts(jhs_hb)
counts_col_hb <- get_counts(col_hb)

# occupation
counts_occ <- get_counts(occupation)

# save counts
write.xlsx(list(counts_income, counts_race, counts_gender, counts_age, 
                counts_edu, counts_gs_hb, counts_jhs_hb, counts_col_hb),
           sheetName= c("income", "race", "gender", "age", 
                        "edu", "gs_hb", "jhs_hb", "col_hb"),
           file = here('output', glue("univariate_counts_{Sys.Date()}.xlsx")))

# changes in highbrow consumption
# data_1973 %>% 
#   select(CASEID, gs_hb, jhs_hb, col_hb) %>% 
  
  

# regression models ============================================================

# only control variables
model_c <- lm(as.numeric(income) ~ race + sex + age_group + edu, 
              data = data_1973, weights = data_1973$WEIGHT)

#  elementary highbrow consumption
model_1 <- lm(as.numeric(income) ~ gs_hb, 
              data = data_1973, weights = data_1973$WEIGHT)

# middle and high school highbrow consumption
model_2 <- lm(as.numeric(income) ~ jhs_hb, 
              data = data_1973, weights = data_1973$WEIGHT)

# controls plus elementary highbrow consumption
model_3 <- lm(as.numeric(income) ~ col_hb, 
               data = data_1973, weights = data_1973$WEIGHT)

# controls plus all consumption periods
model_4 <- lm(as.numeric(income) ~  gs_hb + jhs_hb + col_hb, 
              data = data_1973, weights = data_1973$WEIGHT)

# controls plus elementary highbrow consumption
model_5 <- lm(as.numeric(income) ~ race + sex + age_group + edu + gs_hb, 
              data = data_1973, weights = data_1973$WEIGHT)

# controls plus middle and high school highbrow consumption
model_6 <- lm(as.numeric(income) ~ race + sex + age_group + edu + jhs_hb, 
              data = data_1973, weights = data_1973$WEIGHT)

# controls plus elementary highbrow consumption
model_7 <- lm(as.numeric(income) ~ race + sex + age_group + edu + col_hb, 
              data = data_1973, weights = data_1973$WEIGHT)

# controls plus all consumption periods
model_8 <- lm(as.numeric(income) ~ race + sex + age_group + edu + gs_hb 
              + jhs_hb + col_hb, data = data_1973, weights = data_1973$WEIGHT)

# with interactions

# gender interaction with elementary
model_9 <- lm(as.numeric(income)~ race + sex + age_group + edu + gs_hb 
              + (sex*gs_hb), data=data_1973, weights=data_1973$WEIGHT)
summary(model_9)

# gender interaction with jr/hs
model_10 <- lm(as.numeric(income)~race + sex + age_group + edu + jhs_hb 
               + (sex * jhs_hb), 
               data=data_1973, weights=data_1973$WEIGHT)
summary(model_10)

# gender interaction with college
model_11 <- lm(as.numeric(income)~ race + sex + age_group + edu + col_hb 
               + (sex * col_hb), 
               data=data_1973, weights=data_1973$WEIGHT)
summary(model_11)

# gender interaction with all three
model_12 <- lm(as.numeric(income)~ race + sex + age_group + edu + gs_hb 
                 + jhs_hb + col_hb + (sex * gs_hb)+(sex * jhs_hb)+(sex * col_hb), 
               data=data_1973, weights=data_1973$WEIGHT)
summary(model_12)

# with occupation
model_13 <- lm(as.numeric(income)~ race + sex + age_group + edu + gs_hb + jhs_hb
               + col_hb + (sex * gs_hb) + (sex * jhs_hb) + (sex * col_hb) 
               + as.factor(occupation), data=data_1973, 
               weights=data_1973$WEIGHT)
summary(model_13)
# occupational group for head of household changes things a lot 

# create gender-based subsets

# male
data_1973_male <- data_1973 %>% filter(sex == "(1) Male")

# female
data_1973_female <- data_1973 %>% filter(sex == "(2) Female")


# model 12 gender-based models (unconstrained)

# male
model_12_male <- lm(as.numeric(income) ~ race + age_group + edu + gs_hb + jhs_hb
                    + col_hb, 
                    data = data_1973_male,
                    weights = data_1973_male$WEIGHT)

# female
model_12_female <- lm(as.numeric(income) ~ race + age_group + edu + gs_hb + jhs_hb
                      + col_hb, 
                      data = data_1973_female,
                      weights = data_1973_female$WEIGHT)

# model 13 gender-based (unconstrained) : male
model_13_male <- lm(as.numeric(income) ~ race + age_group + edu + gs_hb + jhs_hb
                    + col_hb + as.factor(occupation), 
                    data = data_1973_male,
                    weights = data_1973_male$WEIGHT)

# model 13 gender-based (unconstrained) : female
model_13_female <- lm(as.numeric(income) ~ race + age_group + edu + gs_hb + jhs_hb
                      + col_hb + as.factor(occupation), 
                      data = data_1973_female,
                      weights = data_1973_female$WEIGHT)



# diagnostics ==================================================================

# function to plot residuals
plot_resid <- function(model, model_name = NULL){
  # get plot to return
  to_return <- qqnorm(resid({{model}}))
  to_return <- qqline(resid({{model}}))
  to_return <- mtext(model_name)
  # return object
  return(to_return)
}

# model c
plot_resid(model_c, model_name = 'Model 0')


# output =======================================================================

# subset, pdf
texreg(l=list(model_c, model_1, model_2, model_3, model_4), 
        file = here('output', 'table_subset_1.tex'), caption = "OLS Models", 
       caption.above = TRUE, custom.note = "* p>.05, ** p<.01, *** p<.001")
#
texreg(l=list(model_5, model_6, model_7, model_8), 
       file = here('output', 'table_subset_2.tex'), caption = "OLS Models", 
       caption.above = TRUE, custom.note = "* p>.05, ** p<.01, *** p<.001")
# 
texreg(l=list(model_9, model_10, model_11, model_12, model_13), 
       file = here('output', 'table_subset_3.tex'), caption = "OLS Models", 
       caption.above = TRUE, custom.note = "* p>.05, ** p<.01, *** p<.001")

# subset, doc
htmlreg(l=list(model_c, model_1, model_2, model_3, model_4, model_5, model_6,
              model_7, model_8), 
       file = here('output', 'table_subset_1.doc'), caption = "OLS Models", 
       caption.above = TRUE, custom.note = "* p>.05, ** p<.01, *** p<.001")
htmlreg(l=list(model_9, model_10, model_11, model_12, model_13), 
       file = here('output', 'table_subset_2.doc'), caption = "OLS Models", 
       caption.above = TRUE, custom.note = "* p>.05, ** p<.01, *** p<.001")

  