# ==============================================================================
# analyses
# ==============================================================================

# load packages
library(here)
library(texreg)
library(ggplot2)

# load data
source(here('code', 'data_load_prep.R'))

# descriptives =================================================================



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



# diagnostics ==================================================================

# unweighted counts

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

# unweighted counts


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

  