us18 <- read.csv("us_18Q1.csv")
age <- us18$DEM_AGE
income <- us18$DEM_INCOME
income <- as.ordered(income)
education <- us18$DEM_EDU
education <- as.ordered(education)
ill_yr <- us18$ILL_YR
ill_yr <- as.factor(ill_yr)
#Modify predictors to remove income level 11 and check the difference
subset_us18 <- subset(us18, DEM_INCOME != 11)
sum(us18$DEM_INCOME == 11)
nrow(us18) - nrow(subset_us18)
#3 predictors
options(contrasts = rep ("contr.treatment", 2))
age <- subset_us18$DEM_AGE
income <- subset_us18$DEM_INCOME
income <- as.ordered(income)
education <- subset_us18$DEM_EDU
education <- as.ordered(education)
ill_yr <- subset_us18$ILL_YR
ill_yr <- as.factor(ill_yr)
#Education and age and age and income (use for model)
#us18glm <- glm(as.numeric(ill_yr) ~ education*age + age*income, family = "binomial")
#us18glm
us18glm <- glm(ill_yr ~ education*age + age*income, family = "binomial")
us18glm
plot(us18glm)
summary(us18glm)
anova(us18glm)
predicted_ill_yr_prob <- predict.glm(us18glm, type = 'response')
predicted_ill_yr <- predicted_ill_yr_prob >= 0.5
sum(as.numeric(ill_yr))
nrow(us18)
ill_yr_ssr <- sqrt(sum((predicted_ill_yr - as.numeric(ill_yr))^2))
sum(predicted_ill_yr)
with(summary(us18glm), 1 - deviance/null.deviance)
length(ill_yr)
sum(as.numeric(ill_yr))
hist(predicted_ill_yr_prob)

test <- glm(ill_yr ~ age, family="binomial")
pred_prob <- predict.glm(test, type='response')
pred_y <- pred_prob >= 0.5
sum(pred_y)




# MAIN MODEL USED IN THE END
options(contrasts = rep ("contr.treatment", 2)) # important to run
us18 <- read.csv("us_18Q1.csv")
subset_us18 <- subset(us18, DEM_INCOME != 11)
age <- subset_us18$DEM_AGE
income <- subset_us18$DEM_INCOME
income <- as.ordered(income)
education <- subset_us18$DEM_EDU
education <- as.ordered(education)
ill_yr <- subset_us18$ILL_YR
ill_yr <- as.factor(ill_yr)
dast_cat <- as.ordered(subset_us18$DAST_CAT)
us18glm_new <- glm(ill_yr ~ age*education + age*income + 
                     age*dast_cat, family = "binomial")
anova(us18glm_new, test = 'Chisq')


summary(anova(us18glm_new))


pred_ill_use_prob <- predict.glm(us18glm_new, type = 'response')
with(summary(us18glm_new), 1 - deviance/null.deviance)
pred_y_use <- pred_ill_use_prob >= 0.5
sum(pred_y_use)



# individual
us18glm_new <- glm(ill_yr ~ age + education + income + dast_cat + marital, family = "binomial")
summary(us18glm_new)
pred_ill_use_prob <- predict.glm(us18glm_new, type = 'response')
with(summary(us18glm_new), 1 - deviance/null.deviance)
pred_y_use <- pred_ill_use_prob >= 0.5
sum(pred_y_use)


















# adding marital
us18glm_new <- glm(ill_yr ~ age*education + age*income + 
                     age*dast_cat + age*marital, family = "binomial")

summary(us18glm_new)
pred_ill_use_prob <- predict.glm(us18glm_new, type = 'response')
with(summary(us18glm_new), 1 - deviance/null.deviance)
pred_y_use <- pred_ill_use_prob >= 0.5
sum(pred_y_use)












####
# trying ill_use instead idk
ill_use <- subset_us18$ILL_USE
age <- subset_us18$DEM_AGE
income <- as.ordered(subset_us18$DEM_INCOME)
education <- as.ordered(subset_us18$DEM_EDU)
us18glm_new <- glm(ill_use ~ education*age + age*income + age*dast_cat, family = "binomial")

summary(us18glm_new)

pred_ill_use_prob <- predict.glm(us18glm_new, type = 'response')
with(summary(us18glm_new), 1 - deviance/null.deviance)


pred_y_use <- pred_ill_use_prob >= 0.5
sum(pred_y_use)




# marital 
ill_yr <- subset_us18$ILL_YR
age <- subset_us18$DEM_AGE
income <- as.ordered(subset_us18$DEM_INCOME)
education <- as.ordered(subset_us18$DEM_EDU)
marital <- as.factor(subset_us18$DEM_MARITAL)
us18glm_new <- glm(ill_yr ~ age*education + age*income + age*marital, family = "binomial")

summary(us18glm_new)

pred_ill_use_prob <- predict.glm(us18glm_new, type = 'response')
pred_y_use <- pred_ill_use_prob >= 0.5
sum(pred_y_use)
with(summary(us18glm_new), 1 - deviance/null.deviance)

plot(us18glm_new)



## adding more vars
# chronic pain
chronic <- as.factor(subset_us18$PAIN_CHRONIC)
us18glm_new <- glm(ill_yr ~ age*education + age*income + age*marital + age*chronic, family = "binomial")
summary(us18glm_new)

pred_ill_use_prob <- predict.glm(us18glm_new, type = 'response')
with(summary(us18glm_new), 1 - deviance/null.deviance)
pred_y_use <- pred_ill_use_prob >= 0.5
sum(pred_y_use)


# anxiety
# anx <- as.factor(subset_us18$MENT_ANX)
# us18glm_new <- glm(ill_yr ~ age*education + age*income + age*marital + age*chronic + age*anx, family = "binomial")
# summary(us18glm_new)
# 
# pred_ill_use_prob <- predict.glm(us18glm_new, type = 'response')
# with(summary(us18glm_new), 1 - deviance/null.deviance)
# pred_y_use <- pred_ill_use_prob >= 0.5
# sum(pred_y_use)

# smoking
smoke <- as.factor(subset_us18$TOB_USE)
us18glm_new <- glm(ill_yr ~ age*education + age*income + 
                     age*marital + age*smoke, family = "binomial")
summary(us18glm_new)

pred_ill_use_prob <- predict.glm(us18glm_new, type = 'response')
with(summary(us18glm_new), 1 - deviance/null.deviance)
pred_y_use <- pred_ill_use_prob >= 0.5
sum(pred_y_use)


# drinking
drink <- as.factor(subset_us18$ALC_USE)
us18glm_new <- glm(ill_yr ~ age*education + age*income + 
                     age*marital + age*drink, family = "binomial")
summary(us18glm_new)

pred_ill_use_prob <- predict.glm(us18glm_new, type = 'response')
with(summary(us18glm_new), 1 - deviance/null.deviance)
pred_y_use <- pred_ill_use_prob >= 0.5
sum(pred_y_use)
