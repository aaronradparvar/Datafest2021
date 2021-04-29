us18 <- read.csv("us_18Q1.csv")
age <- us18$DEM_AGE
income <- us18$DEM_INCOME
education <- us18$DEM_EDU
ill_yr <- us18$ILL_YR
typeof(ill_yr)
ill_yr <- as.factor(ill_yr)
#1 predictor
us18glm <- glm(y~x, family = "binomial")
us18glmplot <- plot(us18glm)
predicted_misuse <- predict.glm(us18glm)
#add more predictors
#2 predictors (Stick with this for now)
us18glm <- glm(ill_yr~age+income, family = "binomial")
us18glm
##us18glmplot <- plot(us18glm)
#3 predictors
us18glm <- glm(ill_yr~education+age+income, family = "binomial")
us18glm
summary(us18glm)
#Education and income
us18glm <- glm(ill_yr~education+income, family = "binomial")
us18glm
summary(us18glm)
#Age and education
us18glm <- glm(ill_yr~age+education, family = "binomial")
us18glm
summary(us18glm)
#Age and income
us18glm <- glm(ill_yr~age+income, family = "binomial")
us18glm
summary(us18glm)
#Use corr