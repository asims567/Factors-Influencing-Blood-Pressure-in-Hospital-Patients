library(readxl)
Blood_Pressure <- read_excel("C:/Users/asims/Downloads/Blood Pressure.XLS")
View(Blood_Pressure)

class(Blood_Pressure)
[1] "tbl_df"     "tbl"        "data.frame"

ls()
[1] "Blood_Pressure"

colnames(Blood_Pressure)
[1] "sbp"     "sex"     "married" "smoke"  
[5] "age"     "weight"  "race"    "BMI"  

Blood_Pressure$sbp <- as.numeric(Blood_Pressure$sbp)
Blood_Pressure$BMI <- as.numeric(Blood_Pressure$BMI)
Blood_Pressure$age <- as.numeric(Blood_Pressure$age)

Blood_Pressure$sex <- factor(Blood_Pressure$sex, labels = c("Male", "Female"))
Blood_Pressure$smoke <- factor(Blood_Pressure$smoke, labels = c("Yes", "No"))
Blood_Pressure$married <- factor(Blood_Pressure$married, labels = c("Yes", "No"))
Blood_Pressure$race <- factor(Blood_Pressure$race, labels = c("white", "asian", "black", "other"))

mean(Blood_Pressure$sbp)
[1] 125.8
median(Blood_Pressure$sbp)
[1] 130
sd(Blood_Pressure$sbp)
[1] 13.39205

mean(Blood_Pressure$BMI)
[1] 28.68717
median(Blood_Pressure$BMI)
[1] 26.60114
sd(Blood_Pressure$BMI)
[1] 8.855383

table(Blood_Pressure$race)
white asian black other 
   40     8     1     1 

library(ggplot2)

ggplot(Blood_Pressure, aes(x = sbp)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "skyblue", color = "black") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(Blood_Pressure$sbp), 
                            sd = sd(Blood_Pressure$sbp)), 
                color = "red", size = 1) + 
  ggtitle("SBP Distribution with Normal Curve")
  theme_classic()
  
  ggplot(Blood_Pressure, aes(x = BMI)) +
    geom_histogram(aes(y = ..density..), bins = 10, fill = "maroon", color = "black") +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(Blood_Pressure$BMI), 
                              sd = sd(Blood_Pressure$BMI)), 
                  color = "red", size = 1) + 
    ggtitle("BMI Distribution with Normal Curve")
  theme_classic()

ggplot(Blood_Pressure, aes(x = race)) + 
  geom_bar() + 
  ggtitle("Race Distribution")

summary(Blood_Pressure$sbp)
 Min. 1st Qu.  Median    Mean 3rd Qu. 
   67.0   117.8   130.0   125.8   133.8 
   Max. 
  140.0 

summary(Blood_Pressure$BMI)
Min. 1st Qu.  Median    Mean 3rd Qu. 
  12.09   21.64   26.60   28.69   34.51 
   Max. 
  49.88


anova_result <- aov(sbp ~ race, data = Blood_Pressure)
summary(anova_result)
 Df Sum Sq Mean Sq F value Pr(>F)
race         3    313   104.3   0.566   0.64
Residuals   46   8475   184.2 

model <- lm(sbp ~ age + BMI + sex + smoke, data = Blood_Pressure)
summary(model)

lm(formula = sbp ~ age + BMI + sex + smoke, data = Blood_Pressure)

Residuals:
    Min      1Q  Median      3Q     Max 
-52.057  -6.345   4.116   7.689  19.746 

Coefficients:
             Estimate Std. Error t value
(Intercept) 117.52064    8.50286  13.821
age          -0.04352    0.13014  -0.334
BMI           0.19682    0.21817   0.902
sexFemale     5.70312    3.85469   1.480
smokeNo       2.81901    3.85615   0.731
            Pr(>|t|)    
(Intercept)   <2e-16 ***
age            0.740    
BMI            0.372    
sexFemale      0.146    
smokeNo        0.469    
---
Signif. codes:  
  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1
  ‘ ’ 1

Residual standard error: 13.46 on 45 degrees of freedom
Multiple R-squared:  0.07288,	Adjusted R-squared:  -0.009529 
F-statistic: 0.8844 on 4 and 45 DF,  p-value: 0.481
