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
