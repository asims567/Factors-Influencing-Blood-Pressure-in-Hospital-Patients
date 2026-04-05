library(readxl)
Blood_Pressure <- read_excel("C:/Users/asims/Downloads/Blood Pressure.XLS")
View(Blood_Pressure)

class(Blood_Pressure)

ls()

Blood_Pressure$sbp

str(Blood_Pressure)
colnames(Blood_Pressure)

Blood_Pressure$sbp <- as.numeric(Blood_Pressure$sbp)
Blood_Pressure$BMI <- as.numeric(Blood_Pressure$BMI)
Blood_Pressure$age <- as.numeric(Blood_Pressure$age)

Blood_Pressure$sex <- factor(Blood_Pressure$sex, labels = c("Male", "Female"))
Blood_Pressure$smoke <- factor(Blood_Pressure$smoke, labels = c("Yes", "No"))
Blood_Pressure$married <- factor(Blood_Pressure$married, labels = c("Yes", "No"))
Blood_Pressure$race <- factor(Blood_Pressure$race, labels = c("white", "asian", "black", "other"))

mean(Blood_Pressure$sbp)
median(Blood_Pressure$sbp)
sd(Blood_Pressure$sbp)

mean(Blood_Pressure$BMI)
median(Blood_Pressure$BMI)
sd(Blood_Pressure$BMI)

table(Blood_Pressure$race)

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
summary(Blood_Pressure$BMI)