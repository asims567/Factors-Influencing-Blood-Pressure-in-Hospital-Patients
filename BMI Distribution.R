ggplot(Blood_Pressure, aes(x = BMI)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "maroon", color = "black") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(Blood_Pressure$BMI), 
                            sd = sd(Blood_Pressure$BMI)), 
                color = "red", size = 1) + 
  ggtitle("BMI Distribution with Normal Curve")
theme_classic()