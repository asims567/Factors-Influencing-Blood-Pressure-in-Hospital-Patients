library(ggplot2)

ggplot(Blood_Pressure, aes(x = sbp)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "skyblue", color = "black") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(Blood_Pressure$sbp), 
                            sd = sd(Blood_Pressure$sbp)), 
                color = "red", size = 1) + 
  ggtitle("SBP Distribution with Normal Curve")
theme_classic()