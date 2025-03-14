iris
str(iris)

install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point(size = 3) +
  labs(title = "wykres dlugosci dzialki i dlugosci platka",
       x = "dlugosc dzialki kielicha",
       y = "dlugosc platka kielicha")

install.packages("EnvStats")
library(EnvStats)
install.packages("moments")
library(moments)


desc_stat <- iris %>%
  group_by(Species) %>%
  summarise(SL_mean = mean(Sepal.Length),
            SL_median = median(Sepal.Length),
            SL_Q1 = quantile(Sepal.Length, probs = 0.25),
            SL_Q3= quantile(Sepal.Length, probs = 0.75),
            IQR = SL_Q3 - SL_Q1,
            SL_sd = sd(Sepal.Length),
            SL_Vs = SL_sd / SL_mean,
            #napisz dominante na zadanie
            #napisac cos jeszcze?????
            SL_skew = EnvStats::skewness(Sepal.Length),
            SL_kurt =  moments::kurtosis(Sepal.Length))


ggplot(iris, aes(x = Sepal.Length)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.1,
                 fill = "pink",
                 color = "black") +
  facet_wrap(~Species) + 
  geom_density(color = "red", size = 1.2) + 
  geom_vline(data = desc_stat, aes(xintercept = SL_mean), linetype = "dashed", size = 1,
             color = "blue") + 
  geom_vline(data = desc_stat, aes(xintercept = SL_median), linetype = "dotted",color = "green", size = 1)


ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot(width = 0.1) +
  geom_violin(fill = "lightpink") +
  geom_boxplot(width = 0.1)

iris %>%
  filter(Sepal.Length > 6) %>%
  group_by(Species) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = Species, y = n)) +
    geom_bar(stat = "identity")

economics %>%
  ggplot(aes(x = date, y = pop)) +
    geom_line() +
    theme_minimal()

