# Clear de "Global Environment"
rm(list=ls()) 

# Sets the working directory
setwd("~/Documents/quants") 

# Install/load packages
install.packages("tidyverse")
library(tidyverse)
library(readxl)

# Import dataset
data <- read_excel("data/paisley-data.xlsx")

# Inspecting the data
data

data |>  
  print(n = 15) # print more observations

view(data)


### Categorical (qualitative) variables

# Frequency table
data |>  
  count(sex) 

data |>  
  count(employed)

data |>  
  count(occup) |>
  print(n = 15)

data |>  
  count(occup, sort = TRUE)

data |>  
  count(occup) |> 
  arrange(n) |> 
  print(n=15) 

data |>  
  count(occup, sort = TRUE) |>              
  mutate(prop = n/sum(n)) 

data |> 
  filter(sex=="female") |>   # narrow down the analysis
  count(literacy, sort = TRUE) |>              
  mutate(prop = n/sum(n)) 

### Numerical variables

data |>  
  count(age) |>              
  mutate(perc = 100*n/sum(n))

# Histogram
data |>   
  ggplot(aes(x = age)) +
  geom_histogram(binwidth = 5) +
  scale_x_continuous(breaks = seq(0, 90, 10))

# Descriptive statistics
data |> 
  summarize(mean_age = mean(age, na.rm = TRUE))

data |> 
  summarize(
    obs = sum(!is.na(age)),
    mean = mean(age, na.rm = TRUE), 
    min = min(age, na.rm = TRUE),
    max = max(age, na.rm = TRUE)) 

### Bivariate statistics

data <- data |>
  mutate(weight_kg = 0.453592*weight) # weight in kgs. (instead of pounds)

# Comparing weight by age-group
data |> 
  filter(age>=20) |>
  mutate(age_class = cut(age, breaks = seq(19, 89, 10))) |> 
  group_by(age_class) |>
  summarise(obs = sum(!is.na(weight_kg)),
            mean_weight = mean(weight_kg, na.rm = TRUE))

# Graph: weight by age
data |> 
  filter(age>=20) |>
  group_by(age) |>
  summarise(mean_weight = mean(weight_kg, na.rm = TRUE)) |>
  ggplot(aes(x = age, y = mean_weight, group = 1)) +
  geom_line()

# Distinguishing by sex
data |>
  filter(age>=20 & age<80) |>
  group_by(age, sex) |>
  summarise(mean_weight = mean(weight_kg, na.rm = TRUE)) |> 
  ggplot(aes(x = age, y = mean_weight, color = sex)) +
    geom_point() +
    geom_line()
