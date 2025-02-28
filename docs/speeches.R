# Clear de "Global Environment"
rm(list=ls()) 

# Sets the working directory
setwd("~/Documents/quants") 

# Install/load packages
install.packages("tidytext")
library(tidyverse)
library(tidytext)

# Importing the data
speeches <- read_csv("data/state-of-the-union-texts.csv")

# Inspecting the data
speeches

speeches |>
  count(Year)

speeches |> 
  count(President, sort = TRUE)

# Reporting a particular speech (number 4 in this case)
speeches$Text[4]


### Word counts

speeches <- speeches |>
  mutate(woman = str_count(Text, "woman"),
         women = str_count(Text, "women"),
         sum_wom = woman + women)

speeches

# Descriptive statistics
speeches |>
  summarize(sum = sum(sum_wom),
            mean = mean(sum_wom, na.rm = TRUE))

# Evolution over time
speeches %>%
  filter(Year>1790) |>
  ggplot(aes(x = Year, y = sum_wom)) +
  geom_line()

# Lenght of each speech
speeches <- speeches |>
  mutate(word_count = str_count(Text, "[\\w]+"))

speeches |>
  filter(Year>1790) |>
  ggplot(aes(x = Year, y = word_count)) + 
  geom_point() + 
  geom_line()

# Relativise word counting (by speech lenght)
speeches %>% 
  mutate(sum_rel = sum_wom/word_count) |>
  filter(Year>1790) %>%
  ggplot(aes(x = Year, y = sum_rel)) + 
  geom_point() + 
  geom_line() 

### Top frequencies

# Tokenisation
data_token <- speeches |> 
  unnest_tokens(output = words, input = Text) 

data_token # 1.8 million rows

data_token |> 
  count(words, sort = TRUE) %>% 
  print(n = 20)

# Stop words
stop_words |>   
  print(n = 25)

# Exclude stop words
data_token <- data_token |>
  anti_join(stop_words, by = c("words" = "word"))
data_token

data_token |> 
  count(words, sort = TRUE) |>  
  print(n = 20)

data_token |> 
  mutate(period = ifelse(Year <= 1900, "19th c.", "20th c.")) |> 
  group_by(period) |> 
  count(words, sort=TRUE) |> 
  mutate(proportion = n/sum(n)*1000) |> 
  slice_max(order_by=proportion, n = 15) |> 
  mutate(words = reorder(words, desc(proportion))) |> 
  ggplot(aes(reorder_within(x = words, 
                            by = proportion, within = period),
             proportion, fill = period)) + 
  geom_col() +
  scale_x_reordered() +
  scale_fill_manual(values = c("blue", "#56B4E9")) +
  coord_flip() +
  facet_wrap(~period, ncol = 2, scales = "free") +
  xlab("Word")

### n-grams

data_token |> 
  mutate(war = ifelse(words == "war", 1, 0)) |>  
  group_by(Year) |>
  summarize(fr_war = mean(war, na.rm = TRUE)) |>
  ggplot() +
  geom_col(aes(x = Year, y = fr_war))

# multiple-grams

data_token2 <- speeches |>
  unnest_tokens(twogram, Text, token = "ngrams", n = 2)

data_token2

# words accompanying other words
women <- data_token2 |> 
  separate_wider_delim(cols = twogram, delim = " ", 
                       names = c("g1", "g2")) |>
  filter(g1 == "women" | g2 == "women") |>
  pivot_longer(g1:g2) |>
  select(!name) |>
  rename(words = value) |>
  filter(words!="women") |>
  anti_join(stop_words, by = c("words" = "word")) 

women

women |> 
  count(words, sort=TRUE)

women |> 
  filter(words == "pregnant") |>
  count(Year)