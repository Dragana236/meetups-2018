library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)

# Load the dataset
maha <- read_csv('162maha.csv')

# Data Cleaning
head(maha)

glimpse(maha)

maha <- maha %>% 
  select(c(2, 3, 4), 8:14, 24:26)

maha

# Change all the character columns to factors #
maha <- maha %>% 
  mutate_if(is.character, as.factor)


# Make a two column dataset with variable names and number of levels MOZDA
maha_levels <- maha %>% 
  summarise_all(nlevels)

maha_levels %>%
  gather(variable, num_levels)

# Examining levels
maha %>%
  pull(Major) %>%
  levels()

maha %>% 
  pull(University) %>% 
  levels()

# EDA
# How many males and Females are in the dataset?
maha %>%  # in general there are more Females
  count(Gender)


# Remove 'Other' level
maha <- maha %>%
  filter(Gender != 'Other') %>%
  droplevels()

maha %>%   # much more Female students
  ggplot(aes(x  = Gender)) +
  geom_bar()

maha %>% 
  count(Major)

maha %>%   
  ggplot(aes(x  = Major)) +
  geom_bar()

ggplot(maha, aes(x = Major)) + 
  geom_bar() + 
  coord_flip()

# or 
ggplot(maha, aes(x = Major)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90))

# 2-way contingency table
table(maha$Gender, maha$Major)

ggplot(maha, aes(x = Gender, fill = Major)) + 
  geom_bar(position = "dodge") +
  ylab("counts")

# Conditional proportions
options(scipen = 999, digits = 3) 
prop.table(table(maha$Gender, maha$Major))


options(scipen = 999, digits = 3) 
prop.table(table(maha$Gender, maha$Major), 1)

#conditionals on columns
prop.table(table(maha$Gender, maha$Major), 2)

# Plot proportion of gender, conditional on align
ggplot(maha, aes(x = Gender, fill = Major)) + 
  geom_bar(position = "fill") +
  ylab("proportion")


ggplot(maha, aes(x = Major, fill = Gender)) + 
  geom_bar(position = "fill") +
  ylab("proportion") +
  theme(axis.text.x = element_text(angle = 90))


# Are girls more confident than boys?
maha_tidy <- maha %>% 
  select(1, 4:13) %>% 
  gather(my_key, my_value, -Gender) %>% 
  mutate(secure = ifelse(my_value %in% c(4,5), 1, 0)) 

maha_summary <- maha_tidy %>% filter(!is.na(secure)) %>% 
  group_by(Gender) %>% 
  summarise(average = mean(secure))

# If...else statement
my_value <- c(1, 2, 3, 4, 5) 
if ((my_value == 4)｜(my_value == 5)) {
  print('1')
} else {
  print('0')
}

maha %>% 
  select(c(1,3), 4:13) %>% 
  gather(my_key, my_value, -c(Gender, Major)) %>% 
  mutate(secure = ifelse(my_value %in% c(4,5), 1, 0)) %>% 
  filter(!is.na(secure)) %>% 
  group_by(Major, Gender) %>% 
  summarise(average = mean(secure)) %>% 
  ggplot(aes(x = Gender, y = average)) +
  geom_col() +
  facet_wrap(~Major)


maha_fem <- maha %>% 
  filter(Gender == 'Female')


# Ordering one variable by another
maha_fem %>% 
  select(c(2, 3), 4:13) %>% 
  gather(my_key, my_value, -c(Major, University)) %>% 
  mutate(secure = ifelse(my_value %in% c(4,5), 1, 0)) %>% 
  filter(!is.na(secure)) %>% 
  group_by(University) %>% 
  summarise(average = mean(secure)) %>% 
  ggplot(aes(x = fct_rev(fct_reorder(University, average)), y = average)) +
  geom_col() +
  coord_flip() +
  labs(title = "Girls are not confident",
       subtitle = "Percentage of confidence per University",
       x = "",
       y = "") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_text(aes(label = scales::percent(average), 
                y = average + .13), 
            position = position_dodge(0.9),
            vjust = 0.5)

