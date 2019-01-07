##### Initial setup ######
library(readr)
library(dplyr)
library(ggplot2)

# Load the file
gapminder <- read_csv('gapminder.csv')

# Inspect the data
head(gapminder)

gapminder <- gapminder %>%
  select(-1)

glimpse(gapminder)


##### filter #####
# Filter the gapminder dataset for the year 1957
gapminder %>%
  filter(year == 1957)

# Filter for China in 2002
gapminder %>%
  filter(country == 'China', year == 1957)

# Filter for China in 1957 and 2002
# comes in handy %in%
gapminder %>%
  filter(country == 'China', year %in% c(1957, 2002))


##### arrange #####
# Sort in ascending order of lifeExp
gapminder %>%
  arrange(lifeExp)

# Sort in descending order of lifeExp
gapminder %>%
  arrange(desc(lifeExp))

# Find the country with the highest population in a  1957 on Africa
gapminder %>%
  filter(year == 1957, continent == 'Africa') %>%
  arrange(desc(pop))


##### mutate #####
# Use mutate to change lifeExp to be in months
gapminder %>%
  mutate(lifeExp = lifeExp * 12)


# Use mutate to create a new column called lifeExpMonths
gapminder %>%
  mutate(lifeExpMonths = lifeExp * 12)


# Find the countries with the highest life expectancy, in months, in the year 2007
gapminder %>%
  filter(year == 2007) %>%
  mutate(lifeExpMonths = 12 * lifeExp) %>%
  arrange(desc(lifeExpMonths)) %>%
  select(country)

##### summarize #####
# Summarize to find the median life expectancy
gapminder %>%
  summarize(medianLifeExp = median(lifeExp))

# Filter for 1957 then summarize the median life expectancy
gapminder %>%
  filter(year == 1957) %>%
  summarize(medianLifeExp = median(lifeExp))

# Filter for 1957 then summarize the median life expectancy and the maximum GDP per capita
gapminder %>%
  filter(year == 1957) %>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))

##### group_by #####
# Find median life expectancy and maximum GDP per capita in each year
gapminder %>%
  group_by(year) %>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))


# Find median life expectancy and maximum GDP per capita in each continent in 1957
gapminder %>%
  filter(year == 1957) %>%
  group_by(continent) %>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))


# Find median life expectancy and maximum GDP per capita in each year/continent combination
by_year_continent <- gapminder %>%
  group_by(continent, year) %>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))


# Plot trends
ggplot(by_year_continent, aes(x = year, y = medianLifeExp, color = continent)) +
  geom_line() +
  expand_limits(y = 0)

