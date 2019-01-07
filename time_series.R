library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# Ucitavanje podataka
test <- read_tsv('test_podaci2.txt', 
                 col_names = c('year', 'month', paste0('day_', 1:31)))

# Inspect podatke
head(test)
glimpse(test)
summary(test)

# Proveri sta ima u koloni day_31
test %>%
  select('day_31')

# Proveri koliko je ovih oznaka za NA u okviru sa podacima
sum(test == -999.9)

# Proveri da li ima NAs
sum(is.na(test))

##### Ciscenje podataka #####

# Koje kolone imaju imaju ove vrednosti?
sapply(test, function(x) sum(x == -999.9))


# Promeni ove vrednosti u NA
test$day_29[which(test$day_29 == -999.9)] <- NA
test$day_30[which(test$day_30 == -999.9)] <- NA
test$day_31[which(test$day_31 == -999.9)] <- NA

sum(is.na(test))

# Skupi kolone
(test_tidy <- test %>%
    gather(day, Tmax, -c(year, month)))

# Razdvoj vrednosti u day koloni
test_tidy <- test_tidy %>%
  separate(day, into = c('day1', 'day'))

test_tidy

# Proveri da li su u day1 koloni samo 'day' vrednosti
test_tidy %>%
  count(day1)

# Ukloni day kolonu
(test_tidy <- test_tidy %>%
    select(-day1))

# Pretvori day kolonu u numericku
(test_tidy <- test_tidy %>%
    mutate(day = as.numeric(day)))

# Napisi prethodne korake u 'jendnom' redu sa %>%
test %>%
  gather(day, Tmax, -c(year, month)) %>%
  separate(day, into = c('day1', 'day')) %>%
  select(-day1) %>%
  mutate(day = as.numeric(day))


##### Explorativna analiza #####

test_tidy %>%
  count(year)

test_tidy %>%
  count(month)

test_tidy %>%
  count(day)  # 31 dan

# Numericka EDA
# Mere centralne tendencije 
test_tidy %>%
  summarize(mean = mean(Tmax, na.rm = TRUE), 
            median = median(Tmax, na.rm = TRUE))

# Odredi modu
table(test_tidy$Tmax)

# Mere centralne tendencije po danima kroz sve mesece i godine
test_tidy %>%  # nema puno smisla ali samo ilustrujemo
  group_by(day) %>%
  summarize(mean = mean(Tmax, na.rm = TRUE), 
            median = median(Tmax, na.rm = TRUE))

# Mere centralne tendencije za januar uzimajuci sve dane u tom mesecu i sve godine
test_tidy %>%    # ovo vec ima smisla
  filter(month == 1) %>%
  group_by(day) %>%
  summarize(mean = mean(Tmax, na.rm = TRUE), 
            median = median(Tmax, na.rm = TRUE))

# Mere centralne tendencije po mesecima kroz sve godine
test_tidy %>%
  group_by(month) %>%
  summarize(mean = mean(Tmax, na.rm = TRUE), 
            median = median(Tmax, na.rm = TRUE))

# Mere centralne tendencije po godinama
test_tidy %>%                 # 1961 godina najtoplija
  group_by(year) %>%
  summarize(mean = mean(Tmax, na.rm = TRUE), 
            median = median(Tmax, na.rm = TRUE))

# Grupisanje po dve varijable korisno posle za grafike

# Mere varijabilnosti
test_tidy %>%
  summarize(SD = sd(Tmax, na.rm = TRUE), IQR = IQR(Tmax, na.rm = TRUE))

ggplot(test_tidy, aes(y = Tmax)) +
  geom_boxplot()


# Range
diff(range(test_tidy$Tmax, na.rm = TRUE))



# Vizuelna EDA
# Distibucija jedne numericke varijable

# Histogram za Tmax (iako nema smisla za sve temperature kroz mesece)
ggplot(test_tidy, aes(x = Tmax)) +
  geom_histogram()


# Density plot za Tmax
ggplot(test_tidy, aes(x = Tmax)) +
  geom_density()


# Box plot plot za Tmax
ggplot(test_tidy, aes(x = 1, y = Tmax)) +
  geom_boxplot() 

# Histogram za samo januar kroz godine
test_tidy %>%
  filter(month == 1) %>%
  ggplot(aes(x = Tmax)) +
  geom_histogram()

# Density plot za samo januar kroz godine
test_tidy %>%
  filter(month == 1) %>%
  ggplot(aes(x = Tmax)) +
  geom_density()

# Grafik dve varijable - jedna kategoricka, druga numericka
# Jedna kategoricka varijabla
test_tidy %>%
  group_by(year) %>%
  summarize(mean = mean(Tmax, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean)) +
  geom_col()


test_tidy %>%
  group_by(month) %>%
  summarize(mean = mean(Tmax, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = mean)) +
  geom_col()


# Samo januar kroz godine
test_tidy %>%
  filter(month == 1) %>%
  group_by(year) %>%
  summarize(mean = mean(Tmax, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean)) +
  geom_col()

# Faceting
# Hocemo da vidimo distribuciju temperature po godini (iako malo nema smisla)
test_tidy %>%
  ggplot(aes(x = Tmax)) +
  geom_histogram() +
  facet_wrap(~ year)

test_tidy %>%
  ggplot(aes(x = Tmax)) +
  geom_density() +
  facet_wrap(~ year)

# Overlaid density plot
test_tidy %>%
  ggplot(aes(x = Tmax, fill = as.factor(year))) +
  geom_density(alpha = .3)

# Box plot
test_tidy %>%
  ggplot(aes(x = as.factor(year), y = Tmax)) +
  geom_boxplot()


# Hocemo da vidimo distribuciju temperature po za januar po godini
test_tidy %>%
  filter(month == 1) %>%
  ggplot(aes(x = Tmax)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~ year)

# Isto i za density plot

# Isto i za overlaid density plot

# Boxplot
test_tidy %>%
  filter(month == 1) %>%
  ggplot(aes(x = as.factor(year), y = Tmax)) +
  geom_boxplot()

# Hocemo da vidimo distirbuciju Tmax za sve mesece
test_tidy %>%
  ggplot(aes(x = Tmax)) +
  geom_histogram() +
  facet_wrap(~ month)

# Isto i sa density plot

# Isto i za overlaid density plot

# Boxplot
test_tidy %>%
  ggplot(aes(x = as.factor(month), y = Tmax)) +
  geom_boxplot()

# Grafik dve varijable - obe numericke
# Koja je prosecna Tmax za svaki mesec kroz godine

test_grouped <- test_tidy %>%
  group_by(year, month) %>%
  summarise(mean = mean(Tmax))

ggplot(test_grouped, aes(x = month, y = mean, col = as.factor(year))) +
  geom_line()


# Koja je max Tmax za svaki mesec kroz godine
test_grouped <- test_tidy %>%
  group_by(year, month) %>%
  summarise(max = max(Tmax))

ggplot(test_grouped, aes(x = month, y = max, col = as.factor(year))) +
  geom_line()


# Koja je sredjna vrednost Tmax po godini
test_tidy %>%
  group_by(year) %>%
  summarize(mean = mean(Tmax)) %>%
  ggplot(aes(x = year, y = mean)) +
  geom_line()

# Koja je godina imala najvisu srednju Tmax
test_tidy %>%
  ggplot(aes(x = Tmax)) +
  geom_histogram() +
  facet_wrap(~ year)


# Kako se menjala temperatura 1. Januar kroz godine
test_tidy %>%
  filter(month == 1, day == 1) %>%
  ggplot(aes(x = year, y = Tmax)) +
  geom_line()


# Za kraj posmatraj samo godinu 1961
test_tidy %>%
  ggplot(aes(x = day, y = Tmax, col = as.factor(month))) +
  geom_line()

test_tidy %>%
  filter(month == 1) %>%
  ggplot(aes(x = day, y = Tmax, col = as.factor(year))) +
  geom_line()

# Iz neurednog data seta moze ovo da se izvuce
test %>%
  ggplot(aes(x = month, y = day_1, col = as.factor(year))) +
  geom_line()


# Line plot
test_line <- test_tidy %>%
  unite(date, year, month, day, sep = '-')

# Pretvori date date kolonu u klasu date
test_line <- test_line %>%
  mutate(date = ymd(date))
test_line

ggplot(test_line, aes(x = date, y = Tmax)) +
  geom_line()
