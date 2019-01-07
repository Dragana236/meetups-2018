library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

data <- read_csv('OPENDATA_SNEZGODE exc.csv')

##### Inspecting Data ######
# Pogledaj podatke
glimpse(data)

# Uzmi u obzir samo drugu, petu, sestu i sedmu varijablu
data <- data[, c(2, 5:7)]


# Promeni naziv kolona
# ovde moze i colnames()
names(data) <- c('Vreme_Nez', 'Vrsta_Nez', 'Naz_Tip', 'Naz_Det')

data

# Proveri nedostajuce vrednosti
(miss <- unlist(lapply(data, function(x) sum(is.na(x)))))

# samo 293 numerickih vrednosti od 12873 u koloni 'naz_tip' i 'naz_det'
nrow(data) - miss['Naz_Tip']  
nrow(data) - miss['Naz_Det']   


# Proveri koliko jedinstvenih vrednosti ima svaka kolona osim prve
(uniq <- unlist(lapply(data[, 2:4], function(x) length(unique(x)))))


# S obzirom da u kolonama 'Vrsta_Nez', 'Naz_Tip' i 'Naz_Det' postoji po nekoliko
# jedinstvenih vrednosti mozemo ih pretvoriti u faktore
(data <- data %>%
    mutate_each(funs(as.factor), Vrsta_Nez:Naz_Det))


# Pogledajmo koliko vrednosti ima svaka kategorija
lapply(data[, 2:4], function(x) table(x))


# Vidimo da najvise ima sa materijalnom stetom, SN SA NAJMANjE DVA VOZILA ??? BEZ SKRETANjA , 
#Najmanje dva vozila koja se kre??u u istom smeru ??? sustizanje

ggplot(data, aes(x = Vrsta_Nez)) + 
  geom_bar()

data %>%
  filter(!is.na(Naz_Tip)) %>%
  ggplot(aes(x = Naz_Tip)) + 
  geom_bar()

data %>%
  filter(!is.na(Naz_Det)) %>%
  ggplot(aes(x = Naz_Det)) + 
  geom_bar()


##### Data Wrangling #####
# Razdvoj Datum od vremena
(data <- data %>%
   separate(Vreme_Nez,
            into = c('Datum', 'Vreme'),
            sep = ','))

# Razdvoj Datum na Dan, Mesec i Godinu
(data <- data %>%
    separate(Datum,
             into = c('Dan', 'Mesec', 'Godina'),
             sep = '\\.'))

# Razdvoj Vreme na Sate i Minute
(data <- data %>%
    separate(Vreme,
             into = c('Sati', 'Minuti'),
             sep = ':'))

# Postoje podaci za 2014 i 2015 godinu iako bi ovo samo trebalo da je za 2015
data %>%
  count(Godina)


# Ispravi vrednosti za 2013 i 2014 u 2015 jer su verovatno greska
data$Godina[which(data$Godina == '2013')] <- '2015'
data$Godina[which(data$Godina == '2014')] <- '2015'

# Jedna vrednost za decembar
data %>%
  count(Mesec)

# Izbaci observaciju gde je mesec 12
(data <- data %>%
    filter(Mesec != 12))


# Kreiraj novu kolonu Ishod i promeni nivoe u 'Steta', 'Smrt', 'Povreda'
levels(data$Vrsta_Nez)
data$Ishod <- factor(data$Vrsta_Nez)
levels(data$Ishod) <- c('Steta', 'Smrt', 'Povreda')

data

# Isto uradi i za Naz_Tip
levels(data$Naz_Tip)
data$Tip_Nez = factor(data$Naz_Tip)
levels(data$Tip_Nez) <- c('1 vozilo', 'najmanje 2 vozila i bez skretanja',
                          'najmanje 2 vozila skretanje ili prelaz',
                          'sa parkiranim',
                          'sa pesacima')

data


##### Eksplorativna Analiza #####

# Contingency table
(tab <- table(data$Tip_Nez, data$Ishod))

(tab <- table(data$Naz_Det, data$Ishod))


# Kreiraj bar plot uzimajuci u obzir dve kategoricke varijable
data %>%                            # Ne postoji asocijativnost
  filter(!is.na(Tip_Nez)) %>%                
  ggplot(aes(x = Tip_Nez, fill = Ishod)) +
  geom_bar() 

data %>%
  filter(!is.na(Naz_Det)) %>%
  ggplot(aes(x = Naz_Det, fill = Ishod)) +
  geom_bar() 

# Fokusirajmo se na vreme
unique(data$Sati)

# Sati su u formi string, pretvori ih u brojeve 
# da bismo mogli aritmeticke operacije da obavljamo na njima
data$Sati <- as.numeric(data$Sati)

data

# Napravi od 'Sati' varijable faktor
# Nivoi su od 0-6 h, pa od 6-12 h itd.
data$Sati_Fakt <- cut(x = data$Sati, breaks = c(0, 6, 12, 18, 23))
levels(data$Sati_Fakt) <- c('night', 'morning', 'day', 'evening')

data

# Napravi contigency table za Ishod i Sati_Fakt (jer su sada i sati faktor)
table(data$Ishod, data$Sati_Fakt)

data %>%                
  ggplot(aes(x = Sati_Fakt, fill =  Ishod)) +
  geom_bar() 


# Grupisi po Sati i Ishod i sumiraj tako da nadjes broj vrednosti sa n()
final <- data %>%
  group_by(Sati, Ishod) %>%
  summarize(n = n())


table(data$Sati)

# Nacrtaj grafik zavisnosti Ishoda od Sati
ggplot(final, aes(x = Sati, y = n, color = Ishod)) +  # Generalno se nesrece najvise desavaju od 6 do 17 h
  geom_line()
