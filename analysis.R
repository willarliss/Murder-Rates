setwd("C:/Users/William/Desktop/school/junior_s2/stat")
m <- read.csv("murder_2016_prelim (2).csv")
library(dplyr)
library(tidyr)
library(ggplot2)

View(m)

m1 <- m %>%
  mutate(region = ifelse(state %in% c('Connecticut', 'Maine', 'Massachusetts',
                                      'New Hampshire', 'Rhode Island', 'Vermont',
                                      'New Jersey', 'New York', 'Pennsylvania',
                                      'Delaware', 'Maryland', 'D.C.'),
                         'Northeast', state),
         region = ifelse(state %in% c('Illinois', 'Indiana', 'Michigan', 'Ohio',
                                      'Wisconsin', 'Iowa', 'Kansas', 'Minnesota',
                                      'Missouri', 'Nebraska', 'North Dakota', 
                                      'South Dakota'), 
                         'Midwest', region),
         region = ifelse(state %in% c('Florida', 'Georgia', 'West Virginia', 
                                      'Kentucky', 'North Carolina', 'South Carolina', 
                                      'Virginia', 'Tennessee', 'Alabama', 
                                      'Mississippi', 'Arkansas', 'Louisiana'),
                         'Southeast', region),
         region = ifelse(state %in% c('Colorado', 'Wyoming', 'Montana', 'Idaho',
                                      'Washington', 'Oregon', 'Utah', 'Nevada', 
                                      'California', 'Alaska', 'Hawaii'),
                         'West', region),
         region = ifelse(state %in% c('Texas', 'Oklahoma', 'New Mexico', 'Arizona'),
                         'Southwest', region)) %>%
  gather(year, murders, X2015_murders, X2016_murders)

MurderData <- m %>%
  mutate(murder_rate_15 = (X2015_murders*10000) / population,
         murder_rate_16 = (X2016_murders*10000) / population,
         murder_rate_change = (murder_rate_16 - murder_rate_15)) %>%
  gather(year, murder_rate, murder_rate_15, murder_rate_16) %>%
  mutate(year = ifelse(year == 'murder_rate_15', '2015', '2016'),
         murders = m1$murders, region = m1$region) %>%
  select(year, city, state, region, population, murders, 
         murder_rate, change, murder_rate_change)

View(MurderData)

# Box plot of murders by region for 2015
MurderData %>%
  filter(year == '2015') %>%
  ggplot(mapping = aes(x = region,
                       y = murders,
                       fill = region)) +
  geom_boxplot() + theme_classic()

MurderData %>%
  filter(year == '2015') %>%
  group_by(region) %>%
  summarise(mean(murders))


# Box plot of murders by region for 2016
MurderData %>% 
  filter(year == '2016') %>%
  ggplot(mapping = aes(x = region,
                       y = murders,
                       fill = region)) +
  geom_boxplot() + theme_classic()

ggplot(data = MurderData,
       mapping = aes(x = region,
                     y = murders,
                     fill = region)) +
  geom_boxplot() + theme_classic() +
  facet_wrap(~year, ncol = 2)

MurderData %>%
  filter(year == '2016') %>%
  group_by(region) %>%
  summarise(mean(murders))

# Box plot of change in murder rate
ggplot(data = MurderData,
       mapping = aes(x = region,
                     y = murder_rate_change,
                     fill = region)) +
  geom_boxplot() + theme_classic()

MurderData %>%
  group_by(region) %>%
  summarise(mean(murder_rate_change))

# Histogram of change in murder rate
ggplot(data = MurderData,
       mapping = aes(x = murder_rate_change)) +
  geom_histogram(fill = 'blue',
                 color = 'white',
                 bins = 70) +
  theme_classic()

# 'Dot plot' of murders by population
MurderData %>%
  filter(year == '2016',
         murders > 115,
         population > 650000) %>%
  ggplot(mapping = aes(x = murders,
                       y = population,
                       color = region)) +
  geom_text(aes(label = city)) +
  theme_classic()

MurderData %>%
  filter(year == '2016') %>%
  ggplot(mapping = aes(x = murders,
                       y = population)) +
  geom_point() + 
  scale_x_log10() + scale_y_log10()

MurderData %>%
  filter(year == '2016') %>%
  ggplot(mapping = aes(x = population,
                       y = murders)) +
  geom_point()

# Top 10 increase in murder rate
MurderData %>%
  filter(year == 2016) %>%
  arrange(desc(murder_rate_change)) %>%
  select(city, population, change, murder_rate_change) %>%
  head(10)

# Top 10 decrease in murder rate
MurderData %>%
  filter(year == 2016) %>%
  arrange(murder_rate_change) %>%
  select(city, population, change, murder_rate_change) %>%
  head(10)

# Top 10 murder rates in 2015
MurderData %>%
  filter(year == '2015') %>%
  arrange(desc(murder_rate)) %>%
  select(city, population, murders, murder_rate) %>%
  head(10)

# Lowest 10 murder rates in 2015
MurderData %>%
  filter(year == '2015') %>%
  arrange(murder_rate) %>%
  select(city, population, murders, murder_rate) %>%
  head(10)

# Top 10 murder rates in 2016
MurderData %>%
  filter(year == '2016') %>%
  arrange(desc(murder_rate)) %>%
  select(city, population, murders, murder_rate) %>%
  head(10)

# Lowest 10 murder rates in 2016
MurderData %>%
  filter(year == '2016') %>%
  arrange(murder_rate) %>%
  select(city, population, murders, murder_rate) %>%
  head(10)

ggplot(data = MurderData,
       mapping = aes(x = population,
                     y = murder_rate_change)) +
  geom_point()
