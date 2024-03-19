# Jared Adam 
# sweep net samples from ce2 beans to supplement the augmentation experiment 
# 3/18/2024

# packages ####
library(tidyverse)
library(vegan)
library(lme4)
library(emmeans)
library(performance)
library(multcomp)
library(ggpmisc)
library(lmtest)

# data ####
sweeps <- beans_sweeps

# wrangling ####
test <- sweeps %>% 
  dplyr::select(-Genus, -adult_juvenile, - Infraorder) %>% 
  pivot_wider(names_from = Order, 
              values_from = Family,
              values_fn = list(Family = length)) %>% 
  replace(is.na(.),0)
colnames(test) 

test2 <- sweeps %>% 
  dplyr::select(-Genus, -adult_juvenile, - Infraorder) %>% 
  pivot_wider(names_from = Family, 
              values_from = Order,
              values_fn = list(Order = length)) %>% 
  replace(is.na(.),0)
unique(sweeps$Infraorder)


now <- sweeps %>%
  group_by(Date) %>% 
  mutate(determin = Family) %>% 
  mutate(determin = case_when(Infraorder == 'Heteroptera' ~ 'Heteroptera',
                              Infraorder == 'Auchenorrhynca' ~ 'Auchenorrhynca',
                              Infraorder == 'Sternorrhyncha' ~ 'Sternorrhyncha',
                              Infraorder == 'Caelifera' ~ 'Caelifera',
                              Infraorder == 'Ensifera' ~ 'Ensifera',
                              Order == 'Thysanoptera' ~ 'Thrips',
                              .default = as.character(determin))) %>% 
  pivot_wider(names_from = determin, 
              values_from = determin, 
              values_fn = list(determin = length)) %>% 
  dplyr::select(-Genus, -adult_juvenile, -Order, -Family, -Infraorder, -na) %>% 
  replace(is.na(.),0) %>% 
  mutate(date = as.Date(Date, "%m/%d/%Y"),
         year = format(date , "%Y"))

groups <- now %>% 
  ungroup() %>% 
  mutate(spiders = Linyphiidae + Thomisidae + Lycosidae + Mysemindae + Mysmenidae + Salticidae + Tetragnathidae +
           Araneidae + Liniphiidae,
         beetle_preds = Cantharidae + Cicindelidae + Lampyridae + Coccinelidae,
         other_beetle = Cerambicidae + Tenebrionidae + Mordellidae + Chrysomedlidae + Chrysomelidae +Curculionidae +Scarabaeidae +
           Elateridae + Nitidulidae,
         other_hem = Heteroptera + Pentatomidae + Auchenorrhynca +Sternorrhyncha + Aphidae,
         pred_hem = Geocoridae,
         cael = Caelifera + Acrididae,
         ensif = Ensifera +Gryllidae) %>% 
  dplyr::select(-Linyphiidae ,- Thomisidae ,- Lycosidae ,- Mysemindae ,- Mysmenidae ,- Salticidae ,- Tetragnathidae ,
                  -Araneidae ,- Liniphiidae, -Cantharidae ,- Cicindelidae ,- Lampyridae ,- Coccinelidae,
                -Cerambicidae,-Tenebrionidae,-Mordellidae,-Chrysomedlidae,-Curculionidae,-Scarabaeidae,
                  -Elateridae,-Nitidulidae, -Chrysomelidae, -Heteroptera ,- Pentatomidae ,- Auchenorrhynca ,-Sternorrhyncha,
                - Aphidae, -Geocoridae, -Caelifera, -Acrididae, -Ensifera, -Gryllidae, -Date, -Plot) %>% 
  relocate(date, year, spiders, beetle_preds, other_beetle)

groups %>% 
  group_by(date) %>% 
  rowwise() %>% 
  mutate(totals = sum(c_across(spiders:ensif)))

group_sum <- groups %>% 
  group_by(date) %>% 
  summarise(sp_sum = sum(spiders),
            bpsum = sum(beetle_preds), 
            forsum = sum(Formicidae), 
            lwsum = sum(Hemerobiidae),
            phem = sum(pred_hem),
            en_sum = sum(ensif)) %>% 
  pivot_longer(
    cols = where(is.numeric))

groups %>% 
  group_by(year) %>% 
  summarise(sp_sum = sum(spiders),
            bpsum = sum(beetle_preds), 
            forsum = sum(Formicidae), 
            lwsum = sum(Hemerobiidae),
            phem = sum(pred_hem),
            en_sum = sum(ensif)) %>% 
  pivot_longer(
    cols = where(is.numeric))

ggplot(group_sum, aes(x = name, y = value))+
  geom_bar(stat = 'identity', position = 'dodge')

# spiders only ####
spider_sum <- now %>% 
  ungroup() %>% 
  dplyr::select(date, year,Linyphiidae, Thomisidae, Lycosidae, Mysemindae, Mysmenidae,Salticidae,Tetragnathidae,
           Araneidae,Liniphiidae) %>% 
  mutate(Linyphiidae = Liniphiidae + Linyphiidae,
         Mysmenidae = Mysemindae + Mysmenidae) %>%
  dplyr::select(-Liniphiidae, -Mysemindae) %>% 
  group_by(date) %>% 
  summarise(Linyphiidae = sum(Linyphiidae),
            Thomisidae = sum(Thomisidae),
            Lycosidae = sum(Lycosidae),
            Mysmenidae = sum(Mysmenidae),
            Salticidae = sum(Salticidae),
            Tetragnathidae = sum(Tetragnathidae),
            Araneidae = sum(Araneidae)
  ) %>% 
  pivot_longer(
    cols = where(is.numeric)
  )

ggplot(spider_sum, aes(x = name, y = value))+
  geom_bar(stat = 'identity', position = 'dodge')

ggplot(spider_sum, aes(x = name, y = value))+
  geom_point()

# stats ####
