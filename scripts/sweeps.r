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
library(RColorBrewer)

# data ####
sweeps <- beans_sweeps

# wrangling ####
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
                - Aphidae, -Geocoridae, -Caelifera, -Acrididae, -Ensifera, -Gryllidae, -Date) %>% 
  rename(plot = Plot) %>%
  mutate(trt = case_when(plot %in% c('101','203','304','401','503') ~ '1',
                         plot %in% c('102', '201','303','402','502') ~ '3',
                         plot %in% c('103','204','302','403','501') ~ '2',
                         plot %in% c('104','202','301','404','504') ~ '4')) %>% 
  relocate(date, year, plot, trt,  spiders, beetle_preds, other_beetle) %>% 
  mutate_at(vars(1:4), as.factor)

groups %>% 
  group_by(date) %>% 
  rowwise() %>% 
  mutate(totals = sum(c_across(spiders:ensif)))

group_sum <- groups %>%  
  summarise(spiders = sum(spiders),
            beetle_preds = sum(beetle_preds), 
            formicid = sum(Formicidae), 
            lacewing = sum(Hemerobiidae),
            hemip_pred = sum(pred_hem),
            ensifera = sum(ensif)) %>% 
  pivot_longer(
    cols = where(is.numeric))

groups %>% 
  group_by(year) %>% 
  summarise(spiders = sum(spiders),
            beetle_preds = sum(beetle_preds), 
            formicid = sum(Formicidae), 
            lacewing = sum(Hemerobiidae),
            hemip_pred = sum(pred_hem),
            ensifera = sum(ensif)) %>% 
  pivot_longer(
    cols = where(is.numeric))

ggplot(group_sum, aes(x = name, y = value))+
  geom_bar(stat = 'identity', position = 'dodge')

# spiders only ####
spider_long_sum <- now %>% 
  ungroup() %>% 
  dplyr::select(date, year, Plot, Linyphiidae, Thomisidae, Lycosidae, Mysemindae, Mysmenidae,Salticidae,Tetragnathidae,
           Araneidae,Liniphiidae) %>% 
  mutate(Linyphiidae = Liniphiidae + Linyphiidae,
         Mysmenidae = Mysemindae + Mysmenidae) %>%
  dplyr::select(-Liniphiidae, -Mysemindae) %>% 
  group_by(date, Plot) %>% 
  mutate(Plot = as.factor(Plot)) %>% 
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

spider_plot_sum <- now %>% 
  ungroup() %>% 
  dplyr::select(date, year, Plot, Linyphiidae, Thomisidae, Lycosidae, Mysemindae, Mysmenidae,Salticidae,Tetragnathidae,
                Araneidae,Liniphiidae) %>% 
  mutate(Linyphiidae = Liniphiidae + Linyphiidae,
         Mysmenidae = Mysemindae + Mysmenidae) %>%
  dplyr::select(-Liniphiidae, -Mysemindae) %>% 
   mutate(trt = case_when(Plot %in% c('101','203','304','401','503') ~ '1',
                         Plot %in% c('102', '201','303','402','502') ~ '3',
                         Plot %in% c('103','204','302','403','501') ~ '2',
                         Plot %in% c('104','202','301','404','504') ~ '4')) %>%
  relocate(date, year, Plot, trt) %>% 
  mutate_at(vars(1:4), as.factor) %>%  
  mutate(Plot = as.factor(Plot)) %>% 
  summarise(Linyphiidae = sum(Linyphiidae),
            Thomisidae = sum(Thomisidae),
            Lycosidae = sum(Lycosidae),
            Mysmenidae = sum(Mysmenidae),
            Salticidae = sum(Salticidae),
            Tetragnathidae = sum(Tetragnathidae),
            Araneidae = sum(Araneidae)) %>% 
  pivot_longer(
    cols = where(is.numeric)
  )

spider_wide <- now %>% 
  ungroup() %>% 
  dplyr::select(date, year, Plot, Linyphiidae, Thomisidae, Lycosidae, Mysemindae, Mysmenidae,Salticidae,Tetragnathidae,
                Araneidae,Liniphiidae) %>% 
  mutate(Linyphiidae = Liniphiidae + Linyphiidae,
         Mysmenidae = Mysemindae + Mysmenidae) %>%
  dplyr::select(-Liniphiidae, -Mysemindae) %>% 
  rename(plot = Plot) %>% 
  mutate(trt = case_when(plot %in% c('101','203','304','401','503') ~ '1',
                          plot %in% c('102', '201','303','402','502') ~ '3',
                          plot %in% c('103','204','302','403','501') ~ '2',
                          plot %in% c('104','202','301','404','504') ~ '4')) %>% 
  relocate(date, year, plot, trt) %>% 
  mutate_at(vars(1:4), as.factor) 


# stats ####
# all groups ~ trt
groups

aov_group <- apply(groups[,5:ncol(groups)], 2, function(x) aov(x ~ trt, data = groups))
tukey_group <- sapply(aov_group, function(x) TukeyHSD(x , 'trt', ordered = TRUE))
# this is nice 

aov_group_df <- do.call(rbind, lapply(aov_group, broom::tidy))
# this df is hard to look at
tukey_group_df <- as.data.frame(do.call(rbind, Map(cbind, Name = names(tukey_group), tukey_group)))


###

# long groups 

group_sum

kruskal.test(value ~ name, data = group_sum)
gt <- as.data.frame(dunn.test::dunn.test(group_sum$value, group_sum$name))

gdf <- flextable(gt) %>% 
  set_header_labels(gdf,
                    values = list(
                      chi2 = 'X^2',
                      Z = 'z',
                      P = 'p value',
                      P.adjusted = 'Adjusted p value',
                      comparisons = 'Group comparisons'
                    )) 
gdf <- theme_zebra(gdf)
gdf <- add_header_lines(gdf, 
                        values = "T.test results for group counts")
autofit(gdf) %>% 
  save_as_docx(path = 'T.test results for group counts.docx')




spider_wide
# spiders ~ trt

aov_fam <- apply(spider_wide[,5:ncol(spider_wide)], 2, function(x) aov(x ~ trt, data = spider_wide))
tukey_fam <- sapply(aov_fam, function(x) TukeyHSD(x , 'trt', ordered = TRUE))
# this is nice 

aov_fam_df <- do.call(rbind, lapply(aov_fam, broom::tidy))
# this df is hard to look at
tukey_fam_df <- as.data.frame(do.call(rbind, Map(cbind, Name = names(tukey_fam), tukey_fam)))

###

# long spider

spider_plot_sum

kruskal.test(value ~ name, data = spider_plot_sum)
st <- as.data.frame(dunn.test::dunn.test(spider_plot_sum$value, spider_plot_sum$name))

sdf <- flextable(st) %>% 
  set_header_labels(sdf,
                    values = list(
                      chi2 = 'X^2',
                      Z = 'z',
                      P = 'p value',
                      P.adjusted = 'Adjusted p value',
                      comparisons = 'Family comparisons'
                    )) 
sdf <- theme_zebra(sdf)
sdf <- add_header_lines(sdf, 
                        values = "T.test results for spider family counts")
autofit(sdf) %>% 
  save_as_docx(path = 'T.test results for spider family counts.docx')



# plots ####
display.brewer.pal(n = 8, name = 'Dark2')
brewer.pal(n = 8, name = 'Dark2')


colors <- c(Thomisidae = "#666666", Salticidae="#E6AB02", Tetragnathidae="#A6761D", Araneidae="#1B9E77", 
            Linyphiidae="#D95F02", Lycosidae="#7570B3", Mysmenidae="#E7298A")
ggplot(spider_plot_sum, aes(x = name, y = value))+
  geom_bar(position = 'dodge', stat = 'identity', aes(fill = name), alpha = 0.7)+
  scale_x_discrete(limits = c("Thomisidae", "Salticidae", "Tetragnathidae", "Araneidae", "Linyphiidae", "Lycosidae", 
                              "Mysmenidae"))+
  scale_fill_manual(values = colors)+
  labs(title = 'Araneomorphae Family Counts',
       subtitle = 'Years: 2022-2023',
       x = 'Family name')+
  ylab(bquote('Counts / 250'(ft ^2)))+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
  annotate('text', x = 1, y = 35, label = 'a', size = 10)+
  annotate('text', x = 7 , y = 5, label = 'b', size = 10)


group_sum

ggplot(group_sum, aes(x = name, y = value))+
  geom_bar(position = 'dodge', stat = 'identity', aes(fill = name), alpha = 0.7)+
  scale_fill_brewer(palette = 'Dark2')+
  scale_x_discrete(limits = c("spiders", "beetle_preds", "ensifera", "formicid", "lacewing", "hemip_pred"),
                   labels = c("Araneomorphae","Coleoptera", "Ensifera", "Formicidae", "Neuroptera", "Hemiptera"))+
  labs(title = 'Sweep Net Counts',
       subtitle = 'Years: 2022-2023',
       x = 'Family name')+
  ylab(bquote('Total counts / 250'(ft ^2)))+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
  annotate('text',  x = 1, y = 95, label = 'a', size = 10)+
  annotate('text', x = 6, y = 5, label = 'b', size = 10)
  



