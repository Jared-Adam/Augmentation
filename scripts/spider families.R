# Jared Adam 
# spider families from aug sweeps 

# packages ####
library(tidyverse)
library(vegan)
library(lme4)
library(emmeans)
library(performance)
library(multcomp)
library(ggpmisc)
library(lmtest)
library(flextable)

# data ####
fam <- spider_families

# wrangling ####
unique(fam$trt)
unique(fam$plot)

fam_wide <- fam %>% 
  dplyr::select(-`10/4/2023`, -a_j) %>% 
  pivot_wider(names_from = family,
              values_from = family, 
              values_fn= list(family= length)) %>% 
  replace(is.na(.),0) %>% 
  mutate(plot = case_when(plot == '101' ~ '100',
                         .default = as.character(plot)))

long_trt_sum <- fam_wide %>% 
  group_by(trt) %>% 
  summarise(Tetrgnathidae = sum(tetrgnathidae),
            Thomisidae = sum(thomisidae),
            Salticidae = sum(salticidae),
            Oxyopidae = sum(oxyopidae), 
            Araneidae = sum(araneidae),
            Lycosidae = sum(lycosidae),
            Philodromidae = sum(philodromidae),
            Linyphiidae = sum(linyphiidae)) %>% 
  pivot_longer(
    cols = where(is.numeric)
  ) %>%   
  mutate(trt = as.factor(trt)) %>% 
  arrange(trt, value) %>% 
  print(n = 15)

long_sum <- fam_wide%>% 
  summarise(Tetrgnathidae = sum(tetrgnathidae),
            Thomisidae = sum(thomisidae),
            Salticidae = sum(salticidae),
            Oxyopidae = sum(oxyopidae), 
            Araneidae = sum(araneidae),
            Lycosidae = sum(lycosidae),
            Philodromidae = sum(philodromidae),
            Linyphiidae = sum(linyphiidae)) %>% 
  pivot_longer(
    cols = where(is.numeric)
  ) %>% 
  mutate(name = as.factor(name)) %>% 
  arrange(value) %>% 
  print(n = 15)

# stats ####
fam_wide

# all groups 
aov_fam <- apply(fam_wide[,3:ncol(fam_wide)], 2, function(x) aov(x~trt, data = fam_wide))

tukey_fam <- sapply(aov_fam, function(x) TukeyHSD(x, 'trt', ordered = TRUE))

aov_fam_df <- do.call(rbind, lapply(aov_fam, broom::tidy))

tuksey_fam_df <- as.data.frame(do.call(rbind, Map(cbind, Name = names(tukey_fam), tukey_fam)))


###

kruskal.test(value ~ name, data = long_sum)
dt <- as.data.frame(dunn.test::dunn.test(long_sum$value, long_sum$name))
dtf <- flextable(dt) %>% 
  set_header_labels(dtf,
                         values = list(
                           chi2 = 'X^2',
                           Z = 'z',
                           P = 'p value',
                           P.adjusted = 'Adjusted p value',
                           comparisons = 'Family comparisons'
                         )) 
dtf <- theme_zebra(dtf)
dtf <- add_header_lines(dtf, 
                   values = "T.test results for spider family counts")
autofit(dtf) %>% 
  save_as_docx(path = 'T.test results for spider family counts.docx')
  



# plots ####

# long trt plot 
ggplot(long_trt_sum, aes(name, value))+
  geom_bar(position = 'dodge', stat = 'identity', aes(fill = name))+
  scale_fill_brewer(palette = 'Dark2')+
  facet_wrap(~trt)+
  scale_x_discrete(limits = c("Thomisidae", "Salticidae", "Oxyopidae", "Tetrgnathidae", "Araneidae", "Linyphiidae", "Lycosidae", 
                              "Philodromidae"))+
  theme(legend.position = 'none')

ggplot(long_sum, aes(name, value))+
  geom_bar(position = 'dodge', stat = 'identity', aes(fill = name), alpha = 0.7)+
  scale_fill_brewer(palette = "Dark2")+
  scale_x_discrete(limits = c("Thomisidae", "Salticidae", "Oxyopidae", "Tetrgnathidae", "Araneidae", "Linyphiidae", "Lycosidae", 
                              "Philodromidae"))+
  labs(title = 'Araneomorphae Family Populations',
       y = 'Counts',
       x = 'Family name')+
  scale_y_continuous(breaks = seq(0,180, by = 25))+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))
