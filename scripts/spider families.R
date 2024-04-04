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
library(ggpubr)

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

long_mean <- fam_wide %>% 
  pivot_longer(
    cols = where(is.numeric)
  ) %>% 
  group_by( name) %>% 
  summarise(mean = mean(value),
            sd = sd(value), 
            n = n(), 
            se = sd/sqrt(n))

long_trt_mean <- fam_wide %>% 
  pivot_longer(
    cols = where(is.numeric)
  ) %>% 
  group_by(trt, name) %>% 
  summarise(mean = mean(value),
            sd = sd(value), 
            n = n(), 
            se = sd/sqrt(n))
# stats ####
fam_wide

# all groups 
aov_fam <- apply(fam_wide[,3:ncol(fam_wide)], 2, function(x) aov(x~trt, data = fam_wide))

tukey_fam <- sapply(aov_fam, function(x) TukeyHSD(x, 'trt', ordered = TRUE))
# $tetrgnathidae.trt
# diff        lwr      upr      p adj
# ctl-dep 0.8181818 -0.6434458 2.279809 0.36385896
# aug-dep 1.8181818  0.3565542 3.279809 0.01229692
# aug-ctl 1.0000000 -0.4616276 2.461628 0.22677642
# 
# $thomisidae.trt
# diff        lwr      upr      p adj
# ctl-dep 2.272727 -1.2433406 5.788795 0.26395993
# aug-dep 4.272727  0.7566594 7.788795 0.01462698
# aug-ctl 2.000000 -1.5160678 5.516068 0.35250848
# 
# $salticidae.trt
# diff        lwr      upr      p adj
# ctl-dep 1.3636364 -0.3819731 3.109246 0.14893350
# aug-dep 2.0909091  0.3452996 3.836519 0.01623046
# aug-ctl 0.7272727 -1.0183368 2.472882 0.56588453
# 
# $oxyopidae.trt
# diff        lwr      upr       p adj
# ctl-dep 2.1818182  0.1180070 4.245629 0.036549169
# aug-dep 2.9090909  0.8452798 4.972902 0.004371917
# aug-ctl 0.7272727 -1.3365384 2.791084 0.663804707


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
  save_as_docx(path = 'T.test results for augmentation spider family counts.docx')
  



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

ggplot(filter(long_mean, name != 'unknown'), aes(name, mean))+
  geom_bar(position = 'dodge', stat = 'identity', aes(fill = name), alpha = 0.7)+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  scale_fill_brewer(palette = "Dark2")+
  scale_x_discrete(limits = c("thomisidae",  "oxyopidae", "salticidae","tetrgnathidae", "araneidae", "linyphiidae", "lycosidae", 
                              "philodromidae"),
                   labels = c("Thomisidae", "Oxyopidae","Salticidae" , "Tetrgnathidae", "Araneidae", "Linyphiidae", "Lycosidae", 
                              "Philodromidae"))+
  labs(title = 'Average Araneomorphae Family Populations',
       subtitle = 'Year: Augmentation 2023',
       y = 'Average Counts',
       x = 'Family name')+
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
  annotate('text', y = 6.5, x = 1, label = 'a', size = 10)+
  annotate('text', y = 6.5, x = 2, label = 'ac', size = 10)+
  annotate('text', y = 6.5, x = 3, label = 'abc', size = 10)+
  annotate('text', y = 6.5, x = 4, label = 'abc', size = 10)+
  annotate('text', y = 6.5, x = 5, label = 'abc', size = 10)+
  annotate('text', y = 6.5, x = 6, label = 'abc', size = 10)+
  annotate('text', y = 6.5, x = 7, label = 'bc', size = 10)+
  annotate('text', y = 6.5, x = 8, label = 'bd', size = 10)

# plot for sig families by treatment

thom <- ggplot(filter(long_trt_mean, name == 'thomisidae'), aes(trt, mean))+
  geom_bar(position = 'dodge', stat = 'identity', aes(fill = trt), alpha = 0.7)+
  scale_x_discrete(limits = c('dep', 'ctl', 'aug'),
                     labels = c("Depletion","Control","Augmentation"))+
  scale_fill_manual(values = c("#1B9E77","#7570B3","#D95F02"))+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  labs(title = 'Thomisidae',
       #subtitle = 'Year: Augmentation 2023',
       x = 'Treatment')+
  ylab(bquote('Average Counts / 2000'(ft^2)))+
  theme(legend.position = 'none',
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size=26),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
  annotate('text', x = 1, y = 10.5, label = 'a', size = 10)+
  annotate('text', x = 2, y = 10.5, label = 'ab', size = 10)+
  annotate('text', x = 3, y = 10.5, label = 'b', size = 10)

oxy <- ggplot(filter(long_trt_mean, name == 'oxyopidae'), aes(trt, mean))+
  geom_bar(position = 'dodge', stat = 'identity', aes(fill = trt), alpha = 0.7)+
  scale_x_discrete(limits = c('dep', 'ctl', 'aug'),
                   labels = c("Depletion","Control","Augmentation"))+
  scale_fill_manual(values = c("#1B9E77","#7570B3","#D95F02"))+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  labs(title = 'Oxyopidae',
       #subtitle = 'Year: Augmentation 2023',
       x = 'Treatment')+
  ylab(bquote('Average Counts / 2000'(ft^2)))+
  theme(legend.position = 'none',
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 26),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
  annotate('text', x = 1, y = 4.5, label = 'a', size = 10)+
  annotate('text', x = 2, y = 4.5, label = 'b', size = 10)+
  annotate('text', x = 3, y = 4.5, label = 'b', size = 10)

salt <- ggplot(filter(long_trt_mean, name == 'salticidae'), aes(trt, mean))+
  geom_bar(position = 'dodge', stat = 'identity', aes(fill = trt), alpha = 0.7)+
  scale_x_discrete(limits = c('dep', 'ctl', 'aug'),
                   labels = c("Depletion","Control","Augmentation"))+
  scale_fill_manual(values = c("#1B9E77","#7570B3","#D95F02"))+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  labs(title = 'Salticidae',
       #subtitle = 'Year: Augmentation 2023',
       x = 'Treatment')+
  ylab(bquote('Average Counts / 2000'(ft^2)))+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
  annotate('text', x = 1, y = 3.8, label = 'a', size = 10)+
  annotate('text', x = 2, y = 3.8, label = 'ab', size = 10)+
  annotate('text', x = 3, y = 3.8, label = 'b', size = 10)

tet <- ggplot(filter(long_trt_mean, name == 'tetrgnathidae'), aes(trt, mean))+
  geom_bar(position = 'dodge', stat = 'identity', aes(fill = trt), alpha = 0.7)+
  scale_x_discrete(limits = c('dep', 'ctl', 'aug'),
                   labels = c("Depletion","Control","Augmentation"))+
  scale_fill_manual(values = c("#1B9E77","#7570B3","#D95F02"))+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  labs(title = 'Tetragnathidae',
       #subtitle = 'Year: Augmentation 2023',
       x = 'Treatment')+
  ylab(bquote('Average Counts / 2000'(ft^2)))+
  theme(legend.position = 'none', 
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
  annotate('text', x = 1, y = 2.9, label = 'a', size = 10)+
  annotate('text', x = 2, y = 2.9, label = 'ab', size = 10)+
  annotate('text', x = 3, y = 2.9, label = 'b', size = 10)

figure <- ggarrange(thom, oxy, salt, tet)
annotate_figure(figure, 
                # top = text_grob("Average Family Counts x Treatment", size = 28),
                bottom = text_grob("Treatment", size = 32),
                left = text_grob("Average Counts / 61 m^2", size = 32, rot = 90))























