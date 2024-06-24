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
library(flextable)
library(lmtest)
library(MASS)

# data ####
sweeps <- beans_sweeps
colnames(sweeps)
unique(sweeps$Date)
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
         year = format(date , "%Y"))%>% 
  rename(plot = Plot) %>%
  mutate(plot = case_when(plot == '308' ~ '302',
                          .default = as.factor(plot))) %>% 
  mutate(trt = case_when(plot %in% c('101','203','304','401','503') ~ '1',
                         plot %in% c('102', '201','303','402','502') ~ '3',
                         plot %in% c('103','204','302','403','501') ~ '2',
                         plot %in% c('104','202','301','404','504') ~ '4')) 

#total 
now %>% 
  pivot_longer(cols = where(is.numeric),
               values_to = 'count') %>% 
  group_by(trt) %>% 
  summarise(sum = sum(count))
# trt     sum
# <chr> <int>
# 1 1       178
# 2 2       193
# 3 3       134
# 4 4       105


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
  relocate(date, year, plot, trt,  spiders, beetle_preds, other_beetle) %>% 
  mutate_at(vars(1:4), as.factor)

# I am attempting to make then values constant since I took double the samples in 2023
g_22 <- groups %>% 
  filter(year == '2022') 
g_23 <- groups %>% 
  filter(year == '2023') %>% 
  mutate_if(is.numeric, ~ . * 0.5)

groups <- rbind(g_22, g_23)
# new_group_sum <- new_group %>%  
#   summarise(spiders = sum(spiders),
#             beetle_preds = sum(beetle_preds), 
#             formicid = sum(Formicidae), 
#             lacewing = sum(Hemerobiidae),
#             hemip_pred = sum(pred_hem),
#             ensifera = sum(ensif)) %>% 
#   pivot_longer(
#     cols = where(is.numeric))
# 

groups %>% 
  group_by(date) %>% 
  rowwise() %>% 
  mutate(totals = sum(c_across(spiders:ensif)))

#
##
###

# some stuff for a plot later on 



group_sum <- groups %>%  
  summarise(spiders = sum(spiders),
            beetle_preds = sum(beetle_preds), 
            formicid = sum(Formicidae), 
            lacewing = sum(Hemerobiidae),
            hemip_pred = sum(pred_hem),
            ensifera = sum(ensif)) %>% 
  pivot_longer(
    cols = where(is.numeric))

group_mean <- groups %>%  
  summarise(spiders = mean(spiders),
            beetle_preds = mean(beetle_preds), 
            formicid = mean(Formicidae), 
            lacewing = mean(Hemerobiidae),
            hemip_pred = mean(pred_hem),
            ensifera = mean(ensif)) %>% 
  pivot_longer(
    cols = where(is.numeric))

group_mean_plot <- groups %>% 
  pivot_longer(
    where(is.numeric)
  ) %>% 
  group_by(name) %>% 
  summarise(mean = mean(value),
            sd = sd(value) ,
            n = n(), 
            se = sd/sqrt(n)) %>% 
  filter(name != 'Hemerobiidae', name != 'Syrphidae', name != 'Thrips', name != 'cael', name != 'other_beetle',name !='other_hem')

###
##
#


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

###
##
#




# spiders only ####
# accounting for the number of samples in 2023 

now_22 <- now %>% 
  filter(year == '2022') 
now_23 <- now %>% 
  ungroup() %>% 
  filter(year == '2023')  %>% 
  mutate_if(is.numeric, ~ . *0.5)

now <- rbind(now_22, now_23)



spider_long_sum <- now %>% 
  ungroup() %>% 
  dplyr::select(date, year, plot, trt, Linyphiidae, Thomisidae, Lycosidae, Mysemindae, Mysmenidae,Salticidae,Tetragnathidae,
           Araneidae,Liniphiidae) %>% 
  mutate(Linyphiidae = Liniphiidae + Linyphiidae,
         Mysmenidae = Mysemindae + Mysmenidae) %>%
  dplyr::select(-Liniphiidae, -Mysemindae) %>% 
  group_by(date, plot, trt) %>% 
  mutate(plot = as.factor(plot)) %>% 
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
  dplyr::select(date, year, plot, Linyphiidae, Thomisidae, Lycosidae, Mysemindae, Mysmenidae,Salticidae,Tetragnathidae,
                Araneidae,Liniphiidae) %>% 
  mutate(Linyphiidae = Liniphiidae + Linyphiidae,
         Mysmenidae = Mysemindae + Mysmenidae) %>%
  dplyr::select(-Liniphiidae, -Mysemindae) %>% 
   mutate(trt = case_when(plot %in% c('101','203','304','401','503') ~ '1',
                          plot %in% c('102', '201','303','402','502') ~ '3',
                          plot %in% c('103','204','302','403','501') ~ '2',
                          plot %in% c('104','202','301','404','504') ~ '4')) %>%
  relocate(date, year, plot, trt) %>% 
  mutate_at(vars(1:4), as.factor) %>%  
  mutate(plot = as.factor(plot)) %>% 
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
### plot df 

spider_plot <- now %>% 
  ungroup() %>% 
  dplyr::select(date, year, plot, Linyphiidae, Thomisidae, Lycosidae, Mysemindae, Mysmenidae,Salticidae,Tetragnathidae,
                Araneidae,Liniphiidae) %>% 
  mutate(Linyphiidae = Liniphiidae + Linyphiidae,
         Mysmenidae = Mysemindae + Mysmenidae) %>%
  dplyr::select(-Liniphiidae, -Mysemindae) %>% 
  mutate(trt = case_when(plot %in% c('101','203','304','401','503') ~ '1',
                         plot %in% c('102', '201','303','402','502') ~ '3',
                         plot %in% c('103','204','302','403','501') ~ '2',
                         plot %in% c('104','202','301','404','504') ~ '4')) %>%
  relocate(date, year, plot, trt) %>% 
  mutate_at(vars(1:4), as.factor) %>%  
  mutate(plot = as.factor(plot)) %>% 
  pivot_longer(
    cols = where(is.numeric)
  ) %>% 
  group_by(name) %>% 
  summarise(mean = mean(value), 
            sd = sd(value),
            n = n(),
            se = sd/sqrt(n))

###



spider_wide <- now %>% 
  ungroup() %>% 
  dplyr::select(date, year, plot, Linyphiidae, Thomisidae, Lycosidae, Mysemindae, Mysmenidae,Salticidae,Tetragnathidae,
                Araneidae,Liniphiidae) %>% 
  mutate(Linyphiidae = Liniphiidae + Linyphiidae,
         Mysmenidae = Mysemindae + Mysmenidae) %>%
  dplyr::select(-Liniphiidae, -Mysemindae) %>% 
  rename(plot = plot) %>% 
  mutate(plot = case_when(plot == '308' ~ '304',
         .default = as.factor(plot))) %>% 
  mutate(trt = case_when(plot %in% c('101','203','304','401','503') ~ '1',
                          plot %in% c('102', '201','303','402','502') ~ '3',
                          plot %in% c('103','204','302','403','501') ~ '2',
                          plot %in% c('104','202','301','404','504') ~ '4')) %>% 
  relocate(date, year, plot, trt) %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  arrange(plot) %>% 
  print(n = Inf)



# need to append empty rows to time points to ensure that all dates have n = n 

test_df <- spider_wide %>% 
  group_by(date, year, plot) %>% 
  summarise(
    Linyphiidae = sum(Linyphiidae),
    Thomisidae = sum(Thomisidae),
    Lycosidae = sum(Lycosidae),
    Mysmenidae = sum(Mysmenidae),
    Salticidae = sum(Salticidae),
    Tetragnathidae = sum(Tetragnathidae),
    Araneidae = sum(Araneidae)
  )


# creating the dfs for the missing data 

t1 <- test_df %>% 
  filter(date == '2022-07-01')

t1_append <- data.frame(date = '2022-07-01',
           year = '2022',
           plot = c('101','102','103','201','202','203','204','301','302','303','401','402','403','404',
                    '501','502','503')
           )

t1_fixed <-
  rbind(t1, t1_append) %>% 
  replace(is.na(.),0) %>% 
  arrange(plot)

t2 <- test_df %>% 
  filter(date =='2023-06-28')
unique(t2$plot)

t2_append <- data.frame(date = '2023-06-28',
                        year = '2023',
                        plot = c('104','204'))

t2_fixed <- rbind(t2, t2_append)%>% 
  replace(is.na(.),0) %>% 
  arrange(plot) 

# removing then adding for plot

spider_data <- test_df %>%   
  filter(date != '2023-05-04', date != '2023-05-22', date != '2023-06-28', date != '2022-07-01') 

spider_data_plot <- rbind(spider_data, t1_fixed, t2_fixed) %>% 
  mutate(spiders = Linyphiidae + Thomisidae + Mysmenidae + Salticidae + Tetragnathidae + Araneidae) %>% 
  mutate(block = case_when(plot %in% c('101', '102', '103', '104') ~ '1',
                           plot %in% c('201', '202', '203', '204') ~ '2',
                           plot %in% c('301', '302', '303', '304') ~ '3',
                           plot %in% c('401', '402', '403', '404') ~ '4',
                           plot %in% c('501', '502', '503', '504') ~ '5'),
         block = as.factor(block)) %>%  
  group_by(date) %>% 
  summarise(mean = mean(spiders),
            sd = sd(spiders),
            n = n(), 
            se = sd/sqrt(n))
  
# removing then adding for the models
  
spider_mo <- test_df %>%   
  filter(date != '2023-05-04', date != '2023-05-22', date != '2023-06-28', date != '2022-07-01') 

spider_model <- rbind(spider_mo, t1_fixed, t2_fixed) %>% 
  mutate(spiders = Linyphiidae + Thomisidae + Mysmenidae + Salticidae + Tetragnathidae + Araneidae) %>% 
  mutate(block = case_when(plot %in% c('101', '102', '103', '104') ~ '1',
                           plot %in% c('201', '202', '203', '204') ~ '2',
                           plot %in% c('301', '302', '303', '304') ~ '3',
                           plot %in% c('401', '402', '403', '404') ~ '4',
                           plot %in% c('501', '502', '503', '504') ~ '5')) %>% 
   mutate(trt = case_when(plot %in% c('101','203','304','401','503') ~ '1',
                       plot %in% c('102', '201','303','402','502') ~ '3',
                       plot %in% c('103','204','302','403','501') ~ '2',
                       plot %in% c('104','202','301','404','504') ~ '4'),
       trt = as.factor(trt)) %>% 
  relocate(date, year, plot, block, trt) %>% 
  mutate_at(vars(1:5), as.factor) 



# stats ####
# individual populations
# all groups ~ trt
groups

aov_group <- apply(groups[,5:ncol(groups)], 2, function(x) aov(x ~ trt, data = groups))
tukey_group <- sapply(aov_group, function(x) TukeyHSD(x , 'trt', ordered = TRUE))
# this is nice 

aov_group_df <- do.call(rbind, lapply(aov_group, broom::tidy))
aov_group_df %>% 
  print(n = Inf)
# this df is hard to look at
tukey_group_df <- as.data.frame(do.call(rbind, Map(cbind, Name = names(tukey_group), tukey_group)))

# spider models by trt
groups_m <- groups %>% 
  mutate(block = case_when(
    str_detect(plot, '10') ~ '1',
    str_detect(plot, '20') ~ '2',
    str_detect(plot, '30') ~ '3',
    str_detect(plot, '40') ~ '4',
    str_detect(plot, '50') ~ '5'
  )) %>% 
  mutate(block = as.factor(block)) %>% 
  mutate(pests = other_beetle + Thrips + other_hem + cael + ensif +Hesperiidae) %>% 
  group_by(plot, block, year, trt) %>% 
  summarise(average_spider = mean(spiders),
            average_pest = mean(pests)) %>% 
  print(n =Inf)
  
nb <- glmer.nb(average_spider ~ trt*year +
                 (1|block/plot), data = groups_m)

p <- glmer(average_spider ~ trt*year +
             (1|block/plot), data = groups_m, family = poisson)

g <- lmer(average_spider ~ trt*year +
            (1|block/plot), data = groups_m)

anova(nb, p, g)

m0 <- glmer.nb(average_spider ~
                 (1|block), data = groups_m)

m1 <- glmer.nb(average_spider ~ trt +
           (1|block), data = groups_m)

m2 <- glmer.nb(average_spider ~ trt+year +
           (1|block), data = groups_m)

m3 <- glmer.nb(average_spider ~ trt*year +
           (1|block), data = groups_m)

anova(m0, m1, m2, m3)
# npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# m0    3 83.360 88.427 -38.680   77.360                          
# m1    6 89.226 99.359 -38.613   77.226  0.1346  3     0.9874    
# m2    7 65.930 77.752 -25.965   51.930 25.2958  1  4.918e-07 ***
# m3   10 71.854 88.743 -25.927   51.854  0.0756  3     0.9946 
summary(m3)
hist(residuals(m3))
plot(m3)

cld(emmeans(m3, ~year), Letters = letters)
# year emmean    SE  df asymp.LCL asymp.UCL .group
# 2023 -3.109 1.109 Inf     -5.28    -0.936  a    
# 2022  0.137 0.208 Inf     -0.27     0.544   b  

# pest models by trt
nb <- glmer.nb(average_pest ~ trt*year +
                 (1|block/plot), data = groups_m)

p <- glmer(average_pest ~ trt*year +
             (1|block/plot), data = groups_m, family = poisson)

g <- lmer(average_pest ~ trt*year +
            (1|block/plot), data = groups_m)

anova(nb, p, g)

p0 <- glmer.nb(average_pest ~
                 (1|block/plot), data = groups_m)

p1 <- glmer.nb(average_pest ~ trt +
                 (1|block/plot), data = groups_m)

p2 <- glmer.nb(average_pest ~ trt+year +
                 (1|block/plot), data = groups_m)

p3 <- glmer.nb(average_pest ~ trt*year +
                 (1|block/plot), data = groups_m)

anova(p0, p1, p2, p3)
# npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# p0    4 49.722 56.477 -20.861   41.722                          
# p1    7 55.321 67.143 -20.660   41.321  0.4008  3  0.9400679    
# p2    8 44.458 57.970 -14.229   28.458 12.8624  1  0.0003352 ***
# p3   11 50.458 69.036 -14.229   28.458  0.0000  3  1.0000000 

cld(emmeans(m3, ~year, type = 'response'), Letters = letters)
# year response     SE  df asymp.LCL asymp.UCL .group
# 2023   0.0447 0.0495 Inf   0.00508     0.392  a    
# 2022   1.1466 0.2382 Inf   0.76309     1.723   b  

###

# long groups 

group_sum

# kruskal.test(value ~ name, data = group_sum)
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
####
###
##
#

perm_df <- spider_model %>% 
  dplyr::select(-spiders) %>% 
  group_by(date, year, plot, trt) %>% 
  summarise(
    Linyphiidae = sum(Linyphiidae),
    Thomisidae = sum(Thomisidae),
    Lycosidae = sum(Lycosidae),
    Mysmenidae = sum(Mysmenidae),
    Salticidae = sum(Salticidae),
    Tetragnathidae = sum(Tetragnathidae),
    Araneidae = sum(Araneidae)
  )
unique(perm_df$date)
str(perm_df)

spider_no_zero <- perm_df[rowSums(perm_df[5:11])>0,]
spider_fams <- spider_no_zero[5:11]

spider_dist <- vegdist(spider_fams, 'bray')
adonis2(spider_dist ~ year + date + trt, permutations = factorial(10), method = 'bray', data = spider_no_zero)

# Df SumOfSqs      R2      F   Pr(>F)   
# year      1   1.4938 0.09369 4.9068 0.000916 ***
#   date      2   1.8295 0.11474 3.0046 0.001020 ** 
#   trt       3   1.0521 0.06599 1.1520 0.305328    
# Residual 38  11.5690 0.72558                    
# Total    44  15.9445 1.00000   

# which year had the most?
# which trt had the most?

unique(spider_model$year)
# using these values for model selection and for the paper values
spider_model %>% 
  summarise(var = var(spiders), 
            mean = mean(spiders),
            sd = sd(spiders),
            n = n(),
            se = sd/sqrt(n)
            )
# gosh this is close. Going to stick with Poisson

m <- glm.nb(spiders ~ date,
          data = spider_model)

p <- glm(spiders ~ date, 
           data = spider_model,
           family = poisson)

lrtest(m, p)

summary(p)
hist(residuals(p))
cld(emmeans(p, ~date), Letters = letters)

# date       emmean    SE  df asymp.LCL asymp.UCL .group
# 2022-07-01 -2.996 1.000 Inf    -4.956    -1.036  a    
# 2023-06-28 -1.492 0.471 Inf    -2.416    -0.568  a    
# 2023-07-26 -0.357 0.267 Inf    -0.880     0.167  a    
# 2022-08-12  0.668 0.160 Inf     0.354     0.982   b  
library(broom)
p_df <- as.data.frame(tidy(p))
f_p <- flextable(p_df)

f_p <- theme_zebra(f_p)

autofit(f_p) %>% 
  save_as_docx(path = 'spider_date.docx')


# old: before I changed the values 
# date       emmean    SE  df asymp.LCL asymp.UCL .group
# 2022-07-01 -2.996 0.707 Inf   -4.3816    -1.610  a    
# 2023-06-28 -0.799 0.236 Inf   -1.2605    -0.337   b   
# 2023-07-26  0.336 0.189 Inf   -0.0339     0.707    c  
# 2022-08-12  0.668 0.160 Inf    0.3540     0.982    c  


####
###
##
#


spider_wide
# spiders ~ trt

aov_fam <- apply(spider_wide[,5:ncol(spider_wide)], 2, function(x) aov(x ~ trt, data = spider_wide))
tukey_fam <- sapply(aov_fam, function(x) TukeyHSD(x , 'trt', ordered = TRUE))
# this is nice 
# $Tetragnathidae.trt
# diff          lwr        upr      p adj
# 3-2 0.006024096 -0.074371489 0.08641968 0.99742333
# 4-2 0.043975904 -0.043786720 0.13173853 0.56701693
# 1-2 0.086833046  0.002787656 0.17087844 0.03983607 *
# 4-3 0.037951807 -0.049810816 0.12571443 0.67911899
# 1-3 0.080808950 -0.003236440 0.16485434 0.06446528 #
# 1-4 0.042857143 -0.048260669 0.13397495 0.61760736

plot(tukey_fam$Tetragnathidae.trt)

aov_fam_df <- do.call(rbind, lapply(aov_fam, broom::tidy))
# this df is hard to look at
tukey_fam_df <- as.data.frame(do.call(rbind, Map(cbind, Name = names(tukey_fam), tukey_fam)))

###

# long spider

spider_plot_sum

# kruskal.test(value ~ name, data = spider_plot_sum)
dunn.test::dunn.test(spider_plot_sum$value, spider_plot_sum$name, altp = TRUE, list = TRUE,'none')
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
spider_plot_sum %>% 
  group_by(name) %>% 
  summarise(mean = mean(value), 
            sd = sd(value),
            n = n(),
            se = sd/sqrt(n))

display.brewer.pal(n = 8, name = 'Dark2')
brewer.pal(n = 8, name = 'Dark2')

colors <- c(Thomisidae = "#666666", Salticidae="#E6AB02", Tetragnathidae="#A6761D", Araneidae="#1B9E77", 
            Linyphiidae="#D95F02", Lycosidae="#7570B3", Mysmenidae="#E7298A")
ggplot(spider_plot, aes(x = name, y = mean))+
  geom_bar(position = 'dodge', stat = 'identity', aes(fill = name), alpha = 0.7)+
  scale_x_discrete(limits = c("Thomisidae", "Salticidae", "Tetragnathidae", "Araneidae", "Linyphiidae", "Lycosidae", 
                              "Mysmenidae"))+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  scale_fill_manual(values = colors)+
  labs(title = 'Average Araneomorphae Family Counts',
       subtitle = 'Years: 2022-2023',
       x = 'Family name')+
  ylab(bquote('Average counts / 76'(m ^2)))+
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
  annotate('text', x = 1, y = .13, label = 'a', size = 10)+
  annotate('text', x = 2 , y = .13, label = 'ab', size = 10)+
  annotate('text', x = 3 , y = .13, label = 'ab', size = 10)+
  annotate('text', x = 4 , y = .13, label = 'ab', size = 10)+
  annotate('text', x = 5 , y = .13, label = 'ab', size = 10)+
  annotate('text', x = 6 , y = .13, label = 'ab', size = 10)+
  annotate('text', x = 7 , y = .13, label = 'b', size = 10)


# individual spider families for the marginal plot in supplementary 
# lycosidae and tetrgnathidae by trt

tet_plot <- spider_wide %>% 
  dplyr::select(trt, Tetragnathidae) %>% 
  group_by(trt) %>% 
  summarise(
    sum = sum(Tetragnathidae),
    mean = mean(Tetragnathidae),
    sd = sd(Tetragnathidae),
    n = n(),
    se = sd/sqrt(n)
  )

ggplot(tet_plot, aes(x = trt, y = mean))+
  geom_bar(position = 'dodge', stat = 'identity',  alpha = 0.7)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),
                color = 'black', alpha = 1, width = 0.2, linewidth = 1.5)




group_sum
group_mean
group_mean_plot

ggplot(group_mean_plot, aes(x = name, y = mean))+
  geom_bar(position = 'dodge', stat = 'identity', aes(fill = name), alpha = 0.7)+
  scale_fill_brewer(palette = 'Dark2')+
  scale_x_discrete(limits = c("spiders", "beetle_preds", "ensif", "Formicidae", "Hesperiidae", "pred_hem"),
                   labels = c("Araneomorphae","Coleoptera", "Ensifera", "Formicidae", "Neuroptera", "Hemiptera"))+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  labs(title = 'Average Sweep Net Counts x Group',
       subtitle = 'Years: 2022-2023',
       x = 'Group name')+
  ylab(bquote('Average counts / 76'(m ^2)))+
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
  annotate('text',  x = 1, y = 0.28, label = 'a', size = 10)+
  annotate('text', x = 2, y = 0.28, label = 'ab', size = 10)+
  annotate('text', x = 3, y = 0.28, label = 'ab', size = 10)+
  annotate('text', x = 4, y = 0.28, label = 'ab', size = 10)+
  annotate('text', x = 5, y = 0.28, label = 'ab', size = 10)+
  annotate('text', x = 6, y = 0.28, label = 'b', size = 10)
  

ggplot(spider_data_plot, aes(factor(date), mean))+
  geom_bar(stat = 'identity', position = 'dodge', aes(fill = date), alpha = 0.7)+
  scale_fill_brewer(palette = 'Dark2')+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  labs(title = 'Mean Araneomoprhae x Sampling Date',
       subtitle = 'Years: 2022-2023',
       x = 'Sampling Date', 
       y = 'Mean population')+
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
  annotate('text',  x = 1, y = 2.4, label = 'a', size = 10)+
  annotate('text', x = 2, y = 2.4, label = 'b', size = 10)+
  annotate('text', x = 3, y = 2.4, label = 'a', size = 10)+
  annotate('text', x = 4, y = 2.4, label = 'a', size = 10)

