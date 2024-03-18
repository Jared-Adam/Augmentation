# Assessing the populations of sample arthropods by trt and a spacial analysis
# omitting 1200s 
# packages ####
library(tidyverse)
library(vegan)
library(lme4)
library(emmeans)
library(performance)
library(multcomp)
library(ggpmisc)
library(lmtest)


# cleaning of the data ####
counts<- as_tibble(augmentation_counts)

# merging pentatomid into hermiptera 
# I also want to add a new column for total pest
# this will be useful in the glmer phase 

counts_clean <- counts %>% 
  mutate(trt = case_when(trt == "aug" ~ 3,
              trt == "dep" ~ 2,
              trt == "ctl" ~ 1)) %>% 
  mutate(hemipteran = hemiptera + pentatomid) %>% 
  rename(Hemiptera = hemipteran, 
         Coleoptera = coleoptera, 
         Caelifera = caelifera, 
         Lepidoptera = lepidoptera, 
         Araneomorphae = spiders) %>% 
  select(-pentatomid, -hemiptera) %>% 
  mutate(site = case_when(plot == 100 ~ 1,
                           plot == 200 ~ 1,
                           plot == 300 ~ 2,
                           plot == 400 ~ 2,
                           plot == 500 ~ 3,
                           plot == 500 ~ 3,
                           plot == 600 ~ 4,
                           plot == 700 ~ 4,
                           plot == 800 ~ 5,
                           plot == 900 ~ 5,
                           plot == 1000 ~ 6,
                           plot == 1100 ~ 6)) %>% 
  rowwise() %>% 
  mutate(total_pest = sum(Coleoptera, Hemiptera, Caelifera, Lepidoptera, na.rm = T))

counts_clean$trt <- as.factor(counts_clean$trt)
counts_clean$site <- as.factor(counts_clean$site)

fig_df <- counts_clean %>% 
  rename(Treatment = trt) 

# %>% 
#   group_by(Treatment) %>% 
#   mutate( mean = mean(Araneomorphae),
#           var = var(Araneomorphae), # avg of sqaured differenes from the mean
#           sd = sd(Araneomorphae))



# having a look at the spread of spiders and hemiptera
# it would make sense that there would be higher counts in augmentation for both
group_by(counts_clean, trt) %>% 
  summarise(
    count = n(), 
    mean = mean(Araneomorphae),
    var = var(Araneomorphae), # avg of sqaured differenes from the mean
    sd = sd(Araneomorphae), 
    median = median(Araneomorphae), 
      IQR = IQR(Araneomorphae)
  )

group_by(counts_clean, trt) %>% 
  summarise(
    count = n(), 
    mean = mean(Hemiptera),
    var = var(Hemiptera),
    sd = sd(Hemiptera), 
    median = median(Hemiptera), 
    IQR = IQR(Hemiptera)
  )

group_by(counts_clean, trt) %>% 
  summarise(
    count = n(), 
    mean = mean(total_pest),
    var = var(total_pest),
    sd = sd(total_pest), 
    median = median(total_pest), 
    IQR = IQR(total_pest)
  )
# Permanova ####
# need a data matrix for just the groups of interest
# vegdist needs numeric values
functional_groups <- counts_clean[3:7]

# calculating the distance between groups
dist <- vegdist(functional_groups, "bray")

# permanova with seed set
# standard = 999
# dist object from above
# running the distance values by treatment
?adonis2
permanova_trt <- adonis2(dist ~ trt, permutations = 999, method = "bray", data = counts_clean)
permanova_trt

permanova_site <- adonis2(dist ~ site, permutations = 999, method = "bray", data = counts_clean )
permanova_site

permanovas_both <- adonis2(dist ~ trt*site, permutations = 999, method = "bray", data = counts_clean)
permanovas_both

####
###
##
#

# distribution ####

# how is the spread? 
plot(counts_clean$Araneomorphae)
hist(counts_clean$Araneomorphae)
# not normal 

plot(counts_clean$total_pest)
hist(counts_clean$total_pest)
#not normal

# overdispersed? 
# var > mean 
mean(counts_clean$Araneomorphae)
var(counts_clean$Araneomorphae)
# yes

mean(counts_clean$total_pest)
var(counts_clean$total_pest)


# The fixed effects are the coefficients (intercept, slope) as we usually think about the. (pops, trt, etc.)
# The random effects are the variances of the intercepts or slopes across groups (site/ location/ block)
# nested (russian doll, e.g., (1|location/block/plot))
# crossed (not related, e.g., (1|location) + (1|date)

# intercept = predicted value of the dependent varaible when the indep values are 0
# do you need to care? 
   # depends! 

# spider model ####
# want model of each population by trt with site as random effect 

s0 <- glmer.nb(Araneomorphae ~ 
                 (1|site), data = counts_clean)

s1 <- glmer.nb(Araneomorphae ~ trt +
                 (1|site), data = counts_clean)

anova(s0, s1)

summary(s1)
hist(residuals(s1))

sp_emm <- emmeans(s1, ~trt)
cld(sp_emm, Letters = letters)




ggplot(fig_df, aes(x= Treatment, y = Araneomorphae, fill = Treatment))+
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#7570B3","#D95F02","#1B9E77"))+
  #geom_errorbar(aes(x=fig_df, ymin = mean-sd, ymax=mean + sd))+
  scale_x_discrete(labels = c("Control","Depletion","Augmentation"))+
  labs(title = "Spider Population x Treatment",
       x = "Treatment",
       y = "Spider populations")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        plot.title = element_text(size = 26),
        legend.key.height = unit(1, 'cm'),
        legend.key.size = unit(4, 'cm'),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20))+
  annotate('text', x = 1, y = 75, label = 'b', size = 10)+
  annotate('text', x = 2, y = 75, label = 'a', size = 10)+
  annotate('text', x = 3, y = 75, label = 'b', size = 10)


# pest model####
p0 <- glmer.nb(total_pest ~ 
                         (1|site), data = counts_clean)

p1 <- glmer.nb(total_pest ~ trt + 
                         (1|site), data = counts_clean)
anova(p0, p1)

summary(p1)
hist(residuals(p1))

p_emm <- emmeans(p1, ~trt)
cld(p_emm, Letters = letters)



ggplot(fig_df, aes(x= Treatment, y = total_pest, fill = Treatment))+
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#7570B3","#D95F02","#1B9E77"))+
  #geom_errorbar(aes(x=fig_df, ymin = mean-sd, ymax=mean + sd))+
  scale_x_discrete(labels = c("Control","Depletion","Augmentation"))+
  labs(title = "Total Pest Population x Treatment",
       x = "Treatment",
       y = "Total Pest Population")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        plot.title = element_text(size = 26),
        legend.key.height = unit(1, 'cm'),
        legend.key.size = unit(4, 'cm'),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20))+
  annotate('text', x = 1, y = 75, label = 'b', size = 10)+
  annotate('text', x = 2, y = 75, label = 'a', size = 10)+
  annotate('text', x = 3, y = 75, label = 'b', size = 10)


# spider ~ pest model ####

# model of pest by pred
# dropping trt because I can see that trt influences populations 
plot(total_pest ~ Araneomorphae, counts_clean)

r1 <- glm(total_pest ~ Araneomorphae, data = counts_clean)

r.nb <- glm.nb(total_pest ~ Araneomorphae, data = counts_clean)

lrtest(r1, r.nb)

summary(r.nb)
hist(residuals(r.nb))
coef(summary(r.nb))
rX2 = (73.617-33.774)

r_emm <- emmeans(r.nb, ~Araneomorphae)
pwpm(r_emm)
cld(r_emm, Letters= letters)


ggplot(fig_df, aes(Araneomorphae, total_pest, color = Treatment, shape = Treatment))+
  geom_point(size = 5)+
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE)+
  stat_poly_eq()+
  scale_color_manual(labels = c("Control","Depletion","Augmentation"),values = c("#7570B3","#D95F02","#1B9E77"))+
  scale_shape_manual(labels = c("Control","Depletion","Augmentation"), values = c(19,17,15))+
  labs(title = "Total pest populations by total spider populations",
       x = "Spider populations",
       y = "Total pest population")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        plot.title = element_text(size = 26),
        legend.key.height = unit(1, 'cm'),
        legend.key.size = unit(4, 'cm'),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20))

# I like this one 
ggplot(fig_df, aes(Araneomorphae, total_pest))+
  geom_point(size = 5, aes(color = Treatment))+
  scale_color_manual(labels = c("Control","Depletion","Augmentation"),values = c("#7570B3","#D95F02","#1B9E77"))+
  geom_smooth(method = lm, se = TRUE, fullrange = TRUE, color = 'black')+
  stat_poly_eq(size = 10)+
  labs(title = "Total Pest Population x Total Spider Population",
       subtitle = 'Year: 2023',
       x = "Spider population",
       y = "Total pest population")+
  theme(legend.position = "bottom",
        legend.key.size = unit(.50, 'cm'),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
  annotate('text', x = 5, y = 192, label = 'p value < 0.0001', size = 10)


ggplot(filter(fig_df, Treatment == '1'), aes(Araneomorphae, total_pest))+
  geom_point(size = 5, aes(color = Treatment))+
  geom_smooth(method = lm, se = TRUE)+
  stat_poly_eq()+
  labs(title = "Ctl: Total pest populations by total spider populations",
       x = "Spider populations",
       y = "Total pest population")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        plot.title = element_text(size = 26),
        legend.key.height = unit(1, 'cm'),
        legend.key.size = unit(4, 'cm'),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20))

ggplot(filter(fig_df, Treatment == '2'), aes(Araneomorphae, total_pest))+
  geom_point(size = 5, aes(color = Treatment))+
  geom_smooth(method = lm, se = TRUE)+
  stat_poly_eq()+
  labs(title = "Dep: Total pest populations by total spider populations",
       x = "Spider populations",
       y = "Total pest population")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        plot.title = element_text(size = 26),
        legend.key.height = unit(1, 'cm'),
        legend.key.size = unit(4, 'cm'),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20))

ggplot(filter(fig_df, Treatment == '3'), aes(Araneomorphae, total_pest))+
  geom_point(size = 5, aes(color = Treatment))+
  geom_smooth(method = lm, se = TRUE)+
  stat_poly_eq()+
  labs(title = "Aug: Total pest populations by total spider populations",
       x = "Spider populations",
       y = "Total pest population")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        plot.title = element_text(size = 26),
        legend.key.height = unit(1, 'cm'),
        legend.key.size = unit(4, 'cm'),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20))

# hemiptera pest ~ spiders ####
# models for each group ~ spider
hem_model <- glm.nb(Hemiptera ~ Araneomorphae, data = counts_clean)
summary(hem_model) # hemipterans increase with more spiders
hist(residuals(hem_model))

ggplot(fig_df, aes(Araneomorphae, Hemiptera, color = Treatment, shape = Treatment))+
  geom_point(size = 5)+
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE)+ 
  scale_color_manual(labels = c("Control","Depletion","Augmentation"),values = c("#7570B3","#D95F02","#1B9E77"))+
  scale_shape_manual(labels = c("Control","Depletion","Augmentation"), values = c(19,17,15))+
  labs(title = "Hemipteran pest populations by total spider populations",
       x = "Spider populations",
       y = "Hemipteran population")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        plot.title = element_text(size = 26),
        legend.key.height = unit(1, 'cm'),
        legend.key.size = unit(4, 'cm'),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20))


# cael pest ~ spiders ####
ceal_model <- glm.nb(Caelifera ~ Araneomorphae, data = counts_clean)

summary(ceal_model) # cael increase with more spiders
hist(residuals(ceal_model))

ggplot(fig_df, aes(Araneomorphae, Caelifera, color = Treatment, shape = Treatment))+
  geom_point(size = 5)+
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE)+ 
  scale_color_manual(labels = c("Control","Depletion","Augmentation"),values = c("#7570B3","#D95F02","#1B9E77"))+
  scale_shape_manual(labels = c("Control","Depletion","Augmentation"), values = c(19,17,15))+
  labs(title = "Caelifera pest populations by total spider populations",
       x = "Spider populations",
       y = "Caelifera population")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        plot.title = element_text(size = 26),
        legend.key.height = unit(1, 'cm'),
        legend.key.size = unit(4, 'cm'),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20))


# coleoptera pest ~ spiders ####

col_model <- glm.nb(Coleoptera ~ Araneomorphae, data = counts_clean)
summary(col_model) # nothing 
hist(residuals(col_model))

col_plot <- ggplot(fig_df, aes(Araneomorphae, Coleoptera, color = Treatment, shape = Treatment))+
  geom_point(size = 5)+
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE)+ 
  scale_color_manual(labels = c("Control","Depletion","Augmentation"),values = c("#7570B3","#D95F02","#1B9E77"))+
  scale_shape_manual(labels = c("Control","Depletion","Augmentation"), values = c(19,17,15))+
  labs(title = "Coleoptera pest populations by total spider populations",
       x = "Spider populations",
       y = "Coleoptera population")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        plot.title = element_text(size = 26),
        legend.key.height = unit(1, 'cm'),
        legend.key.size = unit(4, 'cm'),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20))


# lep pests ~ spiders ####
lep_model <- glm.nb(Lepidoptera ~ Araneomorphae, data = counts_clean)
summary(lep_model) # nothing 
hist(residuals(lep_model))

ggplot(fig_df, aes(Araneomorphae, Lepidoptera, color = Treatment, shape = Treatment))+
  geom_point(size = 5)+
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE)+ 
  scale_color_manual(labels = c("Control","Depletion","Augmentation"),values = c("#7570B3","#D95F02","#1B9E77"))+
  scale_shape_manual(labels = c("Control","Depletion","Augmentation"), values = c(19,17,15))+
  labs(title = "Lepidoptera pest populations by total spider populations",
       x = "Spider populations",
       y = "Lepidoptera population")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        plot.title = element_text(size = 26),
        legend.key.height = unit(1, 'cm'),
        legend.key.size = unit(4, 'cm'),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20))


# pests ~ spiders: WO hemiptera ####
w_o_hem <- counts_clean %>% 
  dplyr::select(-total_pest, -Hemiptera) %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(Coleoptera:Lepidoptera))) %>% 
  relocate(plot, trt, site)

wo1 <- glm.nb(total ~ Araneomorphae, data = w_o_hem)
summary(wo1)
hist(residuals(wo1))

ggplot(w_o_hem, aes(Araneomorphae, total))+
  geom_point(size = 5, aes(color = trt))+
  stat_poly_eq(size = 12)+
  geom_smooth(method = lm, se = TRUE, fullrange = TRUE, color = 'black')+ 
  scale_color_manual(labels = c("Control","Depletion","Augmentation"),values = c("#7570B3","#D95F02","#1B9E77"))+
  scale_shape_manual(labels = c("Control","Depletion","Augmentation"), values = c(19,17,15))+
  labs(title = "All except hemipteran pest populations by total spider populations",
       x = "Spider populations",
       y = "pests population")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        plot.title = element_text(size = 26),
        legend.key.height = unit(1, 'cm'),
        legend.key.size = unit(4, 'cm'),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20))



# NMDS ####

# metaMDS must be numeric
# stress level?
# 2 dimensional 
ord <- metaMDS(functional_groups, k = 2)
reord$stress # stress = 0.14
stressplot(ord)
# screeplot(ord)
# ?screeplot

# 3D
# ord_2 <- metaMDS(functional_groups, k = 3)
# ord_2$stress # stress = 0.06
# stressplot(ord_2)
# plot(ord_2, type = "t", choices = 2:3) 
# devtools::install_github("AckerDWM/gg3D")
# library(gg3D)


# need to get site scores for ordination
# I think I want display  = "sites"
?scores
scrs <- scores(ord, display = "sites")
# adding my scores from metaMDS to their associated trts 
scrs <- cbind(as.data.frame(scrs), trt = counts_clean$trt)
scrs <- cbind(as.data.frame(scrs), site = counts_clean$site)



# i want to add functional group to this df 
# "species" = averaged site scores
# as_tibble here gets rid of the name and replaces the groups with numbers != what I want
functional_scores <- as.data.frame(scores(ord, "species"))
functional_scores$species <- rownames(functional_scores)

# going to chull the objects to get trts into their own shapes
aug <- scrs[scrs$trt == "3",][chull(scrs[scrs$trt == "3",c("NMDS1", "NMDS2")]),]
dep <- scrs[scrs$trt == "2",][chull(scrs[scrs$trt == "2",c("NMDS1", "NMDS2")]),]
ctl <- scrs[scrs$trt == "1",][chull(scrs[scrs$trt == "1",c("NMDS1", "NMDS2")]),]

hull.data <- rbind(aug, dep, ctl)
as_tibble(hull.data) #trt = factor
hull.data$trt <- as.factor(hull.data$trt)
library(ggrepel)

ggplot()+
  geom_polygon(data = hull.data, (aes(x = NMDS1, y = NMDS2, group = trt, fill = trt)), alpha = 0.5)+
  scale_fill_manual(name = "Treatment", labels = c('Control', 'Depletion', 'Agumentation'), values = c("#7570B3","#D95F02","#1B9E77"))+
  geom_segment(data = functional_scores, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
                                             arrow = arrow(length = unit(0.25, "cm")),
               color = "grey10", lwd = 0.3)+
  geom_text_repel(data = functional_scores, aes(x = NMDS1, y = NMDS2, label = species), cex = 8, direction = "both",
                  segment.size = 0.25)+
  annotate("label", x = 0, y=.5, label ="Stress: 0.1380172", size = 6)+
  coord_equal()+
  theme_bw()+
  labs(title = "Arthropod abundance by treatment")+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_blank(), # remove x-axis labels
        axis.title.y = element_blank(), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        plot.title = element_text(size = 22))+
  theme(
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.key.size = unit(1.5, 'cm')
  )
