# Assessing the populations of sample arthropods by trt and a spacial analysis
# omitting 1200s 
# packages ####
library(tidyverse)
library(vegan)

# cleaning of the data ####
counts<- as_tibble(augmentation_counts)

# merging pentatomid into hermiptera 
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
  select(-pentatomid, -hemiptera)


# having a look at the spread of spiders and hemiptera
# it would make sense that there would be higher counts in augmentation for both
group_by(counts_clean, trt) %>% 
  summarise(
    count = n(), 
    mean = mean(Araneomorphae),
    sd = sd(Araneomorphae), 
    median = median(Araneomorphae), 
      IQR = IQR(Araneomorphae)
  )

group_by(counts_clean, trt) %>% 
  summarise(
    count = n(), 
    mean = mean(Hemiptera),
    sd = sd(Hemiptera), 
    median = median(Hemiptera), 
    IQR = IQR(Hemiptera)
  )

counts_clean$trt <- as.factor(counts_clean$trt)
counts_clean$site <- as.factor(counts_clean$site)

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

#going to attempt the perm disp 
# seeing whether differences are due to location and/ or dispersion
trt.res.betadisper <- betadisper(d = dist, group = counts_clean$trt, type = 'centroid')

trt.res.betadisper$distances

anova(trt.res.betadisper)
TukeyHSD(trt.res.betadisper)

boxplot(trt.res.betadisper)
plot(trt.res.betadisper)

# IDK how to do a post hoc on this? 
# Do I even need one, or is my NMDS sufficient for spatial? 
# Should I add site? 
# Would be a glm if adding site 
#trt is significant here, need to investigate this further
# library(devtools)
# install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
# library(pairwiseAdonis)
# ?pairwise.adonis
# #post-hoc
# post_test <- pairwise.adonis2(dist ~ trt, data = counts_clean)

# GLM ####
# overdispersed?
mean(counts_clean$Araneomorphae)
var(counts_clean$Araneomorphae)
# yes

mean(counts_clean$Coleoptera)
var(counts_clean$Coleoptera)
# yes 

mean(counts_clean$Hemiptera)
var(counts_clean$Hemiptera)
# yes

mean(counts_clean$Caelifera)
var(counts_clean$Caelifera)
# yes 

mean(counts_clean$Lepidoptera)
var(counts_clean$Lepidoptera)
# yes

library(lme4)
# want model of each population by trt with site as effect 
spider <- glmer.nb(Araneomorphae ~ trt + (1|site), data = counts_clean)
summary(spider)
hist(residuals(spider))
plot(x = counts_clean$trt, y = counts_clean$Araneomorphae)

ggplot(counts_clean, aes(x=trt,y=Araneomorphae, fill=site))+
  geom_bar(aes(x=trt,y=Araneomorphae, fill=site),stat = "identity",position="dodge")+
  labs(x= "Treatment", y="Abundance", title = "Araneomorphae: Abundance x treatment*site")+
  scale_fill_brewer(palette = "Dark2", labels=c("Site 1", "Site 2", "Site 3", "Site 4", "Site 5", "Site 6"),name = "Site")+
  scale_x_discrete(limits = c("1", "2", "3"),
                   labels = c("Control", "Depletion", "Augmentation"))



hemip <- glmer.nb(Hemiptera ~ trt + (1|site), data = counts_clean)
summary(hemip)
hist(residuals(hemip))
plot(x = counts_clean$trt, y = counts_clean$Hemiptera)

cael <- glmer.nb(Caelifera ~ trt + (1|site), data = counts_clean)
summary(cael)
hist(residuals(cael))
plot(x = counts_clean$trt, y = counts_clean$Caelifera)

col <- glmer.nb(Coleoptera ~ trt + (1|site), data = counts_clean)
summary(col)
hist(residuals(col))
plot(x = counts_clean$trt, y = counts_clean$Coleoptera)

lep <- glmer.nb(Lepidoptera ~ trt + (1|site), data = counts_clean)
summary(lep)
hist(residuals(lep))
plot(x = counts_clean$trt, y = counts_clean$Lepidoptera)



# NMDS ####

# metaMDS must be numeric
# stress level?
# 2 dimensional 
ord <- metaMDS(functional_groups, k = 2)
ord$stress # stress = 0.14
stressplot(ord)

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
