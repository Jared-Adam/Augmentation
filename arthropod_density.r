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
  select(-pentatomid, -hemiptera)


# having a look at the spread of spiders and hemiptera
# it would make sense that there would be higher counts in augmentation for both
group_by(counts_clean, trt) %>% 
  summarise(
    count = n(), 
    mean = mean(spiders),
    sd = sd(spiders), 
    median = median(spiders), 
      IQR = IQR(spiders)
  )

group_by(counts_clean, trt) %>% 
  summarise(
    count = n(), 
    mean = mean(hemipteran),
    sd = sd(hemipteran), 
    median = median(hemipteran), 
    IQR = IQR(hemipteran)
  )

counts$trt <- as.factor(counts$trt)

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
permanova <- adonis2(dist ~ trt, permutations = 999, method = "bray", data = counts_clean)

permanova
#trt is significant here, need to investigate this further


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
  geom_polygon(data = hull.data, (aes(x = NMDS1, y = NMDS2, group = trt, fill = trt)), alpha = 0.3)+
  scale_fill_manual(values = c("black", "red", "blue"))+
  geom_segment(data = functional_scores, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), 
                                             arrow = arrow(length = unit(0.25, "cm")),
               color = "grey10", lwd = 0.3)+
  geom_text_repel(data = functional_scores, aes(x = NMDS1, y = NMDS2, label = species), cex = 5, direction = "both",
                  segment.size = 0.25)+
  annotate(geom = "text", x=.4, y=.5, label ="Stress: 0.1380172", size = 5)+
  coord_equal()+
  theme_minimal()+
  labs(fill="Treatment")+
  labs(title = "Arthropod abundance by treatment")+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_blank(), # remove x-axis labels
        axis.title.y = element_blank(), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
