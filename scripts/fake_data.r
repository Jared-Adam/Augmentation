# fake data set for hypothesis testing 
fake_data <- data.frame(trt = c(1,1,1,1,2,2,2,2,3,3,3,3),
                        spider = c(3,4,5,6,1,2,3,4,7,8,9,10),
                        pest = c(4,5,6,7,2,3,4,5,1,2,3,4))

library(ggplot2)
fake_data$trt <- as.factor(fake_data$trt)
ggplot(fake_data)+
  geom_col(aes(spider, pest, fill = trt), stat = "identity", position = "dodge")
  
?rnorm
bugs <- data.frame(
  Treatment = c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3),
  spiders = c(25,26,25,25,33,25,11,13,15,10,5,6,32,35,40,44,38,41),
  pests =   c(24,25,27,24,25,25,22,29,27,25,24,8,15,7,16,8,12,11)
)


bugs_two <- data.frame(
  Treatment = c(1,1,1,1,1,1),
  spiders = c(30,35,40,45,46,48),
  pests =   c(14,12,10,8,6,4)
)
bugs_two$Treatment <- as.factor(bugs_two$Treatment)

ggplot(bugs_two, aes(spiders, pests))+
  geom_point(size = 5,color = "#1B9E77")+
  geom_smooth(method = lm, se = FALSE, fullrange = FALSE, color = "black", lwd = 1)+ 
  labs(title = "Hypothesis plot : Total pest populations X total spider populations",
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

library(tidyverse)
as_tibble(bugs)
bugs$Treatment <- as.factor(bugs$Treatment)

 ggplot(bugs, aes(spiders, pests, color = Treatment, shape = Treatment))+
   geom_point(size = 5)+
   geom_smooth(method = lm, se = FALSE, fullrange = FALSE)+ 
   scale_color_manual(labels = c("Control","Removal","Addition"),values = c("#7570B3","#D95F02","#1B9E77"))+
   scale_shape_manual(labels = c("Control","Removal","Addition"), values = c(19,17,15))+
   labs(title = "Hypothesis plot : Total pest populations X total spider populations",
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
# spiders
# 1 : 25,26,25,27,28
# 2 : 12,13,15,10,5
# 3 : 35,40,44,38,41
 
# pest 
# 1: 22,27,21,25,28,23
# 2: 19,22,29,27,25,24
# 3: 8,15,7,16,8,12,11
# beans ####
 
 
 beans <- data.frame(
   plot = c(100,100,100,200,200,200,300,300,300,400,400,400),
   trt = c(1,2,3,1,2,3,1,2,3,1,2,3),
   damage = c(4400,6000,3800,4500,6100,3000,5000,5400,3400,4100,5200,3100)
 )
 as_tibble(beans)
 # ctl : 4000,3800,3900,4100
 # dep : 6000,6100,6200,5800
 # aug : 3200,300,3400,3100
 
 
 beans$trt <- as.factor(beans$trt)
 ggplot(beans, aes(trt, damage, fill = trt))+
   geom_boxplot(alpha = 0.6)+
   scale_fill_manual(values = c("#7570B3","#D95F02","#1B9E77"))+
   stat_boxplot(geom = 'errorbar',
                width = 0.2,
                size = 0.75)+
   theme_light()+
   theme(axis.text = element_text(size = 18), 
         axis.title = element_text(size = 22),
         plot.title = element_text(size = 24))+
   labs(title = 'Hypothesis plot: Total Damage by Treatment')+
   theme(legend.position = "none")+
   scale_x_discrete(name = "Treatment",
                    limits = c("1", "2", "3"),
                    labels = c("Control","Fewer Spiders","More Spiders"))+
   scale_y_continuous(name = "Total Damage (mm^2)")
 
 