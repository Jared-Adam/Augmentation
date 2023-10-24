# augmentation exp

library(tidyverse)
beans <- as_tibble(soybean_damage)
beans

# remove .jpg
beans <- beans %>% 
  mutate(leaf_plot = gsub(".jpg", "", leaf_plot))

# test code #### 
# time to do math 
# I just want a few rows to mess with first
beans_test <- beans[1:10,] #making a subset of the first 10 rows to practice 
beans_test

timing <- c("total", "damaged") #new column to identify damage timing

# test of difference ###
test <- beans_test %>% 
  group_by(leaf_plot) %>% 
  mutate(timing = timing) %>% # adding new timing column
  select(leaf_plot, total_area, timing) # selecting the columns I want to work with
test

### omit anything above 10000
#get the difference of each two columns
?lag # compute lagged or leading values
test_new <- test %>% 
  group_by(leaf_plot) %>% 
  mutate(dmg_diff = total_area - lag(total_area)) %>% #subtract the first total_area value by the next unique/new total_area value. If lag() is left blank, it will subtract the next value. E.g., 1-2, 2-3, 3-4, 4-5, etc. 
  na.omit() %>% 
  select(leaf_plot, dmg_diff) %>% 
  mutate(dmg_diff = abs(dmg_diff)) # these values came out to be negative, so I absolute value them 
test_new

#check to make sure these values are correct 
diff(test$total_area[1:2]) #checking work to ensure lag worked with diff()
diff(test$total_area[3:4])

# test summing plots  
test_sum <- test_new 
test_sum <- test_sum %>% 
  mutate(leaf_plot = gsub("_[0-9]*$","", leaf_plot)) #removes all trailing numbers starting at the underscore
                          # _ = starting point, [0-9]* = numeric values, $ = trailing numeric values
                          # ONLY works if the all trailing values are numeric
test_here <- aggregate(dmg_diff ~ leaf_plot , data = test_sum, FUN = sum) # add all values with same name

# full data set ####
beans$leaf_plot <- as.factor(beans$leaf_plot)
# now to do this to the whole data set (beans)
beans_new <- beans %>% 
  group_by(leaf_plot) %>% 
  mutate(dmg_diff = total_area - lag(total_area)) %>% 
  na.omit() %>% 
  select(leaf_plot, dmg_diff) %>% 
  mutate(dmg_diff = abs(dmg_diff)) %>% 
  mutate(across(c('dmg_diff'),round,2))
  
beans_new 

# difference by plot 
beans_sum <- beans_new %>% 
  mutate(leaf_plot = gsub("_[0-9]*$", "", leaf_plot))
beans_sum
beans_sum <- aggregate(dmg_diff ~ leaf_plot, data = beans_sum, FUN = sum)
beans_sum$leaf_plot[beans_sum$leaf_plot=='Aug_1100'] <-'1100_Aug'
beans_sum$leaf_plot[beans_sum$leaf_plot=='Aug_1200'] <-'1200_Aug'
beans_sum$leaf_plot[beans_sum$leaf_plot=='Ctl_1200'] <-'1200_Ctl'
beans_sum$leaf_plot[beans_sum$leaf_plot=='Dep_1200'] <-'1200_Dep'

# split the names to have a new column of rep and trt 
# beans_split <- beans_sum
# beans_split$trt <- rownames(beans_split)
# beans_split$trt <- str_split_fixed(beans_split$trt, "_",2)

beans_split[c('plot', 'trt')] <- str_split_fixed(beans_split$leaf_plot, '_',2)
beans_final <- beans_split %>% 
 select(plot, trt, dmg_diff) %>% 
  rename(damage = dmg_diff) %>% 
  arrange(plot) %>% 
  mutate(plot = as.numeric(plot)) %>% 
  mutate(trt = as.factor(trt)) %>%
  filter(damage < 15000)
as_tibble(beans_final)

unique(beans_final$trt)

# normality 
?shapiro.test
shapiro.test(beans_final$damage)

# beans_final$damage <- log10(beans_final$damage)

qqnorm(beans_final$damage)

hist(beans_final$damage)

##
?aov
model <- aov(damage ~ trt, beans_final)
summary(model)
shapiro.test(model$residuals)
qqnorm(model$residuals)
##


##
# if damage is not normal 
kruskal.test(damage ~ trt, data = beans_final)
##

# vis of data with summary stats 
group_by(beans_final, trt) %>% 
  summarise(
    count = n(), 
    mean = mean(damage),
    sd = sd(damage), 
    median = median(damage), 
      IQR = IQR(damage)
  )
library(ggpubr)
ggboxplot(beans_final, x = 'trt', y = 'damage')

ggplot(beans_final, aes(trt, damage, fill = trt))+
  geom_boxplot()+
  stat_boxplot(geom = 'errorbar',
               width = 0.2)+
  theme_light()+
  labs(title = 'Total Damage by Treatment')+
  theme(legend.position = "none")+
  scale_x_discrete(name = "Treatment")+
  scale_y_continuous(name = "Total Damage (mm)")






