# augmentation exp

library(tidyverse)
beans <- as_tibble(soybean_damage)
beans

# remove .jpg
beans <- beans %>% 
  mutate(leaf_plot = gsub(".jpg", "", leaf_plot))

# time to do math 
# I just want a few rows to mess with first
beans_test <- beans[1:10,]
beans_test

timing <- c("total", "damaged")

# test of difference ###
test <- beans_test %>% 
  group_by(leaf_plot) %>% 
  mutate(timing = timing) %>% 
  select(leaf_plot, total_area, timing)
test

#get the difference of each two columns
test_new <- test %>% 
  group_by(leaf_plot) %>% 
  mutate(dmg_diff = total_area - lag(total_area)) %>% 
  na.omit() %>% 
  select(leaf_plot, dmg_diff) %>% 
  mutate(dmg_diff = abs(dmg_diff)) 
test_new

#check to make sure these values are correct 
diff(test$total_area[1:2])
diff(test$total_area[3:4])

# full data set ###
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

# test summing plots  
test_sum <- test_new 
test_sum <- test_sum %>% 
  mutate(leaf_plot = gsub("_[0-9]*$","", leaf_plot)) #removes all trailing numbers starting at the underscore
test_here <- aggregate(dmg_diff ~ leaf_plot , data = test_sum, FUN = sum)

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
beans_split <- beans_sum
beans_split$trt <- rownames(beans_split)
beans_split$trt <- str_split_fixed(beans_split$trt, "_",2)

beans_split[c('plot', 'trt')] <- str_split_fixed(beans_split$leaf_plot, '_',2)
beans_final <- beans_split %>% 
 select(plot, trt, dmg_diff) %>% 
  rename(damage = dmg_diff) %>% 
  arrange(plot) %>% 
  mutate(plot = as.numeric(plot)) %>% 
  mutate(trt = as.factor(trt))
as_tibble(beans_final)

unique(beans_final$trt)

# damage is not normal 
kruskal.test(damage ~ trt, data = beans_final)

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


##

?shapiro.test
shapiro.test(beans_final$damage)

# beans_final$damage <- log10(beans_final$damage)

qqnorm(beans_final$damage)

hist(beans_final$damage)

?aov
model <- aov(damage ~ trt, beans_final)
summary(model)
shapiro.test(model$residuals)


##

#now I need to go back and get the total area
# the first number is total, so I want to every other value, starting with 1 
beans_total_test <- beans_test %>% 
  select(leaf_plot, total_area) 
# going to extract every odd row number
rows <- nrow(beans_total_test)
odd_rows <- seq_len(rows) %% 2
beans_test_odd <- beans_total_test[odd_rows == 1, ]

# all data odd extraction









