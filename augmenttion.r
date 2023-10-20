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

# test of difference ####
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

#now I need to go back and get the total area