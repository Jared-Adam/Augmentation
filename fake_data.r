# fake data set for hypothesis testing 
fake_data <- data.frame(trt = c(1,1,1,1,2,2,2,2,3,3,3,3),
                        spider = c(3,4,5,6,1,2,3,4,7,8,9,10),
                        pest = c(4,5,6,7,2,3,4,5,1,2,3,4))

library(ggplot2)
fake_data$trt <- as.factor(fake_data$trt)
ggplot(fake_data)+
  geom_col(aes(spider, pest, fill = trt), stat = "identity", position = "dodge")
  
