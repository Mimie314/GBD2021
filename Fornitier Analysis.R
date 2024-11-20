

## frontier analysis################
library(dplyr)
library(data.table)
library(purrr)
library(tidyr)
library(ggplot2)
library(ggrepel)


case<- vroom::vroom("country.csv")

unique(case$age_name)
case<- subset(case,case$age_name=="All ages"|case$age_name=="Age-standardized" )

SDI <- read.csv('SDI_2021.csv',header = T)

## Merge SDI values 
frontier_data <- subset(case,case$age_name=='Age-standardized' & 
                          case$metric_name== 'Rate' &
                          case$sex_name =='Both' &
                          case$measure_name=='DALYs (Disability-Adjusted Life Years)')
frontier_data<-frontier_data[,c(2,7,8)]#####choose location year val

frontier_SDI <- left_join(frontier_data,SDI,by=c('location_name','year')) 

boostrap_DEA  <-  as.data.frame(matrix(nrow=0,ncol=6)) 
names(boostrap_DEA) <- c(names(frontier_SDI),'frontier','super')
boostrap_num <- 100 ### boostrap times
for(interation_number in 1:boostrap_num){
  boot_sample <- frontier_SDI[sample(1:nrow(frontier_SDI),nrow(frontier_SDI),replace = TRUE),] ## With replacement sampling, sample `nrow(frontier_SDI)` times.

  boot_sample <- boot_sample %>% arrange(SDI,desc(val))  ## We arrange the SDI in ascending order and the ASR in descending order.
  boot_sample$super <- NA
  boot_sample$super[1] <- 0 ## The "super" variable is used to determine whether it is super-efficient.
  for (i in 2:nrow(frontier_SDI)) {
    data <- boot_sample[-i,] ## Exclude each point and determine whether it is super-efficient.
    data$frontier <- NA ## Generate the frontier variable.
    min <- data$val[1] ## The ASR has been arranged from highest to lowest, thus the first point is the default frontier point.
    for (j in 1:(i-1)) { ## To save computational space, we only need to generate frontier values for the data prior to the excluded point to determine if this point is super-efficient.
      min <- ifelse(data$val[j]<min,data$val[j],min) 
      data$frontier[j] <- min} ## Determine if the value at that point is smaller than the frontier value; if so, it becomes the new frontier.
    boot_sample$super[i] <- ifelse(boot_sample[i,1]==boot_sample[i+1,1] & boot_sample[i,2]==boot_sample[i+1,2],0,  
                                   ifelse(boot_sample$val[i]<data$frontier[i-1],0,1)) ## To determine if an excluded point is super-efficient, we consider two scenarios: 
    ##First, if the excluded point has the same value as the point following it, it is by default not considered super-efficient. Second, if the excluded point is different from the subsequent point, we compare its ASR with the frontier value of the preceding point. 
    ##If the ASR is less than the frontier value, then the excluded point is deemed super-efficient.

  
    }
  ## After excluding super-efficient points, calculate the frontier value for each remaining point.
  boot_sample_exclude <- boot_sample[boot_sample$super==0,]
  min <- boot_sample_exclude$val[1]
  for (z in 1:nrow(boot_sample_exclude)){
    min <- ifelse(boot_sample_exclude$val[z]<min,boot_sample_exclude$val[z],min)
    boot_sample_exclude$frontier[z] <- min
  }
  boostrap_DEA <- rbind(boostrap_DEA,boot_sample_exclude)
  boostrap_DEA <-boostrap_DEA %>% 
    group_by(location_name,year,val,SDI) %>%
    summarize(frontier=mean(frontier))
  print(interation_number)
}

load(file='boostrap.Rdata')
boostrap_DEA <- boostrap_DEA %>% 
  mutate(eff_diff = val - frontier)

boostrap_DEA_1990 <- boostrap_DEA %>% 
  filter(year == 1990)

boostrap_DEA_2021 <- boostrap_DEA %>% 
  filter(year == 2021) 

boostrap_DEA_2021$trend <- ifelse(boostrap_DEA_2021$val > boostrap_DEA_1990$val, "Increase",
                                  "Decrease")
###Plot
plotA <- ggplot(boostrap_DEA, aes(SDI,val)) + geom_point(aes(color = year),size=1.8)+ 
  scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0),limits = c(0,1)) +
  scale_y_reverse() + 
  scale_color_gradient(low='#228B22',high='#90EE90') + 
  stat_smooth(data=boostrap_DEA, aes(SDI,frontier),colour='black',formula=y ~ poly(x, 1),
              stat = "smooth",method='loess',se=F,span=0.2,fullrange=T) + ## 
  theme_bw()
plotA

black <- boostrap_DEA_2021[order(boostrap_DEA_2021$eff_diff,decreasing = T),][1:15,]   
blue <- subset(boostrap_DEA_2021,SDI<0.5)[order(subset(boostrap_DEA_2021,SDI<0.5)$eff_diff),][1:5,]  
red <- subset(boostrap_DEA_2021,SDI>0.85)[order(subset(boostrap_DEA_2021,SDI>0.85)$eff_diff,decreasing = T),][1:5,]

plotB <- ggplot(boostrap_DEA_2021, aes(SDI,val)) + geom_point(aes(color = trend),size=2.5)+
  scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0),limits = c(0,1)) + 
  scale_y_reverse() + 
  stat_smooth(data=boostrap_DEA, aes(SDI,frontier),colour='black',formula=y ~ poly(x, 1),
              stat = "smooth",method='loess',se=F,span=0.2,fullrange=T)  + 
  geom_text_repel(data=black,colour='black',aes(SDI,val, label = location_name),size=2.5,fontface= 'bold',max.overlaps = 160) + 
  geom_text_repel(data=red,colour='darkred',aes(SDI,val, label = location_name),size=2.5,fontface= 'bold',max.overlaps = 160) + 
  geom_text_repel(data=blue,colour='darkblue',aes(SDI,val, label = location_name),size=2.5,fontface= 'bold',max.overlaps = 160)  + 
  theme_bw()
plotB
write.csv(black,"Black.csv")
write.csv(blue,"Blue.csv")
write.csv(red,"Red.csv")
write.csv(frontier_data,"Frontier data.csv")
