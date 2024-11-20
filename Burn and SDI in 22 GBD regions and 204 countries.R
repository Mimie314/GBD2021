library(reshape)
library(ggplot2)
library(ggrepel)
library(reshape2)


EC <- read.csv('EC_region.csv',header = T)  
order_SDI <- read.csv('order_SDI.csv',header = F)##The names of 22 GBD regions
SDI <- read.csv('1990-2021-SDI.csv',header = T)##SDI values in regions worldwide
###Correlation analysis between SDI and ASR in 22 GBD regions
## Using the reshape packet, the SDI data format is visually converted from wide data to long data format
SDI <- melt(SDI,id='Location')
SDI$variable <- as.numeric(gsub('\\X',replacement = '', SDI$variable))
names(SDI) <- c('location','year','SDI')


### Standard incidence data for EC from 1990 to 2021 were obtained
EC <- subset(EC, EC$age=='Age-standardized' & 
                 EC$metric== 'Rate' &
                 EC$measure=='Incidence')
EC <- EC[,c(2,7,8)]
names(EC)[3] <- 'ASR'

### 合并SDI与ASR数据
EC_ASR_SDI <- merge(EC,SDI,by=c('location','year'))

EC_ASR_SDI$location <- factor(EC_ASR_SDI$location, 
                              levels=order_SDI$V1, 
                              ordered=TRUE) ## location Legends are arranged in our order

##The above variables are ASR and SDI. The color and shape of the graph can be adjusted according to different regions. 
##At the same time, a fitting curve is drawn with all the data
ggplot(EC_ASR_SDI, aes(SDI,ASR)) + geom_point(aes(color = location, shape= location))+
  scale_shape_manual(values = 1:22) + 
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) 





#### Correlation analysis between SDI and national ASR
EC <- read.csv('EC_nation.csv',header = T)  
SDI <- read.csv('1990-2021-SDI.csv',header = T)

## Using the reshape packet, the SDI data format is converted from a wide data format to a long data format
SDI <- melt(SDI,id='Location')
SDI$variable <- as.numeric(gsub('\\X',replacement = '', SDI$variable))
names(SDI) <- c('location','year','SDI')
SDI <- SDI[SDI$year== 2021,] 

### ASR data for EC 2021 were obtained
EC <- subset(EC, EC$age=='Age-standardized' & 
               EC$metric== 'Rate' &
               EC$measure=='Prevalence' &
               EC$year=='2021')
EC <- EC[,c(2,7,8)]
names(EC)[3] <- 'ASR'

### Adjust SDI to be consistent with the location name in the EC
EC$location[EC$location == 'United States of America'] = 'USA'
EC$location[EC$location == 'Russian Federation'] = 'Russia'
EC$location[EC$location == 'United Kingdom'] = 'UK'
EC$location[EC$location == 'Congo'] = 'Republic of Congo'
EC$location[EC$location == "Iran (Islamic Republic of)"] = 'Iran'
EC$location[EC$location == "Democratic People's Republic of Korea"] = 'North Korea'
EC$location[EC$location == "Taiwan (Province of China)"] = 'Taiwan'
EC$location[EC$location == "Republic of Korea"] = 'South Korea'
EC$location[EC$location == "United Republic of Tanzania"] = 'Tanzania'
EC$location[EC$location == "C?te d'Ivoire"] = 'Saint Helena'
EC$location[EC$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
EC$location[EC$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
EC$location[EC$location == "Czechia"] = 'Czech Republic'
EC$location[EC$location == "Republic of Moldova"] = 'Moldova'
EC$location[EC$location == "Viet Nam"] = 'Vietnam'
EC$location[EC$location == "Lao People's Democratic Republic"] = 'Laos'
EC$location[EC$location == "Syrian Arab Republic"] = 'Syria'
EC$location[EC$location == "North Macedonia"] = 'Macedonia'
EC$location[EC$location == "Micronesia (Federated States of)"] = 'Micronesia'
EC$location[EC$location == "Macedonia"] = 'North Macedonia'
EC$location[EC$location == "Trinidad and Tobago"] = 'Trinidad'
EC <- rbind(EC,EC[EC$location == "Trinidad",])
EC$location[EC$location == "Trinidad"] = 'Tobago'
EC$location[EC$location == "Cabo Verde"] = 'Cape Verde'
EC$location[EC$location == "United States Virgin Islands"] = 'Virgin Islands'
EC$location[EC$location == "Antigua and Barbuda"] = 'Antigu'
EC <- rbind(EC,EC[EC$location == "Antigu",])
EC$location[EC$location == "Antigu"] = 'Barbuda'
EC$location[EC$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
EC <- rbind(EC,EC[EC$location == "Saint Kitts",])
EC$location[EC$location == "Saint Kitts"] = 'Nevis'
EC$EC[EC$location == "Côte d'Ivoire"] = 'Ivory Coast'
EC$location[EC$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
EC <- rbind(EC,EC[EC$location == "Saint Vincent",])
EC$location[EC$location == "Saint Vincent"] = 'Grenadines'
EC$location[EC$location == "Eswatini"] = 'Swaziland'
EC$location[EC$location == "Brunei Darussalam"] = 'Brunei'
EC$location[EC$location == "Türkiye"] = 'Turkey'

EC_ASR_SDI_2021 <- merge(EC,SDI,by=c('location','year'))
write.csv(EC,"ASPR_204.csv")
ggplot(EC_ASR_SDI_2021, aes(SDI,ASR,label=location)) + 
  geom_point(aes(color = location)) + 
  geom_text_repel(aes(color = location),size=2,fontface= 'bold',max.overlaps = 160) +
  geom_smooth(colour='black',stat = "smooth",method='loess',se=F,span=0.5) +
  theme(legend.position="none")
