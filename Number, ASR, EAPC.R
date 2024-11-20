library(dplyr)
library(ggplot2)
############################# Calculation of 1990,2021 Number
##Read file
EC <- read.csv('EC_region.csv',header = T)  
EC_1990 <- subset(EC,  EC$year==1990 &
                 EC$age=='All ages' &
                 EC$metric== 'Number' &
                 EC$measure=='Incidence')
##extract Number and Uncertain Interval
EC_1990 <- EC_1990[, c(2,8,9,10)]  
EC_1990$val < round(EC_1990$val,1)
EC_1990$lower <- round(EC_1990$lower,1) 
EC_1990$upper <- round(EC_1990$upper,1)
EC_1990$Num_1990 <- paste(EC_1990$lower,EC_1990$upper,sep = '-')
EC_1990$Num_1990 <- paste(EC_1990$Num_1990,')',sep = '')
EC_1990$Num_1990 <- paste('(',EC_1990$Num_1990,sep = '')
EC_1990$Num_1990 <- paste(EC_1990$val,EC_1990$Num_1990,sep = ' ')

##Read file
EC_2019 <- subset(EC,  EC$year==2021 &
                    EC$age=='All ages' &
                    EC$metric== 'Number' &
                    EC$measure=='Incidence')
##extract Number and Uncertain Interval
EC_2021 <- EC_2021[, c(2,8,9,10)] 
EC_2021$val < round(EC_2021$val,1)
EC_2021$lower <- round(EC_2021$lower,1) 
EC_2021$upper <- round(EC_2021$upper,1)
EC_2021$Num_2021 <- paste(EC_2021$lower,EC_2021$upper,sep = '-')
EC_2021$Num_2021 <- paste(EC_2021$Num_2021,')',sep = '')
EC_2021$Num_2021 <- paste('(',EC_2021$Num_2021,sep = '')
EC_2021$Num_2021 <- paste(EC_2021$val,EC_2021$Num_2021,sep = ' ')

########################  Calculation 1990,2021 Age-Standardized Rate(ASR)

##Read file
ASR_1990 <- subset(EC,  EC$year==1990 &
                    EC$age=='Age-standardized' &
                    EC$metric== 'Rate' &
                    EC$measure=='Incidence')
##extract ASR and Uncertain Interval
ASR_1990 <- ASR_1990[, c(2, 8, 9, 10)]  
ASR_1990$val < round(ASR_1990$val,4)
ASR_1990$lower <- round(ASR_1990$lower,4) 
ASR_1990$upper <- round(ASR_1990$upper,4)#
ASR_1990$ASR_1990 <- paste(ASR_1990$lower,ASR_1990$upper,sep = '-')
ASR_1990$ASR_1990 <- paste(ASR_1990$ASR_1990,')',sep = '')
ASR_1990$ASR_1990 <- paste('(',ASR_1990$ASR_1990,sep = '')
ASR_1990$ASR_1990 <- paste(ASR_1990$val,ASR_1990$ASR_1990,sep = ' ')


##Read file
ASR_2021 <- subset(EC,  EC$year==2021 &
                     EC$age=='Age-standardized' &
                     EC$metric== 'Rate' &
                     EC$measure=='Incidence')
##extract ASR and Uncertain Interval
ASR_2021 <- ASR_2021[, c(2, 8, 9, 10)]  
ASR_2021$val < round(ASR_2021$val,4)
ASR_2021$lower <- round(ASR_2021$lower,4) 
ASR_2021$upper <- round(ASR_2021$upper,4)
ASR_2021$ASR_2021 <- paste(ASR_2021$lower,ASR_2021$upper,sep = '-')
ASR_2021$ASR_2021 <- paste(ASR_2021$ASR_2021,')',sep = '')
ASR_2021$ASR_2021 <- paste('(',ASR_2021$ASR_2021,sep = '')
ASR_2021$ASR_2021 <- paste(ASR_2021$val,ASR_2021$ASR_2021,sep = ' ')


################### Calculation of Estimate Annual Percentage Change(EAPC)
##Read file
EAPC <- subset(EC, EC$age=='Age-standardized' & 
                 EC$metric== 'Rate' &
                 EC$measure=='Incidence')
##Calculation of EAPC and its confidence interval
EAPC <- EAPC[,c(2,7,8)]
country_name <- subset(EC,EC$year==1990 & 
                         EC$age=='Age-standardized' & 
                         EC$metric== 'Rate' &
                         EC$measure=='Incidence') 
country <-EC_1990$location 
EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=22),UCI=rep(0,times=22),LCI=rep(0,times=22))
for (i in 1:22){
  country_cal <- as.character(EAPC_cal[i,1])
  a <- subset(EAPC, EAPC$location==country_cal)
  a$y <- log(a$val)
  mod_simp_reg<-lm(y~year,data=a)
  estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
  low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
  EAPC_cal[i,2] <- estimate
  EAPC_cal[i,4] <- low
  EAPC_cal[i,3] <- high
}
EAPC_incidence <- EAPC_cal[,c(1,2)]
names(EAPC_incidence)[2] <- 'EAPC_incidence'

EAPC_cal$EAPC <- round(EAPC_cal$EAPC,4) 
EAPC_cal$UCI <- round(EAPC_cal$UCI,4)
EAPC_cal$LCI <- round(EAPC_cal$LCI,4)
EAPC_cal$EAPC_CI <- paste(EAPC_cal$LCI,EAPC_cal$UCI,sep = '-')
EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC_CI,')',sep = '')
EAPC_cal$EAPC_CI <- paste('(',EAPC_cal$EAPC_CI,sep = '')
EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC,EAPC_cal$EAPC_CI,sep = ' ')


EC_1990 <- EC_1990[,c(1,5)] 
ASR_1990 <- ASR_1990[,c(1,5)]
EC_2019 <- EC_2019[,c(1,5)]
ASR_2019 <- ASR_2019[,c(1,5)]
EAPC_cal <- EAPC_cal[,c(1,5)]
Incidence <- merge(EC_1990,ASR_1990,by='location')
Incidence <- merge(Incidence,EC_2019,by='location')
Incidence <- merge(Incidence,ASR_2019,by='location')
Incidence <- merge(Incidence,EAPC_cal,by='location')
write.csv(Incidence,'Results for Incidence.csv')
