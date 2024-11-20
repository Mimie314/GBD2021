
library(caTools)
library(Epi)
library(fanplot)
library(colorspace)
library(BAPC)
library(magrittr)
library(dplyr)
library(remotes)
library(data.table)

age_stand = read.csv("A_age_stand.csv", header=T)##The age-standardized age group structure.
wstand = (age_stand$std_population)/100
sum(wstand)

#Read Population Data
china_ay <- read.csv("population_female.csv", header=T, row.names = 1)
china_ay <- apply(china_ay, c(1,2), as.integer) %>% as.data.frame()

#Read the burden of burn data.
esoph_ay <- read.csv("Female_DALYs.csv", header=T, row.names = 1)
esoph_ay = esoph_ay/10
esoph_ay <- apply(esoph_ay, c(1,2), as.integer) %>% as.data.frame()


##Construct the BAPC model using the INLA package.
lc_esoph <- APCList(esoph_ay, china_ay, gf = 5)
library(sp)
library(parallel)
library(foreach)
library(Matrix)
library(INLA)
require(INLA)
bapc_result <- BAPC(lc_esoph, predict = list(npredict = 29, retro = T),
               secondDiff = FALSE, stdweight = wstand, verbose = TRUE)
##Polt
plotBAPC(bapc_result, scale=10^6, start = "1990", type = 'ageStdRate', showdata = TRUE)


ageGroup = c("0-9 years","10-14 years","15-19 years","20-24 years","25-29 years",
"30-34 years","35-39 years","40-44 years","45-49 years","50-54 years","55-59 years",
"60-64 years","65-69 years","70-74 years","75-79 years","80-84 years","85+ years")
#Calculate the number of incident cases across different age strata.
proj = agespec.proj(x=bapc_result)%>% as.data.frame()
proj_mean = proj[, colnames(proj)%like%"mean"]
colnames(proj_mean) = ageGroup
#Calculate the incidence rate for each age stratum.
rate = agespec.rate(x=bapc_result)%>% as.data.frame()
rate_mean = rate[, colnames(proj)%like%"mean"]
colnames(rate_mean) = ageGroup
#Calculate the ASR.
ASR = agestd.rate(x=bapc_result)%>% as.data.frame()
ASR$mean = ASR$mean*100000
ASR$year = rownames(ASR)
#Calculate the total number of incident cases.
sum_year = apply(proj_mean,1,sum)%>% as.data.frame()
colnames(sum_year) = "number"
sum_year$year = rownames(sum_year)




