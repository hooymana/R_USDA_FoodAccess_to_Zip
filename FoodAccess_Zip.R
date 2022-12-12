rm(list = ls())


library(ggplot2)
library(zipcodeR)
library(tidycensus)

# alabama_income <- get_acs(
#   geography = "tract", 
#   variables = "B19013_001",
#   state = "AL", 
#   year = 2020,
#   geometry = TRUE
# )
# 
# alabama_income
# 
# alabama_county <- get_acs(
#   geography = "county", 
#   variables = "B19013_001",
#   state = "AL", 
#   year = 2020,
#   geometry = TRUE
# )
# 
# alabama_county
# 
# plot(alabama_county["estimate"])
# plot(alabama_income["estimate"])



#PUT YOUR DATA HERE
# setwd("C:/Users/Andrew Hooyman/Documents/ADIallzip")
# myzip=read.csv("MyZip.csv")

#Algorithm to add 0 to zip codes with 4 digits
for(i in 1:dim(myzip)[1]){
  if(nchar(myzip$zip5[i])<5){
    myzip$zip5[i]=paste("0",myzip$zip5[i],sep="")
  }
}

#Get longitudinal and latitude data
for(i in 1:dim(myzip)[1]){
  #Get data from zipcodeR
  z=zipcodeR::reverse_zipcode(myzip$zip5[i])
  myzip$COUNTY[i]=z$county
  myzip$lat[i]=z$lat
 
  myzip$lng[i]=z$lng
  
  myzip$medianincome[i]=z$median_household_income
  myzip$medianhouse[i]=z$median_home_value
  myzip$popdense[i]=z$population_density
  myzip$population[i]=z$population
  #Get all census tracts for a given 5 digit zip code
  tryCatch({
  myzip$ziptract[i]=get_tracts(myzip$zip5[i])[,3]
  },error=function(e){}
  )
  
  print(i)
}

head(myzip$ziptract)
names(myzip)

#https://www.ers.usda.gov/data-products/food-access-research-atlas/go-to-the-atlas/
#https://www.ers.usda.gov/data-products/food-access-research-atlas/download-the-data/

setwd("C:/Users/Andrew Hooyman/Dropbox (ASU)/ReproRehab/Workshop_ADI")
# https://www.ers.usda.gov/data-products/food-access-research-atlas/download-the-data/
FAA=read.csv("FoodAccessResearchAtlasData2019.csv",header = T,colClasses = c("character"))
names(FAA)[1]="CensusTract"

#List which variables you want from FAA here
anames=c("PovertyRate","MedianFamilyIncome","LILATracts_1And10","lahunvhalfshare","LAPOP1_10",
         "lapop1share","lawhitehalfshare","lablackhalfshare","LALOWI1_10","Pop2010","lapophalfshare")
a=matrix(data = NA,nrow=1397,ncol = length(anames))
colnames(a)=anames

#Identify matching census tracts from your data to FAA census tracts for all anames variables
for(i in 1:dim(myzip)[1]){
  
  #Use try catch to move through loop in the event a tract does not exist or entry is NA
  tryCatch({
    a[i,]=colMeans(apply(FAA[FAA$CensusTract %in% myzip$ziptract[[i]],c(anames)],2,as.numeric),na.rm = T)
    #Get mean poverty rate among all census tracts within zip
    #myzip$PovertyRate[i]=mean(as.numeric(FAA[FAA$CensusTract %in% myzip$ziptract[[i]],c("PovertyRate","MedianFamilyIncome")]),na.rm = T)
    
  },error=function(e){}
  )
  print(i)
}

a=as.data.frame(a)

#Success of matching
apply((!apply(a,2,is.na)),2,sum)/dim(myzip)[1]*100

myzip=cbind(myzip,a)

#Compare median income from zipcoder to that of Food Atlas
ggplot(myzip,aes(x=medianincome,y=MedianFamilyIncome))+
  geom_point()


ggplot(myzip,aes(x=Pop2010,y=population))+
  geom_point()+
  geom_smooth(method = 'lm')+
  ylab("Population ZipCodeR")+
  xlab("Population 2010 FAA")

ggplot(myzip,aes(x=PovertyRate,y=lahunvhalfshare))+
  geom_point()+
  geom_smooth(method = 'lm')+
  ylab("Low Access at half mile and no Vehicle")+
  xlab("Poverty Rate")

fit=lm(lahunvhalfshare~PovertyRate,myzip)
summary(fit)

#Look at how relationship within my data matches that of the relationship within the FAA overall
ggplot(FAA,aes(x=as.numeric(PovertyRate),y=as.numeric(lahunvhalfshare)))+
  geom_point()+
  geom_abline(intercept = .96,slope = .255,color="red",size=2)+
  geom_smooth(method = 'lm')+
  ylab("Low Access at half mile and no Vehicle")+
  xlab("Poverty Rate")

# ggplot(myzip,aes(y=lapophalfshare,x=PovertyRate))+
#   geom_point()+
#   geom_smooth(method = 'lm')
# 
# ggplot(FAA,aes(y=as.numeric(lapophalfshare),x=as.numeric(PovertyRate)))+
#   geom_point()+
#   geom_smooth(method = 'lm')
# 
# ggplot(myzip,aes(y=lawhitehalfshare,x=PovertyRate))+
#   geom_point()+
#   geom_smooth(method = 'lm')
# 
# ggplot(FAA,aes(y=as.numeric(lawhitehalfshare),x=as.numeric(PovertyRate)))+
#   geom_point()+
#   geom_smooth(method = 'lm')
# 
# ggplot(myzip,aes(y=lablackhalfshare,x=PovertyRate))+
#   geom_point()+
#   geom_smooth(method = 'lm')
# 
# ggplot(FAA,aes(y=as.numeric(lablackhalfshare),x=as.numeric(PovertyRate)))+
#   geom_point()+
#   geom_smooth(method = 'lm')
# 
# ggplot(myzip,aes(y=lablackhalfshare,x=lawhitehalfshare,color=PovertyRate))+
#   geom_point()+
#   geom_smooth(method = 'lm')

