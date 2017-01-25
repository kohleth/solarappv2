library(tidyverse)
library(ape)
library(cluster)

### cluster analysis
weatherdata=foreach(ff=list.files("~/Downloads/weather/",full.names = TRUE),.combine="rbind.fill")%do%{
  read.csv(ff,header = TRUE)
}

temp=weatherdata%>%
  as.tbl%>%
  mutate(postcode=factor(postcode))%>%
  select(time,temperatureMin,temperatureMax,humidity,cloudCover,pressure,postcode)%>%
  group_by(postcode)%>%
  mutate(temperatureMin=ifelse(is.na(temperatureMin),mean(temperatureMin,na.rm=T),temperatureMin),
         cloudCover=ifelse(!is.finite(cloudCover),mean(cloudCover,na.rm=T),cloudCover),
         pressure=ifelse(is.na(pressure),mean(pressure,na.rm=T),pressure))%>%
  ungroup%>%
  mutate(cloudCover=ifelse(is.nan(cloudCover),mean(cloudCover,na.rm=T),cloudCover))%>%
  gather("var","value",-postcode,-time)%>%
  mutate(dv=paste(time,var,sep=":"))%>%
  select(-time,-var)%>%
  spread(dv,value)%>%
  remove_rownames()%>%
  select(-postcode)%>%
  scale%>%
  dist%>%
  additive%>%
  as.dist%>%
  pam(k=15)

filter(VICdata,postcode%in%pc[temp$medoids])%>%select(lat,lon)
