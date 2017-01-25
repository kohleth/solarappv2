library(plyr)
library(tidyverse)
library(rdrop2)
library(darksky)
source("Auth.R")
load("VICdata.RData")


pvdataList=drop_dir("solarappv2/data",n=0)
pvdataDate=as.Date(pvdataList$path,format="/solarappv2/data/%Y%m%d.csv")
weatherList=drop_dir("solarappv2/weather",n=0)
weatherDate=as.Date(weatherList$path,format="/solarappv2/weather/W%Y%m%d.csv")

count=0
for(dd in as.character(pvdataDate)[!pvdataDate%in%weatherDate]){
  print(dd)
  if(count<750){
    filename=paste0("W",gsub("-","",dd),".csv")
    pvdata=drop_read_csv(paste0("solarappv2/data/",gsub("-","",dd),".csv"))
    pc=unique(pvdata$postcode)
    Coord=subset(VICdata,postcode%in%pc)%>%
      group_by(postcode)%>%
      summarize(lat=lat[1],lon=lon[1])
    Wlist <- pmap(list(Coord$lat, Coord$lon),
                  get_forecast_for,
                  timestamp=paste0(dd,"T12:00:00+1000"),
                  exclude="currently,minutely,hourly,alerts,flags")
    weather=do.call(rbind.fill,lapply(Wlist,`[[`,1))%>%
      mutate(postcode=Coord$postcode)
    write.csv(weather,file=filename)
    drop_upload(filename,"solarappv2/weather/")
    file.remove(filename)
    count=count+nrow(Coord)
  }else{
    break
  }
}

