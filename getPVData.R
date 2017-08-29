library(rdrop2)
library(RCurl)
library(darksky)
source("Auth.R")

input=list(date=Sys.Date())
Havedata = drop_exists(paste0("solarappv2/data/",gsub("-","",input$date),".csv"))
if (!Havedata) {
  print("Dont have data. Sourcing...")
  req = paste0("http://data.pvoutput.org/service/r2/getregionoutput.jsp?r=1:victoria&sid=40450&key=",pvoutputKey)
  h <- basicHeaderGatherer()
  doc <- getURI(req, headerfunction = h$update)
  h$value()
  csv = read.csv(text = doc,header = FALSE)
  names(csv) = c("id","size","postcode","output")
  csv$size = csv$size / 1000
  csv$output = csv$output / 1000
  filename = paste0(h$value()["X-Payload-Date"],".csv")
  write.csv(csv,row.names = FALSE,file = filename)
  drop_upload(file = filename,dest = "solarappv2/data/") ## store at dropbox
  file.remove(filename)
  print("Sourcing done!")
}else{
  print("Already have today's data.")
}