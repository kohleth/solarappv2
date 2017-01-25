library(shiny)
library(foreach)
library(latticeExtra)
library(iterators)
library(quantreg)
library(rdrop2)
library(RCurl)
library(darksky)
library(mgcv)
source("Auth.R")

load("VICdata.RData")
# load("bestgamm.Rda")
load("bestlme.Rda")
Sys.setenv(TZ = 'Australia/Sydney')
enrich = function(data) {
  data$cloudef68 = with(data,cloudCover * (cloudCover >= 0.6) * (cloudCover <=
                                                                   0.8))
  data$cloudef8 = with(data,ifelse(cloudCover > 0.8,cloudCover,0))
  data$dewPointl50 = with(data,ifelse(dewPoint < 47,dewPoint,0))
  data$dewPointg50 = with(data,ifelse(dewPoint >= 47,dewPoint,0))
  data$maxTempeftl65 = with(data,ifelse(temperatureMax <= 65,temperatureMax,0))
  data$maxTempeftg65 = with(data,ifelse(temperatureMax > 65,temperatureMax,0))
  data$SystemID = 1000
  data$`Max.Temp` = data$temperatureMax
  data$sunDuration = as.numeric(with(data,sunsetTime - sunriseTime))
  
  return(data)
}


currentDateTime = format(Sys.time(),tz = "Australia/Sydney")
currentDatechr = strftime(currentDateTime,format = "%Y-%m-%d")
if (currentDateTime > paste(currentDatechr,"21:00:01 AEST",sep = " ")) {
  defaultDate = as.Date(currentDatechr,tz = "Australia/Sydney")
}else{
  defaultDate = as.Date(currentDatechr,tz = "Australia/Sydney") - 1
}


shinyServer(function(input,output,session) {
  ## Update PVdata if don't have it.
  observe({
    notfuture = input$date <= defaultDate
    if (notfuture) {
      Havedata = drop_exists(paste0("solarappv2/data/",gsub("-","",input$date),".csv"))
      if (!Havedata) {
        # print("Dont have data. Sourcing...")
        req = "http://data.pvoutput.org/service/r2/getregionoutput.jsp?r=1:victoria&sid=40450&key=d0186e6915e81da69523ef40ab958ace6271f3e9"
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
        # print("Sourcing done!")
      }
    }
  })
  
  ## Get relevant PVdata
  PVdata = reactive({
    userfile=paste0(gsub("-","",input$date),".csv")
    if(file.exists(userfile)){
      print("reading local csv file.")
      read.csv(userfile,header=TRUE)
    }else{
      print("reading csv from dropbox.")
      drop_read_csv(
        paste0("solarappv2/data/",userfile),header = TRUE)
    }
    
    # print("read csv from dropbox.")
  })
  
  
  userpostcode = eventReactive(input$postcode,{gsub("[[:alpha:]]| ","",input$postcode)})
  
  userdist = eventReactive(userpostcode(),{
    if (userpostcode() != "") {
      sqrt(rowSums(sweep(coords,2,coords[userpostcode(),],FUN =
                           "-") ^ 2))
    }
  })
  
  ## clean and format data for compare.
  compareDF = reactive({
    print("forming compareDF.")
    nearbypostcode = unique(names(userdist()[userdist() < 0.1]))
    dd = subset(PVdata(),postcode %in% nearbypostcode)
    dd = subset(dd,cooks.distance(lm(output ~ size,data = dd)) < 0.5)
    dd
  })
  
  ## Fit lm for comparison
  lmfit = reactive({
    print("fitting lmfit.")
    lm(output ~ size,data = compareDF())
  })
  
  ## Calc neighbour Mean output.
  neighbrAvg = reactive({
    print("calculating neighbrAvg.")
    predict(lmfit(),newdata = list(size = input$mySystemSize))
  })
  
  
  ## Predict next 0-1 day
  predlist = reactive({
    print("forecasting.")
    usercoord = coords[userpostcode(),]
    foreach(i = 0:1) %do% {
      day = get_forecast_for(
        latitude = usercoord["lat"],longitude = usercoord["lon"], paste0(input$date +
                                                                           i,"T12:00:00+1000")
      )$daily
      daydf = enrich(data.frame(day))
      pred = predict(bestlme,daydf,level = 0)
      Designmat <-
        model.matrix(eval(eval(bestlme$call$fixed)[-2]), daydf)
      predvar <- diag(Designmat %*% bestlme$varFix %*% t(Designmat))
      confse = sqrt(predvar)
      predse = sqrt(predvar + bestlme$sigma ^ 2)
      data.frame(pred = pred,confse = confse,predse = predse,day)
    }
  })
  
  observe({
    if (!input$postcode == "") {
      output$tmrtext = renderText(paste("Forecast for",input$date + 1,":"))
      if (!inherits(try(predlist()[[2]]),"try-error")) {
        output$tmrvalue=renderText(paste(round(predlist()[[2]]["pred"] * input$mySystemSize,1),"kWh"))
        output$tmrweatherSum=renderText(predlist()[[2]]$summary)
        output$tmrMaxTemp=renderText(paste("Max Temp.:",round((predlist()[[2]]$temperatureMax-30)/2,1),"C"))
        output$tmrSun=renderText(with(predlist()[[2]],paste("Sun rise/set:",format(sunriseTime,"%H:%m"),"—",format(sunsetTime,"%H:%m"))))
      }
      output$todayweatherSum=renderText(predlist()[[1]]$summary)
      output$todayMaxTemp=renderText(paste("Max Temp.:",round((predlist()[[1]]$temperatureMax-30)/2,1),"C"))
      output$todaySun=renderText(with(predlist()[[1]],paste("Sun rise/set:",format(sunriseTime,"%H:%m"),"—",format(sunsetTime,"%H:%m"))))
      
      if (!inherits(try(neighbrAvg()),"try-error")) {
        output$todaytext = renderText(paste("Average on",input$date,":"))
        output$todayvalue=renderText(paste(round(neighbrAvg(),1),"kWh"))
      }else{
        output$todaytext = renderText(paste("Forecast for",input$date,":"))
        output$todayvalue=renderText(paste(round(predlist()[[1]]["pred"] * input$mySystemSize,1),"kWh"))
      }
    }else{
      output$tmrvalue=output$todayvalue=renderText("")
      output$todayweatherSum=output$tmrweatherSum=renderText("")
      output$todayMaxTemp=output$tmrMaxTemp=renderText("")
      output$todaySun=output$tmrSun=renderText("")
    }
  })
  
  #   output$todayweatherSum=renderText({
  #     # print(predlist()[[1]])
  #     predlist()[[1]]$summary
  #   })
  
  observe({
    if (!input$postcode == "") {
      output$tmrweatherSum=renderText({
        # print(predlist()[[1]])
        predlist()[[2]]$summary
      })
    }
  })
  
  output$distPlot = renderPlot({
    if (input$date %in% as.Date(
      c(
        "2015-09-22","2015-09-23","2015-09-24","2015-09-25","2015-09-26","2015-09-27"
      )
    )) {
      plot(
        0:1,0:1,type = "n",xlab = "",ylab = "",axes = FALSE
      )
      text(0.5,0.5,labels = "Sorry. No data. See Message below.")
      output$MsgBadDate = renderText(
        "Sorry. We don't currently have data for dates between 2015-09-23 to 2015-09-27.
        We are trying to retrieve them, but this is not that easy.
        Alternatively, you could use the forecast facility to predict what would have happened on those dates."
      )
    }else{
      output$MsgBadDate = renderText("")
      if (input$date > defaultDate) {
        plot(
          0:1,0:1,type = "n",xlab = "",ylab = "",axes = FALSE
        )
        text(0.5,0.5,labels = "You cannot use Compare. But you can use Forecast. See Message below.")
        output$MsgCompareFuture = renderText(
          "You cannot compare with what has not happen (the future)! But you can use the Forecast facility to predict what might happen."
        )
      }else{
        output$MsgCompareFuture = renderText("")
        if (input$postcode != "") {
          main = paste0(
            "PVoutput.org data from ",nrow(compareDF())," systems within 10km of ",userpostcode()
          )
          cols = trellis.par.get("superpose.symbol")$col
          if (nrow(compareDF()) > 2) {
            output$MsgBadArea = renderText("")
            output$MsgSelected = renderText(paste(
              "You have selected",with(VICdata,suburb[postcode == input$postcode]),"postcode:",input$postcode,"."
            ))
            output$MsgfewData = renderText("")
            if (nrow(compareDF()) < 10) {
              output$MsgfewData = renderText(
                paste(
                  msg,"Warning: the amount of data we have for your area is not great. Any statistical model (including the regression line) will not be reliable."
                )
              )
            }
            output$neighbrAvg = renderText(neighbrAvg())
            xyplot(
              output ~ size,type = c("g","p"),data = compareDF(),xlim = c(0,6),ylim = c(0,40),
              panel = function(x,y,...) {
                panel.xyplot(x,y,...)
                panel.points(
                  x = input$mySystemSize,y = as.numeric(input$myOutput),col = cols[2],pch =
                    19
                )
                if (input$normalReg) {
                  panel.ablineq(reg = lm(y ~ x),label = "regression line",at = 0.1)
                }
                if (input$origReg) {
                  panel.ablineq(
                    reg = lm(y ~ x - 1),lty = 2,label = "regression through the origin",at =
                      0.7
                  )
                }
                if (input$quantile) {
                  # panel.quantile(x,y,tau=c(0.1,0.9),...)
                  taus = c(0.1,0.9)
                  rq0 = rq(y ~ x,tau = taus)
                  for (i in 1:ncol(coef(rq0))) {
                    panel.ablineq(
                      a = coef(rq0)[1,i],b = coef(rq0)[2,i],lty = 2,label = taus[i],at = 0.5
                    )
                    
                  }
                  
                }
                if (input$myOutput != "") {
                  draw.key(
                    list(
                      text = list(lab = c(
                        "PVoutput.org data", "Your System"
                      )),
                      points = list(pch = c(1,19),col = cols[1:2])
                    ),draw = TRUE,
                    vp = grid::viewport(
                      x = grid::unit(0.2, "npc"), y = grid::unit(0.9, "npc")
                    )
                  )
                }else{
                  draw.key(
                    list(
                      text = list(lab = c("PVoutput.org data")),
                      points = list(pch = c(1),col = cols[1])
                    ),draw = TRUE,
                    vp = grid::viewport(
                      x = grid::unit(0.2, "npc"), y = grid::unit(0.9, "npc")
                    )
                  )
                }
                
              },
              xlab = "system size (kw)",ylab = "output (kWh)",main =
                main
            )
            
          }else{
            ## try to find a nearby postcode with enough data.
            morethan1 = (table(PVdata()$postcode))
            morethan1 = morethan1[morethan1 > 1]
            morethan1 = subset(morethan1,names(morethan1) != userpostcode())
            morethan1id = which(as.numeric(names(
              sort(userdist(),decreasing = FALSE)
            )) %in% names(morethan1))[1:3]
            morethan1postcode = unique(as.numeric(names(
              sort(userdist(),decreasing = FALSE)
            ))[morethan1id])
            morethan1suburb = with(VICdata,paste(suburb[postcode %in% morethan1postcode],morethan1postcode))
            output$MsgBadArea = renderText(
              paste(
                "Sorry there isn't enough data in your area. You could try",
                paste(morethan1suburb,collapse = ", "),". Alternatively, you could use the Forecast facility to predict what might have happened in your area."
              )
            )
            plot(
              0:1,0:1,type = "n",axes = FALSE,xlab = "",ylab = ""
            )
            #         print(morethan1suburb)
            text(
              0.5,0.5,labels = paste(
                "Sorry, but there isn't enough data from your area. See message below."
              )
            )
            #         text(0.5,0.4,labels=paste("Try another suburb such as",paste(morethan1suburb,collapse ="")))
          }
          
        }else{
          output$msg = renderText("")
        }
      }
    }
    
  })
  
  output$downloadData = downloadHandler(
    filename = function() {
      paste0("PV",gsub("-","",input$date),".csv")
    },
    content = function(file) {
      write.csv(PVdata()[,-1],file,row.names = FALSE)
    }
  )
  
  output$predictplot = renderPlot({
    if (userpostcode() != "") {
      ptestmsg = predintmsg = ""
      userpred=input$mySystemSize * predlist()[[1]]$pred
      prednorm = function(x)
        dnorm(x,userpred,input$mySystemSize * predlist()[[1]]$predse)
      predqnorm = function(x)
        qnorm(x,userpred,input$mySystemSize * predlist()[[1]]$predse)
      curve(
        prednorm,xlim = predqnorm(c(1e-5,1 - 1e-5)),ylim = c(0,1.15 * prednorm(userpred)),
        ylab = "Probability Density",yaxt = "n",xlab = "predicted output (kWh)",main =
          paste(
            "Predicted output Dsitribution for",input$postcode,"on",input$date
          )
      )
      if (input$predint) {
        output$MsgPredInt = renderText(paste0(
          "The 95% prediction interval is ","(",paste(round(predqnorm(
            c(0.025,0.975)
          ),1),collapse = ","),") kWh."
        ))
        polygon(
          x = c(predqnorm(0.025),
                predqnorm(seq(
                  0.025,0.975,length = 50
                )),
                predqnorm(0.975)),
          y = c(-100,
                prednorm(predqnorm(
                  seq(0.025,0.975,length = 50)
                )),-100),
          col = "dodgerblue2",lty = 2
        )
        text(
          x = predqnorm(c(0.005,0.995)),y = prednorm(predqnorm(c(0.05,0.95))),
          labels = paste(round(predqnorm(
            c(0.025,0.975)
          ),1),"kWh"),col = "dodgerblue2",cex = 2
        )
      }
      if (input$ptest) {
        output$MsgPtEst = renderText(
          paste(
            "The prediction for",input$postcode,"on",input$date,"is",round(predqnorm(0.5),1),"kWh."
          )
        )
        lines(
          x = rep(predqnorm(0.5),2),y = c(-10,prednorm(userpred)),lty =
            2,col = "blue"
        )
        points(
          x = predqnorm(0.5),y = prednorm(userpred),pch = 19,col =
            "blue"
        )
        text(
          x = predqnorm(0.5),y = prednorm(userpred) * 1.1,
          labels = paste(round(predqnorm(0.5),1),"kWh"),col = "dodgerblue2",cex =
            2
        )
      }
    }
  })
  
  
  
  
})
