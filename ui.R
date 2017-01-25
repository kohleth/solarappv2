

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
# library(shinythemes)
load("suburbpostcode.RData")
Sys.setenv(TZ='Australia/Sydney')

## determine default date
currentDateTime = Sys.time()
# attr(currentDateTime,"tzone") = "Australia/Sydney"
currentDatechr = strftime(currentDateTime,format = "%Y-%m-%d",tz="Australia/Sydney")
if (currentDateTime > paste(currentDatechr,"21:00:01 AEST",sep = " ")) {
  defaultDate = as.Date(currentDatechr)
}else{
  defaultDate = as.Date(currentDatechr)-1
}

print(defaultDate+3)

analyticPage = fluidPage(
  # Application title
  titlePanel(
    "Compare your PV output against systems within 10km of your own suburb! (v0.4)"
  ),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "postcode","Select your Suburb/Postcode",choices = c(`Enter Suburb/Postcode` =
                                                               '',suburbpostcode),selected = NULL,
        multiple = FALSE
      ),
      dateInput(
        "date","Select a date (Australia/Victoria timezone)",min = "2015-09-16",max =
          defaultDate + 3,value = defaultDate
      ),
      sliderInput(
        "mySystemSize","Select your System's Size in kW",min = 1,max = 6,value = 3,step =
          0.25
      ),
      # numericInput("myOutput","Enter your System's Output in kWh (e.g. 12.5, 15.8)",NULL,step = 0.1,min=0,max=50),
      textInput("myOutput","Enter your System's Output in kWh (e.g. 12.5, 15.8)"),
      # checkboxInput("advance",label="More statistics",value=FALSE),
      downloadButton("downloadData","Download copmarison data as a .csv file.")#,
      # conditionalPanel("input.myOutput=='--refresh'", actionButton("resource", label="",icon=icon("refresh"))),
      #       HTML(
      #         '<input type="text" id="client_time" name="client_time" style="display: none;" > '
      #       )
    ),
    mainPanel(tabsetPanel(
      tabPanel(
        "Summary",
        column(5,h3(textOutput("todaytext")),
               h1(textOutput("todayvalue")),
               textOutput("todayweatherSum"),
               textOutput("todayMaxTemp"),
               textOutput("todaySun")),
        column(5,h3(textOutput("tmrtext")),
               h1(textOutput("tmrvalue")),
               textOutput("tmrweatherSum"),
               textOutput("tmrMaxTemp"),
               textOutput("tmrSun"))
      ),
      tabPanel(
        "Compare",
        sidebarLayout(
          mainPanel = mainPanel(plotOutput("distPlot")),
          sidebarPanel = sidebarPanel(
            "Statistics",
            checkboxInput("normalReg",label = "Add regression line?",TRUE),
            checkboxInput("origReg",label = "Add regression line with intercept 0?",FALSE),
            checkboxInput("quantile",label =
                            "Add quantile regression (10/90 percentile)?")
          ),
          position = "right"
        )
      ),
      tabPanel(
        "Forecast",
        sidebarLayout(
          mainPanel = mainPanel(plotOutput("predictplot")),
          sidebarPanel = sidebarPanel(
            "Inference",
            checkboxInput("ptest",label = "Show predicted output?",TRUE),
            checkboxInput("predint",label = "Add 95% prediction interval?",FALSE)
            # checkboxInput("quantile",label="Add quantile regression (10/90 percentile)?")),
          ),
          position = "right"
        )
      )
    ))
  ),
  hr(),
  h3("Message"),
  textOutput("MsgSelected"),
  textOutput("MsgBadArea"),
  textOutput("MsgBadDate"),
  textOutput("MsgCompareFuture"),
  textOutput("MsgfewData"),
  textOutput("MsgPtEst"),
  textOutput("MsgPredInt"),
  hr(),
  HTML(
    "Have an issue? Or have an idea on how to improve this site? <a href='mailto:kohleth@gmail.com?
    body='PV compare enquiry'
    &subject='PV compare enquiry'' >Tell me!</a>"
  )
)

faqPage = fluidPage(includeHTML("faq.html"))

chipinPage = fluidPage(includeHTML("chipin2.html"))

footercontent = ""




shinyUI(
  navbarPage(
    title = "CheckMySolar",
    tabPanel("Analytics",analyticPage),
    tabPanel("FAQ",faqPage),
    tabPanel("Chip in",chipinPage),
    footer = "",inverse = FALSE
  )
)
