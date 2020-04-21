# ui.R
library(leaflet)
library(shiny)

navbarPage("Criminal Data Visualization",
tabPanel("Record vary on year",
sidebarLayout(
sidebarPanel(
selectInput("criminal", "criminal",
c("Assault" = "Assault",
"Burglary(Dwelling)" = "Burglary.Dwelling",
"Burglary(Other)" = "Burglary.Other",
"Graffiti" = "Graffiti",
"Robbery" = "Robbery")
))),
leafletOutput("myMap")
),

tabPanel("Data on Map",
mainPanel(plotOutput("mpgPlot")
),
tabPanel("Component 3")
)



fluidPage(

leafletOutput("myMap"),
verbatimTextOutput("Click_text"),
headerPanel("Coral breaching"),

sidebarLayout(
sidebarPanel(
selectInput("criminal", "criminal",
c("Assault" = "Assault",
"Burglary(Dwelling)" = "Burglary.Dwelling",
"Burglary(Other)" = "Burglary.Other",
"Graffiti" = "Graffiti",
"Robbery" = "Robbery")
),

selectInput("method", "Smooth method:",
c(
"no_smoother" = "null",
"lm" = "lm",
"glm" = "glm",
"gam" = "gam",
"loess" = "loess",
"rlm" = "rlm")),
verbatimTextOutput("Description_text")
),
))))










# More info:
#   https://github.com/jcheng5/googleCharts
# Install:
#   devtools::install_github("jcheng5/googleCharts")
library(googleCharts)

# Use global max/min for axes so the view window stays
# constant as the user moves between years
xlim <- list(
  min = min(data$Health.Expenditure) - 500,
  max = max(data$Health.Expenditure) + 500
)
ylim <- list(
  min = min(data$Life.Expectancy),
  max = max(data$Life.Expectancy) + 3
)

shinyUI(fluidPage(
  # This line loads the Google Charts JS library
  googleChartsInit(),

  # Use the Google webfont "Source Sans Pro"
  tags$link(
    href=paste0("http://fonts.googleapis.com/css?",
                "family=Source+Sans+Pro:300,600,300italic"),
    rel="stylesheet", type="text/css"),
  tags$style(type="text/css",
    "body {font-family: 'Source Sans Pro'}"
  ),

  h2("Google Charts demo"),

  googleBubbleChart("chart",
    width="100%", height = "475px",
    # Set the default options for this chart; they can be
    # overridden in server.R on a per-update basis. See
    # https://developers.google.com/chart/interactive/docs/gallery/bubblechart
    # for option documentation.
    options = list(
      fontName = "Source Sans Pro",
      fontSize = 13,
      # Set axis labels and ranges
      hAxis = list(
        title = "Health expenditure, per capita ($USD)",
        viewWindow = xlim
      ),
      vAxis = list(
        title = "Life expectancy (years)",
        viewWindow = ylim
      ),
      # The default padding is a little too spaced out
      chartArea = list(
        top = 50, left = 75,
        height = "75%", width = "75%"
      ),
      # Allow pan/zoom
      explorer = list(),
      # Set bubble visual props
      bubble = list(
        opacity = 0.4, stroke = "none",
        # Hide bubble label
        textStyle = list(
          color = "none"
        )
      ),
      # Set fonts
      titleTextStyle = list(
        fontSize = 16
      ),
      tooltip = list(
        textStyle = list(
          fontSize = 12
        )
      )
    )
  ),
  fluidRow(
    shiny::column(4, offset = 4,
      sliderInput("year", "Year",
        min = min(data$Year), max = max(data$Year),
        value = min(data$Year), animate = TRUE)
    )
  )
))



googleLineChart(ui)
----------------------
library(googleCharts)
library(readxl)
dataByYear <- read_excel('dataByYear.xlsx')



shinyUI(navbarPage("Criminal Data Visualization",
tabPanel("Record vary on year",
fluidPage(
verbatimTextOutput("Click_text"),
googleChartsInit(),
h2("Google Charts demo"),
googleLineChart(
"mpgPlot",width="100%",height=500,
option = list(hAxis = "{slantedText:true, slantedTextAngle:50 ,title : 'CountrySide', textStyle:{fontSize:8},showTextEvery:2}",fontSize = 10, vAxis="{ticks : [200,600,1000]}", width = "100%", height = "80%",chartArea ="{left:20,top:0,width:'50%',height:'75%'}"
)),
fluidRow(
sliderInput("year", "Year",
min = min(dataByYear$year,na.rm = TRUE), max = max(dataByYear$year,na.rm = TRUE),
value = min(dataByYear$year, na.rm = TRUE), animate = TRUE)
)
)),
tabPanel("Criminal Distribution On Map",
sidebarLayout(
sidebarPanel(
selectInput("criminal", "criminal",
c("Assault" = "Assault",
"Burglary(Dwelling)" = "Burglary.Dwelling",
"Burglary(Other)" = "Burglary.Other",
"Graffiti" = "Graffiti",
"Robbery" = "Robbery")
)),
mainPanel(leafletOutput("myMap")))
),
tabPanel("Component 3")
))




server
------------------------
# server.R
library(shiny)
library(datasets)
library(ggplot2)
library(readxl)
library(dplyr)
myData <- read_excel('criminal.xlsx')
dataByYear <- read_excel('dataByYear.xlsx')

shinyServer(function(input, output, session) {
    
  
    crim <- reactive({
        if(input$criminal == "Assault")
        {
            crim <- myData$Assault
        }
        else if(input$criminal == "Burglary.Dwelling")
        {
            crim <- myData$Burglary.Dwelling.
        }
        else if(input$criminal == "Burglary.Other")
        {
            crim <- myData$Burglary.other.
        }
        else if(input$criminal == "Graffiti")
        {
            crim <- myData$Graffiti
        }
        else if(input$criminal == "Robbery")
        {
            crim <- myData$Robbery
        }
    })
    #和下面的mymap换了一下位置
    output$myMap <- renderLeaflet({leaflet(myData) %>% addTiles() %>%addCircles(lng = ~myData$longitude, lat = ~myData$latitude, weight = 1,radius = ~sqrt(crim()) * 200, popup = myData$suburb)})
    
    
    #year variable
    yearData <- reactive({
        yearData <- data.frame(filter(dataByYear,year == input$year)[2],filter(dataByYear,year == input$year)[6],filter(dataByYear,year == input$year)[7],
        filter(dataByYear,year == input$year)[8],filter(dataByYear,year == input$year)[9],filter(dataByYear,year == input$year)[10])
    })
    

    output$Click_text<-renderText({
        # dataByYear$year
    })
    
    output$mpgPlot <- reactive({
    list(
    data = yearData(),option = list(
    title = sprintf("Criminal on",input$year)
    )
    )
    })
    
    #  output$mpgPlot <- gvisLineChart(data.frame(filter(dataByYear,year == 2000)[1],filter(dataByYear,year == 2000)[3],filter(dataByYear,year == 2000)[4],filter(dataByYear,year == 2000)[5],filter(dataByYear,year == 2000)[6],filter(dataByYear,year == 2000)[7]),option = list(hAxis = "{slantedText:true, slantedTextAngle:50 ,title : 'CountrySide', textStyle:{fontSize:8},showTextEvery:2}",fontSize = 10, vAxis="{ticks : [200,600,1000]}", width = "100%", height = "80%",chartArea ="{left:20,top:0,width:'50%',height:'75%'}"))

    observeEvent(input$myMap_marker_click, {
    ## Get the click info like had been doing click
    click <- input$myMap_marker_click
    clat <- click$lat
    clng <- click$lng
    if(is.null(click))
    return()
    if(clng == 151.914){
    text <- "site06" }
    if(clng == 151.717){
    text <- "site07"}
    if(clng == 150.444){
    text <- "site04"}
    if(clng == 147.898){
    text <- "site09"}
    if(clng == 146.589){
    text <- "site03"}
    if(clng == 145.715){
    text <- "site10"}
    if(clng == 145.043){
    text <- "site02"}
    if(clng == 143.786){
    text <- "site05"}
    if(clng == 143.515){
    text <- "site01"}
    if(clng == 144.081){
    text <- "site08"}
    
    output$mpgPlot <- renderPlot({
            ggplot(subset(myData, kind == input$variable), aes(year, bleaching)) +
            geom_point() +
            geom_rect(data = subset(myData, site == text & kind == input$variable),aes(fill = site),xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,alpha = 0.1) + facet_grid(kind~reorder(site,latitude)) + xlab("years") +
            ylab("bleaching") +
            geom_smooth(method = input$method) +
            ggtitle(paste( "Bleaching on ",input$variable)) +
                facet_grid(kind~reorder(site,latitude))
})})})

first iteration:完图版
-------------
ui
---
library(googleCharts)

shinyUI(navbarPage("Criminal Data Visualization",
tabPanel("Record vary on year",
fluidPage(
#verbatimTextOutput("Click_text"),
h3("Criminal vary on year"),
#--
htmlOutput("mpgPlot"),
#--
fluidRow(align="center",
sliderInput("year", "Year",
min = min(dataByYear$year,na.rm = TRUE), max = max(dataByYear$year,na.rm = TRUE),
value = min(dataByYear$year, na.rm = TRUE), animate = TRUE)
)
)),
tabPanel("Criminal Distribution On Map",
sidebarLayout(
sidebarPanel(
selectInput("criminal", "criminal",
c("Assault" = "Assault",
"Burglary(Dwelling)" = "Burglary.Dwelling",
"Burglary(Other)" = "Burglary.Other",
"Graffiti" = "Graffiti",
"Robbery" = "Robbery")
)),
mainPanel(leafletOutput("myMap")))
),
tabPanel("Further Investigation")
))

-------------
server
--
# server.R
library(shiny)
library(dplyr)


shinyServer(function(input, output, session) {
    
  
    crim <- reactive({
        if(input$criminal == "Assault")
        {
            crim <- myData$Assault
        }
        else if(input$criminal == "Burglary.Dwelling")
        {
            crim <- myData$Burglary.Dwelling.
        }
        else if(input$criminal == "Burglary.Other")
        {
            crim <- myData$Burglary.other.
        }
        else if(input$criminal == "Graffiti")
        {
            crim <- myData$Graffiti
        }
        else if(input$criminal == "Robbery")
        {
            crim <- myData$Robbery
        }
    })
    #和下面的mymap换了一下位置
    output$myMap <- renderLeaflet({leaflet(myData) %>% addTiles() %>%addCircles(lng = ~myData$longitude, lat = ~myData$latitude, weight = 1,radius = ~sqrt(crim()) * 200, popup = myData$suburb)})
    
    
    #year variable
     yearData <- reactive({
        yearData <- data.frame(filter(dataByYear,year == input$year)[2],filter(dataByYear,year == input$year)[6],filter(dataByYear,year == input$year)[7],
        filter(dataByYear,year == input$year)[8],filter(dataByYear,year == input$year)[9],filter(dataByYear,year == input$year)[10])
          })
    
    output$Click_text<-renderText({
        #   input$year
    })
    
    output$mpgPlot <- renderGvis({
    myChart <- gvisLineChart(yearData(),option = list(hAxis = "{slantedText:true, slantedTextAngle:50 ,title : 'CountrySide', textStyle:{fontSize:8},showTextEvery:2}",fontSize = 10, vAxis="{ticks : [200,600,1000]}", width = 2000, height = 500,chartArea ="{left:100,top:20,width:'50%',height:'75%'}"))
    return(myChart)
    })
    
    #  output$mpgPlot <- gvisLineChart(data.frame(filter(dataByYear,year == 2000)[1],filter(dataByYear,year == 2000)[3],filter(dataByYear,year == 2000)[4],filter(dataByYear,year == 2000)[5],filter(dataByYear,year == 2000)[6],filter(dataByYear,year == 2000)[7]),option = list(hAxis = "{slantedText:true, slantedTextAngle:50 ,title : 'CountrySide', textStyle:{fontSize:8},showTextEvery:2}",fontSize = 10, vAxis="{ticks : [200,600,1000]}", width = "100%", height = "80%",chartArea ="{left:20,top:0,width:'50%',height:'75%'}"))

    observeEvent(input$myMap_marker_click, {
    ## Get the click info like had been doing click
    click <- input$myMap_marker_click
    clat <- click$lat
    clng <- click$lng
    if(is.null(click))
    return()
    if(clng == 151.914){
    text <- "site06" }
    if(clng == 151.717){
    text <- "site07"}
    if(clng == 150.444){
    text <- "site04"}
    if(clng == 147.898){
    text <- "site09"}
    if(clng == 146.589){
    text <- "site03"}
    if(clng == 145.715){
    text <- "site10"}
    if(clng == 145.043){
    text <- "site02"}
    if(clng == 143.786){
    text <- "site05"}
    if(clng == 143.515){
    text <- "site01"}
    if(clng == 144.081){
    text <- "site08"}
    
    output$mpgPlot <- renderPlot({
            ggplot(subset(myData, kind == input$variable), aes(year, bleaching)) +
            geom_point() +
            geom_rect(data = subset(myData, site == text & kind == input$variable),aes(fill = site),xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,alpha = 0.1) + facet_grid(kind~reorder(site,latitude)) + xlab("years") +
            ylab("bleaching") +
            geom_smooth(method = input$method) +
            ggtitle(paste( "Bleaching on ",input$variable)) +
                facet_grid(kind~reorder(site,latitude))
})})})


