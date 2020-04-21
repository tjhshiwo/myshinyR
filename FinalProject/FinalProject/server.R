# server.R
library(shiny)
library(dplyr)


shinyServer(function(input, output, session) {
    

  #The text in the page "Further information"
  output$text1<-renderText({
        paste("This line chart above provide the general ideal of how different ","crime distributed in different suburb and the"," relationship with different elements such as income, population. Different view will be shown by"," selecting different elements in dropdown list below. Exploring now!!")})
    
  output$text3<-renderText({
        paste( "The Bubble chart above provide the picture about the relationship between different crime types and median weekly income, only criminal type can be changed in this graph by picking the Crimine Type below.")})
  
  #The dynamic map show in page "Criminal Distribution on Map"
  observe({
      colorBy <- input$color
      sizeBy <- input$size

       colorData <- myData[[colorBy]]
       pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
       radius <- myData[[sizeBy]] / max(myData[[sizeBy]]) * 150000
      
      output$map <- renderLeaflet({
          leaflet(myData) %>%
          addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
          ) %>%
          setView(lng = 124, lat = -27, zoom = 4) %>%
          clearShapes() %>%
          addCircles(lng = ~myData$longitude, lat = ~myData$latitude, radius=radius,layerId=~suburb,
          stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
          addLegend("bottomright", pal=pal, values=colorData, title=colorBy,
          layerId="colorLegend")
      })
  })
  
   #Draw the bar chart and enable the color change based on the year which selected by the user
   observe({
      color <- c('rgba(204,204,204,1)',
                 'rgba(204,204,204,1)',
                 'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
                 'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
                 'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
                 'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
                 'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
                 'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
                 'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
                 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)')
      label <- c("","","","","","","","","","","","","","","","","","","")
                 
       color[which(totalData$Year ==input$PieYear)] <- 'rgba(222,45,38,0.8)'
       label[which(totalData$Year == input$PieYear)] <- paste("Year: ",input$PieYear,'\n',"Total Crime",totalData[which(totalData$Year == input$PieYear),]$Total)
       
       output$plot <- renderPlotly({
           plot_ly(totalData, x= ~Year, y=~Total,type = 'bar',marker = list(color = color)) %>%
           layout(height = 200,annotations = list(x= ~Year, y=~Total,text=label,showarrow = FALSE ))
       })
    
  })
  
    #Draw the bubble chart which enable the change of year.
    output$bubble <-renderGvis({
        bubble <-  gvisBubbleChart(filter(dataByYear,year == input$yearOfRe),xvar='Median.weekly.household.income',yvar = input$CriminalType,colorvar = "suburb",sizevar="Population",option = list(legend = "{position: 'none'}",bubble = "{opacity:0.4,stroke:'none',textStyle:{color:'none'}}",hAxis = "{title:'Income'}",vAxis = paste("{title:'",input$CriminalType,"',ticks:[-150,150,300,450,600,750]}"),title = "The relationship between different crime types with income",height = 300))
    })
    
    #Draw the line chart whihc enable the change of the criminal type, year, and income or population
    output$relationship <- renderGvis({
        line1 <- gvisLineChart(filter(dataByYear,year == input$yearOfRe),xvar = "suburb", yvar = c(input$population,input$CriminalType), options = list(hAxis = "{slantedText:true, slantedTextAngle:50 ,title : 'CountrySide', textStyle:{fontSize:6},showTextEvery:3}",series = "[{targerAxisIndex : 0},{targetAxisIndex : 1}]",title = paste("The relationship between ",input$CriminalType," and ", input$population),vAxes = paste("[{title:'",input$population,"'},{title:'",input$CriminalType,"'}]"),height = 300))})

    #Create the function that enable the relevant information show in popup
    showPopup <- function(suburb, lat, lng) {
        selectedZip <- myData[myData$suburb == suburb,]
        content <- as.character(tagList(
        tags$h4("Suburb:", selectedZip$suburb),tags$br(),
        sprintf("Assault: %s", selectedZip$Assault), tags$br(),
        sprintf("Burglary.Dwelling: %s", selectedZip$`Burglary.Dwelling.`), tags$br(),
        sprintf("Burglary.other: %s", selectedZip$`Burglary.other.`), tags$br(),
        sprintf("Graffiti: %s", selectedZip$Graffiti), tags$br(),
        sprintf("Robbery: %s", selectedZip$Robbery), tags$br(),
        sprintf("Steal Montor Vehicle: %s", selectedZip$`Steal.Montor.Vehicle`)
        ))
        leafletProxy("map") %>% addPopups(lng, lat, content, layerId = suburb)
    }

    #Provide the parameter and call the function showPopup that I created above, to fulfill the function
    observe({
        leafletProxy("map") %>% clearPopups()
        event <- input$map_shape_click
        if (is.null(event))
        return()
        
        isolate({
            showPopup(event$id, event$lat, event$lng)
        })
    })

    #Provide the dynamic data when year change
     yearData <- reactive({
        yearData <- data.frame(filter(dataByYear,year == input$year)[1],filter(dataByYear,year == input$year)[5],filter(dataByYear,year == input$year)[6],
        filter(dataByYear,year == input$year)[7],filter(dataByYear,year == input$year)[8],filter(dataByYear,year == input$year)[9],filter(dataByYear,year == input$year)[10])
          })
    #Provide the output for the page "Record vary on year"
    output$chart <- reactive({
        list(data = googleDataTable(yearData()), options = list(title = paste("Criminal record on", input$year,", Western Australia")))
    })
    
    #Draw pie chart
    reactivePieData <- reactive({
        pieData[,c("Year",input$PieYear)]
    })

    output$pieChart <- reactive({
    list(data = googleDataTable(reactivePieData()), options=list(legend = "{textStyle:{fontSize:8}}",
    slices="{4: {offset: 0.2}, 0: {offset: 0.3}}",title=paste('Different criminal types(PieChart) & Total criminal(BarChart) on', input$PieYear),pieSliceText='label', pieHole=0.2))
    })
})

