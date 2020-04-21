library(shiny)
if("package:googleCharts" %in% search()) detach("package:googleCharts", unload=TRUE)
column1 = column
library(googleCharts)
vars <- c(
"Assault" = "Assault",
"Burglary(Dwelling)" = "Burglary.Dwelling.",
"Burglary(Other)" = "Burglary.other.",
"Graffiti" = "Graffiti",
"Robbery" = "Robbery",
"Steal Montor Vehicle" = "Steal.Montor.Vehicle")

navbarPage("Criminal Data Visualization",
tabPanel("Record vary on year",
    h3("Criminal vary on year"),
        fluidPage(
            googleChartsInit(),
            googleLineChart("chart",width = "1500px",height = "500px",
                option = list(
                            hAxis = list( slantedText = TRUE,
                                          slanatedTextAngle = 50,
                                          title = "CountrySide",
                                          textStyle = list(fontSize = 8),showTextEvery = 2),
                            vAxis = list(textPosition = 'out',
                                          ticks = array(c(100,500,900),dim = c(3,1))),
                fontSize = 10,
                chartArea = list(left = 80, top = 70)))),

        fluidRow(align="center",
        sliderInput("year", "Year",
                    min = min(dataByYear$year,na.rm = TRUE), max = max(dataByYear$year,na.rm = TRUE),
                    value = min(dataByYear$year, na.rm = TRUE), animate = TRUE))),

tabPanel("Criminal Distribution On Map",
        div(class="outer",tags$head(
        # Include our custom CSS
        includeCSS("styles.css")),

        leafletOutput("map", width="100%", height="100%"),

        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, top = 60, left = 20, right = "auto", bottom = "auto",
                      width = "400px", height = "auto",
    
        h2(" Criminal explorer"),
        selectInput("color", "Color", vars),
        selectInput("size", "Size", vars, selected = "Assault"),

        googlePieChart("pieChart", width = "370px",height = "230px",option = list(chartArea = list(width = '100%',left = 10, top = 70))),
        plotlyOutput("plot",height = "200px"),
        sliderInput("PieYear", "Year",
                    min = 1999, max = 2016,
                    value = 1999, animate = TRUE)),
        tags$div(id="cite",'Data compiled for ', tags$em('Coming Apart: Australia total crime in 1997–2017'), ' by JianHua Tao (Monash Uni, 2017).'))),

tabPanel("Further Information",
        fluidPage(
            fluidRow(
                splitLayout(cellWidths = c("50%" , "50%"),
                htmlOutput('relationship',width = "1000px", height = "450px"),
                htmlOutput('bubble',width = "1000px", height = "450px"))),
                hr(),
            fluidRow(column1(4,
                     textOutput('text1')),
                     column1(4,offset = 3,
                     textOutput('text3'))),
                hr(),
            fluidRow(
                        column1(3,
                        h4("Criminal record with different elements"),
                        radioButtons("yearOfRe","Year",c(
                        "2006" = "2006",
                        "2011" = "2011"))),
                        column1(3,
                        selectInput("CriminalType", "Criminal Type", vars)),
                        column1(3,offset = 1,
                        selectInput("population","Population or Income",c(
                        "Population" = "Population",
                        "Male" = "Male",
                        "Female" = "Female",
                        "Median weekly household income" ="Median.weekly.household.income")))),
            fluidRow(
                        column1(10,"Note: All the absent data show in the graph is caused by the missing data in the source website."))


)))

