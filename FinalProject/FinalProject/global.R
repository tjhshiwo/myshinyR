#Import the library to enable some functionalities.
library(googleVis)
library(ggplot2)
library(readxl)
library(leaflet)
library(plotly)

#Load the file from excel file
myData <- read_excel('criminal.xlsx')
dataByYear <- read_excel('dataByYear.xlsx')
pieData <- read_excel('pie.xlsx')
totalData <- read_excel('total.xlsx')
