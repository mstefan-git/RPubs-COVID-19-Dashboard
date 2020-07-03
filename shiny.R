# packages
library(readxl)
library(geojsonio)
library(tidyverse)
library(viridis) 
library(RColorBrewer) 
library(leaflet)
library(shinydashboard)

# import data
covid <- read_excel("covid.xlsx")  # read excel file
covid <- covid[complete.cases(covid),]  # remove NAs
covid <- covid %>% 
  select(-day, -month, -year) %>%       # drop variables
  rename(                               # rename variables
    date = dateRep,
    country = countriesAndTerritories,
    code2 = geoId,
    code3 = countryterritoryCode
  )

# aggregate data by country
covid <- covid %>% 
  select(country,code3,cases,deaths,popData2019) %>%
  group_by(country) %>%
  summarise(code = code3[1],
            cases = sum(cases),
            deaths = sum(deaths),
            population = popData2019[1]
  )

# new variables
covid$fatality <- covid$deaths/covid$cases
covid$casesMill <- covid$cases/covid$population*1000000
covid$deathsMill <- covid$deaths/covid$population*1000000

# geo data
geo <- geojson_read("geo.json", what = "sp")

# add variables = 0
geo@data$cases <- 0
geo@data$deaths <- 0
geo@data$casesMill <- 0
geo@data$deathsMill <- 0
geo@data$fatality <- 0

# auxiliary function
'%!in%' <- function(x,y)!('%in%'(x,y))

# loop through each row and add covid data
for(i in 1:nrow(geo@data)){
  id = geo@data$id[i]
  if(id %!in% covid$code) next()
  geo@data$cases[i] <- covid$cases[covid$code == id]
  geo@data$deaths[i] <- covid$deaths[covid$code == id]
  geo@data$fatality[i] <- covid$fatality[covid$code == id]
  geo@data$casesMill[i] <- covid$casesMill[covid$code == id]
  geo@data$deathsMill[i] <- covid$deathsMill[covid$code == id]
}
rm(i, id)

# packages
library(shiny)
library(shinydashboard)
library(viridis)
library(RColorBrewer)

# ui
ui <- dashboardPage(
  
  # title
  dashboardHeader(title = "COVID-19 Dashboard"),
  
  # sidebar
  dashboardSidebar(
    
    selectInput("variable", "Variable:", 
                c("Fatality rate" = "fatality",
                  "Cases" = "cases", 
                  "Cases/Million" = "casesMill",
                  "Deaths" = "deaths",
                  "Deaths/Million" = "deathsMill"
                )
    ),
    
    selectInput("palette", "Color palette:",
                c("Magma",
                  "Red",
                  "Blue",
                  "Viridis")
    )
  ),
  
  # main body
  dashboardBody(
    leafletOutput("map")
  )
  
)

# server
server <- function(input, output){
  
  # function to make colors
  makeCol <- reactive({
    colorBin(
      if(input$palette == "Blue"){
        palette = brewer.pal(9, "Blues")
      } else if(input$palette == "Red"){
        palette = brewer.pal(9, "Reds")
      } else if(input$palette == "Magma"){
        palette = rev(magma(587))
      } else palette = rev(viridis(587)),
      domain = geo@data[,input$variable],
      bins = 20,
      pretty = T
    )
  })
  
  # make map
  observe({
    colors <- makeCol()
    map <- leaflet(geo) %>%
      addPolygons(
        stroke = F,
        smoothFactor = 0.5,
        fillOpacity = .85,
        color = ~colors(geo@data[,input$variable])
      ) %>%
      setView(lng = 20,
              lat = 30,
              zoom = 1
      )
    output$map <- renderLeaflet(map)
  })
  
}

# run shiny
shinyApp(ui, server, options = list(height = 500))
