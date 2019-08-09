#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(leaflet)
library(plyr)
library(shiny)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(ggplot2)
library(ggthemes)
library(viridis)
library(scales)
library(RColorBrewer)

species_data <- read.csv("data/ESAIntrastateSpecies.csv")
shapeData <- readOGR("data/states_proj.shp")
ogrInfo("data/states_proj.shp")
mergestates <- merge(shapeData, species_data, by="NAME")


ui <- fluidPage(theme = "leaflet_adj.css",
  br(),
  column(9,leafletOutput("map", height="800px", width = "1800px")),
  column(3,textOutput("testHTML")),
  br(),
  sidebarLayout(
    sidebarPanel(
      br(),
      p("Click on each state to learn more about a threatened or endangered species found there!"),
      h1(textOutput("state")),
      br(),
      #imageOutput("spimg"),
      #br(),
      #em("Image credit:", textOutput("imgcred")),
      #br(),
      p(strong("Focal Species:"), (textOutput("focalsp"))), 
      em(textOutput("sciname")), 
      br(),
      p(strong("Species Listing Status:"), (textOutput("listing"))),
      br(),
      p(textOutput("text")),      
      width = 3), 
    mainPanel()
  ))


server <- function(input, output) {

  
  # create a reactive value that will store the click location
  observeEvent(input$map_shape_click,{
    click_loc <- input$map_shape_click
    lat <- click_loc$lat
    lng <- click_loc$lng
    coords <- data.frame(x = lng, y = lat)
    coords <- SpatialPoints(coords)
    proj4string(coords) <- proj4string(mergestates)
    polygon_select <- as.data.frame(over(coords, mergestates))
    state_select <- polygon_select$NAME
    focal_sp_select <- polygon_select$CommonName
    sci_name_select <- polygon_select$SciName
    listing_select <- polygon_select$Listing
    text_select <- polygon_select$text
    img_select <- polygon_select$Image
    imgcred_select <- polygon_select$Credits
    output$state <- renderText({paste(state_select)})
    output$focalsp <- renderText({paste(focal_sp_select)})
    output$sciname <- renderText({paste(sci_name_select)})
    output$listing <- renderText({paste(listing_select)})
    output$spimg <- renderImage({paste(src = img_select,
                                       width = 300,
                                       height = 300)})
    output$imgcred <- renderText({paste(imgcred_select)})
    output$text <- renderText({paste(text_select)})
    
  })
  
  # variables for map
  bins <- c(0, 5, 10, 25, 75, 150, 250, 350, 450, 550)
  col_pal <- colorBin("viridis", domain = mergestates$count, bins = bins)
  
  # popup layout
  pop_up_layout <- paste(sep = "<br/>",
                         paste0("<b>" ,mergestates$NAME, "</b>"),
                         paste0("<b>Number of ESA Listed Species: </b>", mergestates$count),
                         paste0("<b>" ,mergestates$CommonName, "</b>", " ", "<i>" ,mergestates$SciName, "</i>"),
                         paste0("<img style=max-height:200px;max-width:300px; src=", mergestates$Image, "' />"),
                         paste0("<i>Image Credit: </i>", mergestates$Credits),
                         paste0("<a href=" ,mergestates$Map, ">U.S. Fish and Wildlife Service Information</a>"))
  
  # Leaflet map with 2 markers
  output$map <- renderLeaflet({
    leaflet(mergestates)  %>% addTiles() %>% setView(lng = -87.251475, lat=39.835402,zoom=4) %>% 
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addPolygons(data=mergestates,weight=1, smoothFactor = 0.5, col="grey", fillColor = ~col_pal(count), 
                  popup = pop_up_layout,
                  fillOpacity = 0.75) %>% 
      addLegend(title = "Number of ESA-Listed Species", position = "topleft", 
                pal = col_pal, values = mergestates$count)
    
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)

