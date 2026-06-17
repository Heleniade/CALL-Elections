library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(DT)
library(glue)
library(thematic)


CALL <- read_csv("data/Liste des communes de la CALL.csv")
Nuance <- read_csv("data/Nuances de liste.csv")
Contours_Communes <- read_sf("data/Contours des communes.geojson")
CALL_Spatial <- read_sf("data/Cartographie Spatiale de la CALL.geojson")

# Reprojection en WGS84 pour leaflet ####
{

  CALL_Spatial_4326 <- CALL_Spatial %>%
    st_transform(4326)
}

ui <- fluidPage(
  tags$style(HTML(".leaflet-container { background: #ffffff; }")),
  titlePanel("Carte électorale interactive — CALL"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable_elect", "Variable à afficher",
                  choices = c("Abstention" = "% Abstentions")) # à étendre plus tard avec les scores de nuances
    ),
    mainPanel(
      leafletOutput("carte_electorale", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  

  ####
  bbox <- st_bbox(CALL_Spatial_4326)
  
  pal <- reactive({
    colorNumeric(palette = c("white", "black"), domain = c(0, 100))
  })
  
  output$carte_electorale <- renderLeaflet({
    leaflet() %>%
      addMapPane("communes_pane", zIndex = 450) %>% 
      fitBounds(lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
                lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]) %>%
      addPolylines(
        data = distinct(Contours_Communes, `codeCommune`, geometry) %>%
          st_set_geometry("geometry"),
        color = "black", weight = 1.5, group = "communes",
        options = pathOptions(pane = "communes_pane")
      )
  })
  
  
  observe({
    proxy <- leafletProxy("carte_electorale") %>%
      clearGroup("choropleth") %>%
      addPolygons(
        data = CALL_Spatial_4326,
        fillColor = ~pal()(CALL_Spatial_4326[[input$variable_elect]]),
        fillOpacity = 1,
        color = "white", weight = 0.3, opacity = 0.4,
        group = "choropleth",
        label = ~paste0(`Libellé commune`, ", Bureau de vote n°", `Code BV`),
        popup = ~lapply(
          paste0("<b>", `Libellé commune`, "</b><br>",
                 "Bureau de vote n°", `Code BV`, "<br>",
                 input$variable_elect, " : ", round(CALL_Spatial_4326[[input$variable_elect]], 1), "%"),
          htmltools::HTML
        ),
        highlightOptions = highlightOptions(weight = 2, opacity = 1, color = "white", bringToFront = TRUE)
      )
    
    proxy %>%
      clearControls() %>%
      addLegend(
        position = "bottomright",
        pal = pal(),
        values = c(0,100),
        title = input$variable_elect,
        opacity = 0.8
      )
  })
}

shinyApp(ui, server)