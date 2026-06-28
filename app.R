library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(DT)
library(glue)
library(thematic)
library(readr)

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
                  choices = c("Abstention" = "% Abstentions",
                              "Liste Extrême-Gauche"= "Score_LEXG",
                              "Liste La France Insoumise"= "Score_LFI",
                              "Liste Parti Communiste Français"= "Score_LCOM",
                              "Liste Parti Socialiste"= "Score_LSOC",
                              "Liste Union de Gauche"= "Score_LUG",
                              "Liste Divers Gauche"= "Score_LDVG",
                              "Liste Les Ecologistes"= "Score_LECO",
                              "Liste Divers Droite"= "Score_LDVD",
                              "Liste Rassemblement National" = "Score_LRN",
                              "Liste Divers"= "Score_LDIV"))
    ),
    mainPanel(
      leafletOutput("carte_electorale", height = "700px")
    )
  )
)

labels_variables <- c(
  "% Abstentions" = "Abstention (%)",
  "Score_LEXG" = "Liste EXG (%)",
  "Score_LFI" = "Liste LFI (%)",
  "Score_LCOM" = "Liste PCF (%)",
  "Score_LSOC" = "Liste PS (%)",
  "Score_LUG" = "Liste UG (%)",
  "Score_LDVG" = "Liste DVG (%)",
  "Score_LECO" = "Liste ECO (%)",
  "Score_LDIV" = "Liste DIV (%)",
  "Score_LDVD" = "Liste DVD (%)",
  "Score_LRN"  = "Liste RN (%)"
)

server <- function(input, output, session) {
  
  bbox <- st_bbox(CALL_Spatial_4326)
  
  pal <- reactive({
    couleurs <- switch(input$variable_elect,
                       "% Abstentions" = c("white", "black"),
                       "Score_LEXG"     = c("white",  "red4"),
                       "Score_LFI"     = c("white",  "purple"),
                       "Score_LCOM"     = c("white",  "red"),
                       "Score_LSOC"     = c("white", "pink"),
                       "Score_LUG"     = c("white", "palevioletred1"),
                       "Score_LDVG"     = c("white", "lightsalmon"),
                       "Score_LECO"     = c("white", "green"),
                       "Score_LDVD"     = c("white", "blue"),
                       "Score_LRN"     = c("white", "saddlebrown"),
                       "Score_LDIV"     = c("white", "yellow")
    )
    colorNumeric(palette = couleurs, domain = c(0,100), na.color = "transparent")
  })
  
  output$carte_electorale <- renderLeaflet({
    leaflet() %>%
      addMapPane("communes_pane", zIndex = 450) %>%
      fitBounds(
        lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
        lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
      ) %>%
      addPolylines(
        data = distinct(Contours_Communes, codeCommune, geometry) %>%
          st_set_geometry("geometry"),
        color = "black", weight = 1.5, group = "communes",
        options = pathOptions(pane = "communes_pane")
      )
  })
  
  observeEvent(input$variable_elect, {
    leafletProxy("carte_electorale") %>%
      clearGroup("choropleth") %>%
      addPolygons(
        data = CALL_Spatial_4326,
        fillColor = ~pal()(CALL_Spatial_4326[[input$variable_elect]]),
        fillOpacity = 1,
        color = "white", weight = 0.3, opacity = 0.4,
        group = "choropleth",
        label = ~paste0(`Libellé commune`, ", Bureau de vote n°", `Code BV`),
        popup = ~lapply(
          paste0(
            "<b>", `Libellé commune`, "</b><br>",
            "Bureau de vote n°", `Code BV`, "<br>",
            labels_variables[input$variable_elect], " : ",
            round(CALL_Spatial_4326[[input$variable_elect]], 1), "%"
          ),
          htmltools::HTML
        ),
        highlightOptions = highlightOptions(
          weight = 2, opacity = 1, color = "white", bringToFront = TRUE
        )
      ) %>%
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