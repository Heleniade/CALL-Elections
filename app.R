library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(DT)
library(glue)
library(thematic)
library(readr)
library(ggplot2)

CALL <- read_csv("data/Liste des communes de la CALL.csv")
Nuance <- read_csv("data/Nuances de liste.csv")
Contours_Communes <- read_sf("data/Contours des communes.geojson")
CALL_Spatial <- read_sf("data/Cartographie Spatiale de la CALL.geojson")

# Reprojection en WGS84 pour leaflet ####
CALL_Spatial_4326 <- CALL_Spatial %>%
  st_transform(4326)

CALL_df <- st_drop_geometry(CALL_Spatial_4326)  # manquait

couleurs_variables <- list(  # manquait, utilisé dans l'histogramme
  "% Abstentions" = c("white", "black"),
  "Score_LEXG"    = c("white", "red4"),
  "Score_LFI"     = c("white", "purple"),
  "Score_LCOM"    = c("white", "red"),
  "Score_LSOC"    = c("white", "pink"),
  "Score_LUG"     = c("white", "palevioletred1"),
  "Score_LDVG"    = c("white", "lightsalmon"),
  "Score_LECO"    = c("white", "green"),
  "Score_LDVD"    = c("white", "blue"),
  "Score_LRN"     = c("white", "saddlebrown"),
  "Score_LDIV"    = c("white", "yellow")
)

ui <- fluidPage(
  tags$style(HTML("
    .leaflet-container { background: #ffffff; }
    #stats_barre { font-size: 13px; padding-top: 8px; }
  ")),
  titlePanel("Carte électorale interactive, Communauté d'Agglomération Lens-Liévin"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable_elect", "Variable à afficher",
                  choices = c(
                    "Abstention"                  = "% Abstentions",
                    "Liste Extrême-Gauche"        = "Score_LEXG",
                    "Liste La France Insoumise"   = "Score_LFI",
                    "Liste Parti Communiste"       = "Score_LCOM",
                    "Liste Parti Socialiste"       = "Score_LSOC",
                    "Liste Union de Gauche"        = "Score_LUG",
                    "Liste Divers Gauche"          = "Score_LDVG",
                    "Liste Les Ecologistes"        = "Score_LECO",
                    "Liste Divers Droite"          = "Score_LDVD",
                    "Liste Rassemblement National" = "Score_LRN",
                    "Liste Divers"                 = "Score_LDIV"
                  ))
    ),
    mainPanel(
      leafletOutput("carte_electorale", height = "500px"),
      fluidRow(
        column(8,
               plotOutput("histogramme", height = "250px")  # click = "hist_click" supprimé
        ),
        column(4,
               uiOutput("stats_barre")
        )
      )
    )
  )
)

labels_variables <- c(
  "% Abstentions" = "Abstention (%)",
  "Score_LEXG"    = "Liste EXG (%)",
  "Score_LFI"     = "Liste LFI (%)",
  "Score_LCOM"    = "Liste PCF (%)",
  "Score_LSOC"    = "Liste PS (%)",
  "Score_LUG"     = "Liste UG (%)",
  "Score_LDVG"    = "Liste DVG (%)",
  "Score_LECO"    = "Liste ECO (%)",
  "Score_LDIV"    = "Liste DIV (%)",
  "Score_LDVD"    = "Liste DVD (%)",
  "Score_LRN"     = "Liste RN (%)"
)

server <- function(input, output, session) {
  
  bbox            <- st_bbox(CALL_Spatial_4326)
  commune_cliquee <- reactiveVal(NULL)  # manquait
  
  pal <- reactive({
    couleurs <- switch(input$variable_elect,
                       "% Abstentions" = c("white", "black"),
                       "Score_LEXG"    = c("white", "red4"),
                       "Score_LFI"     = c("white", "purple"),
                       "Score_LCOM"    = c("white", "red"),
                       "Score_LSOC"    = c("white", "pink"),
                       "Score_LUG"     = c("white", "palevioletred1"),
                       "Score_LDVG"    = c("white", "lightsalmon"),
                       "Score_LECO"    = c("white", "green"),
                       "Score_LDVD"    = c("white", "blue"),
                       "Score_LRN"     = c("white", "saddlebrown"),
                       "Score_LDIV"    = c("white", "yellow")
    )
    colorNumeric(palette = couleurs, domain = c(0, 100), na.color = "transparent")
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
    commune_cliquee(NULL)  # reset histogramme au changement de variable
    
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
        title = as.character(labels_variables[input$variable_elect]),
        opacity = 0.8
      )
  })
  bv_clique <- reactiveVal(NULL)
  
  observeEvent(input$carte_electorale_shape_click, {
    click <- input$carte_electorale_shape_click
    
    pt  <- st_point(c(click$lng, click$lat)) %>% st_sfc(crs = 4326)
    idx <- st_within(pt, CALL_Spatial_4326, sparse = FALSE)
    bv  <- CALL_Spatial_4326[which(idx), ]
    
    if (nrow(bv) > 0) {
      commune_cliquee(bv$`Libellé commune`[1])
      bv_clique(bv$`Code BV`[1])  # ← stocke le BV cliqué
    }
  })
  
  
  output$histogramme <- renderPlot({
    var     <- input$variable_elect
    commune <- commune_cliquee()
    
    if (is.null(commune)) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Cliquez sur un bureau de vote",
                   size = 5, color = "grey60") +
          theme_void()
      )
    }
    
    couleur_barre <- tail(couleurs_variables[[var]], 1)
    df_commune    <- CALL_df %>% filter(`Libellé commune` == commune)
    moy           <- round(mean(df_commune[[var]], na.rm = TRUE), 1)
    
    ggplot(df_commune, aes(x = `Code BV`, y = .data[[var]],
                           alpha = `Code BV` == bv_clique())) +
      scale_y_continuous(limits = c(0, 100)) +
      geom_col(fill = couleur_barre, color = "white") +
      scale_alpha_manual(values = c("TRUE" = 0.3, "FALSE" = 1),
                         guide = "none") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(
        x     = "Bureau de vote",
        y     = unname(labels_variables[var]),
        title = paste0(commune, ",", nrow(df_commune), " BV, moy. ", moy, "%")
      )
  })
}

shinyApp(ui, server)