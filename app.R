library(tidyverse)
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(raster)
library(leaflet)
library(leaflet.extras)
library(leaflegend)
library(scales)
library(plotly)
library(DT)
library(lubridate)

source("path.R")
load("data.RData")

max_date <- as.Date(max(dta2$Date))
report_start <- max_date - 6
report_end <- max_date
report_start_fmt <- format_report_date(report_start)
report_end_fmt <- format_report_date(report_end)
# report_end_fmt <- format_report_date(as.Date("2026-06-29"))
forecast_end <- lubridate::mdy(unique(forecast$`Forecast End Date`))
forecast_start <- forecast_end - 6
forecast_start_fmt <- format_report_date(forecast_start)
forecast_end_fmt <- format_report_date(forecast_end)

shinyApp(
  
  ui = shinydashboardPlus::dashboardPage(
    options = list(sidebarExpandOnHover = FALSE),
    header = shinydashboardPlus::dashboardHeader(titleWidth = 0, disable = TRUE),
    
    sidebar = shinydashboardPlus::dashboardSidebar(width = "0px"),
    
    body = shinydashboard::dashboardBody(
      
      tags$head(
        tags$style(HTML('/* logo */
                         .skin-blue .main-header .logo {
                         background-color: #23769a;
                         }
                         /* logo when hovered */
                         .skin-blue .main-header .logo:hover {
                         background-color: #23769a;
                         }
                         /* navbar (rest of the header) */
                         .skin-blue .main-header .navbar {
                         background-color: #23769a;
                         }
                         /* main sidebar */
                         .skin-blue .main-sidebar {
                         background-color: #23769a;
                         }
                         .main-sidebar {
                         font-size: 20px;
                         }
                         /* active selected tab in the sidebarmenu */
                         .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                         background-color: #23769a;
                         }
                         /* other links in the sidebarmenu */
                         .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                         background-color: #23769a;
                         color: white;
                         }
                         /* other links in the sidebarmenu when hovered */
                         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                         background-color: #23769a;
                         }
                         /* toggle button when hovered  */
                         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                         background-color: #23769a;
                         }
                         /* body */
                         .content-wrapper, .right-side {
                         background-color: white;
                         }
                         /* box */
                         .box{
                         -webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;
                         }
                         .box-body {
                         padding-left: 10px;
                         padding-right: 10px;
                         }
                         /* sidebar */
                         .sidebar {
                         padding-top: 100px;
                         }
                         /* pickerinput_waterbody */
                         .selectpicker {
                         z-index:99999 !important;
                         }
                         /* datepicker */
                         .datepicker {
                         z-index:99999 !important;
                         }
                         #caption {
                         font-size: 18px;
                         }
                         a {
                         color: #0000FF;
                         }
                         /* Adjust the percentage as needed */
                         .image-zoom {
                         zoom: 80%; 
                         }
                         .shiny-notification {
                         position: fixed;
                         top: 40%;
                         left: 50%;
                         transform: translate(-50%, -50%);
                         width: 400px;
                         padding: 20px;
                         font-size: 20px;
                         text-align: center;    }
                         '))
      ),
      
      # _ Header ----
      shinydashboard::box(
        width = 12,
        solidHeader = TRUE,
        
        tags$img(src = "DEQ-logo-color-horizontal370x73.png"),
        tags$div(span("Satellite Estimates of Cyanobacteria in Oregon Lakes and Reservoirs",
                      style = "color: black; font-size: 40px; font-weight:bold")),
        
        tags$h3("Reporting Period: ", report_start_fmt, " - ", report_end_fmt)
        
      ), # Header END
      
      # _ 1. Introduction ----
      shinydashboardPlus::box(
        width = 12,
        title = "Introduction",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        collapsed = FALSE,
        
        h4("This report presents recent and historical estimates of chlorophyll a concentrations for 49 large waterbodies in Oregon. ",
           "These estimates are derived from satellite imagery provided by the  ", 
           a("Cyanobacteria Assessment Network (CyAN)", href="https://www.epa.gov/water-research/cyanobacteria-assessment-network-cyan",target="_blank"),
           " project. The Oregon DEQ updates the estimates weekly from spring through fall each year. ",
           # "The current report presents Version 6 (V6) data reprocessed by NASA in February 2025. ",
           # "The V6 dataset includes updated calibration for Sentinel 3A and 3B, an enhanced filter for turbid water, and atmospheric correction for water vapor. ",
           # "Additional information about the V6 dataset can be found on the ",
           # a("NASA Ocean Color website", href="https://oceancolor.gsfc.nasa.gov/data/reprocessing/projects/cyan/version/6/",.noWS = "outside",target="_blank"),
           # ". ",
           "This report also includes available field measurements collected by the Oregon DEQ and other entities, ",
           "as well as recreational health advisories for cyanobacterial bloom issued by the Oregon Health Authority.",
           .noWS = c("after-begin", "before-end")),
        
        h4("Concentrations of chlorophyll a (μg/L) associated with cyanobacteria dominance are shown at three levels: Low: 3-12 μg/L, Moderate: 12-24 μg/L, and High: >24 μg/L. ",
           "These levels correspond to the World Health Organization (WHO) exposure guideline values for recreational waters ",
           "(",a("WHO, 2021", href="https://www.who.int/publications/m/item/toxic-cyanobacteria-in-water---second-edition",.noWS = "outside",target="_blank"),"). ",
           "Also included are ",
           a("EPA’s seven-day forecasts", href="https://www.epa.gov/water-research/cyanobacterial-harmful-algal-blooms-forecasting-research",.noWS = "outside",target="_blank"), 
           " from the experimental cyanoHAB forecasting model based on CyAN satellite data. ",
           "The model provides weekly probabilities that the median surface chlorophyll a concentration is ≥12 µg/L. ",
           "Higher probabilities indicate greater likelihoods of bloom occurrence. ",
           # tags$i("EPA has currently suspended the update of the forecasts. DEQ will resume reporting the forecasts once data become available. "),
           "For more information on harmful algal blooms in Oregon, visit the ",
           a("Oregon DEQ", href="https://www.oregon.gov/deq/wq/Pages/Harmful-Algal-Blooms.aspx",target="_blank")," and ",
           a("Oregon Health Authority", href="https://www.oregon.gov/oha/ph/healthyenvironments/recreation/harmfulalgaeblooms/pages/blue-greenalgaeadvisories.aspx",.noWS = "outside",target="_blank"),
           " websites.",
           .noWS = c("after-begin", "before-end")),
        
        h4("All data presented in this report are provisional and subject to change. Satellite-derived estimates do not ",
           "confirm the presence of cyanotoxins or other water quality impairments and do not have regulatory implications. ",
           tags$b("Visit the ",
                  a("Oregon Health Authority", href="https://www.oregon.gov/oha/ph/healthyenvironments/recreation/harmfulalgaeblooms/pages/blue-greenalgaeadvisories.aspx",.noWS = "outside",target="_blank"),
                  " to learn about recreational use and drinking water advisories related to cyanobacteria blooms. "),
           "Additional assessments using ",
           a("Sentinel 2", href="https://rs-algal-blooms.users.earthengine.app/view/idaho#lon=-120.94342697507778;lat=44.24513598650389;zoom=7",
             target="_blank"),
           "imagery, local visual assessments, and/or water quality sampling are needed to provide further information on potential human health ",
           "and environmental effects of cyanobacteria. Factors such as cloud cover, ice, sun glint, water surface roughness, dry lake beds, algal mats, and shoreline effects can interfere with satellite imagery and estimation accuracy.",
           .noWS = c("after-begin", "before-end")),
        
        h4("DISCLAIMER: Information is preliminary. Additional data are needed to confirm the presence of cyanobacteria blooms.",
           .noWS = c("after-begin", "before-end"))
        
      ), # Introduction End
      
      # _ 2. Highlighted waterbodies ----
      shinydashboardPlus::box(
        width = 12,
        title = "Highlighted Waterbodies",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        collapsed = FALSE,
        
        # ___ Section Introduction ----
        tags$h4(p(tags$strong(report_start_fmt), " - ", tags$strong(report_end_fmt), "- ",
                  "Waterbodies with high chlorophyll a concentration (≥24 μg/L) are identified based on ",
                  "the maximum value of the weekly means of daily maximums (",tags$strong("'Weekly Mean Daily Max'"),").",
                  "The weekly median of daily maximums (",tags$strong("'Weekly Median Daily Max'"),") is also reported for each highlighted waterbody. ",
                  "Both metrics represent 7-day moving averages calculated using daily maximum values from ",
                  "the most recent available data date and the preceding six days. ",
                  "The", tags$strong("'Days of Data'"), "field indicates the number of valid observation days within ",
                  "each 7-day window used for computing both Weekly Mean Daily Max and Weekly Median Daily Max.",
                  "The", tags$strong("'Date of Daily Max'"), "indicates the date on which the daily maximum value occurred.")),
        
        tags$h4(p(tags$strong(forecast_start_fmt), " - ", tags$strong(forecast_end_fmt), " - ",
                  "Modeled probabilities of chlorophyll a concentrations ≥12 µg/L are shown in the '",tags$strong("% Chance of CyanoHAB"), "' column.",
                  "These probabilities are presented for all highlighted waterbodies and for any other waterbodies ",
                  "where the modeled probabilities are ≥50%.")),
        
        # ___ 7-Day Table ----
        shinydashboard::box(
          width = 6,
          solidHeader = TRUE,
          
          shinycssloaders::withSpinner(DT::dataTableOutput("tbl7dadm")),
          
          tags$br(),
          tags$em("*GNISID: ",a("USGS Geographic Names Information System Identifier", 
                                href="https://www.usgs.gov/faqs/what-geographic-names-information-system-gnis",
                                .noWS = "outside",
                                target="_blank"),
                  .noWS = c("after-begin", "before-end"))
          
        ),
        
        # ___ 7-Day Map ----
        shinydashboard::box(
          width = 6,
          solidHeader = TRUE,
          
          tags$img(src = "map_7d.jpg", width = "100%")
          
        )
        
      ), # Highlighted Waterbodies End
      
      # _ 3. Satellite imagery  ----
      shinydashboardPlus::box(
        width = 12,
        title = "Satellite Imagery",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        collapsed = FALSE,
        
        # ___ Section Introduction ----
        tags$h4(p("The interactive map provides satellite imagery for 49 Oregon waterbodies from April 1, 2026 to the present.")),
        
        shinydashboard::box(
          width = 3,
          #title = "left",
          solidHeader = TRUE,
          
          
          # ___ Select a Waterbody ----
          tags$hr(),
          tags$h4(p("Select a waterbody to zoom in on its location on the map. ",
                    "Once selected, information will be displayed indicating whether the waterbody is used for recreation or as a public drinking water source.")),
          
          selectInput(inputId = "waterbody", 
                      label = tags$h4(strong("Select a Waterbody:")),
                      choices = c("Oregon", sort(as.character(unique(lakes.resolvable$GNISIDNAME)))),
                      selected = "Oregon",
                      selectize = FALSE,
                      size = 10),
          # ___ Drinking Water Area ----
          shiny::textOutput("dw"),
          
          tags$br(),
          tags$hr(),
          
          # ___ Select a Date ----
          tags$h4(p(paste0("Select a date to update the map with satellite imagery from that day. Imagery is available from April 1, 2026 to ", report_end_fmt, "."))),
          
          shiny::dateInput(inputId = "date_map",
                           label = tags$h4(strong("Select a Date:")),
                           value = as.Date(max(dta2$Date)),
                           min = as.Date("2026-04-01"),
                           max = as.Date(max(dta2$Date)),
                           format = "yyyy-mm-dd",
                           startview = "month",
                           weekstart = 0,
                           datesdisabled = missing.dates$Date),
          
          tags$br(),
          tags$hr(),
          
          # ___ Select Layers ----
          tags$h4(p("Select layers to display satellite imagery (selected by default), forecasting data, HABs monitoring stations, ",
                    "and/or watersheds delineated using the USGS 6-digit Hydrologic Unit Code (HUC6).")),
          
          tags$h4(strong("Select Layers:")),
          tags$br(),
          
          div(style = "font-size: 16px;",
              shiny::checkboxGroupInput(
                inputId = "map_layers",
                label = NULL,
                choices = c("Satellite Imagery (Selected Date)", 
                            "Forecast: % chance of Chl-a ≥12 μg/L",
                            "Monitoring Stations", 
                            "Basins (HUC6)"),
                selected = "Satellite Imagery (Selected Date)"
              )
          )
          
        ),
        
        # ___ Interactive Map ----
        shinydashboard::box(
          width = 9,
          #title = "right",
          solidHeader = TRUE,
          
          shinycssloaders::withSpinner(leaflet::leafletOutput("map", height = "900px"))
          
        )
        
      ),
      
      # _ 4. Time series data ----
      shinydashboardPlus::box(
        width = 12,
        height = "100%",
        title = "Time Series Data",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        collapsed = FALSE,
        
        # ___ Section Introduction ----
        tags$h4(p("Time series plots for each of the 49 Oregon waterbodies display satellite estimates of chlorophyll a concentrations and ",
                  "available field measurements. Satellite estimates follow methods established by the ",
                  a("CyAN Project", href="https://www.epa.gov/water-research/cyanobacteria-assessment-network-cyan",.noWS = "outside",target="_blank"),", ",
                  " including data from Sentinel-3A (2016-present) and Sentinel-3B (2018-present). ",
                  "Field measurements include chlorophyll a and cyanotoxin concentrations. ",
                  .noWS = c("after-begin", "before-end"))),
        
        # ___ Plot and Table ----
        shinydashboard::box(
          width = 12,
          #title = "plot+table",
          solidHeader = TRUE,
          
          shinydashboard::box(
            width = 3,
            #title = "left",
            solidHeader = FALSE,
            
            # ____ Select a Waterbody 2 ----
            selectInput(inputId = "waterbody2", 
                        label = tags$h4(strong("Select a Waterbody:")),
                        choices = c("Oregon", sort(as.character(unique(lakes.resolvable$GNISIDNAME)))),
                        selected = "Oregon",
                        selectize = FALSE,
                        size = 10),
            
            # ____ Date range ----
            shiny::radioButtons(
              inputId = "ploty",
              label = tags$h4(strong("Date Range:")),
              choices = c("Current Year: 2026",
                          "Reset to Complete Data Range",
                          "Select a Date Range"),
              selected = "Current Year: 2026"),
            
            shiny::dateRangeInput(inputId = "date_plot",
                                  label = "",
                                  start = min(dta$Date),
                                  end = max(dta$Date),
                                  min = min(dta$Date),
                                  max = max(dta$Date),
                                  separator = "to",
                                  format = "yyyy-mm-dd",
                                  startview = "year",
                                  weekstart = 0),
            
            uiOutput("dataDate"),
            
            tags$br(),
            tags$hr(),
            
            # ____ Parameters ----
            tagList(
              tags$h4(strong("Parameters:")),
              
              tags$hr(style = "margin-top: 5px; margin-bottom: 5px; border: none;"),
              
              # Checkbox group
              tagList(
                tags$h5(tags$i(tags$strong("CyAN Chlorophyll a Estimates:"))),
                tags$hr(style = "margin-top: 5px; margin-bottom: 5px; border: none;"),
                checkboxGroupInput(
                  inputId = "matrix_cyan",
                  label = NULL,
                  choices = c(
                    "Weekly Mean Daily Max" = "Weekly Mean Daily Max",
                    "Weekly Median Daily Max" = "Weekly Median Daily Max",
                    "Daily Maximum" = "Daily Maximum",
                    "Daily Mean" = "Daily Mean"
                  ),
                  selected = c("Weekly Mean Daily Max", "Daily Maximum")
                ),
                
                tags$h5(tags$i(tags$strong("Field data:"))),
                tags$hr(style = "margin-top: 5px; margin-bottom: 5px; border: none;"),
                checkboxGroupInput(
                  inputId = "matrix_field",
                  label = NULL,
                  choices = c(
                    "Chlorophyll a" = "Chlorophyll a",
                    "Anatoxin-A" = "Anatoxin-A",
                    "Cylindrospermopsin" = "Cylindrospermopsin",
                    "Microcystins" = "Microcystins",
                    "Saxitoxin" = "Saxitoxin"
                    # "Pheophytin-a" = "Pheophytin a"
                  ),
                  selected = c("Anatoxin-A", "Cylindrospermopsin","Microcystins","Saxitoxin")
                )
              ),
              
              tags$br(),
              
              # Check/Clear all on Checkboxs
              fluidRow(
                column(4, actionButton("select_all", "Check All", icon = icon("check-square"))),
                column(8, actionButton("clear_all", "Clear All", icon = icon("square")))
              )
              
            ),
            
            tags$br(),
            tags$br(),
            
            # ____ Plot types ----
            #   checkboxGroupInput(
            #     inputId = "plot_log",
            #     label = tags$h4(strong("y-axis:")),
            #     choices = c("Log Scale" = "log"))
            #   
            # ),
            
            checkboxGroupInput(
              inputId = "plot_log",
              label = tags$h4(strong(""))),
            
            # ____ Advisory ----
            tags$hr(),
            
            tags$div("Show or hide OHA advisory periods on the plot. Hover over the upward triangle to see advisory dates and associated cyanotoxins.",
                     style = "font-size: 1.2em; line-height: 1.2em;"),
            
            tags$h4(strong("OHA Advisories:")),
            
            shinyWidgets::switchInput(
              inputId = "advisory_bars", 
              label = "", 
              value = TRUE, 
              onLabel = "Show", 
              offLabel = "Hide", 
              size = "normal")
            
          ),
          
          shinydashboard::box(
            width = 9,
            #title = "right",
            solidHeader = FALSE,
            
            # ____ Time series plot ----
            tags$h4(p(strong("Chlorophyll a estimates from CyAN and field data (if available)."))),
            
            textOutput("no_plot_1"),
            
            plotlyOutput("plot_chl"),
            
            tags$br(),
            
            tags$h4(p(strong("Cyanotoxin data from field data."))),
            
            textOutput("no_plot_2"),
            
            plotlyOutput("plot_toxin"),
            
            tags$br(),
            tags$br(),
            
            uiOutput("who_line"),
            
            tags$br(),
            tags$br(),
            
            # ____ Data table ----
            shinydashboard::box(
              width = 12,
              #title = "right",
              solidHeader = FALSE,
              
              tags$h4(p(strong(("Time series data of the selected waterbody during the selected date range.")))),
              
              textOutput("no_data"),
              
              tags$br(),
              uiOutput("caption"),
              
              DT::dataTableOutput("table")
              
            )
            
          ),
          
          # _ 5. Copyright and Contacts ----
          shinydashboard::box(
            width = 12,
            #title = "copyright",
            solidHeader = FALSE,
            
            h4("The report is provided by the Oregon DEQ Watershed Management Section. Copyright (C) 2020-2026, Oregon DEQ."),
            h4("The source code of this report is publicly available at GitHub repository: ", 
               a("Satellite Estimates of Cyanobacteria in Oregon Lakes and Reservoirs",
                 href="https://github.com/OR-Dept-Environmental-Quality/CyAN_imagery_update",.noWS = "outside",target="_blank"),".",
               .noWS = c("after-begin", "before-end")),
            h4("For more information on this report, please contact"),
            h4("Daniel Sobota (Lead), ", a("daniel.sobota@deq.oregon.gov",href="mailto:dan.sobota@deq.oregon.gov",target="_blank")),
            h4("Yuan Grund, ", a("yuan.grund@deq.oregon.gov",href="mailto:yuan.grund@deq.oregon.gov",target="_blank"))
            
          )
          
        )
        
      )
      
    ) # Body End
    
  ), # ui End
  
  server = function(input, output, session) {
    
    # 1. Maps ----
    progress <- reactiveValues(value = 0)
    # _ initial map ----
    output$map <- leaflet::renderLeaflet({
      
      leaflet::leaflet() %>% 
        leaflet::addMapPane("OpenStreetMap", zIndex = -40) %>% 
        leaflet::addMapPane("National Geographic World Map", zIndex = -40) %>%
        leaflet::addMapPane("state.boundary", zIndex = -30) %>%
        leaflet::addMapPane("HUC6",zIndex = -20) %>% 
        leaflet::addMapPane("lakes.resolvable", zIndex = 400) %>%
        leaflet::addMapPane("stations", zIndex = 600) %>%
        leaflet::addMapPane("forecasting", zIndex = 600) %>%
        leaflet::addProviderTiles("OpenStreetMap",group = "OpenStreetMap",
                                  options = leaflet::pathOptions(pane = "OpenStreetMap")) %>% 
        leaflet::addProviderTiles(providers$Esri.NatGeoWorldMap,group = "National Geographic World Map",
                                  options = leaflet::pathOptions(pane = "National Geographic World Map")) %>% 
        leaflet::setView(lng = -120, lat = 44, zoom=7) %>%
        leaflet.extras::addResetMapButton() %>% 
        leaflet::addScaleBar(position = c("bottomright"),
                             options = leaflet::scaleBarOptions()) %>% 
        leaflet::addMiniMap(position = "bottomright",
                            width = 180,
                            height = 200,
                            zoomLevelFixed = 5) %>% 
        leaflet::addPolygons(data = lakes.resolvable, 
                             color = "blue",
                             weight = 2,
                             layer = ~lakes.resolvable$GNISIDNAME,
                             smoothFactor = 0.5,
                             opacity = 0.5,
                             fillColor = "transparent",
                             fillOpacity = 1.0,
                             label = ~lakes.resolvable$GNIS_Name,
                             labelOptions = leaflet::labelOptions(style = list("font-size" = "18px",
                                                                               "color" = "blue")),
                             options = leaflet::pathOptions(pane = "lakes.resolvable"),
                             group = "lakes.resolvable") %>% 
        leaflet::addPolygons(data = huc6, 
                             group = "Basins (HUC6)",
                             color = "grey",
                             weight = 2,
                             smoothFactor = 0.5,
                             opacity = 0.5,
                             fillColor = ~pal.huc6(HU_6_NAME),
                             fillOpacity = 0.2,
                             label = ~huc6$HU_6_NAME,
                             labelOptions = leaflet::labelOptions(noHide = TRUE,
                                                                  textOnly = TRUE,
                                                                  style = list("font-size" = "12px",
                                                                               "color" = "black")),
                             options = leaflet::pathOptions(pane = "HUC6")) %>% 
        leaflet::addPolygons(data = state.boundary, 
                             color = "black",
                             weight = 2,
                             fillColor = "transparent",
                             fillOpacity = 1.0,
                             options = leaflet::pathOptions(pane = "state.boundary")) %>% 
        leaflet::addCircleMarkers(data = field_stations,
                                  group = "Monitoring Stations",
                                  clusterOptions = leaflet::markerClusterOptions(),
                                  ~Long_DD, ~Lat_DD,
                                  popup = ~paste0(
                                    "<div style='font-size:14px;'>",
                                    "<b>Station ID:</b> ", MLocID, "<br>",
                                    "<b>Station:</b> ", StationDes, "<br>",
                                    "<b>Data counts:</b><br> <span style='white-space:nowrap;'>", CharData,"</span><br>",
                                    "</div>"),
                                  radius = 8,
                                  color = "blue",
                                  fillOpacity = 0.7,
                                  layerId = ~MLocID,
                                  options = leaflet::pathOptions(pane = "stations")) %>%
        leaflet::addCircleMarkers(data = forecast,
                                  group = "Forecast: % chance of Chl-a ≥12 μg/L",
                                  ~as.numeric(Longitude), ~as.numeric(Latitiude),
                                  radius = ~(`% Chance of CyanoHAB_map`/100)*50,
                                  color = ~ifelse(`% Chance of CyanoHAB_map` >= 50, "red", "orange"),
                                  fillOpacity = 0.7,
                                  layerId = ~GNIS_Name,
                                  popup = ~paste0(
                                    "<div style='font-size:14px;'>",
                                    "<b>",GNIS_Name,"</b>", "<br>",
                                    "Probability of Chl-a ≥12 μg/L: ", "<b>",`% Chance of CyanoHAB_map`, "%","</b><br>",
                                    "Date range of forecast: ", "<br>",
                                    "<b>",`Date Range of Forecast`,"</b><br>",
                                    "</div>"),
                                  options = leaflet::pathOptions(pane = "forecasting")) %>% 
        leaflet::addLayersControl(
          baseGroups = c("OpenStreetMap", "National Geographic World Map"),
          position = "topleft",
          options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)) %>% 
        leaflet.extras::addSearchFeatures(targetGroups = "lakes.resolvable",
                                          options = leaflet.extras::searchFeaturesOptions(openPopup = TRUE,
                                                                                          zoom = 8,
                                                                                          textPlaceholder = "Search a waterbody..."))
      
    })
    
    # _ map reactive @ layer selector ----
    observe({
      all_layers <- c("Satellite Imagery (Selected Date)", "Forecast: % chance of Chl-a ≥12 μg/L", "Monitoring Stations", "Basins (HUC6)")
      selected_layers <- input$map_layers %||% character(0)  # `%||%` ensures it's a character vector even if NULL
      
      layers_to_hide <- setdiff(all_layers, selected_layers)
      
      leafletProxy("map") %>%
        hideGroup(layers_to_hide) %>%
        showGroup(selected_layers)
    })
    
    # _ map reactive @ date selector ----
    df.map.date <- reactive({
      lookup.date %>% dplyr::filter(Date == as.Date(input$date_map))
    })
    
    map.tif.dir <- reactive({
      if (nrow(df.map.date()) == 0) return(NULL)
      paste0("./data/", df.map.date()$Year.dates, "/")
    })
    
    file.name <- reactive({
      if (nrow(df.map.date()) == 0) return(NULL)
      paste0(df.map.date()$CyAN_File_NUM, ".tif")
    })
    
    raster.path <- reactive({
      if (is.null(map.tif.dir()) || is.null(file.name())) return(NULL)
      paste0(map.tif.dir(), file.name())
    })
    
    rst <- reactive({
      if (!file.exists(raster.path())) return(NULL)
      r <- raster::raster(raster.path())
      raster::crs(r) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs"
      r
    })
    
    observeEvent(input$date_map, {
      progress$value <- 0
      withProgress(message = 'Updating data, please wait...', value = progress$value, {
        
        incProgress(0.3)
        if (is.null(rst())) {
          showNotification(paste("Raster not available for date:", input$date_map), type = "error")
          return(NULL)
        }
        
        incProgress(0.4)
        leafletProxy("map") %>%
          clearImages() %>%
          clearControls() %>%
          addRasterImage(rst(), layerId = "Value", project = FALSE, colors = pal.map, opacity = 1,
                         group = "Satellite Imagery (Selected Date)") %>%
          addLegend(pal = pal.map, values = thevalues, title = "Chlorophyll a (μg/L)", position = "topright",
                    labFormat = function(type, cuts, p) { paste0(labels) }, opacity = 1) %>%
          addLegend(position = "topright",
                    colors = c("orange", "red"),
                    labels = c("<50% chance", "≥50% chance"),
                    title = "Forecast<br><small>(Circle size ~ % chance)</small>",
                    opacity = 0.7) %>%
          addLayersControl(
            baseGroups = c("OpenStreetMap", "National Geographic World Map"),
            position = "topleft",
            options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)
          )
        incProgress(0.3)
      })
    })
    
    # _ map reactive @ waterbody picker ----
    selected_waterbody <- reactiveVal(NULL)
    
    observeEvent(input$waterbody,{
      
      if (input$waterbody == input$waterbody2) {
        
        if(input$waterbody == c("Oregon")) {
          
          leafletProxy("map") %>% 
            leaflet::setView(lng = -120, lat = 44, zoom=7)
          
        } else {
          
          one.lake <- reactive({
            
            lakes.resolvable[which(lakes.resolvable$GNISIDNAME == input$waterbody),]
            
          })
          
          bounds <- reactive({
            
            sf::st_bbox(one.lake())
            
          })
          
          leafletProxy("map") %>% 
            leaflet::fitBounds(lng1=bounds()[[1]], lat1=bounds()[[2]], lng2=bounds()[[3]], lat2=bounds()[[4]])
        }
        
      }else{
        
        selected_waterbody(input$waterbody)
        
        shiny::updateSelectInput(session, "waterbody2", selected = selected_waterbody())
        
        if(input$waterbody == c("Oregon")) {
          
          leafletProxy("map") %>% 
            leaflet::setView(lng = -120, lat = 44, zoom=7)
          
        } else {
          
          one.lake <- reactive({
            
            lakes.resolvable[which(lakes.resolvable$GNISIDNAME == input$waterbody),]
            
          })
          
          bounds <- reactive({
            
            sf::st_bbox(one.lake())
            
          })
          
          leafletProxy("map") %>% 
            leaflet::fitBounds(lng1=bounds()[[1]], lat1=bounds()[[2]], lng2=bounds()[[3]], lat2=bounds()[[4]])
        }
        
      }
      
    })
    
    observeEvent(input$waterbody2, {
      if (input$waterbody != input$waterbody2) {
        selected_waterbody(input$waterbody2)
        
        shiny::updateSelectInput(session, "waterbody", selected = selected_waterbody())
      }
    })
    
    # 2. Plots ----
    # _ Time series plot ----
    parameters <- c(
      "Weekly Mean Daily Max", "Weekly Median Daily Max", "Daily Maximum", "Daily Mean", 
      "Chlorophyll a", "Anatoxin-A", "Cylindrospermopsin", "Microcystins", "Saxitoxin", "Pheophytin a")
    
    parameter_colors <- c(
      "blue", "brown", "#17becf", "green", 
      "orange", "#d01c8b", "#f4a582", "#4dac26", "#b8e186", "gray")
    
    pal.plot <- setNames(parameter_colors, parameters)
    
    selected_matrix <- reactive({
      c(input$matrix_cyan, input$matrix_field)
    })
    
    observeEvent(input$select_all, {
      updateCheckboxGroupInput(
        session,
        inputId = "matrix_cyan",
        selected = c("Weekly Mean Daily Max", "Weekly Median Daily Max", "Daily Maximum", "Daily Mean")
      )
      updateCheckboxGroupInput(
        session,
        inputId = "matrix_field",
        selected = c("Chlorophyll a", "Anatoxin-A", "Cylindrospermopsin", "Microcystins", "Saxitoxin")
      )
    })
    
    observeEvent(input$clear_all, {
      updateCheckboxGroupInput(session, inputId = "matrix_cyan", selected = character(0))
      updateCheckboxGroupInput(session, inputId = "matrix_field", selected = character(0))
    })
    
    yr <- reactive({ 
      
      if(input$ploty == "Current Year: 2026"){"2026"}else{sort(unique(dta$Year))}
      
    })
    
    type <- reactive({
      
      input$plot_log
      
    })
    
    yaxis <- reactive({
      
      if_else(length(input$plot_log)>0,
              "Concentration (μg/L)",
              "Concentration (μg/L)")
      
    })
    
    # plot data
    
    df_temp <- reactive({
      
      if(input$ploty == "Current Year: 2026"){
        
        dta %>%
          dplyr::filter(GNISIDNAME %in% input$waterbody) %>% 
          dplyr::filter(Year %in% c(yr())) 
        
      } else if (input$ploty == "Reset to Complete Data Range") {
        
        dta %>% 
          dplyr::filter(GNISIDNAME %in% input$waterbody) 
        
      } else {
        
        dta %>% 
          dplyr::filter(GNISIDNAME %in% input$waterbody) %>% 
          dplyr::filter(Date >= input$date_plot[1],Date <= input$date_plot[2])
        
      }
      
    })
    
    df <- reactive({
      
      if (length(selected_matrix()) == 0) {
        return(NULL)
      }
      
      if(input$ploty == "Current Year: 2026"){
        
        dta %>%
          dplyr::filter(GNISIDNAME %in% input$waterbody) %>% 
          dplyr::filter(Year %in% c(yr())) %>% 
          dplyr::filter(Parameter %in% selected_matrix()) %>% 
          dplyr::mutate(Value = round(Value,2))
        
      } else if (input$ploty == "Reset to Complete Data Range") {
        
        dta %>% 
          dplyr::filter(GNISIDNAME %in% input$waterbody) %>% 
          dplyr::filter(Parameter %in% selected_matrix()) %>%
          dplyr::mutate(Value = round(Value,2))
        
      } else {
        
        dta %>% 
          dplyr::filter(GNISIDNAME %in% input$waterbody) %>% 
          dplyr::filter(Date >= input$date_plot[1],Date <= input$date_plot[2]) %>% 
          dplyr::filter(Parameter %in% selected_matrix()) %>% 
          dplyr::mutate(Value = round(Value,2))
        
      }
      
    })
    
    # Advisories
    advisory_shapes_chl <- reactive({
      
      if (is.null(df()) || nrow(df()) == 0) return(NULL)
      
      advisories %>%
        dplyr::filter(GNIS_Name_ID == input$waterbody) %>%
        dplyr::filter(Issued <= max(df_temp()$Date), Lifted >= min(df_temp()$Date)) %>%
        # tidyr::drop_na() %>% 
        purrr::pmap(function(Issued, Lifted, ...) {
          list(
            type = "rect",
            x0 = as.Date(Issued),
            x1 = as.Date(Lifted),
            y0 = 0.5,
            y1 = max(max(df()$Value, na.rm = TRUE), 25),
            fillcolor = "red",
            line = list(color = "red"),
            opacity = 0.2
          )
        })
    })
    
    advisory_hover_markers_chl <- reactive({
      
      if (is.null(df()) || nrow(df()) == 0) return(NULL)
      
      advisories %>%
        dplyr::filter(GNIS_Name_ID == input$waterbody) %>% 
        dplyr::filter(Issued <= max(df_temp()$Date), Lifted >= min(df_temp()$Date)) %>%
        # tidyr::drop_na() %>%
        dplyr::mutate(
          x = as.Date(Lifted),
          y = max(max(df()$Value, na.rm = TRUE), 25) * 1.05,
          label = as.character(paste0(
            "<span style='color:black;'>",
            "<b>Advisory</b><br>",
            "Issued: ", format(Issued, "%b %d, %Y"), "<br>",
            "Lifted: ", Lifted_label, "<br>",
            "`", `Dominant genus/toxin`, "`: ", `Cell Count/Toxin`, " ", Unit,
            "</span>"
          ))
        )
    })
    
    advisory_shapes_toxins <- reactive({
      
      if (is.null(df()) || nrow(df()) == 0) return(NULL)
      
      advisories %>%
        dplyr::filter(GNIS_Name_ID == input$waterbody) %>%
        dplyr::filter(Issued <= max(df_temp()$Date), Lifted >= min(df_temp()$Date)) %>%
        purrr::pmap(function(Issued, Lifted, ...) {
          list(
            type = "rect",
            x0 = as.Date(Issued),
            x1 = as.Date(Lifted),
            y0 = 0.5,
            y1 = max(
              max((df() %>% dplyr::filter(Parameter %in% c("Anatoxin-A", "Cylindrospermopsin", "Microcystins", "Saxitoxin")))$Value, na.rm = TRUE),
              16),
            fillcolor = "red",
            line = list(color = "red"),
            opacity = 0.2
          )
        })
    })
    
    advisory_hover_markers_toxins <- reactive({
      
      if (is.null(df()) || nrow(df()) == 0) return(NULL)
      
      advisories %>%
        dplyr::filter(GNIS_Name_ID == input$waterbody) %>% 
        dplyr::filter(Issued <= max(df_temp()$Date), Lifted >= min(df_temp()$Date)) %>%
        dplyr::mutate(
          x = as.Date(Lifted),
          y = max(
            max((df() %>% dplyr::filter(Parameter %in% c("Anatoxin-A", "Cylindrospermopsin", "Microcystins", "Saxitoxin")))$Value, na.rm = TRUE),
            16) * 1.05,
          label = as.character(paste0(
            "<span style='color:black;'>",
            "<b>Advisory</b><br>",
            "Issued: ", format(Issued, "%b %d, %Y"), "<br>",
            "Lifted: ", Lifted_label, "<br>",
            "`", `Dominant genus/toxin`, "`: ", `Cell Count/Toxin`, " ", Unit,
            "</span>"
          ))
        )
    })
    
    report_shape <- reactive({
      list(
        type = "rect",
        x0 = as.Date(max(dta2$Date)) - 6,
        x1 = as.Date(max(dta2$Date)),
        y0 = 0.5,
        y1 = max(max(df()$Value, na.rm = TRUE), 25),
        fillcolor = "green",
        line = list(color = "green"),
        opacity = 0.2
      )
    })
    
    report_hover_markers <- reactive({
      req(df(), dta2)
      
      tibble::tibble(
        x = as.Date(max(dta2$Date)) - 4,
        y = max(max(df()$Value, na.rm = TRUE), 25) * 1.05,
        label = as.character(glue::glue(
          "<span style='color:black;'><b>Reporting period:</b><br>{report_start_fmt} - {report_end_fmt}</span>"
        ))
      )
    })
    
    all_shapes <- reactive({c(list(report_shape()), advisory_shapes_chl())})
    # all_annotations <- reactive({c(list(report_label))})
    
    # render plots
    
    observeEvent(input$waterbody,{
      
      if(input$waterbody == c("Oregon")) {
        
        output$no_plot_1 <- renderText({ 
          
          "Select a waterbody to view the time series plot."
          
        })
        
        output$no_plot_2 <- renderText({ 
          
          "Select a waterbody to view the plot. Field cyanotoxin data are based on individual samples and shown only when available."
          
        })
        
      } else {
        
        output$no_plot_1 <- renderText({})
        
        output$no_plot_2 <- renderText({})
        
        # __ Plot Clh-a ----
        output$plot_chl <- renderPlotly({
          
          if (is.null(df()) || nrow(df()) == 0) return(NULL)
          
          y_max <- max(max(df()$Value[df()$Parameter %in% selected_matrix()], na.rm = TRUE) * 1.1, 27)
          
          make_timeseries_plot(
            df = df(),
            df_temp = df_temp(),
            parameters_line = c("Weekly Mean Daily Max", "Weekly Median Daily Max", "Daily Maximum", "Daily Mean"),
            parameter_chla = "Chlorophyll a",
            parameter_toxins = NULL,
            pal.plot = pal.plot,
            selected_matrix = selected_matrix(),
            y_max = y_max,
            report_markers = report_hover_markers(),
            advisory_markers = if(input$advisory_bars) advisory_hover_markers_chl() else NULL,
            shapes = if(input$advisory_bars) all_shapes() else list(report_shape()),
            horizontal_lines = list(list(y=24, color='#006d2c', name="Chl-a: 24*", group="high")),
            plot_title = as.character(unique(df()$GNISIDNAME))
          )
        })
        
        # __ Plot Toxins ----
        output$plot_toxin <- renderPlotly({
          
          if (is.null(df()) || nrow(df()) == 0) return(NULL)
          
          y_max <- max(
            max(
              df() %>%
                dplyr::filter(Parameter %in% c("Anatoxin-A", "Cylindrospermopsin", "Microcystins", "Saxitoxin"),
                              Parameter %in% selected_matrix()) %>%
                dplyr::pull(Value),
              na.rm = TRUE
            ) * 1.1,
            17
          )
          
          make_timeseries_plot(
            df = df(),
            df_temp = df_temp(),
            parameters_line = NULL,
            parameter_chla = NULL,
            parameter_toxins = c("Anatoxin-A", "Cylindrospermopsin", "Microcystins", "Saxitoxin"),
            pal.plot = pal.plot,
            selected_matrix = selected_matrix(),
            y_max = y_max,
            report_markers = NULL,
            advisory_markers = if(input$advisory_bars) advisory_hover_markers_toxins() else NULL,
            shapes = if(input$advisory_bars) advisory_shapes_toxins() else NULL,
            horizontal_lines = list(
              list(y=15, color='#737373', name="Toxin: 15**", group="high"),
              list(y=8,  color='#cc4c02', name="Toxin: 8***", group="high")
            ),
            add_reporting_legend = FALSE
            # plot_title = as.character(unique(df()$GNISIDNAME))
          )
        })
        
        output$who_line <- renderUI(HTML(paste("&nbsp;","&nbsp;","&nbsp;","&nbsp;",
                                               # em(paste0("*RP: Reporting period from ",
                                               #           report_start_fmt," to ", report_end_fmt,".")),
                                               "<br/>",
                                               "&nbsp;","&nbsp;","&nbsp;","&nbsp;",
                                               em("*Chl-a: 24 μg/L: World Health Organization (WHO) Alert Level 2 Guideline for monitoring and managing cyanobacteria in waterbodies used for recreation."),
                                               "<br/>",
                                               "&nbsp;","&nbsp;","&nbsp;","&nbsp;",
                                               em("**Toxin: 15 μg/L: Oregon Health Authority (OHA)'s Recreational Use Value (RUV) for anatoxin-a and cylindrospermopsin."),
                                               "<br/>",
                                               "&nbsp;","&nbsp;","&nbsp;","&nbsp;",
                                               em("**Toxin: 8 μg/L: OHA's RUV for microcystin and saxitoxin."),
                                               "<br/>",
                                               "&nbsp;","&nbsp;","&nbsp;","&nbsp;",
                                               em("Chlorophyll a concentration at 0 μg/L is derived from low imagery digital values, indicating non-detection."))))
        
      }
      
    })
    
    # 3. Tables ----
    # _ 7-Day Table ----
    output$tbl7dadm <- DT::renderDataTable({
      
      DT::datatable(
        data = map.tbl.data,
        style = 'bootstrap',
        extensions = 'Buttons',
        options = list(#dom = 'frtilpB',
          dom = 'rtilpB',
          pageLength = 10,
          compact = TRUE,
          nowrap = TRUE,
          scorllX = TRUE,
          scorllY = TRUE,
          autoWidth = TRUE,
          columnDefs = list(
            list(targets = 0:6, className = "dt-left"),
            list(targets = 0, width = "50%"),
            list(targets = 1, width = "50%"),
            list(targets = 2, width = "10%"),
            list(targets = 3, width = "10%"),
            list(targets = 4, width = "10%"),  
            list(targets = 5, className = "dt-nowrap"), # no wrap for "Date of Max"
            list(targets = 6, width = "10%")
          ),
          language = list(emptyTable = "No highlighted waterbodies for this reporting period."),
          buttons = list(#'print',
            list(extend = 'collection',
                 buttons = c('csv','excel'),
                 text = 'Download')
          )),
        rownames = FALSE,
        filter = 'bottom'
      ) #%>%
      #DT::formatDate("Date","toLocaleString")
    }, server = FALSE
    
    )
    
    # _ Data table ----
    df_tbl <- reactive({
      
      req(df())
      
      df() %>% 
        dplyr::select(GNISIDNAME,Date,Parameter,`Detection Qualifier`,Value,Unit,`Result Status`,`Data Source`) %>% 
        dplyr::mutate(Note = dplyr::if_else((Value == 0) | (`Detection Qualifier` %in% c("<")), "Non-detect", "")) %>% 
        # dplyr::mutate(Value = scales::comma(Value)) %>%
        dplyr::rename(Waterbody_GNISID = GNISIDNAME)
    })
    
    observeEvent(input$waterbody,{
      
      if (is.null(df_tbl()) || nrow(df_tbl()) == 0) return(NULL)
      
      if(input$waterbody == c("Oregon")) {
        
        output$no_data <- renderText({ 
          
          "Select a waterbody to show the data table."
          
        })
        
      } else {
        
        output$no_data <- renderText({})
        
        output$caption <- renderUI({
          if (is.null(df_tbl()) || nrow(df_tbl()) == 0) return(NULL)
          HTML(unique(df_tbl()$Waterbody_GNISID))
        })
        
        output$table <- DT::renderDataTable({
          
          DT::datatable(
            data = df_tbl(),
            style = 'bootstrap',
            extensions = 'Buttons',
            options = list(dom = 'frtilpB',
                           pageLength = 10,
                           compact = TRUE,
                           nowrap = TRUE,
                           scorllX = TRUE,
                           buttons = list(
                             list(extend = 'collection',
                                  buttons = c('csv','excel'),
                                  text = 'Download')),
                           columnDefs = list(
                             list(targets = 0, width = '150px'), 
                             list(targets = 4, width = '30px'))
            ),
            rownames = FALSE,
            filter = 'bottom')
        }, server = FALSE)
        
        output$simpleTable <- renderTable({
          data()
        })
        
      }
      
    })
    
    # 4. Text ----
    # _ Drinking Water Area ----
    dw <- reactive({
      
      dta %>% 
        dplyr::filter(GNISIDNAME %in% input$waterbody) %>% 
        dplyr::mutate(dwsa = ifelse(wi_DWSA == "Yes", "Public Drinking Water Source", "Recreational Waterbody")) %>% 
        pull(dwsa)
      
    })
    
    output$dw <- renderText({ 
      
      if(input$waterbody == c("Oregon")) {}
      else {
        unique(dw())
      }
    })
    
    # _ Data Date ----
    dd <- reactive({
      
      dta %>% dplyr::filter(GNISIDNAME %in% input$waterbody) 
      
    })
    
    observeEvent(input$waterbody,{
      
      if(input$waterbody == c("Oregon")) {} 
      
      else {
        
        output$dataDate <- renderUI(HTML(paste0(
          "Data for ",input$waterbody," is available since ",
          ifelse(month(as.Date(min(dd()$Date))) %in% c(8,9,10,11,12,1,2), 
                 gsub("(\\D)0", "\\1", format(as.Date(min(dd()$Date)),'%b. %d, %Y')), 
                 gsub("(\\D)0", "\\1", format(as.Date(min(dd()$Date)),'%B %d, %Y'))),
          "."
          
        )))
        
      }
      
    })
    
  }
  
)
