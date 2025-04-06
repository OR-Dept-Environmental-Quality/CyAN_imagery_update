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
library(mapview)
library(scales)
library(plotly)
library(DT)
library(lubridate)

load("data.RData")

format_report_date <- function(date) {
  if (month(date) %in% c(8,9,10,11,12,1,2)) {
    gsub("(\\D)0", "\\1", format(date, '%b. %d, %Y'))
  } else {
    gsub("(\\D)0", "\\1", format(date, '%B %d, %Y'))
  }
}

max_date <- as.Date(max(dta2$Date))
report_start <- max_date - 6
report_end <- max_date
report_start_fmt <- format_report_date(report_start)
report_end_fmt <- format_report_date(report_end)

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
        
        h4("This report provides an update on satellite estimates of chlorophyll-a concentrations during the bloom season for 49 large waterbodies in Oregon. ",
          "Estimates are derived from satellite imagery from the ", 
          a("Cyanobacteria Assessment Network (CyAN)", href="https://www.epa.gov/water-research/cyanobacteria-assessment-network-cyan",target="_blank"),
          " project. Updates are scheduled to occur weekly from spring to fall each year. ",
          "The report presents Version 6 (V6) data reprocessed by NASA in February 2025.",
          "The V6 dataset includes updated calibration for Sentinel 3A and 3B, an enhanced filter for turbid water, and atmospheric correction for water vapor. ",
          "For more details on V6 data, please refer to the ",
          a("NASA Ocean Color website", href="https://oceancolor.gsfc.nasa.gov/data/reprocessing/projects/cyan/version/6/",.noWS = "outside",target="_blank"),
          ". This report also includes available field measurements and historical cyanobacteria advisories issued by the Oregon Health Authority.",
          .noWS = c("after-begin", "before-end")),
        
        h4("This report illustrates the concentrations of chlorophyll-a (μg/L) with cyanobacteria dominance in three levels: Low: 3-12 μg/L, Moderate: 12-24 μg/L, and High: >24 μg/L. ",
          "The levels correspond to the World Health Organization (WHO) exposure guideline values for recreational waters ",
          "(",a("WHO, 2021", href="https://www.who.int/publications/m/item/toxic-cyanobacteria-in-water---second-edition",.noWS = "outside",target="_blank"),"). ",
          "For more information on harmful algal blooms in Oregon, visit the ",
          a("Oregon DEQ", href="https://www.oregon.gov/deq/wq/Pages/Harmful-Algal-Blooms.aspx",target="_blank")," and ",
          a("Oregon Health Authority", href="https://www.oregon.gov/oha/ph/healthyenvironments/recreation/harmfulalgaeblooms/pages/blue-greenalgaeadvisories.aspx",.noWS = "outside",target="_blank"),
          " websites.",
          .noWS = c("after-begin", "before-end")),
        
        h4("All data presented in this report are provisional and subject to change. Satellite-derived estimates do not ",
          "imply the presence of cyanotoxins or other water quality impairments and do not have regulatory implications. ",
          tags$b("Visit the ",
            a("Oregon Health Authority", href="https://www.oregon.gov/oha/ph/healthyenvironments/recreation/harmfulalgaeblooms/pages/blue-greenalgaeadvisories.aspx",.noWS = "outside",target="_blank"),
            " to learn about recreational use and drinking water advisories related to cyanobacteria blooms. "),
          "Additional assessments using ",
          a("Sentinel 2", href="https://browser.dataspace.copernicus.eu/?zoom=7&lat=44.3466&lng=-119.25&themeId=DEFAULT-THEME&visualizationUrl=https%3A%2F%2Fsh.dataspace.copernicus.eu%2Fogc%2Fwms%2F274a990e-7090-4676-8f7d-f1867e8474a7&datasetId=S2_L1C_CDAS&fromTime=2023-07-01T00%3A00%3A00.000Z&toTime=2024-01-01T23%3A59%3A59.999Z&layerId=1_TRUE_COLOR&demSource3D=%22MAPZEN%22&cloudCoverage=100&dateMode=MOSAIC",
            target="_blank"),
          "imagery, local visual assessments, and/or water quality sampling are needed to provide further information on potential human health ",
          "and environmental effects of cyanobacteria. Factors such as cloud cover, ice, sun glint, water surface roughness, dry lake beds, algal mats, and shoreline effects can interfere with satellite imagery and estimation accuracy.*",
          .noWS = c("after-begin", "before-end")),
        
        h4("*DISCLAIMER: Information is preliminary. Additional data are needed to confirm the presence of cyanobacteria blooms.",
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
        tags$h4(p("Waterbodies with high chlorophyll-a concentration (>24 μg/L) are identified based on ",
          "the maximum value of the 7-Day Geometric Mean Daily Maximum (7DGMDM) during the reporting period. ",
          "The corresponding 'Date_7DGMDM' indicates the date on which this maximum value occurred. ",
          "The 7-Day Average Daily Maximum (7DADM) is also reported for each highlighted waterbody. ",
          "Both metrics represent 7-day moving averages calculated using daily maximum values from ",
          "the most recent available data date and the preceding six days. ",
          "The 'Days of Data' field indicates the number of valid observation days within each 7-day window used for computing both 7DGMDM and 7DADM.")),
        
        tags$h4(p(strong("Reporting Period: ", report_start_fmt, " - ", report_end_fmt))),
        
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
          # shinyfullscreen::fullscreen_this(shiny::imageOutput("maps7"))
          # leaflet::leafletOutput("map7d", height = "700px")
          
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
        tags$h4(p("The interactive map provides satellite imagery for 49 Oregon waterbodies from July 1, 2024 to the present.")),
        
        shinydashboard::box(
          width = 3,
          #title = "left",
          solidHeader = TRUE,
          
          # ___ Select a Waterbody ----
          tags$hr(),
          tags$h4(p("Select a waterbody to zoom in on its location on the map. ",
            "Once selected, information will be displayed indicating whether the waterbody is used for recreation or as a public drinking water source.")),
          
          shinyWidgets::pickerInput(inputId = "waterbody",
            label = tags$h4(strong("Select a Waterbody:")),
            choices = list("Oregon",
              "Waterbody Name_GNISID" = sort(unique(lakes.resolvable$GNISIDNAME))),
            multiple = FALSE),
          # ___ Drinking Water Area ----
          shiny::textOutput("dw"),
          
          tags$br(),
          tags$hr(),
          
          # ___ Select a Date ----
          tags$h4(p("Select a date to update the map with satellite imagery from that day. Imagery is available from July 1, 2024 to ", report_end_fmt, ".")),
          
          shiny::dateInput(inputId = "date_map",
            label = tags$h4(strong("Select a Date:")),
            value = as.Date(max(dta2$Date)),
            # min = as.Date(max(dta2$Date))-6,
            min = as.Date("2024-07-01"),
            max = as.Date(max(dta2$Date)),
            format = "yyyy-mm-dd",
            startview = "month",
            weekstart = 0,
            datesdisabled = missing.dates$Date)
          
          # tags$hr(),
          
          # ___ Boxplot ----
          # tags$h4("Boxplot of cyanobacteria estimates (cells/mL) in waterbody on selected date:"),
          # 
          # shinycssloaders::withSpinner(plotlyOutput("boxplot"))
          
        ),
        
        # ___ Interactive Map ----
        shinydashboard::box(
          width = 9,
          #title = "right",
          solidHeader = TRUE,
          
          shinycssloaders::withSpinner(leaflet::leafletOutput("map", height = "700px"))
          
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
        tags$h4(p("Time series plots for each of the 49 Oregon waterbodies display satellite estimates of chlorophyll-a concentrations and ",
          "available field measurements. Satellite estimates follow methods established by the ",
          a("CyAN Project", href="https://www.epa.gov/water-research/cyanobacteria-assessment-network-cyan",.noWS = "outside",target="_blank"),", ",
          " including data from Sentinel-3A (2016-present) and Sentinel-3B (2018-present). ",
          "Field measurements include chlorophyll-a and cyanotoxin concentrations. ",
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
            
            # h3("Time Series Plot and Data:"),
            
            # ____ Select a Waterbody 2 ----
            shinyWidgets::pickerInput(inputId = "waterbody2",
              label = tags$h4(strong("Select a waterbody:")),
              choices = list("Oregon",
                "Waterbody Name_GNISID" = sort(unique(lakes.resolvable$GNISIDNAME))),
              multiple = FALSE),
            
            # ____ Date range ----
            shiny::radioButtons(
              inputId = "ploty",
              label = tags$h4(strong("Date Range:")),
              choices = c("Current Year: 2025",
                "Reset to Complete Data Range",
                "Select a Date Range"),
              selected = "Current Year: 2025"),
            
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
            tags$br(),
            
            # ____ Parameters ----
            tagList(
              tags$h4(strong("Parameters:")),
              
              tags$br(),
              
              # Checkbox group
              checkboxGroupInput(
                inputId = "matrix",
                label = NULL,
                choices = c(
                  "CyAN-Chlorophyll-a: 7DADM" = "7DADM",
                  "CyAN-Chlorophyll-a: 7DGMDM" = "7DGMDM",
                  "CyAN-Chlorophyll-a: Daily Maximum" = "Daily Maximum",
                  "CyAN-Chlorophyll-a: Daily Mean" = "Daily Mean",
                  "Field data: Chlorophyll-a" = "Chlorophyll a",
                  "Field data: Anatoxin-A" = "Anatoxin-A",
                  "Field data: Cylindrospermopsin" = "Cylindrospermopsin",
                  "Field data: Microcystins" = "Microcystins",
                  "Field data: Saxitoxin" = "Saxitoxin"#,
                  # "Field data: Pheophytin-a" = "Pheophytin a"
                ),
                selected = c("7DGMDM", "Daily Maximum")
              ),
              
              tags$br(),
              
              # Buttons in a row
              fluidRow(
                column(4, actionButton("select_all", "Check All", icon = icon("check-square"))),
                column(8, actionButton("clear_all", "Clear All", icon = icon("square")))
              )
              
            ),
            
            tags$br(),
            tags$br(),
            
            # ____ Plot types ----
            checkboxGroupInput(
              inputId = "plot_log",
              label = tags$h4(strong("y-axis:")),
              choices = c("Log Scale" = "log"))
            
          ),
          
          shinydashboard::box(
            width = 9,
            #title = "right",
            solidHeader = FALSE,
            
            # ____ Time series plot ----
            tags$h4(p(strong("Time series plot of Chlorophyll-a (μg/L) with cyanobacteria dominance in the selected waterbody."))),
            
            textOutput("no_plot"),
            
            plotlyOutput("plot_cell"),
            
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
            
            h4("The report is provided by the Oregon DEQ Watershed Management Section. Copyright (C) 2020-2025, Oregon DEQ."),
            h4("The source code of this report is publicly available at GitHub repository: ", 
              a("Satellite Estimates of Cyanobacteria in Oregon Lakes and Reservoirs",
                href="https://github.com/OR-Dept-Environmental-Quality/CyAN_imagery_update",.noWS = "outside",target="_blank"),".",
              .noWS = c("after-begin", "before-end")),
            h4("For more information on this report, please contact"),
            h4("Daniel Sobota (Lead), ", a("daniel.sobota@deq.oregon.gov",href="mailto:dan.sobota@deq.oregon.gov",target="_blank")),
            # h4("Erin Costello, ", a("erin.costello@deq.oregon.gov",href="mailto:erin.costello@deq.oregon.gov",target="_blank")),
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
          popup = ~paste0("<b>Station ID:</b> ", MLocID, "<br>",
            "<b>Station:</b> ", StationDes, "<br>",
            "<b>Data Counts:</b><br>", CharData),
          radius = 8,
          color = "blue",
          fillOpacity = 0.7,
          layerId = ~MLocID,
          options = leaflet::pathOptions(pane = "stations")) %>% 
        leaflet::addLayersControl(baseGroups = c("OpenStreetMap","National Geographic World Map"),
          overlayGroups = c("Monitoring Stations","Basins (HUC6)"),
          position = "topleft",
          options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)) %>% 
        leaflet::hideGroup(c("Monitoring Stations","Basins (HUC6)")) %>% 
        leaflet.extras::addSearchFeatures(targetGroups = "lakes.resolvable",
          options = leaflet.extras::searchFeaturesOptions(openPopup = TRUE,
            zoom = 8,
            textPlaceholder = "Search a waterbody..."))
      
    })
    
    # _ map reactive @ date selector ----
    observeEvent(input$date_map,{
      
      progress$value <- 0
      withProgress(message = 'Updating data, please wait...', value = progress$value, {
        
        df.map.date <- reactive({
          
          lookup.date %>% dplyr::filter(Date %in% as.Date(input$date_map))
          
        })
        
        if(is.na(df.map.date()$Year.dta)){
          
          return(NULL)
          
        } else {
          
          map.tif.dir <- reactive(paste0("./data/", df.map.date()$Year.dates, "/"))
          file.name <- reactive(paste0(df.map.date()$CyAN_File_NUM,".tif"))
          
          rst <- reactive({
            
            r <- raster::raster(paste0(map.tif.dir(), file.name()))
            raster::crs(r) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs"
            return(r)
            
          })
          
          leafletProxy("map") %>% 
            leaflet::clearImages() %>% 
            leaflet::clearControls() %>%
            leaflet::addRasterImage(rst(), layerId = "Value", project = FALSE, colors=pal.map, opacity = 1,
              group = "Satellite Imagery") %>% 
            leaflet::addLegend(pal = pal.map, values = thevalues, title = "Chlorophyll-a (μg/L)", position = "topright",
              labFormat = function(type,cuts,p){paste0(labels)},opacity = 1) %>% 
            leaflet::addLayersControl(baseGroups = c("OpenStreetMap","National Geographic World Map"),
              overlayGroups = c("Satellite Imagery","Monitoring Stations","Basins (HUC6)"),
              position = "topleft",
              options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)) %>% 
            leaflet::hideGroup(c("Monitoring Stations","Basins (HUC6)"))
          
        }
        
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
        updatePickerInput(session, "waterbody2", selected = selected_waterbody())
        
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
        updatePickerInput(session, "waterbody", selected = selected_waterbody())
      }
    })
    
    # 2. Plots ----
    # _ Time series plot ----
    parameters <- c(
      "7DADM", "7DGMDM", "Daily Maximum", "Daily Mean", 
      "Chlorophyll a", "Anatoxin-A", "Cylindrospermopsin", "Microcystins", "Saxitoxin", "Pheophytin a")
    
    parameter_colors <- c(
      "brown", "blue", "orange", "green", 
      "purple", "#17becf", "gold", "red", "pink", "gray")
    
    pal.plot <- setNames(parameter_colors, parameters)
    
    observeEvent(input$select_all, {
      updateCheckboxGroupInput(
        session,
        inputId = "matrix",
        selected = c(
          "7DADM", "7DGMDM", "Daily Maximum", "Daily Mean",
          "Chlorophyll a", "Anatoxin-A", "Cylindrospermopsin",
          "Microcystins", "Saxitoxin", "Pheophytin a"
        )
      )
    })
    
    observeEvent(input$clear_all, {
      updateCheckboxGroupInput(
        session,
        inputId = "matrix",
        selected = character(0)
      )
    })
    
    yr <- reactive({ 
      
      if(input$ploty == "Current Year: 2025"){"2025"}else{sort(unique(dta$Year))}
      
    })
    
    df <- reactive({
      
      if(input$ploty == "Current Year: 2025"){
        
        dta %>% 
          dplyr::filter(GNISIDNAME %in% input$waterbody) %>% 
          dplyr::filter(Parameter %in% input$matrix) %>% 
          dplyr::filter(Year %in% c(yr())) %>% 
          dplyr::mutate(Value = round(Value,2))
        
      }else if (input$ploty == "Reset to Complete Data Range") {
        
        dta %>% 
          dplyr::filter(GNISIDNAME %in% input$waterbody) %>% 
          dplyr::filter(Parameter %in% input$matrix) %>%
          dplyr::mutate(Value = round(Value,2))
        
      } else {
        
        dta %>% 
          dplyr::filter(GNISIDNAME %in% input$waterbody) %>% 
          dplyr::filter(Parameter %in% input$matrix) %>% 
          dplyr::filter(Year %in% c(yr())) %>%
          dplyr::mutate(Value = round(Value,2)) %>%
          dplyr::filter(Date >= input$date_plot[1],Date <= input$date_plot[2])
        
      }
      
    })
    
    df_after_gap <- reactive({ 
      df() %>% dplyr::filter(Date >= as.Date("2016-01-01"))
    })
    
    type <- reactive({
      
      input$plot_log
      
    })
    
    yaxis <- reactive({
      
      if_else(length(input$plot_log)>0,
        "Chlorophyll-a (μg/L)",
        "Chlorophyll-a (μg/L)")
      
    })
    
    observeEvent(input$waterbody,{
      
      if(input$waterbody == c("Oregon")) {
        
        output$no_plot <- renderText({ 
          
          "Select a waterbody to show the plot."
          
        })
        
      } else {
        
        output$no_plot <- renderText({})
        
        advisory_shapes <- reactive({
          advisories %>%
            dplyr::filter(GNIS_Name_ID == input$waterbody) %>%
            dplyr::filter(Issued <= max(df()$Date), Lifted >= min(df()$Date)) %>% 
            tidyr::drop_na() %>% 
            purrr::pmap(function(Issued, Lifted, ...) {
              list(
                type = "rect",
                x0 = as.Date(Issued),
                x1 = as.Date(Lifted),
                y0 = 0.5,
                y1 = max(df()$Value, na.rm = TRUE) + 10,
                fillcolor = "red",
                line = list(color = "red"),
                opacity = 0.1
              )
            })
        })
        
        # advisory_labels <- reactive({
        #   advisories %>%
        #   dplyr::filter(GNIS_Name_ID == input$waterbody) %>%
        #   dplyr::filter(Issued <= max(df()$Date), Lifted >= min(df()$Date)) %>% 
        #   tidyr::drop_na() %>% 
        #   purrr::pmap(function(Issued, Lifted, ...) {
        #     list(
        #       x = as.Date(Lifted),
        #       y = max(df()$Value, na.rm = TRUE) * 1.05,
        #       text = paste0("Advisory<br>", format(Issued, "%b %d"), "–", format(Lifted, "%b %d")),
        #       font = list(size = 10, color = "red"),
        #       xref = "x", yref = "y",
        #       showarrow = FALSE,
        #       align = "right"
        #     )
        #   })
        # })
        
        advisory_hover_markers <- reactive({
          req(df())
          
          advisories %>%
            dplyr::filter(
              GNIS_Name_ID == input$waterbody,
              Issued <= max(df()$Date),
              Lifted >= min(df()$Date)
            ) %>%
            tidyr::drop_na() %>%
            dplyr::mutate(
              x = as.Date(Lifted),
              y = max(df()$Value, na.rm = TRUE) * 1.05,
              label = paste0(
                "<span style='color:black;'>",
                "<b>Advisory</b><br>",
                "Issued: ", format(Issued, "%b %d, %Y"), "<br>",
                "Lifted: ", format(Lifted, "%b %d, %Y"), "<br>",
                "`", `Dominant genus/toxin`, "`: ", `Cell Count/Toxin`, " ", Unit
              )
            )
        })
        
        report_shape <- list(
          type = "rect",
          x0 = as.Date(max(dta2$Date)) - 6,
          x1 = as.Date(max(dta2$Date)),
          y0 = 0.5,
          y1 = max(df()$Value, na.rm = TRUE) + 10,
          fillcolor = "green",
          line = list(color = "green"),
          opacity = 0.2
        )
        
        report_label <- list(
          x = as.Date(max(dta2$Date)) - 4,
          y = max(df()$Value, na.rm = TRUE) + 8,
          text = "RP*",
          textfont = list(size = 12),
          showlegend = FALSE
        )
        
        who_line_annotation <- list(
          x = max(df()$Date),
          y = 24,
          text = "High (24 μg/L)**",
          font = list(size = 12),
          xref = "x", yref = "y",
          showarrow = TRUE,
          arrowhead = 3,
          arrowsize = 1,
          ax = -60, ay = -20
        )
        
        all_shapes <- reactive({c(list(report_shape), advisory_shapes())})
        # all_annotations <- reactive({c(list(report_label), advisory_labels(), list(who_line_annotation))})
        all_annotations <- reactive({c(list(report_label), list(who_line_annotation))})
        
        output$plot_cell <- renderPlotly({
          
          if(input$ploty == "Current Year: 2025"){
            
            plotly::plot_ly() %>%
              plotly::add_trace(
                data = df_after_gap() %>% dplyr::filter(Parameter %in% c("7DADM", "7DGMDM", "Daily Maximum", "Daily Mean")),
                x = ~as.Date(Date), 
                y = ~Value,
                split = ~Parameter,
                type = "scatter",
                mode = "lines+markers",
                color = ~Parameter,
                colors = pal.plot,
                marker = list(size = 8),
                legendgroup = "line") %>%
              plotly::add_trace(
                data = df_after_gap() %>% dplyr::filter(Parameter %in% c("Chlorophyll a", "Anatoxin-A", "Cylindrospermopsin", "Microcystins", "Saxitoxin", "Pheophytin a")),
                x = ~as.Date(Date),
                y = ~Value,
                type = "bar",
                name = ~Parameter,
                color = ~Parameter,
                colors = pal.plot,
                legendgroup = "bar") %>%
              plotly::add_trace(
                y = 24, 
                mode = "lines",
                # x = ~as.Date(dta$Date),
                x = ~as.Date(df()$Date),
                line = list(shape = 'spline', color = '#006d2c', width = 3),
                name = "High",
                legendgroup = "high",
                showlegend = FALSE) %>% 
              plotly::add_trace(
                data = advisory_hover_markers(),
                x = ~x,
                y = ~y,
                text = ~label,
                type = "scatter",
                mode = "markers",
                hoverinfo = "text",
                marker = list(
                  size = 10,
                  symbol = "triangle-up",
                  color = "#D3D3D3"),
                name = "Advisory",
                showlegend = FALSE) %>% 
              plotly::layout(
                xaxis = list(title = "Date", range = c(min(df()$Date),max(df()$Date)+1)),
                # yaxis = list(title = "Cyanobacteria (cells/mL)"),
                title = as.character(unique(df()$GNISIDNAME)),
                shapes = all_shapes(),
                annotations = all_annotations()) %>% 
              plotly::layout(
                yaxis = list(type = type(),
                  title = yaxis()))
            
          } else if (input$ploty == "Reset to Complete Data Range") {
            
            plotly::plot_ly() %>%
              plotly::add_trace(
                data = df_after_gap() %>% dplyr::filter(Parameter %in% c("7DADM", "7DGMDM", "Daily Maximum", "Daily Mean")),
                x = ~as.Date(Date), 
                y = ~Value,
                split = ~Parameter,
                type = "scatter",
                mode = "lines+markers",
                color = ~Parameter,
                colors = pal.plot,
                marker = list(size = 8),
                legendgroup = "line") %>%
              plotly::add_trace(
                data = df_after_gap() %>% dplyr::filter(Parameter %in% c("Chlorophyll a", "Anatoxin-A", "Cylindrospermopsin", "Microcystins", "Saxitoxin", "Pheophytin a")),
                x = ~as.Date(Date),
                y = ~Value,
                type = "bar",
                name = ~Parameter,
                color = ~Parameter,
                colors = pal.plot,
                legendgroup = "bar") %>%
              plotly::add_trace(
                y = 24, 
                mode = "lines",
                # x = ~as.Date(dta$Date),
                x = ~as.Date(df()$Date),
                line = list(shape = 'spline', color = '#006d2c', width = 3),
                name = "High",
                legendgroup = "high",
                showlegend = FALSE) %>% 
              plotly::add_trace(
                data = advisory_hover_markers(),
                x = ~x,
                y = ~y,
                text = ~label,
                type = "scatter",
                mode = "markers",
                hoverinfo = "text",
                marker = list(
                  size = 10,
                  symbol = "triangle-up",
                  color = "#D3D3D3"),
                name = "Advisory",
                showlegend = FALSE) %>% 
              plotly::layout(
                xaxis = list(
                  title = "Date", 
                  range = c(min(df()$Date),max(df()$Date)+1)),
                # yaxis = list(title = "Cyanobacteria (cells/mL)"),
                title = as.character(unique(df()$GNISIDNAME)),
                shapes = all_shapes(),
                annotations = all_annotations()) %>% 
              plotly::layout(
                yaxis = list(type = type(),
                  title = yaxis()))
            
          } else {
            
            plotly::plot_ly() %>%
              plotly::add_trace(
                data = df_after_gap() %>% dplyr::filter(Parameter %in% c("7DADM", "7DGMDM", "Daily Maximum", "Daily Mean")),
                x = ~as.Date(Date), 
                y = ~Value,
                split = ~Parameter,
                type = "scatter",
                mode = "lines+markers",
                color = ~Parameter,
                colors = pal.plot,
                marker = list(size = 8),
                legendgroup = "line") %>%
              plotly::add_trace(
                data = df_after_gap() %>% dplyr::filter(Parameter %in% c("Chlorophyll a", "Anatoxin-A", "Cylindrospermopsin", "Microcystins", "Saxitoxin", "Pheophytin a")),
                x = ~as.Date(Date),
                y = ~Value,
                type = "bar",
                name = ~Parameter,
                color = ~Parameter,
                colors = pal.plot,
                legendgroup = "bar") %>%
              plotly::add_trace(
                y = 24, 
                mode = "lines",
                # x = ~as.Date(dta$Date),
                x = ~as.Date(df()$Date),
                line = list(shape = 'spline', color = '#006d2c', width = 3),
                name = "High",
                legendgroup = "high",
                showlegend = FALSE) %>% 
              plotly::add_trace(
                data = advisory_hover_markers(),
                x = ~x,
                y = ~y,
                text = ~label,
                type = "scatter",
                mode = "markers",
                hoverinfo = "text",
                marker = list(
                  size = 10,
                  symbol = "triangle-up",
                  color = "#D3D3D3"),
                name = "Advisory",
                showlegend = FALSE) %>% 
              plotly::layout(
                xaxis = list(title = "Date", range = c(min(df()$Date),max(df()$Date)+1)),
                # yaxis = list(title = "Cyanobacteria (cells/mL)"),
                title = as.character(unique(df()$GNISIDNAME)),
                shapes = all_shapes(),
                annotations = all_annotations()) %>% 
              plotly::layout(
                yaxis = list(type = type(),
                  title = yaxis()))
            
          }
          
        })
        
        output$who_line <- renderUI(HTML(paste("&nbsp;","&nbsp;","&nbsp;","&nbsp;",
          em(paste0("*RP: Reporting period from ",
            report_start_fmt," to ", report_end_fmt,".")),
          "<br/>",
          "&nbsp;","&nbsp;","&nbsp;","&nbsp;",
          em("**High (24 μg/L): World Health Organization (WHO) Alert Level 2 Guideline for monitoring and managing cyanobacteria in waterbodies used for recreation."),
          "<br/>",
          "&nbsp;","&nbsp;","&nbsp;","&nbsp;",
          em("Chlorophyll-a concentration at 0 μg/L is derived from low imagery digital values, indicating non-detection."))))
        
      }
      
    })
    
    # _ Boxplot ----
    # df.box <- reactive({
    #   
    #   dn_tbl %>%
    #     dplyr::filter(GNISIDNAME %in% input$waterbody) %>%
    #     dplyr::filter(as.Date(Date) %in% as.Date(input$date_map))  %>%
    #     tidyr::pivot_longer(cols = tidyr::all_of(grep("^VALUE_", colnames(dn_tbl), value = TRUE)),
    #                         names_to = "pixcel_value", values_to = "counts") %>%
    #     dplyr::mutate(pixcel_value = as.numeric(str_remove(pixcel_value, "VALUE_"))) %>%
    #     dplyr::filter(!pixcel_value %in% c(254,255)) %>%
    #     dplyr::filter(!is.na(counts)) %>%
    #     dplyr::slice(rep(dplyr::row_number(), counts)) %>%
    #     dplyr::select(-counts) %>%
    #     dplyr::mutate(`Cyanobacteria (cells/mL)` = (10^(3.0 / 250.0 * pixcel_value - 4.2)) * 100000000)
    #   
    # })
    # 
    # output$boxplot <- renderPlotly({
    #   
    #   plotly::plot_ly(data = df.box(), y = ~`Cyanobacteria (cells/mL)`, type = "box")
    #   
    #   plotly::plot_ly(data = df.box(),
    #                   x = ~as.Date(Date, format= "%Y-%m-%d"),
    #                   y = ~`Cyanobacteria (cells/mL)`,
    #                   type = "box",
    #                   name = ~unique(GNISIDNAME)) %>%
    #     plotly::add_trace(x = as.Date(input$date_map),
    #                       y = 100000,
    #                       line = list(color = "#006d2c")) %>%
    #     plotly::layout(xaxis = list(title = "",
    #                                 zeroline = FALSE,
    #                                 showline = FALSE,
    #                                 showticklabels = FALSE,
    #                                 showgrid = FALSE),
    #                    yaxis = list(type = "log",
    #                                 title = "",
    #                                 zeroline = TRUE,
    #                                 showline = TRUE,
    #                                 showticklabels = TRUE,
    #                                 showgrid = FALSE),
    #                    showlegend = FALSE,
    #                    annotations = list(x = as.Date(input$date_map),
    #                                       y = log(100000)/log(10),
    #                                       #y = 100000,
    #                                       text = "WHO Threshold",
    #                                       font = list(size = 8),
    #                                       xref = "x",
    #                                       yref = "y",
    #                                       showarrow = TRUE,
    #                                       arrowhead = 3,
    #                                       arrowsize = 1,
    #                                       ax = 40,
    #                                       ay = 50))
    #   
    # })
    
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
          columnDefs = list(list(targets = 0:3, className = "dt-left"),
            list(targets = (0), width = "50%"),
            list(targets = (1), width = "50%"),
            list(targets = (2), width = "10%"),
            list(targets = (3), width = "10%")),
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
      
      df() %>% 
        dplyr::select(GNISIDNAME,Date,Parameter,Value,Unit,`Result Status`,`Data Source`) %>% 
        dplyr::mutate(Note = ifelse(Value == 0, "Non-detect", "")) %>% 
        dplyr::mutate(Value = scales::comma(Value)) %>%
        dplyr::rename(Waterbody_GNISID = GNISIDNAME)
    })
    
    observeEvent(input$waterbody,{
      
      if(input$waterbody == c("Oregon")) {
        
        output$no_data <- renderText({ 
          
          "Select a waterbody to show the data table."
          
        })
        
      } else {
        
        output$no_data <- renderText({})
        
        output$caption <- renderUI(HTML(unique((df_tbl()$Waterbody_GNISID))))
        
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
                  text = 'Download')
              )),
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
