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
library(scales)
library(plotly)
library(DT)
library(lubridate)

load("data.RData")

# Shiny App ----
shinyApp(
  ui = shinydashboardPlus::dashboardPage(
    options = list(sidebarExpandOnHover = FALSE),
    #header = shinydashboardPlus::dashboardHeader(titleWidth = 400),
    header = shinydashboardPlus::dashboardHeader(titleWidth = 0, disable = TRUE),
    
    # Sidebar ----
    sidebar = shinydashboardPlus::dashboardSidebar(
      width = "0px" #---remove sidebar
    ),              #---remove sidebar
    #  minified = TRUE, collapsed = TRUE, width = 400,
    
    #  sidebarMenu(
    #    menuItem("About", icon = icon("info-circle"),
    #             menuSubItem(h4(HTML("
    #             This web application provides an interactive<br/>
    #             map to view satellite derived data on<br/>
    #             cyanobacteria harmful algal blooms in<br/>
    #             freshwater ecosystems of Oregon. Satellite<br/>
    #             data come from the US EPA CyAN project<br/>
    #             and are updated on a regular basis.<br/>
    #             <br/>
    #             Copyright (C) 2020-2021, ODEQ.")))),
    #    menuItem("User Guide",  icon = icon("cog"), href="userGuide.html"),
    #    menuItem("Contact", icon = icon("envelope"),
    #             menuSubItem(h5(HTML("
    #             For more information on the Oregon HABs Map<br/>
    #             Application Project, please contact<br/>
    #             <br/>
    #             Dan Sobota, Water Quality Analyst (Lead) <br/>
    #             Daniel.Sobota@deq.state.or.us<br/>
    #             <br/>
    #             Erin Costello, Water Quality Analyst<br/>
    #             Erin.Costello@deq.state.or.us<br/>
    #             <br/>
    #             Yuan Grund, Water Quality Analyst<br/>
    #             Yuan.Grund@deq.state.or.us"))))
    #  ) # sidebarMenu END
    #), # dashboardSidebar END
    
    # Body ----
    body = shinydashboard::dashboardBody(
      
      tags$div(
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
                         
                         /*pickerinput_waterbody*/
                         .selectpicker {
                         z-index: 999999999 !important;
                         }
                         
                         /*datepicker*/
                         .datepicker {
                         z-index:99999 !important;
                         }
                         
                         #caption {
                         font-size: 18px;
                         }
                         '))),
      
      # _ Header ----
      shinydashboard::box(
        width = 12,
        solidHeader = TRUE,
        
        tags$img(src = "DEQ-logo-color-horizontal370x73.png"),
        tags$div(span("Satellite Estimates of Cyanobacteria in Oregon Lakes and Reservoirs",
                      style = "color: black; font-size: 40px")),
        
        tags$h3("Reporting Period: ",format(as.Date(max(dta2$Date))-6, "%B %d, %Y")," - ",format(max(dta2$Date),'%B %d, %Y'),".")
        
        #tags$div(span(HTML(paste0("Last sourced from the ",
        #                          a("U.S. EPA CyAN Project", 
        #                            href="https://www.epa.gov/water-research/cyanobacteria-assessment-network-cyan"),
        #                          " on: ",
        #                          max(dta$Date))),
        #              style = "color: black; font-size: 20px"))
        
      ), # Header box END 
      
      # _ 1. Introduction ----
      shinydashboardPlus::box(
        width = 12,
        title = "1. Introduction",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        collapsed = FALSE,
        #dropdownMenu = boxDropdown(),
        
        h4("This report provides an update to estimates of cyanobacteria abundance derived from satellite imagery for 49 large Oregon waterbodies. ",
           "Updates are scheduled to occur weekly from March to October each year. Estimates derive from the ", 
           a("Cyanobacteria Assessment Network (CyAN)", href="https://www.epa.gov/water-research/cyanobacteria-assessment-network-cyan"),
           " project. Three levels illustrate cyanobacteria abundance (cells/mL): Low: <20,000, Moderate: 20,000-100,000, and High: >100,000. ",
           "The levels correspond to the World Health Organization (WHO) exposure guideline values ",
           "(",a("WHO, 2003", href="https://apps.who.int/iris/bitstream/handle/10665/42591/9241545801.pdf?sequence=1&isAllowed=y"),"). ",
           "For more information on Harmful Algal Blooms in Oregon, please visit websites from the ",
           a("Oregon DEQ", href="https://www.oregon.gov/deq/wq/Pages/Harmful-Algal-Blooms.aspx")," and the ",
           a("Oregon Health Authority", href="https://www.oregon.gov/oha/ph/healthyenvironments/recreation/harmfulalgaeblooms/pages/blue-greenalgaeadvisories.aspx"),"."),
        
        h4("All data presented in this report are provisional and subject to change. Estimates of cyanobacteria abundance may be skewed by cloud cover, ",
           "ice cover, sun glint, water surface roughness, dry lake beds, algal mats and shoreline effects. We suggest examining additional imagery from ",
           a("Sentinel 2", href="https://www.sentinel-hub.com/explore/sentinelplayground/")," and/or following up with local information to confirm on the ground conditions.")
      ),
      
      # _ 2. Table and Oregon map ----
      shinydashboardPlus::box(
        width = 12,
        title = "2. Highlighted Waterbodies",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        collapsed = FALSE,
        #dropdownMenu = boxDropdown(),
        
        tags$h4(p(strong("Waterbodies with high cyanobacteria abundance (>100,000 cells/mL) based on the 7-Day Average Daily Maximum (7DADM)."))),
        tags$h4(p(strong(paste0("Reporting Period: ",format(as.Date(max(dta2$Date))-6, "%B %d, %Y")," - ",format(max(dta2$Date),'%B %d, %Y'),".")))),
        
        # ___ Table 7DADM ----
        shinydashboard::box(
          width = 5,
          #title = "Table",
          solidHeader = TRUE,
          
          shinycssloaders::withSpinner(DT::dataTableOutput("tbl7dadm"))
          
        ),
        
        # ___ Oregon map ----
        shinydashboard::box(
          width = 7,
          #title = "ORMap",
          solidHeader = TRUE,
          
          #h4("Waterbodies with high cyanobacteria estimates from ",
          #   format(as.Date(max(dta2$Date))-6, "%B %d, %Y")," to ",format(max(dta2$Date),'%B %d, %Y'),
          #   "are outlined in red. Other resolvable waterbodies are in blue."),
          
          shinycssloaders::withSpinner(leaflet::leafletOutput("map", height = "650px"))
          
        )
      ),
      
      # _ 3. Maps and Time series plots ----
      shinydashboardPlus::box(
        width = 12,
        height = "100%",
        title = "3. Data Visualization",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        collapsed = FALSE,
        
        tags$h4(p(strong("Maps and time series plot of cyanobacteria estimates for each of the 49 resolvable waterbodies according to the methods outlined in the ",
                         a("CyAN Project", href="https://www.epa.gov/water-research/cyanobacteria-assessment-network-cyan"),"."))),
        
        # ___ 7maps ----
        shinydashboard::box(
          width = 12,
          height = "100%",
          #title = "map",
          solidHeader = TRUE,
          
          shinydashboard::box(
            width = 3,
            #title = "left",
            solidHeader = FALSE,
            
            # ____ Select a waterbody ----
            shinyWidgets::pickerInput(inputId = "waterbody",
                                      label = tags$h3("Select a Waterbody:"),
                                      choices = list(
                                        "Oregon",
                                        "Waterbody Name_GNISID" = sort(unique(lakes.resolvable$GNISIDNAME))
                                      ),
                                      multiple = FALSE),
            # ____ Drinking water area ----
            shiny::textOutput("dw"),
            
            # ____ Lake images ----
            tags$br(),
            textOutput("non_select_image"),
            shiny::imageOutput("lakeImage", width = "300",height = "100%", inline = TRUE),
            tags$h5("Lake image will be updated soon.")
            
          ),
          
          shinydashboard::box(
            width = 9,
            #title = "right",
            solidHeader = FALSE,
            
            # ____ 7maps ----
            tags$h4(p(strong(paste0("Satellite estimates of cyanobacteria abundance from ",format(as.Date(max(dta2$Date))-6, "%B %d, %Y")," to ",format(max(dta2$Date),'%B %d, %Y'),".")))),
            
            textOutput("non_select"),
            uiOutput("no_pixels"),
            shiny::imageOutput("maps7")
            
          )
        ),
        
        # ___ Plot and Table ----
        shinydashboard::box(
          width = 12,
          #title = "plot+table",
          solidHeader = TRUE,
          
          shinydashboard::box(
            width = 3,
            #title = "left",
            solidHeader = FALSE,
            
            h3("Time Series Plot and Data:"),
            # ____ Date range ----
            shiny::radioButtons(
              inputId = "ploty",
              label = tags$h4("Date Range:"),
              choices = c("Current Year: 2022",
                          "Select a Date Range"),
              selected = "Current Year: 2022"
            ),
            
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
            
            h5("Data available since June 7, 2016."),
            
            tags$br(),
            tags$br(),
            
            # ____ Summary statistics ----
            checkboxGroupInput(
              inputId = "matrix",
              label = tags$h4("Summary Statistics:"),
              choices = c("Maximum" = "Maximum",
                          "Mean" = "Mean",
                          "Minimum" = "Minimum"),
              selected = "Mean"),
            
            tags$br(),
            tags$br(),
            
            # ____ Plot types ----
            checkboxGroupInput(
              inputId = "plot_log",
              label = tags$h4("y-axis:"),
              choices = c("Log Scale" = "log"))
            
          ),
          
          shinydashboard::box(
            width = 9,
            #title = "right",
            solidHeader = FALSE,
            
            # ____ Time series plot ----
            tags$h4(p(strong("Time series plot of cyanobacteria abundance (cells/mL) of the selected waterbody."))),
            
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
          
          # _ 4. Copyright and Contacts ----
          shinydashboard::box(
            width = 12,
            #title = "copyright",
            solidHeader = FALSE,
            
            h4("The report is provided by the Oregon DEQ Watershed Management Section. Copyright (C) 2020-2022, ODEQ."),
            h4("The source code of this report is publicly available at GitHub repository: ", 
               a("Satellite Estimates of Cyanobacteria in Oregon Lakes and Reservoirs",
                 href="https://github.com/OR-Dept-Environmental-Quality/CyAN_imagery_update"),"."),
            h4("For more information on this report, please contact"),
            h4("Daniel Sobota, ", a("daniel.sobota@deq.oregon.gov",href="mailto:dan.sobota@deq.oregon.gov")),
            h4("Erin Costello, ", a("erin.costello@deq.oregon.gov",href="mailto:erin.costello@deq.oregon.gov")),
            h4("Yuan Grund, ", a("yuan.grund@deq.oregon.gov",href="mailto:yuan.grund@deq.oregon.gov"))
            
          )
          
        )
        
      )
      
    )
    
  ),
  
  server = function(input, output, session) {
    
    # (1) Plot ----
    # _ Time series plot ----
    pal.plot <- c("orange","blue","green","white","white","white")
    pal.plot <- setNames(pal.plot,unique(sort(dta$`Summary Statistics`)))
    
    yr <- reactive({ 
      
      if(input$ploty == "Current Year: 2022"){"2022"}else{sort(unique(dta$Year))}
      
    })
    
    df <- reactive({
      
      dta %>% 
        dplyr::filter(GNISIDNAME %in% input$waterbody) %>% 
        dplyr::filter(`Summary Statistics` %in% input$matrix) %>% 
        dplyr::filter(Year %in% c(yr())) %>% 
        dplyr::filter(Date >= input$date_plot[1],Date <= input$date_plot[2])
      
    })
    
    type <- reactive({
      
      input$plot_log
      
    })
    
    yaxis <- reactive({
      
      if_else(length(input$plot_log)>0,
              "Cyanobacteria (cells/mL)",
              "Cyanobacteria (cells/mL)")
      
    })
    
    observeEvent(input$waterbody,{
      
      if(input$waterbody == c("Oregon")) {
        
        output$no_plot <- renderText({ 
          
          "Select a waterbody to show the plot."
          
        })
        
      } else {
        
        output$no_plot <- renderText({})
        
        output$plot_cell <- renderPlotly({
          
          plotly::plot_ly(data = df(), x = ~as.Date(Date)) %>% 
            plotly::add_trace(y = ~`Cyanobacteria (cells/mL)`,
                              split = ~`Summary Statistics`,
                              type = "scatter",
                              mode = "lines+markers",
                              #connectgaps = TRUE,
                              color = ~`Summary Statistics`,
                              colors = pal.plot,
                              marker = list(size = 8),
                              legendgroup = "sta") %>% 
            plotly::layout(xaxis = list(title = "Date", range = c(min(df()$Date),max(df()$Date))),
                           # yaxis = list(title = "Cyanobacteria (cells/mL)"),
                           title = as.character(unique(df()$GNISIDNAME))) %>% 
            plotly::layout(yaxis = list(type = type(),
                                        title = yaxis())) %>% 
            plotly::add_trace(y = 100000, mode = "lines",
                              line = list(shape = 'spline', color = '#006d2c', width = 3),
                              name = "High",
                              legendgroup = "high",
                              showlegend = FALSE) %>% 
            plotly::layout(annotations = list(x = max(df()$Date),
                                              y = 100000,
                                              text = "High (100,000)*",
                                              font = list(size = 12),
                                              xref = "x",
                                              yref = "y",
                                              showarrow = TRUE,
                                              arrowhead = 3,
                                              arrowsize = 1,
                                              ax = -60,
                                              ay = -20)) 
          
        })
        
        output$who_line <- renderUI(HTML(paste("&nbsp;","&nbsp;","&nbsp;","&nbsp;",
                                               em("*High (100,000): World Health Organization (WHO) Recreational Use Value (RUV) Guideline for moderate probability of adverse health effects."))))
        
      }
      
    })
    
    # (2) Tables ----
    # _ 7DADM ----
    output$tbl7dadm <- DT::renderDataTable({
      
      DT::datatable(
        data = tbl.data,
        style = 'bootstrap',
        extensions = 'Buttons',
        options = list(dom = 'frtilpB',
                       pageLength = 10,
                       compact = TRUE,
                       nowrap = TRUE,
                       scorllX = TRUE,
                       buttons = list(#'print',
                         list(extend = 'collection',
                              buttons = c('csv','excel','pdf'),
                              text = 'Download')
                       )),
        rownames = FALSE,
        filter = 'bottom'
      ) #%>% 
      #DT::formatDate("Date","toLocaleString")
    }, server = FALSE)
    
    # _ Data table ----
    df_tbl <- reactive({
      
      df() %>% 
        dplyr::select(GNISIDNAME,Date,`Cyanobacteria (cells/mL)`,`Summary Statistics`) %>% 
        dplyr::mutate(`Cyanobacteria (cells/mL)` = ifelse(`Cyanobacteria (cells/mL)` <= 6310, "Non-detect",
                                                          scales::comma(`Cyanobacteria (cells/mL)`))) %>%
        dplyr::rename(Waterbody_GNISID = GNISIDNAME)
    })
    
    observeEvent(input$waterbody,{
      
      if(input$waterbody == c("Oregon")) {
        
        output$no_data <- renderText({ 
          
          "Select a waterbody to show the data table."
          
        })
        
      } else {
        
        output$no_data <- renderText({})
        
        output$caption <- renderUI(HTML(unique((paste0(#"&nbsp;","&nbsp;","&nbsp;","&nbsp;",
          df_tbl()$Waterbody_GNISID,", ",
          format(as.Date(min(df_tbl()$Date)),"%B %d, %Y")," - ",
          format(as.Date(max(df_tbl()$Date)),"%B %d, %Y"))))))
        
        output$table <- DT::renderDataTable({
          
          DT::datatable(
            data = df_tbl(),
            #caption = unique(paste0(df_tbl()$Waterbody_GNISID,", ",
            #                        format(as.Date(min(df_tbl()$Date)),"%B %d, %Y")," - ",
            #                        format(as.Date(max(df_tbl()$Date)),"%B %d, %Y"))),
            style = 'bootstrap',
            extensions = 'Buttons',
            options = list(dom = 'frtilpB',
                           pageLength = 10,
                           compact = TRUE,
                           nowrap = TRUE,
                           scorllX = TRUE,
                           buttons = list(#'print',
                             list(extend = 'collection',
                                  buttons = c('csv','excel','pdf'),
                                  text = 'Download')
                           )),
            rownames = FALSE,
            filter = 'bottom'
          ) #%>% 
          #DT::formatDate("Date","toLocaleString")
        }, server = FALSE)
        
      }
      
    })
    
    # (3) Text: Drinking Water Area ----
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
    
    # (4) Image: Lake images ----
    observeEvent(input$waterbody,{
      
      if(input$waterbody == c("Oregon")) {
        
        output$non_select_image <- renderText({ 
          
          "Select a waterbody to show the lake image."
          
        })
        
      } else {
        
        output$non_select_image <- renderText({})
        
        output$lakeImage <- renderImage({
          
          list(src = paste0("./Lake_Images/",input$waterbody,".jpg"),
               width = 400,
               height = 300)
          
        }, deleteFile = FALSE)
        
      }
      
    })
    
    # (5) Maps ----
    # _ initial map ----
    output$map <- leaflet::renderLeaflet({
      
      #Create a palette function, using the selected color
      palette7dadm <- leaflet::colorFactor(palette = c('red','blue'), 
                                           domain = unique(sort(lakes.resolvable.7dadm$`7dadm`)))
      
      leaflet::leaflet() %>% 
        leaflet::addMapPane("OpenStreetMap", zIndex = -40) %>% 
        leaflet::addMapPane("National Geographic World Map", zIndex = -40) %>%
        leaflet::addMapPane("state.boundary", zIndex = -30) %>%
        leaflet::addMapPane("HUC6",zIndex = -20) %>% 
        leaflet::addMapPane("lakes.resolvable.7dadm", zIndex = 400) %>%
        leaflet::addProviderTiles("OpenStreetMap",group = "OpenStreetMap",
                                  options = pathOptions(pane = "OpenStreetMap")) %>% 
        leaflet::addProviderTiles(providers$Esri.NatGeoWorldMap,group = "National Geographic World Map",
                                  options = pathOptions(pane = "National Geographic World Map")) %>% 
        leaflet::setView(lng = -120, lat = 44, zoom=7) %>% 
        leaflet.extras::addResetMapButton() %>% 
        leaflet::addScaleBar(position = c("bottomright"),
                             options = scaleBarOptions()) %>% 
        leaflet::addMiniMap(position = "bottomright",
                            width = 180,
                            height = 200,
                            zoomLevelFixed = 5) %>% 
        leaflet::addPolygons(data = lakes.resolvable.7dadm, 
                             color = ~palette7dadm(lakes.resolvable.7dadm$`7dadm`),
                             weight = 2,
                             layer = ~lakes.resolvable.7dadm$GNISIDNAME,
                             smoothFactor = 0.5,
                             opacity = 1,
                             fillColor = "transparent",
                             fillOpacity = 1.0,
                             label = ~lakes.resolvable.7dadm$GNIS_Name,
                             labelOptions = labelOptions(style = list("font-size" = "18px",
                                                                      "color" = "blue")),
                             options = pathOptions(pane = "lakes.resolvable.7dadm")) %>% 
        leaflet::addLegend(pal = palette7dadm, 
                           values = lakes.resolvable.7dadm$`7dadm`, 
                           title = "Watebody 7DADM:",
                           position = "topright") %>% 
        leaflet::addPolygons(data = huc6, 
                             group = "Basins (HUC6)",
                             color = "grey",
                             weight = 2,
                             smoothFactor = 0.5,
                             opacity = 0.5,
                             fillColor = ~pal.huc6(HU_6_NAME),
                             fillOpacity = 0.2,
                             label = ~huc6$HU_6_NAME,
                             labelOptions = labelOptions(noHide = TRUE,
                                                         textOnly = TRUE,
                                                         style = list("font-size" = "12px",
                                                                      "color" = "black")),
                             options = pathOptions(pane = "HUC6")) %>% 
        leaflet::addPolygons(data = state.boundary, 
                             color = "black",
                             weight = 2,
                             fillColor = "transparent",
                             fillOpacity = 1.0,
                             options = pathOptions(pane = "state.boundary")) %>% 
        #leaflet::addRasterImage(rst, colors = pal.map, opacity = 1) %>% 
        leaflet::addLayersControl(baseGroups = c("OpenStreetMap","National Geographic World Map"),
                                  overlayGroups = c("Basins (HUC6)"),
                                  position = "topleft",
                                  options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)) %>% 
        leaflet::hideGroup(c("Basins (HUC6)"))
      
    })
    
    # _ map reactive @ waterbody picker ----
    observeEvent(input$waterbody,{
      
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
      
    })
    
    # _ 7 maps ----
    observeEvent(input$waterbody,{
      
      if(input$waterbody == c("Oregon")) {
        
        output$non_select <- renderText({ 
          
          "Select a waterbody to show the maps."
          
        })
        
        
      } else {
        
        output$non_select <- renderText({})
        
        output$maps7 <- renderImage({
          
          list(src = paste0("./Report_Images/",input$waterbody,".png"),
               width = "100%")
          
        }, deleteFile = FALSE)
        
        output$no_pixels <- renderUI(HTML(paste("&nbsp;","&nbsp;","&nbsp;","&nbsp;",
                                                em("No pixels on the map indicates no data for the lake on that day."))))
        
      }
      
    })
    
  }
  
) # shinyApp END
