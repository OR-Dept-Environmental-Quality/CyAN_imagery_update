library(tidyverse)
library(sp)
library(sf)
library(leaflet)
library(mapview)
library(htmltools)

lakes.resolvable <- rgdal::readOGR(dsn = "//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/HAB_Shiny_app/data/NHDwaterbody_resolvable_lakes_dissolved_oregon_clean_huc6.shp",
                                   layer = "NHDwaterbody_resolvable_lakes_dissolved_oregon_clean_huc6")

state.boundary <- sf::st_read("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/HAB_Shiny_app/data/state_boundary_blm.shp") %>% 
  st_transform(crs="+init=epsg:4326")

# Example dataset for a html test
dta2 <- readxl::read_xlsx("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/HAB_Shiny_app/data/HAB_resolvablelakes_2016_2021.xlsx",
                          sheet = "HAB_resolvablelakes_2016_2021") %>% 
  dplyr::filter(!GNISIDNAME == "Goose Lake_01520146") %>% # located in the WA state
  dplyr::filter(GNISIDNAME %in% unique(sort(lakes.resolvable@data$GNISIDNAME))) %>% 
  dplyr::filter(Year == "2021") %>% #
  dplyr::filter(Day < 215)          #

dta3 <- readxl::read_xlsx("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/HAB_Shiny_app/data/Resolvable_Lakes.xlsx",
                          sheet = "cyan_resolvable_lakes")

GNISNameID <- unique(sort(dta3$wi_DWSA))

dta <- dta2 %>% 
  dplyr::rename(Mean = MEAN_cellsml,
                Maximum = MAX_cellsml) %>% 
  tidyr::gather(`Summary Statistics`, `Cyanobacteria (cells/mL)`, -GNISIDNAME,-COUNT,-AREA,-PercentArea_Value, -Day,-Year,-Date) %>% 
  tidyr::separate(GNISIDNAME,c("GNISNAME","GNISID"), sep="_") %>% 
  dplyr::mutate(GNISIDNAME = paste0(GNISNAME,"_",GNISID)) %>% 
  dplyr::mutate(Date = lubridate::ymd(Date)) %>% 
  dplyr::arrange(desc(Date)) %>% 
  dplyr::mutate(wi_DWSA = ifelse(GNISIDNAME %in% GNISNameID, "Yes", "No")) %>% 
  dplyr::filter(`Summary Statistics` %in% c("Mean","Maximum"))

# OR report ----
tbl.data.7days <- dta2 %>% 
  dplyr::arrange(GNISIDNAME, desc(Date)) %>% 
  dplyr::filter(as.Date(Date) <= as.Date(max(dta2$Date)) & as.Date(Date) >= as.Date(max(dta2$Date))-7)

tbl.mean.of.daily.max <- tbl.data.7days %>% 
  dplyr::group_by(GNISIDNAME) %>% 
  dplyr::summarise(mean_7DayMax = mean(MAX_cellsml)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(mean_7DayMax)) %>% 
  dplyr::left_join(lakes.resolvable@data, by = "GNISIDNAME") %>% 
  dplyr::mutate(Basin = ifelse(Name_1 == "Willamette",Name,Name_1)) %>% 
  dplyr::select(GNISIDNAME,Hydro_Type,Basin,mean_7DayMax) %>% 
  dplyr::distinct(GNISIDNAME, .keep_all = TRUE)  

num <- nrow(tbl.mean.of.daily.max[which(tbl.mean.of.daily.max$mean_7DayMax>=100000),])
#numb <- nrow(tbl.7dmdm[which(!tbl.7dmdm$`7-Day Average Daily Maximum (cells/mL)` == "Non-detect"),])

tbl.7dmdm <- tbl.mean.of.daily.max %>%
  dplyr::mutate(mean_7DayMax = ifelse(mean_7DayMax<= 6310, "Non-detect",
                                      format(round(mean_7DayMax,0),big.mark=",",scientific = FALSE))) %>% 
  dplyr::rename(Waterbody_GNISID = GNISIDNAME,
                `Hydrographic Type` = Hydro_Type,
                `7-Day Average Daily Maximum (cells/mL)` = mean_7DayMax)

gnisidname <- unique(sort(dta2$GNISIDNAME))

caption.or <- paste0("Waterbodies ranked by the 7-Day Average Daily Maximum of cyanobacteria abundance (cells/mL) ",
                     "that are above the WHO guideline (100,000 cells/mL) for cyanobacteria in recreational freshwater during the 7 days from ", 
                     as.Date(max(dta2$Date))-7, " to ",max(dta2$Date), 
                     ". The waterbody hydrographic types and basin names are shown in the table.")

# Waterbody images ----

tag.map.date <- tags$style(HTML("
  .leaflet-control.map-date { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 0%;
    text-align: left;
    padding-left: 5px; 
    padding-right: 5px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))

thevalues <-c(0,6310,13000,25000,50000,100000,200000,400000,800000,1000000,3000000,6000000,7000000)
paletteFunc <- grDevices::colorRampPalette(c('gray','yellow','orange','dark red'))
palette     <- paletteFunc(12)
pal.map <- leaflet::colorBin(palette = palette,
                             bins = c(0,6310,13000,25000,50000,100000,200000,400000,800000,1000000,3000000,6000000,7000000),
                             domain = c(0,6310,13000,25000,50000,100000,200000,400000,800000,1000000,3000000,6000000,7000000),
                             na.color = "transparent")

# Data subset for testing:
#subset <- lakes.resolvable[which(lakes.resolvable@data$GNISIDNAME %in% c("Upper Klamath Lake_01151685", "Detroit Lake_01639301")),]
waterbody.list <- sort(unique(lakes.resolvable@data$GNISIDNAME))

# Dates:
fulldays <- readxl::read_xlsx("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/HAB_Shiny_app/data/calendar-dates.xlsx",
                              sheet = "calendar-dates") %>% 
  dplyr::mutate(Date = lubridate::ymd(Date))

lookup.date <- dta %>% 
  dplyr::group_by(Date, Year, Day) %>% 
  dplyr::summarise(n=n()) %>% 
  dplyr::right_join(fulldays, by="Date") %>% 
  dplyr::rename(Year.dta = Year.x,
                Day.dta = Day.x,
                Year.fulldays = Year.y,
                Day.fulldays = Day.y)

last7days <- sort(unique(as.Date(tbl.data.7days$Date)))

map.file.name <- data.frame(File_waterbody = character(),
                            File_name = character())

for (x in 1:length(waterbody.list)){
  
  # test: x <- 1
  # test: y <- 3
  
  print(waterbody.list[x])
  
  map.file.name.x <- data.frame(File_waterbody = character(),
                                File_name = character())
  
  for(y in 1:length(last7days)){
    
    print(last7days[y])
    
    title.date <- tags$div(tag.map.date, HTML(paste0(last7days[y])))
    
    one.lake <- lakes.resolvable[which(lakes.resolvable@data$GNISIDNAME == waterbody.list[x]),]
    
    bounds <- data.frame(sp::bbox(one.lake))
    
    df.map.date <- lookup.date %>% 
      dplyr::filter(Date %in% as.Date(last7days[y])) %>% 
      dplyr::mutate(Day.dta = ifelse(Day.dta < 10, paste0("00",as.character(Day.dta)),
                                     ifelse((Day.dta >= 10 & Day.dta < 100), paste0("0",as.character(Day.dta)), Day.dta))) %>% 
      dplyr::mutate(map_day = paste0(Year.dta,Day.dta))
    
    tif.dir <- paste0("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/HAB_Shiny_app/data/", df.map.date$Year.dta, "/")
    file.name <- paste0(df.map.date$map_day,".tif")
    
    rst <- raster::raster(paste0(tif.dir,file.name))
    
    map <- leaflet::leaflet() %>% 
      leaflet::addControl(title.date, position = "topleft", className="map-date")%>% 
      leaflet::addProviderTiles("OpenStreetMap",group = "OpenStreetMap") %>% 
      leaflet::addRasterImage(rst, layerId = "Value", project = FALSE, colors=pal.map, opacity = 1) %>% 
      leaflet::addLegend(pal = pal.map, values = thevalues, title = "Cyanobacteria (cells/mL)", position = "bottomright") %>% 
      leaflet::addPolygons(data = lakes.resolvable, 
                           color = "blue",
                           weight = 2,
                           layer = ~lakes.resolvable$GNISIDNAME,
                           smoothFactor = 0.5,
                           opacity = 0.5,
                           fillColor = "transparent",
                           fillOpacity = 1.0,
                           label = ~lakes.resolvable$GNIS_Name,
                           labelOptions = labelOptions(style = list("font-size" = "18px",
                                                                    "color" = "blue"))) %>% 
      leaflet::fitBounds(lng1=bounds$min[1], lat1=bounds$min[2], lng2=bounds$max[1], lat2=bounds$max[2])
    
    mapview::mapshot(map, file = paste0("./Images/",waterbody.list[x],"-",last7days[y],".jpg"))
    
    map.file.name.y <- paste0("./Images/",waterbody.list[x],"-",last7days[y],".jpg")
    
    map.file.name.x <- map.file.name.x %>% 
      dplyr::add_row(File_waterbody = waterbody.list[x],
                     File_name = map.file.name.y)
    
  }
  
  map.file.name <- rbind(map.file.name,map.file.name.x)
  
}

save(lakes.resolvable,
     dta,
     dta2,
     tbl.data.7days,
     tbl.mean.of.daily.max,
     num,
     tbl.7dmdm,
     gnisidname,
     caption.or,
     map.file.name,
     file = "report.RData")

# test ----
library(magick)

for(i in sort(unique(map.file.name$File_waterbody))){
  
  # test: i <- "Odell Lake_01147159"
  
  df.imgs <- map.file.name %>% dplyr::filter(File_waterbody == i)
  img.files <- c(df.imgs$File_name[1:7],"./Images/legend.jpg")
  imgs <- image_read(img.files)
  imgs.comb <- image_montage(imgs, tile = '4x2', geometry = "x200+3+5")
  image_write(imgs.comb, path = paste0("./Report_Images/",i,".png"), format = "png")

}

