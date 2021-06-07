library(tidyverse)

lakes.resolvable <- rgdal::readOGR(dsn = "//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/HAB_Shiny_app/data/NHDwaterbody_resolvable_lakes_dissolved_oregon_clean_huc6.shp",
                                   layer = "NHDwaterbody_resolvable_lakes_dissolved_oregon_clean_huc6")

dta2 <- readxl::read_xlsx("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/HAB_Shiny_app/data/HAB_resolvablelakes_2021.xlsx",
                          sheet = "HAB_resolvable_lake_data") %>% 
  dplyr::filter(!GNISIDNAME == "Goose Lake_01520146") %>% # located in the WA state
  dplyr::filter(GNISIDNAME %in% unique(sort(lakes.resolvable@data$GNISIDNAME)))

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

tbl.max.of.daily.mean <- tbl.data.7days %>% 
  dplyr::group_by(GNISIDNAME) %>% 
  dplyr::summarise(max_7DayMean = max(MEAN_cellsml)) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(tbl.data.7days,by="GNISIDNAME") %>% 
  dplyr::filter(max_7DayMean == MEAN_cellsml) %>% 
  dplyr::arrange(desc(max_7DayMean)) %>% 
  dplyr::left_join(lakes.resolvable@data, by = "GNISIDNAME") %>% 
  dplyr::mutate(Basin = ifelse(Name_1 == "Willamette",Name,Name_1)) %>% 
  dplyr::select(GNISIDNAME,Basin,Date,max_7DayMean) %>% 
  dplyr::distinct(GNISIDNAME, .keep_all = TRUE) %>% 
  dplyr::mutate(max_7DayMean = format(round(max_7DayMean,0),big.mark=",",scientific = FALSE)) %>% 
  dplyr::rename(Waterbody_GNISID = GNISIDNAME,
                `Maximum 7 Daily Mean (cells/mL)` = max_7DayMean)

tbl.mean.of.daily.max <- tbl.data.7days %>% 
  dplyr::group_by(GNISIDNAME) %>% 
  dplyr::summarise(mean_7DayMax = mean(MAX_cellsml)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(mean_7DayMax)) %>% 
  dplyr::left_join(lakes.resolvable@data, by = "GNISIDNAME") %>% 
  dplyr::mutate(Basin = ifelse(Name_1 == "Willamette",Name,Name_1)) %>% 
  dplyr::select(GNISIDNAME,Basin,mean_7DayMax) %>% 
  dplyr::distinct(GNISIDNAME, .keep_all = TRUE) %>% 
  dplyr::mutate(mean_7DayMax = format(round(mean_7DayMax,0),big.mark=",",scientific = FALSE)) %>% 
  dplyr::rename(Waterbody_GNISID = GNISIDNAME,
                `Average 7 Daily Maximum (cells/mL)` = mean_7DayMax)
#plot.dta <- dta2 %>% 
#  dplyr::rename(Mean = MEAN_cellsml,
#                Maximum = MAX_cellsml,
#                Minimum = MIN_cellsml) %>% 
#  tidyr::gather(`Summary Statistics`, `Cyanobacteria (cells/mL)`, -GNISIDNAME,-COUNT,-AREA,-PercentArea_Value,-RANGE_cellsml,-STD_cellsml,-Day,-Year,-Date) %>% 
#  tidyr::separate(GNISIDNAME,c("GNISNAME","GNISID"), sep="_") %>% 
#  dplyr::mutate(GNISIDNAME = paste0(GNISNAME,"_",GNISID)) %>% 
#  dplyr::mutate(Date = lubridate::ymd(Date)) %>% 
#  dplyr::arrange(desc(Date)) %>% 
#  dplyr::mutate(wi_DWSA = ifelse(GNISIDNAME %in% GNISNameID, "Yes", "No"))

# cyano.max <- max(plot.dta$log_cyano)

gnisidname <- unique(sort(dta2$GNISIDNAME))

save(lakes.resolvable,
     dta2,
     dta,
     tbl.data.7days,
     tbl.max.of.daily.mean,
     tbl.mean.of.daily.max,
     gnisidname,
     file = "report.RData")
