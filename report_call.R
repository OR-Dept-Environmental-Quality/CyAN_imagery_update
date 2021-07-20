library(rmarkdown)
library(officedown)
library(tidyverse)

# OR Report ----
rmarkdown::render(input="reportOR.Rmd",
                  output_format = "officedown::rdocx_document",
                  output_dir = "./Reports/",
                  output_file= paste0("Satellite imagery of cyanobacteria for Oregon waterbodies_", Sys.Date(),".docx"))

# Region Reports ----
region <- c("Eastern Region","Northwest Region","Western Region")
bc <- read.csv("bc.csv")

for (r in region) {
  
  # test: 
  # r <- "Eastern Region"
  # r <- "Northwest Region"
  # r <- "Western Region"
  
  print(r)
  load("report.RData")

  tbl.mean.of.daily.max <- tbl.data.7days %>% 
    dplyr::group_by(GNISIDNAME) %>% 
    dplyr::summarise(mean_7DayMax = mean(MAX_cellsml)) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(desc(mean_7DayMax)) %>% 
    dplyr::left_join(lakes.resolvable@data, by = "GNISIDNAME") %>% 
    dplyr::filter(Region == r) %>% # Region selection
    dplyr::mutate(Basin = ifelse(Name_1 == "Willamette",Name,Name_1)) %>% 
    dplyr::select(GNISIDNAME,Hydro_Type,Basin,mean_7DayMax) %>%  
    dplyr::distinct(GNISIDNAME, .keep_all = TRUE) 
  
  num <- nrow(tbl.mean.of.daily.max[which(tbl.mean.of.daily.max$mean_7DayMax>=100000),])
  
  tbl.7dmdm <- tbl.mean.of.daily.max %>%
    dplyr::mutate(mean_7DayMax = ifelse(mean_7DayMax<= 6310, "Non-detect",
                                        format(round(mean_7DayMax,0),big.mark=",",scientific = FALSE))) %>% 
    dplyr::rename(Waterbody_GNISID = GNISIDNAME,
                  `Hydrographic Type` = Hydro_Type,
                  `Average 7 Daily Maximum (cells/mL)` = mean_7DayMax)

  dta2.mean.of.daily.max <- dta2 %>% 
    dplyr::filter(GNISIDNAME %in% tbl.mean.of.daily.max$GNISIDNAME)
  
  gnisidname <- unique(sort(dta2.mean.of.daily.max$GNISIDNAME))
  
  bc_region <- bc %>% dplyr::filter(REGION == r)
  
  save(r,
       bc_region,
       dta2,
       num,
       tbl.7dmdm,
       gnisidname,
       caption_1,
       caption_2,
       caption_3,
       email.address_ER,
       email.address_NWR,
       email.address_WR,
       email.address_cc,
       file = "report_region.RData")
  
  rmarkdown::render(input="reportRegions.Rmd",
                    output_format = "officedown::rdocx_document",
                    output_dir = "./Reports/",
                    output_file= paste0("Satellite imagery of cyanobacteria for ", r, " waterbodies_", Sys.Date(),".docx"))
  
}
