# Restart R

# install.packages("devtools")  # if not already installed

devtools::install.packages("Rcpp")
devtools::install_version("terra", version = "1.7-83", repos = "http://cran.us.r-project.org")
devtools::install_version("raster", version = "3.6-30", repos = "http://cran.us.r-project.org")

library(raster)
library(terra)
packageVersion("terra")
packageVersion("raster")


devtools::install_version('gert', '2.1.4', repos = "http://cran.us.r-project.org")

# Check renv.lock or DESCRIPTION file and remove no-needed package: eg. gert
file.edit("renv.lock")
lockfile$Packages$gert  # See gert details if present
# "gert": {
#   "Package": "gert",
#   "Version": "2.1.4",
#   "Source": "Repository",
#   "Type": "Package",
#   "Title": "Simple Git Client for R",
#   "Authors@R": "c( person(\"Jeroen\", \"Ooms\", role = c(\"aut\", \"cre\"), email = \"jeroenooms@gmail.com\", comment = c(ORCID = \"0000-0002-4035-0289\")), person(\"Jennifer\", \"Bryan\", role = \"ctb\", email = \"jenny@posit.co\", comment = c(ORCID = \"0000-0002-6983-2759\")))",
#   "Description": "Simple git client for R based on 'libgit2' <https://libgit2.org> with support for SSH and HTTPS remotes. All functions in 'gert' use basic R data  types (such as vectors and data-frames) for their arguments and return values. User credentials are shared with command line 'git' through the git-credential store and ssh keys stored on disk or ssh-agent.",
#   "License": "MIT + file LICENSE",
#   "URL": "https://docs.ropensci.org/gert/, https://ropensci.r-universe.dev/gert",
#   "BugReports": "https://github.com/r-lib/gert/issues",
#   "Imports": [
#     "askpass",
#     "credentials (>= 1.2.1)",
#     "openssl (>= 2.0.3)",
#     "rstudioapi (>= 0.11)",
#     "sys",
#     "zip (>= 2.1.0)"
#   ],
#   "Suggests": [
#     "spelling",
#     "knitr",
#     "rmarkdown",
#     "testthat"
#   ],
#   "VignetteBuilder": "knitr",
#   "Encoding": "UTF-8",
#   "RoxygenNote": "7.3.2.9000",
#   "SystemRequirements": "libgit2 (>= 1.0): libgit2-devel (rpm) or libgit2-dev (deb)",
#   "Language": "en-US",
#   "NeedsCompilation": "yes",
#   "Author": "Jeroen Ooms [aut, cre] (<https://orcid.org/0000-0002-4035-0289>), Jennifer Bryan [ctb] (<https://orcid.org/0000-0002-6983-2759>)",
#   "Maintainer": "Jeroen Ooms <jeroenooms@gmail.com>",
#   "Repository": "CRAN"
# },
# 
# "ggplot2": {

renv::dependencies()
# Finding R package dependencies ... Done!
#   Source
# 1           C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/rsconnect
# 2               C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/app.R
# 3               C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/app.R
# 4               C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/app.R
# 5               C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/app.R
# 6               C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/app.R
# 7               C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/app.R
# 8               C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/app.R
# 9               C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/app.R
# 10              C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/app.R
# 11              C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/app.R
# 12              C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/app.R
# 13              C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/app.R
# 14              C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/app.R
# 15              C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/app.R
# 16              C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/app.R
# 17              C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/app.R
# 18              C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/app.R
# 19              C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/app.R
# 20              C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/app.R
# 21              C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/app.R
# 22              C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/app.R
# 23        C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/data_py.Rmd
# 24        C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/data_py.Rmd
# 25        C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/data_py.Rmd
# 26        C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/data_py.Rmd
# 27        C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/data_py.Rmd
# 28        C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/data_py.Rmd
# 29        C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/data_py.Rmd
# 30        C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/data_py.Rmd
# 31        C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/data_py.Rmd
# 32        C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/data_py.Rmd
# 33        C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/data_py.Rmd
# 34        C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/data_py.Rmd
# 35        C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/data_py.Rmd
# 36        C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/data_py.Rmd
# 37        C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/data_py.Rmd
# 38        C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/data_py.Rmd
# 39        C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/data_py.Rmd
# 40        C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/data_py.Rmd
# 41        C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/data_py.Rmd
# 42        C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/data_py.Rmd
# 43        C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/data_py.Rmd
# 44        C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/data_py.Rmd
# 45 C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/package_versions.R
# 46 C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/package_versions.R
# 47 C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/package_versions.R
# 48          C:/Users/ygrund/OneDrive - Oregon/ygrund_oneDrive/PROJECTS/HABs/HABs_Dashboard/GitHub/CyAN_imagery_update/renv.lock
# Package Require Version   Dev
# 1           rsconnect                 FALSE
# 2               dplyr                 FALSE
# 3                  DT                 FALSE
# 4                glue                 FALSE
# 5          leaflegend                 FALSE
# 6             leaflet                 FALSE
# 7      leaflet.extras                 FALSE
# 8           lubridate                 FALSE
# 9              plotly                 FALSE
# 10              purrr                 FALSE
# 11             raster                 FALSE
# 12             scales                 FALSE
# 13                 sf                 FALSE
# 14              shiny                 FALSE
# 15            shinyBS                 FALSE
# 16    shinycssloaders                 FALSE
# 17     shinydashboard                 FALSE
# 18 shinydashboardPlus                 FALSE
# 19        shinythemes                 FALSE
# 20       shinyWidgets                 FALSE
# 21             tibble                 FALSE
# 22          tidyverse                 FALSE
# 23          rmarkdown                 FALSE
# 24              dplyr                 FALSE
# 25              knitr                 FALSE
# 26             readxl                 FALSE
# 27               curl                 FALSE
# 28              dplyr                 FALSE
# 29             raster                 FALSE
# 30            stringr                 FALSE
# 31              dplyr                 FALSE
# 32          lubridate                 FALSE
# 33            writexl                 FALSE
# 34                zoo                 FALSE
# 35          AWQMSdata                 FALSE
# 36              dplyr                 FALSE
# 37            writexl                 FALSE
# 38              dplyr                 FALSE
# 39            leaflet                 FALSE
# 40          lubridate                 FALSE
# 41       RColorBrewer                 FALSE
# 42             readxl                 FALSE
# 43                 sf                 FALSE
# 44              tidyr                 FALSE
# 45           devtools                 FALSE
# 46             raster                 FALSE
# 47              terra                 FALSE
# 48               renv                 FALSE