# Script to run python script used to update satellite data on cyanobacteria

Sys.setenv(RETICULATE_PYTHON = "C:\\Program Files\\ArcGIS\\Pro\\bin\\Python\\envs\\arcgispro-py3\\python.exe") # Need to correct python default in R

# Need to use version 1.22 for now.
#require(devtools)
#install_version("reticulate", version = "1.22", repos = "http://cran.us.r-project.org")

library(reticulate)   # Bridging R and Python

# Need to point to the ArcPro Python version - Change as needed
use_python("C:\\Program Files\\ArcGIS\\Pro\\bin\\Python\\envs\\arcgispro-py3\\python.exe",
           required = T)

# Runs python script
py_run_file("DEQ_CyAN_HABs_Get_NASA_Data.py")
