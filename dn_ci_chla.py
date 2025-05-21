import sys
import arcpy
from arcpy.sa import *
import pandas as pd
import os
import shutil
import glob
import tempfile
import time

sys.stdout.reconfigure(line_buffering=True)
arcpy.env.overwriteOutput = True
arcpy.CheckOutExtension("Spatial")

zones = r"\\deqhq1\WQ-Share\Harmful Algal Blooms Coordination Team\CyAN_Weekly_Report\Data\gis\CyAN_Waterbodies.shp"
base_path = r"\\deqhq1\wq-share\Harmful Algal Blooms Coordination Team\Satellite data\CyAN_Data_V6\OLCI"
habs_project_path = r"\\deqhq1\WQ-Share\Harmful Algal Blooms Coordination Team\HAB_Shiny_app_chl\data"
cyan_project_path = r"C:\Users\ygrund\OneDrive - Oregon\ygrund_oneDrive\PROJECTS\HABs\HABs_Dashboard\GitHub\CyAN_imagery_update\data"

print("Create a temporary location to store the temporary output tables.")
temp_dir = os.path.join(base_path, "temp")
os.makedirs(temp_dir, exist_ok=True)

print("Initialize empty dataframes to store tabulate and zonal results.")
tabulate_result_df = pd.DataFrame(dtype='object')
zonal_ci_df = pd.DataFrame(dtype='object')
zonal_chl_df = pd.DataFrame(dtype='object')

extent = arcpy.Extent(-2315637.32, 2919264.13, -1566307.49, 2284415.24)

year = 2025
raw_path = os.path.join(base_path, f"raw\\{year}")
dn_path = os.path.join(base_path, f"dn\\{year}")
ci_path = os.path.join(base_path, f"ci\\{year}")
chl_path = os.path.join(base_path, f"chl\\{year}")

temp_tabulate = os.path.join(temp_dir, f"tabulate_result_{year}.dbf")
temp_zonal_ci = os.path.join(temp_dir, f"zonal_ci_{year}.dbf")
temp_zonal_chl = os.path.join(temp_dir, f"zonal_chl_{year}.dbf")

# update r.download_file_list as needed; also update it in Step 3.
# download_file_list = ["2025133"]

from download_list import download_file_list

# for raw_name in download_file_list:
for i, raw_name in enumerate(download_file_list, start=1):
  print(f"[{i}/{len(download_file_list)}] Processing {raw_name}...", flush=True)

  # test: raw_name = '2025133'
  print(raw_name)
  
  raw_tif_path = os.path.join(raw_path, f"{raw_name}.tif")
  output_dn_path = os.path.join(dn_path, f"{raw_name}.tif")
  output_ci_path = os.path.join(ci_path, f"{raw_name}.tif")
  output_chl_path = os.path.join(chl_path, f"{raw_name}.tif")
  
  raw_tif = arcpy.Raster(raw_tif_path)
  print("...Clip the raster for Oregon's extent")
  raw_tif_clip = arcpy.sa.ExtractByRectangle(raw_tif, extent)
  print("...Convert raster values to integer")
  raw_tif_int = arcpy.sa.Int(raw_tif_clip)
  print("...Set 254 and 255 to null")
  outSetNull = arcpy.sa.SetNull(raw_tif_int,raw_tif_int,"Value = 254 Or Value = 255") # 255=no_data 254=land
  print("...Convert DN to CI cells/ml")
  outCellsML = (arcpy.sa.Power(10, (3.0 / 250.0 * outSetNull - 4.2))) * 100000000
  print("...Convert CI to Chl-a ug/L")
  outchla = (outCellsML/100000000)*6620-3.07
  # outchla_corrected = Con(outchla < 0, 0, outchla)
  # outchla_corrected_int = arcpy.sa.Int(outchla_corrected)
  
  print("...Check if the output raster files already exists")
  if arcpy.Exists(output_dn_path): 
    arcpy.management.Delete(output_dn_path)
    
  if arcpy.Exists(output_ci_path):
    arcpy.management.Delete(output_ci_path)
    
  if arcpy.Exists(output_chl_path):
    arcpy.management.Delete(output_chl_path)
    
  print("...Reproject and save the output rasters")
  out_coor_system = (
    "PROJCS['WGS_1984_Web_Mercator_Auxiliary_Sphere',"
    "GEOGCS['GCS_WGS_1984',"
    "DATUM['D_WGS_1984',"
    "SPHEROID['WGS_1984',6378137.0,298.257223563]],"
    "PRIMEM['Greenwich',0.0],"
    "UNIT['Degree',0.0174532925199433]],"
    "PROJECTION['Mercator_Auxiliary_Sphere'],"
    "PARAMETER['False_Easting',0.0],"
    "PARAMETER['False_Northing',0.0],"
    "PARAMETER['Central_Meridian',0.0],"
    "PARAMETER['Standard_Parallel_1',0.0],"
    "PARAMETER['Auxiliary_Sphere_Type',0.0],"
    "UNIT['Meter',1.0]]"
    )
    
  in_coor_system = (
    "PROJCS['USA_Contiguous_Albers_Equal_Area_Conic_USGS_version',"
    "GEOGCS['GCS_North_American_1983',"
    "DATUM['D_North_American_1983',"
    "SPHEROID['GRS_1980',6378137.0,298.257222101]],"
    "PRIMEM['Greenwich',0.0],"
    "UNIT['Degree',0.0174532925199433]],"
    "PROJECTION['Albers'],"
    "PARAMETER['False_Easting',0.0],"
    "PARAMETER['False_Northing',0.0],"
    "PARAMETER['Central_Meridian',-96.0],"
    "PARAMETER['Standard_Parallel_1',29.5],"
    "PARAMETER['Standard_Parallel_2',45.5],"
    "PARAMETER['Latitude_Of_Origin',23.0],"
    "UNIT['Meter',1.0]]"
    )
    
  arcpy.management.ProjectRaster(
    in_raster=outSetNull,
    out_raster=output_dn_path,
    out_coor_system=out_coor_system,
    resampling_type="NEAREST",
    cell_size="300 300",
    geographic_transform="WGS_1984_(ITRF00)_To_NAD_1983",
    Registration_Point="",
    in_coor_system=in_coor_system
    )
      
  arcpy.management.ProjectRaster(
    in_raster=outCellsML,
    out_raster=output_ci_path,
    out_coor_system=out_coor_system,
    resampling_type="NEAREST",
    cell_size="300 300",
    geographic_transform="WGS_1984_(ITRF00)_To_NAD_1983",
    Registration_Point="",
    in_coor_system=in_coor_system
    )

  arcpy.management.ProjectRaster(
    # in_raster=outchla_corrected_int,
    in_raster=outchla,
    out_raster=output_chl_path,
    out_coor_system=out_coor_system,
    resampling_type="NEAREST",
    cell_size="300 300",
    geographic_transform="WGS_1984_(ITRF00)_To_NAD_1983",
    Registration_Point="",
    in_coor_system=in_coor_system
    )
    
  # print("...Copy the CI raster to the HABs project folder")
  # pattern = os.path.join(ci_path, raw_name + ".*")
  # files_to_copy = glob.glob(pattern)
  # for file_path in files_to_copy:
  #   file_name = os.path.basename(file_path)
  #   base_name, extension = file_name.rsplit('.', 1)
  #   copy_ci_path_1 = os.path.join(habs_project_path, str(year), str(file_name))
  #   shutil.copy(file_path, copy_ci_path_1)
  #   
  # print("...Copy the CI raster to the CyAN project folder")
  # file_to_copy = os.path.join(ci_path, raw_name + ".tif")
  # copy_ci_path_2 = os.path.join(cyan_project_path, str(year), raw_name + ".tif")
  # shutil.copy(file_to_copy, copy_ci_path_2)
  
  print("...Copy the Chl-a raster to the HABs project folder")
  file_to_copy = os.path.join(chl_path, raw_name + ".tif")
  copy_chl_path_1 = os.path.join(habs_project_path, str(year), raw_name + ".tif")
  shutil.copy(file_to_copy, copy_chl_path_1)
  # 
  # pattern = os.path.join(chl_path, raw_name + ".*")
  # files_to_copy = glob.glob(pattern)
  # for file_path in files_to_copy:
  #   file_name = os.path.basename(file_path)habs_project_path
  #   base_name, extension = file_name.rsplit('.', 1)
  #   copy_chl_path_1 = os.path.join(, str(year), str(file_name))
  #   shutil.copy(file_path, copy_chl_path_1)
    
  print("...Copy the Chl-a raster to the CyAN project folder")
  file_to_copy = os.path.join(chl_path, raw_name + ".tif")
  copy_chl_path_2 = os.path.join(cyan_project_path, str(year), raw_name + ".tif")
  shutil.copy(file_to_copy, copy_chl_path_2)
      
  print("...Tabulate Area")
  tabulate_result = arcpy.sa.TabulateArea(
    in_zone_data=zones,
    zone_field="GNISIDNAME",
    in_class_data=raw_tif_int, # Use raw_tif_int to include 254 & 255
    class_field="Value",
    out_table=temp_tabulate
    )
      
  print("...Add File_NUM")
  if "File_NUM" not in [f.name for f in arcpy.ListFields(temp_tabulate)]:
    arcpy.AddField_management(temp_tabulate, "File_NUM", "TEXT")
    
  with arcpy.da.UpdateCursor(temp_tabulate, ["File_NUM"]) as cursor:
    for row in cursor:
      row[0] = raw_name
      cursor.updateRow(row)
      
  print("...Convert tabulate result to dataframe and append to the final dataframe")
  tabulate_df = pd.DataFrame(arcpy.da.TableToNumPyArray(tabulate_result, '*'))
  if "OID" in tabulate_df.columns:
    tabulate_df = tabulate_df.drop(columns=['OID'])
  tabulate_result_df = pd.concat([tabulate_result_df, tabulate_df], ignore_index=True)
    
  print("...Zonal Statistics: CI")
  arcpy.sa.ZonalStatisticsAsTable(
    in_zone_data=zones,
    zone_field="GNISIDNAME",
    in_value_raster=outCellsML,
    out_table=temp_zonal_ci,
    ignore_nodata="DATA",
    statistics_type="ALL",
    process_as_multidimensional="CURRENT_SLICE",
    percentile_values=[100],
    percentile_interpolation_type="AUTO_DETECT",
    circular_calculation="ARITHMETIC",
    circular_wrap_value=360,
    out_join_layer=None
    )
      
  print("...Add File_NUM")
  if "File_NUM" not in [f.name for f in arcpy.ListFields(temp_zonal_ci)]:
    arcpy.AddField_management(temp_zonal_ci, "File_NUM", "TEXT")
    
  with arcpy.da.UpdateCursor(temp_zonal_ci, ["File_NUM"]) as cursor:
    for row in cursor:
      row[0] = raw_name
      cursor.updateRow(row)
    
  print("...Convert zonal statistics result to dataframe and append to the final dataframe")
  zonal_df = pd.DataFrame(arcpy.da.TableToNumPyArray(temp_zonal_ci, '*'))
  if "OID" in zonal_df.columns:
    zonal_df = zonal_df.drop(columns=['OID'])
  zonal_ci_df = pd.concat([zonal_ci_df, zonal_df], ignore_index=True)
    
  print("...Zonal Statistics: Chl-a")
  arcpy.sa.ZonalStatisticsAsTable(
    in_zone_data=zones,
    zone_field="GNISIDNAME",
    # in_value_raster=outchla_corrected,
    in_value_raster=outchla,
    out_table=temp_zonal_chl,
    ignore_nodata="DATA",
    statistics_type="ALL",
    process_as_multidimensional="CURRENT_SLICE",
    percentile_values=[100],
    percentile_interpolation_type="AUTO_DETECT",
    circular_calculation="ARITHMETIC",
    circular_wrap_value=360,
    out_join_layer=None
    )
      
  print("...Add File_NUM")
  if "File_NUM" not in [f.name for f in arcpy.ListFields(temp_zonal_chl)]:
    arcpy.AddField_management(temp_zonal_chl, "File_NUM", "TEXT")
    
  with arcpy.da.UpdateCursor(temp_zonal_chl, ["File_NUM"]) as cursor:
    for row in cursor:
      row[0] = raw_name
      cursor.updateRow(row)
    
  print("...Convert zonal statistics result to dataframe and append to the final dataframe")
  zonal_df = pd.DataFrame(arcpy.da.TableToNumPyArray(temp_zonal_chl, '*'))
  if "OID" in zonal_df.columns:
    zonal_df = zonal_df.drop(columns=['OID'])
  zonal_chl_df = pd.concat([zonal_chl_df, zonal_df], ignore_index=True)

  print("...Save intermediate results to CSV after each raw_name")
  tabulate_result_df.to_csv(os.path.join(temp_dir, f"tabulate_result_{raw_name}.csv"), index=False)
  zonal_ci_df.to_csv(os.path.join(temp_dir, f"zonal_ci_{raw_name}.csv"), index=False)
  zonal_chl_df.to_csv(os.path.join(temp_dir, f"zonal_chl_{raw_name}.csv"), index=False)
    
  print("...Clear dataframes for the next iteration")
  tabulate_result_df = pd.DataFrame(dtype='object')
  zonal_ci_df = pd.DataFrame(dtype='object')
  zonal_chl_df = pd.DataFrame(dtype='object')

print("Step 3.1 - conversion is completed")
