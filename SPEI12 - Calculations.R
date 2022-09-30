# -------------------------------------------------------------------------
# UKCP18 SPEI12 Calulations
# Amy Green & Ben Smith
# 21/07/2022
# OpenCLIM Project
# -------------------------------------------------------------------------

# This code takes UKCP18 rainfall and Potential Evapotranspiration data and
# calculates SPEI12 statistics that will be used for reporting the affect
# of climate change on droughts.
# All climate datasets are in water years (they start in November).

# NOTE: ALL WARMING PERIODS THAT EXTEND BEYOND 1980 ARE SIMPLY CUT SHORT. E.g. 08/10/13/15

# Useful resources:
#   - https://core.ac.uk/download/pdf/36144297.pdf
#   - https://cran.r-project.org/web/packages/SPEI/SPEI.pdf
#   - https://hess.copernicus.org/preprints/hess-2022-94/hess-2022-94.pdf
#   - https://climate-scenarios.canada.ca/?page=spei-technical-notes

# Check that this has actually writtent he rasters (didn't the first time as the folders weren't ready)

# Set SPEI Processes ------------------------------------------------------

calculate_spei = T
store_spei = T
read_spei  = F
write_asciis = T
output_path = "I:/SHETRAN_GB_2021/analysis/OpenCLIM July 2022 - Hist and UKCP_01/Ouputs/Climate_Droughts_SPEI/"
# spei_backup_load_path = paste0(output_path, "SPEI 12.csv")


# Preamble --- load packages --------------------------------------------
# install.packages("SPEI")
# install.packages("ncdf4")
# install.packages("raster")
# install.packages("rgdal")
# install.packages("plyr")
# install.packages("abind")
# install.packages("raster")
# install.packages("grid")

library(SPEI)
library(ncdf4)
library(raster)
library(rgdal)
library(plyr)
library(abind)
library(raster)
library(grid)
library(lattice)
library(plotly)


# Preamble --- functions --------------------------------------------------

# Function for reading in netcdf files:
read_netcdf <- function(f, obj) {
  
  nc_data = nc_open(f)
  
  data = ncvar_get(nc_data, obj)
  
  # nc_close(f)
  
  return(data)
}

# Function to accumulate 1 month of daily data to monthly:
# period_indexi: a variable that can be used to set the month/period of interest.
# arr: the array to be used. Time is axis 3.
# days: use to specify if a period other then 30 is desired.
# This runs through the cells and sums each month.
# The result is a list of vectors, each vector represents 1 month of PET, with 1 value per cell.
# Vectors represent cells running from top to bottom and then from left to right.

monthly_acc <- function(period_index, arr, days=30) {
  
  s = rowSums(arr[, , (period_index*days + 1) : ((period_index+1) * days)], dim=2)
  
  s = as.vector(matrix(s, ncol=1))
  
  return(s)
}

# Function to apply SPEI-12 calculations:
# arr: an array of Easting, Northing, time.
# spei_function <- function(arr, start, end) {
#   
#   # Convert into a time series:
#   pixel_data = ts(arr, start=start, end=end, deltat=1/12)
#   
#   # Calculate SPEI12
#   spei12 = spei(pixel_data, 12)$fitted
#   
#   return(as.vector(spei12))
# }

# Function to calculate length statistics for droughts:
length_stats <- function(x, FUN){
  
  runs = rle(x)
  
  # If all values are NAs then return NA instead of -Inf:
  if(all(is.na(runs$values))){
    r=NA
  }else{
    # Else find the max length of TRUE values:
    r = FUN(runs$lengths[runs$values == 1], na.rm=TRUE)
  }
  
  return(r)
}

# Function to flip axies to match another dataset:

grid_apermer = function(data_grid, reference_dataset, grid_3D=FALSE){
  # data_grid: to be changed
  # reference_dataset: matrix/array of same lengths (can be rearranged).
  # grid_3D: is the dataset 3D?
  
  grid_dim = dim(data_grid)
  mask_dim = dim(reference_dataset)
  
  axis_perm = c(which(grid_dim==mask_dim[1]),
                which(grid_dim==mask_dim[2]))
  
  if(grid_3D){axis_perm = c(axis_perm, setdiff(1:3, axis_perm))}
  
  return(aperm(data_grid, axis_perm))
  
}


grid_cropper <- function(data_grid, mask, grid_3D=FALSE){
  # Two objects of same extent and resolution.
  #   data_grid = type: raster or matrix.
  #   mask = type: raster or matrix.
  
  # # Check dimensions match:
  # origional_dims = data_grid
  # data_grid_aperm = grid_apermer(data_grid = data_grid, 
  #                                reference_dataset = mask, 
  #                                grid_3D = grid_3D)
  
  row_crop = which(rowSums(mask)>0)
  col_crop = which(colSums(mask)>0)
  extent_cropper = list("row" = (min(row_crop)-1) : (max(row_crop)+1),
                        "col" = (min(col_crop)-1) : (max(col_crop)+1))
  
  if(any(is(data_grid)=="RasterLayer")){
    
    rast_res = res(data_grid)
    rast_dim = dim(data_grid)
    rast_ext = extent(data_grid)
    
    # xmin
    rast_ext[1] = rast_ext[1] + (min(extent_cropper$col)-1) * rast_res[1]
    # xmax
    rast_ext[2] = rast_ext[2] - (rast_dim[2] - max(extent_cropper$col)) * rast_res[1]
    # ymin
    rast_ext[3] = rast_ext[3] + (rast_dim[1] - max(extent_cropper$row)) * rast_res[2]
    # ymax
    rast_ext[4] = rast_ext[4] - (min(extent_cropper$row)-1) * rast_res[2]
    
    if(grid_3D){
      raster_cropped = as.matrix(data_grid)[extent_cropper$row, extent_cropper$col,]
    }else{
      raster_cropped = as.matrix(data_grid)[extent_cropper$row, extent_cropper$col]
    }
    
    cropped = raster(raster_cropped)
    extent(cropped) = rast_ext
    
  }else{
    if(grid_3D){
      cropped = data_grid[extent_cropper$row, extent_cropper$col,]
    }else{
      cropped = data_grid[extent_cropper$row, extent_cropper$col]
    }
  }
  
  # # Change dimensions back
  # cropped = grid_apermer(data_grid = cropped, 
  #                        reference_dataset = origional_dims, 
  #                        grid_3D = grid_3D)
  
  return(cropped)
  
}


rotate <- function(x){
  t(apply(x, 2, rev))
}

# Calculate Moving average
ma <- function(x, n = 12){
  # https://stackoverflow.com/questions/743812/calculating-moving-average
  stats::filter(x, rep(1 / n, n), sides = 1)
}


# Preamble --- read in datasets -------------------------------------------

# set the working directory
setwd("I:/")

# Load in a shapefile of the UK to check whether the output looks nice over the UK
coast <- readOGR(dsn="I:/GIS Data", layer = "high_water_polyline - simplified")

climate_projections = c("13", "15", "05") #  "01", "04", "06", "07", "08", "09", "10", "11", "12"

# Run through the Climate Projections -------------------------------------
for(rcp in climate_projections){
  
  print(rcp)

  # Set file names for PET data:

  file_names <- c(
    paste0("SHETRAN_GB_2021/ukcp18rcm_pet/", rcp, "/pet/day/latest/pet_rcp85_land-rcm_uk_12km_", rcp, "_day_19801201-19901130.nc"),
    paste0("SHETRAN_GB_2021/ukcp18rcm_pet/", rcp, "/pet/day/latest/pet_rcp85_land-rcm_uk_12km_", rcp, "_day_19901201-20001130.nc"),
    paste0("SHETRAN_GB_2021/ukcp18rcm_pet/", rcp, "/pet/day/latest/pet_rcp85_land-rcm_uk_12km_", rcp, "_day_20001201-20101130.nc"),
    paste0("SHETRAN_GB_2021/ukcp18rcm_pet/", rcp, "/pet/day/latest/pet_rcp85_land-rcm_uk_12km_", rcp, "_day_20101201-20201130.nc"),
    paste0("SHETRAN_GB_2021/ukcp18rcm_pet/", rcp, "/pet/day/latest/pet_rcp85_land-rcm_uk_12km_", rcp, "_day_20201201-20301130.nc"),
    paste0("SHETRAN_GB_2021/ukcp18rcm_pet/", rcp, "/pet/day/latest/pet_rcp85_land-rcm_uk_12km_", rcp, "_day_20301201-20401130.nc"),
    paste0("SHETRAN_GB_2021/ukcp18rcm_pet/", rcp, "/pet/day/latest/pet_rcp85_land-rcm_uk_12km_", rcp, "_day_20401201-20501130.nc"),
    paste0("SHETRAN_GB_2021/ukcp18rcm_pet/", rcp, "/pet/day/latest/pet_rcp85_land-rcm_uk_12km_", rcp, "_day_20501201-20601130.nc"),
    paste0("SHETRAN_GB_2021/ukcp18rcm_pet/", rcp, "/pet/day/latest/pet_rcp85_land-rcm_uk_12km_", rcp, "_day_20601201-20701130.nc"),
    paste0("SHETRAN_GB_2021/ukcp18rcm_pet/", rcp, "/pet/day/latest/pet_rcp85_land-rcm_uk_12km_", rcp, "_day_20701201-20801130.nc")
  )
  
  # Read in PET data:
  data_list <- lapply(file_names, read_netcdf, "pet")
  pet <- abind(data_list)
  cat("PET data has dimentions of: ", dim(pet))
  
      # CHECKER --- 1

  # Read in precipitation data:
  file_names <- c(
    paste0("UKCP18_UK_12km/", rcp, "/pr/day/latest/pr_rcp85_land-rcm_uk_12km_", rcp, "_day_19801201-19901130.nc"),
    paste0("UKCP18_UK_12km/", rcp, "/pr/day/latest/pr_rcp85_land-rcm_uk_12km_", rcp, "_day_19901201-20001130.nc"),
    paste0("UKCP18_UK_12km/", rcp, "/pr/day/latest/pr_rcp85_land-rcm_uk_12km_", rcp, "_day_20001201-20101130.nc"),
    paste0("UKCP18_UK_12km/", rcp, "/pr/day/latest/pr_rcp85_land-rcm_uk_12km_", rcp, "_day_20101201-20201130.nc"),
    paste0("UKCP18_UK_12km/", rcp, "/pr/day/latest/pr_rcp85_land-rcm_uk_12km_", rcp, "_day_20201201-20301130.nc"),
    paste0("UKCP18_UK_12km/", rcp, "/pr/day/latest/pr_rcp85_land-rcm_uk_12km_", rcp, "_day_20301201-20401130.nc"),
    paste0("UKCP18_UK_12km/", rcp, "/pr/day/latest/pr_rcp85_land-rcm_uk_12km_", rcp, "_day_20401201-20501130.nc"),
    paste0("UKCP18_UK_12km/", rcp, "/pr/day/latest/pr_rcp85_land-rcm_uk_12km_", rcp, "_day_20501201-20601130.nc"),
    paste0("UKCP18_UK_12km/", rcp, "/pr/day/latest/pr_rcp85_land-rcm_uk_12km_", rcp, "_day_20601201-20701130.nc"),
    paste0("UKCP18_UK_12km/", rcp, "/pr/day/latest/pr_rcp85_land-rcm_uk_12km_", rcp, "_day_20701201-20801130.nc")
  )
  
  data_list <- lapply(file_names, read_netcdf, "pr")
  pr <- abind(data_list)
  cat("Precipitation (pr) data has dimentions of: ", dim(pr))
  
  # Get coordinates of the climate data:
  x_coords = read_netcdf(file_names[1], "projection_x_coordinate_bnds")
  y_coords = read_netcdf(file_names[1], "projection_y_coordinate_bnds")
  grid_extent = c(min(x_coords), max(x_coords), min(y_coords), max(y_coords))

  # Create Mask -------------------------------------------------------------
  
  # Read in a raster mask of the UK:
  UK_mask_full_extent <- rotate(as.matrix(read.table("GIS Data/UK mask UKCP18 extent BNG.asc", skip = 5)))
  
      # CHECKER 2 ---
  
  uk_mask_raster = raster(t(UK_mask_full_extent[,ncol(UK_mask_full_extent):1]))
  extent(uk_mask_raster) <- grid_extent
  
    # CHECKER 3 ---

  # Crop All Datasets -------------------------------------------------------
      
  pr = grid_cropper(pr, UK_mask_full_extent, grid_3D = TRUE)
  pet = grid_cropper(pet, UK_mask_full_extent, grid_3D = TRUE)
  
      # CHECKER 4 ---
  
  uk_mask_raster = grid_cropper(data_grid = uk_mask_raster, mask = uk_mask_raster)
    
    # CHECKER 5 ---
  
  UK_mask = grid_cropper(data_grid = UK_mask_full_extent, mask = UK_mask_full_extent)
    
    # CHECKER 6 ---
  
  # Update the extent to the new, masked layer:
  grid_extent = extent(uk_mask_raster)
  
  # Mask the data as well to remove values in the sea:
  pr_masked = pr ; pr_masked[!UK_mask] = 0
  pet_masked  = pet ; pet_masked[!UK_mask] = 0
  
      # CHECKER 7 ---
  
  # Calculate the Climate Balance -------------------------------------------
  
  # Convert the daily Precipitation and PET data to monthly Climate Balance data:
  climate_balance_daily <- pr_masked - pet_masked
      #  climate_balance_daily[,1,1] is the bottom row
      #  climate_balance_daily[1,,1] is the left column
  
      # CHECKER 8---
  
  # Calculate the number of months in the Climate Balance dataset:
  n_months <- (dim(climate_balance_daily)[3] / 30) - 1 # -1 so that we start at 0 months/days, not 1 month/30 days
  cat(paste0("There are ", n_months+1), "months in the dataset")
  
  # Accumulate the Climate Balance data to monthly values:
  monthly_list <- lapply(0:n_months, monthly_acc, climate_balance_daily) # Amy, I changed to 0 from 1 as we want to start at 0?
    # str(monthly_list[1:3]) # This creates a list of vectors, each one representing 1 month - each value is monthly PET
    # These are ordered from south to north, from east to west.
    # So the first value that is positive in monthly_list[[1]] is sum(climate_balance_daily[8,2,1:30])
    # Then: sum(climate_balance_daily[9,2,1:30]) > sum(climate_balance_daily[16,2,1:30]) > sum(climate_balance_daily[9,3,1:30])
  
  # TODO - ERROR this conversion to a dataframe creates a load of NAs, that break the SPEI function.
  climate_balance_monthy <- t(data.frame(monthly_list))
    # Again, ordered S>N, E>W
    # rownames(climate_balance_monthy) = NULL
  
  cat(
    "Climate Balance (climate_balance_monthy) data has",
    dim(climate_balance_monthy)[1], "months (rows) and", dim(climate_balance_monthy)[2], "cells (columns)."
  )
  
    # CHECKER 9 ---
  
  # Calculate SPEI-12 -------------------------------------------------------
  
  # Set the start and end dates of the datasets:
  start <- c(1980, 12)
  end   <- c(2080, 11)
  
  # Convert the Climate Balance into a time series:
  climate_balance_ts <- ts(climate_balance_monthy, start=start, end=end, frequency=12) 
  # ^^ I don't think this actually uses the dates, as these are incorrect anyway
  
  # Calculate SPEI12 - this is slow as I think it processes the masked 0s :(
  if(calculate_spei==TRUE){
    spei_full <- spei(data=climate_balance_ts, scale=12) # 
    
    # Extract just the data part of this:
    spei12 <- spei_full$fitted
    # print(spei12[1:20,1:5]) # data[rows (months), columns (cells)]
    # str(spei12)
    
    print("SPEI12 calculated...")
  }


  # Save SPEI as csv --------------------------------------------------------
  spei_backup_load_path = paste0(output_path, "UKCP18_", rcp, " SPEI 12.csv")

  if(store_spei==TRUE){
    write.csv(spei12, spei_backup_load_path, row.names=FALSE)
  }
  
  if(read_spei==TRUE){
    spei12 = as.matrix(read.csv(file = spei_backup_load_path, header = TRUE))
    colnames(spei12) = NULL
    str(spei12)
  }


  # Reshape SPEI 12 ----------------------------------------------------------
  
  # Reshape SPEI-12 array to be same dimensions as input data:
  spei12_reshape <- array(spei12, # don't need to vectorise: as.vector(spei12)
                  dim=c(dim(climate_balance_monthy)[1], # months 
                        dim(climate_balance_daily)[1], # columns (Easting) 
                        dim(climate_balance_daily)[2]) # rows (Northing)
  )
  
  spei_map <- aperm(spei12_reshape, c(2, 3, 1))

    # CHECKER 10 ---

  # Plot Average SPEI 12 --- [surplus] ---------------------------------------
  
  # spei_mean_raster <- rowMeans(spei_map, dims=2, na.rm=TRUE)
  # spei_mean_raster <- flip(t(raster(spei_mean_raster, crs=27700)))
  # extent(spei_mean_raster) <- grid_extent
  # 
  # plot(spei_mean_raster, main="Average SPEI-12", 
  #      legend.args=list(text="", side=4, font=2, line=2.5, cex=0.8))


  # Calculate Drought Statistics --------------------------------------------
  
  # Create an indicator array showing droughts (SPEI < threshold)
  threshold = -1
  
  # Plot SPEI time series scatter plot(1 cell)
  plot(ts(spei_map[30,25,]))#, title="Average SPEI @ [30,25,:]")
  abline(h=threshold, col="red")
  # plot(ma(climate_balance_monthy_map[30, 25,]), col = "blue")
  
  
  # Create indicator variable for if it is in drought:
  drought_indicator <- spei_map[,,] <= threshold # 12:n_months if you want to Remove initial NA Values from the data.
  # dim(drought_indicator)
  # ind_1s <- array(0, dim=dim(drought_indicator))
  # ind_1s[drought_indicator] = 1
  
  # Calculate the probability that a month is in drought:
  probability_of_drought_grid <- rowMeans(drought_indicator, na.rm=T, dim=2)
  
  
  # Plot Climate Data -------------------------------------------------------
  
  pr_plot = flip(t(raster(pr_masked[,,480], crs=27700)))
  pet_plot = flip(t(raster(pet_masked[,,480], crs=27700)))
  cb_plot = climate_balance_daily[,,480]
  cb_plot[!UK_mask] = NA
  cb_plot = flip(t(raster(cb_plot, crs=27700)))
  
  extent(pr_plot) = grid_extent
  extent(pet_plot) = grid_extent
  extent(cb_plot) = grid_extent
  
  # Create Plots:
  par(mfrow=c(1, 3), mar = c(3,3,5,2))
  
  plot(pr_plot, main="Precipitation\n(mm/day)", 
       legend.args=list(text="", side=4, font=2, line=2, cex=1))
  plot(coast, add=TRUE)
  
  plot(pet_plot, main="Potential Evapotranspiration\n(mm/day)", 
       legend.args=list(text="", side=4, line=2.5, cex.axis=3))
  plot(coast, add=TRUE)
  
  plot(cb_plot, main="Climate Balance\n(mm/day)", 
       legend.args=list(text="", side=4, font=4, line=2.5, 
                        cex.lab=3, cex.axis=3, cex.main=3, cex.sub=3))
  plot(coast, add=TRUE)
  
  # Plot Drought Statistics -------------------------------------------------
  # [WHOLE PERIOD]
  
  # Probability that a month is a drought:
  drought_probability_raster <- flip(t(raster(probability_of_drought_grid, crs=27700)))
  extent(drought_probability_raster) = grid_extent
  
  # Average drought length for each pixel over the data:
  mean_drought_length <- apply(drought_indicator, c(1,2), length_stats, mean)
  mean_drought_length_raster <- flip(t(raster(mean_drought_length, crs=27700)))
  extent(mean_drought_length_raster) <- grid_extent
  
  # Maximum drought length for each pixel over the data:
  max_drought_length <- apply(drought_indicator, c(1,2), length_stats, max)
  max_drought_length_raster <- flip(t(raster(max_drought_length, crs=27700)))
  extent(max_drought_length_raster) <- grid_extent
  
  # Create Plots:
  par(mfrow=c(1, 3), mar = c(6,5,5,2))
  
  plot(drought_probability_raster,
       main="Probability a month\nis in drought", 
       legend.args=list(text="", side=4, font=2, line=2.5, cex=2))
  plot(coast, add=TRUE)
  
  plot(mean_drought_length_raster, 
       main="Average drought length", 
       legend.args=list(text="Months", side=4, font=2, line=2.5, cex=0.8))
  plot(coast, add=TRUE)
  
  plot(max_drought_length_raster, 
       main="Maximum drought length", 
       legend.args=list(text="Months", side=4, font=2, line=2.5, cex=0.8))
  plot(coast, add=TRUE)


  # Create Time Averaged Metrics --------------------------------------------

  if(write_asciis==TRUE){
  
    # Create metrics consisting of a single number for each period. The number 
    # represents the average for all grids and all times within that period.
    
    # get length of decades (there are 10 decades):
    dec_len = dim(drought_indicator)
    
    decades = c("19801201-19901130", "19901201-20001130", "20001201-20101130",
                "20101201-20201130", "20201201-20301130", "20301201-20401130", 
                "20401201-20501130", "20501201-20601130", "20601201-20701130", 
                "20701201-20801130")
    
    periods = c("19801201-20101130", "19901201-20201130", "20001201-20301130",
                "20101201-20401130", "20201201-20501130", "20301201-20601130",
                "20401201-20701130", "20501201-20801130")
    
    
    warming_levels = c("1.5", "2.0", "2.5", "3.0", "3.5", "4.0")
    warming_database = data.frame(
      "rcm_01" = c(2006, 2016, 2026, 2034, 2042, 2049),
      "rcm_04" = c(2003, 2013, 2023, 2031, 2039, 2046),
      "rcm_05" = c(2007, 2018, 2028, 2037, 2044, 2051),
      "rcm_06" = c(2005, 2016, 2025, 2034, 2042, 2049),
      "rcm_07" = c(2005, 2017, 2027, 2036, 2043, 2050),
      "rcm_08" = c(2006, 2018, 2029, 2038, 2047, 2055),
      "rcm_09" = c(2004, 2014, 2023, 2030, 2037, 2044),
      "rcm_10" = c(2008, 2018, 2027, 2036, 2045, 2052),
      "rcm_11" = c(2004, 2015, 2025, 2034, 2042, 2050),
      "rcm_12" = c(2010, 2020, 2030, 2038, 2045, 2052),
      "rcm_13" = c(2005, 2016, 2026, 2035, 2043, 2050),
      "rcm_15" = c(2006, 2019, 2030, 2038, 2046, 2054), 
      row.names = warming_levels
    )
    
    # Set up directories for holding data will create warning if it already exists:
    dir.create(paste0(output_path, "Probability of Climatic Drought - Gridded - ", rcp, "/"))
    dir.create(paste0(output_path, "Average Length of Climatic Drought - Gridded - ", rcp, "/"))
    dir.create(paste0(output_path, "Maximum Length of Climatic Drought - Gridded - ", rcp, "/"))
  
    # Create grids of drought probability for the different periods (10 decades):
    for(i in 1:10){
      
      # --- DECADES--- 
      
      t1 = 120*(i-1)+1
      t2 = 120*i # 10yrs
      
      # Probability that a month is a drought:
      temp_raster = flip(t(raster(rowMeans(drought_indicator[,,t1:t2], na.rm=T, dim=2), crs=27700)))
      extent(temp_raster) = grid_extent
    
      writeRaster(temp_raster, format='ascii', overwrite=TRUE,
                  filename=paste0(output_path, "Probability of Climatic Drought - Gridded - ", rcp, "/drought_probability_raster_BC04_", decades[i], ".asc"))
      
      
      # Average drought length for each pixel over the data:
      temp_raster <- flip(t(raster(apply(drought_indicator[,,t1:t2], c(1,2), length_stats, mean), crs=27700)))
      extent(temp_raster) <- grid_extent
      
      writeRaster(temp_raster, format='ascii', overwrite=TRUE,
                  filename=paste0(output_path, "Average Length of Climatic Drought - Gridded - ", rcp, "/drought_mean_length_raster_", rcp, "_", decades[i], ".asc"))
      
      # Maximum drought length for each pixel over the data:
      # Supresses warning: no non-missing arguments to max; returning -Inf (fixed below)
      temp_raster <- apply(drought_indicator[,,t1:t2], c(1,2), length_stats, max)
      temp_raster[temp_raster==-Inf]=0 ; temp_raster[!UK_mask] = NA
      temp_raster <- flip(t(raster(temp_raster, crs=27700)))
      extent(temp_raster) <- grid_extent
      
      writeRaster(temp_raster, format='ascii', overwrite=TRUE,
                  filename=paste0(output_path, "Maximum Length of Climatic Drought - Gridded - ", rcp, "/drought_max_length_raster_", rcp, "_", decades[i], ".asc"))
    }
    
      # --- 30 YEAR ROLLING PERIODS---
    
    for(i in 1:10){
      t1 = 120*(i-1)+1
      t2 = 120*(i+2) # 30 yrs
      
      # If there are still periods within the range:
      if(t2<=dec_len[3]){
        
        temp_raster = flip(t(raster(rowMeans(drought_indicator[,,t1:t2], na.rm=T, dim=2), crs=27700)))
        extent(temp_raster) = grid_extent
        
        writeRaster(temp_raster,
                    filename=paste0(output_path, "Probability of Climatic Drought - Gridded - ", rcp, "/drought_probability_raster_", rcp, "_", periods[i], ".asc"),
                    format='ascii', overwrite=TRUE)
        
        # Average drought length for each pixel over the data:
        temp_raster <- flip(t(raster(apply(drought_indicator[,,t1:t2], c(1,2), length_stats, mean), crs=27700)))
        extent(temp_raster) <- grid_extent
        
        writeRaster(temp_raster, format='ascii', overwrite=TRUE,
                    filename=paste0(output_path, "Average Length of Climatic Drought - Gridded - ", rcp, "/drought_mean_length_raster_", rcp, "_", periods[i], ".asc"))
        
        # Maximum drought length for each pixel over the data:
        # Supresses warning: no non-missing arguments to max; returning -Inf (fixed below)
        temp_raster <- apply(drought_indicator[,,t1:t2], c(1,2), length_stats, max)
        temp_raster[temp_raster==-Inf]=0 ; temp_raster[!UK_mask] = NA
        temp_raster <- flip(t(raster(temp_raster, crs=27700)))
        extent(temp_raster) <- grid_extent
        
        writeRaster(temp_raster, format='ascii', overwrite=TRUE,
                    filename=paste0(output_path, "Maximum Length of Climatic Drought - Gridded - ", rcp, "/drought_max_length_raster_", rcp, "_", periods[i], ".asc"))
        
      
      }
    }
    
    # --- WARMING PERIODS ---
    
    for(i in warming_levels){
      
      start_year = warming_database[which(rownames(warming_database)==i),
                                    which(colnames(warming_database)==paste0("rcm_", rcp))]
      wp = 12*(start_year-1980)
      wp = c((wp+1) : min((wp+30*12), dim(drought_indicator)[3])) # add 30 yrs
      
      temp_raster = flip(t(raster(rowMeans(drought_indicator[,,wp], na.rm=T, dim=2), crs=27700)))
      extent(temp_raster) = grid_extent
      
      writeRaster(temp_raster, format='ascii', overwrite=TRUE,
                  filename=paste0(output_path, "Probability of Climatic Drought - Gridded - ", rcp, "/drought_probability_raster_", rcp, "_", as.character(start_year), "-", as.character(start_year+30), ".asc"))
      
      # Average drought length for each pixel over the data:
      temp_raster <- flip(t(raster(apply(drought_indicator[,,wp], c(1,2), length_stats, mean), crs=27700)))
      extent(temp_raster) <- grid_extent
      
      writeRaster(temp_raster, format='ascii', overwrite=TRUE,
                  filename=paste0(output_path, "Average Length of Climatic Drought - Gridded - ", rcp, "/drought_mean_length_raster_", rcp, "_", as.character(start_year), "-", as.character(start_year+30), ".asc"))
      
      # Maximum drought length for each pixel over the data:
      # Supresses warning: no non-missing arguments to max; returning -Inf (fixed below)
      temp_raster <- apply(drought_indicator[,,wp], c(1,2), length_stats, max)
      temp_raster[temp_raster==-Inf]=0 ; temp_raster[!UK_mask] = NA
      temp_raster <- flip(t(raster(temp_raster, crs=27700)))
      extent(temp_raster) <- grid_extent
      
      writeRaster(temp_raster, format='ascii', overwrite=TRUE,
                  filename=paste0(output_path, "Maximum Length of Climatic Drought - Gridded - ", rcp, "/drought_max_length_raster_", rcp, "_", as.character(start_year), "-", as.character(start_year+30), ".asc"))
      
    }
  }
}

