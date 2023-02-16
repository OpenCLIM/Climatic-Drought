# UKCP18 SPEI Timeseries Analysis -----------------------------------------
# Script 2

# Ben Smith
# Newcastle University
# 15/02/2023

# Notes -------------------------------------------------------------------

# This script will restructure the climate .asc files into Excel metrics
# for plotting.

# This script assumes that all the .asc files read in are the same size 
# and shape. If this is not true for some reason, you will have to put more
# code into using the header coordinates.

# Preamble ----------------------------------------------------------------

library(tidyr)
library(dplyr)

# Specify root folder (maybe surplus if using project):
root <- "I:/SHETRAN_GB_2021/08_Analysis/Climatic-Drought/"


# Create a map of active cells --------------------------------------------

# Give a path to one of the .asc ouputs:
asc_path <- paste0(root, "Outputs/SPEI_12/Average Length of Drought - Gridded - 01/drought_mean_length_raster_01_2049-2079.asc")

# Read in the header of this file:
asc_header <- read.csv(asc_path, nrows = 6, header = FALSE,  strip.white = TRUE, sep = "")

# Read in some data that can be used to check were active cells are:
asc_body <- read.csv(asc_path, skip = 6, sep = "", header = FALSE)

# Create an empty matrix of the same shape to hold the cell look up grid:
map_body <- matrix(nrow=nrow(asc_body), ncol=ncol(asc_body), data = -9999)
active_cells <- length(which(asc_body != -9999))
lookup_table <- data.frame("cell_ID" = 1:active_cells,
                           "row_no" = rep(NA, active_cells),
                           "column_no" = rep(NA, active_cells))

# Run through each row and column of the output and check for data, numbering active cells:
counter = 0
for(r in 1:nrow(map_body)){
  for(c in 1:ncol(map_body)){
    
    # If there is an active cell...
    if(asc_body[r,c]!=-9999){ 
      
      # Increase the counter:
      counter = counter + 1
      
      # Fill the lookup map:
      map_body[r,c] = counter
      
      # Fill the lookup table:
      lookup_table[counter, 2:3] = c(r,c)
    }
  }
}


# Write the map:
write.table(file = paste0(root, "SPEI Climate Metrics/Cell_Map.asc"), x = asc_header[,1:2], 
            row.names = FALSE, col.names = FALSE, quote = FALSE, na = "")
write.table(file = paste0(root, "SPEI Climate Metrics/Cell_Map.asc"), x = map_body, 
            row.names = FALSE, col.names = FALSE, quote = FALSE, na = "", 
            append = TRUE)



# Create DataFrames to hold Metrics ---------------------------------------


# Create datasets variants to loop through:
metrics = c("Average Length of Drought - Gridded",
           "Maximum Length of Drought - Gridded",
           "Probability of Drought - Gridded")

# File names of the output metrics:
metric_file_names = c("drought_mean_length_raster_",
                      "drought_max_length_raster_",
                      "drought_probability_raster_")

# Names for the metrics in the columns (must match lower down):
metric_column_names = c("mean_drought_length", "max_drought_length", "probability_of_drought")
                      
# List the Names you use for RCPs (must match folder/file names):
rcps = c('01', '04', '05', '06', '07', '08', 
         '09', '10', '11', '12', '13', '15')

# List the names of the periods you have outputs for:
periods = c("1980-1990", "1990-2000", "2000-2010", "2010-2020", "2020-2030", 
            "2030-2040", "2040-2050", "2050-2060", "2060-2070", "2070-2080",
            
            "1980-2010", "1990-2020", "2000-2030", "2010-2040", 
            "2020-2050", "2030-2060", "2040-2070", "2050-2080", 
            
            "WP1.5", "WP2.0", "WP2.5", "WP3.0", "WP3.5", "WP4.0")

# List the file names used to represent these periods in the outputs:
period_file_names = data.frame(
  "column_name" = periods,
  "file_name" = c("19801201-19901130", "19901201-20001130", "20001201-20101130", "20101201-20201130", "20201201-20301130", 
                  "20301201-20401130", "20401201-20501130", "20501201-20601130", "20601201-20701130", "20701201-20801130",
                  
                  "19801201-20101130", "19901201-20201130", "20001201-20301130", "20101201-20401130", 
                  "20201201-20501130", "20301201-20601130", "20401201-20701130", "20501201-20801130",
                  
                  "1", "2", "3", "4", "5", "6"))



# Create a Template Excel Output ------------------------------------------

library(openxlsx)

# Setup template workbook:
wb = createWorkbook()
addWorksheet(wb, "SPEI_3")

# Add row names:
writeData(wb = wb, sheet = "SPEI_3", x = lookup_table[,1], startRow = 3,
          rowNames = FALSE, colNames = TRUE)

# Add RCPs as merged top row:
p1 = 2
for(rcp in rcps){
  
  # Set the right hand limit of the columns to merge
  p2 = p1 + length(periods) -1
  
  # Merge columns for each RCP:
  mergeCells(wb, "SPEI_3", cols = p1:p2, rows = 1)
  
  # Write in the name of the RCP into the merged cell:
  writeData(wb = wb, sheet = "SPEI_3", x = rcp, startRow = 1, startCol = p1)
  
  # Move the start column to the next set for the next RCP:
  p1 = p2 + 1
}

# Add period column names:
for(r in 1:length(rcps)){
  write_column = (r-1)*length(periods) + 1
  
  for(p in 1:length(periods)){
    writeData(wb = wb, sheet = "SPEI_3", x = periods[p], startRow = 2, startCol = write_column+p)
  }
}


# Set header styles:
addStyle(wb, "SPEI_3", createStyle(textDecoration = c("BOLD"), halign = "center", numFmt = "TEXT"),
         rows = 1:2, cols = 1:(length(periods)*12 + 1), gridExpand = TRUE)

# Copy this worksheet layout to the other sheets to create a template for all workbooks:
cloneWorksheet(wb, "SPEI_6", "SPEI_3")
cloneWorksheet(wb, "SPEI_12", "SPEI_3")


spei_names = c("SPEI_3", "SPEI_6", "SPEI_12")



# Write the data to the Excel files ---------------------------------------


# Run through the three metrics:
for(m in 1:length(metrics)){
  
  m_wb = wb
  
  # Get the folder name of the metric:
  metric = metrics[m]
  
  # Get the file name of the metric:
  metric_fn = metric_file_names[m]
  
  # Get the column name of the metric:
  metric_cn = metric_column_names[m]
  

  print(metric)
  
  # Run through the three SPEIs:
  for(spei_name in spei_names){
    
    # Get the name of the SPEI we are using
    # spei_name = spei_names[s]
    
    # Create a path to the metric:
    metric_path = paste0(root, "Outputs/", spei_name, "/", metric)
    
    # Create output path:
    output_file = paste0(root, "SPEI Climate Metrics/", metric_cn, ".xlsx")
    
    print(spei_name)
    
    # Run through the RCPs
    for(r in 1:length(rcps)){
      
      rcp = rcps[r]
      
      print(rcp)
      
      # Complete the path the the folder containing the data:
      rcp_path = paste0(metric_path, " - ", rcp, "/")
      
      # Check which files are in the folder:
      file_paths = list.files(rcp_path, pattern = ".asc$")
      
      # Fiddle to get just the names of the warming period files (as these names differ between RCPs):
      file_paths_lookup = substr(x = file_paths, start = nchar(metric_fn)+4, stop = nchar(file_paths)-4)
      file_paths_lookup = file_paths_lookup[nchar(file_paths_lookup)==9]
      file_paths_lookup = as.numeric(substr(x = file_paths_lookup, start = 0, stop = 4))
      file_paths_lookup = sort(file_paths_lookup)
      
      write_column = (r-1)*length(periods) + 1
      
      # Run through the periods:
      for(p in 1:length(periods)){
        
        period = periods[p]
        # print(period)
        
        # Find period name to add to the file path:
        p_file = period_file_names$file_name[period_file_names$column_name==period]
        
        # If it is a warming period, find a file name for that warming period:
        if(grepl("WP", period)){
          p_file = as.numeric(p_file)
          p_file = file_paths_lookup[p_file]
          p_file = paste0(as.character(p_file), "-", as.character(p_file+30))
        }
        
        # Load in the metric .asc:
        file_path = paste0(rcp_path, metric_fn, rcp, "_", p_file, ".asc")
        array = as.matrix(read.csv(file_path, skip = 6, sep = "", header = FALSE))
        
        # Convert the matrix into a list of active cells (use t() to make row wise):
        a = t(array)[t(map_body)!=-9999]
        
        writeData(wb = m_wb, sheet = spei_name, x = round(a, 2),
                  startRow = 3, startCol = write_column+p, colNames = FALSE)
        }
      }
    }
  saveWorkbook(m_wb, file = output_file, overwrite = TRUE)
}



# Good Job! ---------------------------------------------------------------

