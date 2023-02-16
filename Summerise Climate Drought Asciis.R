# Summarise Climate Drought Asciis ----------------------------------------
# Script 3

# Ben Smith
# Newcastle University
# 14/11/2022

# Notes -------------------------------------------------------------------

# This script will summarise the drought asciis produced from the UKCP18 data.
# Min, Mean, Max functions will be applied across the different RCPs for each
# warming period and written to a single ascii file.


# Preamble ----------------------------------------------------------------

# Set some variables for folder/file paths:
raw_ouput_folder = "I:/SHETRAN_GB_2021/08_Analysis/Climatic-Drought/Outputs/SPEI_3/"
PP_folder = "I:/SHETRAN_GB_2021/08_Analysis/Climatic-Drought/Post Processing and GIS/SPEI_3"

rcps = c('01', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '15')

number_of_warming_periods = 6

# SPEI_periods = c("3", "12")

# Set some more specifying the names of the outputs:
av_folder <- "Average Length of Drought - Gridded - "
av_file_name <- "drought_mean_length_raster_"

mx_folder <- "Maximum Length of Drought - Gridded - "
mx_file_name <- "drought_max_length_raster_"

pr_folder <- "Probability of Drought - Gridded - "
pr_file_name <- "drought_probability_raster_"

# Get an ascii header:
asc_header = matrix(ncol = 57,  nrow = 6)
asc_header[,1:2] = 
  as.matrix(read.csv(
    paste0(raw_ouput_folder, pr_folder, "01/drought_probability_raster_01_2006-2036.asc"), 
    nrows = 6, header = FALSE,  strip.white = TRUE, sep = "", 
    colClasses = c("character", "character")))

# Set character length of files for filtering:
file_name_length = 43

# List the folders within outputs:
folders = list.dirs(raw_ouput_folder, full.names = TRUE)

# List all asciis within those folders:
files=c()
for(f in folders){
  files = c(files, list.files(f, pattern = ".asc$"))
}

# Crop the files (by path length) to take only the ones of interest:
files = files[nchar(files) == file_name_length]



# Run through the calculated statistics -----------------------------------

stat_file_names = list(c("Drought_Probability", pr_folder, pr_file_name),
                       c("Drought_Mean_Length", av_folder, av_file_name),
                       c("Drought_Max_Length", mx_folder, mx_file_name))

for(stat in stat_file_names){
  
  # Collect data for the warming period -------------------------------------
  
  for(i in 1:number_of_warming_periods){
    
    wp_list = list()
    
    # Subset files to only those of the statistic of interest:
    stat_files = files[grepl(x = files, pattern = stat[3])]
    
    # Run through each RCP for the warming period of interest:
    for(rcp in rcps){
      
      rcp_files = sort(stat_files[grepl(pattern = paste0("_", rcp, "_"), x = stat_files)])
      
      # Set ascii file path to read:
      asc = paste0(raw_ouput_folder, stat[2], rcp, "/", rcp_files[i])
      
      # Store that ascii in a list:
      wp_list[[rcp]] = as.matrix(read.csv(asc, skip=6, na.strings = -9999, header = FALSE, sep = " "))
      
    }
    
    # Convert the list of asciis for that warming period into an array:
    rcp_array <- array(data = unlist(wp_list), 
                       dim = c(dim(wp_list[[1]])[1], dim(wp_list[[1]])[2], length(wp_list)))
    
    # Process the data in the warming period list -----------------------------
    
    # Min:
    asc_min = apply(X = rcp_array, MARGIN = 1:2, FUN = min)
    asc_min[is.na(asc_min)] = -9999
    
    # Mean:
    asc_mean = rowMeans(rcp_array, dims = 2)
    asc_mean[is.na(asc_mean)] = -9999
    
    # Max:
    asc_max = apply(rcp_array, 1:2, max)
    asc_max[is.na(asc_max)] = -9999
    
    
    # Write the summaries -----------------------------------------------------
    
    # Min:
    write.table(file = paste0(PP_folder, " ", stat[1], " Warming_Period_", i, " min.asc"),
                x = rbind(asc_header, round(asc_min, 4)), 
                row.names = FALSE, col.names = FALSE, quote = FALSE, na = "")
    
    # Mean:
    write.table(file = paste0(PP_folder, " ", stat[1], " Warming_Period_", i, " mean.asc"),
                x = rbind(asc_header, round(asc_mean, 4)), 
                row.names = FALSE, col.names = FALSE, quote = FALSE, na = "")
    
    # Max:
    write.table(file = paste0(PP_folder, " ", stat[1], " Warming_Period_", i, " max.asc"),
                x = rbind(asc_header, round(asc_max, 4)), 
                row.names = FALSE, col.names = FALSE, quote = FALSE, na = "")
    
  }
}


# The END -----------------------------------------------------------------

