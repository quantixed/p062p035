# Written by James Shelford
# Script to upload csv files containing results from 3DOC
# Extract the volume and calculate average and sum per cell. Extract the number of objects (centrosomes) per cell.
# Run this on a folder of one experiments worth of data to get an rds file in Output/Dataframe

# Working directory should be 'Pericentrin_quant'
# Lookup.csv should be in the 'Data' subdirectory
# Setup preferred directory structure in wd
ifelse(!dir.exists("Data"), dir.create("Data"), "Folder exists already")
ifelse(!dir.exists("Output"), dir.create("Output"), "Folder exists already")
ifelse(!dir.exists("Output/Dataframe"), dir.create("Output/Dataframe"), "Folder exists already")
ifelse(!dir.exists("Output/Plots"), dir.create("Output/Plots"), "Folder exists already")
ifelse(!dir.exists("Scripts"), dir.create("Scripts"), "Folder exists already")

# Select directory containing the .csv files
datadir <- rstudioapi::selectDirectory()

# Extract the experiment number for use later (useful when combining experiments)
Experiment_number<- basename(datadir)

# search all .csv files in current working directory
my_files <- list.files(datadir,pattern='*PCNT.csv',full.names = TRUE) #Output from 3DOC
my_files_names <- list.files(datadir,pattern='*PCNT.csv')
GFP_files <- list.files(datadir,pattern='*fluorescence_intensity.csv',full.names = TRUE) # GFP intensity
GFP_files_names <- list.files(datadir,pattern='*fluorescence_intensity.csv')

# Create matrix to store the data and convert to dataframe
headings <- c('pericentrin_foci', 'ave_volume', 'sum_volume', 'volume_diff', 'mean_surface_area', 'sum_surface_area', 
              'mean_intensity', 'sum_intensity', 'GFP_intensity_mean', 'GFP_intensity_IntDen', 'file_name')
my_matrix <- matrix(0, length(my_files), length(headings))
colnames(my_matrix) <- headings
df1 <- as.data.frame(my_matrix)

# function definition
build_df1 <- function(df, my_filename, my_GFP_filename, row_number) {
  
  # import data
  my_raw_data <- read.csv(file=my_filename, header=TRUE, stringsAsFactors=FALSE)
  # GFP data
  my_GFP_data <- read.csv(file=my_GFP_filename, header=TRUE, stringsAsFactors=FALSE)
  
  # Get the values and add to the correct row&col in dataframe
  
  df1$pericentrin_foci[i] <- length(my_raw_data[,'Volume..micron.3.'])
  df1$ave_volume[i] <- mean(my_raw_data[,'Volume..micron.3.'])
  df1$sum_volume[i] <- sum(my_raw_data[,'Volume..micron.3.'])
  df1$volume_diff[i] <- abs(my_raw_data[1, 'Volume..micron.3.'] - my_raw_data[2, 'Volume..micron.3.'])
  df1$mean_intensity[i] <- mean(my_raw_data[,'Mean'])
  df1$sum_intensity[i] <- sum(my_raw_data[,'Mean'])
  df1$mean_surface_area[i] <- mean(my_raw_data[,'Surface..micron.2.'])
  df1$sum_surface_area[i] <- sum(my_raw_data[, 'Surface..micron.2.'])
  df1$GFP_intensity_mean[i] <- my_GFP_data[, 'Mean']
  df1$GFP_intensity_IntDen[i] <- my_GFP_data[, 'IntDen']
  df1$file_name[i] <- my_files_names[i]
  
  return(df1)
}

# call the function for each file in the list
for(i in 1:length(my_files)){
  my_filename <- my_files[i]
  my_GFP_filename <- GFP_files[i]
  df1 <- build_df1(df1, my_filename, my_GFP_filename, i)
}

# function to find partial strings in a column and classify them
add_categories = function(x, patterns, replacements = patterns, fill = NA, ...) {
  stopifnot(length(patterns) == length(replacements))
  ans = rep_len(as.character(fill), length(x))    
  empty = seq_along(x)
  for(i in seq_along(patterns)) {
    greps = grepl(patterns[[i]], x[empty], ...)
    ans[empty[greps]] = replacements[[i]]  
    empty = empty[!greps]
  }
  return(ans)
}

# function to find partial strings in a column and classify them
add_categories = function(x, patterns, replacements = patterns, fill = NA, ...) {
  stopifnot(length(patterns) == length(replacements))
  ans = rep_len(as.character(fill), length(x))    
  empty = seq_along(x)
  for(i in seq_along(patterns)) {
    greps = grepl(patterns[[i]], x[empty], ...)
    ans[empty[greps]] = replacements[[i]]  
    empty = empty[!greps]
  }
  return(ans)
}

# load external look-up table
look_up_table <- read.table("Data/lookup.csv", header = TRUE, stringsAsFactors = FALSE, sep = ",")

# add a new column to dataframe where categories are defined by searching original name for partial strings
df1$Category <- add_categories(df1$file_name,
                                        look_up_table$Search_name,
                                        look_up_table$Search_category,
                                        "NA", ignore.case = TRUE)

df1$Category <- as.factor(df1$Category)

# Plot the categories in a specific order
df1$Category <- factor(df1$Category, levels = unique(look_up_table$Search_category) )

# needs to be done like this because
# a) blind_log is in a random order
# b) your list of *.csv names could be in any order (although they're probably sorted alphanumerically)

# Add the experiment number to the df
df1$Experiment_number <- Experiment_number

# save the dateframe so it can be combined with other experiments in a new script
file_name<- paste0("Output/Dataframe/", Experiment_number, "_dataframe.rds")
saveRDS(df1, file = file_name)
