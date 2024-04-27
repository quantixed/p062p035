# Script written by James Shelford to process csv files containing centrin and pericentrin data
# csv files are output from ImageJ

# Script will generate a df that will be used to calculate the % cells with >2 PCNT (df1) and a df that will be used for plotting (tidy_df)
# These df's will be combined with other experiments in a separate script named 'Centrin_pericentrin_combine'

# wd should be Centrin / Pericentrin
# setup preferred directory structure in wd
ifelse(!dir.exists("Data"), dir.create("Data"), "Folder exists already")
ifelse(!dir.exists("Output"), dir.create("Output"), "Folder exists already")
ifelse(!dir.exists("Output/Dataframes"), dir.create("Output/Dataframes"), "Folder exists already")
ifelse(!dir.exists("Output/Dataframes/RawData"), dir.create("Output/Dataframes/RawData"), "Folder exists already")
ifelse(!dir.exists("Output/Plots"), dir.create("Output/Plots"), "Folder exists already")
ifelse(!dir.exists("Scripts"), dir.create("Scripts"), "Folder exists already")

# Load required packages
library(tidyverse)

# select directory that contains the csv files (name of this folder is reused later)
datadir <- rstudioapi::selectDirectory()
my_files <- list.files(datadir, pattern = "*.csv", full.names = TRUE)
my_files_names <- list.files(datadir, pattern = "*.csv")

# Extract the experiment number (will be used later). Useful for combining data from multiple experiments
Experiment_number <- basename(datadir)

# create a dataframe without column names. This is because the num columns in the csv files varies within the dataset 
df1 <- data.frame(col.names = NULL, row.names = NULL)

# function definition. Add file data to the df
build_df1 <- function(df, the_filename, row_number){
  
  # import data
  the_data <- read.csv(file=the_filename, header=TRUE, stringsAsFactors=FALSE)
  
  # Add the filename
  modified_filename <- paste0(my_files_names[row_number], '_', Experiment_number)
  df[row_number, 1] <- modified_filename
  
  # Extracting the numbers from the csv file
  # Each column with the header 'Type' is a pericentrin blob. The number in each column corresponds to the number of centrin foci in that blob
  
  # There are 5 columns containing useless info that we dont want
  # Create a for loop to extract numbers only from the 'Type' columns
  
  col_num <- ncol(the_data) - 5
  
  # Add pericentrin number to the dataframe
  df[row_number, 2] <- col_num
  
  for(type_num in 1:col_num){
    
    df[row_number, type_num + 2] <- the_data[1, type_num + 1]

  }
  return(df)
}

# call the function for each file in the list
for(i in 1:length(my_files)){
  my_filename <- my_files[i]
  df1 <- build_df1(df1, my_filename, i)
}

# What is the maximum number of PCNT foci in this dataset?
print(max(df1$V2))

# Add names to the columns of df1. Change according to the max num of PCNT foci in the dataset.
col_headings <- c('file_name', 'total_pericentrin_foci', '1', '2', '3', '4', '5', '6', '7')
colnames(df1) <- col_headings

## The data need to be unblinded and categorised
# Make list of the names of the blinded filenames with *.csv removed & put it all together in data frame
blind_list <- gsub(".csv","", my_files_names)
df1$blind_list <- blind_list

# load the log.txt file
blind_log <- read.delim(paste(datadir,"log.txt", sep = "/"), header = TRUE) 

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

# We need a look up table
look_up_table <- read.table("Data/lookup.csv", header = TRUE, stringsAsFactors = F, sep = ",")

# add a new column to dataframe where categories are defined by searching original name for partial strings
blind_log$Category <- add_categories(blind_log$Original_Name,
                                     look_up_table$Search_name,
                                     look_up_table$Search_category,
                                     "NA", ignore.case = TRUE)

# Now we have a dataframe that can be used to lookup the real values

# This line looks up the correct Category from the blind_log
df1$Category <- with(blind_log,
                        Category[match(df1$blind_list,
                                       Blinded_Name)])

# Add the experiment number to the df
df1$Experiment_number <- Experiment_number

# save the dateframe so it can be combined with other experiments in a new script. 
# This df will be used to calculate % cells with >2 PCNT foci
saveRDS(df1, file = paste0("Output/Dataframes/RawData/RawData", Experiment_number, ".rds"))

# Subset dataframe for each construct
# Use gather() to make new column 'cell' and 'PCNT blob', PCNT blob column will need to be numeric 
# Plot cell on x axis, each cell will be its own category, y axis is PCNT blob and colour the dots by centrin foci.

# Subset category
mCh_data <- subset(df1, Category == 'mCherry')
E4_data <- subset(df1, Category == 'E4')
E7_data <- subset(df1, Category == 'E7')
E8_data <- subset(df1, Category == 'E8')

# reshape the dfs. Change the column numbers to 'gather' based on PCNT number
mCh_data <- gather(mCh_data, 'PCNT', 'Centrin_foci', 3:9)
E4_data <- gather(E4_data, 'PCNT', 'Centrin_foci', 3:9)
E7_data <- gather(E7_data, 'PCNT', 'Centrin_foci', 3:9)
E8_data <- gather(E8_data, 'PCNT', 'Centrin_foci', 3:9)

tidy_df <- rbind(mCh_data, E4_data, E7_data, E8_data)

# Remove NA
tidy_df <- na.omit(tidy_df)

# Covert PCNT to numeric
tidy_df$PCNT <- as.numeric(tidy_df$PCNT)

# save the dateframe so it can be combined with other experiments in a new script
# tidy_df will be used for plotting
saveRDS(tidy_df, file = paste0("Output/Dataframes/", Experiment_number, ".rds"))

