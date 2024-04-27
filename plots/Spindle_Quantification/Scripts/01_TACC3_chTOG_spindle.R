# Script written by James Shelford to upload csv files to quantify spindle recruitment

# Working directory should be 'TACC3chTOG_spindle_recruitment/Quantification'
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
my_files <- list.files(datadir,pattern='*.csv',full.names = TRUE)
my_files_names <- list.files(datadir, pattern='*.csv')


my_matrix <- matrix(0,length(my_files),21)

# function definition
build_matrix <- function(my_matrix,my_filename,row_number){
  
  # import data
  my_raw_data <- read.csv(file=my_filename, header=TRUE, stringsAsFactors=FALSE)
  
  # take mean column and transpose
  my_data <- subset(my_raw_data, select=Mean)
  my_data <- t(my_data)
  my_matrix[row_number, 1:21] <- my_data[1,1:21]
  return(my_matrix)
}

# call the function for each file in the list
for(i in 1:length(my_files)){
  my_filename <- my_files[i]
  my_matrix <- build_matrix(my_matrix,my_filename,i)
}

# Generating the mean values and ratio

TACC3_spindle_matrix <- matrix(0,length(my_files),3)
TACC3_spindle_matrix[1:length(my_files),1:3] <- my_matrix[1:length(my_files),c(1,4,7)]

chTOG_spindle_matrix <- matrix(0,length(my_files),3)
chTOG_spindle_matrix[1:length(my_files),1:3] <- my_matrix[1:length(my_files),c(2,5,8)]

Affimer_spindle_matrix <- matrix(0,length(my_files),3)
Affimer_spindle_matrix[1:length(my_files),1:3] <- my_matrix[1:length(my_files),c(3,6,9)]

TACC3_cytoplasm_matrix <- matrix(0,length(my_files),3)
TACC3_cytoplasm_matrix[1:length(my_files),1:3] <- my_matrix[1:length(my_files),c(10,13,16)]

chTOG_cytoplasm_matrix <- matrix(0,length(my_files),3)
chTOG_cytoplasm_matrix[1:length(my_files),1:3] <- my_matrix[1:length(my_files),c(11,14,17)]

Affimer_cytoplasm_matrix <- matrix(0,length(my_files),3)
Affimer_cytoplasm_matrix[1:length(my_files),1:3] <- my_matrix[1:length(my_files),c(12,15,18)]

TACC3_spindle_means <- rowMeans(TACC3_spindle_matrix, na.rm=TRUE)
TACC3_cytoplasm_means <- rowMeans(TACC3_cytoplasm_matrix, na.rm=TRUE)
chTOG_spindle_means <- rowMeans(chTOG_spindle_matrix, na.rm=TRUE)
chTOG_cytoplasm_means <- rowMeans(chTOG_cytoplasm_matrix, na.rm=TRUE)
Affimer_spindle_means <- rowMeans(Affimer_spindle_matrix, na.rm=TRUE)
Affimer_cytoplasm_means <- rowMeans(Affimer_cytoplasm_matrix, na.rm=TRUE)

TACC3_background <- c(my_matrix[1:length(my_files),19])
chTOG_background <- c(my_matrix[1:length(my_files),20])
Affimer_background <- c(my_matrix[1:length(my_files),21])

TACC3_spindle_values <- TACC3_spindle_means - TACC3_background
TACC3_cytoplasm_values <- TACC3_cytoplasm_means - TACC3_background
chTOG_spindle_values <- chTOG_spindle_means - chTOG_background
chTOG_cytoplasm_values <- chTOG_cytoplasm_means - chTOG_background
Affimer_spindle_values <- Affimer_spindle_means - Affimer_background
Affimer_cytoplasm_values <- Affimer_cytoplasm_means - Affimer_background

TACC3_spindle_ratio <- TACC3_spindle_values/TACC3_cytoplasm_values
chTOG_spindle_ratio <- chTOG_spindle_values/chTOG_cytoplasm_values
Affimer_spindle_ratio <- Affimer_spindle_values/Affimer_cytoplasm_values


# Adding the values to the matrix
my_matrix <- cbind(my_matrix,TACC3_spindle_values, chTOG_spindle_values, Affimer_spindle_values, TACC3_cytoplasm_values, 
                   chTOG_cytoplasm_values, Affimer_cytoplasm_values, TACC3_spindle_ratio, chTOG_spindle_ratio, Affimer_spindle_ratio)

# Make blind list with *.csv removed
blind_list <- gsub(".csv","", my_files_names)
df1 <- as.data.frame(my_matrix)
df1$blind_list <- blind_list

# load the log.txt file
logfile_path <- paste0(datadir,"/log.txt")
blind_log <- read.table(logfile_path, header = TRUE)

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

#Load the look-up table
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
# needs to be done like this because
# a) blind_log is in a random order
# b) your list of *.csv names could be in any order (although they're probably sorted alphanumerically)

# Add the experiment number to the df
df1$Experiment_number <- Experiment_number

# save the dateframe so it can be combined with other experiments in a new script
file_name<- paste0("Output/Dataframe/", Experiment_number, "_dataframe.rds")
saveRDS(df1, file = file_name)
