# Script written by James Shelford
# Script to process output from Western blot analysis in Fiji
# The output is a dataframe saved as .rds for combining with other experiments to further process

# Working directory should be 'TACC3_IP'
# Lookup.csv should be in the 'Data' subdirectory
# Setup preferred directory structure in wd
ifelse(!dir.exists("Data"), dir.create("Data"), "Folder exists already")
ifelse(!dir.exists("Output"), dir.create("Output"), "Folder exists already")
ifelse(!dir.exists("Output/Dataframe"), dir.create("Output/Dataframe"), "Folder exists already")
ifelse(!dir.exists("Output/Plots"), dir.create("Output/Plots"), "Folder exists already")
ifelse(!dir.exists("Script"), dir.create("Script"), "Folder exists already")

# Select directory containing the .csv files
datadir <- rstudioapi::selectDirectory()

# Extract the experiment number for use later (useful when combining experiments)
Experiment_number<- basename(datadir)

# search all .csv files in current working directory
mCh_data <- list.files(datadir,pattern='mCherry',full.names = TRUE)
chTOG_data <- list.files(datadir,pattern='chTOG',full.names = TRUE)
TACC3_data <- list.files(datadir,pattern='TACC3',full.names = TRUE)

# import data
mCh_values <- read.csv(file=mCh_data, header=TRUE, stringsAsFactors=FALSE)
chTOG_values <- read.csv(file=chTOG_data, header=TRUE, stringsAsFactors=FALSE)
TACC3_values <- read.csv(file=TACC3_data, header=TRUE, stringsAsFactors=FALSE)

# Create matrix to store the data and convert to dataframe
headings <- c('Category', 'mCh_value', 'chTOG_value', 'TACC3_value')
my_matrix <- matrix(0, 4, length(headings))
colnames(my_matrix) <- headings
df1 <- as.data.frame(my_matrix)

# Function to fill the dataframe 
# Invert mean pixel values and subtract bg (255 - mean pixel)
# Calculate the ratio of protein over loading control. Loading control in this case is TACC3.        

  make_dataframe <- function(dataframe, row_num) {
    
    if(row_num == 1){
      dataframe$Category[1] <- 'mCh'
      dataframe$mCh_value[1] <- (255 - mCh_values[1,'Mean']) - (255 - mCh_values[2, 'Mean'])
      dataframe$chTOG_value[1] <- (255 - chTOG_values[1,'Mean']) - (255 - chTOG_values[2, 'Mean'])
      dataframe$TACC3_value[1] <- (255 - TACC3_values[1,'Mean']) - (255 - TACC3_values[2, 'Mean'])
      dataframe$normalised_mCh[1] <- dataframe$mCh_value[1] / dataframe$mCh_value[1]
      dataframe$normalised_chTOG[1] <- dataframe$chTOG_value[1] / dataframe$chTOG_value[1]
      dataframe$mCh_ratio[1] <- dataframe$mCh_value[1] / dataframe$TACC3_value[1]
      dataframe$chTOG_ratio[1] <- dataframe$chTOG_value[1] / dataframe$TACC3_value[1]
      
    }
    
    if(row_num == 2){
      dataframe$Category[2] <- 'E4'
      dataframe$mCh_value[2] <- (255 - mCh_values[3,'Mean']) - (255 - mCh_values[4, 'Mean'])
        dataframe$chTOG_value[2] <- (255 - chTOG_values[3,'Mean']) - (255 - chTOG_values[4, 'Mean'])
        dataframe$TACC3_value[2] <- (255 - TACC3_values[3,'Mean']) - (255 - TACC3_values[4, 'Mean'])
        dataframe$normalised_mCh[2] <- dataframe$mCh_value[2] / dataframe$mCh_value[1]
        dataframe$normalised_chTOG[2] <- dataframe$chTOG_value[2] / dataframe$chTOG_value[1]
        dataframe$mCh_ratio[2] <- dataframe$mCh_value[2] / dataframe$TACC3_value[2]
        dataframe$chTOG_ratio[2] <- dataframe$chTOG_value[2] / dataframe$TACC3_value[2]
    }
    
    if(row_num == 3){
      dataframe$Category[3] <- 'E7'
      dataframe$mCh_value[3] <- (255 - mCh_values[5,'Mean']) - (255 - mCh_values[6, 'Mean'])
      dataframe$chTOG_value[3] <- (255 - chTOG_values[5,'Mean']) - (255 - chTOG_values[6, 'Mean'])
      dataframe$TACC3_value[3] <- (255 - TACC3_values[5,'Mean']) - (255 - TACC3_values[6, 'Mean'])
      dataframe$normalised_mCh[3] <- dataframe$mCh_value[3] / dataframe$mCh_value[1]
      dataframe$normalised_chTOG[3] <- dataframe$chTOG_value[3] / dataframe$chTOG_value[1]
      dataframe$mCh_ratio[3] <- dataframe$mCh_value[3] / dataframe$TACC3_value[3]
      dataframe$chTOG_ratio[3] <- dataframe$chTOG_value[3] / dataframe$TACC3_value[3]
    }
    
    if(row_num == 4){
      dataframe$Category[4] <- 'E8'
      dataframe$mCh_value[4] <- (255 - mCh_values[7,'Mean']) - (255 - mCh_values[8, 'Mean'])
      dataframe$chTOG_value[4] <- (255 - chTOG_values[7,'Mean']) - (255 - chTOG_values[8, 'Mean'])
      dataframe$TACC3_value[4] <- (255 - TACC3_values[7,'Mean']) - (255 - TACC3_values[8, 'Mean'])
      dataframe$normalised_mCh[4] <- dataframe$mCh_value[4] / dataframe$mCh_value[1]
      dataframe$normalised_chTOG[4] <- dataframe$chTOG_value[4] / dataframe$chTOG_value[1]
      dataframe$mCh_ratio[4] <- dataframe$mCh_value[4] / dataframe$TACC3_value[4]
      dataframe$chTOG_ratio[4] <- dataframe$chTOG_value[4] / dataframe$TACC3_value[4]
    }
    
    return(dataframe)
  }
  
  for(i in 1:4){
  df1 <- make_dataframe(df1, i)
  }
  
  # Add experiment number
  df1$Experiment <- Experiment_number
  
  # save the dateframe so it can be combined with other experiments in a new script
  file_name<- paste0("Output/Dataframe/", Experiment_number, "_dataframe.rds")
  saveRDS(df1, file = file_name)
  