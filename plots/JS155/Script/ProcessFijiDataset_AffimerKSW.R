# Script to process multiple csv files containing output from Fiji
# Written by James Shelford, generalised by Stephen Royle
## Modified for the Fixed Affimer KSW expt. The output from Fiji is already bg corrected.

require(ggplot2)
library(dplyr)
library(multcomp)
library(scales)
library(cowplot)
library(rstatix)
library(ggpubr)

# wd should be fixed_ksw_figure
# setup preferred directory structure in wd
ifelse(!dir.exists("Data"), dir.create("Data"), "Folder exists already")
ifelse(!dir.exists("Output"), dir.create("Output"), "Folder exists already")
ifelse(!dir.exists("Output/Data"), dir.create("Output/Data"), "Folder exists already")
ifelse(!dir.exists("Output/Plots"), dir.create("Output/Plots"), "Folder exists already")
ifelse(!dir.exists("Script"), dir.create("Script"), "Folder exists already")

# select directory that contains the csv files (name of this folder is reused later)
datadir <- rstudioapi::selectDirectory()
my_files <- list.files(datadir, pattern = "*.csv", full.names = TRUE)
my_files_names <- list.files(datadir, pattern = "*.csv")

# Extract the experiment number (will be used later). Useful for combining data from multiple experiments
Experiment_number <- basename(datadir)

# base R is used to pull in all the data
my_matrix <- matrix(0, length(my_files), 18)

# function definition
build_matrix <- function(my_matrix, my_filename, row_number){
  
  # import data
  my_raw_data <- read.csv(file = my_filename, header = TRUE, stringsAsFactors = FALSE)
  
  # take mean column and transpose
  my_data <- subset(my_raw_data, select = Mean)
  my_data <- t(my_data)
  my_matrix[row_number, 1:18] <- my_data[1, 1:18]
  return(my_matrix)
}

# call the function for each file in the list
for(i in 1:length(my_files)){
  my_filename <- my_files[i]
  my_matrix <- build_matrix(my_matrix, my_filename, i)
}

# Generating the mean values and ratio
reference_spindle_matrix <- matrix(0, length(my_files), 3)
reference_spindle_matrix[1:length(my_files), 1:3] <- my_matrix[1:length(my_files), c(1:3)]

GFP_spindle_matrix <- matrix(0, length(my_files), 3)
GFP_spindle_matrix[1:length(my_files), 1:3] <- my_matrix[1:length(my_files), c(4:6)]

POI_spindle_matrix <- matrix(0, length(my_files), 3)
POI_spindle_matrix[1:length(my_files), 1:3] <- my_matrix[1:length(my_files), c(7:9)]

reference_cytoplasm_matrix <- matrix(0, length(my_files), 3)
reference_cytoplasm_matrix[1:length(my_files), 1:3] <- my_matrix[1:length(my_files), c(10:12)]

GFP_cytoplasm_matrix <- matrix(0, length(my_files), 3)
GFP_cytoplasm_matrix[1:length(my_files), 1:3] <- my_matrix[1:length(my_files), c(13:15)]

POI_cytoplasm_matrix <- matrix(0, length(my_files), 3)
POI_cytoplasm_matrix[1:length(my_files), 1:3] <- my_matrix[1:length(my_files), c(16:18)]

reference_spindle_means <- rowMeans(reference_spindle_matrix, na.rm = TRUE)
reference_cytoplasm_means <- rowMeans(reference_cytoplasm_matrix, na.rm = TRUE)
POI_spindle_means <- rowMeans(POI_spindle_matrix, na.rm = TRUE)
POI_cytoplasm_means <- rowMeans(POI_cytoplasm_matrix, na.rm = TRUE)
GFP_spindle_means <- rowMeans(GFP_spindle_matrix, na.rm = TRUE)
GFP_cytoplasm_means <- rowMeans(GFP_cytoplasm_matrix, na.rm = TRUE)

# ratio calculation
reference_spindle_ratio <- reference_spindle_means / reference_cytoplasm_means
POI_spindle_ratio <- POI_spindle_means / POI_cytoplasm_means
GFP_spindle_ratio <- GFP_spindle_means / GFP_cytoplasm_means

# Adding these values to the matrix
my_matrix <- cbind(my_matrix, reference_spindle_means, POI_spindle_means, GFP_spindle_means, reference_cytoplasm_means, POI_cytoplasm_means, GFP_cytoplasm_means, reference_spindle_ratio, POI_spindle_ratio, GFP_spindle_ratio)

# Make list of the names of the blinded filenames with *.csv removed put it all together in data frame
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

# load external look-up table
look_up_table <- read.table("Data/lookup.csv", header = TRUE, stringsAsFactors = FALSE, sep = ",")

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

# How many cells in each group
summary(df1$Category)

df1$Category <- as.factor(df1$Category)

# # Add column to df to group by POI
df1$Affimer <- 'E4'
df1$Affimer[df1$Category == 'Rapamycin E7'] <- 'E7'
df1$Affimer[df1$Category == 'Control E7'] <- 'E7'
df1$Affimer[df1$Category == 'Rapamycin E8'] <- 'E8'
df1$Affimer[df1$Category == 'Control E8'] <- 'E8'

df1$Affimer <- as.factor(df1$Affimer)

# # spindle / (spindle + cyto)
# # function to calculate GFP/POI value
# new_value <- function(df, row_number){
#   
#   df$GFP_value[row_number] <- df$GFP_spindle_means[row_number] / (df$GFP_spindle_means[row_number] + df$GFP_cytoplasm_means[row_number])
#   df$POI_value[row_number] <- df$POI_spindle_means[row_number] / (df$POI_spindle_means[row_number] + df$POI_cytoplasm_means[row_number])
#   
#   return(df)
# }
# # Call function
# for (i in 1:nrow(df1)) {
#   df1 <- new_value(df1, i)
#   
# }

## Generating the plot
# we need to know the name of the cell line
theCellLine <- basename(datadir)

# find the min and max of the dataframe
loVal <- min(df1[,26:27], na.rm=T)
hiVal <- max(df1[,26:27], na.rm=T)

# we want symmetry about 1
findAxisLimit <- function(val1,val2){
  highVal <- max(1/val1,val2)
  highVal <- ceiling(log2(highVal))
  highVal <- 2^highVal
  return(highVal)
}
axVal <- findAxisLimit(loVal,hiVal)

# function to generate the plots
makeTheScatterPlot <- function(preData, postData, yLab, xLab) {
  ggplot(filter(df1, Category == preData | Category == postData),
         aes(x=GFP_spindle_ratio, y=POI_spindle_ratio, color=Category, alpha=0.5)) +
    geom_point() +
    scale_x_continuous(trans='log2', limits = c(1/axVal,axVal), breaks = trans_breaks("log2", function(x) 2^x), labels = trans_format("log2", math_format(.x))) +
    scale_y_continuous(trans='log2', limits = c(1/axVal,axVal), breaks = trans_breaks("log2", function(x) 2^x), labels = trans_format("log2", math_format(.x))) +
    labs(y = NULL, x = xLab) +
    theme(legend.position = 'none')
}

p1 <- makeTheScatterPlot("Control E4","Rapamycin E4", "TACC3", 'E4')
p2 <- makeTheScatterPlot("Control E7","Rapamycin E7", "TACC3", 'E7')
p3 <- makeTheScatterPlot("Control E8","Rapamycin E8", "TACC3", 'E8')

all_scatter_plots <- plot_grid(p1,p2,p3, nrow = 1) 
all_scatter_plots
ggsave(paste0("Output/Plots/all_scatter_plots_",theCellLine,".pdf"), plot = all_scatter_plots, width = 130, height = 40, units = 'mm', useDingbats = FALSE)

# Stats

# Check distribution 
# Density plot
ggdensity(df1$POI_spindle_ratio, fill = 'lightgray')
# QQ plot
ggqqplot(df1$POI_spindle_ratio)

# Normality significance test, this uses the rstatix package which is pipe friendly. Null hypothesis is norm dist.
df1 %>% group_by(Category) %>% shapiro_test(POI_spindle_ratio)

# Check variance using levene test, uses rstatix package
df1 %>% group_by(POI) %>% levene_test(POI_value ~ Category)

## Independent two-samples t-test, uses rstatix package
stat_test_results <- df1 %>% group_by(Affimer) %>% t_test(POI_spindle_ratio ~ Category, var.equal = FALSE)

