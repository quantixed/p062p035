# Script written by James Shelford to combine dataframes from individual experiments and plot the data 

# Working directory should be 'TACC3chTOG_spindle_recruitment/Quantification'
# log file should be in the directory with the csv files
# Lookup.csv should be in the 'Data' subdirectory
# Setup preferred directory structure in wd
ifelse(!dir.exists("Data"), dir.create("Data"), "Folder exists already")
ifelse(!dir.exists("Output"), dir.create("Output"), "Folder exists already")
ifelse(!dir.exists("Output/Dataframe"), dir.create("Output/Dataframe"), "Folder exists already")
ifelse(!dir.exists("Output/Plots"), dir.create("Output/Plots"), "Folder exists already")
ifelse(!dir.exists("Scripts"), dir.create("Scripts"), "Folder exists already")

# Load the required packages
require(ggplot2)
require(cowplot)
library(ggbeeswarm)
library(scales)
library(dplyr)
require(RColorBrewer)
require(egg) # to allow use of ggarrange function
require(rstatix)
require(SuperPlotR)

# Load the dataframes for all experiments
# these are rds files found in Output/Dataframe
datadir <- "Output/Dataframe"
my_files <- list.files(datadir,pattern='*.rds',full.names = TRUE)
combined_df <- bind_rows(lapply(my_files, readRDS))

# Load lookup
look_up_table <- read.table("Data/lookup.csv", header = TRUE, stringsAsFactors = F, sep = ",")

# Plotting the Categories in a specific order
combined_df$Category <- as.factor(combined_df$Category)
combined_df$Category <- factor(combined_df$Category, levels = look_up_table$Search_category)

# how many cells in each condition?
summary(combined_df$Category)

# Plot the combined data using superplot
bg <- ggplot() + 
  geom_hline(yintercept = 1, linetype='dashed', colour='black')
TACC3_plot <- superplot(combined_df, "TACC3_spindle_ratio", "Category", "Experiment_number",
                        ylab = 'TACC3 spindle recruitment', gg = bg) +
  ylim(0,6)

TACC3_plot

chTOG_plot <- superplot(combined_df, "chTOG_spindle_ratio", "Category", "Experiment_number",
                        ylab = 'ch-TOG spindle recruitment', gg = bg) +
  ylim(0,4)

chTOG_plot

# Use cowplot to combine the two plots side by side
# ggarrange from the egg package to remove x axis text for the TACC3 plot
combined_plot <- ggarrange(TACC3_plot + theme(axis.text.x = element_blank()), 
                           chTOG_plot, ncol = 1)
combined_plot

## Statistics ##

# TACC3
# ANOVA to compare the mean spindle recruitment between the different Categories
# Tukey post hoc

# TACC3
TACC3_summary <- combined_df %>% 
  group_by(Category, Experiment_number) %>%
  summarise(Mean = mean(TACC3_spindle_ratio, na.rm = TRUE)) %>%
  ungroup()
Anova_TACC3 <- anova_test(data = TACC3_summary, formula = Mean ~ Category)
get_anova_table(Anova_TACC3, correction = 'auto')
TACC3_tukey <- aov(Mean ~ Category, data = TACC3_summary) %>% tukey_hsd()
TACC3_tukey

# chTOG
chTOG_summary <- combined_df %>% 
  group_by(Category, Experiment_number) %>%
  summarise(Mean = mean(chTOG_spindle_ratio, na.rm = TRUE)) %>%
  ungroup()
Anova_chTOG <- anova_test(data = chTOG_summary, formula = Mean ~ Category)
get_anova_table(Anova_chTOG, correction = 'auto')
chTOG_tukey <- aov(Mean ~ Category, data = chTOG_summary) %>% tukey_hsd()
chTOG_tukey

# save the plots
# when importing the plot into illustrator save as pdf so it can be edited
# use the useDingbats = FALSE command to ensure the plot looks the same in illustrator

ggsave("Output/Plots/TACC3_Plot.pdf", plot = TACC3_plot, width = 75, height = 75, units = 'mm', useDingbats = FALSE)
ggsave("Output/Plots/chTOG_boxPlot.pdf", plot = chTOG_plot, width = 75, height = 75, units = 'mm', useDingbats = FALSE)
ggsave("Output/Plots/combined_plot.pdf", plot = combined_plot, width = 62.599, height = 101, units = 'mm', useDingbats = FALSE)
