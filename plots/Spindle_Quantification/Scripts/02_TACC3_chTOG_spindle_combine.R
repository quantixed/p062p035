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

# Plot the combined data using ggplot
TACC3_plot <- ggplot(data = combined_df, aes(x = Category, y = TACC3_spindle_ratio, colour = '#00A651')) +
  theme_cowplot() +
  scale_colour_manual(values = '#00A651') +
  geom_hline(yintercept = 1, linetype='dashed', colour='black') +
  geom_quasirandom(alpha = 0.5, stroke = 0) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult=1), aes(group = Category)) + 
  theme(axis.text.x = element_text(face = "plain", color = 'black', size = 9, angle = 0, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(face = 'plain', color = 'black', size = 9)) +
  theme(axis.title.y = element_text(size = 10,face = 'plain', color = 'black', margin = margin(t = 0, r = 5, b = 0, l = 0)), 
        legend.title = element_text(size = 10, face = 'bold', colour = 'black'),
        legend.text = element_text(size = 10, face = 'plain', colour = 'black'),
        legend.justification = 'center',
        legend.background = element_rect(colour = 'black'),
        legend.margin = margin(0.2,0.2,0.2,0.2,'cm'),
        plot.margin = margin(0.1,0.1,0.1,0.1, 'cm')) +
  labs(y = 'TACC3 spindle recruitment', x = NULL) + 
  theme(legend.position = 'none', legend.title = element_blank()) +
  ylim(0,6)
TACC3_plot

chTOG_plot <- ggplot(data = combined_df, aes(x = Category, y = chTOG_spindle_ratio, colour = '#F8766D')) +
  theme_cowplot() +
  scale_colour_manual(values = '#F8766D') +
  geom_hline(yintercept =1, linetype='dashed', colour='black') +
  geom_quasirandom(alpha = 0.5, stroke = 0) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult=1), aes(group = Category)) + 
  theme(axis.text.x = element_text(face = "plain", color = 'black', size = 9, angle = 0, hjust = 0.5, vjust = 0.5), 
          axis.text.y = element_text(face = 'plain', color = 'black', size = 9)) +
    theme(axis.title.y = element_text(size = 10,face = 'plain', color = 'black', margin = margin(t = 0, r = 5, b = 0, l = 0)), 
          legend.title = element_text(size = 10, face = 'bold', colour = 'black'),
          legend.text = element_text(size = 10, face = 'plain', colour = 'black'),
          legend.justification = 'center',
          legend.background = element_rect(colour = 'black'),
          legend.margin = margin(0.2,0.2,0.2,0.2,'cm'),
          plot.margin = margin(0.1,0.1,0.1,0.1, 'cm')) +
    labs(y = 'ch-TOG spindle recruitment', x = NULL) + 
    theme(legend.position = 'none', legend.title = element_blank()) +
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
Anova_TACC3 <- combined_df %>% anova_test(TACC3_spindle_ratio ~ Category)
get_anova_table(Anova_TACC3, correction = 'auto')
TACC3_tukey <- aov(TACC3_spindle_ratio ~ Category, data = combined_df) %>% tukey_hsd()

# chTOG
Anova_chTOG <- combined_df %>% anova_test(chTOG_spindle_ratio ~ Category)
get_anova_table(Anova_chTOG, correction = 'auto')
chTOG_tukey <- aov(chTOG_spindle_ratio ~ Category, data = combined_df) %>% tukey_hsd()


# save the plots
# when importing the plot into illustrator save as pdf so it can be edited
# use the useDingbats = FALSE command to ensure the plot looks the same in illustrator

ggsave("Output/Plots/TACC3_Plot.pdf", plot = TACC3_plot, width = 75, height = 75, units = 'mm', useDingbats = FALSE)
ggsave("Output/Plots/chTOG_boxPlot.pdf", plot = chTOG_plot, width = 75, height = 75, units = 'mm', useDingbats = FALSE)
ggsave("Output/Plots/combined_plot.pdf", plot = combined_plot, width = 62.599, height = 101, units = 'mm', useDingbats = FALSE)
