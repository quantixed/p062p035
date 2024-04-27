# Centrin/Pericentrin data visualisation script written by James Shelford
# Script to combine the df's generated in the process script and produce plots

# Load the required packages
library(tidyverse)
require(cowplot)
require(RColorBrewer)

# wd should be Centrin / Pericentrin
# setup preferred directory structure in wd
ifelse(!dir.exists("Data"), dir.create("Data"), "Folder exists already")
ifelse(!dir.exists("Output"), dir.create("Output"), "Folder exists already")
ifelse(!dir.exists("Output/Dataframes"), dir.create("Output/Dataframes"), "Folder exists already")
ifelse(!dir.exists("Output/Dataframes/RawData"), dir.create("Output/Dataframes/RawData"), "Folder exists already")
ifelse(!dir.exists("Output/Plots"), dir.create("Output/Plots"), "Folder exists already")
ifelse(!dir.exists("Scripts"), dir.create("Scripts"), "Folder exists already")

# Load the dataframes for all experiments. These are rds files found in Output/Dataframes
datadir <- "Output/Dataframes"
my_files <- list.files(datadir,pattern='*.rds',full.names = TRUE)
combined_df <- bind_rows(lapply(my_files, readRDS))
combined_df$Category <- as.factor(combined_df$Category)
combined_df$Centrin_foci <- as.factor(combined_df$Centrin_foci)
combined_df$file_name <- as.factor(combined_df$file_name)

# Set levels for centrin
combined_df$Centrin_foci <- factor(combined_df$Centrin_foci, levels = c('0', '1', '2', '3', '4', '5', '6'))

# Combining the Raw data from the individual dfs. This df will be used to calculate % cells with >2 PCNT foci
Rawdatadir <- "Output/Dataframes/RawData"
my_raw_files <- list.files(Rawdatadir,pattern='*.rds',full.names = TRUE)
combined_df_raw <- bind_rows(lapply(my_raw_files, readRDS))
combined_df_raw$Category <- as.factor(combined_df_raw$Category)

# Add a category to classify number of objects detected
combined_df_raw$pole_cat <- 2
combined_df_raw$pole_cat[combined_df_raw$total_pericentrin_foci>2] <- '>2'

# How many cells in each condition? Add this to df.
num_conditions <- summary(combined_df_raw$Category)
combined_df_raw$total_cell_num[combined_df_raw$Category=='mCherry'] <- num_conditions['mCherry']
combined_df_raw$total_cell_num[combined_df_raw$Category=='E4'] <- num_conditions['E4']
combined_df_raw$total_cell_num[combined_df_raw$Category=='E7'] <- num_conditions['E7']
combined_df_raw$total_cell_num[combined_df_raw$Category=='E8'] <- num_conditions['E8']
combined_df_raw$total_cell_num <- as.numeric(combined_df_raw$total_cell_num)

# Subset df to contain only cells >2 foci. Use this to work out % frequency of cells >2 pericentrin foci
subset_df <- subset(combined_df_raw, pole_cat == '>2', select = c('file_name', 'total_pericentrin_foci', 'blind_list', 'Category', 
                                                                  'Experiment_number', 'pole_cat', 'total_cell_num'))
num_GreaterThan2 <- summary(subset_df$Category)
mCh_freq <- (num_GreaterThan2['mCherry'] / num_conditions['mCherry']) * 100
mCh_freq
E4_freq <- (num_GreaterThan2['E4'] / num_conditions['E4']) * 100
E4_freq
E7_freq <- (num_GreaterThan2['E7'] / num_conditions['E7']) * 100
E7_freq
E8_freq <- (num_GreaterThan2['E8'] / num_conditions['E8']) * 100
E8_freq

# Diverging or sequential palette for scale gradient?

# Create custom colour scale
myColours <- brewer.pal(7, 'Set1')
names(myColours) <- levels(combined_df$Centrin_foci)
colourScale <- scale_color_manual(name = 'Centrin foci', values = myColours)

plot_the_data <- function(dataframe, category){
  the_plot <- ggplot(filter(dataframe, Category == category ), aes(x = reorder(file_name, PCNT), y = PCNT, colour = Centrin_foci)) +
    theme_cowplot() +
    geom_point(size=1) +
    colourScale +
    theme(axis.text.x = element_blank(), 
          axis.text.y = element_text(face = 'plain', color = 'black', size = 9)) +
    theme(axis.title.y = element_text(size = 9, face = 'plain', color = 'black', 
                                      margin = margin(0,0,0,0,'mm')),
          axis.title.x = element_text(size = 9, face = 'plain', color = 'black'),
          legend.title = element_text(size = 9, face = 'bold', colour = 'black'),
          legend.text = element_text(size = 9, face = 'plain', colour = 'black'),
          legend.justification = 'center',
          legend.background = element_rect(colour = 'black'),
          legend.margin = margin(0.2,0.2,0.2,0.2,'cm'),
          plot.margin = margin(.5,1,.5,1, 'mm')) + # top, right, bottom, left
    labs(x = 'Cell') + 
    scale_y_continuous(name = 'PCNT foci', limits = c(0,10), breaks = c(0,1,2,3,4,5,6,7,8,9,10)) 
    #scale_color_gradientn(0 = '#990000', 1 = '#d7103f', 2 = 'ef6548', 3 = '#fc8d59', 4 = 'fdbb84', 5 = '#fdd49e', 6 = '#fef0d9')
    #scale_colour_gradientn(colours = rainbow(6)) 
  
  return(the_plot)
}

mCh_plot <- plot_the_data(combined_df, 'mCherry')
mCh_plot
E4_plot <- plot_the_data(combined_df, 'E4')
E4_plot
E7_plot <- plot_the_data(combined_df, 'E7')
E7_plot
E8_plot <- plot_the_data(combined_df, 'E8')
E8_plot

combined_plot <- plot_grid(nrow = 4, mCh_plot + theme(legend.position = "none"), E4_plot + theme(legend.position = "none"),
                           E7_plot + theme(legend.position = 'none'), E8_plot + theme(legend.position = 'none'),
                           align = 'hv', rel_widths = c(1, 1), rel_heights = c(1,1))
combined_plot

combined_plot_ppt <- plot_grid(nrow = 2, mCh_plot + theme(legend.position = "none"), E4_plot + theme(legend.position = "none"),
                               E7_plot + theme(legend.position = 'none'), E8_plot + theme(legend.position = 'none'),
                               align = 'hv', rel_widths = c(1, 1), rel_heights = c(1,1))
combined_plot_ppt

# Extracting a legend to add to the bottom of the combined plot
legend <- get_legend(E4_plot + guides(color = guide_legend(nrow = 1)) +
                       theme(legend.position = "bottom"))

legend_side <- get_legend(E4_plot + theme(legend.box.margin = margin(0,0,0,0)))

# Add the legend to the bottom of the combined plot
combined_plot_bottom <- plot_grid(combined_plot, legend, ncol = 1, rel_heights = c(1, .05))
combined_plot_bottom
# Add legend to the side of the combined plot
combined_plot_side <- plot_grid(combined_plot, legend_side, rel_widths = c(3,.4))
combined_plot_side
## Save the plots 
ggsave(filename = "Output/Plots/combined_plots_bottom.pdf", plot = combined_plot_bottom, units = "mm", height = 250, width = 250, useDingbats = FALSE)
ggsave(filename = "Output/Plots/combined_plots_side.pdf", plot = combined_plot_side, units = "mm", height = 300, width = 300, useDingbats = FALSE)
# combined plot no legend
ggsave(filename = "Output/Plots/combined_plot.pdf", plot = combined_plot, units = "mm", height = 140, width = 73.5, useDingbats = FALSE)
ggsave(filename = "Output/Plots/combined_plot_ppt.pdf", plot = combined_plot_ppt, units = "mm", height = 116, width = 135, useDingbats = FALSE)
