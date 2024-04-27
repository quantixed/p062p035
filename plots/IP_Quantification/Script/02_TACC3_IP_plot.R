# Script written by James Shelford
# Script to combine dataframes and plot results from Western blot quantification
# Working directory should be 'TACC3_IP'

# Load required packages
require(ggplot2)
require(ggbeeswarm)
library(dplyr)
library(multcomp)
library(cowplot)
library(ggpubr)
library(scales)

# Load the dataframes (.rds) for each experiment. These should be in Output/Dataframe
datadir <- 'Output/Dataframe'

my_dataframes <- list.files(datadir, pattern = '*.rds', full.names = TRUE)
combined_df <- bind_rows(lapply(my_dataframes, readRDS))
combined_df$Category <- as.factor(combined_df$Category)

# Calculate mean, sd and max/min error for the ratio value of mCh / chTOG for each category

summary_table <- summarise(group_by(combined_df, Category), mCh_norm_mean = mean(normalised_mCh), mCh_norm_sd = sd(normalised_mCh), 
                               mCh_norm_error_min = mean(normalised_mCh) - sd(normalised_mCh), mCh_norm_error_max = mean(normalised_mCh) + sd(normalised_mCh),
                               chTOG_norm_mean = mean(normalised_chTOG), chTOG_norm_sd = sd(normalised_chTOG), chTOG_norm_error_min = mean(normalised_chTOG) - sd(normalised_chTOG), chTOG_norm_error_max = mean(normalised_chTOG) + sd(normalised_chTOG), 
                               mCh_raw_mean = mean(mCh_value), mCh_raw_sd = sd(mCh_value), 
                               mCh_error_min = mean(mCh_value) - sd(mCh_value), mCh_error_max = mean(mCh_value) + sd(mCh_value),
                               chTOG_raw_mean = mean(chTOG_value), chTOG_raw_sd = sd(chTOG_value), 
                               chTOG_error_min = mean(chTOG_value) - sd(chTOG_value), chTOG_error_max = mean(chTOG_value) + sd(chTOG_value),
                               TACC3_raw_mean = mean(TACC3_value), TACC3_raw_sd = sd(TACC3_value),
                               TACC3_error_min = mean(TACC3_value) - sd(TACC3_value), TACC3_error_max = mean(TACC3_value) + sd(TACC3_value))

summary_table_ratio <- summarise(group_by(combined_df, Category), mCh_ratio_mean = mean(mCh_ratio), mCh_ratio_sd = sd(mCh_ratio), 
                                 mCh_error_min = mean(mCh_ratio) - sd(mCh_ratio), mCh_error_max = mean(mCh_ratio) + sd(mCh_ratio), 
                                 chTOG_ratio_mean = mean(chTOG_ratio), chTOG_ratio_sd = sd(chTOG_ratio), 
                                 chTOG_error_min = mean(chTOG_ratio) - sd(chTOG_ratio), chTOG_error_max = mean(chTOG_ratio) + sd(chTOG_ratio))

# Set levels for plotting in order
combined_df$Category <- factor(combined_df$Category, levels = c('mCh', 'E4', 'E7', 'E8'))
combined_df$Experiment <- factor(combined_df$Experiment, levels = c('JS132', 'JS173', 'JS174'))
summary_table$Category <- factor(summary_table$Category, levels = c('mCh', 'E4', 'E7', 'E8'))
summary_table_ratio$Category <- factor(summary_table_ratio$Category, levels = c('mCh', 'E4', 'E7', 'E8'))

dot_plot_func <- function(dataframe, measurement, y_label){
  the_plot <- ggplot(data = dataframe, aes_string(x = 'Category', y = measurement, colour = 'Experiment')) +
    theme_cowplot() +
    geom_point(alpha=0.9, stroke=0, size = 3, position = position_dodge(width = 0.5)) +
    stat_summary(fun.data = mean_sdl, geom = 'crossbar', width = 0.3, colour ='black', fun.args = list(mult=0), aes(group=Category)) +
    theme(axis.title.y = element_text(size = 10, face = 'plain', color = 'black'),
          axis.title.x = element_text(size = 10, face = 'plain', color = 'black'),
          axis.text.y = element_text(face = 'plain', color = 'black', size = 9),
          axis.text.x = element_text(face = 'plain', color = 'black', size = 9, hjust = 0.5),
          legend.title = element_text(size = 10, face = 'bold', colour = 'black'),
          legend.text = element_text(size = 10, face = 'plain', colour = 'black'),
          legend.justification = 'center',
          #legend.background = element_rect(colour = 'black'),
          legend.margin = margin(0.2,0.2,0.2,0.2,'cm'),
          plot.margin = margin(0.4,0.5,0.4,0.5, 'cm')) +
    scale_color_hue(name = NULL, labels = c("1", "2", '3')) +
    labs(y = y_label, x = NULL) +
    theme(legend.position = 'right', legend.box = 'vertical') +
    ylim(0, NA)
  
  return(the_plot)
  }
  
# Dot plots
mCh_dot_plot <- dot_plot_func(combined_df, 'normalised_mCh', 'mCherry binding (normalised)')
mCh_dot_plot
chTOG_dot_plot <- dot_plot_func(combined_df, 'normalised_chTOG', 'chTOG binding (normalised)')
chTOG_dot_plot


# Combined dot plots
combined_dot_plots <- plot_grid(mCh_dot_plot + theme(legend.position = 'none'), chTOG_dot_plot + theme(legend.position = 'none'), 
                                align = 'hv', rel_widths = c(1, 1), rel_heights = c(1,1)) + theme(aspect.ratio=0.45)
combined_dot_plots

# Extracting a legend to add to the bottom of the combined plot
legend <- get_legend(mCh_dot_plot + guides(color = guide_legend(nrow = 1)) +
                       theme(legend.position = "bottom"))

legend_side <- get_legend(mCh_dot_plot + theme(legend.box.margin = margin(0,0,0,0)))


# Add legend to the side of the combined plot
combined_plot_side <- plot_grid(combined_dot_plots, legend_side, rel_widths = c(3,.4))
combined_plot_side

# Save the plots
# when importing the plot into illustrator save as pdf 
ggsave("./Output/Plots/combined_dot_plots_side.pdf", plot = combined_plot_side, width = 120, height = 90, units = 'mm', useDingbats = FALSE)
