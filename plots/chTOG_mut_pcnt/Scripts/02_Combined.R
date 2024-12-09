#Script written by James Shelford to combine analysis from multiple experiments
# Load in dataframes produced from processing individual experiments and plot data

# Working directory should be 'Pericentrin_quant'
# Setup preferred directory structure in wd
ifelse(!dir.exists("Data"), dir.create("Data"), "Folder exists already")
ifelse(!dir.exists("Output"), dir.create("Output"), "Folder exists already")
ifelse(!dir.exists("Output/Dataframe"), dir.create("Output/Dataframe"), "Folder exists already")
ifelse(!dir.exists("Output/Plots"), dir.create("Output/Plots"), "Folder exists already")
ifelse(!dir.exists("Scripts"), dir.create("Scripts"), "Folder exists already")

# Load the required packages
require(ggplot2)
require(ggbeeswarm)
library(dplyr)
library(multcomp)
library(cowplot)
library(ggpubr)
library(scales)
library(ggforce)

# Load the dataframes (.rds) for each experiment. These should be in Output/Dataframe
datadir <- 'Output/Dataframe'

my_dataframes <- list.files(datadir, pattern = '*.rds', full.names = TRUE)
combined_df <- bind_rows(lapply(my_dataframes, readRDS))
combined_df$Category <- as.factor(combined_df$Category)

# Use filter to remove delTOG6 category
combined_df <- filter(combined_df, Category != 'delTOG6')

### ----------------------------------- Defining PCNT category -------------------------------------- 

# Add a category to classify number of objects detected
combined_df$pole_cat <- 2
combined_df$pole_cat[combined_df$pericentrin_foci>2] <- '>2'

# How many cells in each condition? Add this to df.
num_conditions <- summary(combined_df$Category)
combined_df$total_cell_num[combined_df$Category=='GFP'] <- num_conditions['GFP']
combined_df$total_cell_num[combined_df$Category=='WT chTOG'] <- num_conditions['WT chTOG']
combined_df$total_cell_num[combined_df$Category=='LLAA'] <- num_conditions['LLAA']
combined_df$total_cell_num <- as.numeric(combined_df$total_cell_num)

# Order the PCNT categories for plotting
combined_df$pole_cat <- factor(combined_df$pole_cat, levels = c('2', '>2'))

###--------------------- Plot the data -----------------------

# how many cells in each condition?
summary(combined_df$Category)

# Set plotting levels 
combined_df$Category <- factor(combined_df$Category, levels = c('GFP', 'WT chTOG', 'LLAA'))

plot_the_data <- function(df, measurement, y_label){
  the_plot <- ggplot(data = df, aes_string(x = "Category", y = measurement, colour = "pole_cat")) +
    theme_cowplot() +
    geom_quasirandom(alpha = 0.5, stroke = 0) +
    stat_summary(fun.data = mean_sdl, fun.args = list(mult=1), aes(group=Category)) +
    #geom_violin() +
    #geom_boxplot(width=0.1, fill = "white", outlier.shape = NA) +
    theme(axis.text.x = element_text(face = "plain", color = 'black', size = 9, angle = 0, hjust = 0.5, vjust = 0.5), 
          axis.text.y = element_text(face = 'plain', color = 'black', size = 9)) +
    theme(axis.title.y = element_text(size = 10,face = 'plain', color = 'black'), 
          legend.title = element_text(size = 10, face = 'bold', colour = 'black'),
          legend.text = element_text(size = 10, face = 'plain', colour = 'black'),
          legend.justification = 'center',
          legend.background = element_rect(colour = 'black'),
          legend.margin = margin(0.2,0.2,0.2,0.2,'cm'),
          plot.margin = margin(0.2,0.2,0.2,0.2, 'cm')) +
    labs(y = y_label, x = NULL) + 
    theme(legend.position = 'bottom') +
    theme(legend.title = element_blank()) +
    ylim(0, NA)
  return(the_plot)
}

## pericentrin intensity
mean_intens <- plot_the_data(combined_df, "mean_intensity", "pericentrin pole mean intensity")
mean_intens
sum_intens <- plot_the_data(combined_df, "sum_intensity", "pericentrin pole sum intensity")
sum_intens

combined_intensity <- plot_grid(mean_intens + theme(legend.position = "none"), sum_intens + theme(legend.position = "none"), 
                                align = 'hv', rel_widths = c(1, 1), rel_heights = c(1,1)) + theme(aspect.ratio = 1)
combined_intensity

# Extracting a legend to add to the bottom of the combined plot
legend <- get_legend(mean_intens + guides(color = guide_legend(nrow = 1)) +
                       theme(legend.position = "bottom"))

# Add the legend to the bottom of the combined plot
combined_intensity <- plot_grid(combined_intensity, legend, ncol = 1, rel_heights = c(1, .05))
combined_intensity

# Pericentrin volume
mean_volume <- plot_the_data(combined_df, "ave_volume", "Mean pericentrin volume (micron^3)")
mean_volume
sum_volume <- plot_the_data(combined_df, "sum_volume", "Sum pericentrin volume (micron^3)")
sum_volume
mean_surface_area <- plot_the_data(combined_df, "mean_surface_area", "mean surface area (micron^2)")
mean_surface_area
sum_surface_area <- plot_the_data(combined_df, "sum_surface_area", "sum surface area")
sum_surface_area

combined_volumes <- plot_grid(mean_volume + theme(legend.position = "none"), sum_volume + theme(legend.position = "none"),
                              mean_surface_area + theme(legend.position = "none"), sum_surface_area + theme(legend.position = "none"),
                              align = 'hv', rel_widths = c(1, 1), rel_heights = c(1,1)) + theme(aspect.ratio = 1)
combined_volumes

# Add the legend to the bottom of the combined plot
combined_volumes <- plot_grid(combined_volumes, legend, ncol = 1, rel_heights = c(1, .05))
combined_volumes

# Plot object number (pericentrin foci) as a stacked barchart to see the frequency
pericentrin_foci <- ggplot(data = combined_df, aes(fill=pole_cat, y=total_cell_num, x=Category)) + 
  theme_cowplot() + 
  coord_flip() +
  geom_bar(position = "fill",stat = "identity",width = 0.96) +
  theme(axis.text.x = element_text(face= "plain", color= 'black', size=9, angle = 0, hjust = 0.3, vjust = 0.5), 
        axis.text.y = element_text(face = 'plain', color= 'black', size=9)) + 
  theme(axis.title.x = element_text(size = 10, face='plain',color='black')) +
  labs(y = "Frequency", x = NULL) + 
  scale_y_continuous(labels=percent, expand = c(0,0)) +
  scale_x_discrete(limits = rev(levels(combined_df$Category)), expand = c(0,0)) +
  theme(legend.title = element_blank(), legend.position = 'right', legend.justification = 'centre', legend.text = element_text(size = 9, colour = 'black')) + 
  scale_fill_discrete(labels = c(">2", "2")) 
pericentrin_foci

# plot as scatter

foci_dot <- ggplot(data = combined_df, aes(x = Category, y = pericentrin_foci, colour = pole_cat)) +
  theme_cowplot() +
  geom_quasirandom(alpha = 0.5, stroke = 0) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult=1), aes(group=Category)) +
  theme(axis.text.x = element_text(face = "plain", color = 'black', size = 9, angle = 0, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(face = 'plain', color = 'black', size = 9)) +
  theme(axis.title.y = element_text(size = 10,face = 'plain', color = 'black'), 
        plot.margin = margin(0.2,0.2,0.2,0.2, 'cm')) + # top, right, bottom, left
  labs(x = NULL) + 
  theme(legend.position = 'none') +
  theme(legend.title = element_blank()) +
  scale_y_continuous(name = 'Pericentrin foci number', limits = c(0,8), breaks = c(0,2,4,6,8))
foci_dot

###### Combine PCNT foci dot plot and PCNT volume plot

combined_plot <- plot_grid(foci_dot + theme(legend.position = 'none'), 
                           sum_volume + theme(legend.position = 'none'), align = 'hv', rel_widths = c(1, 1), rel_heights = c(1,1))
combined_plot

# Add legend
# Extracting a legend to add to the bottom of the combined plot
legend <- get_legend(sum_volume + guides(color = guide_legend(nrow = 1)) +
                       theme(legend.position = "bottom"))

# Add the legend to the bottom of the combined plot
combined_plot <- plot_grid(combined_plot, legend, ncol = 1, rel_heights = c(1, .05))
combined_plot



# Subset df to contain only cells >2 foci. Use this to work out % frequency of cells >2 pericentrin foci
subset_df <- subset(combined_df, pole_cat == '>2')
num_GreaterThan2 <- summary(subset_df$Category)
GFP_freq <- (num_GreaterThan2['GFP'] / num_conditions['GFP']) * 100
GFP_freq
WT_chTOG_freq <- (num_GreaterThan2['WT chTOG'] / num_conditions['WT chTOG']) * 100
WT_chTOG_freq
LLAA_freq <- (num_GreaterThan2['LLAA'] / num_conditions['LLAA']) * 100
LLAA_freq


### For paper ----

p1 <- ggplot(combined_df, aes(x = pericentrin_foci, fill = pole_cat)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(. ~ Category, ncol = 4) +
  labs(x = "PCNT foci", y = "Count") +
  theme_cowplot(9) +
  theme(legend.position = "none")

ggsave("Output/Plots/pcnt_histo.pdf", p1, width = 69, height = 42, units = "mm")

p2 <- sum_volume + theme(legend.position = 'none')

ggsave("Output/Plots/pcnt_vol.pdf", p2, width = 70, height = 46, units = "mm")

# Superplot of the data
# make summaries of the data per experiment
expt_df <- combined_df %>% 
  group_by(Category, Experiment_number, pole_cat) %>%
  summarise(mean = mean(sum_volume), sd = sd(sum_volume), n = n())

# statistical test
aov_res <- aov(mean ~ Category*pole_cat, data = expt_df)
summary(aov_res)

# prepare data for superplot
combined_df$condAB <- paste0(combined_df$Category, "\n", ifelse(combined_df$pole_cat == "2", "2", "Multi"))
expt_df$condAB <- paste0(expt_df$Category, "\n", ifelse(expt_df$pole_cat == "2", "2", "Multi"))
# refactor so that x-axis is correct
combined_df$condAB <- factor(combined_df$condAB, levels = c("GFP\n2","GFP\nMulti","WT chTOG\n2","WT chTOG\nMulti","LLAA\n2","LLAA\nMulti"))
expt_df$condAB <- factor(expt_df$condAB, levels = c("GFP\n2","GFP\nMulti","WT chTOG\n2","WT chTOG\nMulti","LLAA\n2","LLAA\nMulti"))

# plot the data
ggplot() +
  geom_sina(data = combined_df, aes(x = condAB, y = sum_volume, colour = Experiment_number, shape = pole_cat), alpha = 0.5, position = "auto", size = 0.8, maxwidth = 0.3) +
  geom_point(data = expt_df, aes(x = condAB, y = mean, fill = Experiment_number), shape = 22, size = 1.5, stroke = 0.5, alpha = 0.7) +
  scale_color_manual(values = c("#4477aa", "#ccbb44", "#ee6677", "#000000")) +
  scale_fill_manual(values = c("#4477aa", "#ccbb44", "#ee6677", "#000000")) +
  scale_shape_manual(values = c(1, 16)) +
  labs(x = "", y = "Sum pericentrin volume (micron^3)") +
  ylim(0, NA) +
  theme_cowplot(9) +
  theme(legend.position = "none")

ggsave("Output/Plots/pcnt_vol2.pdf", width = 70, height = 50, units = "mm")

# stats ----

xtab <- as.table(rbind(
  c(num_conditions['GFP'] - num_GreaterThan2['GFP'], num_conditions['WT chTOG'] - num_GreaterThan2['WT chTOG'], num_conditions['LLAA'] - num_GreaterThan2['LLAA']),
  c(num_GreaterThan2['GFP'], num_GreaterThan2['WT chTOG'], num_GreaterThan2['LLAA'])
))
dimnames(xtab) <- list(
  pcnt = c("two", "plustwo"),
  cond = c("GFP", "WT", "LLAA")
)
library(rstatix)
pairwise_fisher_test(xtab)