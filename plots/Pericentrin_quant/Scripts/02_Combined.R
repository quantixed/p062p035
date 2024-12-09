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
library(tidyverse)
library(rstatix)
require(ggbeeswarm)
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

### ----------------------------------- Defining PCNT category -------------------------------------- 

# Add a category to classify number of objects detected
combined_df$pole_cat <- 2
combined_df$pole_cat[combined_df$pericentrin_foci>2] <- '>2'

# How many cells in each condition? Add this to df.
num_conditions <- summary(combined_df$Category)
combined_df$total_cell_num[combined_df$Category=='mCherry C1'] <- num_conditions['mCherry C1']
combined_df$total_cell_num[combined_df$Category=='mCherry E4'] <- num_conditions['mCherry E4']
combined_df$total_cell_num[combined_df$Category=='mCherry E7'] <- num_conditions['mCherry E7']
combined_df$total_cell_num[combined_df$Category=='mCherry E8'] <- num_conditions['mCherry E8']
combined_df$total_cell_num <- as.numeric(combined_df$total_cell_num)

# Order the PCNT categories for plotting
combined_df$pole_cat <- factor(combined_df$pole_cat, levels = c('2', '>2'))

# Subset df to contain only cells >2 foci. Use this to work out % frequency of cells >2 pericentrin foci
subset_df <- subset(combined_df, pole_cat == '>2')
num_GreaterThan2 <- summary(subset_df$Category)
mCh_freq <- (num_GreaterThan2['mCherry C1'] / num_conditions['mCherry C1']) * 100
mCh_freq
E4_freq <- (num_GreaterThan2['mCherry E4'] / num_conditions['mCherry E4']) * 100
E4_freq
E7_freq <- (num_GreaterThan2['mCherry E7'] / num_conditions['mCherry E7']) * 100
E7_freq
E8_freq <- (num_GreaterThan2['mCherry E8'] / num_conditions['mCherry E8']) * 100
E8_freq
###--------------------- Plot the data -----------------------

## Is there a correlation between intensity of the fluorescent protein and a phenotype? # Can I gate bright cells and dim cells?
## Is there a difference between experiment
## Look at expression of GFP in cells >2 PCNT, correlation with localisation?

# how many cells in each condition?
summary(combined_df$Category)

# Colour by PCNT category
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
mean_intens <- plot_the_data(combined_df, "mean_intensity", "pericentrin pole intensity (mean)")
mean_intens
sum_intens <- plot_the_data(combined_df, "sum_intensity", "pericentrin pole intensity (sum)")
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
# Can I colour the dots by PCNT cat? 
mean_volume <- plot_the_data(combined_df, "ave_volume", "Mean pericentrin volume (micron^3)")
mean_volume
sum_volume <- plot_the_data(combined_df, "sum_volume", "Sum pericentrin volume (micron^3)")
sum_volume
mean_surface_area <- plot_the_data(combined_df, "mean_surface_area", "Mean pericentrin surface area (micron^2)")
mean_surface_area
sum_surface_area <- plot_the_data(combined_df, "sum_surface_area", "Sum pericentrin surface area (micron^2)")
sum_surface_area
volume_diff <- plot_the_data(combined_df, 'volume_diff', 'PCNT volume difference (micron^3)')
volume_diff 

combined_volumes <- plot_grid(mean_volume + theme(legend.position = "none"), sum_volume + theme(legend.position = "none"),
                              mean_surface_area + theme(legend.position = "none"), sum_surface_area + theme(legend.position = "none"),
                              align = 'hv', rel_widths = c(1, 1), rel_heights = c(1,1)) + theme(aspect.ratio = 1)
combined_volumes

# Add the legend to the bottom of the combined plot
combined_volumes <- plot_grid(combined_volumes, legend, ncol = 1, rel_heights = c(1, .05))
combined_volumes

#### Plot object number (pericentrin foci) as a scatter
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
  scale_y_continuous(name = 'Pericentrin foci number', limits = c(0,12), breaks = c(0,2,4,6,8,10,12))
foci_dot

# Plotting the cell intensity values
p <- ggplot(data = combined_df, aes(x = Category, y = GFP_intensity_mean, colour = pole_cat)) +
  theme_cowplot() +
  geom_quasirandom(alpha=0.5, stroke=0) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult=1), aes(group=Category), colour = 'black') +
  theme(axis.text.x = element_text(face = "plain", color = 'black', size = 9, angle = 0, hjust = 0.5, vjust = 0.5), 
        axis.text.y = element_text(face = 'plain', color = 'black', size = 9)) +
  theme(axis.title.y = element_text(size = 10,face = 'plain', color = 'black'), 
        legend.title = element_text(size = 10, face = 'bold', colour = 'black'),
        legend.text = element_text(size = 10, face = 'plain', colour = 'black'),
        legend.justification = 'center',
        legend.background = element_rect(colour = 'black'),
        legend.margin = margin(0.2,0.2,0.2,0.2,'cm'),
        plot.margin = margin(0.2,0.2,0.2,0.2, 'cm')) +
  labs(y = 'mCh intensity (mean)', x = NULL) + 
  theme(legend.position = 'bottom') +
  theme(legend.title = element_blank()) 
p

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

## Save the plots
# when importing the plot into illustrator save as pdf 
ggsave("./Output/Plots/combined_volumes.pdf", plot = combined_volumes, width = 200, height = 200, units = 'mm', useDingbats = FALSE)
ggsave("./Output/Plots/combined_int.pdf", plot = combined_intensity, width = 200, height = 200, units = 'mm', useDingbats = FALSE)
ggsave("./Output/Plots/foci_dot.pdf", plot = foci_dot, height = 150, width = 180, units = 'mm', useDingbats = FALSE)

# Combined plot
ggsave("./Output/Plots/combined_plot.pdf", plot = combined_plot, height = 78, width = 139, units = 'mm', useDingbats = FALSE)


##  --------------------- Statistics ------------------------

# Packages to visualise contingency tables
library(summarytools)
library(corrplot)
library(gplots)

# Make 2X2 contingency tables to look at association between PCM fragmentation and Affimer
# mCh Vs E4
# mCh Vs E7
# mCh Vs E8
tab <- table(combined_df$Category, combined_df$pole_cat)
mCh_E4_tab <- table(combined_df$Category, combined_df$pole_cat, exclude = c('mCherry E7', 'mCherry E8'))
mCh_E7_tab <- table(combined_df$Category, combined_df$pole_cat, exclude = c('mCherry E4', 'mCherry E8'))
mCh_E8_tab <- table(combined_df$Category, combined_df$pole_cat, exclude = c('mCherry E4', 'mCherry E7'))

# chi square test, if expected freq < 5, use Fisher test
chisq_res <- chisq.test(mCh_E4_tab)
chisq_res$expected

# Fisher's exact test
# Examines whether rows and columns of contingency table are statistically significantly associated.
# Null hypothesis (H0): the row and the column variables of the contingency table are independent.
# Alternative hypothesis (H1): row and column variables are dependent/associated. 
# The outcome of PCM fragmentation is affected by the construct type
mCh_E4_fishers <- fisher.test(mCh_E4_tab)
mCh_E7_fishers <- fisher.test(mCh_E7_tab)
mCh_E8_fishers <- fisher.test(mCh_E8_tab)

# Add p values to a vector
pVal <- c(mCh_E4_fishers$p.value, mCh_E7_fishers$p.value, mCh_E8_fishers$p.value)

# Adjust p values using Bonferroni method for multiple comparisons
p.adjust(pVal, method="bonferroni", n = length(pVal))

# Percentages by row
round(100*prop.table(table(combined_df$Category, combined_df$pole_cat), 1), 2)

# Visualise contengency table using balloon plot
#tranpose t(tab)
balloonplot(t(tab), main = NULL, xlab ="Phenotype", ylab="Category",
            label = FALSE, show.margins = FALSE)

# Calculate Pearson residuals for each cell to determine the most contributing cells to the Chi-square score
# Cells with the highest absolute (includes -ve values) standardized residuals contribute the most to the total Chi-square score
round(chisq_res$residuals, 3)

# Visualise Pearson residuals
# Size of the circle is proportional to the contribution
# Positive association shown in blue
# Negative association / not associated shown in red
Pearson_residuals <- corrplot(chisq_res$residuals, is.cor = FALSE)

# The contribution (in %) of a given cell to the total Chi-square score
contrib <- 100*chisq_res$residuals^2/chisq_res$statistic
round(contrib, 3)

# visualise contribution
corrplot(contrib, is.cor = FALSE)

# ------------ Statistics on the PCNT volume data -------------

# Spindle length
# Visualise distribution of data
# Density plot
ggdensity(combined_df$sum_volume, fill = 'lightgray')
# QQ plot
ggqqplot(combined_df$sum_volume)

# Use significance test to check normality
# Use the rstatix package which is pipe-friendly. Null hypothesis is norm dist.
combined_df %>% group_by(Category) %>% shapiro_test(sum_volume)

# Check for homogeneity of variance across the Categories using levene test, 
# uses rstatix package and is pipe-friendly. Null is equal variances
combined_df %>% levene_test(sum_volume ~ Category)

### ----------- Non parametric test ------------
# Data does not follow a normal dist

# Kruskal-Wallis rank sum test
PCNT_volume_Kruskal <- combined_df %>% kruskal_test(sum_volume ~ Category)
# Post-hoc pairwise comparisons
PCNT_volume_Dunn <- combined_df %>% dunn_test(sum_volume ~ Category, p.adjust.method = 'bonferroni')


### ------------ Final figure ---------------

p1 <- ggplot(combined_df, aes(x = pericentrin_foci, fill = pole_cat)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(. ~ Category, ncol = 4) +
  labs(x = "PCNT foci", y = "Count") +
  theme_cowplot(9) +
  theme(legend.position = "none")

ggsave("Output/Plots/pcnt_histo.pdf", width = 84, height = 42, units = "mm")

p2 <- sum_volume + theme(legend.position = 'none')

ggsave("Output/Plots/pcnt_vol.pdf", width = 88, height = 46, units = "mm")

# Superplot of the data
# make summaries of the data per experiment
expt_df <- combined_df %>% 
  group_by(Category, Experiment_number, pole_cat) %>%
  summarise(mean = mean(sum_volume), sd = sd(sum_volume), n = n())

# statistical test
aov_res <- aov(mean ~ Category*pole_cat, data = expt_df)
summary(aov_res)
tukey_res <- TukeyHSD(aov_res)
tukey_res

# prepare data for superplot
combined_df$condAB <- paste0(combined_df$Category, "\n", ifelse(combined_df$pole_cat == "2", "2", "Multi"))
expt_df$condAB <- paste0(expt_df$Category, "\n", ifelse(expt_df$pole_cat == "2", "2", "Multi"))
# remove mCherry C1_Multi and mCherry E7_Multi from the data because there are so few points
combined2_df <- combined_df %>% filter(!(condAB == "mCherry C1\nMulti" | condAB == "mCherry E4\nMulti"))
expt2_df <- expt_df %>% filter(!(condAB == "mCherry C1\nMulti" | condAB == "mCherry E4\nMulti"))

# plot the data
ggplot() +
  geom_sina(data = combined2_df, aes(x = condAB, y = sum_volume, colour = Experiment_number, shape = pole_cat), alpha = 0.5, position = "auto", size = 0.8, maxwidth = 0.3) +
  geom_point(data = expt2_df, aes(x = condAB, y = mean, fill = Experiment_number), shape = 22, size = 1.5, stroke = 0.5, alpha = 0.7) +
  scale_color_manual(values = c("#4477aa", "#ccbb44", "#ee6677", "#000000")) +
  scale_fill_manual(values = c("#4477aa", "#ccbb44", "#ee6677", "#000000")) +
  scale_shape_manual(values = c(1, 16)) +
  labs(x = "", y = "Sum pericentrin volume (micron^3)") +
  ylim(0, NA) +
  theme_cowplot(9) +
  theme(legend.position = "none")

ggsave("Output/Plots/pcnt_vol2.pdf", width = 88, height = 46, units = "mm")


### --------------- Does expression correlate with phenotype? ---------------

# Referee 3 Point 2b asked if increased expression of E7 or E8 correlates with phenotype.

# make summaries of the data per experiment
expt_df <- combined_df %>% 
  group_by(Category, Experiment_number, pole_cat) %>%
  summarise(mean = mean(GFP_intensity_mean), sd = sd(GFP_intensity_mean), n = n())

# statistical test
aov_res <- aov(mean ~ Category*pole_cat, data = expt_df)
summary(aov_res)
tukey_res <- TukeyHSD(aov_res)
tukey_res

# prepare data for superplot
combined_df$condAB <- paste0(combined_df$Category, "\n", ifelse(combined_df$pole_cat == "2", "2", "Multi"))
expt_df$condAB <- paste0(expt_df$Category, "\n", ifelse(expt_df$pole_cat == "2", "2", "Multi"))
# remove mCherry C1_Multi and mCherry E7_Multi from the data because there are so few points
combined2_df <- combined_df %>% filter(!(condAB == "mCherry C1\nMulti" | condAB == "mCherry E4\nMulti"))
expt2_df <- expt_df %>% filter(!(condAB == "mCherry C1\nMulti" | condAB == "mCherry E4\nMulti"))

# plot the data
ggplot() +
  geom_sina(data = combined2_df, aes(x = condAB, y = GFP_intensity_mean, colour = Experiment_number, shape = pole_cat), alpha = 0.5, position = "auto", size = 0.8, maxwidth = 0.3) +
  geom_point(data = expt2_df, aes(x = condAB, y = mean, fill = Experiment_number), shape = 22, size = 1.5, stroke = 0.5, alpha = 0.7) +
  scale_color_manual(values = c("#4477aa", "#ccbb44", "#ee6677", "#000000")) +
  scale_fill_manual(values = c("#4477aa", "#ccbb44", "#ee6677", "#000000")) +
  scale_shape_manual(values = c(1, 16)) +
  scale_y_log10() +
  labs(x = "", y = "mCherry intensity (A.U.)") +
  theme_cowplot(9) +
  theme(legend.position = "none")

ggsave("Output/Plots/pcnt_expression.pdf", width = 120, height = 50, units = "mm")
