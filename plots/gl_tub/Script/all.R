library(dplyr)
library(ggplot2)
library(tidyr)
library(SuperPlotR)
# load all csv files in Data/ into a big data frame
# get all csv files in Data/ they are in subfolders
files <- list.files("Data/", pattern = ".csv", recursive = TRUE, full.names = TRUE)
# read all csv files into a list
df <- do.call(rbind, lapply(files, function(x) {
  temp <- read.csv(x);
  if(NROW(temp) == 0) return(NULL);
  temp$file <- basename(x);
  temp$expt <- basename(dirname(x));
  temp}))
# parse the file column for the presence of E4, E7 or E8, anything else is considered mCh
df$cond <- ifelse(grepl("E4", df$file), "E4",
                  ifelse(grepl("E7", df$file), "E7",
                         ifelse(grepl("E8", df$file), "E8",
                                "mCh")))

# extract channel 3 only
df <- df[df$channel == 3,]

# factor the cond column as mCh, E4, E7, E8
df$cond <- factor(df$cond, levels = c("mCh", "E4", "E7", "E8"))

# quant microtubule/cytoplasm in each file
# start by smmarising mean of microtubule and cytoplasm
quant_df <- df %>%
  group_by(expt, file, cond, measurement) %>%
  summarise(avg = mean(Mean))
# for each file we now have two rows one for microtubule and one for cytoplasm
# pivot wider the data so that we have one row per file
quant_df <- quant_df %>%
  pivot_wider(names_from = measurement, values_from = avg)

# calculate the ratio of microtubule/cytoplasm
quant_df$rat <- quant_df$microtubule / quant_df$cytoplasm

# superplot(quant_df, "rat", "cond", "expt", ylab = "Microtubule/Cytoplasm ratio")
superplot(quant_df, "microtubule", "cond", "expt",
          ylab = "Microtubule intensity", bars = "mean_sd", size = c(0.8,1.5), fsize = 9)
ggsave("Output/Plots/microtubule_intensity.pdf", width = 50, height = 50, units = "mm")

# anova of microtubule intensity followed by tukeyHSD
anova_df <- quant_df %>%
  group_by(expt, cond) %>%
  summarise(avg = mean(microtubule))
anova_res <- aov(avg ~ cond, data = anova_df)
summary(anova_res)

tukey_res <- TukeyHSD(anova_res)
tukey_res