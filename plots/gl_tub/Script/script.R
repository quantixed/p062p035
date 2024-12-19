library(dplyr)
library(ggplot2)
library(tidyr)
# load all csv files in Data/ into a big data frame
# get all csv files in Data/
files <- list.files("Data/", pattern = ".csv", full.names = TRUE)
# read all csv files into a list
df <- do.call(rbind, lapply(files, function(x) {
  temp <- read.csv(x);
  if(NROW(temp) == 0) return(NULL);
  temp$file <- basename(x);
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
  group_by(file, cond, measurement) %>%
  summarise(avg = mean(Mean))
# for each file we now have two rows one for microtubule and one for cytoplasm
# pivot wider the data so that we have one row per file
quant_df <- quant_df %>%
  pivot_wider(names_from = measurement, values_from = avg)

# calculate the ratio of microtubule/cytoplasm
quant_df$rat <- quant_df$microtubule / quant_df$cytoplasm
# make a boxplot of the ratio of microtubule/cytoplasm and facet by cond
ggplot(quant_df, aes(x = cond, y = rat)) +
  geom_boxplot() +
  labs(y = "Microtubule/Cytoplasm ratio", x = "") +
  theme_classic()

count_df <- df %>%
  group_by(file, condAB, cond, noco) %>%
  summarise(foci = n())
# make a frequency plot of foci counts and facet by cond
ggplot(count_df, aes(x = foci)) +
  geom_bar() +
  facet_grid(cond~noco) +
  theme_classic()
ggplot(count_df, aes(x = foci)) +
  geom_histogram(binwidth = 1) +
  facet_grid(cond~noco, scales = "free_y") +
  theme_classic()
