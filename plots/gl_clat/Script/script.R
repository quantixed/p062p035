library(dplyr)
library(ggplot2)
library(tidyr)
library(SuperPlotR)
library(patchwork)
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
                                ifelse(grepl("MLN", df$file), "MLN",
                                  "mCh"))))

# factor the cond column as mCh, E4, E7, E8
df$cond <- factor(df$cond, levels = c("mCh", "MLN", "E4", "E7", "E8"))

# extract channel 3 clathrin, channel 7 or 5 is TACC3
clat <- df[df$channel == 3,]
tacc <- df[df$channel == 7 | df$channel == 5,]

# quant microtubule/cytoplasm in each file
# start by smmarising mean of microtubule and cytoplasm
clat_df <- df %>%
  group_by(file, cond, measurement) %>%
  summarise(avg = mean(Mean))
tacc3_df <- tacc %>%
  group_by(file, cond, measurement) %>%
  summarise(avg = mean(Mean))
# for each file we now have two rows one for microtubule and one for cytoplasm
# pivot wider the data so that we have one row per file
clat_df <- clat_df %>%
  pivot_wider(names_from = measurement, values_from = avg)
tacc3_df <- tacc3_df %>%
  pivot_wider(names_from = measurement, values_from = avg)
# calculate the ratio of microtubule/cytoplasm
clat_df$rat <- clat_df$microtubule / clat_df$cytoplasm
tacc3_df$rat <- tacc3_df$microtubule / tacc3_df$cytoplasm

# merge the two data frames
quant_df <- merge(clat_df, tacc3_df, by = c("file", "cond"))
# I now have cytoplasm.x microtubule.x and rat.x for clathrin and cytoplasm.y microtubule.y and rat.y for TACC3
# Pivot longer the data so that I have one row per file and cond
quant_df <- quant_df %>%
  pivot_longer(cols = c("rat.x", "rat.y"), names_to = "measurement", values_to = "rat")
# rename rat.x and rat.y to clathrin and TACC3
quant_df$measurement <- factor(quant_df$measurement,
levels = c("rat.x", "rat.y"), labels = c("Clathrin", "TACC3"))
# add column that has experiment 1
quant_df$exp <- "1"
clat_df <- quant_df[quant_df$measurement == "Clathrin",]
tacc3_df <- quant_df[quant_df$measurement == "TACC3",]


base <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", col = "grey")

p1 <- superplot(clat_df, "rat", "cond", "exp",
                 ylab = "CLTA spindle recruitment",
                 xlab = "",
                 bars = "mean_sd",
                 size = c(0.8,1.5),
                 fsize = 9,
                gg = base) + lims(y = c(-5,NA))

p2 <- superplot(tacc3_df, "rat", "cond", "exp",
                ylab = "TACC3 spindle recruitment",
                xlab = "",
                bars = "mean_sd",
                size = c(0.8,1.5),
                fsize = 9,
                gg = base) + lims(y = c(-5,NA))

q <- p1 / p2
q

ggsave("Output/Plots/quant.pdf", q, width = 48, height = 80, units = "mm")

clat_anova <- aov(rat ~ cond, data = clat_df)
summary(clat_anova)
TukeyHSD(clat_anova)
# filter for TACC3 and do anova

tacc3_anova <- aov(rat ~ cond, data = tacc3_df)
summary(tacc3_anova)
TukeyHSD(tacc3_anova)
