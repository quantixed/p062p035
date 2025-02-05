library(ggplot2)
library(cowplot)
library(patchwork)

## Functions ----

pie_maker <- function(x1, cols) {
  df <- data.frame(
    pvalues = x1,
    pcolours = as.factor(seq_along(cols))
  )
  cs <- cumsum(x1)
  df$ymin <- c(0, cs[seq_along(cs) - 1])
  df$ymax <- cs
  
  p <- ggplot() +
    geom_rect(data = df, aes(fill = pcolours,
                             ymin = ymin, ymax = ymax, xmin = 0, xmax = 3)) +
    scale_fill_manual(values = cols) +
    xlim(c(0, 3)) +
    theme_void() + # switch to minimal theme if you need to check values
    theme(aspect.ratio = 1) +
    coord_polar(theta = "y") +
    theme(legend.position = "none")
  
  return(p)
}

## Script ----

df <- read.csv("Data/quantifall.csv")
# convert to long format, foci is column headers become variable names
df_long <- reshape2::melt(df, id.vars = c("foci","class"))
# remove E4.ctrl
df_long <- df_long[df_long$variable != "E4.ctrl",]
# variable names are like this mCh.ctrl, make two columns containing the first part and the second part
df_long$prot <- gsub("\\..*", "", df_long$variable)
df_long$treat <- gsub(".*\\.", "", df_long$variable)
# levels are mCh, E7 and E8
df_long$prot <- factor(df_long$prot, levels = c("mCh", "E7", "E8"))
# levels are normal, fragmented and spread
df_long$class <- factor(df_long$class, levels = c("fragmented", "normal", "spread"))
# change name of treat so that ctrl = control and noc = nocodazole
df_long$treat <- factor(df_long$treat, levels = c("ctrl", "noc"), labels = c("Control", "Nocodazole"))

# make a column where foci of 2 is "1" and anything else is "2"
df_long$col <- ifelse(df_long$foci == 2, "1", "2")
# this is not needed

ggplot(df_long, aes(x = foci, y = value, fill = class)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D", "#F8766d7f")) +
  lims(y = c(0,100)) +
  labs(x = "PCNT foci", y = "Percentage of cells") +
  facet_grid(treat~prot) +
  theme_cowplot(9) +
  theme(legend.position = "none")
ggsave("Output/Plots/noc.pdf", width = 80, height = 60, units = "mm")

ggplot(df_long, aes(x = foci, y = value, fill = class)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D", "#F8766d")) +
  lims(y = c(0,100)) +
  labs(x = "PCNT foci", y = "Percentage of cells") +
  facet_grid(treat~prot) +
  theme_cowplot(9) +
  theme(legend.position = "none")
ggsave("Output/Plots/noc_rev.pdf", width = 80, height = 60, units = "mm")

# make pie charts
df_pie <- df_long[df_long$foci == 2,]
my_cols <- c("#F8766D", "#F8766d7f")
# use lapply to make pie charts for all the other treatments
pie_list <- lapply(unique(df_pie$variable), function(x) {
  pie_maker(df_pie$value[df_pie$variable == x], my_cols)
})
# use patchwork to make a single plot of all pies
design <- "ACE
BDF"
q <- wrap_plots(pie_list, design = design)
ggsave("Output/Plots/pie.pdf", q, width = 40, height = 30, units = "mm")