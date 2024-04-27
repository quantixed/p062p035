# Making Sankey diagrams using the Combined_NA_removed.csv dataset

## ------ To do --------------

# Comment out links that have a value of 0

library(networkD3)
library(dplyr)
# To save html files as static images
library(htmlwidgets)
#library(colorspace)

mCh_nodes = data.frame("name" = 
                     c("Before NEB 2", # Node 0
                       "Before NEB >2", # Node 1
                       "NEB 2", # Node 2 
                       "NEB >2", # Node 3
                       "Meta 2", # Node 4
                       "Meta >2", #Node 5
                       "Ana 2", # Node 6
                       "Ana >2" # Node 7
                       ), "blank_name" = c("", "", "", "", "", "", "", ""))

# Add a 'group' column to each node. Use split or normal to colour code them
mCh_nodes$group <- as.factor(c("normal", "split", "normal", "split", "normal", "split", "normal", "split"))

mCh_links = as.data.frame(matrix(c(
  # Column 1 = start node
  # Column 2 = finish node
  # Column 3 = number of cells
  0, 2, 70, # Num of cells that go from 2 before NEB to 2 for NEB
  1, 3, 5, # Num of cells that go from >2 before NEB to >2 for NEB
  # 1, 2, 0, # Num of cells that go from >2 before NEB to 2 for NEB
  2, 4, 69, # Num cells that go from 2 NEB to 2 Meta
  2, 5, 1, # Num of cells that go from 2 NEB to >2 Meta
  3, 5, 2, # Num of cells that go from >2 NEB to >2 Meta
  3, 4, 3, # Num of cells that go from >2 NEB to 2 Meta
  4, 6, 70, # Num cells that go from 2 Meta to 2 Ana
  4, 7, 2, # Num of cells that go from 2 Meta to >2 Ana
  5, 7, 3), # Num of cells that go from >2 Meta to >2 Ana
  byrow = TRUE, ncol = 3))
names(mCh_links) = c("source", "target", "value")

# Add a group column to each link. Make them all the same group to colour grey
mCh_links$group <- as.factor(c("unique_group"))

# --------------- E4 -----------------

E4_nodes = data.frame("name" = 
                         c("Before NEB 2", # Node 0
                           "Before NEB >2", # Node 1
                           "NEB 2", # Node 2 
                           "NEB >2", # Node 3
                           "Meta 2", # Node 4
                           "Meta >2", #Node 5
                           "Ana 2", # Node 6
                           "Ana >2" # Node 7
                         ), "blank_name" = c("", "", "", "", "", "", "", ""))

# Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
E4_nodes$group <- as.factor(c("normal", "split", "normal", "split", "normal", "split", "normal", "split"))

E4_links = as.data.frame(matrix(c(
  # Column 1 = start node
  # Column 2 = finish node
  # Column 3 = number of cells
  0, 2, 77, # Num of cells that go from 2 before NEB to 2 for NEB
  1, 3, 4, # Num of cells that go from >2 before NEB to >2 for NEB
  #1, 2, 0, # Num of cells that go from >2 before NEB to 2 for NEB
  2, 4, 77, # Num cells that go from 2 NEB to 2 Meta
  #2, 5, 0, # Num of cells that go from 2 NEB to >2 Meta
  3, 5, 4, # Num of cells that go from >2 NEB to >2 Meta
  #3, 4, 0, # Num of cells that go from >2 NEB to 2 Meta
  4, 6, 72, # Num cells that go from 2 Meta to 2 Ana
  4, 7, 5, # Num of cells that go from 2 Meta to >2 Ana
  5, 7, 4), # Num of cells that go from >2 Meta to >2 Ana
  byrow = TRUE, ncol = 3))
names(E4_links) = c("source", "target", "value")

# Add a group column to each link
E4_links$group <- as.factor(c("unique_group"))

# --------------- E7 -----------------

E7_nodes = data.frame("name" = 
                         c("Before NEB 2", # Node 0
                           "Before NEB >2", # Node 1
                           "NEB 2", # Node 2 
                           "NEB >2", # Node 3
                           "Meta 2", # Node 4
                           "Meta >2", #Node 5
                           "Ana 2", # Node 6
                           "Ana >2" # Node 7
                         ), "blank_name" = c("", "", "", "", "", "", "", ""))

# Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
E7_nodes$group <- as.factor(c("normal", "split", "normal", "split", "normal", "split", "normal", "split"))

E7_links = as.data.frame(matrix(c(
  # Column 1 = start node
  # Column 2 = finish node
  # Column 3 = number of cells
  0, 2, 77, # Num of cells that go from 2 before NEB to 2 for NEB
  1, 3, 5, # Num of cells that go from >2 before NEB to >2 for NEB
  #1, 2, 0, # Num of cells that go from >2 before NEB to 2 for NEB
  2, 4, 76, # Num cells that go from 2 NEB to 2 Meta
  2, 5, 1, # Num of cells that go from 2 NEB to >2 Meta
  3, 5, 5, # Num of cells that go from >2 NEB to >2 Meta
  #3, 4, 0, # Num of cells that go from >2 NEB to 2 Meta
  4, 6, 65, # Num cells that go from 2 Meta to 2 Ana
  4, 7, 11, # Num of cells that go from 2 Meta to >2 Ana
  5, 7, 6), # Num of cells that go from >2 Meta to >2 Ana
  byrow = TRUE, ncol = 3))
names(E7_links) = c("source", "target", "value")

# Add a group column to each link
E7_links$group <- as.factor(c("unique_group"))


# --------------- E8 -----------------
E8_nodes = data.frame("name" = 
                         c("Before NEB 2", # Node 0
                           "Before NEB >2", # Node 1
                           "NEB 2", # Node 2 
                           "NEB >2", # Node 3
                           "Meta 2", # Node 4
                           "Meta >2", #Node 5
                           "Ana 2", # Node 6
                           "Ana >2" # Node 7
                         ), "blank_name" = c("", "", "", "", "", "", "", ""))

# Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
E8_nodes$group <- as.factor(c("normal", "split", "normal", "split", "normal", "split", "normal", "split"))

E8_links = as.data.frame(matrix(c(
  # Column 1 = start node
  # Column 2 = finish node
  # Column 3 = number of cells
  0, 2, 69, # Num of cells that go from 2 before NEB to 2 for NEB
  1, 3, 3, # Num of cells that go from >2 before NEB to >2 for NEB
  #1, 2, 0, # Num of cells that go from >2 before NEB to 2 for NEB
  2, 4, 69, # Num cells that go from 2 NEB to 2 Meta
  #2, 5, 0, # Num of cells that go from 2 NEB to >2 Meta
  3, 5, 3, # Num of cells that go from >2 NEB to >2 Meta
  #3, 4, 0, # Num of cells that go from >2 NEB to 2 Meta
  4, 6, 58, # Num cells that go from 2 Meta to 2 Ana
  4, 7, 11, # Num of cells that go from 2 Meta to >2 Ana
  5, 7, 3), # Num of cells that go from >2 Meta to >2 Ana
  byrow = TRUE, ncol = 3))
names(E8_links) = c("source", "target", "value")

# Add a group column to each link
E8_links$group <- as.factor(c("unique_group"))

# --------------- Make the plots -----------------

# Give a color for each group:
my_colour <- 'd3.scaleOrdinal() .domain(["split", "normal", "unique_group"]) .range(["#00BFC4", "#F8766D", "#808285"])'

Make_Sankey <- function(my_links, my_nodes){
  
 The_Sankey <- sankeyNetwork(Links = my_links, Nodes = my_nodes,
                Source = "source", Target = "target",
                Value = "value", NodeID = "blank_name",
                colourScale = my_colour,
                fontSize= 12, nodeWidth = 30, fontFamily = "Helvetica", LinkGroup="group", NodeGroup="group",
                height = 240, width = 400)
  
 return(The_Sankey)
}

mCh_Sankey <- Make_Sankey(mCh_links, mCh_nodes)
mCh_Sankey

E4_Sankey <- Make_Sankey(E4_links, E4_nodes)
E4_Sankey

E7_Sankey <- Make_Sankey(E7_links, E7_nodes)
E7_Sankey

E8_Sankey <- Make_Sankey(E8_links, E8_nodes)
E8_Sankey

## ----------------- Save the diagrams -------------

# save the widget as html
saveNetwork(mCh_Sankey, file=paste0( getwd(), "/Output/mCh_sankey.html"))
saveNetwork(E4_Sankey, file=paste0( getwd(), "/Output/E4_sankey.html"))
saveNetwork(E7_Sankey, file=paste0( getwd(), "/Output/E7_sankey.html"))
saveNetwork(E8_Sankey, file=paste0( getwd(), "/Output/E8_sankey.html"))

# open the Sankey in a web browser and save to pdf via print
