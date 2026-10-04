rm(list=ls())
library(ggplot2)
library(lubridate)
library(gridExtra)
library(grid)
library(igraph)
library(ggraph)
library(tidygraph)
library(dplyr)


e_token <- "SNC"
### Select period of data
lim_dates <- c(ymd("2020-12-08"), ymd("2026-01-31"))
load(paste0("Data/return_", e_token, ".RData"))
################################################################################
# Read returns 
### TOTAL
AVG_total <- (read.csv(paste0("julia/Output/avg_tab_total_", e_token, ".csv"), 
                       header = F, sep=",", dec="."))[-1,]
colnames(AVG_total) <- colnames(ret)[-1]
rownames(AVG_total) <- colnames(ret)[-1]
df_total <- as.data.frame(as.table(as.matrix(AVG_total)))
df_total$Freq <- as.numeric(as.character(df_total$Freq))

p_heatmap_total <- ggplot(df_total, aes(Var2, Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL)

print(p_heatmap_total)

ggsave(paste0("Output/Frequencies/heatmap_total_", e_token, ".png"), 
       p_heatmap_total, 
       width = 3000, 
       height = 3000, 
       dpi = 500, 
       units = "px", 
       limitsize = FALSE,
       bg = "transparent")

################################################################################
############################ NETWORK FROM HEATMAP ##############################
################################################################################

### TOTAL NETWORK
AVG_total <- (read.csv(paste0("julia/Output/avg_tab_total_", e_token, ".csv"), 
                       header = F, sep=",", dec="."))[-1,]
colnames(AVG_total) <- colnames(ret)[-1]
rownames(AVG_total) <- colnames(ret)[-1]

# Convert ALL columns to numeric
AVG_total <- as.data.frame(lapply(AVG_total, function(x) as.numeric(as.character(x))))
rownames(AVG_total) <- colnames(ret)[-1]

# Convert to numeric matrix
AVG_total_matrix <- as.matrix(AVG_total)

# Get all node names
all_nodes <- colnames(ret)[-1]

# Set a threshold for connections (adjust to control density)
threshold <- 0.05  # Adjust based on your data

# Convert matrix to edge list
edge_list_total <- data.frame(
  from = rep(rownames(AVG_total_matrix), each = ncol(AVG_total_matrix)),
  to = rep(colnames(AVG_total_matrix), times = nrow(AVG_total_matrix)),
  weight = as.vector(AVG_total_matrix),
  stringsAsFactors = FALSE
)

# Ensure weight is numeric
edge_list_total$weight <- as.numeric(edge_list_total$weight)

# Filter edges based on threshold and remove self-loops
edge_list_total <- edge_list_total %>%
  filter(weight > threshold, from != to)

# Create nodes data frame to ensure ALL nodes are included
nodes_total <- data.frame(name = all_nodes, stringsAsFactors = FALSE)

# Create igraph object with all nodes
network_total <- graph_from_data_frame(edge_list_total, directed = FALSE, vertices = nodes_total)

# Calculate degree centrality for node sizing
V(network_total)$degree <- degree(network_total)

# Circular network plot - NO TITLE, TRANSPARENT BACKGROUND, PROPER MARGINS
p_network_total <- ggraph(network_total, layout = "circle") +
  geom_edge_link(aes(alpha = weight), color = "gray70", width = 0.5) +
  geom_node_point(aes(size = degree), color = "steelblue", alpha = 0.7) +
  geom_node_text(aes(label = name), size = 4, fontface = "bold", repel = FALSE) +
  scale_edge_alpha(range = c(0.1, 0.5)) +
  scale_size_continuous(range = c(8, 25)) +
  coord_fixed(clip = "off") +  # CRITICAL: Prevents clipping and maintains aspect ratio
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(40, 40, 40, 40)  # Add margins to prevent cutoff
  )

print(p_network_total)

# Save the plot
ggsave(paste0("Output/Frequencies/network_total_circular_", e_token, ".png"), 
       p_network_total, 
       width = 5000, 
       height = 3000, 
       dpi = 300, 
       units = "px", 
       limitsize = FALSE,
       bg = "transparent")



### SHORT
AVG_short <- (read.csv(paste0("julia/Output/avg_tab_short_", e_token, ".csv"), header = F, sep=",", dec="."))[-1,]
colnames(AVG_short) <- colnames(ret)[-1]
rownames(AVG_short) <- colnames(ret)[-1]
df <- as.data.frame(as.table(as.matrix(AVG_short)))
df$Freq <- as.numeric(as.character(df$Freq))

ggplot(df, aes(Var2, Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = NULL, fill = "Value")

### MEDIUM
AVG_medium <- (read.csv(paste0("julia/Output/avg_tab_medium_", e_token, ".csv"), header = F, sep=",", dec="."))[-1,]
colnames(AVG_medium) <- colnames(ret)[-1]
rownames(AVG_medium) <- colnames(ret)[-1]
df <- as.data.frame(as.table(as.matrix(AVG_medium)))
df$Freq <- as.numeric(as.character(df$Freq))

ggplot(df, aes(Var2, Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = NULL, fill = "Value")

### LONG
AVG_long <- (read.csv(paste0("julia/Output/avg_tab_long_", e_token, ".csv"), header = F, sep=",", dec="."))[-1,]
colnames(AVG_long) <- colnames(ret)[-1]
rownames(AVG_long) <- colnames(ret)[-1]
df <- as.data.frame(as.table(as.matrix(AVG_long)))
df$Freq <- as.numeric(as.character(df$Freq))

ggplot(df, aes(Var2, Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = NULL, fill = "Value")

