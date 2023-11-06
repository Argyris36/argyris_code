


# Load the required libraries
library(igraph)
library(ggplot2)

# Define nodes and edges
nodes <- c("Child Psychiatry", "Medicine", "Sociology", "Philosophy", "Epidemiology", 
           "Anatomy", "Physiol", "Biochem", "E1", "E2", "E3", "F1", "F2", "F3", "G1", 
           "G2", "G3", "H1", "H2", "I1", "I2", "J1", "J2", "K1", "K2", "L1", "L2", "M1", "M2", "N1", "N2", "O1", "O2", "P1", "P2", 
           "R1", "R2", "S1", "S2", "T1", "T2")
edges <- c(
  "Child Psychiatry-Medicine", "Child Psychiatry-Sociology", "Child Psychiatry-Philosophy", "Child Psychiatry-Epidemiology",
  "Medicine-Anatomy", "Medicine-Physiol", "Medicine-Biochem",
  "Sociology-E1", "Sociology-E2", "Sociology-E3",
  "Philosophy-F1", "Philosophy-F2", "Philosophy-F3",
  "Epidemiology-G1", "Epidemiology-G2", "Epidemiology-G3",
  "Anatomy-H1", "Anatomy-H2", "Physiol-I1", "Physiol-I2", "Biochem-J1", "Biochem-J2",
  "E1-K1", "E1-K2", "E2-L1", "E2-L2", "E3-M1", "E3-M2",
  "F1-N1", "F1-N2", "F2-O1", "F2-O2", "F3-P1", "F3-P2", 
  "G1-R1", "G1-R2", "G2-S1", "G2-S2", "G3-T1", "G3-T2"
)

# Create a graph
graph <- graph_from_data_frame(data.frame(from = sub("-.*", "", edges), to = sub(".*-", "", edges)))

# Plot the graph using ggplot2
ggraph(graph, layout = "tree") +
  geom_edge_link() +
  geom_node_point(color = "skyblue", size = 5) +
  geom_node_text(aes(label = name), vjust = -0.6) +
  theme_void() +
  theme(legend.position = "none")


