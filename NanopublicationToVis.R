# Many thanks to this amazing tutorial: https://kateto.net/network-visualization
# Load required libraries
library(xml2)
library(igraph)
library(plotly)
library(XML)
library(dplyr)
library(stringr)
library(tidyr)
library(visNetwork)

# Download and read the RDF/XML file
url <- "http://np.knowledgepixels.com/RAzquSkwsTAZm61nReG6MOjXEXUx8fNVfdWnAzyn6sOhU.xml"  # Replace with your actual URL
name <- "RAzquSkwsT"
xml_data <- read_xml(url)

# Function to get a human-readable label
get_label <- function(uri) {
    # There's no labels in nanopubs so we just process the URI here. 
    #Basically this string checks if it has a nice label (indicated by #)
    #If not it returns the tail of the URL which tends to be the ontology URI
    label_tails <- str_extract(uri, "/([^/]+)$") %>% str_remove("^/")
    label_neat <- str_extract(label_tails, "#([^#]+)$") %>% str_remove("^#")
    label <- coalesce(label_neat, label_tails)
    return(label)
}

# Extract nodes and edges
# The XML namespace needs to be referenced in the function:
# Find all graph elements
graphs <- xml_find_all(xml_data, ".//d1:graph", xml_ns(xml_data))

# Function to process a single graph into dataframe
process_graph <- function(graph) {
  graph_uri <- xml_text(xml_find_first(graph, "./d1:uri"))
  triples <- xml_find_all(graph, ".//d1:triple")
  
  triples %>%
    lapply(function(triple) {
      uris <- xml_find_all(triple, ".//d1:uri") %>% xml_text()
      data.frame(
        graph = graph_uri,
        subject = uris[1],
        predicate = uris[2],
        object = uris[3],
        stringsAsFactors = FALSE
      )
    }) %>%
    bind_rows()
}

# Process all graphs and combine results
df <- graphs %>%
  lapply(process_graph) %>%
  bind_rows()

# Print the resulting dataframe for debugging
print(df)

#Here we're going to strip the messy URI from graph
#This regex just pulls the end of the URI which is usually the label
df <- df %>%
  mutate(graph_labels = get_label(df$graph)) %>%
  mutate(object_labels = get_label(df$object)) %>%
  mutate(predicate_labels = get_label(df$predicate)) %>%
  mutate(subject_labels = get_label(df$subject))

#todo - split up nodes including graph label info
# - create edges table
# - stylize graph



#We need to reorder the df to be subject & object, then predicate.
#We also need to create a separate vertex metadata df for labels
#This is because igraph processes the first two columns as edgelist
edges <- df[, c("subject", "object", "predicate", "predicate_labels")]
#We drop any edges with NA in them.
edges <- drop_na(edges, c("subject", "object"))

#We then need to pull out all unique vertices from subject and object and their
#associated metadata

# Create two dataframes with unique values from column1 and column2
temp1 <- df %>% 
  select(subject, graph_labels, subject_labels) %>% 
  distinct(subject, .keep_all = TRUE)

temp2 <- df %>% 
  select(object, graph_labels, object_labels) %>% 
  distinct(object, .keep_all = TRUE)

# Combine the two dataframes
merged_vertices <- bind_rows(
  temp1 %>% rename(uri = subject),
  temp2 %>% rename(uri = object)
  ) %>%
  distinct(uri, .keep_all = TRUE)

merged_vertices <- drop_na(merged_vertices, "uri")
# We need to merge the labels into one label column. 
merged_vertices <- merged_vertices %>% unite("label", c("subject_labels", "object_labels"), na.rm = TRUE, remove = FALSE)

#Let's change each graph into a colour so we can split things up.
merged_vertices <- merged_vertices %>% mutate(colour = recode(graph_labels, Head = "blue", Assertion = "green", Provenance = "orange", Pubinfo = "grey"))

clean_vertices <- merged_vertices[,c("uri", "label", "graph_labels")]
clean_vertices <- rename(clean_vertices, clean_label = label)

#NOW we add our edges and vertices and make an igraph object
graph <- graph_from_data_frame(edges, directed = TRUE, vertices = clean_vertices)

#Let's convert for visNetwork which is a nicer visualization
g_vis <- toVisNetworkData(graph)

vis.nodes <- g_vis$nodes
vis.edges <- g_vis$edges

vis.nodes$shape <- "dot"
vis.nodes$shadow <- TRUE # Nodes will drop shadow
vis.nodes$title  <- vis.nodes$uri # Text on click
vis.nodes$label <- vis.nodes$clean_label
vis.nodes$borderWidth <- 2 # Node border width
vis.nodes$color <- vis.nodes$colour
vis.nodes$group <- vis.nodes$graph_labels

vis.edges$title <- vis.edges$predicate_labels
vis.edges$label <- vis.edges$predicate_labels

#TODO: Make the below code turn nodes into links when clicked

visNetwork(vis.nodes, vis.edges) %>% 
  visGroups(groupname = "Head", color = "grey") %>%
  visGroups(groupname = "Assertion", color = "green") %>%
  visGroups(groupname = "Pubinfo", color = "purple") %>%
  visGroups(groupname = "Provenance", color = "orange") %>%
  visLegend()