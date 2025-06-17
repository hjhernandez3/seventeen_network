library(dplyr) 
library(igraph)
library(ggraph)
library(tidygraph)
library(readxl)
library(ggplot2)
library(tidyr)
library(stringr)
library(visNetwork)
library(htmlwidgets)


dat_disco <- read_xlsx("data/SVT_Discography.xlsx")
vocal_Team <- c("Jeonghan", "Seungkwan", "DK", "Joshua", "Woozi")
performance_Team <- c("Jun", "Hoshi", "The8", "Dino")
hiphop_Team <- c("S.Coups", "Wonwoo", "Mingyu", "Vernon")
bss <- c("Seungkwan", "DK", "Hoshi")
hxw <- c("Hoshi", "Woozi")
maknae <- c("Seungkwan", "Vernon", "Dino")
seventeen <- c(vocal_Team, performance_Team, hiphop_Team)
hyung_95z <- c("S.Coups", "Jeonghan", "Joshua")
hyung_96z <- c("Jun", "Hoshi", "Wonwoo", "Woozi")
maknae_97z <- c("The8", "DK", "Mingyu")



dat_disco <- dat_disco %>%
  mutate(Feat = ifelse(Artist == "Seventeen",
                       ifelse(is.na(Feat) | Feat == "", "Seventeen", paste(Feat, "Seventeen", sep = ", ")),
                       Feat)) %>%
  mutate(Feat = ifelse(Artist == "Vocal Team",
                       ifelse(is.na(Feat) | Feat == "", "Vocal Team", paste(Feat, "Vocal Team", sep = ", ")),
                       Feat)) %>%
  mutate(Feat = ifelse(Artist == "HipHop Team",
                       ifelse(is.na(Feat) | Feat == "", "HipHop Team", paste(Feat, "HipHop Team", sep = ", ")),
                       Feat)) %>%
    mutate(Feat = ifelse(Artist == "Performance Team",
                         ifelse(is.na(Feat) | Feat == "", "Performance Team", paste(Feat, "Performance Team", sep = ", ")),
                         Feat)) %>%
    mutate(Feat = ifelse(Artist == "Maknae Line",
                         ifelse(is.na(Feat) | Feat == "", "Maknae Line", paste(Feat, "Maknae Line", sep = ", ")),
                         Feat)) %>%
    mutate(Feat = ifelse(Artist == "BSS",
                         ifelse(is.na(Feat) | Feat == "", "BSS", paste(Feat, "BSS", sep = ", ")),
                         Feat))%>%
    mutate(Feat = ifelse(Artist == "HxW",
                         ifelse(is.na(Feat) | Feat == "", "HxW", paste(Feat, "HxW", sep = ", ")),
                         Feat)) %>%
  mutate(Feat = ifelse(str_detect(Artist, ","),
                       ifelse(is.na(Feat) | Feat == "",
                              Artist,
                              paste(Feat, Artist, sep = ", ")),
                       Feat))





disco_pre <- dat_disco %>%
  separate(`Date Released`, into = c("Year", "Month", "Day"), sep = "-") %>%
  select(-c("Month", "Day")) %>%
  mutate(Artist = case_when(
    Artist == "HipHop Team" ~ paste(hiphop_Team, collapse = ","),
    Artist == "Performance Team"~ paste(performance_Team, collapse = ","),
    Artist == "Vocal Team"~ paste(vocal_Team, collapse = ","),
    Artist == "Seventeen"~ paste(seventeen, collapse = ","),
    Artist == "BSS"~ paste(bss, collapse = ","),
    Artist == "HxW"~ paste(hxw, collapse = ","),
    Artist == "Maknae Line"~ paste(maknae, collapse = ","),
    TRUE ~ Artist
  )) %>%
  mutate(Feat = case_when(
    Feat == "HipHop Team" ~ paste(hiphop_Team, collapse = ","),
    Feat == "Performance Team"~ paste(performance_Team, collapse = ","),
    Feat == "Vocal Team"~ paste(vocal_Team, collapse = ","),
    Feat == "Seventeen"~ paste(seventeen, collapse = ","),
    Feat == "BSS"~ paste(bss, collapse = ","),
    Feat == "HxW"~ paste(hxw, collapse = ","),
    Feat == "Maknae Line"~ paste(maknae, collapse = ","),
    TRUE ~ Feat
  )) %>%
  separate_rows(Artist, sep = ",\\s*") %>%
  separate_rows(Feat, sep = ",\\s*") 

disco_pre1 <- disco_pre %>%
  mutate(Feat = case_when(
    Feat == "HipHop Team" ~ paste(hiphop_Team, collapse = ","),
    Feat == "Performance Team"~ paste(performance_Team, collapse = ","),
    Feat == "Vocal Team"~ paste(vocal_Team, collapse = ","),
    Feat == "Seventeen"~ paste(seventeen, collapse = ","),
    Feat == "BSS"~ paste(bss, collapse = ","),
    Feat == "HxW"~ paste(hxw, collapse = ","),
    Feat == "Maknae Line"~ paste(maknae, collapse = ","),
    TRUE ~ Feat
  )) %>%
  separate_rows(Feat, sep = ",\\s*") 

#deleting self edges
disco_pre2 <- disco_pre1 %>%
  filter(Artist != Feat)

# NEED TO COUNT ALL SONGS!!!! doesnt matter if they dont have a Feat or not
unique_songs_per_artist <- disco_pre2 %>%
  select(Song, Artist, Feat) %>%
  pivot_longer(cols = c(Artist, Feat), names_to = "role", values_to = "person") %>%
  distinct(person, Song) %>%
  count(person, name = "num_unique_songs") %>%
  arrange(desc(num_unique_songs))

member_songs <- disco_pre2 %>%
  select(Song, Artist, Group) %>%
  filter(Artist %in% seventeen) %>%
  distinct(Song, Artist, Group)  # avoid overcounting

# Count group vs. non-group songs
member_counts <- member_songs %>%
  mutate(Participation = ifelse(Group == "SEVENTEEN", "Group", "Non-Group")) %>%
  count(Artist, Participation) %>%
  tidyr::pivot_wider(names_from = Participation, values_from = n, values_fill = 0) %>%
  arrange(desc(Group))

member_counts <- member_counts %>%
  rename(`Non-Group` = `NA`) %>%
  mutate(Total_songs = Group + `Non-Group`)

member_long <- member_counts %>%
  tidyr::pivot_longer(cols = c("Group", "Non-Group"), names_to = "Type", values_to = "Count")


comp_songs <- ggplot(member_long, aes(x = reorder(Artist, Count, sum), y = Count, fill = Type)) +
  geom_col() +
  
  # Non-Group count label (inside gray bar)
  geom_text(
    data = member_long %>% filter(Type == "Non-Group"),
    aes(label = Count),
    color = "black",
    size = 3,
    hjust = 1.1
  ) +
  
  # Total song count label (outside the full bar)
  geom_text(
    data = member_long %>% filter(Type == "Group"),
    aes(x = Artist, y = Total_songs + 1, label = Count),
    inherit.aes = FALSE,
    size = 3.5
  ) +
  
  coord_flip() +
  scale_fill_manual(values = c("Group" = "purple", "Non-Group" = "gray80")) +
  labs(
    x = NULL,
    y = "Number of Songs",
    title = "Songs within SEVENTEEN vs Solo/Other",
    fill = "Song Type",
    caption = "Graph by Hortencia Josefina Hernandez ·\nData: SEVENTEEN Discography · Updated: June 16, 2025"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(face = "bold", size = 10),
    axis.text.x = element_text(face = "italic", size = 12),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 1, face = "italic", size = 10),
    plot.title = element_text(face = "bold", size = 19),
    legend.position = "bottom"
  )


ggsave("images/Group_Non_Group.png", plot = comp_songs, width = 10, height = 8, dpi = 300)

edge_list <- disco_pre2 %>%
  select(2, 3) %>%
  filter(!is.na(Artist) & !is.na(Feat))

disco_graph <- graph_from_data_frame(edge_list, directed = FALSE)

V(disco_graph)$size <- member_counts$Total_songs[match(V(disco_graph)$name, member_counts$Artist)] 

p1 <- ggraph(disco_graph, layout = "fr") +
  geom_edge_link(aes(alpha = ..index..), color = "gray70") +
  geom_node_point(aes(size = size), color = "#CFA9FF") +
  #scale_color_manual(
  #  values = c("Current Artist" = "#CFA9FF", "Previous Artist" = "#87CEFA"),
  #  name = "Artist Status"
  #) +   
  geom_node_text(aes(label = name), size = 3.5, fontface = "bold",repel = FALSE, color = "darkblue") +
  theme_graph() +
  guides(size = "none", edge_alpha = "none") +
  scale_size_continuous(range = c(5, 20)) +
  #scale_color_discrete(name = "Music Genre") + 
  labs(
    title = "SEVENTEEN Network",
    subtitle = "",
    caption = "Graph by Hortencia Josefina Hernandez·\nData: SEVENTEEN (plus solos) discography \nUpdate: 16 June, 2025"
  ) + theme(
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold"),
    legend.key.size = unit(1.2, "cm")  
  )+
  guides(
    #color = guide_legend(override.aes = list(size = 6)), 
    size = "none", 
    edge_alpha = "none"
  )

#p1 # takes too long
#ggsave("images/svt_sna_draft5.png", plot = p1, width = 10, height = 8, dpi = 300)



# Making an interactive plot!

all_artists <- unique(c(disco_pre2$Artist, disco_pre2$Feat)) %>%
  strsplit(",\\s*") %>% unlist() %>% unique()

get_groups <- function(name) {
  groups <- c()
  if (name %in% vocal_Team) groups <- c(groups, "Vocal Team")
  if (name %in% performance_Team) groups <- c(groups, "Performance Team")
  if (name %in% hiphop_Team) groups <- c(groups, "Hip-Hop Team")
  if (name %in% bss) groups <- c(groups, "BSS")
  if (name %in% hxw) groups <- c(groups, "H×W")
  if (name %in% maknae) groups <- c(groups, "Maknae Line")
  if(name %in% hyung_95z) groups <- c(groups, "95z Line")
  if(name %in% hyung_96z) groups <- c(groups, "96z Line")
  if(name %in% maknae_97z) groups <- c(groups, "97z Line")
  if (name %in% seventeen) groups <- c(groups, "SEVENTEEN")
  if (length(groups) == 0) return("Other")
  return(paste(groups, collapse = " / "))
}

get_primary_group <- function(name) {
  if (name %in% vocal_Team) return("Vocal Team")
  if (name %in% performance_Team) return("Performance Team")
  if (name %in% hiphop_Team) return("Hip-Hop Team")
  return("Other")
}

# Create node table
nodes <- data.frame(id = all_artists, label = all_artists, stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(
    group = get_primary_group(id),  # for color
    title = get_groups(id)          # for tooltip
  ) %>%
  ungroup()

nodes$SEVENTEEN_Member <- ifelse(nodes$id %in% seventeen, "SEVENTEEN", "Other")

edges <- disco_pre2 %>%
  filter(!is.na(Artist) & !is.na(Feat)) %>%
  separate_rows(Artist, Feat, sep = ",\\s*") %>%
  select(from = Artist, to = Feat) %>%
  count(from, to, name = "weight") 

# So edges between SEVENTEEN members ONLY are colored
edges <- edges %>%
  mutate(
    color = ifelse(from %in% seventeen & to %in% seventeen, "purple", "gray")
  )


# FULL visNetwork graph
graph <- visNetwork(nodes, edges) %>%
  visGroups(groupname = "Vocal Team", color = "pink") %>%
  visGroups(groupname = "Performance Team", color = "orange") %>%
  visGroups(groupname = "Hip-Hop Team", color = "lightblue") %>%
  visGroups(groupname = "Other", color = "lightgray") %>%
  visNodes(
    font = list(size = 50, valign = "top"),
    color = list(
      highlight = list(background = "inherit", border = "inherit"),
      hover = list(background = "inherit", border = "inherit")
    )
  ) %>%
  visOptions(
    selectedBy = list(
      variable = "group",
      values = unique(nodes$group[nodes$group != "Other"])
    ),
    highlightNearest = list(
      enabled = TRUE,
      degree = 0,
      hover = FALSE,
      algorithm = "all"  # ensures both dropdowns trigger highlights
    ),
    nodesIdSelection = list(
      enabled = TRUE,
      values = nodes$id[nodes$id %in% seventeen]
    )
  ) %>%
  visEdges(
    smooth = TRUE,
    scaling = list(min = 1, max = 5),
    width = "weight"
  ) %>%
  visLayout(randomSeed = 42) %>%
  visPhysics(
    solver = "repulsion",
    repulsion = list(
      nodeDistance = 400,  # increase to push nodes farther apart
      springLength = 200,
      springConstant = 0.01,
      damping = 0.1
    ),
    stabilization = list(enabled = FALSE, iterations = 500)
  )%>%
  visInteraction(navigationButtons = TRUE) %>%
  visLegend()



saveWidget(graph, file = "docs/index.html", selfcontained = TRUE)

# SOME NETWORK PROPERTIES


g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

btw <- betweenness(g)
deg <- degree(g)
degMax <- which.max(deg)
# View top members
sort(btw, decreasing = TRUE)



# Counting how many people are connected to each seventeen member
edges_tagged <- edges %>%
  left_join(nodes %>% select(id, SEVENTEEN_Member), by = c("from" = "id")) %>%
  rename(from_group = SEVENTEEN_Member) %>%
  left_join(nodes %>% select(id, SEVENTEEN_Member), by = c("to" = "id")) %>%
  rename(to_group = SEVENTEEN_Member)

svt_to_non_edges <- edges_tagged %>%
  filter(
    (from_group == "SEVENTEEN" & to_group != "SEVENTEEN") |
      (from_group != "SEVENTEEN" & to_group == "SEVENTEEN")
  )

svt_connections <- svt_to_non_edges %>%
  mutate(
    svt_member = ifelse(from_group == "SEVENTEEN", from, to),
    non_svt_partner = ifelse(from_group == "SEVENTEEN", to, from)
  ) 

svt_external_counts <- svt_connections %>%
  count(svt_member, name = "non_svt_connections") %>%
  arrange(desc(non_svt_connections))
