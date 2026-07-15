# Explore final CS2 AIRItaxa rule sets with a taxon-condition network
source("R/prepare_session.R")
source("R/plot_helper.R")

cs2_rule_set_paths <- c(
  "Improvement" = "rule-sets/emose_airi_by_improvement.csv",
  "Complexity" = "rule-sets/emose_airi_by_complexity.csv",
  "Mutual information" = "rule-sets/emose_airi_by_mutual_information.csv"
)

read_airitaxa_rule_set <- function(path, metric){
  rule_set <- read.csv(path, check.names = FALSE)
  rule_set <- rule_set[names(rule_set) != ""]

  rule_set %>%
    mutate(Metric = metric)
}

short_taxa_label <- function(taxa){
  map_chr(str_split(taxa, " "), function(parts){
    parts <- parts[nzchar(parts)]
    parts <- parts[parts != "NA"]

    if(length(parts) == 0){
      return(NA_character_)
    }

    tail(parts, 1)
  })
}

load_cs2_airitaxa_rule_sets <- function(){
  if(all(file.exists(cs2_rule_set_paths))){
    return(imap_dfr(cs2_rule_set_paths, read_airitaxa_rule_set))
  }

  source("R/cs2_airi_emose.R")

  bind_rows(
    emose_dependent_rules_non_redundant %>%
      mutate(Metric = "Improvement"),
    emose_non_redundant_by_complexity %>%
      mutate(Metric = "Complexity"),
    emose_non_redundant_by_mutualInfo %>%
      mutate(Metric = "Mutual information")
  )
}

parse_rule_items <- function(rule_sets){
  rule_sets %>%
    mutate(rule_id = row_number(),
           Classification = str_remove_all(RHS, "\\{|\\}|Classification="),
           rule_label = paste0("R", rule_id),
           LHS_clean = str_remove_all(LHS, "^\\{|\\}$")) %>%
    separate_rows(LHS_clean, sep = ",") %>%
    mutate(item = str_squish(LHS_clean),
           variable = str_extract(item, "^[^=]+"),
           value = str_remove(item, "^[^=]+="),
           is_taxa = variable == "taxa") %>%
    select(-LHS_clean)
}

cs2_airitaxa_rules <- load_cs2_airitaxa_rule_sets() %>%
  mutate(Metric = factor(Metric,
                         levels = c("Improvement", "Complexity", "Mutual information")),
         Classification = str_remove_all(RHS, "\\{|\\}|Classification="),
         rule_label = paste0("R", row_number()))

cs2_airitaxa_rule_items <- parse_rule_items(cs2_airitaxa_rules)

cs2_airitaxa_rule_taxa <- cs2_airitaxa_rule_items %>%
  filter(is_taxa) %>%
  transmute(rule_id,
            taxa = value,
            taxa_label = short_taxa_label(value))

cs2_airitaxa_rules <- cs2_airitaxa_rules %>%
  mutate(rule_id = row_number()) %>%
  left_join(cs2_airitaxa_rule_taxa, by = "rule_id")

dir.create("figures", showWarnings = FALSE, recursive = TRUE)

## Bipartite taxon-condition network
cs2_network_edges <- cs2_airitaxa_rule_items %>%
  filter(!is_taxa) %>%
  left_join(cs2_airitaxa_rule_taxa, by = "rule_id") %>%
  mutate(condition = paste(variable, value, sep = "=")) %>%
  count(Metric, taxa_label, condition, Classification, name = "edge_weight")

cs2_network_nodes <- bind_rows(
  cs2_network_edges %>%
    distinct(name = taxa_label) %>%
    mutate(node_type = "Taxon"),
  cs2_network_edges %>%
    distinct(name = condition) %>%
    mutate(node_type = "Condition")
)

cs2_network_graph <- graph_from_data_frame(
  d = cs2_network_edges %>% select(from = taxa_label,
                                   to = condition,
                                   Metric,
                                   edge_weight,
                                   Classification),
  vertices = cs2_network_nodes,
  directed = FALSE
)

cs2_rule_network_plot <- ggraph(cs2_network_graph, layout = "fr") +
  geom_edge_link(aes(colour = Classification),
                 linewidth = 0.45,
                 alpha = 0.5) +
  geom_node_point(aes(shape = node_type),
                  size = 3,
                  colour = "grey20") +
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 size = 2.7) +
  scale_edge_colour_manual(values = c("Abundant" = "#0072B2",
                                      "Rare" = "#D55E00",
                                      "Undetermined" = "#009E73")) +
  facet_edges(~Metric) +
  airi_plot_theme(base_size = 10) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()) +
  labs(edge_colour = "Classification",
       shape = "Node type")

save_airi_plot(cs2_rule_network_plot,
               filename = "cs2_rule_set_taxon_condition_network.png",
               width = 10,
               height = 5.8)
