# Explore visualization options for final CS1 AIRItaxa rule sets
source("R/prepare_session.R")
source("R/plot_helper.R")

cs1_rule_set_paths <- c(
  "Improvement" = "rule-sets/mosj_airi_by_improvement.csv",
  "Complexity" = "rule-sets/mosj_airi_by_complexity.csv",
  "Mutual information" = "rule-sets/mosj_airi_by_mutual_information.csv"
)

read_airitaxa_rule_set <- function(path, metric){
  rule_set <- read.csv(path, check.names = FALSE)
  rule_set <- rule_set[names(rule_set) != ""]

  rule_set %>%
    mutate(Metric = metric)
}

short_taxon_label <- function(taxon){
  map_chr(str_split(taxon, "_"), function(parts){
    parts <- parts[nzchar(parts)]

    if(length(parts) == 0){
      return(NA_character_)
    }

    tail(parts, 1)
  })
}

load_cs1_airitaxa_rule_sets <- function(){
  if(all(file.exists(cs1_rule_set_paths))){
    return(imap_dfr(cs1_rule_set_paths, read_airitaxa_rule_set))
  }

  source("R/cs1_airi_steps.R")

  bind_rows(
    dependent_rules_non_redundant %>%
      mutate(Metric = "Improvement"),
    non_redundant_by_complexity %>%
      mutate(Metric = "Complexity"),
    non_redundant_by_mutualInfo %>%
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
           is_taxon = variable == "taxon") %>%
    select(-LHS_clean)
}

cs1_airitaxa_rules <- load_cs1_airitaxa_rule_sets() %>%
  mutate(Metric = factor(Metric,
                         levels = c("Improvement", "Complexity", "Mutual information")),
         Classification = str_remove_all(RHS, "\\{|\\}|Classification="),
         rule_label = paste0("R", row_number()))

cs1_airitaxa_rule_items <- parse_rule_items(cs1_airitaxa_rules)

cs1_airitaxa_rule_taxa <- cs1_airitaxa_rule_items %>%
  filter(is_taxon) %>%
  transmute(rule_id,
            taxon = value,
            taxon_label = short_taxon_label(value))

cs1_airitaxa_rules <- cs1_airitaxa_rules %>%
  mutate(rule_id = row_number()) %>%
  left_join(cs1_airitaxa_rule_taxa, by = "rule_id")

dir.create("figures", showWarnings = FALSE, recursive = TRUE)

## Bipartite taxon-condition network
cs1_network_edges <- cs1_airitaxa_rule_items %>%
  filter(!is_taxon) %>%
  left_join(cs1_airitaxa_rule_taxa, by = "rule_id") %>%
  mutate(condition = paste(variable, value, sep = "=")) %>%
  count(Metric, taxon_label, condition, Classification, name = "edge_weight")

cs1_network_nodes <- bind_rows(
  cs1_network_edges %>%
    distinct(name = taxon_label) %>%
    mutate(node_type = "Taxon"),
  cs1_network_edges %>%
    distinct(name = condition) %>%
    mutate(node_type = "Condition")
)

cs1_network_graph <- graph_from_data_frame(
  d = cs1_network_edges %>% select(from = taxon_label,
                                   to = condition,
                                   Metric,
                                   edge_weight,
                                   Classification),
  vertices = cs1_network_nodes,
  directed = FALSE
)

cs1_rule_network_plot <- ggraph(cs1_network_graph, layout = "fr") +
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

save_airi_plot(cs1_rule_network_plot,
               filename = "cs1_rule_set_taxon_condition_network.png",
               width = 10,
               height = 5.8)

