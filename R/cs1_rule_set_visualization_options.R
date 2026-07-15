# Explore visualization options for final CS1 AIRItaxa rule sets
source("R/prepare_session.R")
source("R/plot_helper.R")

cs1_rule_set_paths <- c(
  "Improvement" = "rule-sets/mosj_airi_by_improvement.csv",
  "Complexity" = "rule-sets/mosj_airi_by_complexity.csv",
  "Mutual information" = "rule-sets/mosj_airi_by_mutual_information.csv"
)

optional_package <- function(package){
  if(!requireNamespace(package, quietly = TRUE)){
    message("Skipping optional plot: install package '", package, "' to enable it.")
    return(FALSE)
  }

  TRUE
}

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

## Option 1 - rule quality space
cs1_rule_quality_space_plot <- cs1_airitaxa_rules %>%
  ggplot(aes(confidence, lift)) +
  geom_point(aes(size = count,
                 colour = Classification,
                 shape = Metric),
             alpha = 0.8) +
  facet_wrap(~Metric) +
  scale_size_continuous(range = c(2, 6)) +
  scale_colour_manual(values = c("Abundant" = "#0072B2",
                                 "Rare" = "#D55E00",
                                 "Undetermined" = "#009E73")) +
  airi_plot_theme() +
  labs(x = "Confidence",
       y = "Lift",
       colour = "Classification",
       shape = "AIRItaxa metric",
       size = "Rule count")

save_airi_plot(cs1_rule_quality_space_plot,
               filename = "cs1_rule_set_quality_space.png",
               width = 7,
               height = 4.5)

if(interactive()){
  cs1_rule_quality_space_plot
}

## Option 2 - rule-by-condition heatmap
cs1_rule_order <- cs1_airitaxa_rules %>%
  arrange(Metric, desc(lift)) %>%
  transmute(rule_label, rule_order = row_number())

cs1_condition_order <- cs1_airitaxa_rule_items %>%
  filter(!is_taxon) %>%
  mutate(condition = paste(variable, value, sep = "="),
         rule_label = factor(rule_label, levels = cs1_rule_order$rule_label)) %>%
  left_join(cs1_rule_order, by = "rule_label") %>%
  group_by(condition) %>%
  summarise(first_rule = min(rule_order), .groups = "drop") %>%
  arrange(first_rule) %>%
  pull(condition)

cs1_condition_items <- cs1_airitaxa_rule_items %>%
  filter(!is_taxon) %>%
  mutate(condition = paste(variable, value, sep = "="),
         rule_label = factor(rule_label, levels = cs1_rule_order$rule_label),
         condition = factor(condition, levels = cs1_condition_order))

cs1_rule_condition_heatmap_plot <- cs1_condition_items %>%
  ggplot(aes(condition, rule_label, fill = Classification)) +
  geom_tile(colour = "white", linewidth = 0.25) +
  facet_grid(Metric ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = c("Abundant" = "#0072B2",
                               "Rare" = "#D55E00",
                               "Undetermined" = "#009E73")) +
  airi_plot_theme(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.y = element_blank(),
        panel.grid = element_blank()) +
  labs(x = "Antecedent item",
       fill = "Classification")

save_airi_plot(cs1_rule_condition_heatmap_plot,
               filename = "cs1_rule_set_condition_heatmap.png",
               width = 8,
               height = 7)

if(interactive()){
  cs1_rule_condition_heatmap_plot
}

## Option 3 - bipartite taxon-condition network
if(optional_package("igraph") && optional_package("ggraph")){
  cs1_network_edges <- cs1_airitaxa_rule_items %>%
    filter(!is_taxon) %>%
    left_join(cs1_airitaxa_rule_taxa, by = "rule_id") %>%
    mutate(condition = paste(variable, value, sep = "=")) %>%
    count(taxon_label, condition, Classification, name = "edge_weight")

  cs1_network_nodes <- bind_rows(
    cs1_network_edges %>%
      distinct(name = taxon_label) %>%
      mutate(node_type = "Taxon"),
    cs1_network_edges %>%
      distinct(name = condition) %>%
      mutate(node_type = "Condition")
  )

  cs1_network_graph <- igraph::graph_from_data_frame(
    d = cs1_network_edges %>% select(from = taxon_label,
                                     to = condition,
                                     edge_weight,
                                     Classification),
    vertices = cs1_network_nodes,
    directed = FALSE
  )

  cs1_rule_network_plot <- ggraph::ggraph(cs1_network_graph, layout = "fr") +
    ggraph::geom_edge_link(aes(width = edge_weight,
                               colour = Classification),
                           alpha = 0.45) +
    ggraph::geom_node_point(aes(shape = node_type),
                            size = 3,
                            colour = "grey20") +
    ggraph::geom_node_text(aes(label = name),
                           repel = TRUE,
                           size = 2.7) +
    ggraph::scale_edge_colour_manual(values = c("Abundant" = "#0072B2",
                                                "Rare" = "#D55E00",
                                                "Undetermined" = "#009E73")) +
    ggraph::scale_edge_width(range = c(0.3, 1.4)) +
    airi_plot_theme(base_size = 10) +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank()) +
    labs(edge_colour = "Classification",
         edge_width = "Shared rules",
         shape = "Node type")

  save_airi_plot(cs1_rule_network_plot,
                 filename = "cs1_rule_set_taxon_condition_network.png",
                 width = 8,
                 height = 6)

  if(interactive()){
    cs1_rule_network_plot
  }
}

## Option 4 - alluvial summary from metric to classification
if(optional_package("ggalluvial")){
  cs1_rule_alluvial_plot <- cs1_airitaxa_rules %>%
    count(Metric, Classification, name = "n") %>%
    ggplot(aes(axis1 = Metric, axis2 = Classification, y = n)) +
    ggalluvial::geom_alluvium(aes(fill = Classification), width = 1/12, alpha = 0.75) +
    ggalluvial::geom_stratum(width = 1/12, fill = "grey90", colour = "grey45") +
    ggplot2::geom_text(stat = "stratum",
                       aes(label = after_stat(stratum)),
                       size = 3) +
    scale_x_discrete(limits = c("AIRItaxa metric", "Classification"),
                     expand = c(0.08, 0.08)) +
    scale_fill_manual(values = c("Abundant" = "#0072B2",
                                 "Rare" = "#D55E00",
                                 "Undetermined" = "#009E73")) +
    airi_plot_theme() +
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    labs(fill = "Classification")

  save_airi_plot(cs1_rule_alluvial_plot,
                 filename = "cs1_rule_set_metric_classification_alluvial.png",
                 width = 6.5,
                 height = 4.2)

  if(interactive()){
    cs1_rule_alluvial_plot
  }
}
