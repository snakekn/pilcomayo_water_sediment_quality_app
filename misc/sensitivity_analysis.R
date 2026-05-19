# sensitivity analysis
##### helper functions #####
#' get rankings (non-parametric) from raw HQ values
prepare_rankings = function(merged_df, wider = FALSE, c = "station") {
  ranked = merged_df |>
    group_by(method) |>
    mutate(rank = dense_rank(desc(HQ))) |> # to give high HQs value #1
    ungroup() 
  
  if(wider) {
    ranked = ranked |>
      pivot_wider(names_from = method, values_from = rank, id_cols=!!c)
  }
  
  return(ranked) # ranks for each method at each categorical value
}

# print pretty corplots
#' corplot_pretty(cor_station_long, title = "Method Sensitivity Analysis: Station Rankings", x = "Method", y="Method"
corplot_pretty = function(ranking_wide, title = "", subtitle = "", 
                          x="Method",y="Method",
                          m1="Temporal Aggregation", m2="HQ Aggregation", return_cor=FALSE) {
  
  if(anyNA(ranking_wide)) {
    cor_df = cor(ranking_wide[,-1],
                 method="spearman", 
                 use="pairwise.complete.obs")
  } else {
    cor_df = cor(ranking_wide[,-1],
                 method="spearman")
                 #use="pairwise.complete.obs") 
  }
  cor_df_long = reshape2::melt(cor_df) %>%
    filter(as.character(Var1) < as.character(Var2))  # Lower triangle only
  
  if(return_cor) return(cor_df_long)
  
  ggplot(cor_df_long, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(
      aes(label = round(value,2)),#ifelse(value < 0.7, round(value, 2), "")),  # Only show ρ<0.7
      size = 2.5, 
      color = "black"
    ) +
    scale_fill_gradient2(low = "red", mid = "white", high = "lightblue",
                         midpoint = 0, limit = c(-1, 1),
                         name = "Spearman (ρ)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          axis.text.y = element_text(size = 8),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray30")) +
    labs(title = title,
         subtitle = sprintf("Method Format: [%s]_[%s]",m1, m2),
         x = x, 
         y = y) +
    coord_fixed()  # Square tiles
}

#' get p-vals for confirmation from correlations
#' @example 
view_cor_pval = function(rank_df) {
  # prepare ranking data
  corr_matrix <- cor(rank_df[,-1], use = "complete.obs")
  p_matrix <- ggcorrplot::cor_pmat(rank_df[,-1])
  
  ggcorrplot::ggcorrplot(corr_matrix, p.mat = p_matrix, 
             sig.level = 0.05, insig = "pch")
}

# comprae 
compare_rho = function(df_corrs, model, grouping = FALSE, boxplots=FALSE, horizontal_graph=FALSE, force_scale=TRUE) {
  if(grouping) {
    df = df_corrs |>
      mutate(
        in_group = grepl(model, Var1) | grepl(model, Var2),
        group = ifelse(in_group, model, "other")
      )
  } else {
    df = df_corrs |>
      mutate(group = if_else(Var1 == model, model, "Other")) 
  }
  
  if(boxplots) {
    n_labels <- df %>% 
      group_by(group) %>% 
      summarise(n = n()) %>%
      mutate(label = paste0(group, " (n=", n, ")"))
    label_names <- setNames(n_labels$label, n_labels$group)
    
    
    g = ggplot(df, aes(x = group, y = value, fill = group)) +
      geom_boxplot(outlier.shape = 21, outlier.fill = "gray30", outlier.size = 2, alpha = 0.7) +
      scale_fill_manual(values = setNames(c("#56B4E9", "#E69F00"), levels(df$type))) +
      scale_x_discrete(labels = label_names) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7, vjust = 1),
        panel.grid.major.x = element_blank()
      ) +
      labs(
        x = "",
        y = "Spearman (ρ)",
        title = "Correlation between Method Groups"
      )
    
    if(force_scale) {
      g = g +
        ylim(-1, 1)
    }
    
    if(horizontal_graph) {
      g = g +
        coord_flip()+
        stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), 
                     hjust = 1.5, vjust=-0.5, 
                     size = 3.5, fontface = "bold")
      
    } else {
      g = g +
        stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), 
                     vjust=-0.5, 
                     size = 3.5, fontface = "bold")
    }
    
    return(g)
  }
  
  df = df |>
    rename(rho = value) |>
    group_by(group) |>
    summarize(n=n(),
              min = min(rho, na.rm=TRUE),
              median = median(rho, na.rm=TRUE),
              mean = mean(rho, na.rm=TRUE),
              max = max(rho, na.rm=TRUE))
  return(df)
}

#' Run analysis when we only want to consider the priority list
list_priorities = function(rank_df, m1 = "Var1", m2 = "Var2") {
  # get column names so we can dynamically use them later (easier for user to not deal with)
  col_name = names(rank_df)[1]
  method_names = names(rank_df)[-1]
  
  # split methods so we can facet on them
  top_rankings_df = rank_df |>
    pivot_longer(cols = -1, 
                 names_to=c(m1, m2),
                 names_sep = "_",
                 values_to="rank") |>
    group_by(!!sym(col_name))|>
    filter(any(rank<=5, na.rm=TRUE)) |>
    ungroup()|>
    mutate(
      method = paste0(!!sym(m1), "_",!!sym(m2)),
      rank_group = case_when(
        rank == 1 ~ "1",
        rank == 2 ~ "2",
        rank == 3 ~ "3",
        rank == 4 ~ "4",
        rank == 5 ~ "5",
        rank > 5 ~ "Not Ranked in Top 5",    # *** Anything above 5 ***
        is.na(rank) ~ "Not Ranked in Top 5",  # *** Also handle NAs ***
        TRUE ~ "Not Ranked in Top 5"
      ),
      rank_group = factor(rank_group, levels = c("1", "2", "3", "4", "5", "Not Ranked in Top 5"))
    )|>
    select(!!sym(m1), !!sym(m2), !!col_name, rank, rank_group, method)
  
  order <- top_rankings_df %>%
    filter(!is.na(rank)) %>%
    group_by(!!sym(col_name)) %>%
    summarise(min_rank = min(rank, na.rm=TRUE),
              num_min = sum(min_rank==rank)) %>%
    arrange(desc(min_rank), num_min) %>%
    pull(!!sym(col_name))
  
  top_rankings_df = top_rankings_df |>
    mutate(!!sym(col_name) := factor(!!sym(col_name), levels = order))
  
  ggplot(top_rankings_df, aes(x = !!sym(m1), y = !!sym(col_name), fill = rank_group)) +
    geom_tile(color = "white", linewidth = 1.2) +
    geom_text(
      aes(label = ifelse(!is.na(rank), rank, "")),
      size = 5,
      #fontface = "bold",
      color = case_when(
        top_rankings_df$rank <= 2 ~ "white",
        top_rankings_df$rank >= 4 ~ "gray30",
        TRUE ~ "gray30"
      )
    ) +
    facet_wrap(vars(!!sym(m2)), nrow = 1) +
    scale_fill_manual(
      values = c(
        "1" = "#d73027",
        "2" = "#fc8d59",
        "3" = "#fee090",
        "4" = "#ffffbf",
        "5" = "#d0e5f5",
        "Not Ranked in Top 5" = "gray85"
      ),
      name = "Rank",
      drop = FALSE
    ) +
    # scale_x_discrete(labels = c(unique(!!sym(m2))))+
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 11, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      panel.grid = element_blank(),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 15, face = "bold", margin = margin(b = 15)),
      plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40", margin = margin(b = 10)),
      plot.margin = margin(15, 15, 15, 15)
    ) +
    labs(
      x = m1,
      y = "",
      title = sprintf("Top 5 %s Rankings Across Model Methods", str_to_title(col_name)),
      subtitle = sprintf("Faceted by %s shown at the top, and %s in each facet.", m2, m1)
    )
}

##### sensitivity analysis #####
#' plan:
#' - stations: temporal, param aggregation
#' - parameters: temporal, spatial

##### parameter: across temporal & spatial #####
### prepare dataset
temporal_aggregation = c("recent", "mean", "max", "weighted")
spatial_aggregation = c("mean", "median", "max", "pct95")
parameter_methods = crossing(
  temporal_aggregation = temporal_aggregation,
  spatial_aggregation = spatial_aggregation,
)

# Note: Removing various parameters I know won't make sense to show
removed_parameters = c("Oxygen Saturation", "COD", "BOD", "alkalinity", "Average Velocity", "Decimal latitude", "Decimal longitude", "Flow", "hardness", "Oxygen", "Partial Pressure", "Temperature", "Resistivity")
params_df = bol_water_scored |> filter(!parameter %in% removed_parameters)

# run the model using all methods.  
sensitivity_df_param <- parameter_methods %>% pmap_dfr(~plot_top_hq_params(
  params_df,
  media_type = "water", 
  fraction = "all", 
  station = "all",
  temporal_aggregation = ..1,
  spatial_aggregation = ..2,
  decay_per_day = NULL,
  return_data = TRUE,
  all_params = TRUE),
  .id="method_id") |> 
  mutate(
    method = paste(
      parameter_methods$temporal_aggregation[as.numeric(method_id)],
      parameter_methods$spatial_aggregation[as.numeric(method_id)],
      sep = "_"
    )
  ) |>
  select(parameter, hq, method) |>
  rename(HQ = hq)

# get rankings for each parameter across methods
rankings_param = prepare_rankings(sensitivity_df_param, wider=TRUE, c = "parameter")

### calculate differences & stats

# correlation between the methods
param_cor = cor(rankings_param|>select(-parameter), method = "spearman") # rho values (correlation)

### plot for ease of use in the paper
# Prepare data
cor_long <- reshape2::melt(param_cor) %>%
  filter(as.character(Var1) < as.character(Var2))  # Lower triangle only

param_corplot = corplot_pretty(cor_long, 
                               title = "Method Sensitivity Analysis: Parameter Rankings",
                               x = "Method", y="Method") 
  
param_corplot

### see the difference in correlation between those using median and those not
median_methods <- grep("median", rownames(param_cor), value = TRUE)
cor_df <- cor_long %>%
  mutate(
    type = ifelse(
      Var1 %in% median_methods & Var1 %in% median_methods,
      "Uses Median HQ",
      "Non-Median HQ"
    )
  ) |>
  rename(rho = value)


cor_df %>%
  group_by(type) %>%
  summarise(max_rho = max(rho), median_rho = median(rho), mean_rho = mean(rho), min_rho = min(rho), .groups="drop")

library(patchwork)
param_corplot = param_corplot + theme(plot.margin = margin(5, 20, 5, 5))  # top, right, bottom, left
param_median_boxplot = param_median_boxplot +
  labs(title = "HQ Methods") +  # Remove individual title
  theme(plot.margin = margin(5, 5, 5, 10))

param_plots = param_corplot + param_median_boxplot + plot_layout(widths = c(5, 1))

param_plots

### get the top 10 parameters we'd recommend based on each method

# run the model using all methods.  
sensitivity_df_param_10 <- parameter_methods %>% pmap_dfr(~plot_top_hq_params(
  params_df,
  media_type = "water", 
  fraction = "all", 
  station = "all",
  temporal_aggregation = ..1,
  spatial_aggregation = ..2,
  decay_per_day = NULL,
  return_data = TRUE,
  all_params = FALSE),
  .id="method_id") |> 
  mutate(
    method = paste(
      parameter_methods$temporal_aggregation[as.numeric(method_id)],
      parameter_methods$spatial_aggregation[as.numeric(method_id)],
      sep = "_"
    )
  ) |>
  select(parameter, hq, method) |>
  rename(HQ = hq)

# get rankings for each parameter across methods
rankings_param = prepare_rankings(sensitivity_df_param_10, wider=TRUE, c = "parameter")


top_rankings_param_10 = rankings_param |>
  pivot_longer(cols=-parameter,values_to = "rank", names_to = "method") |>
  group_by(method) |>
  slice_min(rank, n = 10) |>
  select(method, parameter, rank) |>
  pivot_wider(names_from = method, values_from = rank)

# correlation between the methods
param_cor_10 = cor(top_rankings_param_10|>select(-parameter), 
                   method = "spearman",
                   use="pairwise.complete.obs") 

### plot for ease of use in the paper
# Prepare data
cor_long_10 <- reshape2::melt(param_cor_10) %>%
  filter(as.character(Var1) < as.character(Var2))  # Lower triangle only

param_corplot_10 = corplot_pretty(cor_long_10,
                              title = "Top 10 Parameters", x="Method",y="Method")
param_corplot_10

# note: has na's
top_params_long = top_rankings_param_10 |>
  pivot_longer(cols=-parameter, names_to = "method", values_to = "rank") |>
  mutate(rank_group = case_when(
    rank == 1 ~ "1",
    rank == 2 ~ "2",
    rank == 3 ~ "3",
    rank == 4 ~ "4",
    rank == 5 ~ "5",
    rank <= 10 ~ "6-10",
    is.na(rank) ~ "Not Ranked",
    TRUE ~ "Not Ranked"
    ),
    rank_group = factor(rank_group, levels = c("1", "2", "3", "4", "5", "6-10", "Not Ranked")),
    #method = factor(top_params_long$method, levels = colnames(rank_matrix)[-1][method_names_ordered]),
    temporal = case_when(
      grepl("max_", method) ~ "Max",
      grepl("mean_", method) ~ "Mean",
      grepl("recent_", method) ~ "Recent",
      grepl("weighted_", method) ~ "Weighted"
    ),
    spatial = case_when(
      grepl("_max$", method) ~ "Max",
      grepl("_mean$", method) ~ "Mean",
      grepl("_median$", method) ~ "Median",
      grepl("_pct95$", method) ~ "95th %ile"
    )
  )

# order for the plot. Can also use to describe which are commonly the worst
param_order <- top_params_long %>%
  group_by(parameter) %>%
  summarise(
    n = n(),
    min_rank = min(rank, na.rm=TRUE),
    num_min = sum(rank==min_rank, na.rm=TRUE)
  ) %>%
  arrange(min_rank, desc(num_min)) |>
  pull(parameter)

top_params_ordered = top_params_long |>
  mutate(parameter = factor(parameter, levels = rev(param_order)))

# Facet by temporal method
ggplot(top_params_ordered, aes(x = spatial, y = parameter, fill = rank_group)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(
    aes(label = ifelse(rank <= 5, rank, "")), 
    size = 4,
    fontface = "bold",
    color = case_when(
      top_params_long$rank <= 2 ~ "white",
      top_params_long$rank >= 4 ~ "gray30",
      TRUE ~ "black"
    )
  ) +
  facet_wrap(~temporal, nrow = 1) +
  scale_fill_manual(
    values = c(
      "1" = "#d73027", "2" = "#fc8d59", "3" = "#fee090",
      "4" = "#ffffbf", "5" = "#d0e5f5", "6-10" = "gray40",
      "Not Ranked" = "grey80"
    ),
    name = "Rank"
  ) +
  theme_void() +  # Start from blank
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, 
                               size = 10, face = "bold", margin = margin(t = 5)),
    axis.text.y = element_text(size = 11, face = "bold", 
                               margin = margin(r = 8), hjust = 1),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 12)),
    strip.text = element_text(face = "bold", size = 13, margin = margin(b = 10)),
    panel.spacing = unit(2, "lines"),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(15, 15, 15, 15),
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold", margin = margin(b = 20))
  ) +
  labs(
    x = "HQ Aggregation (Temporal Aggregation Above)",
    y = "",
    title = "Parameter Priority Rankings Across Method Combinations",
  )



##### station all #####
temporal_aggregation = c("recent", "mean", "max", "weighted")
spatial_aggregation = c("mean", "median", "max", "pct95")
station_methods = crossing(
  temporal_aggregation = temporal_aggregation,
  spatial_aggregation = spatial_aggregation,
)

# Note: Removing various parameters I know won't make sense to show
removed_parameters = c("Oxygen Saturation", "COD", "BOD")
bol_data_filtered = bol_water_scored |> filter(!parameter %in% removed_parameters)


sensitivity_df_station_all <- station_methods %>% pmap_dfr(~plot_top_hq_stations(
  data = bol_data_filtered, 
  media = "water", 
  param = "all", 
  param_aggregation = ..2, 
  temporal_aggregation = ..1,
  all_stations = FALSE,
  return_data_only=TRUE),
  .id="method_id") |>
  mutate(
    method = paste(
      station_methods$temporal_aggregation[as.numeric(method_id)],
      station_methods$spatial_aggregation[as.numeric(method_id)],
      sep = "_"
    )
  ) |>
  select(station, HQ, method)

# get rankings for each parameter across methods
rankings_station_all = prepare_rankings(sensitivity_df_station_all, wider=TRUE, c = "station")

### calculate differences & stats

# correlation between the methods
station_cor = cor(rankings_station_all|>select(-station), 
                  method = "spearman",
                  use="pairwise.complete.obs") # rho values (correlation)

### plot for ease of use in the paper
# Prepare data
cor_station_long <- reshape2::melt(station_cor) %>%
  filter(as.character(Var1) < as.character(Var2))  # Lower triangle only

station_corplot = corplot_pretty(cor_station_long, 
                               title = "Method Sensitivity Analysis: Station Rankings",
                               x = "Method", y="Method") 

station_corplot

# compare medians for stations
compare_rho(df_corrs = cor_station_long, model = "median", 
            grouping = TRUE, boxplots = TRUE, horizontal_graph = TRUE)



##### station: temporal ("recent", "mean", "average", "max", "weighted", "nemerow") #####
# second method i wrote. getting better with purrr! includes how the stations would've changed with each option

### prepare dataset
temporal_aggregation = c("mean", "max", "weighted", "recent")
parms = as.data.frame(temporal_aggregation)
colnames(parms) = "temporal_aggregation"

sensitivity_df_station_temporal <- parms %>% pmap_dfr(~plot_top_hq_stations(
                          data = bol_water_scored, 
                          media = "water", 
                          param = "all", 
                          param_aggregation = "pct95", 
                          temporal_aggregation = ..1,
                          all_stations = TRUE,
                          return_data_only=TRUE),
                          .id="method_id") |>
  mutate(method = temporal_aggregation[as.numeric(method_id)]) |>
  select(station, HQ, method)

rankings_station_temporal = prepare_rankings(sensitivity_df_station_temporal, wider=TRUE)
# View(rankings_station_temporal)

### calculate differences & stats

# correlation between the methods
cor(rankings_station_temporal|>select(-station), method = "spearman") # rho values (correlation)

rank_matrix <- as.matrix(rankings_station_temporal[, -1])  # another method here! has p-vals
rcorr_result <- Hmisc::rcorr(rankings_station_temporal, type = "spearman")
print(round(rcorr_result$r,2)) # check rho values. Vals between .7-.98 (.7=50% to 95%). All methods agree
print(rcorr_result$P) # check p-vals. All methods are related and values can be interpreted

### get the top 5 stations we'd recommend based on each method
top_rankings_station_temporal = rankings_station_temporal |>
  pivot_longer(cols=-station,values_to = "rank", names_to = "method") |>
  group_by(method) |>
  slice_min(rank, n = 5) |>
  select(method, station, rank) |>
  pivot_wider(names_from = method, values_from = rank)

### make the table look pretty :)
station_ranks_long <- top_rankings_station_temporal %>%
  pivot_longer(cols = -station, names_to = "method", values_to = "rank") %>%
  mutate(
    rank_group = case_when(
      rank == 1 ~ "1", rank == 2 ~ "2", rank == 3 ~ "3",
      rank == 4 ~ "4", rank == 5 ~ "5",
      TRUE ~ "Not Top 5"
    ),
    rank_group = factor(rank_group, levels = c("1", "2", "3", "4", "5", "Not Top 5")),
    method = factor(method, levels = c("max", "mean", "recent", "weighted"))
  )

# Order stations
station_order <- station_ranks_long %>%
  filter(!is.na(rank)) %>%
  group_by(station) %>%
  summarise(min_rank = min(rank)) %>%
  arrange(desc(min_rank)) %>%
  pull(station)

station_ranks_long$station <- factor(station_ranks_long$station, levels = station_order)

ggplot(station_ranks_long, aes(x = method, y = station, fill = rank_group)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(
    aes(label = ifelse(!is.na(rank), rank, "")),
    size = 5,
    #fontface = "bold",
    color = case_when(
      station_ranks_long$rank <= 2 ~ "white",
      station_ranks_long$rank >= 4 ~ "gray30",
      TRUE ~ "black"
    )
  ) +
  scale_fill_manual(
    values = c(
      "1" = "#d73027",
      "2" = "#fc8d59",
      "3" = "#fee090",
      "4" = "#ffffbf",
      "5" = "#d0e5f5",
      "Not Top 5" = "gray90"
    ),
    name = "Rank",
    drop = FALSE
  ) +
  scale_x_discrete(labels = c("max" = "Max", "mean" = "Mean", 
                              "recent" = "Recent", "weighted" = "Weighted")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid = element_blank(),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold", margin = margin(b = 15)),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40", margin = margin(b = 10)),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  labs(
    x = "Temporal Aggregation Method",
    y = "",
    title = "Top 5 Station Rankings Across Temporal Aggregation Methods",
    subtitle = "HQ aggregation held constant at 95% Percentile"
  )


##### station: param #####
param_aggregation = c("mean", "median", "max", "pct95")
parms = as.data.frame(param_aggregation)
colnames(parms) = "param_aggregation"

# run the model using all methods.  
sensitivity_station_param <- parms %>% pmap_dfr(~plot_top_hq_stations(
  bol_water_scored, 
  media = "water", 
  param = "all", 
  temporal_aggregation = "recent", 
  param_aggregation = ..1, 
  all_stations = TRUE,
  return_data_only=TRUE),
  .id="method_id") |>
  mutate(method = param_aggregation[as.numeric(method_id)])|>
  select(station, HQ, method)

# get our rankings
rankings_station_hq = prepare_rankings(sensitivity_station_param, wider=TRUE, c="station")

### view the correlation
rank_matrix <- as.matrix(rankings_station_hq[, -1])  # another method here! has p-vals
rcorr_result <- Hmisc::rcorr(rank_matrix, type = "spearman")
print(round(rcorr_result$r,2)) # check rho values. Vals between .7-.98 (.7=50% to 95%). All methods agree
print(rcorr_result$P) # check p-vals. All methods are related and values can be interpreted

### get the top 5 stations we'd recommend based on each method
top_rankings_station_hq = rankings_station_hq |>
  pivot_longer(cols=-station,values_to = "rank", names_to = "method") |>
  group_by(station)|>
  filter(any(rank<=5, na.rm=TRUE)) |>
  ungroup()|>
  mutate(
    rank_group = case_when(
      rank == 1 ~ "1",
      rank == 2 ~ "2",
      rank == 3 ~ "3",
      rank == 4 ~ "4",
      rank == 5 ~ "5",
      rank > 5 ~ "Not Ranked in Top 5",    # *** Anything above 5 ***
      is.na(rank) ~ "Not Ranked in Top 5",  # *** Also handle NAs ***
      TRUE ~ "Not Ranked in Top 5"
    ),
    rank_group = factor(rank_group, levels = c("1", "2", "3", "4", "5", "Not Ranked in Top 5"))
  )|>
  select(method, station, rank, rank_group)


### make the table look pretty :)
# Order stations
station_order <- top_rankings_station_hq %>%
  filter(!is.na(rank)) %>%
  group_by(station) %>%
  summarise(min_rank = min(rank, na.rm=TRUE),
            num_min = sum(min_rank==rank)) %>%
  arrange(desc(min_rank), num_min) %>%
  pull(station)

top_rankings_station_hq$station <- factor(top_rankings_station_hq$station, levels = station_order)

ggplot(top_rankings_station_hq, aes(x = method, y = station, fill = rank_group)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(
    aes(label = ifelse(!is.na(rank), rank, "")),
    size = 5,
    #fontface = "bold",
    color = case_when(
      top_rankings_station_hq$rank <= 2 ~ "white",
      top_rankings_station_hq$rank >= 4 ~ "gray30",
      TRUE ~ "gray30"
    )
  ) +
  scale_fill_manual(
    values = c(
      "1" = "#d73027",
      "2" = "#fc8d59",
      "3" = "#fee090",
      "4" = "#ffffbf",
      "5" = "#d0e5f5",
      "Not Ranked in Top 5" = "gray85"
    ),
    name = "Rank",
    drop = FALSE
  ) +
  scale_x_discrete(labels = c("max" = "Max", "mean" = "Mean", 
                              "median" = "Median", "pct95" = "95% Percentile")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid = element_blank(),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold", margin = margin(b = 15)),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40", margin = margin(b = 10)),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  labs(
    x = "HQ Aggregation Method",
    y = "",
    title = "Top 5 Station Rankings Across HQ Aggregation Methods",
    subtitle = "Temporal aggregation held constant at most recent values"
  )


##### Sensitivity on recency that uses most recent, 1-, 2-, or 3- yr window
recent_range = c(0:5)
spatial_aggregation = c("mean", "median", "max", "pct95")
recency_methods = crossing(
  recent_range = recent_range,
  spatial_aggregation = spatial_aggregation,
)

# run the model using all methods.  
sensitivity_param_recency <- recency_methods %>% pmap_dfr(~plot_top_hq_params(
  bol_water_scored, 
  media = "water", 
  temporal_aggregation = "recent", 
  spatial_aggregation = ..2,
  all_params = TRUE,
  recent_range = ..1,
  return_data=TRUE),
  .id="method_id") |>
  mutate(method = paste(
    recency_methods$recent_range[as.numeric(method_id)],
    recency_methods$spatial_aggregation[as.numeric(method_id)],
    sep = "_"
  ))|>
  rename(HQ=hq)|>
  select(parameter, HQ, method)

# get our rankings
rankings_param_recency = prepare_rankings(sensitivity_param_recency, wider=TRUE, c="parameter")

# get our corplot
param_recency_corplot = corplot_pretty(rankings_param_recency, title = "Correlation between Model Methods", subtitle = "", x="Method",y="Method",
                                       m1 = "Range of Years", m2="HQ Aggregation")
param_recency_corplot

### view the correlation
param_recency_cor_pval = view_cor_pval(rankings_param_recency)
param_recency_cor_pval

### get the top 5 stations we'd recommend based on each method
priority_params_recency = list_priorities(rankings_param_recency, m1="HQ Aggregation", m2="Range of Years Included")
priority_params_recency

### compare the medians to the non-medians
compare_rho(df_corrs = corplot_pretty(rankings_param_recency, return_cor=TRUE), 
            model = "median", grouping = TRUE, boxplots = TRUE, horizontal_graph = TRUE, force_scale = FALSE)

##### we do sediments now -- parameter #####
spatial_aggregation = c("mean", "median", "max", "pct95")
temporal_aggregation = c("recent", "mean", "max", "weighted")

sed_parms = crossing(
  spatial_aggregation = spatial_aggregation,
  temporal_aggregation = temporal_aggregation,
)

# run the model using all methods.  
sensitivity_sed_param <- sed_parms %>% pmap_dfr(~plot_top_hq_params(
  bol_sed_scored, 
  media = "sediment", 
  temporal_aggregation = ..2, 
  spatial_aggregation = ..1,
  all_params = TRUE,
  return_data=TRUE),
  .id="method_id") |>
  mutate(method = paste(
    sed_parms$spatial_aggregation[as.numeric(method_id)],
    sed_parms$temporal_aggregation[as.numeric(method_id)],
    sep = "_"
  ))|>
  rename(HQ=hq)|>
  select(parameter, HQ, method)

# get our rankings
rankings_param_sed = prepare_rankings(sensitivity_sed_param, wider=TRUE, c="parameter")

# get our corplot
corplot_param_sed = corplot_pretty(rankings_param_sed, title = "Correlation between Model Methods", subtitle = "", x="Method",y="Method",
                                       m1 = "HQ Aggregation", m2="Temporal Aggregation")
corplot_param_sed

### view the correlation
param_sed_cor_pval = view_cor_pval(rankings_param_sed)
param_sed_cor_pval

### get the top 5 stations we'd recommend based on each method
priority_params_sed = list_priorities(rankings_param_sed, m1="HQ Aggregation", m2="Temporal Aggregation")
priority_params_sed

### compare the medians to the non-medians
compare_rho(df_corrs = corplot_pretty(rankings_param_sed, return_cor=TRUE), 
            model = "median", grouping = TRUE, boxplots = TRUE, horizontal_graph = TRUE, force_scale = FALSE)

##### we do sediments now -- station #####
temporal_aggregation = c("recent", "mean", "max", "weighted")
param_aggregation = c("mean", "median", "max", "pct95")
station_methods = crossing(
  temporal_aggregation = temporal_aggregation,
  param_aggregation = param_aggregation,
)

sensitivity_station_sed_all_stations <- station_methods %>% pmap_dfr(~plot_top_hq_stations(
  data = bol_sed_scored, 
  media = "sediment", 
  param = "all", 
  param_aggregation = ..2, 
  temporal_aggregation = ..1,
  all_stations = TRUE,
  return_data_only=TRUE),
  .id="method_id") |>
  mutate(
    method = paste(
      station_methods$temporal_aggregation[as.numeric(method_id)],
      station_methods$param_aggregation[as.numeric(method_id)],
      sep = "_"
    )
  ) |>
  select(station, HQ, method)

# get our rankings
rankings_station_sed = prepare_rankings(sensitivity_station_sed, wider=TRUE, c="station")

# get our corplot
corplot_station_sed = corplot_pretty(rankings_station_sed, title = "Correlation between Model Methods", subtitle = "", x="Method",y="Method",
                                   m1 = "Temporal Aggregation", m2="HQ Aggregation")
corplot_station_sed_as # all stations
corplot_station_sed # just top 10 stations

### view the correlation
station_sed_cor_pval_as = view_cor_pval(rankings_station_sed_as)
station_sed_cor_pval_as

### get the top 5 stations we'd recommend based on each method
priority_station_sed = list_priorities(rankings_station_sed, m1="Temporal Aggregation", m2="HQ Aggregation")
priority_station_sed

### compare the medians to the non-medians
compare_rho(df_corrs = corplot_pretty(rankings_station_sed, return_cor=TRUE), 
            model = "recent", grouping = TRUE, boxplots = TRUE, horizontal_graph = TRUE, force_scale = FALSE)

##### we do sediments now -- recency #####
recent_range = c(0:5)
spatial_aggregation = c("mean", "median", "max", "pct95")
station_methods_recency = crossing(
  recent_range = recent_range,
  spatial_aggregation = spatial_aggregation,
)

sensitivity_param_sed_recency <- station_methods_recency %>% pmap_dfr(~plot_top_hq_params(
  data = bol_sed_scored, 
  media_type = "sediment", 
  spatial_aggregation = ..2, 
  temporal_aggregation = "recent",
  recent_range = ..1,
  all_params = TRUE,
  return_data=TRUE),
  .id="method_id") |>
  mutate(
    method = paste(
      station_methods_recency$recent_range[as.numeric(method_id)],
      station_methods_recency$spatial_aggregation[as.numeric(method_id)],
      sep = "_"
    )
  ) |>
  rename(HQ = hq) |>
  select(parameter, HQ, method)

# get our rankings
rankings_param_sed_r = prepare_rankings(sensitivity_param_sed_recency, wider=TRUE, c="parameter")

# get our corplot
corplot_param_sed_r = corplot_pretty(rankings_param_sed_r, title = "Correlation between Parameter Priorities for Sediment Data", subtitle = "", x="Method",y="Method",
                                     m1 = "Recent Years to Include", m2="HQ Aggregation")
corplot_param_sed_r

### view the correlation
param_sed_r_cor_pval = view_cor_pval(rankings_param_sed_r)
param_sed_r_cor_pval

### get the top 5 stations we'd recommend based on each method
priority_param_sed_r = list_priorities(rankings_param_sed_r, m1="Range of Recent Years", m2="HQ Aggregation")
priority_param_sed_r

### compare the medians to the non-medians
compare_rho(df_corrs = corplot_pretty(rankings_param_sed_r, return_cor=TRUE), 
            model = "median", grouping = TRUE, boxplots = TRUE, horizontal_graph = TRUE, force_scale = FALSE)


##### we do water quality now -- parameter recency #####
recent_range = c(0:5)
spatial_aggregation = c("mean", "median", "max", "pct95")
station_methods_recency = crossing(
  recent_range = recent_range,
  spatial_aggregation = spatial_aggregation,
)

sensitivity_param_water_recency <- station_methods_recency %>% pmap_dfr(~plot_top_hq_params(
  data = bol_water_scored, 
  media_type = "water", 
  spatial_aggregation = ..2, 
  temporal_aggregation = "recent",
  recent_range = ..1,
  all_params = TRUE,
  return_data=TRUE),
  .id="method_id") |>
  mutate(
    method = paste(
      station_methods_recency$recent_range[as.numeric(method_id)],
      station_methods_recency$spatial_aggregation[as.numeric(method_id)],
      sep = "_"
    )
  ) |>
  rename(HQ = hq) |>
  select(parameter, HQ, method)

# get our rankings
rankings_param_water_r = prepare_rankings(sensitivity_param_water_recency, wider=TRUE, c="parameter")

# get our corplot
corplot_param_water_r = corplot_pretty(rankings_param_water_r, title = "Correlation between Parameter Priorities for Water Data", subtitle = "", x="Method",y="Method",
                                     m1 = "Recent Years to Include", m2="HQ Aggregation")
corplot_param_water_r


##### we do water quality now -- station recency #####
recent_range = c(0:5)
spatial_aggregation = c("mean", "median", "max", "pct95")
station_methods_recency = crossing(
  recent_range = recent_range,
  spatial_aggregation = spatial_aggregation,
)

sensitivity_station_water_recency <- station_methods_recency %>% pmap_dfr(~plot_top_hq_stations(
  data = bol_water_scored, 
  media_type = "water", 
  param = "all",
  param_aggregation = "pct95", 
  temporal_aggregation = "recent",
  recent_range = ..1,
  all_stations = TRUE,
  return_data=TRUE),
  .id="method_id") |>
  mutate(
    method = paste(
      station_methods_recency$recent_range[as.numeric(method_id)],
      station_methods_recency$spatial_aggregation[as.numeric(method_id)],
      sep = "_"
    )
  ) |>
  rename(HQ = hq) |>
  select(station, HQ, method)

# get our rankings
rankings_station_water_r = prepare_rankings(sensitivity_station_water_recency, wider=TRUE, c="station")

# get our corplot
rankings_station_water_r = corplot_pretty(rankings_station_water_r, title = "Correlation between Station Priorities for Water Data", subtitle = "", x="Method",y="Method",
                                       m1 = "Recent Years to Include", m2="HQ Aggregation")
rankings_station_water_r

##### View all parameter histograms #####
bol_water_scored |> 
  mutate(HQ = HQ+1e-6) |> 
  ggplot(aes(x=HQ)) + 
  geom_histogram()+
  facet_wrap(~parameter)+
  scale_x_log10()+
  theme_minimal()+
  labs(x="Hazard Quotient", y="Count", title="Hazard Distribution for Sampled Parameters")


bol_water_scored |> 
  mutate(concentration = concentration+1e-6) |>
  group_by(parameter) |>
  ggplot(aes(x=concentration)) + 
  geom_histogram()+
  facet_wrap(~parameter)+
  scale_x_log10()+
  theme_minimal()+
  labs(x="Hazard Quotient", y="Count", title="Hazard Distribution for Sampled Parameters")