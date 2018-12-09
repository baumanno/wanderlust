

# Area plot comments ------------------------------------------------------

area_plot_posts <- function (data,
                             cut_start = NA,
                             cut_end = NA) {
  area_chart(data) +
    labs(x = "", y = "Anteil Kommentare") +
    scale_fill_manual(
      "Topic",
      breaks = topics_present,
      values = cols,
      guide = guide_legend(
        direction = "horizontal",
        title.position = "top",
        label.position = "bottom",
        nrow = 1
      )
    ) +
    geom_vline(aes(xintercept = as.Date(cut_start)),
               linetype = "dotted",
               size = 0.2) +
    geom_vline(aes(xintercept = as.Date(cut_end)),
               linetype = "dotted",
               size = 0.2) +
    # ggplot2::annotate(
    #   "rect",
    #   xmin = as.Date(cut_start),
    #   xmax = as.Date(cut_end),
    #   ymin = 0,
    #   ymax = Inf,
    #   fill = "lightgrey",
    #   alpha = .5
    # ) +
    theme(legend.position = "bottom")
}

# Area plot alters --------------------------------------------------------

area_plot_alters <- function (data,
                              cut_start = NA,
                              cut_end = NA) {
  area_chart(data) +
    labs(x = "", y = "Anteil Alteri") +
    scale_fill_manual(
      "Topic",
      breaks = topics_present,
      values = cols,
      guide = guide_legend(
        direction = "horizontal",
        title.position = "top",
        label.position = "bottom",
        nrow = 1
      )
    ) +
    geom_vline(aes(xintercept = as.Date(cut_start)),
               linetype = "dotted") +
    geom_vline(aes(xintercept = as.Date(cut_end)),
               linetype = "dotted") +
    # ggplot2::annotate(
    #   "rect",
    #   xmin = as.Date(cut_start),
    #   xmax = as.Date(cut_end),
    #   ymin = 0,
    #   ymax = Inf,
    #   fill = "lightgrey",
    #   alpha = .5
    # ) +
    theme(legend.position = "bottom")
}

# Comment distribution (boxplot) ------------------------------------------

comment_dist_boxplot <- function(data) {
  data %>%
    ggplot(mapping = aes(
      x = factor(topic),
      y = num_posts,
      colour = topic
    )) +
    geom_boxplot() +
    labs(x = "Topic",
         y = "Posts") +
    theme(axis.text.x = element_text(angle = 0),
          legend.position = "none") +
    scale_colour_manual(palette = colorRampPalette(brewer.pal(9, "Paired"))) # +
  # guides(colour = guide_legend(title = "Topic",direction = "horizontal",
  #     title.position = "top",
  #     label.position = "bottom",
  #     nrow = 1))
}

# Correlation of distributions --------------------------------------------

distr_correlation <-
  function(ego_data,
           alters_data,
           date_range_cut_start,
           date_range_cut_end,
           cols) {
    joined <- left_join(ego_data,
                        alters_data,
                        by = c("date", "topic")) %>%
      rename(prop_ego = prop.x, prop_alters = prop.y) %>%
      group_by(topic) %>%
      drop_na()
    
    coeffs <- joined %>%
      summarise(
        r = cor.test(prop_ego, prop_alters, method = "spearman")$estimate,
        p = cor.test(prop_ego, prop_alters, method = "spearman")$p.value
      )
    force(coeffs)
    
    joined %>%
      group_by(topic) %>%
      filter(n() > 1) %>%
      filter(date >= date_range_cut_start &
               date <= date_range_cut_end) %>%
      ggplot(mapping = aes(x = prop_ego, y = prop_alters, colour = topic)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", size = .8) +
      geom_abline(size = .1) +
      labs(x = "Anteil Kommentare",
           y = "Anteil Alteri") +
      scale_colour_manual("topic",
                          values = cols) +
      facet_wrap( ~ topic) +
      geom_text(
        data = coeffs,
        mapping = aes(
          x = 0,
          y = Inf,
          label = paste(
            "italic(rho) == ",
            round(r, 2),
            ifelse(
              p <= 0.001,
              "*plain('***')",
              ifelse(
                p <= 0.01,
                "*plain('**')",
                ifelse(p <= 0.05, "*plain('*')", "^plain('n.s.')")
              )
            )
          )
        ),
        parse = TRUE,
        colour = "black",
        size = 3,
        hjust   = .05,
        vjust   = 1.5
      ) +
      theme(axis.text.x = element_text(angle = 0),
            legend.position = "none")
  }

# Number of posts per topic -----------------------------------------------

num_posts_facet <- function(data) {
  data %>%
    group_by(date, topic) %>%
    mutate(sum = sum(num_posts)) %>%
    ggplot(mapping = aes(x = date, y = num_posts, colour = topic)) +
    geom_point(alpha = .8) +
    geom_smooth(method = "loess") +
    labs(x = "",
         y = "Posts") +
    facet_wrap(~ topic) +
    theme(axis.text.x = element_text(angle = 0),
          legend.position = "none") +
    scale_colour_manual(palette = colorRampPalette(brewer.pal(9, "Paired")))
}

# Attractivity ------------------------------------------------------------

attractivity <- function(ego_proportions_full,
                         alters_proportions_full,
                         date_range_cut_end,
                         date_range_cut_start,
                         cols) {
  left_join(ego_proportions_full,
            alters_proportions_full,
            by = c("date", "topic")) %>%
    rename(prop_ego = prop.x, prop_alters = prop.y) %>%
    filter(date <= date_range_cut_end &
             date >= date_range_cut_start) %>%
    mutate(pa_pe = prop_alters * prop_ego) %>%
    group_by(topic) %>%
    filter(sum(pa_pe) > 0) %>%
    ggplot(mapping = aes(x = date, y = pa_pe, colour = topic)) +
    geom_line() +
    geom_smooth(size = .25, method = "loess") +
    labs(x = "", y = expression("italic(g)")) +
    theme(axis.text.x = element_text(angle = 45),
          legend.position = "none") +
    scale_x_date(date_breaks = "1 years",
                 date_labels = "%Y") +
    scale_colour_manual("topic",
                        values = cols) +
    facet_wrap(topic ~ .)
}

# Comment totals in one plot ----------------------------------------------

total_comments <-
  function(ego_proportions,
           date_range_cut_end,
           date_range_cut_start) {
    ego_proportions %>%
      filter(date <= date_range_cut_end &
               date >= date_range_cut_start) %>%
      group_by(date) %>%
      mutate(sum = sum(num_posts)) %>%
      group_by(topic) %>%
      filter(sum(num_posts) > 1) %>%
      ggplot(mapping = aes(x = date, y = num_posts, colour = topic)) +
      geom_line(size = .5) +
      # geom_point(alpha = .5) +
      geom_line(
        mapping = aes(y = sum),
        colour = "red",
        size = .3,
        linetype = "dashed"
      ) +
      labs(x = "",
           y = "Posts",
           legend = "Topic") +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "bottom") +
      scale_colour_manual(
        "Topic",
        values = cols,
        guide = guide_legend(
          direction = "horizontal",
          title.position = "top",
          label.position = "bottom",
          nrow = 1
        )
      ) +
      scale_x_date(
        date_breaks = "1 year",
        date_labels = "%Y",
        date_minor_breaks = "1 month"
      )
  }


# Network structure -------------------------------------------------------

network_structure <- function(graph_data) {
  graph_data %>%
    ggplot(mapping = aes(x = date, y = count, colour = variable)) +
    geom_line(mapping = aes(y = vvcount(graph), colour = "Knoten")) +
    geom_smooth(mapping = aes(y = vvcount(graph), colour = "Knoten"),
                size = .25) +
    geom_line(mapping = aes(y = vecount(graph), colour = "Kanten")) +
    geom_smooth(mapping = aes(y = vecount(graph), colour = "Kanten"),
                size = .25) +
    scale_x_date(
      date_breaks = "1 year",
      date_minor_breaks = "3 month",
      date_labels = "%Y"
    ) +
    theme(axis.text.x = element_text(angle = 0),
          legend.position = "bottom") +
    labs(x = "",
         y = "Anzahl") +
    scale_colour_discrete(name = "Variable")
}

# Plot Katz-Powell reciprocity --------------------------------------------

katz_powell_plot <- function(graph_data) {
  graph_data %>%
    mutate(
      katz_powell = katz_powell_mutuality(graph),
      ratio_reciprocity = map_dbl(graph, reciprocity, mode = "ratio")
    ) %>%
    ggplot(mapping = aes(x = date, y = katz_powell)) +
    geom_line() +
    geom_smooth() +
    scale_x_date(
      date_breaks = "1 year",
      date_minor_breaks = "3 month",
      date_labels = "%Y"
    ) +
    theme(axis.text.x = element_text(angle = 0),
          legend.position = "bottom") +
    labs(x = "", y = expression(rho["KP"]))
}


# Order of topical subgraph -----------------------------------------------

toical_subgraph_order <- function(graph_topical_subgraph) {
  graph_topical_subgraph %>%
    mutate(size_diff = vvcount(graph) - vvcount(topical_subgraph)) %>%
    ggplot(mapping = aes(x = date, y = nodes, colour = graph)) +
    geom_line(mapping = aes(y = vvcount(graph), colour = "G")) +
    geom_line(mapping = aes(y = vvcount(topical_subgraph), colour = "G'")) +
    geom_line(mapping = aes(y = size_diff, colour = "G - G'")) +
    geom_smooth(
      method = "lm",
      se = FALSE,
      size = .3,
      mapping = aes(y = size_diff, colour = "G - G'")
    ) +
    geom_smooth(
      method = "lm",
      se = FALSE,
      size = .3,
      mapping = aes(y = vvcount(graph), colour = "G")
    ) +
    geom_smooth(
      method = "lm",
      se = FALSE,
      size = .3,
      mapping = aes(y = vvcount(topical_subgraph), colour = "G'")
    ) +
    scale_x_date(
      date_breaks = "1 year",
      date_minor_breaks = "3 month",
      date_labels = "%Y"
    ) +
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "bottom")
}


# Reciprocity in topical subgraph -----------------------------------------

topic_subgraph_reciprocity <- function(graph_topical_subgraph) {
  graph_topical_subgraph %>%
    mutate(kp_full = katz_powell_mutuality(graph),
           kp_topical = katz_powell_mutuality(topical_subgraph)) %>%
    ggplot(mapping = aes(x = date, y = nodes, colour = graph)) +
    geom_line(mapping = aes(y = kp_full, colour = "G")) +
    geom_line(mapping = aes(y = kp_topical, colour = "G'")) +
    geom_line(mapping = aes(y = kp_topical - kp_full, colour = "G' - G'")) +
    geom_smooth(mapping = aes(y = kp_topical - kp_full, colour = "G'- G")) +
    scale_x_date(
      date_breaks = "1 year",
      date_minor_breaks = "3 month",
      date_labels = "%Y"
    ) +
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "bottom")
}


# Sizes of single topic subgraphs -----------------------------------------

subgraph_sizes <- function(graph_single_topic_subgraph) {
  graph_single_topic_subgraph %>%
    mutate(vs = vvcount(topic_graph) - 1,
           es = vecount(topic_graph)) %>%
    ggplot(mapping = aes(x = date, y = value, colour = variable)) +
    geom_line(mapping = aes(y = vs, colour = "vertices")) +
    geom_line(mapping = aes(y = es, colour = "edges")) +
    scale_x_date(
      date_breaks = "1 year",
      date_minor_breaks = "3 month",
      date_labels = "%Y"
    ) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "",
         y = "Anzahl") +
    scale_color_discrete(name = "Variable",
                         labels = c("vertices" = "Knoten", "edges" = "Kanten")) +
    facet_wrap( ~ topic)
}


# Size of subgraph for single topic ---------------------------------------

subgraph_size_topic <-
  function(graph_single_topic_subgraph, the_topic) {
    graph_single_topic_subgraph %>%
      filter(topic == the_topic) %>%
      mutate(vs = vvcount(topic_graph) - 1,
             es = vecount(topic_graph)) %>%
      ggplot(mapping = aes(x = date, y = value, colour = variable)) +
      geom_line(mapping = aes(y = vs, colour = "vertices")) +
      geom_line(mapping = aes(y = es, colour = "edges")) +
      scale_x_date(
        date_breaks = "1 year",
        date_minor_breaks = "3 month",
        date_labels = "%Y"
      ) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(x = "",
           y = "Anzahl") +
      scale_color_discrete(name = "Variable",
                           labels = c("vertices" = "Knoten", "edges" = "Kanten")) +
      geom_vline(mapping = aes(xintercept = as.Date("2013-10-01")),
                 linetype = "dotted") +
      facet_wrap( ~ topic)
  }

subgraph_reciprocity_all <-
  function(graph_single_topic_subgraph) {
    graph_single_topic_subgraph %>%
      mutate(kp = katz_powell_mutuality(topic_graph)) %>%
      ggplot(mapping = aes(x = date, y = kp, colour = topic)) +
      geom_line() +
      geom_smooth() +
      scale_x_date(
        date_breaks = "1 year",
        date_minor_breaks = "3 month",
        date_labels = "%Y"
      ) +
      theme(axis.text.x = element_text(angle = 0)) +
      labs(x = "Zeit",
           y = "Katz-Powell-Index") +
      scale_colour_discrete(name = "Topic") +
      facet_grid(topic ~ .)
  }


# Reciprocity in thematic subgraph for single topic -----------------------

subgraph_reciprocity_single_topic <-
  function(graph_single_topic_subgraph, the_topic) {
    graph_single_topic_subgraph %>%
      filter(topic == the_topic) %>%
      mutate(kp = katz_powell_mutuality(topic_graph)) %>%
      ggplot(data = na.omit(.),
             mapping = aes(x = date, y = kp, colour = topic)) +
      geom_line() +
      geom_point(size = .4) +
      scale_x_date(
        date_breaks = "1 year",
        date_minor_breaks = "3 month",
        date_labels = "%Y"
      ) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(x = "Zeit",
           y = "Katz-Powell-Index") +
      scale_colour_discrete(name = "Topic") +
      facet_wrap(~ topic)
  }

total_comments_smooth <- function(ego_proportions, cols) {
  ego_proportions %>%
    group_by(date) %>%
    # mutate(sum = sum(num_posts)) %>%
    group_by(topic) %>%
    filter(sum(num_posts) > 1) %>%
    ggplot(mapping = aes(x = date, y = num_posts, colour = topic)) +
    geom_smooth() +
    # geom_point(alpha = .5) +
    # geom_smooth(
    #   mapping = aes(y = sum),
    #   colour = "red",
    #   size = .3,
    #   linetype = "dashed"
    # ) +
    labs(x = "",
         y = "Posts") +
    theme(axis.text.x = element_text(angle = 0),
          legend.position = "bottom") +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      date_minor_breaks = "3 month"
    ) +
    scale_colour_manual(
      name = "Topic",
      values = cols,
      guide = guide_legend(
        direction = "horizontal",
        title.position = "top",
        label.position = "bottom",
        nrow = 1
      )
    )
}