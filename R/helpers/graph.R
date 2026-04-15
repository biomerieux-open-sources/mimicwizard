graph_color_palette <-
  RColorBrewer::brewer.pal(8, "Set2")

# boxplotdata is a tibble (aggr,interval,strat,stay_id)
# If no stratified interval = strat
stratified_boxplot <- function(boxplot_data, labelx, labely, is_stratified) {
  strat_count <- length(levels(as.factor(boxplot_data$strat)))

  observation_count <-
    boxplot_data %>%
    count(interval, strat)

  strat_factors <- factor(unique(boxplot_data$strat))

  # Initial plot with first strat factor (or all data if not stratified)
  p <- plot_ly(
    data = boxplot_data %>% {
      if (is_stratified)
        filter(., strat == strat_factors[1])
      else
        .
    },
    y = ~ aggr,
    x = ~ interval,
    color = ~ factor(strat),
    customdata =  ~ stay_id,
    type = "box",
    source = "user_plot",
    colors = graph_color_palette[1:strat_count],
    offsetgroup = strat_factors[1],
    xaxis = 'x',
    yaxis = "y"
  )

  # Add box traces for additional strat factors
  if (is_stratified && length(strat_factors) > 1) {
    for (i in 2:length(strat_factors)) {
      sf <- strat_factors[i]
      p <- p %>% add_trace(
        data = boxplot_data %>% filter(strat == sf),
        y = ~ aggr,
        x = ~ interval,
        color = ~ factor(strat),
        customdata =  ~ stay_id,
        type = "box",
        source = "user_plot",
        colors = graph_color_palette[1:strat_count],
        offsetgroup = sf,
        xaxis = 'x',
        yaxis = "y"
      )
    }
  }

  # Add observation count bars for first strat factor (or all if not stratified)
  p <- p %>% add_trace(
    data = observation_count %>% {
      if (is_stratified)
        filter(., strat == strat_factors[1])
      else
        .
    },
    y = ~ n,
    x = ~ interval,
    color = ~ factor(strat),
    customdata = TRUE,
    hoverinfo = ~ n,
    type = "bar",
    yaxis = "y2",
    opacity = 0.5,
    width = 0.1,
    showlegend = F,
    offsetgroup = strat_factors[1],
    marker = list(color = "gray"),
    name = "Total observations",
    xaxis = 'x'
  )

  # Add observation count bars for additional strat factors
  if (is_stratified && length(strat_factors) > 1) {
    for (i in 2:length(strat_factors)) {
      sf <- strat_factors[i]
      p <- p %>% add_trace(
        data = observation_count %>% filter(strat == sf),
        y = ~ n,
        x = ~ interval,
        color = ~ factor(strat),
        customdata = TRUE,
        hoverinfo = ~ n,
        type = "bar",
        yaxis = "y2",
        opacity = 0.5,
        width = 0.1,
        showlegend = F,
        offsetgroup = sf,
        marker = list(color = "gray"),
        name = "Total observations",
        xaxis = 'x'
      )
    }
  }

  p %>% layout(
    boxmode = "group",
    xaxis = list(title = labelx),
    yaxis = list(
      title = labely,
      zeroline = F,
      side = "left"
    ),
    yaxis2 = list(
      overlaying = "y",
      showline = FALSE,
      side = "right",
      title = "Total observations",
      range = list(0, max(observation_count$n) * 4),
      showgrid = F
    )
  )
}

stratified_violin_plot <- function(boxplot_data, labelx, labely, is_stratified) {
  strat_count <- length(levels(as.factor(boxplot_data$strat)))
  observation_count <-
    boxplot_data %>%
    count(interval, strat)

  strat_factors <- factor(unique(boxplot_data$strat))

  # Initial plot with first strat factor (or all data if not stratified)
  p <- plot_ly(
    data = boxplot_data %>% {
      if (is_stratified)
        filter(., strat == strat_factors[1])
      else
        .
    },
    y = ~ aggr,
    x = ~ interval,
    color = ~ factor(strat),
    customdata =  ~ stay_id,
    type = "violin",
    source = "user_plot",
    colors = graph_color_palette[1:strat_count],
    offsetgroup = strat_factors[1],
    xaxis = 'x',
    yaxis = "y"
  )

  # Add violin traces for additional strat factors
  if (is_stratified && length(strat_factors) > 1) {
    for (i in 2:length(strat_factors)) {
      sf <- strat_factors[i]
      p <- p %>% add_trace(
        data = boxplot_data %>% filter(strat == sf),
        y = ~ aggr,
        x = ~ interval,
        color = ~ factor(strat),
        customdata =  ~ stay_id,
        type = "violin",
        source = "user_plot",
        colors = graph_color_palette[1:strat_count],
        offsetgroup = sf,
        xaxis = 'x',
        yaxis = "y"
      )
    }
  }

  # Add observation count bars for first strat factor (or all if not stratified)
  p <- p %>% add_trace(
    data = observation_count %>% {
      if (is_stratified)
        filter(., strat == strat_factors[1])
      else
        .
    },
    y = ~ n,
    x = ~ interval,
    color = ~ factor(strat),
    customdata = TRUE,
    hoverinfo = ~ n,
    type = "bar",
    yaxis = "y2",
    opacity = 0.5,
    width = 0.1,
    showlegend = F,
    offsetgroup = strat_factors[1],
    marker = list(color = "gray"),
    name = "Total observations",
    xaxis = 'x'
  )

  # Add observation count bars for additional strat factors
  if (is_stratified && length(strat_factors) > 1) {
    for (i in 2:length(strat_factors)) {
      sf <- strat_factors[i]
      p <- p %>% add_trace(
        data = observation_count %>% filter(strat == sf),
        y = ~ n,
        x = ~ interval,
        color = ~ factor(strat),
        customdata = TRUE,
        hoverinfo = ~ n,
        type = "bar",
        yaxis = "y2",
        opacity = 0.5,
        width = 0.1,
        showlegend = F,
        offsetgroup = sf,
        marker = list(color = "gray"),
        name = "Total observations",
        xaxis = 'x'
      )
    }
  }

  p %>% layout(
    xaxis = list(title = labelx),
    yaxis = list(title = labely, zeroline = F, side = "left"),
    yaxis2 = list(
      showline = FALSE,
      side = "right",
      overlaying = "y",
      title = "Total observations",
      range = list(0, max(observation_count$n) * 4),
      showgrid = F
    ),
    violinmode = 'group'
  )
}


stratified_pie <- function(pie_data, pie_title, is_stratified) {
  strat_factors <- factor(unique(pie_data$strat))
  strat_count <- length(strat_factors)
  sorted_data <- pie_data %>% arrange(strat, value) %>% count(strat, value)

  # Prepare data for each strat factor (top 10 + "Others")
  strat_data_list <- lapply(strat_factors, function(sf) {
    sorted_data %>%
      filter(strat == sf) %>%
      arrange(desc(n)) %>%
      mutate(value = if_else(row_number() > 10, "Others", value)) %>%
      group_by(value) %>%
      summarise(n = sum(n)) %>%
      arrange(desc(n))
  })

  # Compute pie domain: split x-axis equally if stratified, full width otherwise
  get_domain <- function(i) {
    if (!is_stratified) {
      list(x = c(0, 1), y = c(0, 1))
    } else {
      x_start <- (i - 1) / strat_count
      x_end <- i / strat_count
      list(x = c(x_start, x_end), y = c(0, 1))
    }
  }

  # Initial plot with first strat factor
  p <- plot_ly(
    data = strat_data_list[[1]],
    labels = ~factor(value),
    values = ~n,
    name = strat_factors[1],
    type = "pie",
    marker = list(colors = graph_color_palette),
    domain = get_domain(1)
  )

  # Add pies for additional strat factors
  if (is_stratified && strat_count > 1) {
    for (i in 2:strat_count) {
      p <- p %>% add_pie(
        data = strat_data_list[[i]],
        labels = ~factor(value),
        values = ~n,
        name = strat_factors[i],
        type = "pie",
        marker = list(colors = graph_color_palette),
        domain = get_domain(i)
      )
    }
  }

  p %>% layout(title = pie_title, showlegend = FALSE)
}

stratified_table <- function(table_data, is_stratified){
  DT::renderDataTable(table_data %>% {
    if (is_stratified)
      count(.,value,strat)
    else
      count(.,value)
  } %>% arrange(desc(n)),
  extensions = c(
    "Buttons",
    "RowGroup",
    "Scroller"
  ),
  selection = "none",
  rownames = FALSE,
  filter = list(position = 'top'),
  options = list(
    dom = 'ftrip',
    pageLength = 10,
    autoWidth = F,
    scrollX = T
  ),
  escape = FALSE)
}
