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

  plot_ly(
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
  )  %>% {
    if (is_stratified) {
      add_trace(
        .,
        data = boxplot_data %>% filter(strat == strat_factors[2]),
        y = ~ aggr,
        x = ~ interval,
        color = ~ factor(strat),
        customdata =  ~ stay_id,
        type = "box",
        source = "user_plot",
        colors = graph_color_palette[1:strat_count],
        offsetgroup = strat_factors[2],
        xaxis = 'x',
        yaxis = "y"
      )
    } else{
      .
    }
  } %>%
    add_trace(
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
      offsetgroup =  strat_factors[1],
      marker = list(color = "gray"),
      name = "Total observations",
      xaxis = 'x'
    ) %>%
    {
      if (is_stratified) {
        add_trace(
          .,
          data = observation_count %>% filter(strat == strat_factors[2]),
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
          offsetgroup = strat_factors[2],
          marker = list(color = "gray"),
          name = "Total observations",
          xaxis = 'x'
        )
      } else{
        .
      }
    } %>%
    layout(
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

  plot_ly(
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
  )  %>% {
    if (is_stratified) {
      add_trace(
        .,
        data = boxplot_data %>% filter(strat == strat_factors[2]),
        y = ~ aggr,
        x = ~ interval,
        color = ~ factor(strat),
        customdata =  ~ stay_id,
        type = "violin",
        source = "user_plot",
        colors = graph_color_palette[1:strat_count],
        offsetgroup = strat_factors[2],
        xaxis = 'x',
        yaxis = "y"
      )
    } else{
      .
    }
  } %>%
    add_trace(
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
      offsetgroup =  strat_factors[1],
      marker = list(color = "gray"),
      name = "Total observations",
      xaxis = 'x'
    ) %>%
    {
      if (is_stratified) {
        add_trace(
          .,
          data = observation_count %>% filter(strat == strat_factors[2]),
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
          offsetgroup = strat_factors[2],
          marker = list(color = "gray"),
          name = "Total observations",
          xaxis = 'x'
        )
      } else{
        .
      }
    } %>% layout(
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
  sorted_data <- pie_data %>% arrange(strat,value) %>% count(strat,value)

  first_strat_data <- sorted_data %>% filter(strat == strat_factors[1]) %>%
    arrange(desc(n)) %>%
    mutate(value = if_else(row_number() > 10, "Others", value)) %>%
    group_by(value) %>%
    summarise(n = sum(n)) %>%
    arrange(desc(n))

  second_strat_data <- sorted_data %>% filter(strat == strat_factors[2]) %>%
    arrange(desc(n)) %>%
    mutate(value = if_else(row_number() > 10, "Others", value)) %>%
    group_by(value) %>%
    summarise(n = sum(n)) %>%
    arrange(desc(n))

  plot <- plot_ly(
    data = first_strat_data,
    labels = ~factor(value),
    values = ~n,
    name = strat_factors[1],
    type = "pie",
    marker=list(colors = graph_color_palette),
    domain= {if(is_stratified)
      list(x = c(0, 0.5), y = c(0, 1))
              else
                list(x = c(0, 1), y = c(0, 1))
      }

  ) %>%
  {
    if (is_stratified){
      add_pie(.,data = second_strat_data,
      labels = ~factor(value),
      values = ~n,
      name = strat_factors[2],
      type = "pie",
      marker=list(colors= graph_color_palette),
      domain = list(x = c(0.5, 1), y = c(0, 1))
      )
    }
    else{
      .
    }
  } %>% layout(title = pie_title,showlegend = FALSE)

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
