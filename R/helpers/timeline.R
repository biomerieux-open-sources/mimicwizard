
pal_set <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462",
             "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")

# timeline with timevis package
timeline <- function(x,
                     start = "starttime",
                     end  = "endtime",
                     content = "label",
                     group = "category",
                     group_label = "category",
                     hover = list(Amount = c("amount", "amountuom"),
                                  Rate = c("rate", "rateuom")),
                     id,
                     one_mn_toNA = TRUE,
                     filename = NULL,
                     elementId = "timeline",
                     category_to_merge = list(),
                     hadm_start = NULL,
                     hadm_end = NULL,
                     stay_start = NULL,
                     stay_end = NULL){

  start_group_to_merge <- x %>%
    select(!!sym(start),!!sym(group)) %>%
    count(!!sym(start),!!sym(group)) %>%
    filter(n > 1 & !!sym(group) %in% category_to_merge) %>%
    mutate(pair_str = paste(!!sym(start),!!sym(group))) %>%
    pull(pair_str)

  merged <- x %>%
    mutate(pair_str = paste(!!sym(start), !!sym(group))) %>%
    filter(pair_str %in% start_group_to_merge) %>%
    group_by(!!sym(start),!!sym(group)) %>%
    arrange(!!sym(content)) %>%
    filter(row_number()==1) %>%
    mutate(!!sym(content) := !!sym(group))
  not_to_merge <- x %>%
    mutate(pair_str = paste(!!sym(start), !!sym(group))) %>%
    filter(!(pair_str %in% start_group_to_merge))


  groups_label <- unname(unique(x[[group]]))
  group_option <- data.frame(
    "id" = c(groups_label,"Hospital Stay"),
    "content" = c(groups_label,"Hospital Stay")
  )

  y <- bind_rows(merged,not_to_merge)

  y$type <- "box"
  y <- y %>%
    mutate(type = case_when(
      !is.na(.data[[end]]) ~ "range",
      TRUE ~ type
    ))

  # Format data
  if(nrow(y)>0){
    data <- data.frame(
      id = 1:nrow(y),
      start = y[[start]],
      end  = y[[end]],
      content = y[[content]],
      group = y[[group]],
      type = y[["type"]]
    )
  } else{
    data <- data.frame(
    )
  }


  if(!is.null(hadm_start)){
    hadm <- data.frame(
      id = c(nrow(data)+1),
      start = c(hadm_start),
      end  = c(hadm_end),
      content = c("Hospital Stay"),
      group = c("Hospital Stay"),
      type = c("background"),
      style = c("background-color: rgba(0, 255, 0, 0.2);")
    )
    data <- bind_rows(data,hadm)
  }
  if(!is.null(stay_start)){
    stay <- data.frame(
      id = c(nrow(data)+1),
      start = c(stay_start),
      end  = c(stay_end),
      content = c("ICU Stay"),
      group = c("ICU Stay"),
      type = c("background"),
      style = c("background-color: rgba(255, 0, 0, 0.2);")
    )
    data <- bind_rows(data,stay)
    group_option <- data.frame(
      "id" = c(groups_label,"Hospital Stay","ICU Stay"),
      "content" = c(groups_label,"Hospital Stay","ICU Stay")
    )
  }

  tm <- timevis(
    data,
    elementId = elementId,
    width = '100%',
    groups = group_option,
    options = list(showCurrentTime = FALSE, # Drop vertical line at current date
                   locale = "en")           # date in english format
  )

  # Label outside box if too small
  #style <- ".vis-item .vis-item-overflow { overflow: visible; }"


  tm
}
