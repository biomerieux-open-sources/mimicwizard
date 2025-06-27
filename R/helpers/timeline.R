
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
                     category_to_merge = list()){

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
    "id" = groups_label,
    "content" = groups_label
  )

  y <- bind_rows(merged,not_to_merge)

  extended_content <- unlist(sapply(y[[content]],function(data){HTML(paste0(data,"<div class='ui label'>imagine</div>"))},simplify=F,USE.NAMES = F))
  # Format data
  data <- data.frame(
    id = y[["id"]],
    start = y[[start]],
    end  = y[[end]],
    content = y[[content]],
    group = y[[group]]
  )

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
