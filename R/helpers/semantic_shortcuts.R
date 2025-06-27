labeled_numeric_input <- function (input_id, label, value = NULL, min = NA, max = NA,
          step = NA, type = NULL, icon = NULL, placeholder = NULL, class = NULL,
          ...)
{
  if (!(is.null(placeholder) || is.character(placeholder))) {
    stop("'placeholder' should be NULL or character")
  }
  if (is.null(value) & is.null(placeholder))
    stop("either 'value' or 'placeholder' should be defined")
  if (!is.null(value)) {
    if (!is.numeric(value) & !grepl("^\\d*(\\.\\d*|)$",
                                    value))
      stop("Non-numeric input detected")
  }
  input_tag <- tags$input(id = input_id, value = value, type = "number",
                          placeholder = placeholder, class=if(!is.null(class))class)
  if (!is.na(min)){
    input_tag$attribs$min <- min
  } else{
    min <- ""
  }

  if (!is.na(max)){
    input_tag$attribs$max <- max
  }
  else{
    max <- ""
  }
  if (!is.na(step))
    input_tag$attribs$step <- step
  if (!is.null(icon)) {
    type <- paste(type, "icon")
  }
  label_item <- label(paste0("[",min,";",max,"]"),class="basic",is_link=FALSE)
  shiny::div(class = "field", if (!is.null(label))
    tags$label(label, `for` = input_id), shiny::div(class = paste("ui right labeled",
                                                                  type, "input"), input_tag, label_item, icon))
}

custom_register_search <- function(session, data, search_query, endpoint_name="search_api",database=NULL) {
  session$registerDataObj(endpoint_name, data, function(data, request) { # nolint
    query <- shiny::parseQueryString(request$QUERY_STRING)
    extracted_query <- query$q
    response <- jsonlite::toJSON(list(
      success = TRUE,
      results = search_query(data, extracted_query, database)
    ))
    # Inspired by: https://stat.ethz.ch/pipermail/r-devel/2013-August/067210.html
    # It's because httpResponse is not exported from shiny
    # and triggers NOTE in R CMD check
    f <- shiny::httpResponse
    f(200, "application/json", enc2utf8(response))
  })
}

custom_checkbox_input <- function (input_id, label = "", type = NULL, is_marked = TRUE,
          style = NULL)
{
  div(class = paste("ui", type, if (is_marked)
    "checked", "checkbox"), style = style, tags$input(id = input_id,
                                                      type = "checkbox", checked = if (is_marked)
                                                        NA
                                                      else NULL), htmltools::tagAppendAttributes(tags$label(label),"for"=input_id))
}

custom_text_input <- function (input_id, label = NULL, value = "", type = "text",
          placeholder = NULL, attribs = list())
{
  if (!type %in% c("text", "textarea", "password", "email",
                   "url", "tel")) {
    stop(type, " is not a valid Semantic UI input")
  }
  if (type == "textarea") {
    input <- tags$textarea(id = input_id, value, placeholder = placeholder)
  }
  else {
    input <- tags$input(id = input_id, value = value, type = type,
                        placeholder = placeholder)
  }
  for (i in names(attribs)) input$attribs[[i]] <- attribs[[i]]
  if (is.null(label))
    input
  else tags$div(tags$label(label), input,class="field")
}

customUpdateSelectInput <- function (session, inputId, label = NULL, choices = NULL, selected = NULL)
{
  if (!is.null(selected))
    selected <- paste(as.character(selected), collapse = ",")
  else selected <- NULL
  if (!is.null(choices)) {
    choices_text <- names(choices)
    choice_value <- unlist(unname(choices))
    if (identical(choices_text, NULL))
      choices_text <- choices
    options <- jsonlite::toJSON(list(values = data.frame(name = choices_text,
                                                         text = choices_text, value = choice_value)))
  }
  else {
    options <- NULL
  }
  message <- list(label = label, choices = options, value = selected)
  message <- message[!vapply(message, is.null, FUN.VALUE = logical(1))]
  session$sendInputMessage(inputId, message)
}


createCard <- function(plot,title,subtitle,description,extra=NULL){
  card(
    div(class="image",plot),
    div(class="content",
        div(class="header", title),
        div(class="meta", subtitle),
        div(class="description", description),
        if(!is.null(extra)){
          div(class="extra content", extra)
        }
    )
  )
}

toast <- function(title,content,color){

  shinyjs::runjs(paste0("$('body').toast({
  title: '",title,"',
  message: '",content,"',
  showProgress: 'bottom',
  classProgress: '",color,"',
  displayTime: 'auto'
})
;"))
}
nag <- function(title,content,color){
  tagList(tagAppendAttributes(
      div(
        ifelse(title!="",tagAppendAttributes(div(title),class="title"),span()),
        div(HTML(content)),
        icon("close")
    ),class=paste0("ui nag ",color)
  ),HTML("<script>$('.ui.nag').nag();</script>")
  )
}

define_selection_type <- function(input_id, multiple) {
  multiple_class <- switch(multiple, "multiple", NULL)
  classes <- c("ui", "fluid", "search", "selection",
               "dropdown", multiple_class, input_id)
  paste(classes, collapse = " ")
}

custom_search_selection_api <- function (input_id, search_api_url, multiple = FALSE, default_text = "Select",class="",value=NULL)
{
  selection_type <- define_selection_type(input_id, multiple)
  shiny::tagList(tags$div(class = paste(selection_type,class), shiny_input(input_id,
                                                              tags$input(class = "prompt", type = "hidden", name = input_id),
                                                              type = "text",value = value), icon("search"), tags$div(class = "default text",default_text), tags$div(class = "menu")),
                 HTML(sprintf(
                   "<script>$('.ui.dropdown.%s').dropdown({\nforceSelection: false,\napiSettings: {\nurl: '%s&q={query}'\n}\n}).dropdown('change values',{values:[ {'value', 'text', 'name'} ]} )</script>",
                   input_id, search_api_url)
                 ))
}
#'%s'.split(',')

custom_search_selection_choices <- function(input_id,
                                            choices,
                                            value = NULL,
                                            multiple = FALSE,
                                            default_text = "Select",
                                            groups = NULL,
                                            class = "",
                                            dropdown_settings = list(forceSelection = FALSE,fullTextSearch="exact",ignoreDiacritics="true")) {
  input_class <- define_selection_type(input_id, multiple)
  if (is.null(value)) {
    value <- ""
  }

  shiny::tagList(
    tags$div(class = paste(input_class,class),
             shiny_input(input_id,
                         tags$input(class = "prompt",
                                    type = "hidden",
                                    name = input_id),
                         value = value,
                         type = "text"
             ),
             icon("search"),
             tags$div(class = "default text", default_text),
             tags$div(class = "menu",
                      if (is.null(choices)) {
                        NULL
                      } else if (is.null(groups) & !is.null(names(choices))) {
                        purrr::map2(names(choices),choices, ~
                                      div(class = "item", `data-value` = .y, .x)
                        )  %>% shiny::tagList()
                      } else if (is.null(groups) & is.null(names(choices))) {
                        purrr::map(choices, ~
                                      div(class = "item", `data-value` = ., .)
                        )  %>% shiny::tagList()
                      } else {
                        group_levels <- unique(groups)
                        group_choices <- purrr::map(group_levels, ~ choices[groups == .])
                        divide_choices <- function(group, group_specific_choices) {
                          shiny::tagList(
                            div(class = "ui horizontal divider", style = "border-top: none !important;", group),
                            purrr::map(group_specific_choices, ~
                                         div(class = "item", `data-value` = ., .)
                            )
                          )
                        }
                        purrr::map2(group_levels, group_choices, divide_choices) %>%
                          shiny::tagList()
                      }
             )
    ),
    HTML(
      sprintf(
        "<script>$('.ui.dropdown.%s').dropdown(%s).dropdown('set selected', '%s'.split(','));</script>",
        input_id, jsonlite::toJSON(dropdown_settings, auto_unbox = TRUE), value)
    )
  )
}
