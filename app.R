source("renv/activate.R")
library(shiny)
library(shinyjs)
library(semantic.dashboard)
library(shiny.semantic)
library(tidyr)
library(plotly)
library(RColorBrewer)
library(timevis)
library(purrr)
library(readr)
library(dplyr)
library(dbplyr)
library(DBI)
library(RPostgres)
library(glue)
library(gt)
library(gtsummary)
library(htmltools)
library(jsonlite)
library(DT)
library(varhandle)
library(future)
library(tableone)
library(waiter)
library(lubridate)
library(tryCatchLog)
library(promises)
library(stringr)
library(DescTools)
library(shinycssloaders)
library(broom)
library(cardx)
source("global.R")

print(CONFIG)

plan(sequential)
options(shiny.maxRequestSize = 500 * 1024 ^ 2) #Max upload size is 500MB
options(bitmapType = 'cairo')
options(scipen = 999)
options(spinner.type = 3, spinner.color = "#0dc5c1",spinner.color.background="transparent")

# Add helper function to global env
file.sources = list.files(
  paste0(here::here(), "/R/helpers"),
  pattern = "*.R$",
  full.names = TRUE,
  ignore.case = TRUE
)
sapply(file.sources, source)

# Add helper function to global env
file.sources = list.files(
  paste0(here::here(), "/R/modules"),
  pattern = "*.R$",
  full.names = TRUE,
  ignore.case = TRUE
)
sapply(file.sources, source)

#fix: selectize dependency conflit with dt
#https://github.com/rstudio/shiny/issues/4174
htmltools::findDependencies(selectizeInput("foo", "Foo", choices = NULL))

ui <- tagList({
  if (CONFIG$INTERACTIVE)
    fluidPage(div(
      id = "introduction",
      div(
        class = "title",
        icon("hat wizard"),
        span("MIMICWizard"),
        tags$small("0.8"),
        div("A MIMIC-IV explorer for non technical users", class = "subtitle")
      ),
      div(
        id = "startup-choices",
        div(
          button("init_demo", "Demo initialization"),
          div(
            HTML(
              "Use this option when it's the <b>first time you're running the app in demo mode</b> or if your local demo database is corrupted (this will reset and erase the whole database, you'll lost you're created profile, event and cohort)"
            )
          ),
          class = "choice"
        ),
        div(
          button("run_demo", "Run demo"),
          div(
            "Use this option if you've already loaded the demo data in your local database"
          ),
          class = "choice"
        ),
        div(
          button("run_hosted", "Run full app (hosted)"),
          div(
            "Use this option to connect to the full database (please follow documentation for further configuration, you can force this option if you want to host the app on your server)"
          ),
          class = "choice"
        )
      ),

    ), )
}
, dashboardPage(
  dashboardHeader(
    color = "teal",
    inverted = TRUE,
    title = tags$span(
      icon("hat wizard"),
      "MIMIC Wizard",
      tags$small("0.8", style = "font-size:0.5em")
    ),
    center = uiOutput("demo_top_label"),
    right = tagList(
      span(textOutput("picked_profile"), class = "picked_profile"),
      htmltools::tagAppendAttributes(
        icon("circular grey inverted user outline profile-picker-icon"),
        style = "margin:0 1em"
      ),
      div(
        selectInput(
          inputId = "profile_picker",
          label = "Choose your profile",
          choices =  list(),
          selected = 0
        ),
        class = "ui popup profile-picker-popup",
        style = "z-index:9999999999"
      )
    ),

  ),

  dashboardSidebar(sidebarMenu(
    menuItem(
      "Parameter exploration",
      tabName = "explore",
      icon = icon("th")
    ),
    menuItem(
      "Patient explorer",
      tabName = "patient",
      icon = icon("users")
    ),
    menuItem(
      "Cohort creation",
      tabName = "cohort_creation",
      icon = icon("server")
    ),
    menuItem(
      "Cohort explorer",
      tabName = "cohort_explore",
      icon = icon("database")
    ),
    div(tagList(
      menuItem(
        "Developer settings",
        tabName = "dev_settings",
        icon = icon("code")
      ),
      menuItem(
        "Manage user profile",
        tabName = "user_profile",
        icon = icon("user circle outline")
      )
    ), class = "bottom-menu")
  )),
  dashboardBody(
    useShinyjs(),
    useWaiter(),
    useSteward(colors = c("#23a6d5", "#23d5ab","#23a6d5"),speed = 60),
    tags$head(
      tags$script(
        "$(document).on('shiny:sessioninitialized', function(event) {
        Shiny.setInputValue('cohort_explorer-url', window.location.href);
        $('.profile-picker-icon').popup({on:'click',position:'bottom left', movePopup: true});
        });"
      ),
      tags$style(
        HTML(
          ".mt-10{
      margin-top:10px;
      }
      .my-10{
      margin:1em 0 !important;
      }
      .mb-10{
      margin-bottom:1em;
      }
      .mr-10{
      margin-right:1em !important;
      }
      .m0{
      margin:0 !important;
      }
      .float-right{
      float:right;
      }
      .ui.label.filter {
      margin-right: 5px;
      }
    .white{
    color:#fff;
    }
    .gray{
    color:#aaa;
    }
    .fw-normal{
    font-weight: normal;
    }
    .right-float{
    float:right;
    }
    .label-popup{
      display:inline-block;
    }
    .ui.card.expanded{
      width:100%;
    }
    .full-width{
      width: 100%;
    }
    .pointer{
      cursor:pointer;
    }
    .pointer-scale{
      cursor:pointer;
      transition:all 0.3s;
    }
    .pointer-scale:hover{
      transform: scale(1.2);
    }
    .pointer-scale.reduced:hover{
      transform: scale(1.05);
    }
    .pointer-scale.more-reduced:hover{
      transform: scale(1.02);
    }
    .rotate-on-hover{
      transition:all 0.5s;
    }
    .rotate-on-hover:hover{
      transform: rotate(90deg);
    }
    #introduction{
      width: calc(100vw + 10px);
      height: calc(100vh + 10px);
      position: absolute;
      background: rgb(0,181,173);
      background: linear-gradient(74deg, rgba(0,181,173,1) 0%, rgba(48,99,142,1) 100%);
      z-index: 5;
      left: -10px;
      top: 0;
      display: flex;
      flex-direction: column;
      justify-content: space-evenly;
      align-items: center;
    }
    #introduction > .title{
      font-weight: bolder;
      color: white;
      font-size: 4em;
      width:100%;
      text-align: center;
    }
    #introduction > .title > .subtitle {
      font-size: 0.5em;
      font-weight: lighter;
      color: lightgray;
      margin-top: 20px;
  }
    #startup-choices{
      display: flex;
      flex-wrap:wrap;
      flex-direction: row;
      justify-content: space-evenly;
      align-items: center;
      width: 100vw;
    }
    #startup-choices > .choice{
      display: flex;
      flex-direction: column;
      width:300px;
      transition:all 0.5s;
      margin:20px;
    }
    #startup-choices > .choice:hover{
      transform: scale(1.1);
    }
    #startup-choices > .choice > div{
    min-height: 120px;
    padding:10px;
    margin-top:10px;
    border-radius:5px;
    background-color:rgba(255,255,255,0.75);
    }
    .dashboard-page > .ui[class*='top attached'].menu{
      margin-top:0px;
    }
    .ui.label.filter > .ui.label:empty {
      display: none;
    }
    .constraint-pickable{
      cursor: pointer;
    }
    .ui.tab.active.bottom.attached.grid.segment{
    min-height:70vh;
    overflow-y:clip;
    }
    .irs-from, .irs-to, .irs-single, .irs-bar{
      background-color : #00b5ad !important;
      font:Lato, 'Helvetica Neue', Arial, Helvetica, sans-serif !important;
    }
    .control-label{
      display: block;
      margin: 0 0 .28571429rem 0;
      color: rgba(0,0,0,.87);
      font-size: .92857143em;
      font-weight: 700;
      text-transform: none;
    }
    .cohort-description{
      max-width: calc(100vw - 680px);
      padding: 10px;
    }.ui.toggle.checkbox input:checked~label:before {
    background-color: #0dc5c1 !important;
    }
    .ui.vertical.menu .bottom-menu{
      position: absolute;
      bottom: 0; width:149px
    }
    .ui.vertical.menu > .bottom-menu > .item:before {
    position: absolute;
    content: '';
    top: 0;
    left: 0;
    width: 100%;
    height: 1px;
    background: rgba(34, 36, 38, .1);
    display:block!important;
    }
    .icon.collapsed{

    margin-left: -0.5em;
    margin-right: 0;

    }
    .profile-picker-icon{
    cursor:pointer;
    }
    .picked_profile{
      color:white;
      font-weight:600;
    }
    .right-side-div.visible {
      height: 605px;
      width:25vw;
      right: 0;
      top: 125px;
      background: none;
      z-index: 999;
      position:absolute;
      transition: width 0.5s;
    }
    .right-side-div {
      height: 0;
      width:0;
      right: 0;
      top: 125px;
      background: none;
      z-index: -1;
      transition: width 0.5s;
    }
    .right-side-div.visible > .segment {
      opacity:1;
    position:relative;

    }
    .right-side-div > .segment {
      opacity:0;
    position:absolute;
      transition: none;
    }
    .ui.label .detail {
        background: rgba(0, 0, 0, .1);
    margin: -.5833em -.833em -.5833em .5em;
    padding: .5833em .833em;
    border-radius: 0 .28571429rem .28571429rem 0;
    }
    .item > .ui.label:first-child, .ui.selection.dropdown > .text > .ui.label:first-child{
    margin-left: .78571429rem !important;
    }
    .ui.menu.transition.visible > .item > .ui.label{
      padding:.5833em .833em;
    }
    .waiter-overlay.waiter-fullscreen{
    width: 100vw!important;
    height:100vh!important;
    }
    #refresh-cohort-picker{
    margin-left: 8px;
    margin-right: 10px;
    }
  .lds-ring div {
    border-color: #0dc5c1 transparent transparent transparent;
  }

    "
        )
      )
    ),
    tabItems(
      tabItem(tabName = "explore", parametersExplorationUI("parameters_exploration")),
      tabItem(tabName = "patient", patientExplorerUI("patient_explorer")),
      tabItem(tabName = "cohort_creation", cohortCreationUI("cohort_creator")),
      tabItem(tabName = "cohort_explore", cohortExplorerUI("cohort_explorer")),
      tabItem(tabName = "dev_settings", devSettingsUI("dev_settings")),
      tabItem(tabName = "user_profile", userProfileUI("user_profile"))

    ),
    style = "padding:20px 0px;"
  )
))

w <- Waiter$new(html = "Loading...", color = "rgb(0,163,156)")

server <- function(input, output, session) {


  DATABASE_CREATED <- FALSE

  session_db <- function(w=NULL,init_func=FALSE) {
    if(xor(DATABASE_CREATED,init_func)){
      tryCatch({
        DBI::dbListTables(session$userData$database_keepalive)
      }, error = function(e) {
        print("Lost of connection database link, try to reconnect (Error)")
        print(e)
        session$userData$database_keepalive<- tryCatch(
          {print("Reconnecting")
            connect_to_mimic()},
          error = function(e) {
            print("Failed")
            if(!is.null(w)){
              w$update(html = tagList(
                span(
                  "Error connecting to the database. Are you sure your Postgres database is running ? That your credentials are correct ?"
                ),
                tags$br(),
                tags$b("Full Stack Trace"),
                tags$br(),
                tags$code(iconv(e$message, "latin1", "UTF-8"), style = "max-width: 80vw;text-wrap: wrap;")
              ))
            }
            NULL
          }
        )
      }, warning = function(w) {
        print("Lost of database connection (Warning)")
      })

      return(session$userData$database_keepalive)
    } else{
      NULL
    }

  }

  ############################################
  ### HANDLE INTERACTIVE MODE IF ACTIVATED ###
  ############################################


  if (CONFIG$INTERACTIVE) {
    application_mode <- eventReactive({
      input$init_demo
      input$run_demo
      input$run_hosted
      1
    }, {
      if (input$init_demo) {
        "INIT_DEMO"
      } else if (input$run_demo) {
        "DEMO"
      } else if (input$run_hosted) {
        "HOSTED"
      }

    })
  } else{
    application_mode <- reactive(CONFIG$APPLICATION_MODE)
  }


  ########################################
  ### SET UP FIRST DATABASE CONNECTION ###
  ########################################
  observe({
  if (CONFIG$INTERACTIVE) {
    req(application_mode())
    w$show()
    if (application_mode() == "INIT_DEMO" ||
        application_mode() == "DEMO") {
      CONFIG$DATABASE_MODE <<- "DEMO"
    } else{
      CONFIG$DATABASE_MODE <<- "HOSTED"
    }
    CONFIG$APPLICATION_MODE <<- application_mode()
    hide("introduction")
    tryCatch({
      session$userData$database_keepalive <- connect_to_mimic(w)
      if (CONFIG$APPLICATION_MODE == "INIT_DEMO" && DATABASE_CREATED == FALSE) {

        create_demo_data_structure(session_db)
        define_postgres_function(session_db)
        waiter$update(html = tagList(
          spin_pixel(),
          "Importing data from 'demo' folder (demo-mode)"
        ))
        import_demo_data(session_db, w)
        waiter$update(html = tagList(
          spin_pixel(),
          "Creating application schema (demo-mode)"
        ))
        create_demo_configuration(session_db)
        waiter$update(html = tagList(spin_pixel(), "Generating database views (demo-mode)"))
        create_demo_db_views(session_db)
        waiter$update(
          html = tagList(
            spin_pixel(),
            "Demo database sucessfully populated for demo-mode, starting app..."
          )
        )
      }
      DATABASE_CREATED <<- TRUE
    }, error = function(e) {
      w$update(html = tagList(
        span(
          "Error connecting to the database. Are you sure your Postgres database is running ? That your credentials are correct ?"
        ),
        tags$br(),
        tags$b("Full Stack Trace"),
        tags$br(),
        tags$code(iconv(e$message, "latin1", "UTF-8"), style = "max-width: 80vw;text-wrap: wrap;")
      ))
      NULL
    })
  } else{
    hide("introduction")
    w$show()
    tryCatch({
      session$userData$database_keepalive <- connect_to_mimic(w)
      if (CONFIG$APPLICATION_MODE == "INIT_DEMO" && DATABASE_CREATED == FALSE) {
        create_demo_data_structure(session_db)
        define_postgres_function(session_db)
        w$update(html = tagList(
          spin_pixel(),
          "Importing data from 'demo' folder (demo-mode)"
        ))
        import_demo_data(session_db, w)
        w$update(html = tagList(
          spin_pixel(),
          "Creating application schema (demo-mode)"
        ))
        create_demo_configuration(session_db)
        w$update(html = tagList(spin_pixel(), "Generating database views (demo-mode)"))
        create_demo_db_views(session_db)
        w$update(
          html = tagList(
            spin_pixel(),
            "Demo database sucessfully populated for demo-mode, starting app..."
          )
        )
      }
      DATABASE_CREATED <<- TRUE
    }, error = function(e) {
      w$update(html = tagList(
        span(
          "Error connecting to the database. Are you sure your Postgres database is running ? That your credentials are correct ?"
        ),
        tags$br(),
        tags$b("Full Stack Trace"),
        tags$br(),
        tags$code(iconv(e$message, "latin1", "UTF-8"), style = "max-width: 80vw;text-wrap: wrap;")
      ))
      NULL
    })
  }
})


  profile_list_data <- reactive({
    req(application_mode(),session_db())
    input$update_profile
    dplyr::tbl(session_db(), in_schema("public", "users")) %>% collect()
  })

  profile_list <- reactive({
    profile_list <-
      as.list(
        profile_list_data() %>% select(user_id, user_name) %>%
          pivot_wider(names_from = user_name, values_from = user_id)
      )
    c(list("Default profile" = 0), profile_list)
  })

  session$userData$selected_profile <- reactive({
    req(application_mode())
    raw_data <- profile_list_data() %>% filter(user_id == !!input$profile_picker)
    profile <- c()
    if (is.null(input$profile_picker) ||
        input$profile_picker == 0) {
      profile$user_id <- 0
      profile$user_name <- "Default Profile"
      profile$user_description <- "Application default profile"
      profile$user_itemids <- list()
      profile$user_cohorts <- list()
    } else{
      profile$user_id <- raw_data[["user_id"]]
      profile$user_name <- raw_data[["user_name"]]
      profile$user_description <- raw_data[["user_description"]]
      profile$user_itemids <- as.list(strsplit(raw_data[["user_itemids"]], ",", fixed =
                                                 TRUE)[[1]])
      profile$user_cohorts <- as.list(strsplit(raw_data[["user_cohorts"]], ",", fixed =
                                                 TRUE)[[1]])
    }
    profile
  })

  output$picked_profile <- renderText({
    req(input$profile_picker, application_mode())
    if (input$profile_picker == "0") {
      "Default profile"
    } else{
      as.character(names(which(
        !!input$profile_picker == profile_list()
      )))
    }
  })

  http_get_args <- reactive({
    print("Update clientData$url_search")
    parseQueryString(session$clientData$url_search)
  })

  observe({
    customUpdateSelectInput(
      session,
      inputId = "profile_picker",
      label = "Choose your profile",
      choices =  profile_list(),
      selected = 0
    )
    print("Updated profile_list")
  })



  observe({
    if (!is.null(isolate(http_get_args()[['subject_id']])) &
        !is.null(isolate(http_get_args()[['hadm_id']]))) {
      req(application_mode())
      runjs(
        paste0(
          "$.tab('change tab', 'shiny-tab-patient');
          $('.ui.menu').trigger('change');
          $('#uisidebar .item:not([",
          'data-value="patient"]',
          ")').remove()"
        )
      )

      patientExplorerServer("patient_explorer",
                            session_db)
      w$hide()
    } else{
      req(application_mode())
      if(isTruthy(isolate(session_db(w)))){
        tryCatchLog({
          print("Loading server")
          print(CONFIG)


          ####
          #     LOAD DISTRIBUTED API IN THE APP
          ####

          distinct_events <<-
            dplyr::tbl(isolate(session_db()), in_schema("public", "distinct_events")) %>%
            select(label, category, itemid) %>%
            collect() %>%
            mutate(name = glue::glue("{category} - {label} ({itemid})"),
                   value = itemid) %>%
            select(name, value)


          search_api_distinct_events <- function(data, q, database) {
            if (!is.null(data)) {
              data %>%
                {
                  if(q != "")
                    filter(.,str_detect(name,  coll(q,TRUE)))
                  else
                    .
                } %>% head(100)
            }
          }

          search_api_url_distinct_events <<-
            custom_register_search(session, distinct_events, search_api_distinct_events, "search_api_distinct_events")

          icd_data <-
            dplyr::tbl(isolate(session_db()), in_schema("mimiciv_hosp", "d_icd_diagnoses")) %>%
            select("icd_code", "icd_version", "long_title") %>%
            collect() %>%
            transmute(name=paste0(icd_code,"v",trimws(icd_version)," - ",long_title),
                      value=paste(icd_version,trimws(icd_code)))

          search_api_icd_data <- function(data, q, database) {
            if (!is.null(data)) {
              data %>%
                {
                  if(q != "")
                    filter(.,str_detect(name,  coll(q,TRUE)))
                  else
                    .
                } %>% head(100)
            }
          }

          search_api_url_icd_data <<-
            custom_register_search(session, icd_data, search_api_icd_data, "search_api_icd_data")


          event_colors <<-
            dplyr::tbl(isolate(session_db()), in_schema("public", "distinct_events")) %>%
            select("linksto", "itemid") %>%
            collect() %>%
            mutate(linksto = as.factor(linksto)) %>%
            pivot_wider(names_from = itemid, values_from = linksto)

          ####
          #     API LOADED
          ####

          parametersExplorationServer("parameters_exploration", isolate(session_db))
          patientExplorerServer("patient_explorer", isolate(session_db))
          cohortCreationServer("cohort_creator", isolate(session_db))
          cohortExplorerServer("cohort_explorer", isolate(session_db))
          devSettingsServer("dev_settings", isolate(session_db))
          userProfileServer("user_profile", isolate(session_db))
          output$demo_top_label <- renderUI({
            if (CONFIG$DATABASE_MODE == "DEMO"){
              tags$div("Demo version", style = "color:green;font-weight:bold;background-color:white;")
            }else{
              tags$div("")
            }
          })
          w$hide()
        })
      }

    }

  })

}
shinyApp(ui, server)
