# BSD_2_clause

#############################################################################
# Define the header and sidebar (disabled)
header <- dashboardHeader(disable = TRUE)
sidebar <- dashboardSidebar(disable = TRUE)

#############################################################################
# Define the page(s) with dashboardBody
body <- dashboardBody(fluidPage(
  # theme = shinythemes::shinytheme("yeti"),
  div(class = "outer",
    shinyjs::useShinyjs(),
    tags$style(appCSS),
    tags$head(
      HTML("<link href='https://fonts.googleapis.com/css?family=Open+Sans:300,400'
           rel='stylesheet' type='text/css'>"),
      HTML('<link rel="icon" type="image/png" href="favicon-32x32.png" sizes="32x32" />'),
      tags$script(src = "google_analytics.js"),
      tags$style(HTML(readLines("www/custom_styles.css"))),
      tags$script(src = "enter_button.js")
    ),

    br(),
    fluidRow(
      div(
        id = "spacer",
        br(), br(), br(), br(), br(), br(),
        div(
          id = "esadocs_large",
          tags$a(
            href="https://esadocs.cci-dev.org",
            tabindex = "-1",
            img(src = "ESAdocs_search_lg.svg",
                height = "140px")
          )
        )
      )
    ),
    fluidRow(
      column(1,
        hidden(
          div(id = "esadocs_small",
            tags$a(href="https://esadocs.cci-dev.org",
              img(src = "ESAdocs_search.svg",
                  height = "80px")
            )
          )
        )
      ),
      column(10,
        fluidRow(
          column(2),
          column(8,
            fluidRow(
              div(
                class = "input-group",
                style = "padding-top:20px",
                textInput(
                  inputId = "main_input",
                  label = NULL,
                  placeholder = "Use quotes for better performance",
                  width = "100%"),
                span(
                  class = "input-group-btn",
                  withBusyIndicatorUI(
                    actionButton(
                      inputId = "search",
                      label = NULL,
                      icon = icon("search"),
                      style = "primary",
                      style="font-size: 150%;
                             color: white;
                             background-color: #0E3670;
                             border-color: #2e6da4"
                    )
                  )
                )
              )
            ),
            fluidRow(
              helpText(htmlOutput("n_docs"))
            )
          ),
          column(2,
            fluidRow(
              actionButton(
                inputId = "help",
                label = "Help",
                icon = icon("question")
              )
            ),
            fluidRow(
              actionButton(
                inputId = "tog_extras",
                label = "Filters",
                # icon = icon("filter"),
                size = "small",
                type = "toggle",
                value = FALSE
              )
            )
          )
        ),
        fluidRow(textOutput("testing_msg")),
        fluidRow(
          hidden(
            div(id = "extras",
              # hr(style = "padding-above:2px; margin:1px"),
              column(2,
                div(class = "slim",
                  selectInput("show_n",
                              label = NULL,
                              choices = list(
                                "5 hits/page" = 5,
                                "10 hits/page" = 10,
                                "20 hits/page" = 20,
                                "50 hits/page" = 50,
                                "100 hits/page" = 100),
                              width = "95%",
                              selected = 10,
                              multiple = FALSE)
                )
              ),
              column(1,
                selectInput(
                  inputId = "sortby",
                  label = NULL,
                  choices = list(
                    "Sort" = "score",
                    "Rev. Score" = "rev_score",
                    "Date" = "date",
                    "Rev. Date" = "rev_date"
                  ),
                  selected = "score",
                  width = "125%"
                )
              ),
              column(3,
                dateRangeInput(
                  "date_filt",
                  label = NULL,
                  start = as.Date("1967-01-01"),
                  end = Sys.Date(),
                  width = "95%"
                )
              ),
              column(2,
                selectInput(
                  "type_filt",
                  label = NULL,
                  choices = list(
                    "All document types" = "all",
                    "Candidates" = "candidate",
                    "Conserv. Agreements" = "conserv_agmt",
                    "Consultation" = "consultation",
                    "Critical Habitat" = "critical_habitat",
                    "Federal Register" = "federal_register",
                    "Miscellaneous" = "misc",
                    "Policies" = "policy",
                    "Recovery Plan" = "recovery_plan",
                    "5-year review" = "five_year_review",
                    "7(a)(1)" = "section_7a1"
                  ),
                  width = "95%"
                )
              ),
              column(2,
                selectInput(
                  "min_score",
                  label = NULL,
                  choices = list(
                    "Min score = 0.1" = 0.1,
                    "Min score = 0.5" = 0.5,
                    "Min score = 1" = 1,
                    "Min score = 5" = 5,
                    "Min score = 10" = 10,
                    "No filter (0)" = 0
                  ),
                  width = "95%"
                )
              ),
              column(2,
                selectInput(
                  "max_hits",
                  label = NULL,
                  choices = list(
                    "Max hits = 100" = 100,
                    "Max hits = 500" = 500,
                    "Max hits = 1000" = 1000,
                    "Max hits = 5000" = 5000,
                    "Max hits = 10000" = 10000
                  ),
                  width = "95%",
                  selected = 500
                )
              )
            )
          )
        ),

        fluidRow(
          column(8,
            fluidRow(
              column(3,
                textOutput("n_hits")
              ),
              column(3,
                textOutput("n_filt_hit")
              ),
              column(2),
              column(4,
                hidden(
                  tipify(
                    downloadButton("get_results", "Download"),
                    title = "Download an Excel file of these results.",
                    placement = "right"
                  )
                )
              )
            ),
            hidden(uiOutput("hits"))
          ),
          column(4,
            br(), br(),
            fluidRow(
              uiOutput("summary_figs", height = "300px")
            )
          )
        ),
        fluidRow(
          column(8,
            br(),
            div(
              id = "nextprev",
              style = "width:50%; margin:0 auto;",
              div(style = "display: inline-block",
                hidden(actionButton("prevButton",
                         label = "< Previous",
                         style = "default",
                         size = "small"))
              ),
              hidden(div(id = "res_txt",
                         "Results pages",
                         style = "font-weight:bold; display:inline-block")),
              div(style = "display: inline-block",
                hidden(actionButton("nextButton",
                         label = "Next >",
                         style = "default",
                         size = "small"))
              )
            )
          ),
          column(4)
        ),
        fluidRow(
          br(), br()
        )
      ),
      column(1,
        hidden(div(
          id = "top_dow",
          style = "position: absolute; right:10px",
          tags$a(href="http://www.defenders.org",
            img(src = "DOW_logo_small.png")
          )
        ))
      ),
      br()
    ),
    fluidRow(
      div(
        id = "pad_foot",
        br(), br(), br(), br(), br(), br()
      ),
      hidden(div(
        br(), br(), br()
      ))
    ),
    fluidRow(
      column(5),
      column(1,
        HTML('
          <a href="http://defenders.org">
          <img style="vertical-align:middle" src="DOW_logo_small.png" height="60"></a>
        ')
      ),
      column(1,
        HTML('
          <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">
          <img alt="Creative Commons License" style="border-width:0;padding-top:15px" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a>
        ')
      ),
      column(5)
    ),
    fluidRow(
      column(1),
      column(10,
        div(
          style = "text-align:center",
          HTML('<footer>
            <br />
            <p>This <span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/InteractiveResource" rel="dct:type">work</span>
            by <a xmlns:cc="http://creativecommons.org/ns" href="http://defenders.org" property="cc:attributionName" rel="cc:attributionURL">Defenders of Wildlife</a>
            is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.</p>
            <br />
          </footer>'),
          br()
        )
      ),
      column(1)
    )
  )
))

# dashboardPage(header, sidebar, body, skin="blue")
body
