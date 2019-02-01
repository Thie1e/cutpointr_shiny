library(shinydashboard)
library(cutpointr)
library(readr)
library(ggplot2)
library(tibble)
library(dplyr)
library(markdown)

header <- dashboardHeader(title = "cutpointr")

sidebar <- dashboardSidebar(width = 300, sidebarMenu(
    menuItem("Data Upload", tabName = "data", icon = icon("table")),
    menuItem("Calculate Cutpoints", tabName = "cutpoints", icon = icon("cut")),
    menuItem("Bootstrapping", tabName = "bootstrapping", icon = icon("repeat")),
    menuItem("Simulation", tabName = "simulation", icon = icon("bar-chart")),
    menuItem("About", tabName = "about", icon = icon("info"))
))

data_tab <- tabItem(tabName = "data",
                    fluidRow(
                        box(title = "Introduction", solidHeader = TRUE,
                            status = "warning", width = 8,
                            helpText("Select tasks in the menu on the left.
                                     Start with uploading data or get to know
                                     the app by using the example data that is
                                     already loaded.")),
                        box(
                            title = "Data Upload",
                            solidHeader = TRUE, status = "warning",
                            radioButtons("datatype", "",
                                         choices = list("Example data", "Upload data"),
                                         selected = "Example data"),
                            width = 12,
                            conditionalPanel(condition = "input.datatype == 'Upload data'",
                                             fileInput('file1', 'Choose CSV File',
                                                       accept=c('text/csv',
                                                                'text/comma-separated-values,text/plain',
                                                                '.csv')),
                                             # checkboxInput('header', 'Variable names in first row', TRUE),
                                             selectInput('sep', 'Field Separator',
                                                         choices = list(`,` = ",",
                                                                        `.` = ".",
                                                                        `;` = ";",
                                                                        Tab = "\t")
                                                         ),
                                             selectInput('quote', 'Quoting character',
                                                         choices = list("\"",
                                                                        "\'"),
                                                         selected = "\""),
                                             # selectInput('dec', 'Decimal point',
                                             #             choices = list(".",
                                             #                            ","),
                                             #             selected = "."),
                                             helpText("Blank fields are always interpreted as missing values."),
                                             textInput('na.strings', 'String to be interpreted as NA',
                                                       value = "NA"),
                                             numericInput('skip',
                                                          'Number of lines to skip before beginning to read data',
                                                          value = 0, min = 0)
                                             # numericInput('nrows',
                                             #              'Maximum number of rows to read in',
                                             #              value = Inf, min = 1)
                                             # numericInput('row.names',
                                             #              'If any, which column includes the row names? NULL if none.',
                                             #              value = NULL, min = 1)
                            )
                        ),
                        box(
                            title = "Variable selection", width = 12,
                            solidHeader = TRUE, status = "warning",
                            uiOutput("choose_outcome"),
                            uiOutput("choose_predictor"),
                            uiOutput("choose_group"),
                            uiOutput("choose_pos_class"),
                            uiOutput("choose_direction"),

                            textOutput("method"),
                            textOutput("metric"),
                            textOutput("oc")
                        ),
                        box(
                            width = 12,
                            title = paste("Predictor distribution"),
                            solidHeader = TRUE, status = "warning",
                            plotOutput("xplot"),
                            downloadButton('download_xplot', 'Download Plot')
                        ),
                        # box(
                        #     width = 6,
                        #     title = "ROC curve",
                        #     solidHeader = TRUE, status = "warning",
                        #     plotOutput("rocplot"),
                        #     downloadButton('download_rocplot', 'Download Plot')
                        # ),
                        # box(
                        #     width = 6,
                        #     title = "Precision-Recall curve",
                        #     solidHeader = TRUE, status = "warning",
                        #     plotOutput("prplot"),
                        #     downloadButton('download_prplot', 'Download Plot')
                        # ),
                        box(
                            title = "Outcome summary statistics",
                            solidHeader = TRUE, status = "warning",
                            width = 12,
                            tableOutput("outcome_table")
                        ),
                        box(
                            title = "Predictor summary statistics",
                            solidHeader = TRUE, status = "warning",
                            width = 12,
                            h4("Overall"),
                            tableOutput("pred_table"),
                            h4("Per class"),
                            tableOutput("pred_table_byclass")
                        ),
                        box(
                            title = "Data",
                            solidHeader = TRUE, status = "warning",
                            dataTableOutput('contents'),
                            width = 12
                        )
                    )
)

cutpoints_tab <- tabItem(tabName = "cutpoints",
                    fluidRow(
                        box(
                            title = "Cutpoint calculation",
                            solidHeader = TRUE, status = "warning",
                            width = 6,
                            selectInput("method", "Method",
                                         choices = list("minimize_metric",
                                                        "maximize_metric",
                                                        "minimize_boot_metric",
                                                        "maximize_boot_metric",
                                                        # "minimize_gam_metric",
                                                        # "maximize_gam_metric",
                                                        # "minimize_loess_metric",
                                                        # "maximize_loess_metric",
                                                        # "minimize_spline_metric",
                                                        # "maximize_spline_metric",
                                                        "oc_youden_kernel",
                                                        "oc_youden_normal"),
                                         selected = "maximize_metric"),
                            conditionalPanel(condition = "input.method == 'maximize_boot_metric' |
                                                 input.method == 'minimize_boot_metric'",
                                             helpText("Warning: When switching to the bootstrap tab, the
                                             combination with a bootstrapped cutpoint estimation leads to
                                             an outer an and inner bootstrap loop, which may be very time-consuming."),
                                             numericInput('boot_cut', 'Number of bootstrap repetitions in cutpoint estimation',
                                                          value = 50, min = 10, max = 100000, step = 10)
                                                         ),
                            selectInput('metric', 'Metric',
                                        choices = list("accuracy",
                                                       "youden",
                                                       "sum_sens_spec",
                                                       "cohens_kappa",
                                                       "abs_d_sens_spec",
                                                       "odds_ratio"),
                                        selected = "sum_sens_spec")
                            # uiOutput("choose_pos_class"),
                            # uiOutput("choose_direction")
                        ),
                        box(
                            solidHeader = TRUE, status = "warning",
                            infoBoxOutput("optimal_cutpoint")
                        )
                    ),
                    fluidRow(
                        box(
                            width = 12,
                            title = "Metrics at optimal cutpoint",
                            solidHeader = TRUE, status = "warning",
                            tableOutput("optimal_cutpoint_table")
                        ),
                        box(
                            width = 12,
                            plotOutput("metricplot"),
                            downloadButton('download_metricplot', 'Download Plot')
                        ),
                        box(
                            width = 6,
                            title = "ROC curve",
                            solidHeader = TRUE, status = "warning",
                            plotOutput("rocplot"),
                            downloadButton('download_rocplot', 'Download Plot')
                        ),
                        box(
                            width = 6,
                            title = "Precision-Recall curve",
                            solidHeader = TRUE, status = "warning",
                            plotOutput("prplot"),
                            downloadButton('download_prplot', 'Download Plot')
                        )
                    )
)

bootstrap_tab <- tabItem(tabName = "bootstrapping",
                    fluidRow(
                        box(
                            title = "Bootstrap parameters",
                            solidHeader = TRUE, status = "warning",
                            sliderInput("boot_runs", "Bootstrap repeats",
                                        min = 0, max = 1000, value = 0,
                                        step = 1, round = TRUE
                            ),
                            numericInput("boot_seed", "Seed (for reproducing the result)",
                                        min = 0, max = 999999, value = 100,
                                        step = 1
                            ),
                            width = 6
                        ),
                        conditionalPanel(condition = "input.boot_runs > 0",
                                         box(
                                             title = "Bootstrap summary statistics",
                                             solidHeader = TRUE, status = "warning",
                                             width = 12,
                                             tableOutput("boot_table"),
                                             downloadButton('download_boot_table', 'Download table as CSV-file')
                                         ),
                                         box(
                                             width = 12,
                                             plotOutput("bootplot_cut"),
                                             downloadButton('download_bootplot_cut', 'Download Plot')
                                         ),
                                         box(
                                             width = 12,
                                             plotOutput("bootplot_metric"),
                                             downloadButton('download_bootplot_metric', 'Download Plot')
                                         )
                        )
                    )
)

sim_tab <- tabItem(tabName = "simulation",
                    fluidRow(
                        box(
                            title = "Negative class",
                            solidHeader = TRUE, status = "warning",
                            numericInput("sim_n_cont", "Number of controls",
                                        value = 100, min = 2, max = 10000,
                                        step = 1
                            ),
                            numericInput("sim_m_cont", "Mean value of controls",
                                        value = 100, min = -10000, max = 10000,
                                        step = 0.01
                            ),
                            numericInput("sim_sd_cont", "Standard deviation of value of controls",
                                        value = 15, min = -10000, max = 10000,
                                        step = 0.01
                            )
                        ),
                        box(
                            title = "Positive class",
                            solidHeader = TRUE, status = "warning",
                            numericInput("sim_n_dis", "Number of cases",
                                        value = 15, min = 2, max = 10000,
                                        step = 1
                            ),
                            numericInput("sim_m_dis", "Mean value of cases",
                                        value = 130, min = -10000, max = 10000,
                                        step = 0.01
                            ),
                            numericInput("sim_sd_dis", "Standard deviation of value of cases",
                                        value = 15, min = -10000, max = 10000,
                                        step = 0.01
                            )
                        ),
                        box(
                            title = "Simulation parameters",
                            solidHeader = TRUE, status = "warning",
                            numericInput("sim_repeats", "Number of simulation runs",
                                        value = 100, min = 10, max = 10000,
                                        step = 1
                            ),
                            numericInput("sim_seed", "Seed (for reproducing the result)",
                                        value = 100, min = 1, max = 999999,
                                        step = 1
                            ),
                            width = 6
                        ),
                        box(
                            title = "Optimal cutpoint",
                            solidHeader = TRUE, status = "warning",
                            selectInput("sim_method", "Method",
                                         choices = list("minimize_metric",
                                                        "maximize_metric",
                                                        "oc_youden_normal",
                                                        "oc_youden_kernel"),
                                         selected = "maximize_metric"),
                            width = 6,
                            conditionalPanel(condition = "input.sim_method == 'oc_OptimalCutpoints'",
                                             selectInput('sim_oc_metric', 'Metric for OptimalCutpoints',
                                                         choices = list("ROC01",
                                                                        "Youden",
                                                                        "PROC01"),
                                                         selected = "Youden")
                            ),
                            conditionalPanel(condition = "input.sim_method == 'maximize_metric'",
                                             selectInput('sim_metric', 'Metric',
                                                         choices = list("accuracy",
                                                                        "youden",
                                                                        "sum_sens_spec",
                                                                        "cohens_kappa",
                                                                        "abs_d_sesp",
                                                                        "odds_ratio"),
                                                         selected = "sum_sens_spec")
                            ),
                            conditionalPanel(condition = "input.sim_method == 'minimize_metric'",
                                             selectInput('sim_metric', 'Metric',
                                                         choices = list("accuracy",
                                                                        "youden",
                                                                        "sum_sens_spec",
                                                                        "cohens_kappa",
                                                                        "abs_d_sesp",
                                                                        "odds_ratio"),
                                                         selected = "abs_d_sesp")
                            )
                        ),
                        box(
                            width = 12,
                            plotOutput("simplot_distrs")
                        ),
                        box(
                            width = 12,
                            plotOutput("simplot_cutvar")
                        )
                    )
)

about_tab <- tabItem(tabName = "about",
                    fluidRow(
                        box(
                            includeMarkdown("about.md")
                        )
                    )
)

body <- dashboardBody(
    tabItems(
        data_tab,
        cutpoints_tab,
        bootstrap_tab,
        sim_tab,
        about_tab
    )
)

dashboardPage(header, sidebar, body)
