library(shinydashboard)
library(cutpointr)
library(OptimalCutpoints)
library(readr)
library(ggplot2)

header <- dashboardHeader(title = "cutpointr")

sidebar <- dashboardSidebar(width = 300, sidebarMenu(
    menuItem("Data Upload", tabName = "data", icon = icon("table")),
    menuItem("Calculate Cutpoints", tabName = "cutpoints", icon = icon("cut")),
    menuItem("Bootstrapping", tabName = "bootstrapping", icon = icon("repeat")),
    menuItem("Simulation", tabName = "simulation", icon = icon("bar-chart"))
    # menuItem("About", tabName = "about", icon = icon("info"))
))

data_tab <- tabItem(tabName = "data",
                    fluidRow(
                        box(
                            radioButtons("datatype", "",
                                         choices = list("Example data", "Upload data"),
                                         selected = "Example data"),
                            width = 6,
                            conditionalPanel(condition = "input.datatype == 'Upload data'",
                                             fileInput('file1', 'Choose CSV File',
                                                       accept=c('text/csv',
                                                                'text/comma-separated-values,text/plain',
                                                                '.csv'))
                            ),
                            uiOutput("choose_outcome"),
                            uiOutput("choose_predictor"),
                            uiOutput("choose_pos_class"),
                            uiOutput("choose_direction")
                            # uiOutput("choose_group")

                            # textOutput("pred"),
                            # textOutput("out"),
                            # textOutput("dat"),
                            # textOutput("oc"),
                            # textOutput("oc2"),
                            # textOutput("oc_metric_optimalcutpoints"),
                            # textOutput("unique_class"),
                            # textOutput("neg_class")
                        ),
                        box(
                            width = 6,
                            plotOutput("rocplot")
                        ),
                        box(
                            dataTableOutput('contents'),
                            width = 12
                        )
                    )
)

cutpoints_tab <- tabItem(tabName = "cutpoints",
                    fluidRow(
                        box(
                            selectInput("method", "Method",
                                         choices = list("minimize_metric",
                                                        "maximize_metric",
                                                        "oc_youden_kernel",
                                                        "oc_youden_normal"),
                                                        # "oc_OptimalCutpoints"),
                                         selected = "maximize_metric"),
                            width = 6,
                            conditionalPanel(condition = "input.method == 'oc_OptimalCutpoints'",
                                             selectInput('oc_metric', 'Metric for OptimalCutpoints',
                                                         choices = list("ROC01",
                                                                        "Youden",
                                                                        "PROC01"),
                                                         selected = "Youden")
                            ),
                            # conditionalPanel(condition = "input.method != 'optimal.cutpoints'",
                                             selectInput('metric', 'Metric',
                                                         choices = list("accuracy",
                                                                        "youden",
                                                                        "sum_sens_spec",
                                                                        "cohens_kappa",
                                                                        "abs_d_sesp",
                                                                        "odds_ratio"),
                                                         selected = "sum_sens_spec")
                            # )
                            # conditionalPanel(condition = "input.method == 'minimize_metric'",
                            #                  selectInput('metric', 'Metric',
                            #                              choices = list("accuracy",
                            #                                             "youden",
                            #                                             "sum_sens_spec",
                            #                                             "cohens_kappa",
                            #                                             "abs_d_sesp",
                            #                                             "odds_ratio"),
                            #                              selected = "abs_d_sesp")
                            # )
                        ),
                        box(
                            infoBoxOutput("optimal_cutpoint")
                        ),
                        box(
                            width = 12,
                            plotOutput("distplot")
                        ),
                        box(
                            width = 12,
                            plotOutput("metricplot")
                        )
                    )
)

bootstrap_tab <- tabItem(tabName = "bootstrapping",
                    fluidRow(
                        box(
                            sliderInput("boot_runs", "Bootstrap repeats",
                                        min = 0, max = 1000, value = 0,
                                        step = 1, round = TRUE
                            ),
                            width = 6
                        ),
                        box(
                            width = 12,
                            plotOutput("bootplot_cut")
                        ),
                        box(
                            width = 12,
                            plotOutput("bootplot_metric")
                        )
                    )
)

sim_tab <- tabItem(tabName = "simulation",
                    fluidRow(
                        box(
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
                            numericInput("sim_repeats", "Number of simulation runs",
                                        value = 100, min = 10, max = 10000,
                                        step = 1
                            ),
                            width = 6
                        ),
                        box(
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

body <- dashboardBody(
    tabItems(
        data_tab,
        cutpoints_tab,
        bootstrap_tab,
        sim_tab
    )
)

dashboardPage(header, sidebar, body)
