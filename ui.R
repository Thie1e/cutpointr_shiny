library(shinydashboard)
library(cutpointr)
library(readr)
library(ggplot2)
library(tibble)
library(dplyr)
library(purrr)
library(foreach)
library(markdown)
library(tidyr)
library(rmarkdown)
library(shinycssloaders)

header <- dashboardHeader(title = "cutpointr")

sidebar <- dashboardSidebar(width = 300, sidebarMenu(
    menuItem("Data Upload", tabName = "data", icon = icon("table")),
    menuItem("Estimate Cutpoints", tabName = "cutpoints", icon = icon("cut")),
    menuItem("Bootstrapping", tabName = "bootstrapping", icon = icon("repeat")),
    menuItem("Sample Size Planning", tabName = "simulation", icon = icon("bar-chart")),
    menuItem("CI Width and Coverage Simulation", tabName = "ci_simulation", icon = icon("bar-chart")),
    menuItem("About", tabName = "about", icon = icon("info"))
))

data_tab <- tabItem(tabName = "data",
                    fluidRow(
                        box(title = "Introduction", solidHeader = TRUE,
                            status = "warning", width = 12,
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
                        box(title = "Info: Estimate Cutpoints", solidHeader = TRUE,
                            status = "warning", width = 12,
                            includeMarkdown('cutpoints_tab_introduction.md')
                            ),
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
                                             an outer an and inner bootstrap loop, which may be very time-consuming.
                                             This method does not plot metric values vs. cutpoints in the pane
                                             below."),
                                             numericInput('boot_cut', 'Number of bootstrap repetitions in cutpoint estimation',
                                                          value = 50, min = 10, max = 100000, step = 10)
                            ),
                            conditionalPanel(condition = "input.method == 'oc_youden_kernel' |
                                                 input.method == 'oc_youden_normal'",
                                             helpText("This method does not plot metric values vs. cutpoints in the pane
                                             below.")
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
                        )
                        ### Funktioniert nicht mit mehreren Cutpoints, also
                        ### nicht mit Subgroups
                        # box(
                        #     solidHeader = TRUE, status = "warning",
                        #     title = "Result",
                        #     infoBoxOutput("optimal_cutpoint")
                        # )
                    ),
                    fluidRow(
                        box(
                            width = 12,
                            title = "Metrics and optimal cutpoint",
                            solidHeader = TRUE, status = "warning",
                            tableOutput("optimal_cutpoint_table")
                        ),
                        box(
                            conditionalPanel(condition = "input.method == 'maximize_metric' |
                                                 input.method == 'minimize_metric'",
                                plotOutput("metricplot"),
                                downloadButton('download_metricplot', 'Download Plot')
                            ),
                            width = 12,
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
                        box(title = "Info: Bootstrapping", solidHeader = TRUE,
                            status = "warning", width = 12,
                            includeMarkdown('bootstrapping_tab_introduction.md')
                        ),
                        box(
                            title = "Bootstrap parameters",
                            solidHeader = TRUE, status = "warning",
                            numericInput("boot_runs", "Bootstrap repeats (30 to 10,000)",
                                        min = 30, max = 10000, value = 300,
                                        step = 1
                            ),
                            numericInput("boot_seed", "Seed (for reproducing the result)",
                                        min = 0, max = 999999, value = 1234,
                                        step = 1
                            ),
                            actionButton("start_bootstrap", "Start"),
                            width = 6
                        ),
                        conditionalPanel(condition = "input.boot_runs > 0",
                                         box(
                                             title = "Bootstrap summary statistics",
                                             solidHeader = TRUE,
                                             status = "warning",
                                             width = 12,
                                             tableOutput("boot_table") %>%
                                                 withSpinner(color="#0dc5c1"),
                                             downloadButton('download_boot_table', 'Download table as CSV-file')
                                         ),
                                         box(
                                             title = "Distribution of bootstrapped cutpoints",
                                             status = "warning",
                                             solidHeader = TRUE,
                                             width = 12,
                                             plotOutput("bootplot_cut") %>%
                                                 withSpinner(color="#0dc5c1"),
                                             downloadButton('download_bootplot_cut', 'Download Plot')
                                         ),
                                         box(
                                             title = "Out-of-bag distribution of the selected metric",
                                             status = "warning",
                                             solidHeader = TRUE,
                                             width = 12,
                                             plotOutput("bootplot_metric") %>%
                                                 withSpinner(color="#0dc5c1"),
                                             downloadButton('download_bootplot_metric', 'Download Plot')
                                         )
                        )
                    )
)

sim_tab <- tabItem(tabName = "simulation",
                   # box(title = "Info: CI Width Simulation / Sample Size Planning",
                   #     solidHeader = TRUE,
                   #     status = "warning",
                   #     width = 9,
                   #     helpText("You can define distributions for both classes here
                   #              and simulate the cutpoint estimation. Pick the
                   #              number of repetitions and the method for cutpoint
                   #              estimation below. The chosen distributions
                   #              are shown in the 'Distribution' tab under 'Simulation
                   #              Output'. To start the
                   #              simulation, switch to the 'Cutpoint Distribution' tab
                   #              and click 'Start simulation'.
                   #              For sample size planning, first set the distributions
                   #              to realistic values and then tweak the group sizes
                   #              until a desired width of the confidence interval
                   #              is reached.")),
                   box(title = "Info: CI Width Simulation / Sample Size Planning",
                       solidHeader = TRUE,
                       status = "warning",
                       width = 12,
                       includeMarkdown('sim_tab_introduction.md')
                   ),
                   box(title = "Simulation",
                       width = 12,
                       status = "warning",
                       solidHeader = TRUE,
                       tabBox(id = "sim_output",
                              width = 12,
                              tabPanel("Distributions",
                                       box(
                                           title = "Negative class",
                                           solidHeader = TRUE,
                                           # status = "warning",
                                           numericInput("sim_n_cont", "Number of controls",
                                                        value = 100, min = 2, max = 10000,
                                                        step = 1
                                           ),
                                           selectInput("neg_distr", "Distribution",
                                                       choices = list("Normal",
                                                                      "Lognormal",
                                                                      "Gamma")),
                                           conditionalPanel(condition = "input.neg_distr == 'Normal'",
                                                            numericInput("sim_m_cont", "Mean value of controls",
                                                                         value = 100, min = -10000, max = 10000,
                                                                         step = 1
                                                            ),
                                                            numericInput("sim_sd_cont", "Standard deviation of controls",
                                                                         value = 15, min = -10000, max = 10000,
                                                                         step = 1
                                                            )),
                                           conditionalPanel(condition = "input.neg_distr == 'Lognormal'",
                                                            numericInput("sim_mlog_cont", "Log mean value of controls",
                                                                         value = 0, min = -10000, max = 10000,
                                                                         step = 1
                                                            ),
                                                            numericInput("sim_sdlog_cont", "Log standard deviation of controls",
                                                                         value = 1, min = -10000, max = 10000,
                                                                         step = 1
                                                            )),
                                           conditionalPanel(condition = "input.neg_distr == 'Gamma'",
                                                            numericInput("sim_shape_cont", "Shape parameter of controls",
                                                                         value = 2, min = -10000, max = 10000,
                                                                         step = 1
                                                            ),
                                                            numericInput("sim_rate_cont", "Rate parameter of controls",
                                                                         value = 1, min = -10000, max = 10000,
                                                                         step = 1
                                                            )
                                           )
                                       ),
                                       box(
                                           title = "Positive class",
                                           solidHeader = TRUE,
                                           # status = "warning",
                                           numericInput("sim_n_dis", "Number of cases",
                                                        value = 100, min = 2, max = 10000,
                                                        step = 1
                                           ),
                                           selectInput("pos_distr", "Distribution",
                                                       choices = list("Normal",
                                                                      "Lognormal",
                                                                      "Gamma")),
                                           conditionalPanel(condition = "input.pos_distr == 'Normal'",
                                                            numericInput("sim_m_dis", "Mean value of cases",
                                                                         value = 120, min = -10000, max = 10000,
                                                                         step = 1
                                                            ),
                                                            numericInput("sim_sd_dis", "Standard deviation of cases",
                                                                         value = 15, min = -10000, max = 10000,
                                                                         step = 1
                                                            )),
                                           conditionalPanel(condition = "input.pos_distr == 'Lognormal'",
                                                            numericInput("sim_mlog_dis", "Log mean value of cases",
                                                                         value = 1, min = -10000, max = 10000,
                                                                         step = 1
                                                            ),
                                                            numericInput("sim_sdlog_dis", "Log standard deviation of cases",
                                                                         value = 1, min = -10000, max = 10000,
                                                                         step = 1
                                                            )),
                                           conditionalPanel(condition = "input.pos_distr == 'Gamma'",
                                                            numericInput("sim_shape_dis", "Shape parameter of cases",
                                                                         value = 4, min = -10000, max = 10000,
                                                                         step = 1
                                                            ),
                                                            numericInput("sim_rate_dis", "Rate parameter of cases",
                                                                         value = 1, min = -10000, max = 10000,
                                                                         step = 1
                                                            )
                                           )
                                       ),
                                       box(
                                           numericInput("lower_percentile_cont",
                                                        "Controls: Lower percentile to include in the plot",
                                                        value = 0.01, min = 0.00001, max = 0.9999,
                                                        step = 0.001
                                           ),
                                           numericInput("upper_percentile_cont",
                                                        "Controls: Upper percentile to include in the plot",
                                                        value = 0.99, min = 0.00001, max = 0.9999,
                                                        step = 0.001
                                           )
                                       ),
                                       box(
                                           numericInput("lower_percentile_pat",
                                                        "Cases: Lower percentile to include in the plot",
                                                        value = 0.01, min = 0.00001, max = 0.9999,
                                                        step = 0.001
                                           ),
                                           numericInput("upper_percentile_pat",
                                                        "Cases: Upper percentile to include in the plot",
                                                        value = 0.99, min = 0.00001, max = 0.9999,
                                                        step = 0.001
                                           )
                                       ),
                                       downloadButton('download_simplot_distrs', 'Download Plot'),
                                       plotOutput("simplot_distrs")
                              ),
                              tabPanel("Cutpoint Distribution",
                                       box(
                                           title = "Simulation parameters",
                                           solidHeader = TRUE,
                                           # status = "warning",
                                           numericInput("sim_repeats", "Number of simulation runs",
                                                        value = 300, min = 10, max = 10000,
                                                        step = 10
                                           ),
                                           numericInput("sim_seed", "Seed (for reproducing the result)",
                                                        value = 1234, min = 1, max = 999999,
                                                        step = 1
                                           ),
                                           width = 6
                                       ),
                                       box(
                                           title = "Optimal cutpoint",
                                           solidHeader = TRUE,
                                           # status = "warning",
                                           selectInput("sim_method", "Method",
                                                       choices = list("minimize_metric",
                                                                      "maximize_metric",
                                                                      "minimize_boot_metric",
                                                                      "maximize_boot_metric",
                                                                      "oc_youden_normal",
                                                                      "oc_youden_kernel"),
                                                       selected = "maximize_metric"),
                                           width = 6,
                                           conditionalPanel(condition = "input.sim_method == 'maximize_metric'",
                                                            selectInput('sim_metric', 'Metric',
                                                                        choices = list("accuracy",
                                                                                       "youden",
                                                                                       "sum_sens_spec",
                                                                                       "prod_sens_spec",
                                                                                       "cohens_kappa",
                                                                                       "abs_d_sens_spec",
                                                                                       "abs_d_ppv_npv",
                                                                                       "prod_ppv_npv",
                                                                                       "sum_ppv_npv",
                                                                                       "prod_sens_spec",
                                                                                       "roc01",
                                                                                       "F1_score",
                                                                                       "odds_ratio"),
                                                                        selected = "sum_sens_spec")
                                           ),
                                           conditionalPanel(condition = "input.sim_method == 'minimize_metric'",
                                                            selectInput('sim_metric', 'Metric',
                                                                        choices = list("accuracy",
                                                                                       "youden",
                                                                                       "sum_sens_spec",
                                                                                       "prod_sens_spec",
                                                                                       "cohens_kappa",
                                                                                       "abs_d_sens_spec",
                                                                                       "abs_d_ppv_npv",
                                                                                       "prod_ppv_npv",
                                                                                       "sum_ppv_npv",
                                                                                       "prod_sens_spec",
                                                                                       "roc01",
                                                                                       "F1_score",
                                                                                       "odds_ratio"),
                                                                        selected = "abs_d_sesp")
                                           ),
                                           conditionalPanel(condition = "input.sim_method == 'minimize_boot_metric'",
                                                            selectInput('sim_metric', 'Metric',
                                                                        choices = list("accuracy",
                                                                                       "youden",
                                                                                       "sum_sens_spec",
                                                                                       "prod_sens_spec",
                                                                                       "cohens_kappa",
                                                                                       "abs_d_sens_spec",
                                                                                       "abs_d_ppv_npv",
                                                                                       "prod_ppv_npv",
                                                                                       "sum_ppv_npv",
                                                                                       "prod_sens_spec",
                                                                                       "roc01",
                                                                                       "F1_score",
                                                                                       "odds_ratio"),
                                                                        selected = "abs_d_sesp"),
                                                            numericInput("boot_cut", "Bootstrap Repetitions",
                                                                         value = 200, min = 30, max = 1000,
                                                                         step = 10
                                                            )
                                           ),
                                           conditionalPanel(condition = "input.sim_method == 'maximize_boot_metric'",
                                                            selectInput('sim_metric', 'Metric',
                                                                        choices = list("accuracy",
                                                                                       "youden",
                                                                                       "sum_sens_spec",
                                                                                       "prod_sens_spec",
                                                                                       "cohens_kappa",
                                                                                       "abs_d_sens_spec",
                                                                                       "abs_d_ppv_npv",
                                                                                       "prod_ppv_npv",
                                                                                       "sum_ppv_npv",
                                                                                       "prod_sens_spec",
                                                                                       "roc01",
                                                                                       "F1_score",
                                                                                       "odds_ratio"),
                                                                        selected = "sum_sens_spec"),
                                                            numericInput("boot_cut", "Bootstrap Repetitions",
                                                                         value = 200, min = 30, max = 1000,
                                                                         step = 10
                                                            )
                                           )
                                       ),
                                       box(
                                           title = "Start Simulation",
                                           solidHeader = T, status = "warning",
                                           width = 12,
                                           # actionButton("start_sim", "Start simulation")
                                           actionButton("start_sim", "Start",
                                                        icon("paper-plane"),
                                                        style = "color: #fff;
                                                        background-color: #337ab7;
                                                        border-color: #2e6da4")
                                       ),
                                       box(
                                           title = "Cutpoint distribution and bootstrapped confidence interval",
                                           solidHeader = T, status = "warning",
                                           width = 12,
                                           plotOutput("simplot_cutvar") %>%
                                               withSpinner(color="#0dc5c1"),
                                           downloadButton('download_simplot_cutvar', 'Download Plot')
                                       ),
                                       box(
                                           title = "Descriptive statistics",
                                           solidHeader = T, status = "warning",
                                           width = 12,
                                           tableOutput("sim_table") %>%
                                               withSpinner(color="#0dc5c1")
                                       )
                              )
                       )
                   )
)


ci_sim_tab <- tabItem(
    tabName = "ci_simulation",
    # box(title = "Info: CI Coverage Simulation", solidHeader = TRUE,
    #     status = "warning", width = 9,
    #     helpText("You can define distributions for both classes here
    #                             and simulate the coverage of confidence intervals. Pick the
    #                             number of repetitions, the method for cutpoint
    #                             estimation, and the method for constructing
    #                             confidence intervals below. The chosen distributions
    #                             are shown in the 'Distribution' tab.
    #                             To start the
    #                             simulation, switch to the 'CI coverage simulation' tab
    #                             and click 'Start'.")),
    box(title = "Info: CI Coverage and Width Simulation",
        solidHeader = TRUE,
        status = "warning",
        width = 12,
        includeMarkdown('ci_sim_tab_introduction.md')
    ),
    box(title = "Simulation",
        width = 12, status = "warning", solidHeader = TRUE,
        tabBox(id = "ci_sim_output", width = 12,
               tabPanel("Distributions",
                        box(title = "Negative class",
                            solidHeader = TRUE,
                            # status = "warning",
                            numericInput("ci_sim_n_cont", "Number of controls",
                                         value = 100, min = 2, max = 10000,
                                         step = 1),
                            selectInput("ci_neg_distr", "Distribution",
                                        choices = list("Normal",
                                                       "Lognormal",
                                                       "Gamma")),
                            conditionalPanel(condition = "input.ci_neg_distr == 'Normal'",
                                             numericInput("ci_sim_m_cont", "Mean value of controls",
                                                          value = 100, min = -10000, max = 10000,
                                                          step = 1
                                             ),
                                             numericInput("ci_sim_sd_cont",
                                                          "Standard deviation of controls",
                                                          value = 15, min = -10000, max = 10000,
                                                          step = 1
                                             )),
                            conditionalPanel(condition = "input.ci_neg_distr == 'Lognormal'",
                                             numericInput("ci_sim_mlog_cont",
                                                          "Log mean value of controls",
                                                          value = 0, min = -10000, max = 10000,
                                                          step = 1
                                             ),
                                             numericInput("ci_sim_sdlog_cont",
                                                          "Log standard deviation of controls",
                                                          value = 1, min = -10000, max = 10000,
                                                          step = 1
                                             )),
                            conditionalPanel(condition = "input.ci_neg_distr == 'Gamma'",
                                             numericInput("ci_sim_shape_cont",
                                                          "Shape parameter of controls",
                                                          value = 2, min = -10000, max = 10000,
                                                          step = 1
                                             ),
                                             numericInput("ci_sim_rate_cont",
                                                          "Rate parameter of controls",
                                                          value = 1, min = -10000, max = 10000,
                                                          step = 1
                                             )
                            )
                        ),
                        box(title = "Positive class",
                            solidHeader = TRUE,
                            # status = "warning",
                            numericInput("ci_sim_n_dis", "Number of cases",
                                         value = 100, min = 2, max = 10000,
                                         step = 1
                            ),
                            selectInput("ci_pos_distr", "Distribution",
                                        choices = list("Normal",
                                                       "Lognormal",
                                                       "Gamma")),
                            conditionalPanel(condition = "input.ci_pos_distr == 'Normal'",
                                             numericInput("ci_sim_m_dis",
                                                          "Mean value of cases",
                                                          value = 120, min = -10000, max = 10000,
                                                          step = 1
                                             ),
                                             numericInput("ci_sim_sd_dis",
                                                          "Standard deviation of cases",
                                                          value = 15, min = -10000, max = 10000,
                                                          step = 1
                                             )),
                            conditionalPanel(condition = "input.ci_pos_distr == 'Lognormal'",
                                             numericInput("ci_sim_mlog_dis",
                                                          "Log mean value of cases",
                                                          value = 1, min = -10000, max = 10000,
                                                          step = 1
                                             ),
                                             numericInput("ci_sim_sdlog_dis",
                                                          "Log standard deviation of cases",
                                                          value = 1, min = -10000, max = 10000,
                                                          step = 1
                                             )),
                            conditionalPanel(condition = "input.ci_pos_distr == 'Gamma'",
                                             numericInput("ci_sim_shape_dis",
                                                          "Shape parameter of cases",
                                                          value = 4, min = -10000, max = 10000,
                                                          step = 1
                                             ),
                                             numericInput("ci_sim_rate_dis",
                                                          "Rate parameter of cases",
                                                          value = 1, min = -10000, max = 10000,
                                                          step = 1
                                             )
                            )
                        ),
                        box(
                            numericInput("ci_lower_percentile_cont",
                                         "Controls: Lower percentile to include in the plot",
                                         value = 0.01, min = 0.00001, max = 0.9999,
                                         step = 0.001
                            ),
                            numericInput("ci_upper_percentile_cont",
                                         "Controls: Upper percentile to include in the plot",
                                         value = 0.99, min = 0.00001, max = 0.9999,
                                         step = 0.001
                            )
                        ),
                        box(
                            numericInput("ci_lower_percentile_pat",
                                         "Cases: Lower percentile to include in the plot",
                                         value = 0.01, min = 0.00001, max = 0.9999,
                                         step = 0.001
                            ),
                            numericInput("ci_upper_percentile_pat",
                                         "Cases: Upper percentile to include in the plot",
                                         value = 0.99, min = 0.00001, max = 0.9999,
                                         step = 0.001
                            )
                        ),
                        downloadButton('download_ci_simplot_distrs', 'Download Plot'),
                        plotOutput("ci_simplot_distrs")
               ),
               tabPanel("CI coverage simulation",
                            width = 6,
                        box(title = "Simulation parameters",
                            solidHeader = TRUE,
                            width = 4,
                            # status = "warning",
                            numericInput("ci_sim_repeats",
                                         "Number of simulation runs",
                                         value = 100, min = 10, max = 10000,
                                         step = 10
                            ),
                            numericInput("ci_sim_seed",
                                         "Seed (for reproducing the result)",
                                         value = 1234, min = 1, max = 999999,
                                         step = 1
                            )
                        ),
                        box(title = "Optimal cutpoint",
                            solidHeader = TRUE,
                            width = 4,
                            # status = "warning",
                            numericInput("ci_actual_cutpoint",
                                         "True Cutpoint given the selected distributions",
                                         value = 110, min = -9999999, max = 999999,
                                         step = 1
                            ),
                            selectInput("ci_sim_method", "Method",
                                        choices = list("minimize_metric",
                                                       "maximize_metric",
                                                       "minimize_boot_metric",
                                                       "maximize_boot_metric",
                                                       "oc_youden_normal",
                                                       "oc_youden_kernel"),
                                        selected = "maximize_metric"),
                            conditionalPanel(condition = "input.ci_sim_method == 'maximize_metric'",
                                             selectInput('ci_sim_metric', 'Metric',
                                                         choices = list("accuracy",
                                                                        "youden",
                                                                        "sum_sens_spec",
                                                                        "prod_sens_spec",
                                                                        "cohens_kappa",
                                                                        "abs_d_sens_spec",
                                                                        "abs_d_ppv_npv",
                                                                        "prod_ppv_npv",
                                                                        "sum_ppv_npv",
                                                                        "prod_sens_spec",
                                                                        "roc01",
                                                                        "F1_score",
                                                                        "odds_ratio"),
                                                         selected = "sum_sens_spec")
                            ),
                            conditionalPanel(condition = "input.ci_sim_method == 'minimize_metric'",
                                             selectInput('ci_sim_metric', 'Metric',
                                                         choices = list("accuracy",
                                                                        "youden",
                                                                        "sum_sens_spec",
                                                                        "prod_sens_spec",
                                                                        "cohens_kappa",
                                                                        "abs_d_sens_spec",
                                                                        "abs_d_ppv_npv",
                                                                        "prod_ppv_npv",
                                                                        "sum_ppv_npv",
                                                                        "prod_sens_spec",
                                                                        "roc01",
                                                                        "F1_score",
                                                                        "odds_ratio"),
                                                         selected = "abs_d_sesp")
                            ),
                            conditionalPanel(condition = "input.ci_sim_method == 'minimize_boot_metric'",
                                             selectInput('ci_sim_metric', 'Metric',
                                                         choices = list("accuracy",
                                                                        "youden",
                                                                        "sum_sens_spec",
                                                                        "prod_sens_spec",
                                                                        "cohens_kappa",
                                                                        "abs_d_sens_spec",
                                                                        "abs_d_ppv_npv",
                                                                        "prod_ppv_npv",
                                                                        "sum_ppv_npv",
                                                                        "prod_sens_spec",
                                                                        "roc01",
                                                                        "F1_score",
                                                                        "odds_ratio"),
                                                         selected = "abs_d_sesp"),
                                             numericInput("ci_boot_cut", "Bootstrap Repetitions",
                                                          value = 100, min = 30, max = 1000,
                                                          step = 10
                                             )
                            ),
                            conditionalPanel(condition = "input.ci_sim_method == 'maximize_boot_metric'",
                                             selectInput('ci_sim_metric', 'Metric',
                                                         choices = list("accuracy",
                                                                        "youden",
                                                                        "sum_sens_spec",
                                                                        "prod_sens_spec",
                                                                        "cohens_kappa",
                                                                        "abs_d_sens_spec",
                                                                        "abs_d_ppv_npv",
                                                                        "prod_ppv_npv",
                                                                        "sum_ppv_npv",
                                                                        "prod_sens_spec",
                                                                        "roc01",
                                                                        "F1_score",
                                                                        "odds_ratio"),
                                                         selected = "sum_sens_spec"),
                                             numericInput("ci_boot_cut", "Bootstrap Repetitions",
                                                          value = 100, min = 30, max = 1000,
                                                          step = 10
                                             )
                            )
                        ),
                        box(title = "Confidence Interval for Cutpoint",
                            footer = "Confidence interval based on empirical bootstrap percentile",
                            width = 4,
                            solidHeader = TRUE,
                            # status = "warning",
                            # selectInput("ci_ci_method", "CI method",
                            #             choices = list("Parametric Bootstrap Percentile",
                            #                            "Nonparametric Bootstrap Percentile",
                            #                            "Delta method"),
                            #             selected = "Nonparametric Bootstrap Percentile"),
                            # conditionalPanel(condition = "input.ci_ci_method == 'Parametric Bootstrap Percentile'",
                            #                  numericInput("ci_method_reps",
                            #                               "Bootstrap Repetitions",
                            #                               value = 200, min = 30, max = 1000,
                            #                               step = 10
                            #                  )
                            # ),
                            # conditionalPanel(condition = "input.ci_ci_method == 'Nonparametric Bootstrap Percentile'",
                                             numericInput("ci_method_reps",
                                                          "Bootstrap Repetitions",
                                                          value = 100, min = 30, max = 1000,
                                                          step = 10
                                             # )
                            )
                        ),
                        box(
                            title = "Start Simulation",
                            solidHeader = T,
                            status = "warning",
                            width = 12,
                            # actionButton("start_sim", "Start simulation")
                            actionButton("start_ci_sim", "Start",
                                         icon("paper-plane"),
                                         style = "color: #fff;
                                                        background-color: #337ab7;
                                                        border-color: #2e6da4")
                        ),
                        # box(width = 12,
                        #     title = "DEBUG",
                        #     solidHeader = T,
                        #     status = "warning",
                        #     textOutput("ci_sim_method_text"),
                        #     textOutput("ci_sim_metric_text")
                        # ),
                        box(width = 12,
                            title = "Confidence interval coverage and descriptives",
                            solidHeader = T,
                            status = "warning",
                            tableOutput("ci_sim_table") %>%
                                withSpinner(color="#0dc5c1")
                        ),
                        box(
                            width = 12,
                            title = "Simulated confidence intervals and defined cutpoint",
                            solidHeader = T,
                            status = "warning",
                            plotOutput("simplot_ci") %>%
                                withSpinner(color="#0dc5c1"),
                            downloadButton('download_simplot_ci', 'Download Plot')
                        ),
                        box(
                            width = 12,
                            title = "Histogram of confidence interval widths",
                            solidHeader = T,
                            status = "warning",
                            plotOutput("ci_width_plot") %>%
                                withSpinner(color="#0dc5c1"),
                            downloadButton('download_ci_width_plot', 'Download Plot')
                        )
               )
        )
    )
)


about_tab <- tabItem(tabName = "about",
                     fluidRow(
                         box(
                             includeMarkdown("about.md"),
                             width = 9
                         )
                    )
)

body <- dashboardBody(
    tabItems(
        data_tab,
        cutpoints_tab,
        bootstrap_tab,
        sim_tab,
        ci_sim_tab,
        about_tab
    )
)

dashboardPage(header, sidebar, body)
