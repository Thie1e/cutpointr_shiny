
function(input, output) {
    data_set <- reactive({
        if (input$datatype == "Example data") {
            data(suicide)
            return(suicide)
        }

        inFile <- input$file1

        if (is.null(inFile)) return(NULL)

        # read.csv(inFile$datapath, header=input$header, sep=input$sep,
        #          quote=input$quote)
        read_csv(inFile$datapath)
    })

    output$contents <- renderDataTable({data_set()})

    output$choose_outcome <- renderUI({
        colnames <- names(data_set())
        selectInput("outcome", "Outcome variable",
                           choices  = colnames,
                           selected = "suicide")
    })

    output$choose_predictor <- renderUI({
        colnames <- names(data_set())
        selectInput("predictor", "Predictor variable",
                           choices  = colnames,
                           selected = "dsi")
    })

    unique_class <- reactive(unique(data_set()[, as.character(input$outcome)]))
    output$choose_pos_class <- renderUI({
        colnames <- names(data_set())
        selectInput("pos_class", "Positive class",
                           choices  = unique_class(),
                           selected = unique_class()[1])
    })

    neg_class <- reactive({
        unique_class()[unique_class() != input$pos_class]
    })

    output$choose_direction <- renderUI({
        selectInput("direction", "Direction: A predictor value >= or <= the cutoff implies the positive class",
                           choices  = c(">=", "<="),
                           selected = ">=")
    })

    output$choose_group <- renderUI({
        colnames <- names(data_set())
        selectInput("group", "Grouping variable",
                           choices  = c("None", colnames),
                           selected = "None")
    })

    oc <- reactive({
        if (input$method == "oc_OptimalCutpoints") {
            oc <- cutpointr_(data = data_set(),
                       x = input$predictor,
                       class = input$outcome,
                       pos_class = input$pos_class, neg_class = neg_class(),
                       direction = input$direction,
                       method = "oc_OptimalCutpoints",
                       metric = input$metric,
                       boot_runs = input$boot_runs,
                       oc_metric = as.character(input$oc_metric))
        } else {
            oc <- cutpointr_(data = data_set(), x = input$predictor,
                       class = input$outcome,
                       neg_class = neg_class(), pos_class = input$pos_class,
                       direction = input$direction,
                       method = input$method, metric = input$metric,
                       boot_runs = input$boot_runs)
        }
        return(oc)
    })
    oc2 <- reactive(optimal.cutpoints(X = input$predictor, status = input$outcome,tag.healthy = "no",
                                      methods = "MaxSpSe", data = data_set()))
    # output$pred <- reactive(input$predictor)
    # output$out <- reactive(input$outcome)
    # output$dat <- reactive(as.character(head(data_set()[, c(input$predictor, input$outcome)])))
    output$oc <- reactive(as.character(oc()[, "optimal_cutpoint"]))
    output$oc2 <- reactive(as.character(oc2()$MaxSpSe$Global$optimal.cutoff$cutoff))
    # output$oc_metric_optimalcutpoints <- reactive(as.character(input$oc_metric))
    output$unique_class <- reactive(unique_class())
    output$neg_class <- reactive(neg_class())

    rocplot_reac <- reactive({
        if (is.null(data_set())) return(NULL)
        plot_roc(oc(), display_cutpoint = FALSE)
        })
    output$rocplot <- renderPlot(rocplot_reac())

    distplot_reac <- reactive({
        if (is.null(data_set())) return(NULL)
        plot_x(oc())
        })
    output$distplot <- renderPlot(distplot_reac())

    metricplot_reac <- reactive({
        if (input$method %in% c("maximize_metric", "minimize_metric")) {
            plot_metric(oc())
        } else {
            NULL
        }
    })
    output$metricplot <- renderPlot(metricplot_reac())

    bootplot_cut_reac <- reactive({
        if (input$boot_runs <= 0) {
            NULL
        } else {
            plot_cut_boot(oc())
        }
    })
    output$bootplot_cut <- renderPlot(bootplot_cut_reac())

    bootplot_metric_reac <- reactive({
        if (input$boot_runs <= 0) {
            NULL
        } else {
            plot_metric_boot(oc())
        }
    })
    output$bootplot_metric <- renderPlot(bootplot_metric_reac())

    optimal_cutpoint <- reactive(oc()[, "optimal_cutpoint"])
    output$optimal_cutpoint <- renderInfoBox({
        infoBox(
            title = "Optimal Cutpoint", subtitle = "Optimal Cutpoint",
            icon = icon("cut"), color = "yellow", value = optimal_cutpoint()
        )
    })

    #
    # Simulation tab
    #
    simcut <- function(n_cont, m_cont, sd_cont, n_dis, m_dis, sd_dis, rep) {
        res <- rep(NA, rep)
        for(i in 1:rep){
            cont<-rnorm(n_cont, m_cont, sd_dis)
            dis<-rnorm(n_dis, m_dis, sd_dis)
            dat<-data.frame(pred= c(cont, dis), crit = c(rep("0", n_cont), rep("1", n_dis)))
            res[i]<-suppressMessages(cutpointr_(dat, x = "pred", class = "crit",
                                                method = input$sim_method,
                                                metric = input$sim_metric)$optimal_cutpoint)
        }
        res<-data.frame(res)
        ci<-quantile(res$res, c(.025, .975))
        return(list(res, ci))
    }

    sim <- reactive(simcut(n_cont = input$sim_n_cont,
                           m_cont = input$sim_m_cont,
                           sd_cont = input$sim_sd_cont,
                           n_dis = input$sim_n_dis,
                           m_dis = input$sim_m_dis,
                           sd_dis = input$sim_sd_dis,
                           rep = input$sim_repeats))

    xrange <- reactive(seq(min(input$sim_m_cont-2 * input$sim_sd_cont,
                               input$sim_m_dis-2 * input$sim_sd_dis),
                           max(input$sim_m_cont + 2 * input$sim_sd_cont,
                               input$sim_m_dis + 2 * input$sim_sd_dis),
                           length=250))

    distrs <- reactive(data.frame(xrange = xrange(),
                                  ds = c(dnorm(xrange(), input$sim_m_cont, input$sim_sd_cont),
                                         dnorm(xrange(), input$sim_m_dis, input$sim_sd_dis)),
                                  group = c(rep(c("cont", "diseased"), each = 250))))

    distrs_reac <- reactive({
        ggplot(distrs(), aes(x = xrange, y = ds, col = group)) +
            geom_line() +
            ggtitle("Assumed distributions")
    })
    output$simplot_distrs <- renderPlot(distrs_reac())


    cutvar_reac <- reactive({
        ggplot(sim()[[1]], aes(x = res)) +
            geom_histogram(aes(y = ..density..), bins = 30) +
            geom_density() +
            geom_vline(xintercept = sim()[[2]], col ="red") +
            theme_minimal() +
            ggtitle("Estimated variability of optimal cutpoints")
    })
    output$simplot_cutvar <- renderPlot(cutvar_reac())
}
