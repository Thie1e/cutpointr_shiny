
function(input, output) {
    data_set <- reactive({
        if (input$datatype == "Example data") {
            data(suicide)
            dat <- suicide
            dat$age <- NULL
            return(dat)
        }

        inFile <- input$file1

        if (is.null(inFile)) return(NULL)

        dat <- read_delim(file = inFile$datapath, na = input$na.strings, quote = input$quote,
                          skip = input$skip, delim = input$sep)
        return(as.data.frame(dat))
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
                           selected = unique_class()[2])
    })

    neg_class <- reactive({
        unique_class()[unique_class() != input$pos_class]
    })

    output$choose_direction <- renderUI({
        selectInput("direction", "Direction: A predictor value >= or <= than the cutoff implies the positive class",
                           choices  = c(">=", "<="),
                           # selected = oc()$direction)
                           selected = ">=")
    })

    grouping_vars <- reactive({
        observe(input$pos_class)
        colnames <- colnames(data_set())
        colnames[!(colnames %in% c(input$predictor, input$outcome))]
    })

    output$choose_group <- renderUI({
        cols <- grouping_vars()
        selectInput("group", "Grouping variable",
                    choices  = c("None", cols), selected = "None")
    })

    metric_func <- reactive({
        switch(input$metric,
               "accuracy" = cutpointr::accuracy,
               "youden" = cutpointr::youden,
               "cohens_kappa" = cutpointr::cohens_kappa,
               "abs_d_sens_spec" = cutpointr::abs_d_sens_spec,
               "odds_ratio" = cutpointr::odds_ratio,
               "sum_sens_spec" = cutpointr::sum_sens_spec
        )
    })

    method_func <- reactive({
        switch(input$method,
               "maximize_metric" = cutpointr::maximize_metric,
               "minimize_metric" = cutpointr::minimize_metric,
               "maximize_boot_metric" = cutpointr::maximize_boot_metric,
               "minimize_boot_metric" = cutpointr::minimize_boot_metric,
               "oc_youden_kernel" = cutpointr::oc_youden_kernel,
               "oc_youden_normal" = cutpointr::oc_youden_normal
        )
    })

    oc <- reactive({
        if (input$group == "None") {
            sg <- NULL
        } else {
            sg <- input$group
        }
        oc <- cutpointr_(data = data_set(), x = input$predictor,
                         class = input$outcome,
                         subgroup = sg,
                         neg_class = neg_class(), pos_class = input$pos_class,
                         direction = input$direction,
                         method = method_func(), metric = metric_func(),
                         boot_cut = input$boot_cut)
        return(oc)
    })
    pred_oc <- reactive({
        if ("subgroup" %in% colnames(oc())) {
            s <- summary(oc())$desc
            s <- do.call(rbind, s)
            sg <- oc()$subgroup
            sg <- data.frame(subgroup = sg, stringsAsFactors = FALSE)
            s <- cbind(sg, s)
        } else {
            s <- summary(oc())$desc[[1]]
        }
        return(s)
        })
    output$pred_table <- renderTable(pred_oc())
    pred_byclass_oc <- reactive({
        if ("subgroup" %in% colnames(oc())) {
            soc <- summary(oc())
            dbc <- purrr::map(soc$desc_by_class,
                              function(x) rownames_to_column(x, var = "class"))
            s <- do.call(rbind, dbc)
            s$subgroup <- do.call(c, purrr::map(soc$subgroup, function(x) rep(x, 2)))
            s <- dplyr::arrange(s, subgroup)
            s <- s[colnames(s)[c(length(s), 1:(length(s) - 1))]]
        } else {
            s <- summary(oc())$desc_by_class[[1]]
            s <- rownames_to_column(s, var = "Class")
        }
        return(s)
        })
    output$pred_table_byclass <- renderTable(pred_byclass_oc())
    outcome_oc <- reactive({
        n_pos <- summary(oc())$n_pos
        n_neg <- summary(oc())$n_neg
        auc <- oc()$AUC
        prev <- oc()$prevalence
        if ("subgroup" %in% colnames(oc())) {
            sg <- oc()$subgroup
            o <- data.frame(subgroup = sg, AUC = auc,
                            n_pos = n_pos, n_neg = n_neg, percent_pos = prev)

        } else {
            o <- data.frame(AUC = auc, n_pos = n_pos, n_neg = n_neg,
                            percent_pos = prev)
        }
        return(o)
        })
    output$outcome_table <- renderTable(outcome_oc())
    optimal_cutpoint_oc <- reactive({
        opt_cut <- oc()
        res <- dplyr::select(opt_cut, 1:specificity)
        return(res)
        })
    output$optimal_cutpoint_table <- renderTable(optimal_cutpoint_oc())

    oc_boot <- reactive({
        if (input$boot_runs > 0) {
            set.seed(input$boot_seed)
            if (input$group == "None") {
                sg <- NULL
            } else {
                sg <- input$group
            }

            oc_boot <- cutpointr_(data = data_set(), x = input$predictor,
                                  class = input$outcome,
                                  subgroup = sg,
                                  neg_class = neg_class(), pos_class = input$pos_class,
                                  direction = input$direction,
                                  method = method_func(), metric = metric_func(),
                                  boot_runs = input$boot_runs,
                                  boot_cut = input$boot_cut)
            return(oc_boot)
        }
    })

    # oc2 <- reactive(optimal.cutpoints(X = input$predictor, status = input$outcome,tag.healthy = "no",
    #                                   methods = "MaxSpSe", data = data_set()))
    # output$oc <- reactive(as.character(oc()[["optimal_cutpoint"]]))
    # output$method <- reactive(input$method)
    # output$metric <- reactive(input$metric)

    boot_oc_summary <- reactive({
        sboc <- summary(oc_boot())
        if ("subgroup" %in% colnames(oc())) {
            res <- purrr::map2(sboc$boot, oc_boot()$subgroup, function(x, sg) {
                x$subgroup <- sg
                return(x)
            })
            res <- res %>% dplyr::bind_rows() %>%
                dplyr::select(subgroup, everything())
        } else {
            res <- sboc$boot[[1]]
        }
        return(res)
    })
    output$boot_table <- renderTable(boot_oc_summary())
    output$download_boot_table <- downloadHandler(
        filename = "cutpointr_bootstrap.csv",
        content = function(file) {
            write.csv(boot_oc_summary(), file, row.names = FALSE)
        }
    )

    rocplot_reac <- function() {
        if (is.null(data_set())) return(NULL)
        plot_roc(oc(), display_cutpoint = FALSE) + ggtitle(NULL) +
            ggplot2::theme_bw() + ggplot2::theme(aspect.ratio = 1)
        }
    output$rocplot <- renderPlot(rocplot_reac())
    output$download_rocplot <- downloadHandler(
        filename = "cutpointr_roc.png",
        content = function(file) {
            png(file, width = 500, height = 500)
            print(rocplot_reac())
                      # theme(plot.title = element_text(size = 120),
                      #       axis.title = element_text(size = 100),
                      #       axis.text.x = element_text(size = 80),
                      #       axis.text.y = element_text(size = 80)))
            dev.off()
        })

    prplot_reac <- function() {
        if (is.null(data_set())) return(NULL)
        plot_precision_recall(oc(), display_cutpoint = FALSE) +
            ggtitle(NULL) + ggplot2::theme_bw() + ggplot2::theme(aspect.ratio = 1)
        }
    output$prplot <- renderPlot(prplot_reac())
    output$download_prplot <- downloadHandler(
        filename = "cutpointr_precision_recall.png",
        content = function(file) {
            png(file, width = 500, height = 500)
            print(prplot_reac())
                      # theme(plot.title = element_text(size = 120),
                      #       axis.title = element_text(size = 100),
                      #       axis.text.x = element_text(size = 80),
                      #       axis.text.y = element_text(size = 80))
            dev.off()
        })

    xplot_reac <- function() {
        if (is.null(data_set())) return(NULL)
        plot_x(oc(), display_cutpoint = FALSE) +
            ggtitle(NULL, NULL) +
            ggplot2::theme_bw()
        }
    output$xplot <- renderPlot(xplot_reac())
    output$download_xplot <- downloadHandler(
        filename = "cutpointr_distr_predictions.png",
        content = function(file) {
            png(file, width = 700, height = 500)
            print(xplot_reac() +
                      # theme(plot.title = element_text(size = 120),
                      #       plot.subtitle = element_text(size = 100),
                      #       strip.text.x = element_text(size = 80),
                      #       axis.title = element_text(size = 100),
                      #       axis.text.x = element_text(size = 80),
                      #       axis.text.y = element_text(size = 80)) +
                      ggtitle(paste("Distribution of", input$predictor)))
            dev.off()
        })

    metricplot_reac <- function() {
        if (input$method %in% c("maximize_metric", "minimize_metric")) {
            plot_metric(oc()) + ggplot2::theme_bw()
        } else {
            NULL
        }
    }
    output$metricplot <- renderPlot(metricplot_reac())
    output$download_metricplot <- downloadHandler(
        filename = "metric_vs_cutoff.png",
        content = function(file) {
            png(file, width = 700, height = 500)
            print(metricplot_reac())
                  # theme(plot.title = element_text(size = 120),
                  #       plot.subtitle = element_text(size = 100),
                  #       strip.text.x = element_text(size = 80),
                  #       axis.title = element_text(size = 100),
                  #       axis.text.x = element_text(size = 80),
                  #       axis.text.y = element_text(size = 80)))
            dev.off()
        })

    bootplot_cut_reac <- function() {
        if (input$boot_runs <= 0) {
            NULL
        } else {
            plot_cut_boot(oc_boot()) + ggplot2::theme_bw()
        }
    }
    output$bootplot_cut <- renderPlot(bootplot_cut_reac())
    output$download_bootplot_cut <- downloadHandler(
        filename = "bootstrap_cutoffs.png",
        content = function(file) {
            png(file, width = 700, height = 500)
            print(bootplot_cut_reac())
                      # theme(plot.title = element_text(size = 120),
                      #       plot.subtitle = element_text(size = 100),
                      #       strip.text.x = element_text(size = 80),
                      #       axis.title = element_text(size = 100),
                      #       axis.text.x = element_text(size = 80),
                      #       axis.text.y = element_text(size = 80)))
            dev.off()
        })

    bootplot_metric_reac <- function() {
        if (input$boot_runs <= 0) {
            NULL
        } else {
            plot_metric_boot(oc_boot()) + ggplot2::theme_bw()
        }
    }
    output$bootplot_metric <- renderPlot(bootplot_metric_reac())
    output$download_bootplot_metric <- downloadHandler(
        filename = "bootstrap_metric.png",
        content = function(file) {
            png(file, width = 700, height = 500)
            print(bootplot_metric_reac())
                      # theme(plot.title = element_text(size = 120),
                      #       plot.subtitle = element_text(size = 100),
                      #       strip.text.x = element_text(size = 80),
                      #       axis.title = element_text(size = 100),
                      #       axis.text.x = element_text(size = 80),
                      #       axis.text.y = element_text(size = 80)))
            dev.off()
        })

    optimal_cutpoint <- reactive(oc()[, "optimal_cutpoint"])
    output$optimal_cutpoint <- renderInfoBox({
        infoBox(
            title = "", subtitle = "Optimal Cutpoint",
            icon = icon("cut"), color = "yellow", value = optimal_cutpoint()
        )
    })

    #
    # Simulation tab
    #
    sim_metric <- reactive({
        switch(input$sim_metric,
               "accuracy" = cutpointr::accuracy,
               "youden" = cutpointr::youden,
               "cohens_kappa" = cutpointr::cohens_kappa,
               "abs_d_sens_spec" = cutpointr::abs_d_sens_spec,
               "odds_ratio" = cutpointr::odds_ratio,
               "sum_sens_spec" = cutpointr::sum_sens_spec
        )
    })

    sim_method <- reactive({
        switch(input$sim_method,
               "maximize_metric" = cutpointr::maximize_metric,
               "minimize_metric" = cutpointr::minimize_metric,
               "maximize_boot_metric" = cutpointr::maximize_boot_metric,
               "minimize_boot_metric" = cutpointr::minimize_boot_metric,
               "oc_youden_kernel" = cutpointr::oc_youden_kernel,
               "oc_youden_normal" = cutpointr::oc_youden_normal
        )
    })
    simcut <- function(n_cont, m_cont, sd_cont, n_dis, m_dis, sd_dis, rep, seed) {
        res <- rep(NA, rep)
        set.seed(seed)
        for(i in 1:rep) {
            cont <- rnorm(n_cont, m_cont, sd_dis)
            dis <- rnorm(n_dis, m_dis, sd_dis)
            dat <- data.frame(pred= c(cont, dis), crit = c(rep("0", n_cont), rep("1", n_dis)))
            res[i] <- suppressMessages(
                cutpointr_(data = dat, x = "pred", class = "crit",
                           method = sim_method(),
                           metric = sim_metric(),
                           pos_class = 1, neg_class = 0,
                           break_ties = mean)$optimal_cutpoint[1]
            )
        }
        res <- data.frame(res)
        ci <- quantile(res$res, c(.025, .975))
        return(list(res, ci))
    }

    sim <- reactive(simcut(n_cont = input$sim_n_cont,
                           m_cont = input$sim_m_cont,
                           sd_cont = input$sim_sd_cont,
                           n_dis = input$sim_n_dis,
                           m_dis = input$sim_m_dis,
                           sd_dis = input$sim_sd_dis,
                           rep = input$sim_repeats,
                           seed = input$sim_seed))

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
