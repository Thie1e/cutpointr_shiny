
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

    oc_boot <- eventReactive(input$start_bootstrap, {
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
    })

    # oc2 <- reactive(optimal.cutpoints(X = input$predictor, status = input$outcome,tag.healthy = "no",
    #                                   methods = "MaxSpSe", data = data_set()))
    # output$oc <- reactive(as.character(oc()[["optimal_cutpoint"]]))
    # output$method <- reactive(input$method)
    # output$metric <- reactive(input$metric)

    boot_oc_summary <- eventReactive(input$start_bootstrap, {
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

    generate_data <- function(n, dist, group,
                              mean = NULL, sd = NULL,
                              shape = NULL, rate = NULL,
                              meanlog = NULL, sdlog = NULL) {
        if (dist == "Normal") {
            dat <- data.frame(x = rnorm(n = n, mean = mean, sd = sd),
                              group = group, stringsAsFactors = FALSE)
        } else if (dist == "Gamma") {
            dat <- data.frame(x = rgamma(n = n, shape = shape, rate = rate),
                              group = group, stringsAsFactors = FALSE)
        } else if (dist == "Lognormal") {
            dat <- data.frame(x = rlnorm(n = n, meanlog = meanlog, sdlog = sdlog),
                              group = group, stringsAsFactors = FALSE)
        } else {
            stop("No data generated")
        }
        return(dat)
    }

    sim_metric <- reactive({
        switch(input$sim_metric,
               "accuracy" = cutpointr::accuracy,
               "youden" = cutpointr::youden,
               "cohens_kappa" = cutpointr::cohens_kappa,
               "abs_d_sens_spec" = cutpointr::abs_d_sens_spec,
               "abs_d_ppv_npv" = cutpointr::abs_d_ppv_npv,
               "prod_ppv_npv" = cutpointr::prod_ppv_npv,
               "sum_ppv_npv" = cutpointr::sum_ppv_npv,
               "odds_ratio" = cutpointr::odds_ratio,
               "sum_sens_spec" = cutpointr::sum_sens_spec,
               "prod_sens_spec" = cutpointr::prod_sens_spec,
               "roc01" = cutpointr::roc01,
               "F1_score" = cutpointr::F1_score
        )
    })

    sim_method <- reactive({
        switch(input$sim_method,
               "maximize_metric" = cutpointr::maximize_metric,
               "minimize_metric" = cutpointr::minimize_metric,
               "oc_youden_kernel" = cutpointr::oc_youden_kernel,
               "oc_youden_normal" = cutpointr::oc_youden_normal
        )
    })

    simcut <- function(dist_cont, dist_dis,
                       n_cont, n_dis,
                       mean_cont = NULL, mean_dis = NULL, sd_cont = NULL, sd_dis = NULL,
                       shape_cont = NULL, shape_dis = NULL, rate_cont = NULL, rate_dis = NULL,
                       meanlog_cont = NULL, meanlog_dis = NULL, sdlog_cont = NULL, sdlog_dis = NULL,
                       rep, seed) {
        res <- rep(NA, rep)
        set.seed(seed)
        withProgress(message = "Running simulation", value = 0,
                     for(i in 1:rep) {
                         incProgress(1/rep, detail = paste("Running repetition", i))
                         cont <- generate_data(n = n_cont, dist = dist_cont, group = "negative",
                                               mean = mean_cont, sd = sd_cont, shape = shape_cont,
                                               rate = rate_cont,  meanlog = meanlog_cont,
                                               sdlog = sdlog_cont)
                         dis <- generate_data(n = n_dis, dist = dist_dis, group = "positive",
                                               mean = mean_dis, sd = sd_dis, shape = shape_dis,
                                               rate = rate_dis,  meanlog = meanlog_dis,
                                               sdlog = sdlog_dis)
                         simdat <- dplyr::bind_rows(cont, dis)
                         res[i] <- suppressMessages(
                             cutpointr_(data = simdat, x = "x", class = "group",
                                        method = sim_method(),
                                        metric = sim_metric(),
                                        pos_class = "positive", neg_class = "negative",
                                        break_ties = mean)$optimal_cutpoint[1]
                         )
                     }
        )
        res <- data.frame(res)
        ci <- quantile(res$res, c(.025, .975))
        return(list(res, ci))
    }

    sim <- eventReactive(input$start_sim,
                         simcut(n_cont = input$sim_n_cont,
                                dist_cont = input$neg_distr,
                                mean_cont = input$sim_m_cont,
                                sd_cont = input$sim_sd_cont,
                                n_dis = input$sim_n_dis,
                                dist_dis = input$pos_distr,
                                mean_dis = input$sim_m_dis,
                                sd_dis = input$sim_sd_dis,
                                rep = input$sim_repeats,
                                seed = input$sim_seed,
                                meanlog_cont = input$sim_mlog_cont,
                                meanlog_dis = input$sim_mlog_dis,
                                sdlog_cont = input$sim_sdlog_cont,
                                sdlog_dis = input$sim_sdlog_dis,
                                shape_cont = input$sim_shape_cont,
                                rate_cont = input$sim_rate_cont,
                                shape_dis = input$sim_shape_dis,
                                rate_dis = input$sim_rate_dis)
                         )


    distr_pos <- reactive(
        if (input$pos_distr == "Normal") {
            xrange <- seq(qnorm(p = input$lower_percentile_pat,
                                mean = input$sim_m_dis, sd = input$sim_sd_dis),
                          qnorm(p = input$upper_percentile_pat,
                                mean = input$sim_m_dis, sd = input$sim_sd_dis),
                          length.out = 250)
            tibble::tibble(xrange = xrange,
                                  ds = c(dnorm(xrange, input$sim_m_dis, input$sim_sd_dis)),
                                  group = rep("Positive", 250))
        } else if (input$pos_distr == "Lognormal") {
            xrange <- seq(qlnorm(p = input$lower_percentile_pat,
                                 meanlog = input$sim_mlog_dis, sdlog = input$sim_sdlog_dis),
                          qlnorm(p = input$upper_percentile_pat,
                                 meanlog = input$sim_mlog_dis, sdlog = input$sim_sdlog_dis),
                          length.out = 250)
            tibble::tibble(xrange = xrange,
                                  ds = c(dlnorm(xrange, input$sim_mlog_dis, input$sim_sdlog_dis)),
                                  group = rep("Positive", 250))
        } else if (input$pos_distr == "Gamma") {
            xrange <- seq(qgamma(p = input$lower_percentile_pat,
                                 shape = input$sim_shape_dis, rate = input$sim_rate_dis),
                          qgamma(p = input$upper_percentile_pat,
                                 shape = input$sim_shape_dis, rate = input$sim_rate_dis),
                          length.out = 250)
            tibble::tibble(xrange = xrange,
                                  ds = c(dgamma(xrange, shape = input$sim_shape_dis,
                                                rate = input$sim_rate_dis)),
                                  group = rep("Positive", 250))
        }
    )

    distr_neg <- reactive(
        if (input$neg_distr == "Normal") {
            xrange <- seq(qnorm(p = input$lower_percentile_cont,
                                mean = input$sim_m_cont, sd = input$sim_sd_cont),
                          qnorm(p = input$upper_percentile_cont,
                                mean = input$sim_m_cont, sd = input$sim_sd_cont),
                          length.out = 250)
            tibble::tibble(xrange = xrange,
                       ds = c(dnorm(xrange, input$sim_m_cont, input$sim_sd_cont)),
                       group = rep("Negative", 250))
        } else if (input$neg_distr == "Lognormal") {
            xrange <- seq(qlnorm(p = input$lower_percentile_cont,
                                 meanlog = input$sim_mlog_cont, sdlog = input$sim_sdlog_cont),
                          qlnorm(p = input$upper_percentile_cont,
                                 meanlog = input$sim_mlog_cont, sdlog = input$sim_sdlog_cont),
                          length.out = 250)
            tibble::tibble(xrange = xrange,
                       ds = c(dlnorm(xrange, input$sim_mlog_cont, input$sim_sdlog_cont)),
                       group = rep("Negative", 250))
        } else if (input$neg_distr == "Gamma") {
            xrange <- seq(qgamma(p = input$lower_percentile_cont,
                                 shape = input$sim_shape_cont, rate = input$sim_rate_cont),
                          qgamma(p = input$upper_percentile_cont,
                                 shape = input$sim_shape_cont, rate = input$sim_rate_cont),
                          length.out = 250)
            tibble::tibble(xrange = xrange,
                       ds = c(dgamma(xrange, shape = input$sim_shape_cont,
                                     rate = input$sim_rate_cont)),
                       group = rep("Negative", 250))
        }
    )

    distrs <- reactive(dplyr::bind_rows(distr_pos(), distr_neg()))

    distrs_reac <- reactive({
        ggplot(distrs(), aes(x = xrange, y = ds, linetype = group)) +
            geom_line() +
            ggtitle("Assumed distributions") +
            xlab("x") + ylab("Density") +
            theme_bw()
    })
    output$simplot_distrs <- renderPlot(distrs_reac())

    output$download_simplot_distrs <- downloadHandler(
        filename = "cutpointr_distributions.png",
        content = function(file) {
            png(file, width = 500, height = 500)
            print(distrs_reac())
            dev.off()
        })

    cutvar_reac <- reactive({
        ggplot(sim()[[1]], aes(x = res)) +
            geom_histogram(aes(y = ..density..), bins = 30) +
            geom_density() +
            geom_vline(xintercept = sim()[[2]], col ="red") +
            theme_minimal() +
            ggtitle("Distribution of optimal cutpoints and 95% confidence interval") +
            xlab("Cutpoint") + ylab("Probability")
    })
    output$simplot_cutvar <- renderPlot(cutvar_reac())

    sim_oc_summary <- eventReactive(input$start_sim, {
        tibble::tibble(Min. = min(sim()[[1]]$res),
                      `0.01` = quantile(sim()[[1]]$res, 0.01),
                      `0.025` = quantile(sim()[[1]]$res, 0.025),
                      `0.05` = quantile(sim()[[1]]$res, 0.05),
                      `0.1` = quantile(sim()[[1]]$res, 0.1),
                      mean = mean(sim()[[1]]$res),
                      median = median(sim()[[1]]$res),
                      `0.9` = quantile(sim()[[1]]$res, 0.9),
                      `0.95` = quantile(sim()[[1]]$res, 0.95),
                      `0.975` = quantile(sim()[[1]]$res, 0.975),
                      `0.99` = quantile(sim()[[1]]$res, 0.99),
                      Max. = max(sim()[[1]]$res)
                      )
    })
    output$sim_table <- renderTable(sim_oc_summary())
    output$download_sim_table <- downloadHandler(
        filename = "cutpointr_simulation.csv",
        content = function(file) {
            write.csv(sim_oc_summary(), file, row.names = FALSE)
        }
    )

    output$download_simplot_cutvar <- downloadHandler(
        filename = "cutpointr_variability.png",
        content = function(file) {
            png(file, width = 500, height = 500)
            print(cutvar_reac())
            dev.off()
        })
}
