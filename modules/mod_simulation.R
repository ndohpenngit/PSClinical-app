mod_simulation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Empirical Power Simulation"),
    fluidRow(
      box(
        title = "Inputs", status = "primary", solidHeader = TRUE, width = 6,
        selectInput(ns("design"), "Design Type",
                    choices = c("Continuous" = "continuous",
                                "Binary" = "binary",
                                "Survival" = "survival")),
        selectInput(ns("sub_design"), "Sub-Design (for Continuous/Binary)",
                    choices = c("Two-Sample Parallel" = "parallel",
                                "One-Sample" = "one_sample",
                                "Paired" = "paired")),
        numericInput(ns("nrep"), "Number of Repetitions (outer loop)", value = 100, min = 1, step = 1),
        numericInput(ns("nsim"), "Simulations per Rep (inner loop)", value = 1000, min = 1, step = 1),
        numericInput(ns("n"), "Sample Size per Group (or total for one-sample/paired)", value = 50, min = 1, step = 1),
        numericInput(ns("alpha"), "Significance Level (α)", value = 0.05, min = 0.001, max = 0.5, step = 0.01),
        selectInput(ns("alternative"), "Alternative Hypothesis", choices = c("two.sided", "one.sided")),
        
        # Continuous Inputs
        conditionalPanel(
          condition = paste0("input['", ns("design"), "'] == 'continuous'"),
          numericInput(ns("delta"), "Mean Difference (Δ)", value = 1),
          numericInput(ns("sd"), "Standard Deviation (σ)", value = 2),
          conditionalPanel(
            condition = paste0("input['", ns("sub_design"), "'] == 'paired'"),
            numericInput(ns("sd_diff"), "SD of Differences (σ_diff)", value = 1)
          )
        ),
        
        # Binary Inputs
        conditionalPanel(
          condition = paste0("input['", ns("design"), "'] == 'binary'"),
          numericInput(ns("p1"), "Probability (Control / baseline)", value = 0.3, min = 0, max = 1),
          conditionalPanel(
            condition = paste0("input['", ns("sub_design"), "'] != 'one_sample'"),
            numericInput(ns("p2"), "Probability (Treatment / paired)", value = 0.5, min = 0, max = 1)
          )
        ),
        
        # Survival Inputs
        conditionalPanel(
          condition = paste0("input['", ns("design"), "'] == 'survival'"),
          numericInput(ns("HR"), "Hazard Ratio (Treatment vs Control)", value = 0.7, min = 0.01, max = 2, step = 0.01),
          numericInput(ns("lambdaC"), "Baseline Hazard (Control)", value = 0.1, min = 0.001),
          selectInput(ns("dist"), "Survival Distribution",
                      choices = c("Exponential" = "exponential", "Weibull" = "weibull"), selected = "exponential"),
          conditionalPanel(
            condition = paste0("input['", ns("dist"), "'] == 'weibull'"),
            numericInput(ns("shape"), "Weibull Shape Parameter", value = 1.5, min = 0.01)
          ),
          helpText("Exponential: constant hazard over time. Weibull: allows increasing/decreasing hazard; shape >1 increases hazard, <1 decreases."),
          numericInput(ns("dropout"), "Annual Dropout Rate", value = 0, min = 0, max = 1, step = 0.01)
        ),
        
        actionButton(ns("go"), "Compute Empirical Power", class = "btn-primary")
      ),
      
      # Results Box
      conditionalPanel(
        condition = paste0("input['", ns("go"), "'] > 0"),
        box(
          title = "Results", status = NULL, solidHeader = TRUE, width = 6, collapsible = TRUE,
          verbatimTextOutput(ns("result")),
          hr(),
          plotly::plotlyOutput(ns("simPlot"), height = "300px") %>% withSpinner(type = 8)
        )
      )
    ),
    
    # Instructions Box
    fluidRow(
      box(
        title = "Instructions",
        width = 12,
        solidHeader = FALSE,
        status = NULL,
        collapsible = TRUE,
        collapsed = TRUE,
        p("This module performs Monte Carlo simulations to estimate empirical power for different study designs and outcomes."),
        tags$ul(
          tags$li("Select the outcome type: Continuous, Binary, or Survival."),
          tags$li("For Continuous and Binary outcomes, select the sub-design: Two-Sample Parallel, One-Sample, or Paired."),
          tags$li("Specify sample sizes, number of repetitions (nrep), and number of simulations per repetition (nsim)."),
          tags$li("For Continuous outcomes, provide mean difference and standard deviation (or SD of differences for paired design)."),
          tags$li("For Binary outcomes, provide probabilities for control/baseline (p1) and treatment/paired (p2) as applicable."),
          tags$li("For Survival outcomes, provide hazard ratio, baseline hazard, dropout rate, and select survival distribution (Exponential or Weibull)."),
          tags$li("For Weibull, specify shape parameter; shape >1 implies increasing hazard, <1 decreasing hazard."),
          tags$li("Set the significance level (α) and select the alternative hypothesis (one-sided or two-sided)."),
          tags$li("Click 'Compute Empirical Power' to run the simulations. Results and distribution plots will appear in the Results panel."),
          tags$li("Ensure input values are consistent with the selected design and sub-design to avoid errors.")
        )
      )
    )
  )
}

mod_simulation_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    sim_result <- eventReactive(input$go, {
      req(input$design)
      sub_design <- ifelse(is.null(input$sub_design), "parallel", input$sub_design)
      
      sim_empirical_power(
        design = input$design,
        sub_design = sub_design,
        nsim = input$nsim,
        n = input$n,
        delta = if(input$design=="continuous") input$delta else NULL,
        sd = if(input$design=="continuous") input$sd else NULL,
        sd_diff = if(input$design=="continuous" && sub_design=="paired") input$sd_diff else NULL,
        p1 = if(input$design=="binary") input$p1 else NULL,
        p2 = if(input$design=="binary" && sub_design %in% c("paired","parallel")) input$p2 else NULL,
        HR = if(input$design=="survival") input$HR else NULL,
        lambdaC = if(input$design=="survival") input$lambdaC else NULL,
        dist = if(input$design=="survival") input$dist else NULL,
        shape = if(input$design=="survival" && input$dist=="weibull") input$shape else NULL,
        dropout = if(input$design=="survival") input$dropout else 0,
        alpha = input$alpha,
        alternative = input$alternative,
        nrep = input$nrep,
        plot = FALSE,
        ncores = max(parallel::detectCores() - 1, 1)
      )
    })
    
    # --- Summary Output ---
    output$result <- renderPrint({
      req(sim_result())
      res <- sim_result()
      powers <- res$rep_powers
      mean_power <- res$mean_power
      sd_power <- sd(powers)
      nrep <- length(powers)
      
      se_power <- if(nrep > 1) sd_power / sqrt(nrep) else NA
      ci_lower <- if(nrep > 1) mean_power - 1.96*se_power else NA
      ci_upper <- if(nrep > 1) mean_power + 1.96*se_power else NA
      
      cat("Summary of Empirical Power Estimates:\n")
      print(summary(powers))
      cat("\nMean Empirical Power:", round(mean_power, 3))
      cat("\nMonte Carlo SE:", round(se_power, 4))
      cat("\n95% Monte Carlo CI: [", round(ci_lower, 3), ", ", round(ci_upper, 3), "]\n")
    })
    
    # --- Plot ---
    output$simPlot <- renderPlotly({
      req(sim_result())
      powers <- sim_result()$rep_powers
      df <- data.frame(power = powers)
      
      if(length(unique(powers)) > 1){
        p <- ggplot(df, aes(x = power)) +
          geom_histogram(aes(y=after_stat(density)), bins=20, fill="skyblue", color="white") +
          geom_density(color="blue", linewidth=1) +
          geom_vline(aes(xintercept = mean(power)), color="darkred", linetype="dashed", linewidth=1) +
          labs(title="Distribution of Empirical Power Estimates", x="Empirical Power", y="Density") +
          theme_minimal()
      } else {
        p <- ggplot(df, aes(x = power)) +
          geom_histogram(bins=5, fill="skyblue", color="white") +
          geom_vline(aes(xintercept = mean(power)), color="darkred", linetype="dashed", linewidth=1) +
          labs(title="Empirical Power (Single Value)", x="Empirical Power", y="Count") +
          theme_minimal()
      }
      
      ggplotly(p)
    })
  })
}
