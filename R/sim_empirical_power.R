#' Simulate Empirical Power for Continuous, Binary, or Survival Outcomes
#'
#' This function performs Monte Carlo simulations to estimate the empirical power
#' for one-sample, paired, or two-arm parallel designs for continuous, binary,
#' and survival outcomes. Survival outcomes can be simulated using either
#' an exponential (constant hazard) or Weibull (time-varying hazard) distribution.
#' Supports serial execution for Shiny apps and multi-core execution for local runs.
#'
#' @param design Character. One of `"continuous"`, `"binary"`, `"survival"`.
#' @param sub_design Character. One of `"parallel"`, `"one_sample"`, `"paired"`.
#' @param nsim Integer. Number of simulations per replication (default 1000).
#' @param n Numeric. Sample size per group (or total for one-sample/paired, default 50).
#' @param delta Numeric. Mean difference for continuous outcomes (required if continuous).
#' @param sd Numeric. Standard deviation for continuous outcomes (required unless paired).
#' @param sd_diff Numeric. SD of differences for paired continuous outcomes.
#' @param p1 Numeric. Event probability for control/baseline (binary outcomes).
#' @param p2 Numeric. Event probability for treatment/paired (binary outcomes).
#' @param HR Numeric. Hazard ratio for survival outcomes.
#' @param lambdaC Numeric. Baseline hazard (or scale) for control group (survival outcomes).
#' @param dropout Numeric. Dropout rate between 0 and 1 (default 0, survival outcomes).
#' @param dist Character. `"exponential"` or `"weibull"` for survival simulation.
#' @param shape Numeric. Weibull shape parameter (only used if dist = "weibull").
#' @param alpha Numeric. Significance level (default 0.05).
#' @param alternative Character. `"two.sided"` or `"one.sided"` (default `"two.sided"`).
#' @param nrep Integer. Number of replications (outer loop, default 100).
#' @param plot Logical. If TRUE, returns a plot of empirical power distribution.
#' @param seed Integer. Optional random seed.
#' @param ncores Integer. Number of CPU cores for local parallel execution. Ignored if mode = "shiny".
#' @param mode Character. `"shiny"` for serial execution (safe on shinyapps.io),
#'   `"local"` for multi-core offline execution. Default `"shiny"`.
#'
#' @return A list with:
#'   \item{mean_power}{Mean empirical power across replications.}
#'   \item{rep_powers}{Vector of empirical power estimates (length = nrep).}
#'   \item{plot_powers}{ggplot object if plot = TRUE.}
#'
#' @examples
#' \dontrun{
#' # One-sample continuous
#' sim_empirical_power(design="continuous", sub_design="one_sample",
#'                     n=50, delta=2, sd=5, nrep=50)
#'
#' # Paired binary
#' sim_empirical_power(design="binary", sub_design="paired",
#'                     n=60, p1=0.3, p2=0.5, nrep=50)
#'
#' # Two-arm parallel survival, exponential
#' sim_empirical_power(design="survival", sub_design="parallel",
#'                     n=40, HR=0.7, lambdaC=0.1, dist="exponential",
#'                     dropout=0.05, nrep=20)
#'
#' # Two-arm parallel survival, Weibull
#' sim_empirical_power(design="survival", sub_design="parallel",
#'                     n=40, HR=0.7, lambdaC=0.1, dist="weibull",
#'                     shape=1.5, dropout=0.05, nrep=20)
#' }
#' @export
sim_empirical_power <- function(
    design = c("continuous", "binary", "survival"),
    sub_design = c("parallel", "one_sample", "paired"),
    nsim = 1000,
    n = 50,
    delta = NULL, sd = NULL, sd_diff = NULL,
    p1 = NULL, p2 = NULL,
    HR = NULL, lambdaC = NULL,
    dropout = 0,
    dist = c("exponential", "weibull"),
    shape = 1,
    alpha = 0.05,
    alternative = c("two.sided", "one.sided"),
    nrep = 100,
    plot = TRUE,
    seed = NULL,
    ncores = max(parallel::detectCores() - 1, 1),
    mode = c("shiny", "local")
) {
  
  design <- match.arg(design)
  sub_design <- match.arg(sub_design)
  alternative <- match.arg(alternative)
  dist <- match.arg(dist)
  mode <- match.arg(mode)
  if (!is.null(seed)) set.seed(seed)
  stopifnot(nsim > 0, n > 1, alpha > 0, alpha < 1)
  
  # --- Input validation ---
  if (design == "continuous") {
    if (sub_design == "paired" && (is.null(delta) || is.null(sd_diff)))
      stop("For paired continuous, provide delta and sd_diff.")
    if (sub_design == "one_sample" && (is.null(delta) || is.null(sd)))
      stop("For one-sample continuous, provide delta and sd.")
    if (sub_design == "parallel" && (is.null(delta) || is.null(sd)))
      stop("For parallel continuous, provide delta and sd.")
  }
  
  if (design == "binary") {
    if (sub_design == "one_sample" && is.null(p1))
      stop("For one-sample binary, specify p1.")
    if (sub_design != "one_sample" && (is.null(p1) || is.null(p2)))
      stop("For binary two-arm or paired, specify p1 and p2.")
  }
  
  if (design == "survival" && (is.null(HR) || is.null(lambdaC)))
    stop("For survival, provide HR and lambdaC.")
  if (design == "survival" && dist == "weibull" && shape <= 0)
    stop("For Weibull survival, 'shape' must be > 0.")
  
  # --- Single simulation function ---
  run_sim <- function() {
    if (design == "continuous") {
      if (sub_design == "one_sample") {
        x <- matrix(rnorm(nsim * n, mean = delta, sd = sd), nrow = nsim)
        se <- sd / sqrt(n)
        tstat <- rowMeans(x) / se
        df <- n - 1
      } else if (sub_design == "paired") {
        diff_mat <- matrix(rnorm(nsim * n, mean = delta, sd = sd_diff), nrow = nsim)
        se <- sd_diff / sqrt(n)
        tstat <- rowMeans(diff_mat) / se
        df <- n - 1
      } else {
        g1 <- matrix(rnorm(nsim * n, mean = 0, sd = sd), nrow = nsim)
        g2 <- matrix(rnorm(nsim * n, mean = delta, sd = sd), nrow = nsim)
        mean_diff <- rowMeans(g2) - rowMeans(g1)
        se <- sqrt(2 * sd^2 / n)
        tstat <- mean_diff / se
        df <- 2 * n - 2
      }
      pvals <- if (alternative == "two.sided") 2 * pt(-abs(tstat), df) else pt(tstat, df, lower.tail = (delta > 0))
      return(mean(pvals < alpha))
    }
    
    if (design == "binary") {
      if (sub_design == "one_sample") {
        x <- rbinom(nsim, n, p1)
        se <- sqrt(p1*(1-p1)/n)
        z <- (x/n - 0)/se
      } else if (sub_design == "paired") {
        x1 <- rbinom(nsim, n, p1)
        x2 <- rbinom(nsim, n, p2)
        diff <- x2 - x1
        se <- sqrt(p1*(1-p1) + p2*(1-p2) - 2*sqrt(p1*(1-p1)*p2*(1-p2)))/sqrt(n)
        z <- diff/se
      } else {
        x1 <- rbinom(nsim, n, p1)
        x2 <- rbinom(nsim, n, p2)
        p_pool <- (x1 + x2)/(2*n)
        se <- sqrt(2 * p_pool * (1 - p_pool)/n)
        z <- (x2/n - x1/n)/se
      }
      pvals <- if (alternative == "two.sided") 2 * pnorm(-abs(z)) else pnorm(z, lower.tail = (p2 > p1))
      return(mean(pvals < alpha))
    }
    
    if (design == "survival") {
      n_total <- 2 * n
      group <- rep(c(0,1), each=n)
      if (dist == "exponential") {
        lambdaT <- lambdaC * HR
        time <- rexp(n_total, rate = ifelse(group == 0, lambdaC, lambdaT))
      } else if (dist == "weibull") {
        # Weibull: scale = lambda^(-1/shape)
        scaleC <- lambdaC^(-1/shape)
        scaleT <- (lambdaC*HR)^(-1/shape)
        time <- c(scaleC * (-log(runif(n)))^(1/shape),
                  scaleT * (-log(runif(n)))^(1/shape))
      }
      status <- rbinom(n_total, 1, 1 - dropout)
      df_surv <- data.frame(time = time, status = status, group = factor(group))
      fit <- survival::coxph(survival::Surv(time, status) ~ group, data=df_surv)
      pval <- summary(fit)$coefficients[, "Pr(>|z|)"]
      if (alternative == "one.sided") pval <- pval/2
      return(as.numeric(pval < alpha))
    }
  }
  
  # --- Replications ---
  if (mode == "shiny") {
    rep_powers <- replicate(nrep, run_sim())
  } else {
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl))
    parallel::clusterExport(cl, varlist=ls(), envir=environment())
    rep_powers <- unlist(parallel::parLapply(cl, seq_len(nrep), function(i) run_sim()))
  }
  
  # --- Plot ---
  if (plot) {
    df <- data.frame(power = rep_powers)
    if (length(unique(rep_powers)) > 1) {
      p <- ggplot2::ggplot(df, ggplot2::aes(x = power)) +
        ggplot2::geom_histogram(ggplot2::aes(y=after_stat(density)), bins=20, fill="skyblue", color="white") +
        ggplot2::geom_density(color="blue", linewidth=1) +
        ggplot2::geom_vline(xintercept = mean(rep_powers), color="darkred", linetype="dashed", linewidth=1) +
        ggplot2::labs(title="Distribution of Empirical Power Estimates", x="Estimated Power", y="Density") +
        ggplot2::theme_minimal()
    } else {
      p <- ggplot2::ggplot(df, ggplot2::aes(x = power)) +
        ggplot2::geom_histogram(bins=5, fill="skyblue", color="white") +
        ggplot2::geom_vline(xintercept = mean(rep_powers), color="darkred", linetype="dashed", linewidth=1) +
        ggplot2::labs(title="Empirical Power (Single Value)", x="Estimated Power", y="Count") +
        ggplot2::theme_minimal()
    }
  } else p <- NULL
  
  list(mean_power = mean(rep_powers),
       rep_powers = rep_powers,
       plot_powers = p)
}
