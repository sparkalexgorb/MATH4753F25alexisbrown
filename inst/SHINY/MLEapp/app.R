# Install required packages if not already installed
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("stats4", quietly = TRUE)) install.packages("stats4")

library(shiny)
library(ggplot2)
library(stats4)

# --- MLE Functions for 5 Distributions ---

# Normal Distribution: Params (mean, sd)
mle_normal <- function(data) {
  nLL <- function(mu, sigma) {
    -sum(dnorm(data, mean = mu, sd = sigma, log = TRUE))
  }
  fit <- suppressWarnings(mle(minuslogl = nLL, start = list(mu = mean(data), sigma = sd(data)), method = "L-BFGS-B", lower = c(-Inf, 1e-6)))
  as.list(coef(fit))
}

# Exponential Distribution: Params (rate)
mle_exponential <- function(data) {
  nLL <- function(rate) {
    -sum(dexp(data, rate = rate, log = TRUE))
  }
  fit <- suppressWarnings(mle(minuslogl = nLL, start = list(rate = 1/mean(data)), method = "Brent", lower = 1e-6, upper = 100))
  as.list(coef(fit))
}

# Poisson Distribution: Params (lambda)
mle_poisson <- function(data) {
  nLL <- function(lambda) {
    -sum(dpois(data, lambda = lambda, log = TRUE))
  }
  fit <- suppressWarnings(mle(minuslogl = nLL, start = list(lambda = mean(data)), method = "Brent", lower = 1e-6, upper = 100))
  as.list(coef(fit))
}

# Binomial Distribution: Params (prob, size fixed for this example)
mle_binomial <- function(data) {
  nLL <- function(prob) {
    # Assume size=1 for Bernoulli trials if data allows, otherwise define size.
    # For a general case, we can assume 'size' is known/fixed (e.g., max(data) or defined by user)
    # This example assumes data is count of successes out of a fixed 'size'
    size <- 10 # Fixed size for demonstration
    -sum(dbinom(data, size = size, prob = prob, log = TRUE))
  }
  fit <- suppressWarnings(mle(minuslogl = nLL, start = list(prob = mean(data)/10), method = "Brent", lower = 0, upper = 1))
  as.list(coef(fit))
}

# Gamma Distribution: Params (shape, rate)
mle_gamma <- function(data) {
  nLL <- function(shape, rate) {
    -sum(dgamma(data, shape = shape, rate = rate, log = TRUE))
  }
  # Use an alternative method for multi-parameter optimization
  fit <- suppressWarnings(mle(minuslogl = nLL, start = list(shape = 1, rate = 1/mean(data)), method = "L-BFGS-B", lower = c(1e-6, 1e-6)))
  as.list(coef(fit))
}

# --- Shiny UI ---
ui <- fluidPage(
  titlePanel("Maximum Likelihood Estimation Demonstrator (5 Distributions)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("distribution", "Choose Distribution:",
                  choices = c("Normal", "Exponential", "Poisson", "Binomial", "Gamma")),
      sliderInput("n_samples", "Sample Size (n):", min = 10, max = 1000, value = 100, step = 10),
      actionButton("resample", "Resample Data")
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      h4("True Parameters:"),
      verbatimTextOutput("trueParams"),
      h4("MLE Estimates:"),
      verbatimTextOutput("mleParams")
    )
  )
)

# --- Shiny Server ---
server <- function(input, output, session) {
  
  # Reactive to store simulated data
  simData <- reactiveVal(NULL)
  # Reactive to store true parameters (for display)
  trueParams <- reactiveVal(NULL)
  
  observeEvent(c(input$distribution, input$n_samples, input$resample), {
    n <- input$n_samples
    dist <- input$distribution
    data <- numeric()
    params <- list()
    
    # Simulate data based on selected distribution
    if (dist == "Normal") {
      params <- list(mean = 5, sd = 2)
      data <- rnorm(n, mean = params$mean, sd = params$sd)
    } else if (dist == "Exponential") {
      params <- list(rate = 0.5)
      data <- rexp(n, rate = params$rate)
    } else if (dist == "Poisson") {
      params <- list(lambda = 5)
      data <- rpois(n, lambda = params$lambda)
    } else if (dist == "Binomial") {
      params <- list(size = 10, prob = 0.5)
      data <- rbinom(n, size = params$size, prob = params$prob)
    } else if (dist == "Gamma") {
      params <- list(shape = 2, rate = 0.5)
      data <- rgamma(n, shape = params$shape, rate = params$rate)
    }
    simData(data)
    trueParams(params)
  })
  
  output$distPlot <- renderPlot({
    data <- simData()
    dist <- input$distribution
    
    if (is.null(data)) return()
    
    # Calculate MLE
    mle_estimates <- list()
    if (dist == "Normal") {
      mle_estimates <- mle_normal(data)
    } else if (dist == "Exponential") {
      mle_estimates <- mle_exponential(data)
    } else if (dist == "Poisson") {
      mle_estimates <- mle_poisson(data)
    } else if (dist == "Binomial") {
      mle_estimates <- mle_binomial(data)
    } else if (dist == "Gamma") {
      mle_estimates <- mle_gamma(data)
    }
    
    # Plotting using ggplot2
    df <- data.frame(x = data)
    p <- ggplot(df, aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, color = "white", fill = "skyblue") +
      labs(title = paste("Data Simulation and MLE Fit (", dist, ")"),
           x = "Value", y = "Density") +
      theme_minimal()
    
    # Add distribution curves
    if (dist == "Normal") {
      p <- p + stat_function(fun = dnorm, args = trueParams(), color = "red", linewidth = 1.5, linetype = "dashed")
      p <- p + stat_function(fun = dnorm, args = mle_estimates, color = "blue", linewidth = 1.5)
    } else if (dist == "Exponential") {
      p <- p + stat_function(fun = dexp, args = trueParams(), color = "red", linewidth = 1.5, linetype = "dashed")
      p <- p + stat_function(fun = dexp, args = mle_estimates, color = "blue", linewidth = 1.5)
    } else if (dist == "Poisson") {
      # For discrete, plot points/lines over histogram
      # This is tricky with continuous density plot, a bar chart with dpois would be better.
      # Simplified approach:
      # We skip continuous lines for Poisson in this specific density plot context to avoid misrepresentation
    } else if (dist == "Binomial") {
      # Similar issue as Poisson.
    } else if (dist == "Gamma") {
      p <- p + stat_function(fun = dgamma, args = trueParams(), color = "red", linewidth = 1.5, linetype = "dashed")
      p <- p + stat_function(fun = dgamma, args = mle_estimates, color = "blue", linewidth = 1.5)
    }
    
    # Add legend manually for normal, exp, gamma
    if (dist %in% c("Normal", "Exponential", "Gamma")) {
      p <- p + scale_color_manual(values = c("True" = "red", "MLE" = "blue")) +
        labs(color = "Curve Fit")
    }
    
    print(p)
    
    # Update MLE display output
    output$mleParams <- renderPrint({
      print(mle_estimates)
    })
  })
  
  output$trueParams <- renderPrint({
    print(trueParams())
  })
}

# Run the application
shinyApp(ui = ui, server = server)

