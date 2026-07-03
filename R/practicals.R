#############################################################
# Financial Risk Analytics Practical - Code Library
# PSDSP615b
#
# Usage:
#   source("financial_risk_analytics.R")
#   commands()      -> lists all practical function names + descriptions
#   practical1()     -> prints the code for Practical 1
#   practical2()     -> prints the code for Practical 2
#   ... and so on through practical10()
#############################################################

# ---------------------------------------------------------------
# Metadata table: function name + short description of each practical
# ---------------------------------------------------------------
.practical_info <- data.frame(
  Function    = paste0("practical", 1:10),
  Title       = c(
    "R for Finance",
    "More R Warm-Ups",
    "Term Structure and Splines",
    "Market Risk",
    "Credit Risk",
    "Operational Risk",
    "Measuring Volatility",
    "Portfolio Analytics",
    "Monte Carlo Risk Analysis",
    "Build an App (Shiny)"
  ),
  Description = c(
    "R computations, data structures, financial, probability, and statistics calculations, visualization. Documentation with R Markdown.",
    "Functions, loops, control, bootstrapping, simulation, and more visualization.",
    "Statistical definitions and financial models of bond prices; build a term structure of forward rates; estimate with nonlinear least squares; compare model specifications.",
    "Measure risks using historical and parametric approaches (VaR); interpret results relative to business decisions; visualize market risk.",
    "Use transaction/credit migration data to examine default relationships; simulate default probabilities using Markov chains; explore hazard rates and transition probabilities.",
    "Define frequency and severity; calculate risk of loss and potential loss; fire losses; estimate extremes (VaR).",
    "Fix for volatility clustering; fit AR-GARCH models; simulate volatility from the AR-GARCH model; measure risk of exposures.",
    "Portfolio optimization; combine risk management with portfolio allocations; optimize allocations (mean-variance, ROI/quadprog).",
    "Perform risk analysis using Monte Carlo simulations on a multi-asset portfolio (VaR and Expected Shortfall).",
    "Build a Shiny app with four architectural layers: Analytics, UI, Server, and Application generator for portfolio risk simulation."
  ),
  stringsAsFactors = FALSE
)

#' List all practical functions and their descriptions
#'
#' Prints the function name, title, and description for each practical
#' (practical1 through practical10) contained in this package.
#'
#' @return Invisibly returns the metadata data frame.
#' @export
commands <- function() {
  cat("=============================================================\n")
  cat(" AVAILABLE PRACTICAL FUNCTIONS - Financial Risk Analytics\n")
  cat("=============================================================\n\n")
  for (i in seq_len(nrow(.practical_info))) {
    cat(sprintf("Practical %d\n", i))
    cat(sprintf("  Function    : %s()\n", .practical_info$Function[i]))
    cat(sprintf("  Title       : %s\n", .practical_info$Title[i]))
    cat(sprintf("  Description : %s\n\n", .practical_info$Description[i]))
  }
  invisible(.practical_info)
}

# ---------------------------------------------------------------
# PRACTICAL 1: R for Finance
# ---------------------------------------------------------------
#' Practical 1: R for Finance
#'
#' Prints the R code for Practical 1 (R for Finance) to the console.
#'
#' @return Invisibly returns the code as a character string.
#' @export
practical1 <- function() {
  code <- r"=====(
a <- 10
b <- 5
a+b

a-b

a*b

a/b

a^2

sqrt(16)

log(10)

exp(2)

abs(-5)

v <-c(10,20,30,40)
m<-matrix(1:6,nrow=2,ncol=3)
df <- data.frame(
  name = c("A", "B"),
  marks = c(90, 85)
)
P <- 1000
R <- 5 
T <- 2
SI <- (P * R * T) / 100
CI <- P * (1 + R/100)^T
dnorm(0) 

pnorm(1) 

rnorm(5)

dbinom(2, size = 5, prob = 0.5)

dpois(3, lambda = 2)

x <- c(10, 20, 30, 40, 50)
mean(x)

median(x) 

sd(x) 

var(x)

y <- c(12, 25, 35, 45, 60)
cor(x, y)

plot(x, y)

library(ggplot2)
ggplot(data = df, aes(x = name, y = marks)) +
  geom_bar(stat = "identity")

install.packages("rmarkdown")
summary(cars)

l <- list(name = "adlin", age = 25, marks = c(80, 90))
l
)====="
  cat(code)
  invisible(code)
}

# ---------------------------------------------------------------
# PRACTICAL 2: More R Warm-Ups
# ---------------------------------------------------------------
#' Practical 2: More R Warm-Ups
#'
#' Prints the R code for Practical 2 (More R Warm-Ups) to the console.
#'
#' @return Invisibly returns the code as a character string.
#' @export
practical2 <- function() {
  code <- r"=====(
# 1. Basic Functions in R
my_mean <- function(x) {
sum(x) / length(x)
}
data <- c(10, 20, 30, 40, 50)
my_mean(data)

# 2. Loops (For Loop)
for (i in 1:5) {
print(paste("Square of", i, "=", i^2))
}

# 3. While Loop
i <- 1
while (i <= 5) {
print(i)
i <- i + 1
}

# 4. Control Statements (if-else)
num <- 7
if (num %% 2 == 0) {
print("Even Number")
} else {
print("Odd Number")
}

# 5. Vector Operations
x <- c(5, 10, 15, 20)
x * 2
mean(x)
sd(x)

# 6. Simulation (Random Data Generation)
set.seed(123)
sim_data <- rnorm(100, mean = 50, sd = 10)
head(sim_data)

# Visualization of Simulation
hist(sim_data, col="skyblue", main="Histogram of Simulated Data")
boxplot(sim_data, col="orange", main="Boxplot")
plot(density(sim_data), col="blue", main="Density Plot")

# 7. Bootstrapping (resampling with replacement)
bootstrap_mean <- function(data, n_iter = 1000) {
means <- numeric(n_iter)
for (i in 1:n_iter) {
sample_data <- sample(data, replace = TRUE)
means[i] <- mean(sample_data)
}
return(means)
}

boot_results <- bootstrap_mean(sim_data, 1000)
mean(boot_results)
hist(boot_results, col="green", main="Bootstrap Means Distribution")

# 8. Comparing Original vs Bootstrap Mean
original_mean <- mean(sim_data)
bootstrap_mean_val <- mean(boot_results)
print(paste("Original Mean:", original_mean))
print(paste("Bootstrap Mean:", bootstrap_mean_val))

# 9. Advanced Visualization (ggplot2)
install.packages("ggplot2")
library(ggplot2)
df <- data.frame(values = sim_data)
ggplot(df, aes(x = values)) +
geom_histogram(fill = "blue", bins = 20) +
ggtitle("Histogram using ggplot2")

# 10. Mini Experiment (Simulation + Loop)
results <- c()
for (i in 1:100) {
toss <- sample(c("Head", "Tail"), 1)
results <- c(results, toss)
}
table(results)
)====="
  cat(code)
  invisible(code)
}

# ---------------------------------------------------------------
# PRACTICAL 3: Term Structure and Splines
# ---------------------------------------------------------------
#' Practical 3: Term Structure and Splines
#'
#' Prints the R code for Practical 3 (Term Structure and Splines) to the console.
#'
#' @return Invisibly returns the code as a character string.
#' @export
practical3 <- function() {
  code <- r"=====(
library(splines)
maturity <- c(1, 2, 3)
yield <- c(0.05, 0.06, 0.07)
face_value <- 100
price <- face_value / (1 + yield)^maturity
data.frame(maturity, yield, price)

yield_up <- yield + 0.01
price_up <- face_value / (1 + yield_up)^maturity
yield_down <- yield - 0.01
price_down <- face_value / (1 + yield_down)^maturity
data.frame(
  maturity,
  base_price = price,
  price_when_rate_up = price_up,
  price_when_rate_down = price_down
)

maturity <- c(1, 2, 3)
yield <- c(0.05, 0.06, 0.07)
t_range <- seq(1, 3, length.out = 100)
spline_model <- lm(yield ~ bs(maturity, degree = 3))
smooth_yield <- predict(
  spline_model,
  newdata = data.frame(maturity = t_range)
)
forward_rate <- diff(smooth_yield) / diff(t_range)
plot(maturity, yield,
     pch = 19,
     xlab = "Maturity (Years)",
     ylab = "Yield",
     main = "Term Structure (Spline Model)")
lines(t_range, smooth_yield,
      col = "blue",
      lwd = 2)

plot(t_range[-1], forward_rate,
     type = "l",
     col = "red",
     lwd = 2,
     xlab = "Maturity",
     ylab = "Forward Rate",
     main = "Forward Rates Curve")

head(data.frame(
  maturity = t_range[-1],
  forward_rate = forward_rate
))

install.packages("minpack.lm")
library(minpack.lm)
maturity <- c(1, 2, 3, 4, 5)
yield <- c(0.05, 0.055, 0.06, 0.065, 0.07)
face_value <- 100
price <- face_value / (1 + yield)^maturity
model <- nlsLM(
  price ~ face_value / (1 + (a + b*maturity))^maturity,
  start = list(a = 0.05, b = 0.01)
)
summary(model)

coef(model)

maturity <- c(1, 2, 3, 4, 5)
yield <- c(0.05, 0.055, 0.06, 0.065, 0.07)
poly_model <- lm(yield ~ maturity + I(maturity^2))
library(splines)
spline_model <- lm(yield ~ bs(maturity, degree = 3))
poly_pred <- predict(poly_model)
spline_pred <- predict(spline_model)
poly_error <- sum((yield - poly_pred)^2)
spline_error <- sum((yield - spline_pred)^2)
cat("Polynomial Error:", poly_error, "\n")

cat("Spline Error:", spline_error, "\n")
)====="
  cat(code)
  invisible(code)
}

# ---------------------------------------------------------------
# PRACTICAL 4: Market Risk
# ---------------------------------------------------------------
#' Practical 4: Market Risk
#'
#' Prints the R code for Practical 4 (Market Risk) to the console.
#'
#' @return Invisibly returns the code as a character string.
#' @export
practical4 <- function() {
  code <- r"=====(
install.packages(c("quantmod", "PerformanceAnalytics", "ggplot2"))

library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
getSymbols("AAPL", from = "2022-01-01", to = "2024-01-01")

prices <- na.omit(Cl(AAPL))
returns <- na.omit(dailyReturn(prices))
VaR_hist <- quantile(returns, probs = 0.05)
mean_ret <- mean(returns)
sd_ret <- sd(returns)
VaR_param <- mean_ret + sd_ret * qnorm(0.05)
print(VaR_hist)

print(VaR_param)

ggplot(data = data.frame(returns), aes(x = returns)) +
geom_histogram(bins = 50, fill = "blue", alpha = 0.7) +
geom_vline(xintercept = VaR_hist, color = "red", linetype = "dashed") +
geom_vline(xintercept = VaR_param, color = "green", linetype = "dashed") +
ggtitle("Market Risk Visualization using VaR")
)====="
  cat(code)
  invisible(code)
}

# ---------------------------------------------------------------
# PRACTICAL 5: Credit Risk
# ---------------------------------------------------------------
#' Practical 5: Credit Risk
#'
#' Prints the R code for Practical 5 (Credit Risk) to the console.
#'
#' @return Invisibly returns the code as a character string.
#' @export
practical5 <- function() {
  code <- r"=====(
install.packages("markovchain")
library(markovchain)
states <- c("AAA", "AA", "A", "BBB", "Default")
transition_matrix <- matrix(c(
  0.85, 0.10, 0.03, 0.01, 0.01,
  0.05, 0.80, 0.10, 0.03, 0.02,
  0.02, 0.08, 0.75, 0.10, 0.05,
  0.01, 0.04, 0.10, 0.70, 0.15,
  0.00, 0.00, 0.00, 0.00, 1.00
), byrow = TRUE, nrow = 5)
rownames(transition_matrix) <- states
colnames(transition_matrix) <- states
mc <- new("markovchain", states = states, transitionMatrix = transition_matrix)
simulation <- rmarkovchain(n = 10, object = mc, t0 = "AAA")
print(simulation)

default_probs <- transition_matrix[, "Default"]
print(default_probs)

df <- data.frame(State = states, DefaultProb = default_probs)
ggplot(df, aes(x = State, y = DefaultProb)) +
  geom_bar(stat = "identity") +
  ggtitle("Default Probability by Credit Rating")

hazard_rate <- default_probs / (1 - default_probs)
print(hazard_rate)

two_step <- transition_matrix %*% transition_matrix
print(two_step)
)====="
  cat(code)
  invisible(code)
}

# ---------------------------------------------------------------
# PRACTICAL 6: Operational Risk
# ---------------------------------------------------------------
#' Practical 6: Operational Risk
#'
#' Prints the R code for Practical 6 (Operational Risk) to the console.
#'
#' @return Invisibly returns the code as a character string.
#' @export
practical6 <- function() {
  code <- r"=====(
frequency <- rpois(100, lambda = 5)
severity <- rlnorm(100, meanlog = 10, sdlog = 1)
expected_loss <- mean(frequency) * mean(severity)
aggregate_loss <- sum(frequency * severity)
print(expected_loss)

print(aggregate_loss)

fire_losses <- c(50000, 120000, 30000, 70000, 150000)
print(mean(fire_losses))

print(max(fire_losses))

total_losses <- frequency * severity
VaR_95 <- quantile(total_losses, 0.95)
VaR_99 <- quantile(total_losses, 0.99)
print(VaR_95)

print(VaR_99)
)====="
  cat(code)
  invisible(code)
}

# ---------------------------------------------------------------
# PRACTICAL 7: Measuring Volatility
# ---------------------------------------------------------------
#' Practical 7: Measuring Volatility
#'
#' Prints the R code for Practical 7 (Measuring Volatility) to the console.
#'
#' @return Invisibly returns the code as a character string.
#' @export
practical7 <- function() {
  code <- r"=====(
install.packages("rugarch") 
library(rugarch)
set.seed(123)
returns <- rnorm(500, mean = 0, sd = 0.02)
plot(returns, type="l", main="Returns Series")

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
  mean.model = list(armaOrder = c(1,1)) 
)
fit <- ugarchfit(spec = spec, data = returns)
print(fit)

volatility <- sigma(fit)
plot(volatility, type="l", main="Estimated Volatility")

sim <- ugarchsim(fit, n.sim = 100)
sim_returns <- fitted(sim)
VaR_95 <- quantile(sim_returns, 0.05) 
VaR_99 <- quantile(sim_returns, 0.01)
print(VaR_95)

print(VaR_99)
)====="
  cat(code)
  invisible(code)
}

# ---------------------------------------------------------------
# PRACTICAL 8: Portfolio Analytics
# ---------------------------------------------------------------
#' Practical 8: Portfolio Analytics
#'
#' Prints the R code for Practical 8 (Portfolio Analytics) to the console.
#'
#' @return Invisibly returns the code as a character string.
#' @export
practical8 <- function() {
  code <- r"=====(
install.packages(c("quantmod", "PortfolioAnalytics", "PerformanceAnalytics", "ROI","ROI.plugin.quadprog"))
library(quantmod)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
stocks <- c("AAPL", "MSFT", "GOOG", "AMZN")
getSymbols(stocks, from = "2020-01-01", src = "yahoo")
prices <- na.omit(merge(Cl(AAPL), Cl(MSFT), Cl(GOOG), Cl(AMZN)))
colnames(prices) <- stocks
returns <- na.omit(Return.calculate(prices, method = "log"))
portfolio <- portfolio.spec(assets = colnames(returns))
portfolio <- add.constraint(portfolio, type = "box", min = 0, max = 1)
portfolio <- add.constraint(portfolio, type = "weight_sum", min_sum = 1, max_sum = 1)
portfolio <- add.objective(portfolio, type = "return", name = "mean")
portfolio <- add.objective(portfolio, type = "risk", name = "StdDev")
opt_portfolio <- optimize.portfolio(R = returns, portfolio = portfolio, optimize_method = "ROI")
weights <- extractWeights(opt_portfolio)
print("Optimal Weights")
print(weights)

portfolio_returns <- xts(returns %*% weights, order.by = index(returns))
charts.PerformanceSummary(portfolio_returns, main = "Optimized Portfolio Performance")

print("Portfolio Risk:")
StdDev(portfolio_returns)

print("Sharpe Ratio:")
SharpeRatio(portfolio_returns)

SharpeRatio(portfolio_returns)
weights[abs(weights) < 1e-4] <- 0
cat("\nOptimal Portfolio Weights:\n")
print(round(weights, 4))

portfolio_returns <- xts(returns %*% weights, order.by = index(returns))
port_risk <- StdDev(portfolio_returns)
port_sharpe <- SharpeRatio(portfolio_returns)
cat("\nRisk (Standard Deviation):\n")
print(round(port_risk, 4))

cat("\nSharpe Ratio:\n")
print(round(port_sharpe, 4))
)====="
  cat(code)
  invisible(code)
}

# ---------------------------------------------------------------
# PRACTICAL 9: Monte Carlo Risk Analysis
# ---------------------------------------------------------------
#' Practical 9: Monte Carlo Risk Analysis
#'
#' Prints the R code for Practical 9 (Monte Carlo Risk Analysis) to the console.
#'
#' @return Invisibly returns the code as a character string.
#' @export
practical9 <- function() {
  code <- r"=====(
library(quantmod)
library(PerformanceAnalytics)
stocks <- c("AAPL", "MSFT", "GOOG", "AMZN")
getSymbols(stocks, from = "2020-01-01", src = "yahoo")

prices <- na.omit(merge(Cl(AAPL), Cl(MSFT), Cl(GOOG), Cl(AMZN)))
colnames(prices) <- stocks
returns <- na.omit(Return.calculate(prices, method = "log"))
weights <- c(0.25, 0.25, 0.25, 0.25)
set.seed(123)
n_sim <- 1000 
n_days <- 252
mean_returns <- colMeans(returns)
cov_matrix <- cov(returns)
simulated_returns <- matrix(0, nrow = n_days, ncol = n_sim)
for(i in 1:n_sim){
  sim_data <- MASS::mvrnorm(n_days, mu = mean_returns, Sigma = cov_matrix)
  portfolio_sim <- sim_data %*% weights
  simulated_returns[, i] <- portfolio_sim
}
initial_investment <- 100000
portfolio_values <- apply(simulated_returns, 2, function(x){
  initial_investment * exp(cumsum(x))
})
final_values <- portfolio_values[n_days, ]
VaR_95 <- quantile(final_values, 0.05)
ES_95 <- mean(final_values[final_values <= VaR_95])
matplot(portfolio_values, type = "l", lty = 1,
        main = "Monte Carlo Simulation of Portfolio Value",
        xlab = "Days", ylab = "Portfolio Value")

cat("\n========== MONTE CARLO RISK ANALYSIS ==========\n")
cat("\nInitial Investment:", format(initial_investment, scientific = FALSE), "\n")

cat("\nValue at Risk (95%):\n")
print(round(VaR_95, 2))

cat("\nExpected Shortfall (95%):\n")
print(round(ES_95, 2))
)====="
  cat(code)
  invisible(code)
}

# ---------------------------------------------------------------
# PRACTICAL 10: Build an App (Shiny)
# ---------------------------------------------------------------
#' Practical 10: Build an App (Shiny)
#'
#' Prints the R code for Practical 10 (Build an App (Shiny)) to the console.
#'
#' @return Invisibly returns the code as a character string.
#' @export
practical10 <- function() {
  code <- r"=====(
install.packages("shiny")
install.packages("ggplot2")
install.packages("dplyr")
library(shiny)
library(ggplot2)
library(dplyr)

# Simulation function
run_simulation <- function(n_sims, mean_return, volatility, investment) {
  simulated_returns <- rnorm(n_sims, mean = mean_return, sd = volatility)
  final_values <- investment * (1 + simulated_returns)
  VaR_95 <- quantile(final_values, 0.05)
  ES_95 <- mean(final_values[final_values <= VaR_95])
  return(list(
    final_values = final_values,
    VaR = VaR_95,
    ES = ES_95
  ))
}

# UI
ui <- fluidPage(
  titlePanel("Portfolio Risk Simulation App"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n_sims", "Number of Simulations",
                  min = 100, max = 10000, value = 1000, step = 100),
      sliderInput("mean_return", "Expected Return",
                  min = -0.1, max = 0.2, value = 0.05, step = 0.01),
      sliderInput("volatility", "Volatility",
                  min = 0.01, max = 0.5, value = 0.1, step = 0.01),
      
      sliderInput("investment", "Investment Amount",
                  min = 1000, max = 100000, value = 10000, step = 1000)
    ),
    mainPanel(
      plotOutput("histPlot"),
      verbatimTextOutput("riskText")
    )
  )
)

# Server
server <- function(input, output) {
  sim_data <- reactive({
    run_simulation(
      n_sims = input$n_sims,
      mean_return = input$mean_return,
      volatility = input$volatility,
      investment = input$investment
    )
  })
  output$histPlot <- renderPlot({
    data <- sim_data()
    df <- data.frame(values = data$final_values)
    ggplot(df, aes(x = values)) +
      geom_histogram(bins = 50) +
      labs(
        title = "Distribution of Portfolio Outcomes",
        x = "Final Portfolio Value",
        y = "Frequency"
      )
  })
  output$riskText <- renderText({
    data <- sim_data()
    paste0(
      "Value at Risk (95%): ", round(data$VaR, 2), "\n",
      "Expected Shortfall (95%): ", round(data$ES, 2)
      
    )
  })
}

shinyApp(ui = ui, server = server)
)====="
  cat(code)
  invisible(code)
}

#############################################################
# End of library. Run commands() to see all available functions.
#############################################################
