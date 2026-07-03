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
Practical1_R_for_Finance <- function() {
  code <- '
# ---- Basic R computations ----
x <- c(2, 4, 6, 8, 10)
mean(x); sd(x); var(x)

# ---- Data structures ----
vec <- 1:5
mat <- matrix(1:6, nrow = 2)
lst <- list(name = "Stock A", price = 100.5)
df  <- data.frame(Stock = c("A", "B", "C"), Price = c(100, 150, 200))

print(mat)
print(lst)
print(df)

# ---- Financial calculation: Future Value ----
FV <- function(PV, r, n) PV * (1 + r)^n
FV(1000, 0.08, 5)   # 1000 invested at 8% for 5 years

# ---- Probability & statistics ----
set.seed(1)
returns <- rnorm(100, mean = 0.001, sd = 0.02)   # simulated daily returns
mean(returns); sd(returns)
quantile(returns, 0.05)   # 5th percentile (rough VaR)

# ---- Visualization ----
hist(returns, main = "Simulated Daily Returns", col = "skyblue", breaks = 15)
plot(cumsum(returns), type = "l", main = "Cumulative Returns", col = "darkblue",
     xlab = "Day", ylab = "Cumulative Return")
'
  cat("\n==================== Practical 1: R for Finance ====================\n")
  cat(code)
}

Practical2_R_Warmups <- function() {
  code <- '
# ---- Function ----
compound_interest <- function(P, r, t) P * (1 + r)^t
sapply(1:5, function(t) compound_interest(1000, 0.05, t))

# ---- For loop with control flow ----
for (i in 1:5) {
  if (i %% 2 == 0) {
    cat(i, "is even\\n")
  } else {
    cat(i, "is odd\\n")
  }
}

# ---- While loop ----
i <- 1
total <- 0
while (i <= 10) {
  total <- total + i
  i <- i + 1
}
cat("Sum 1 to 10:", total, "\\n")

# ---- Bootstrapping: estimate CI for mean return ----
set.seed(42)
returns <- rnorm(50, mean = 0.001, sd = 0.02)

boot_means <- replicate(1000, mean(sample(returns, replace = TRUE)))
ci <- quantile(boot_means, c(0.025, 0.975))
cat("Bootstrap 95% CI for mean return:", ci, "\\n")

# ---- Simulation: random walk ----
set.seed(1)
steps <- rnorm(100)
walk <- cumsum(steps)

# ---- Visualization ----
par(mfrow = c(1, 2))
plot(walk, type = "l", main = "Simulated Random Walk", col = "darkred", xlab = "Step")
hist(boot_means, main = "Bootstrap Distribution\\nof Mean Return", col = "lightgreen", breaks = 20)
par(mfrow = c(1, 1))
'
  cat("\n==================== Practical 2: More R Warm-Ups ====================\n")
  cat(code)
}

Practical3_Term_Structure_Splines <- function() {
  code <- '
library(minpack.lm)

# ---- Statistical/financial setup: small synthetic yield curve ----
maturity <- c(0.5, 1, 2, 3, 5, 7, 10, 20, 30)
set.seed(1)
true_yield <- c(0.020, 0.022, 0.025, 0.028, 0.032, 0.035, 0.038, 0.042, 0.044)
yield <- true_yield + rnorm(length(maturity), 0, 0.0008)   # small noise -> "market" yields

# Bond price from yield (zero-coupon, continuous compounding)
bond_price <- function(y, m) exp(-y * m)
prices <- bond_price(yield, maturity)

# ---- Scenario exploration: shock yields up/down ----
shock <- 0.01
yield_up   <- yield + shock
yield_down <- yield - shock

# ---- Model 1: Nelson-Siegel forward/spot rate model ----
ns_model <- function(m, b0, b1, b2, tau) {
  b0 + (b1 + b2) * (tau / m) * (1 - exp(-m / tau)) - b2 * exp(-m / tau)
}

fit_ns <- nlsLM(
  yield ~ ns_model(maturity, b0, b1, b2, tau),
  start = list(b0 = 0.04, b1 = -0.02, b2 = 0.01, tau = 2)
)
summary(fit_ns)

# ---- Model 2: Cubic smoothing spline ----
spline_fit <- smooth.spline(maturity, yield)

# ---- Compare the two model specifications ----
rmse_ns     <- sqrt(mean(residuals(fit_ns)^2))
rmse_spline <- sqrt(mean((yield - predict(spline_fit, maturity)$y)^2))
cat("RMSE Nelson-Siegel:", rmse_ns, "\\n")
cat("RMSE Spline:       ", rmse_spline, "\\n")

# ---- Visualization ----
plot(maturity, yield, pch = 19, main = "Term Structure: Nelson-Siegel vs Spline",
     xlab = "Maturity (yrs)", ylab = "Yield")
lines(maturity, predict(fit_ns), col = "blue", lwd = 2)
lines(spline_fit, col = "red", lwd = 2)
legend("bottomright", legend = c("Nelson-Siegel", "Spline"),
       col = c("blue", "red"), lty = 1, lwd = 2)
'
  cat("\n==================== Practical 3: Term Structure and Splines ====================\n")
  cat(code)
}

Practical4_Market_Risk <- function() {
  code <- '
set.seed(10)
returns <- rnorm(250, mean = 0.0005, sd = 0.015)   # 1 trading year of daily returns

# ---- Parametric (variance-covariance) VaR at 95% ----
mu <- mean(returns)
sigma <- sd(returns)
VaR_param <- -(mu + sigma * qnorm(0.05))

# ---- Historical VaR at 95% ----
VaR_hist <- -quantile(returns, 0.05)

cat("Parametric VaR (95%):", round(VaR_param, 4), "\\n")
cat("Historical VaR (95%):", round(VaR_hist, 4), "\\n")

# ---- Business interpretation: potential $ loss on a portfolio ----
portfolio_value <- 1e6
cat("Estimated 1-day loss (Parametric):", round(VaR_param * portfolio_value, 0), "\\n")
cat("Estimated 1-day loss (Historical): ", round(VaR_hist * portfolio_value, 0), "\\n")

# ---- Visualization ----
hist(returns, breaks = 30, col = "lightblue",
     main = "Return Distribution with VaR Thresholds", xlab = "Daily Return")
abline(v = -VaR_hist, col = "red", lwd = 2)
abline(v = -VaR_param, col = "blue", lwd = 2)
legend("topright", legend = c("Historical VaR", "Parametric VaR"),
       col = c("red", "blue"), lty = 1, lwd = 2)
'
  cat("\n==================== Practical 4: Market Risk ====================\n")
  cat(code)
}

Practical5_Credit_Risk <- function() {
  code <- '
# ---- Simplified credit rating transition matrix ----
states <- c("A", "B", "C", "D")   # D = Default (absorbing state)
P <- matrix(c(
  0.90, 0.08, 0.015, 0.005,
  0.10, 0.80, 0.08,  0.02,
  0.02, 0.15, 0.70,  0.13,
  0.00, 0.00, 0.00,  1.00
), nrow = 4, byrow = TRUE, dimnames = list(states, states))
print(P)

# ---- Simulate one rating path using the Markov chain ----
set.seed(5)
simulate_path <- function(start = "A", steps = 10) {
  path <- start
  current <- start
  for (i in 1:steps) {
    current <- sample(states, 1, prob = P[current, ])
    path <- c(path, current)
    if (current == "D") break
  }
  path
}
simulate_path("B", 10)

# ---- Estimate default probability via Monte Carlo simulation ----
n_sims <- 1000
defaulted <- replicate(n_sims, "D" %in% simulate_path("B", 10))
cat("Simulated 10-yr default probability (starting B):", mean(defaulted), "\\n")

# ---- Hazard rate: instantaneous default intensity from 1-yr survival ----
survival_1yr <- 1 - P["B", "D"]
hazard_rate <- -log(survival_1yr)
cat("1-year hazard rate for B-rated credit:", round(hazard_rate, 4), "\\n")

# ---- n-step transition probabilities (matrix power) ----
P5 <- P %*% P %*% P %*% P %*% P   # 5-year transition matrix
cat("5-year transition matrix:\\n")
print(round(P5, 3))

barplot(defaulted |> table() |> prop.table(), col = c("darkgreen", "darkred"),
        names.arg = c("Survived", "Defaulted"),
        main = "Simulated 10-yr Outcomes (starting B)")
'
  cat("\n==================== Practical 5: Credit Risk ====================\n")
  cat(code)
}

Practical6_Operational_Risk <- function() {
  code <- '
library(evd)
set.seed(7)

# ---- Frequency: number of loss events per year (Poisson) ----
n_years <- 1000
frequency <- rpois(n_years, lambda = 3)

# ---- Severity: loss amount per event (lognormal) ----
severity_fn <- function(n) rlnorm(n, meanlog = 8, sdlog = 1.2)

# ---- Aggregate annual loss (compound frequency-severity model) ----
annual_loss <- sapply(frequency, function(n) if (n == 0) 0 else sum(severity_fn(n)))

cat("Mean annual operational loss:  ", round(mean(annual_loss), 0), "\\n")
cat("Median annual operational loss:", round(median(annual_loss), 0), "\\n")
cat("95% Loss (VaR):                ", round(quantile(annual_loss, 0.95), 0), "\\n")

# ---- Fire losses (a specific severity category) ----
fire_losses <- rlnorm(200, meanlog = 9, sdlog = 1.5)
cat("Mean fire loss:", round(mean(fire_losses), 0), "\\n")
cat("Max fire loss:  ", round(max(fire_losses), 0), "\\n")

# ---- Estimating the extremes: Peaks-Over-Threshold (GPD) ----
threshold <- quantile(fire_losses, 0.90)
gpd_fit <- fpot(fire_losses, threshold = threshold)
summary(gpd_fit)

# ---- Visualization ----
par(mfrow = c(1, 2))
hist(annual_loss, breaks = 30, col = "orange", main = "Annual Operational Loss")
hist(fire_losses, breaks = 30, col = "tomato", main = "Fire Loss Severity")
par(mfrow = c(1, 1))
'
  cat("\n==================== Practical 6: Operational Risk ====================\n")
  cat(code)
}

Practical7_Volatility_GARCH <- function() {
  code <- '
library(rugarch)
set.seed(3)

# ---- Small simulated return series with clustering (fast to fit) ----
n <- 300
returns <- rnorm(n, 0, 0.02)

# ---- Specify and fit AR(1)-GARCH(1,1) model ----
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 0))
)
fit <- ugarchfit(spec, returns)
show(fit)

# ---- Simulate volatility path from the fitted AR-GARCH model ----
sim <- ugarchsim(fit, n.sim = 100, n.start = 0, m.sim = 1)
sim_vol <- sigma(sim)

# ---- Measure risk: 1-day 95% VaR using conditional volatility ----
vol_forecast <- tail(sigma(fit), 1)
VaR_95 <- -qnorm(0.05) * vol_forecast
cat("1-day 95% VaR (GARCH conditional vol):", round(VaR_95, 4), "\\n")

# ---- Visualization ----
par(mfrow = c(1, 2))
plot(sigma(fit), type = "l", main = "Fitted Conditional Volatility", col = "purple")
plot(sim_vol, type = "l", main = "Simulated Future Volatility", col = "darkgreen")
par(mfrow = c(1, 1))
'
  cat("\n==================== Practical 7: Measuring Volatility ====================\n")
  cat(code)
}

Practical8_Portfolio_Analytics <- function() {
  code <- '
library(quadprog)
set.seed(2)

# ---- Small synthetic returns for 4 assets ----
n <- 100
assets <- matrix(rnorm(n * 4, mean = 0.0005, sd = 0.015), ncol = 4)
colnames(assets) <- c("A", "B", "C", "D")

mean_ret <- colMeans(assets)
cov_mat  <- cov(assets)

# ---- Minimum-variance portfolio optimization (fully invested, long-only) ----
n_assets <- 4
Dmat <- 2 * cov_mat
dvec <- rep(0, n_assets)
Amat <- cbind(rep(1, n_assets), diag(n_assets))   # weights sum to 1, weights >= 0
bvec <- c(1, rep(0, n_assets))

sol <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
weights <- sol$solution
names(weights) <- colnames(assets)
cat("Optimal (minimum variance) weights:\\n")
print(round(weights, 3))

# ---- Combine with risk management: portfolio return & risk ----
port_return <- sum(weights * mean_ret)
port_risk   <- sqrt(t(weights) %*% cov_mat %*% weights)
port_VaR95  <- -qnorm(0.05) * port_risk

cat("Expected portfolio return:", round(port_return, 5), "\\n")
cat("Portfolio risk (sd):      ", round(port_risk, 5), "\\n")
cat("Portfolio 95% VaR:        ", round(port_VaR95, 5), "\\n")

# ---- Visualization ----
barplot(weights, main = "Minimum Variance Portfolio Weights",
        col = "steelblue", ylab = "Weight")
'
  cat("\n==================== Practical 8: Portfolio Analytics ====================\n")
  cat(code)
}

Practical9_Monte_Carlo_Risk <- function() {
  code <- '
set.seed(11)

n_sims <- 10000
S0    <- 100    # initial value
mu    <- 0.08   # expected annual return
sigma <- 0.20   # annual volatility
Tt    <- 1      # 1 year horizon

# ---- Simulate terminal values via Geometric Brownian Motion ----
Z  <- rnorm(n_sims)
ST <- S0 * exp((mu - 0.5 * sigma^2) * Tt + sigma * sqrt(Tt) * Z)

# ---- Loss relative to initial value ----
loss <- S0 - ST

# ---- Risk measures ----
VaR_95 <- quantile(loss, 0.95)
ES_95  <- mean(loss[loss > VaR_95])   # Expected Shortfall / Conditional VaR

cat("Monte Carlo 95% VaR:              ", round(VaR_95, 2), "\\n")
cat("Monte Carlo 95% Expected Shortfall:", round(ES_95, 2), "\\n")
cat("Probability of any loss:           ", round(mean(loss > 0), 3), "\\n")

# ---- Visualization ----
par(mfrow = c(1, 2))
hist(ST, breaks = 50, col = "lightgreen", main = "Simulated Terminal Values")
hist(loss, breaks = 50, col = "salmon", main = "Simulated Loss Distribution")
abline(v = VaR_95, col = "red", lwd = 2)
par(mfrow = c(1, 1))
'
  cat("\n==================== Practical 9: Monte Carlo Risk Simulation ====================\n")
  cat(code)
}

Practical10_Shiny_App <- function() {
  code <- '
# ---------------- ANALYTICS LAYER ----------------
library(shiny)

run_analytics <- function(n_sims, mu, sigma, S0) {
  set.seed(123)
  Z    <- rnorm(n_sims)
  ST   <- S0 * exp((mu - 0.5 * sigma^2) + sigma * Z)
  loss <- S0 - ST
  VaR  <- quantile(loss, 0.95)
  list(ST = ST, loss = loss, VaR = VaR)
}

# ---------------- USER INTERFACE (UI) LAYER ----------------
ui <- fluidPage(
  titlePanel("Monte Carlo Risk App"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n_sims", "Number of Simulations", 1000, 20000, 5000, step = 1000),
      sliderInput("mu",     "Expected Return",        0, 0.2, 0.08, step = 0.01),
      sliderInput("sigma",  "Volatility",              0.05, 0.5, 0.2, step = 0.01),
      sliderInput("S0",     "Initial Portfolio Value", 50, 200, 100, step = 10)
    ),
    mainPanel(
      plotOutput("lossPlot"),
      verbatimTextOutput("varText")
    )
  )
)

# ---------------- SERVER LAYER ----------------
server <- function(input, output) {
  results <- reactive({
    run_analytics(input$n_sims, input$mu, input$sigma, input$S0)
  })

  output$lossPlot <- renderPlot({
    hist(results()$loss, breaks = 50, col = "salmon",
         main = "Simulated Loss Distribution", xlab = "Loss")
    abline(v = results()$VaR, col = "red", lwd = 2)
  })

  output$varText <- renderText({
    paste("95% Value at Risk:", round(results()$VaR, 2))
  })
}

# ---------------- APPLICATION GENERATOR ----------------
shinyApp(ui = ui, server = server)
'
  cat("\n==================== Practical 10: Build an App ====================\n")
  cat(code)
}

# ---- Print every practical in order ----
print_all_practicals <- function() {
  Practical1_R_for_Finance()
  Practical2_R_Warmups()
  Practical3_Term_Structure_Splines()
  Practical4_Market_Risk()
  Practical5_Credit_Risk()
  Practical6_Operational_Risk()
  Practical7_Volatility_GARCH()
  Practical8_Portfolio_Analytics()
  Practical9_Monte_Carlo_Risk()
  Practical10_Shiny_App()
}

# ---- Example usage ----
# Practical4_Market_Risk()      # print just Practical 4's code
# print_all_practicals()        # print all 10, in order


#############################################################
# End of library. Run commands() to see all available functions.
#############################################################
