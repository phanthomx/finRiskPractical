#' Practical 1: R for Finance
#'
#' Prints annotated R code covering basic R computations, data structures,
#' a future value calculation, simple return simulation, and plots.
#'
#' @return Invisibly returns \code{NULL}; called for its printed side effect.
#' @examples
#' Practical1_R_for_Finance()
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
  invisible(NULL)
}

#' Practical 2: More R Warm-Ups
#'
#' Prints annotated R code covering functions, control flow, loops,
#' bootstrapping a confidence interval, and a random walk simulation.
#'
#' @return Invisibly returns \code{NULL}; called for its printed side effect.
#' @examples
#' Practical2_R_Warmups()
#' @export
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
  invisible(NULL)
}

#' Practical 3: Term Structure and Splines
#'
#' Prints annotated R code fitting a Nelson-Siegel model and a cubic
#' smoothing spline to a synthetic yield curve, and comparing RMSE.
#'
#' @return Invisibly returns \code{NULL}; called for its printed side effect.
#' @examples
#' Practical3_Term_Structure_Splines()
#' @export
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
  invisible(NULL)
}

#' Practical 4: Market Risk
#'
#' Prints annotated R code computing parametric and historical
#' Value-at-Risk (VaR) on a simulated daily return series.
#'
#' @return Invisibly returns \code{NULL}; called for its printed side effect.
#' @examples
#' Practical4_Market_Risk()
#' @export
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
  invisible(NULL)
}

#' Practical 5: Credit Risk
#'
#' Prints annotated R code simulating rating migration with a Markov
#' chain transition matrix, Monte Carlo default probabilities, and a
#' hazard rate calculation.
#'
#' @return Invisibly returns \code{NULL}; called for its printed side effect.
#' @examples
#' Practical5_Credit_Risk()
#' @export
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
  invisible(NULL)
}

#' Practical 6: Operational Risk
#'
#' Prints annotated R code for a frequency-severity operational loss
#' model, fire loss severity, and a peaks-over-threshold (GPD) fit.
#'
#' @return Invisibly returns \code{NULL}; called for its printed side effect.
#' @examples
#' Practical6_Operational_Risk()
#' @export
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
  invisible(NULL)
}

#' Practical 7: Measuring Volatility (GARCH)
#'
#' Prints annotated R code fitting an AR(1)-GARCH(1,1) model, simulating
#' a future volatility path, and computing a GARCH-based VaR.
#'
#' @return Invisibly returns \code{NULL}; called for its printed side effect.
#' @examples
#' Practical7_Volatility_GARCH()
#' @export
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
  invisible(NULL)
}

#' Practical 8: Portfolio Analytics
#'
#' Prints annotated R code solving a minimum-variance, long-only
#' portfolio via quadratic programming and computing portfolio VaR.
#'
#' @return Invisibly returns \code{NULL}; called for its printed side effect.
#' @examples
#' Practical8_Portfolio_Analytics()
#' @export
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
  invisible(NULL)
}

#' Practical 9: Monte Carlo Risk Simulation
#'
#' Prints annotated R code simulating terminal asset values via
#' Geometric Brownian Motion and computing Monte Carlo VaR and
#' Expected Shortfall.
#'
#' @return Invisibly returns \code{NULL}; called for its printed side effect.
#' @examples
#' Practical9_Monte_Carlo_Risk()
#' @export
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
  invisible(NULL)
}

#' Practical 10: Build a Shiny Risk App
#'
#' Prints annotated R code for a full Shiny application (analytics,
#' UI, and server layers) implementing an interactive Monte Carlo
#' risk dashboard.
#'
#' @return Invisibly returns \code{NULL}; called for its printed side effect.
#' @examples
#' Practical10_Shiny_App()
#' @export
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
  invisible(NULL)
}

#' Print Every Practical in Order
#'
#' Convenience wrapper that calls all ten \code{Practical*_*()} functions
#' in sequence, printing their annotated code one after another.
#'
#' @return Invisibly returns \code{NULL}; called for its printed side effect.
#' @examples
#' \donttest{
#' print_all_practicals()
#' }
#' @export
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
  invisible(NULL)
}
