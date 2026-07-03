# finRiskPractical

Ten worked practicals for a Financial Risk Analytics course, packaged as an
installable R library. Each `Practical*()` function prints the fully
annotated R code for that topic to the console so students can copy it into
a script and run it themselves.

## Install

**From r-universe (once the repo is registered — see below):**

```r
install.packages("finRiskPractical",
  repos = c("https://phanthomx.r-universe.dev", "https://cloud.r-project.org"))
```

**Directly from GitHub:**

```r
# install.packages("remotes")
remotes::install_github("phanthomx/finRiskPractical")
```

## Usage

```r
library(finRiskPractical)

Practical1_R_for_Finance()      # print just Practical 1's code
Practical4_Market_Risk()        # print just Practical 4's code
print_all_practicals()          # print all 10, in order
```

## Contents

| Function | Topic |
|---|---|
| `Practical1_R_for_Finance()` | R basics, data structures, future value |
| `Practical2_R_Warmups()` | Loops, bootstrapping, random walk |
| `Practical3_Term_Structure_Splines()` | Nelson-Siegel vs. smoothing spline |
| `Practical4_Market_Risk()` | Parametric & historical VaR |
| `Practical5_Credit_Risk()` | Markov rating transitions, default probability |
| `Practical6_Operational_Risk()` | Frequency-severity loss modelling, GPD |
| `Practical7_Volatility_GARCH()` | AR(1)-GARCH(1,1) volatility modelling |
| `Practical8_Portfolio_Analytics()` | Minimum-variance portfolio optimisation |
| `Practical9_Monte_Carlo_Risk()` | GBM simulation, VaR & Expected Shortfall |
| `Practical10_Shiny_App()` | Interactive Shiny risk dashboard |

**Note:** the printed code uses `minpack.lm`, `evd`, `rugarch`, `quadprog`,
and `shiny` in a few practicals. This package doesn't require them to
install or load — install them separately if you want to actually run the
printed examples.

## Getting this onto r-universe

1. Push this package (as the repo root, with `DESCRIPTION` at top level) to
   `github.com/phanthomx/finRiskPractical`.
2. In your `phanthomx.r-universe.dev` control repo, add/update
   `packages.json`:

   ```json
   [
     { "package": "finRiskPractical", "url": "https://github.com/phanthomx/finRiskPractical" }
   ]
   ```
3. r-universe polls that control repo and builds automatically. Check
   build status and logs at `https://phanthomx.r-universe.dev/builds`.
