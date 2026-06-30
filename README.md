# icarm <img src="man/figures/logo.png" align="right" height="120"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/Olawaleawe/icarm/workflows/R-CMD-check/badge.svg)](https://github.com/Olawaleawe/icarm/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![CRAN status](https://www.r-pkg.org/badges/version/icarm)](https://CRAN.R-project.org/package=icarm)
<!-- badges: end -->

**icarm** provides a unified, general-purpose R framework for
**Interpretable, Contextual-Accountable and Responsible Machine Learning (ICARM)**
that works with any clean tabular data across any application domain.

> *"Algorithmic decisions must be interpretable, auditable, and fair
> — regardless of domain."*

---

## What makes icarm different?

| Capability | icarm | civic.icarm | DALEX | fairmodels | tidymodels |
|---|---|---|---|---|---|
| Auto-detects task type | **Yes** | Yes | No | No | No |
| Interpretable + extended models | **Yes** | Interpretable only | No | No | No |
| Random Forest / XGBoost / SVM | **Yes** | No | No | No | No |
| Group fairness metrics | **Yes** | Yes | No | Yes | No |
| Probability calibration | **Yes** | Yes | No | No | Yes |
| JSON audit trail | **Yes** | Yes | No | No | No |
| Accountability scorecard | **Yes** | Yes | No | No | No |
| General-purpose (any domain) | **Yes** | Civic/education focus | No | No | No |

icarm is the general-purpose sister package to
[civic.icarm](https://cran.r-project.org/package=civic.icarm).

---

## Installation

```r
# From CRAN (once accepted)
install.packages("icarm")

# Development version from GitHub
remotes::install_github("Olawaleawe/icarm")
```

---

## Quickstart

```r
library(icarm)

# Works with ANY tabular data — task auto-detected
m <- icarm_fit(default ~ ., data = icarm_financial)

# Explain — what drives predictions?
ex <- icarm_explain(m, data = icarm_financial)
icarm_plot_importance(ex)

# Fairness audit across ethnicity
fair <- icarm_fairness(m, icarm_financial,
                       outcome   = "default",
                       protected = "ethnicity",
                       positive  = "Yes")
icarm_plot_fairness(fair, metric = "dp_ratio", ref_line = 0.8)

# Full accountability scorecard
icarm_scorecard(m, icarm_financial,
                outcome   = "default",
                protected = "ethnicity",
                positive  = "Yes",
                project   = "Loan Default Analysis")
```

---

## Model family

```r
# Interpretable (ICARM-compliant)
icarm_fit(y ~ ., data, model = "cart")        # Decision tree
icarm_fit(y ~ ., data, model = "logistic")    # Logistic regression
icarm_fit(y ~ ., data, model = "logistic_l1") # L1-penalised logistic
icarm_fit(y ~ ., data, model = "linear")      # Linear regression
icarm_fit(y ~ ., data, model = "gam")         # Generalised additive
icarm_fit(y ~ ., data, model = "multinomial") # Multinomial logistic

# Extended (post-hoc explanation recommended)
icarm_fit(y ~ ., data, model = "random_forest") # Random forest
icarm_fit(y ~ ., data, model = "xgboost")       # XGBoost
icarm_fit(y ~ ., data, model = "svm")           # Support vector machine
```

---

## Built-in datasets

| Dataset | Rows | Domain | Outcome | Protected attrs |
|---|---|---|---|---|
| `icarm_racism_survey` | 150 | Social science | racism_impact, migrant_status, police_stop | gender, skin_color |
| `icarm_medical` | 500 | Healthcare | readmitted (Yes/No) | gender, insurance |
| `icarm_financial` | 1,000 | Finance | default (Yes/No) | gender, ethnicity |

---

## Key functions

| Function | Description |
|---|---|
| `icarm_fit()` | Train any model — auto-detects task |
| `icarm_split()` | Reproducible train/test split |
| `icarm_metrics()` | Performance metrics for any task |
| `icarm_explain()` | Global feature importance |
| `icarm_explain_local()` | Local per-observation explanation |
| `icarm_fairness()` | Group equity metrics |
| `icarm_equity_summary()` | Pass/fail fairness flags |
| `icarm_calibrate()` | Probability calibration (Brier, ECE) |
| `icarm_thresholds()` | Threshold sweep analysis |
| `icarm_compare()` | Side-by-side model comparison |
| `icarm_audit()` | Reproducible JSON audit trail |
| `icarm_scorecard()` | Full accountability report |

---

## Related package

**civic.icarm** is the civic and political education variant of icarm,
focused on democratic judgment formation and DataCitizen-Pro:
```r
install.packages("civic.icarm")
```

---

## Author

**Prof. Dr. Olushina Olawale Awe**
Alexander von Humboldt Foundation Visiting Professor
Ludwigsburg University of Education (LUE), Germany
[olawaleawe@gmail.com](mailto:olawaleawe@gmail.com)

---

## Citation

```bibtex
@software{awe2025icarm,
  author = {Awe, Olushina Olawale},
  title  = {{icarm}: Interpretable, Accountable and
            Responsible Machine Learning},
  year   = {2025},
  url    = {https://github.com/Olawaleawe/icarm},
  note   = {R package v0.1.0}
}
```
