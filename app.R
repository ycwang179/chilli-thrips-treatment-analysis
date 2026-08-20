# ============================================================
# Chilli Thrips Treatment Analysis Dashboard
# ============================================================

library(shiny)
library(bslib)
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(DT)


# ============================================================
# 1. Import client-provided averaged dataset
# ============================================================

raw_data <- read.csv(
  "data/Treatment_data_averaged.csv",
  check.names = FALSE
)


# ============================================================
# 2. Initial data checks
# ============================================================

dim(raw_data)
names(raw_data)
str(raw_data)
head(raw_data)
summary(raw_data)

# Basic structure
nrow(raw_data)
n_distinct(raw_data$Plot_Name)
unique(raw_data$Plot_Name)
n_distinct(raw_data$Plot_Row_ID)
sort(unique(raw_data$Week))

# Missing values
colSums(is.na(raw_data))

# Observations by treatment
raw_data %>%
  count(Plot_Name)

# Observations by week
raw_data %>%
  count(Week)

# Repeated measurements per plot
raw_data %>%
  count(Plot_Row_ID)

# Check duplicate Plot × Week combinations
raw_data %>%
  count(Plot_Row_ID, Week) %>%
  filter(n > 1)

# Temperature variation by week
raw_data %>%
  group_by(Week) %>%
  summarise(
    n_temp_values = n_distinct(`60cm_T_avg`),
    min_temp = min(`60cm_T_avg`),
    max_temp = max(`60cm_T_avg`),
    .groups = "drop"
  )


# ============================================================
# 3. Prepare analysis-ready variables
# ============================================================

analysis_data <- raw_data %>%
  mutate(
    
    # Treatment
    Treatment = factor(
      Plot_Name,
      levels = c(
        "Control",
        "Pesticide",
        "Vehicle",
        "Manual"
      )
    ),
    
    # Convert study week to meaningful time labels
    Time = case_when(
      Week == 18 ~ "Pretreatment",
      Week == 19 ~ "Day 7",
      Week == 20 ~ "Day 14",
      Week == 21 ~ "Day 21",
      Week == 22 ~ "Day 28",
      TRUE ~ NA_character_
    ),
    
    Time = factor(
      Time,
      levels = c(
        "Pretreatment",
        "Day 7",
        "Day 14",
        "Day 21",
        "Day 28"
      )
    ),
    
    # Experimental unit
    Plot = factor(Plot_Row_ID),
    
    # Field row
    Row = factor(Row),
    
    # Analysis variables
    Severity = Thrips_Rating_Uche,
    Temperature = `60cm_T_avg`
  )


# ============================================================
# 4. Validate initial analysis structure
# ============================================================

glimpse(analysis_data)

levels(analysis_data$Treatment)
levels(analysis_data$Time)

# Number of unique plots
n_distinct(analysis_data$Plot)

# Plot, treatment, and row relationship
analysis_data %>%
  distinct(Plot, Treatment, Row) %>%
  arrange(Treatment, Row)

# Treatment × Time balance
analysis_data %>%
  count(Treatment, Time)

# One observation per Plot × Time
analysis_data %>%
  count(Plot, Time) %>%
  count(n, name = "n_combinations")

# Missing Plot × Time combinations
analysis_data %>%
  complete(Plot, Time) %>%
  filter(is.na(Severity))


# ============================================================
# 5. Reconstruct field column from documented study layout
# ============================================================

# Documented 4 × 4 field layout:
#
#          Col1       Col2       Col3        Col4
# Row 1    Vehicle    Manual     Pesticide   Control
# Row 2    Manual     Control    Vehicle     Pesticide
# Row 3    Control    Pesticide  Manual      Vehicle
# Row 4    Pesticide  Vehicle    Control     Manual

field_layout <- tribble(
  ~Row_key, ~Treatment_key, ~Column,
  
  "1", "Vehicle",   "1",
  "1", "Manual",    "2",
  "1", "Pesticide", "3",
  "1", "Control",   "4",
  
  "2", "Manual",    "1",
  "2", "Control",   "2",
  "2", "Vehicle",   "3",
  "2", "Pesticide", "4",
  
  "3", "Control",   "1",
  "3", "Pesticide", "2",
  "3", "Manual",    "3",
  "3", "Vehicle",   "4",
  
  "4", "Pesticide", "1",
  "4", "Vehicle",   "2",
  "4", "Control",   "3",
  "4", "Manual",    "4"
)


analysis_data <- analysis_data %>%
  mutate(
    Row_key = as.character(Row),
    Treatment_key = as.character(Treatment)
  ) %>%
  
  left_join(
    field_layout,
    by = c(
      "Row_key",
      "Treatment_key"
    )
  ) %>%
  
  mutate(
    Column = factor(
      Column,
      levels = c("1", "2", "3", "4")
    )
  ) %>%
  
  select(
    -Row_key,
    -Treatment_key
  )


# Check reconstructed field layout
layout_check <- analysis_data %>%
  distinct(
    Row,
    Column,
    Treatment
  ) %>%
  arrange(
    Row,
    Column
  )

layout_check


# Check that all observations received a column
sum(is.na(analysis_data$Column))

if (any(is.na(analysis_data$Column))) {
  stop(
    "Some observations could not be matched to the documented field layout."
  )
}


# ============================================================
# 6. Create final model dataset
# ============================================================

model_data <- analysis_data %>%
  select(
    Plot,
    Treatment,
    Row,
    Column,
    Time,
    Severity,
    N_plants,
    Temperature
  )


# Final structure check
glimpse(model_data)


# ============================================================
# 7. Dashboard summary statistics
# ============================================================

n_observations <- nrow(model_data)

n_treatments <- n_distinct(model_data$Treatment)

n_plots <- n_distinct(model_data$Plot)

n_timepoints <- n_distinct(model_data$Time)


# Missing Plot × Time combinations
missing_plot_time <- model_data %>%
  complete(Plot, Time) %>%
  summarise(
    n_missing = sum(is.na(Severity))
  ) %>%
  pull(n_missing)


# Treatment × Time counts
treatment_time_counts <- model_data %>%
  count(Treatment, Time)


# Preview for dashboard
data_preview <- model_data %>%
  arrange(
    Treatment,
    Plot,
    Time
  ) %>%
  select(
    Plot,
    Treatment,
    Row,
    Column,
    Time,
    Severity,
    N_plants,
    Temperature
  )


# ============================================================
# 8. Dashboard theme and colors
# ============================================================

uf_blue   <- "#0021A5"
uf_orange <- "#FA4616"

treatment_colors <- c(
  "Control"   = "#1F4E79",
  "Pesticide" = "#8C8C8C",
  "Vehicle"   = "#E4572E",
  "Manual"    = "#2E8B57"
)


app_theme <- bs_theme(
  version = 5,
  bg = "#F7F8FA",
  fg = "#202124",
  primary = uf_blue,
  secondary = "#6C757D"
)


# ============================================================
# 9. EDA summary data
# ============================================================

# Pretreatment mean and SD
pretreatment_summary <- model_data %>%
  filter(
    Time == "Pretreatment"
  ) %>%
  group_by(
    Treatment
  ) %>%
  summarise(
    Mean = mean(Severity),
    SD = sd(Severity),
    .groups = "drop"
  )


# Mean severity over time
severity_summary <- model_data %>%
  group_by(
    Treatment,
    Time
  ) %>%
  summarise(
    Mean = mean(Severity),
    SE = sd(Severity) / sqrt(n()),
    .groups = "drop"
  )


# ============================================================
# 10. Fit primary repeated-measures mixed model
# ============================================================

primary_model <- lmerTest::lmer(
  
  Severity ~
    Treatment * Time +
    Row +
    Column +
    (1 | Plot),
  
  data = model_data,
  
  REML = TRUE,
  
  contrasts = list(
    Treatment = "contr.sum",
    Time = "contr.sum",
    Row = "contr.sum",
    Column = "contr.sum"
  )
)


# ============================================================
# 11. Model checks and omnibus tests
# ============================================================

# Check whether the random-effect structure is singular
model_is_singular <- lme4::isSingular(
  primary_model,
  tol = 1e-4
)


# Type III tests using Satterthwaite degrees of freedom
model_anova <- stats::anova(
  primary_model,
  type = 3,
  ddf = "Satterthwaite"
)


# Clean result table for dashboard
model_test_results <- model_anova %>%
  as.data.frame() %>%
  rownames_to_column(
    var = "Effect"
  ) %>%
  transmute(
    Effect = Effect,
    NumDF = NumDF,
    DenDF = DenDF,
    F_value = `F value`,
    p_value = `Pr(>F)`
  )

# ============================================================
# Model fit summary for dashboard
# ============================================================

variance_components <- as.data.frame(
  VarCorr(primary_model)
)

plot_random_sd <- variance_components %>%
  filter(
    grp == "Plot",
    is.na(var2)
  ) %>%
  pull(sdcor)

residual_sd <- variance_components %>%
  filter(
    grp == "Residual"
  ) %>%
  pull(sdcor)


# Plot-level intraclass correlation (ICC)
# ICC = plot variance / (plot variance + residual variance)
plot_icc <- (plot_random_sd^2) /
  ((plot_random_sd^2) + (residual_sd^2))


# Temperature structure used to support the modeling decision
#
# Time is treated as a categorical factor here. The goal is descriptive:
# to summarize how much of the observed temperature variation occurs
# between study time points rather than within the same time point.
temperature_structure <- model_data %>%
  group_by(
    Time
  ) %>%
  summarise(
    n_temperature_values = n_distinct(Temperature),
    mean_temperature = mean(Temperature, na.rm = TRUE),
    sd_temperature = sd(Temperature, na.rm = TRUE),
    min_temperature = min(Temperature, na.rm = TRUE),
    max_temperature = max(Temperature, na.rm = TRUE),
    .groups = "drop"
  )

n_single_temperature_times <- sum(
  temperature_structure$n_temperature_values == 1
)

n_two_temperature_times <- sum(
  temperature_structure$n_temperature_values == 2
)

# Exploratory categorical-Time model for temperature.
# R-squared is used only as a descriptive measure of how much observed
# temperature variation is accounted for by categorical study-time differences.
# It is not a causal test and does not imply that Time causes Temperature.
temperature_time_model <- stats::lm(
  Temperature ~ Time,
  data = model_data
)

temperature_time_r2 <- summary(
  temperature_time_model
)$r.squared

temperature_time_percent <- 100 * temperature_time_r2

temperature_evidence_text <- paste0(
  n_single_temperature_times,
  " of the ",
  n_timepoints,
  " time points had a single temperature value across all plots; ",
  "the remaining ",
  n_two_temperature_times,
  " time points had only two observed temperature values. ",
  "In an exploratory model treating Time as a categorical predictor, ",
  "Time accounted for approximately ",
  sprintf("%.1f", temperature_time_percent),
  "% of the observed temperature variation (R² = ",
  sprintf("%.2f", temperature_time_r2),
  ")."
)

cat("\n============================================\n")
cat("EXPLORATORY TEMPERATURE ~ TIME SUMMARY\n")
cat("============================================\n")
print(temperature_structure)
cat(
  "Categorical-Time R-squared:",
  sprintf("%.3f", temperature_time_r2),
  "\n"
)


model_fit_summary <- tibble(
  Metric = c(
    "Observations",
    "Independent plots",
    "Repeated time points",
    "Random effect",
    "Plot ICC",
    "Plot random-effect SD",
    "Residual SD"
  ),
  
  Value = c(
    nrow(model_data),
    n_distinct(model_data$Plot),
    n_distinct(model_data$Time),
    "Random intercept for Plot",
    sprintf(
      "%.2f",
      plot_icc
    ),
    sprintf(
      "%.3f",
      plot_random_sd
    ),
    sprintf(
      "%.3f",
      residual_sd
    )
  )
)

model_fit_summary

# Print results to console
cat("\n============================================\n")
cat("PRIMARY MIXED MODEL - TYPE III TESTS\n")
cat("============================================\n")

print(model_anova)

cat("\nSingular model:", model_is_singular, "\n")

# ============================================================
# 12. Follow-up analysis:
#     Improvement from Pretreatment to Day 28
# ============================================================

# Estimated marginal means of Time within each Treatment
emm_time_by_treatment <- emmeans::emmeans(
  primary_model,
  ~ Time | Treatment
)


# ------------------------------------------------------------
# Improvement within each treatment
#
# Improvement = Pretreatment - Day 28
#
# Time order is:
# Pretreatment, Day 7, Day 14, Day 21, Day 28
# ------------------------------------------------------------

improvement_by_treatment <- emmeans::contrast(
  emm_time_by_treatment,
  
  method = list(
    "Pretreatment - Day 28" = c(
      1,   # Pretreatment
      0,   # Day 7
      0,   # Day 14
      0,   # Day 21
      -1    # Day 28
    )
  ),
  
  by = "Treatment",
  
  adjust = "none"
)


cat("\n============================================\n")
cat("IMPROVEMENT: PRETREATMENT - DAY 28\n")
cat("============================================\n")

print(
  summary(
    improvement_by_treatment,
    infer = c(TRUE, TRUE)
  )
)


# ============================================================
# Compare treatment improvements with Control
# ============================================================

# Remove Treatment as a by-group so that the four
# treatment-specific improvements can be compared directly
improvement_for_comparison <- update(
  improvement_by_treatment,
  by = NULL
)


# Control is the first Treatment level:
# Control, Pesticide, Vehicle, Manual
improvement_vs_control <- emmeans::contrast(
  improvement_for_comparison,
  
  method = "trt.vs.ctrl",
  
  ref = 1
)


# ------------------------------------------------------------
# Multiplicity-adjusted inference
#
# mvt gives a simultaneous multivariate-t adjustment
# for the three treatment-vs-Control comparisons.
# ------------------------------------------------------------

set.seed(1234)

improvement_vs_control_results <- summary(
  improvement_vs_control,
  infer = c(TRUE, TRUE),
  adjust = "mvt"
)


cat("\n============================================\n")
cat("IMPROVEMENT VS CONTROL - ADJUSTED\n")
cat("============================================\n")

print(
  improvement_vs_control_results
)

# ============================================================
# 13. Diagnostic data
# ============================================================

# Conditional residuals and fitted values from the primary mixed model
diagnostic_data <- model_data %>%
  mutate(
    Fitted = fitted(primary_model),
    Residual = residuals(primary_model),
    StdResidual = Residual / residual_sd
  )

# Quick numerical checks for the diagnostics tab
max_abs_std_resid <- max(abs(diagnostic_data$StdResidual), na.rm = TRUE)



# ============================================================
# 14. Results data for dashboard
# ============================================================

# ------------------------------------------------------------
# Helper for displaying p-values
# ------------------------------------------------------------

format_p_value <- function(p) {
  
  if (length(p) == 0 || is.na(p)) {
    return("NA")
  }
  
  if (p < 0.0001) {
    return("< 0.0001")
  }
  
  sprintf("%.3f", p)
}


# ------------------------------------------------------------
# Key omnibus p-values
# ------------------------------------------------------------

treatment_p <- model_test_results %>%
  filter(Effect == "Treatment") %>%
  pull(p_value)

time_p <- model_test_results %>%
  filter(Effect == "Time") %>%
  pull(p_value)

interaction_p <- model_test_results %>%
  filter(Effect == "Treatment:Time") %>%
  pull(p_value)


# ------------------------------------------------------------
# Observed change from Pretreatment to Day 28
#
# Change = Day 28 - Pretreatment
# Negative values indicate improvement because lower severity
# is better.
# ------------------------------------------------------------

observed_change_summary <- model_data %>%
  filter(
    Time %in% c(
      "Pretreatment",
      "Day 28"
    )
  ) %>%
  group_by(
    Treatment,
    Time
  ) %>%
  summarise(
    Mean = mean(Severity),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Time,
    values_from = Mean
  ) %>%
  mutate(
    Change = `Day 28` - Pretreatment
  ) %>%
  arrange(
    Treatment
  )


# ------------------------------------------------------------
# Clean treatment-vs-Control contrast results
#
# Positive estimates indicate greater improvement than Control.
# ------------------------------------------------------------

contrast_results <- improvement_vs_control_results %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(
    
    Treatment = case_when(
      str_detect(
        as.character(contrast),
        "Pesticide"
      ) ~ "Pesticide",
      
      str_detect(
        as.character(contrast),
        "Vehicle"
      ) ~ "Vehicle",
      
      str_detect(
        as.character(contrast),
        "Manual"
      ) ~ "Manual",
      
      TRUE ~ NA_character_
    ),
    
    Treatment = factor(
      Treatment,
      levels = c(
        "Pesticide",
        "Vehicle",
        "Manual"
      )
    ),
    
    Comparison = paste(
      Treatment,
      "vs Control"
    )
  )


# Vehicle result used in the recommendation text
vehicle_result <- contrast_results %>%
  filter(
    Treatment == "Vehicle"
  ) %>%
  slice(1)

vehicle_estimate <- vehicle_result$estimate
vehicle_lower_ci <- vehicle_result$lower.CL
vehicle_upper_ci <- vehicle_result$upper.CL
vehicle_adjusted_p <- vehicle_result$p.value


# ============================================================
# 15. Exploratory future-study power-planning results
# ============================================================
#
# These are PRECOMPUTED exploratory simulation results.
# The dashboard does NOT rerun the simulation when a user changes
# the dropdown selections. It only displays the results from the
# completed 200-simulation-per-scenario analysis.
#
# Power definition:
#   - Target comparison: Vehicle vs Control difference in improvement
#     from Pretreatment to Day 28.
#   - Improvement = Pretreatment - Day 28.
#   - Each simulated study refits the repeated-measures mixed model.
#   - The three treatment-vs-Control comparisons are adjusted using
#     the multivariate-t method.
#   - A simulation counts as successful for power when the adjusted
#     Vehicle-vs-Control p-value is < 0.05.
#
# Monte Carlo uncertainty:
#   - MCSE ≈ sqrt(Power * (1 - Power) / N_sim)
#   - Approximate 95% MC interval ≈ Power ± 1.96 * MCSE,
#     truncated to the [0, 1] range.
#   - This interval reflects uncertainty caused by the finite number
#     of simulation runs. It is NOT a confidence interval for the
#     treatment effect.
#   - At a boundary estimate such as 200/200 successful simulations,
#     the simple normal approximation gives 100% to 100%. This does
#     not prove that the true power is exactly 100%.
#
# The effect and variance inputs are pilot-based, so these results
# should be interpreted as planning guidance rather than a definitive
# sample-size requirement.
# ============================================================

power_results <- tribble(
  ~Scenario,
  ~TargetEffect,
  ~PlotsPerTreatment,
  ~TotalPlots,
  ~SimulationsRequested,
  ~SuccessfulAnalyses,
  ~MeanEstimatedEffect,
  ~Power,
  ~MonteCarloSE,
  ~ApproxPowerLower95,
  ~ApproxPowerUpper95,
  "Conservative (0.50)", 0.500000, 4, 16, 200, 200, 0.460918, 0.105000, 0.021677, 0.062514, 0.147486,
  "Conservative (0.50)", 0.500000, 8, 32, 200, 200, 0.504876, 0.295000, 0.032247, 0.231796, 0.358204,
  "Conservative (0.50)", 0.500000, 12, 48, 200, 200, 0.522274, 0.495000, 0.035354, 0.425707, 0.564293,
  "Conservative (0.50)", 0.500000, 16, 64, 200, 200, 0.483183, 0.540000, 0.035242, 0.470926, 0.609074,
  "Conservative (0.50)", 0.500000, 20, 80, 200, 200, 0.497988, 0.710000, 0.032086, 0.647112, 0.772888,
  "Moderate (0.70)", 0.700000, 4, 16, 200, 200, 0.661715, 0.265000, 0.031207, 0.203834, 0.326166,
  "Moderate (0.70)", 0.700000, 8, 32, 200, 200, 0.689039, 0.570000, 0.035007, 0.501386, 0.638614,
  "Moderate (0.70)", 0.700000, 12, 48, 200, 200, 0.702235, 0.775000, 0.029528, 0.717126, 0.832874,
  "Moderate (0.70)", 0.700000, 16, 64, 200, 200, 0.704748, 0.905000, 0.020733, 0.864362, 0.945638,
  "Moderate (0.70)", 0.700000, 20, 80, 200, 200, 0.696670, 0.975000, 0.011040, 0.953362, 0.996638,
  "Pilot estimate (0.89)", 0.894852, 4, 16, 200, 200, 0.907704, 0.465000, 0.035269, 0.395874, 0.534126,
  "Pilot estimate (0.89)", 0.894852, 8, 32, 200, 200, 0.924856, 0.875000, 0.023385, 0.829165, 0.920835,
  "Pilot estimate (0.89)", 0.894852, 12, 48, 200, 200, 0.883845, 0.960000, 0.013856, 0.932841, 0.987159,
  "Pilot estimate (0.89)", 0.894852, 16, 64, 200, 200, 0.879091, 0.985000, 0.008595, 0.968154, 1.000000,
  "Pilot estimate (0.89)", 0.894852, 20, 80, 200, 200, 0.875575, 1.000000, 0.000000, 1.000000, 1.000000
)


power_scenario_levels <- c(
  "Conservative (0.50)",
  "Moderate (0.70)",
  "Pilot estimate (0.89)"
)


power_results <- power_results %>%
  mutate(
    Scenario = factor(
      Scenario,
      levels = power_scenario_levels
    )
  ) %>%
  arrange(
    Scenario,
    PlotsPerTreatment
  )


power_scenario_colors <- c(
  "Conservative (0.50)" = "#8C8C8C",
  "Moderate (0.70)" = "#1F4E79",
  "Pilot estimate (0.89)" = "#E4572E"
)


power_target <- 0.80

power_iterations <- unique(
  power_results$SimulationsRequested
)

pilot_power_effect <- power_results %>%
  filter(
    Scenario == "Pilot estimate (0.89)"
  ) %>%
  slice(1) %>%
  pull(
    TargetEffect
  )


pilot_power_at_8 <- power_results %>%
  filter(
    Scenario == "Pilot estimate (0.89)",
    PlotsPerTreatment == 8
  ) %>%
  pull(
    Power
  )

moderate_power_at_12 <- power_results %>%
  filter(
    Scenario == "Moderate (0.70)",
    PlotsPerTreatment == 12
  ) %>%
  pull(
    Power
  )

moderate_power_at_16 <- power_results %>%
  filter(
    Scenario == "Moderate (0.70)",
    PlotsPerTreatment == 16
  ) %>%
  pull(
    Power
  )

conservative_power_at_20 <- power_results %>%
  filter(
    Scenario == "Conservative (0.50)",
    PlotsPerTreatment == 20
  ) %>%
  pull(
    Power
  )


# ============================================================
# 16. Reusable UI components
# ============================================================

question_box <- function(text) {
  
  div(
    class = "question-box",
    
    tags$strong(
      "Question this section answers: "
    ),
    
    text
  )
}


takeaway_box <- function(text) {
  
  div(
    class = "takeaway-box",
    
    tags$strong(
      "Takeaway: "
    ),
    
    text
  )
}


# ============================================================
# 17. User Interface
# ============================================================

ui <- page_navbar(
  
  title = "Chilli Thrips Treatment Analysis",
  
  theme = app_theme,
  
  id = "main_nav",
  
  header = tagList(
    
    # --------------------------------------------------------
    # Purpose
    # --------------------------------------------------------
    
    div(
      class = "purpose-box",
      
      tags$strong("Purpose: "),
      
      "This dashboard summarizes the analysis workflow from data review
       to final results and provides supporting details for the main conclusions
       and exploratory future-study planning."
    ),
    
    
    # --------------------------------------------------------
    # Workflow
    # --------------------------------------------------------
    
    uiOutput(
      "workflow_ui"
    ),
    
    
    # --------------------------------------------------------
    # CSS
    # --------------------------------------------------------
    
    tags$style(
      HTML("

      body {
        background-color: #F7F8FA;
      }

      .tab-container {
        padding: 18px 25px 30px 25px;
      }


      /* Purpose */

      .purpose-box {
        background-color: #E8F1F8;
        border-left: 5px solid #FA4616;
        padding: 12px 18px;
        margin: 16px 25px 8px 25px;
        border-radius: 8px;
        font-size: 15px;
      }


      /* Workflow */

      .workflow-box {
        display: flex;
        justify-content: center;
        align-items: center;
        flex-wrap: wrap;
        gap: 10px;
        background-color: #FFFFFF;
        margin: 8px 25px 18px 25px;
        padding: 14px;
        border-radius: 10px;
        color: #0B2E59;
        font-weight: 600;
      }

      .workflow-step {
        padding: 6px 11px;
        border-radius: 7px;
        color: #536273;
      }

      .workflow-step.active {
        background-color: #0B2E59;
        color: white;
      }

      .workflow-arrow {
        color: #FA4616;
        font-size: 18px;
        font-weight: bold;
      }


      /* Question */

      .question-box {
        background-color: #F0F5FA;
        border-left: 5px solid #1F4E79;
        padding: 13px 17px;
        margin-bottom: 18px;
        border-radius: 8px;
      }


      /* Takeaway */

      .takeaway-box {
        background-color: #E8F1F8;
        border-left: 5px solid #FA4616;
        padding: 13px 17px;
        margin-top: 18px;
        border-radius: 8px;
      }


      /* Cards */

      .card {
        border-radius: 12px;
        margin-bottom: 16px;
      }


      /* Data preparation flow */

      .data-flow {
        text-align: center;
      }

      .flow-step {
        border: 1px solid #D5DCE4;
        border-radius: 8px;
        padding: 11px;
      }

      .flow-step-source {
        background-color: #F4F5F7;
      }

      .flow-step-prep {
        background-color: #FFF0EA;
        border-color: #F8C5B3;
      }

      .flow-step-ready {
        background-color: #E3EEF7;
        border-color: #B9CDDF;
      }

      .flow-arrow {
        color: #FA4616;
        font-size: 22px;
        font-weight: bold;
        padding: 4px;
      }


      /* Notes below plots */

      .plot-note {
        margin-top: 10px;
        margin-bottom: 3px;
        padding: 9px 12px;
        background-color: #F7F9FB;
        border-left: 3px solid #8FA9BF;
        border-radius: 5px;
        color: #536273;
        font-size: 14px;
        line-height: 1.45;
      }

      .plot-note strong {
        color: #0B2E59;
      }


      /* Model page */

      .model-formula {
        background-color: #0B2E59;
        color: white;
        text-align: center;
        padding: 18px;
        margin-bottom: 14px;
        border-radius: 8px;
        font-size: 21px;
        font-weight: 600;
      }

      .model-reason {
        background-color: #F0F5FA;
        border-top: 4px solid #1F4E79;
        border-radius: 8px;
        padding: 14px 16px;
        min-height: 125px;
      }

      .model-reason strong {
        color: #0B2E59;
        font-size: 16px;
      }

      /* Plot ICC explanation */

      .icc-note {
        background-color: #F1F8F4;
        border-left: 4px solid #2E8B57;
        border-radius: 8px;
        padding: 12px 15px;
        margin-top: 12px;
        margin-bottom: 14px;
        color: #43564A;
        font-size: 14px;
        line-height: 1.45;
      }

      .icc-note strong {
        color: #215C3A;
      }

      .term-box {
        background-color: #F7F9FB;
        border: 1px solid #D8E0E8;
        border-radius: 8px;
        padding: 13px 15px;
        min-height: 105px;
      }

      .term-box strong {
        color: #0B2E59;
        font-size: 16px;
      }

      .temperature-box {
        background-color: #FFF8F3;
        border-left: 4px solid #FA4616;
        border-radius: 8px;
        padding: 14px 17px;
      }


      /* Results page */

      .results-interpretation {
        background-color: #F7F9FB;
        border-left: 4px solid #1F4E79;
        border-radius: 8px;
        padding: 13px 16px;
        margin-top: 12px;
      }

      .results-interpretation p {
        margin-bottom: 8px;
      }

      .recommendation-box {
        background-color: #FFF8F3;
        border-left: 5px solid #FA4616;
        border-radius: 8px;
        padding: 16px 18px;
        line-height: 1.55;
      }

      .recommendation-box strong {
        color: #0B2E59;
      }

      .next-step-box {
        background-color: #F0F5FA;
        border-top: 4px solid #1F4E79;
        border-radius: 8px;
        padding: 14px 16px;
        min-height: 120px;
      }

      .next-step-box strong {
        color: #0B2E59;
        font-size: 16px;
      }
      
      /* Future-study planning */

      .power-control-box {
        background-color: #F7F9FB;
        border: 1px solid #D8E0E8;
        border-radius: 10px;
        padding: 15px 17px;
      }

      .power-control-box .form-group {
        margin-bottom: 14px;
      }

      .power-selected-note {
        background-color: #F0F5FA;
        border-left: 4px solid #1F4E79;
        border-radius: 8px;
        padding: 13px 16px;
        margin-top: 12px;
        line-height: 1.5;
      }

      .power-selected-note p {
        margin-bottom: 7px;
      }

      .power-method-step {
        background-color: #F7F9FB;
        border: 1px solid #D8E0E8;
        border-radius: 8px;
        padding: 11px 13px;
        margin-bottom: 9px;
        line-height: 1.4;
      }

      .power-method-step strong {
        color: #0B2E59;
      }

      .power-disclaimer {
        background-color: #FFF8F3;
        border-left: 5px solid #FA4616;
        border-radius: 8px;
        padding: 14px 17px;
        margin-top: 16px;
        line-height: 1.5;
      }

      .power-disclaimer strong {
        color: #8A351A;
      }

      .power-finding-box {
        background-color: #F1F8F4;
        border-left: 4px solid #2E8B57;
        border-radius: 8px;
        padding: 13px 16px;
        line-height: 1.5;
      }

      .power-finding-box p {
        margin-bottom: 8px;
      }

      .power-finding-box strong {
        color: #215C3A;
      }

      .power-definition-box {
        background-color: #FFFFFF;
        border: 1px solid #D8E0E8;
        border-radius: 10px;
        padding: 15px 17px;
        line-height: 1.55;
      }

      .power-definition-box p {
        margin-bottom: 10px;
      }

      .power-definition-box strong {
        color: #0B2E59;
      }

      .power-formula-box {
        background-color: #F1F8F4;
        border-left: 4px solid #2E8B57;
        border-radius: 8px;
        padding: 12px 15px;
        margin: 10px 0 12px 0;
        color: #43564A;
        line-height: 1.5;
      }

      .power-formula-box strong {
        color: #215C3A;
      }

      .power-example-box {
        background-color: #F0F5FA;
        border-left: 4px solid #1F4E79;
        border-radius: 8px;
        padding: 12px 15px;
        margin-top: 10px;
        line-height: 1.5;
      }

      .power-example-box strong {
        color: #0B2E59;
      }

      .mc-interval-box {
        background-color: #FFF8F3;
        border-left: 4px solid #FA4616;
        border-radius: 8px;
        padding: 13px 16px;
        margin-top: 12px;
        line-height: 1.5;
      }

      .mc-interval-box p {
        margin-bottom: 9px;
      }

      .mc-interval-box strong {
        color: #8A351A;
      }

      /* Dashboard title */
      
      .navbar-brand {
      color: #0B2E59 !important;
      font-weight: 700;
      font-size: 28px;
      letter-spacing: -0.3px;
      }
      
      .navbar-brand:hover,
      .navbar-brand:focus {
      color: #0B2E59 !important;
      }
      
      /* Navigation tabs */
      
      .navbar-nav .nav-link {
      color: #536273 !important;
      font-size: 18px;
      font-weight: 500;
      }
      
      .navbar-nav .nav-link.active {
      color: #0B2E59 !important;
      font-weight: 700;
      }
      
      .navbar-nav .nav-link:hover {
      color: #FA4616 !important;
      }
      
      .navbar-nav .nav-link.active {
      border-bottom-color: #0B2E59 !important;
      }
      
      ")
    )
  ),
  
  
  # ==========================================================
  # TAB 1: Study & Data
  # ==========================================================
  
  nav_panel(
    "Study & Data",
    
    div(
      class = "tab-container",
      
      question_box(
        "What is the study design and data structure?"
      ),
      
      
      # ------------------------------------------------------
      # Summary value boxes
      # ------------------------------------------------------
      
      layout_columns(
        
        value_box(
          title = "Treatments",
          value = n_treatments,
          
          theme = value_box_theme(
            bg = "#DCE8F3",
            fg = "#0B2E59"
          )
        ),
        
        value_box(
          title = "Independent Plots",
          value = n_plots,
          
          theme = value_box_theme(
            bg = "#DCE8F3",
            fg = "#0B2E59"
          )
        ),
        
        value_box(
          title = "Time Points",
          value = n_timepoints,
          
          theme = value_box_theme(
            bg = "#DCE8F3",
            fg = "#0B2E59"
          )
        ),
        
        value_box(
          title = "Plot-level Observations",
          value = n_observations,
          
          theme = value_box_theme(
            bg = "#DCE8F3",
            fg = "#0B2E59"
          )
        ),
        
        col_widths = c(
          3,
          3,
          3,
          3
        )
      ),
      
      
      # ------------------------------------------------------
      # Study structure + data preparation
      # ------------------------------------------------------
      
      layout_columns(
        
        card(
          
          card_header(
            tags$strong(
              "Study Structure"
            )
          ),
          
          card_body(
            
            tags$p(
              tags$strong("Treatments: "),
              "Control, Pesticide, Vehicle, and Manual"
            ),
            
            tags$p(
              tags$strong("Experimental unit: "),
              "Plot"
            ),
            
            tags$p(
              tags$strong("Replication: "),
              "4 independent plots per treatment"
            ),
            
            tags$p(
              tags$strong("Repeated measurements: "),
              "Each plot was measured at five time points"
            ),
            
            tags$p(
              tags$strong("Field blocking: "),
              "Row and column positions"
            ),
            
            tags$p(
              tags$strong("Study times: "),
              "Pretreatment, Day 7, Day 14, Day 21, and Day 28"
            )
          )
        ),
        
        
        card(
          
          card_header(
            tags$strong(
              "Data Preparation"
            )
          ),
          
          card_body(
            
            div(
              class = "data-flow",
              
              div(
                class = "flow-step flow-step-source",
                
                tags$strong(
                  "Client-provided data"
                ),
                
                tags$br(),
                
                "Treatment_data_averaged.csv"
              ),
              
              div(
                class = "flow-arrow",
                "↓"
              ),
              
              div(
                class = "flow-step flow-step-prep",
                
                tags$strong(
                  "Preparation"
                ),
                
                tags$br(),
                
                "Recode time, define factors, reconstruct field column from the documented layout, and standardize analysis variables"
              ),
              
              div(
                class = "flow-arrow",
                "↓"
              ),
              
              div(
                class = "flow-step flow-step-ready",
                
                tags$strong(
                  "Analysis-ready data"
                ),
                
                tags$br(),
                
                "One record per Plot × Time"
              )
            )
          )
        ),
        
        col_widths = c(
          6,
          6
        )
      ),
      
      
      # ------------------------------------------------------
      # Checks + preview
      # ------------------------------------------------------
      
      layout_columns(
        
        card(
          
          card_header(
            tags$strong(
              "Initial Data Checks"
            )
          ),
          
          card_body(
            
            tags$p(
              paste0(
                "✓ ",
                n_observations,
                " plot-level observations"
              )
            ),
            
            tags$p(
              paste0(
                "✓ ",
                n_plots,
                " unique plots"
              )
            ),
            
            tags$p(
              paste0(
                "✓ ",
                n_timepoints,
                " repeated time points"
              )
            ),
            
            tags$p(
              if (missing_plot_time == 0) {
                
                "✓ No missing Plot × Time combinations"
                
              } else {
                
                paste(
                  "⚠",
                  missing_plot_time,
                  "missing Plot × Time combinations"
                )
              }
            ),
            
            tags$p(
              "✓ Balanced Treatment × Time structure: 4 plots per treatment at each time point"
            ),
            
            tags$p(
              "✓ Row and column positions follow the documented 4 × 4 field layout"
            ),
            
            tags$p(
              tags$strong("Outcome note: "),
              "The original severity response is an ordinal 0–4 scale; the primary analysis uses the client-provided plot-level averages."
            )
          )
        ),
        
        
        card(
          
          card_header(
            tags$strong(
              "Analysis-ready Data Preview"
            )
          ),
          
          card_body(
            DTOutput(
              "data_preview_table"
            )
          )
        ),
        
        col_widths = c(
          5,
          7
        )
      ),
      
      
      takeaway_box(
        "The plot is the experimental unit, and each plot is measured repeatedly over time. The analysis must therefore account for repeated observations from the same plot."
      )
    )
  ),
  
  
  # ==========================================================
  # TAB 2: EDA
  # ==========================================================
  
  nav_panel(
    "EDA",
    
    div(
      class = "tab-container",
      
      question_box(
        "What patterns do we see before fitting the model?"
      ),
      
      
      # ------------------------------------------------------
      # Pretreatment + mean trajectory
      # ------------------------------------------------------
      
      layout_columns(
        
        card(
          
          card_header(
            tags$strong(
              "Pretreatment Severity by Treatment"
            )
          ),
          
          card_body(
            
            plotOutput(
              "pretreatment_plot",
              height = "360px"
            ),
            
            tags$p(
              class = "plot-note",
              
              tags$strong(
                "What to notice: "
              ),
              
              "These are descriptive pretreatment summaries based on the four independent plots in each treatment group. Bars show the observed mean and error bars show ±1 SD. Pretreatment severity was not identical across treatment groups."
            )
          )
        ),
        
        
        card(
          
          card_header(
            tags$strong(
              "Mean Severity Over Time"
            )
          ),
          
          card_body(
            
            plotOutput(
              "mean_trajectory_plot",
              height = "360px"
            ),
            
            tags$p(
              class = "plot-note",
              
              tags$strong(
                "What to notice: "
              ),
              
              "Each point is the observed mean of the four plots within a treatment at that time point. These are descriptive summaries, not model-fitted means. Severity increased around Day 14 and then declined, with different patterns across treatments."
            )
          )
        ),
        
        col_widths = c(
          6,
          6
        )
      ),
      
      
      # ------------------------------------------------------
      # Individual plot trajectories
      # ------------------------------------------------------
      
      card(
        
        card_header(
          tags$strong(
            "Individual Plot Trajectories"
          )
        ),
        
        card_body(
          
          plotOutput(
            "individual_trajectory_plot",
            height = "520px"
          ),
          
          tags$p(
            class = "plot-note",
            
            tags$strong(
              "What to notice: "
            ),
            
            "Each line represents one independent plot measured repeatedly across the five time points, with four plots per treatment. There was noticeable plot-to-plot variation within treatments, as well as changes within the same plot over time. The repeated measurements within each plot are accounted for by the plot random effect in the mixed model."
          )
        )
      ),
      
      
      # ------------------------------------------------------
      # Plant counts + temperature
      # ------------------------------------------------------
      
      layout_columns(
        
        card(
          
          card_header(
            tags$strong(
              "Plants Sampled Over Time"
            )
          ),
          
          card_body(
            
            plotOutput(
              "plant_count_plot",
              height = "300px"
            ),
            
            tags$p(
              class = "plot-note",
              
              tags$strong(
                "What to notice: "
              ),
              
              "The number of plant ratings contributing to each plot-level summary varied most at Pretreatment, while counts were generally close to 20 at later time points. These counts describe the number of available plant ratings underlying each plot-level mean; they are not treated as independent treatment replicates. The plot remains the experimental unit in the primary analysis."
            )
          )
        ),
        
        
        card(
          
          card_header(
            tags$strong(
              "Temperature Pattern"
            )
          ),
          
          card_body(
            
            plotOutput(
              "temperature_plot",
              height = "300px"
            ),
            
            tags$p(
              class = "plot-note",
              
              tags$strong(
                "What to notice: "
              ),
              
              paste0(
                "Temperature varied mainly across study time, with limited variation among plots within the same time point. ",
                "In an exploratory model treating Time as a categorical predictor, Time accounted for approximately ",
                sprintf("%.1f", temperature_time_percent),
                "% of the observed temperature variation (R² = ",
                sprintf("%.2f", temperature_time_r2),
                "). Because Temperature and Time are difficult to separate in this design, Temperature was not included in the primary model. ",
                "This R² is descriptive and is not interpreted causally."
              )
            )
          )
        ),
        
        col_widths = c(
          6,
          6
        )
      ),
      
      
      takeaway_box(
        "Pretreatment severity differed descriptively across groups, and there was noticeable plot-to-plot variation. Treatment trajectories also changed over time, supporting a repeated-measures analysis."
      )
    )
  ),
  
  
  # ==========================================================
  # TAB 3: Model
  # ==========================================================
  
  nav_panel(
    "Model",
    
    div(
      class = "tab-container",
      
      question_box(
        "What model is appropriate for this study design?"
      ),
      
      
      # ------------------------------------------------------
      # Why this model?
      # ------------------------------------------------------
      
      card(
        
        card_header(
          tags$strong(
            "Why a Repeated-Measures Mixed Model?"
          )
        ),
        
        card_body(
          
          layout_columns(
            
            div(
              class = "model-reason",
              
              tags$strong(
                "Plot-level treatment"
              ),
              
              tags$p(
                "Treatment was assigned to plots, so the plot is the experimental unit."
              )
            ),
            
            div(
              class = "model-reason",
              
              tags$strong(
                "Repeated measurements"
              ),
              
              tags$p(
                "The same plots were measured five times, so measurements from the same plot are related."
              )
            ),
            
            div(
              class = "model-reason",
              
              tags$strong(
                "Field layout"
              ),
              
              tags$p(
                "Row and column are included to account for spatial blocking in the field."
              )
            ),
            
            col_widths = c(
              4,
              4,
              4
            )
          )
        )
      ),
      
      
      # ------------------------------------------------------
      # Formula
      # ------------------------------------------------------
      
      card(
        
        card_header(
          tags$strong(
            "Primary Model"
          )
        ),
        
        card_body(
          
          div(
            class = "model-formula",
            
            HTML(
              "Severity ~ Treatment × Time + Row + Column + (1 | Plot)"
            )
          ),
          
          tags$p(
            class = "plot-note",
            
            tags$strong(
              "Modeling approach: "
            ),
            
            "The client-provided plot-level averaged severity is treated as approximately continuous for the primary analysis."
          )
        )
      ),
      
      # ------------------------------------------------------
      # Model fit at a glance
      # ------------------------------------------------------
      
      card(
        
        card_header(
          tags$strong(
            "Model Summary at a Glance"
          )
        ),
        
        card_body(
          
          # --------------------------------------------------
          # Basic model information
          # --------------------------------------------------
          
          layout_columns(
            
            value_box(
              title = "Observations",
              value = nrow(model_data),
              
              theme = value_box_theme(
                bg = "#DCE8F3",
                fg = "#0B2E59"
              )
            ),
            
            value_box(
              title = "Independent Plots",
              value = n_distinct(model_data$Plot),
              
              theme = value_box_theme(
                bg = "#DCE8F3",
                fg = "#0B2E59"
              )
            ),
            
            value_box(
              title = "Time Points",
              value = n_distinct(model_data$Time),
              
              theme = value_box_theme(
                bg = "#DCE8F3",
                fg = "#0B2E59"
              )
            ),
            
            value_box(
              title = "Plot ICC",
              value = sprintf(
                "%.2f",
                plot_icc
              ),
              
              theme = value_box_theme(
                bg = "#E7F3EC",
                fg = "#215C3A"
              )
            ),
            
            col_widths = c(
              3,
              3,
              3,
              3
            )
          ),
          
          # --------------------------------------------------
          # Plot ICC explanation
          # --------------------------------------------------
          
          div(
            class = "icc-note",
            
            tags$strong(
              "Plot ICC (Intraclass Correlation Coefficient): "
            ),
            
            paste0(
              "The estimated ICC is ",
              sprintf(
                "%.2f",
                plot_icc
              ),
              ". "
            ),
            
            "The ICC summarizes how strongly repeated measurements from the same plot are related after accounting for the fixed effects in the model. ",
            
            paste0(
              "An ICC of ",
              sprintf("%.2f", plot_icc),
              " indicates strong within-plot similarity: measurements from the same plot tend to be more alike than measurements from different plots. "
            ),
            
            "This is why the repeated observations from a plot should not be treated as independent observations. ",
            
            "The repeated-measures study design is the main reason for including a plot random intercept; the ICC provides numerical support that the within-plot dependence is substantial."
          ),
          
          
          # --------------------------------------------------
          # Variance components
          # --------------------------------------------------
          
          tags$hr(),
          
          tags$strong(
            "Variance Components"
          ),
          
          tags$p(
            style = "margin-top: 6px; margin-bottom: 10px; color: #536273;",
            
            "The mixed model separates the remaining variability into two parts: differences between plots and residual variation within the observed plot-by-time measurements."
          ),
          
          tableOutput(
            "model_variance_table"
          ),
          
          
          # --------------------------------------------------
          # Interpretation note
          # --------------------------------------------------
          
          tags$div(
            class = "plot-note",
            
            tags$p(
              tags$strong(
                "What to notice: "
              ),
              
              paste0(
                "The model uses all ",
                nrow(model_data),
                " plot-level observations, but they come from only ",
                n_distinct(model_data$Plot),
                " independent plots measured repeatedly over time. Therefore, the 80 observations should not be treated as 80 independent experimental units."
              )
            ),
            
            tags$p(
              paste0(
                "The plot random-intercept SD is ",
                sprintf("%.2f", plot_random_sd),
                ". This describes the remaining differences in overall severity levels between plots after Treatment, Time, Treatment × Time, Row, and Column are accounted for. "
              ),
              
              paste0(
                "The residual SD is ",
                sprintf("%.2f", residual_sd),
                ". This describes the remaining observation-level variation after the fixed effects and plot-level differences are accounted for."
              )
            ),
            
            tags$p(
              paste0(
                "The plot ICC is ",
                sprintf("%.2f", plot_icc),
                ". This indicates strong similarity among repeated measurements from the same plot. In practical terms, the plot itself is an important source of variation, so the mixed model treats each plot as a repeated-measures cluster through the random intercept."
              )
            )
          )
        )
      ),
      
      # ------------------------------------------------------
      # Model terms
      # ------------------------------------------------------
      
      card(
        
        card_header(
          tags$strong(
            "What Each Model Term Tells Us"
          )
        ),
        
        card_body(
          
          layout_columns(
            
            div(
              class = "term-box",
              
              tags$strong(
                "Treatment"
              ),
              
              tags$p(
                "Are treatments different overall?"
              )
            ),
            
            div(
              class = "term-box",
              
              tags$strong(
                "Time"
              ),
              
              tags$p(
                "Does severity change during the study?"
              )
            ),
            
            div(
              class = "term-box",
              
              tags$strong(
                "Treatment × Time"
              ),
              
              tags$p(
                "Do treatment differences change over time?"
              )
            ),
            
            div(
              class = "term-box",
              
              tags$strong(
                "Row + Column"
              ),
              
              tags$p(
                "Blocking factors that account for the documented field layout and possible spatial variation."
              )
            ),
            
            div(
              class = "term-box",
              
              tags$strong(
                "(1 | Plot)"
              ),
              
              tags$p(
                "Accounts for repeated measurements from the same plot."
              )
            ),
            
            col_widths = c(
              4,
              4,
              4,
              6,
              6
            )
          )
        )
      ),
      
      
      # ------------------------------------------------------
      # Temperature
      # ------------------------------------------------------
      
      card(
        
        card_header(
          tags$strong(
            "Why Was Temperature Not Included?"
          )
        ),
        
        card_body(
          
          div(
            class = "temperature-box",
            
            tags$p(
              "Temperature was considered as a possible covariate."
            ),
            
            tags$p(
              "Exploratory review showed that temperature varied mainly across study time, with limited variation among plots within the same time point."
            ),
            
            tags$p(
              tags$strong(
                "Data evidence: "
              ),
              temperature_evidence_text
            ),
            
            tags$p(
              "Because Temperature and Time are closely tied in this design, their separate effects would be difficult to estimate reliably in this small study. Temperature was therefore not included in the primary model."
            )
          ),
          
          tags$p(
            class = "plot-note",
            
            tags$strong(
              "Important: "
            ),
            
            "This does not mean temperature is biologically unimportant. The exploratory R² is descriptive, not causal. It indicates that most observed temperature variation occurred across study time rather than independently within the same time point."
          )
        )
      ),
      
      
      takeaway_box(
        "The mixed model reflects the study design by accounting for treatment, time, their interaction, field blocking, and repeated measurements from the same plot."
      )
    )
  ),
  
  
  # ==========================================================
  # TAB 4: Diagnostics
  # ==========================================================
  
  nav_panel(
    "Diagnostics",
    
    div(
      class = "tab-container",
      
      question_box(
        "Does the model fit the data reasonably well?"
      ),
      
      
      # ------------------------------------------------------
      # Row 1: Residual pattern + normality
      # ------------------------------------------------------
      
      layout_columns(
        
        card(
          
          card_header(
            tags$strong(
              "Residuals vs Fitted"
            )
          ),
          
          card_body(
            
            plotOutput(
              "residual_fitted_plot",
              height = "360px"
            ),
            
            tags$p(
              class = "plot-note",
              
              tags$strong(
                "What we observed: "
              ),
              
              "Residuals were generally centered around zero across the fitted-value range. The smooth trend showed some mild curvature, but there was no strong funnel-shaped pattern suggesting severe heteroscedasticity. Overall, the residual pattern did not indicate a major lack of fit."
            )
          )
        ),
        
        
        card(
          
          card_header(
            tags$strong(
              "Normal Q-Q Plot"
            )
          ),
          
          card_body(
            
            plotOutput(
              "qq_plot",
              height = "360px"
            ),
            
            tags$p(
              class = "plot-note",
              
              tags$strong(
                "What we observed: "
              ),
              
              "Most standardized residuals followed the reference line reasonably closely, with some deviation in the tails. This supports approximate residual normality for the primary linear mixed-model analysis, although normality is not exact."
            )
          )
        ),
        
        col_widths = c(
          6,
          6
        )
      ),
      
      
      # ------------------------------------------------------
      # Row 2: Residual spread + observed/fitted agreement
      # ------------------------------------------------------
      
      layout_columns(
        
        card(
          
          card_header(
            tags$strong(
              "Residuals by Time"
            )
          ),
          
          card_body(
            
            plotOutput(
              "residual_time_plot",
              height = "340px"
            ),
            
            tags$p(
              class = "plot-note",
              
              tags$strong(
                "What we observed: "
              ),
              
              "Residuals were generally centered near zero at each time point. The spread was reasonably comparable across time, although Pretreatment and Day 28 showed somewhat greater variability. There was no clear evidence of a severe time-specific variance problem."
            )
          )
        ),
        
        
        card(
          
          card_header(
            tags$strong(
              "Observed vs Fitted"
            )
          ),
          
          card_body(
            
            plotOutput(
              "observed_fitted_plot",
              height = "340px"
            ),
            
            tags$p(
              class = "plot-note",
              
              tags$strong(
                "What we observed: "
              ),
              
              "Observed and fitted severity values generally followed the 45-degree reference line, indicating reasonable overall agreement between the model and the observed data. Some scatter remains, as expected, but there was no obvious systematic over- or under-prediction across the fitted-value range."
            )
          )
        ),
        
        col_widths = c(
          6,
          6
        )
      ),
      
      
      takeaway_box(
        "Overall, the diagnostics were reasonably supportive of the linear mixed-model approximation. Residuals were approximately normal, no severe heteroscedasticity or systematic lack-of-fit pattern was evident, and observed and fitted values showed reasonable agreement. Some mild curvature and time-specific variation remain, so results should still be interpreted with appropriate caution."
      ),
      
      tags$div(
        class = "plot-note",
        
        tags$strong("Modeling note: "),
        
        "These diagnostics assess the linear mixed model applied to the plot-level average severity, which is treated as approximately continuous. Because the original plant-level ratings are ordinal (0–4), an ordinal mixed model would require its own model assessment as part of a sensitivity analysis."
      )
    )
  ),
  
  
  # ==========================================================
  # TAB 5: Results & Recommendation
  # ==========================================================
  
  nav_panel(
    "Results & Recommendation",
    
    div(
      class = "tab-container",
      
      question_box(
        "What do the results mean for the client?"
      ),
      
      
      # ------------------------------------------------------
      # Main mixed-model results
      # ------------------------------------------------------
      
      card(
        
        card_header(
          tags$strong(
            "Primary Mixed-Model Results"
          )
        ),
        
        card_body(
          
          layout_columns(
            
            value_box(
              title = "Treatment",
              value = paste0(
                "p = ",
                format_p_value(
                  treatment_p
                )
              ),
              
              theme = value_box_theme(
                bg = "#DCE8F3",
                fg = "#0B2E59"
              )
            ),
            
            value_box(
              title = "Time",
              value = paste0(
                "p ",
                format_p_value(
                  time_p
                )
              ),
              
              theme = value_box_theme(
                bg = "#E7F3EC",
                fg = "#215C3A"
              )
            ),
            
            value_box(
              title = "Treatment × Time",
              value = paste0(
                "p = ",
                format_p_value(
                  interaction_p
                )
              ),
              
              theme = value_box_theme(
                bg = "#FFF0EA",
                fg = "#8A351A"
              )
            ),
            
            col_widths = c(
              4,
              4,
              4
            )
          ),
          
          div(
            class = "results-interpretation",
            
            tags$p(
              tags$strong(
                "Statistical inference: "
              ),
              "Type III F-tests from the repeated-measures linear mixed model Severity ~ Treatment × Time + Row + Column + (1 | Plot). Denominator degrees of freedom were obtained using the Satterthwaite method, with sum-to-zero contrasts for categorical fixed effects."
            ),
            
            tags$p(
              tags$strong(
                "Treatment: "
              ),
              "There was no consistent overall difference among treatments across the full study period."
            ),
            
            tags$p(
              tags$strong(
                "Time: "
              ),
              "Thrips severity changed clearly over the course of the study."
            ),
            
            tags$p(
              tags$strong(
                "Treatment × Time: "
              ),
              "Treatment patterns changed differently over time, so the treatment comparison depends on when it is evaluated."
            ),
            
            tags$p(
              tags$strong(
                "Blocking factors: "
              ),
              "Row and Column were included as blocking factors to account for field-position variation."
            )
          )
        )
      ),
      
      
      # ------------------------------------------------------
      # Observed change + model-based contrasts
      # ------------------------------------------------------
      
      layout_columns(
        
        card(
          
          card_header(
            tags$strong(
              "Observed Change: Pretreatment to Day 28"
            )
          ),
          
          card_body(
            
            plotOutput(
              "observed_change_plot",
              height = "360px"
            ),
            
            tags$p(
              class = "plot-note",
              
              tags$strong(
                "What to notice: "
              ),
              
              "This is a descriptive comparison based on observed plot-level means. Negative values indicate improvement because lower severity is better. Vehicle showed the largest observed reduction from Pretreatment to Day 28."
            )
          )
        ),
        
        
        card(
          
          card_header(
            tags$strong(
              "Improvement Relative to Control"
            )
          ),
          
          card_body(
            
            plotOutput(
              "contrast_forest_plot",
              height = "360px"
            ),
            
            tags$p(
              class = "plot-note",
              
              tags$strong(
                "What to notice: "
              ),
              
              "Model-based treatment-vs-Control contrasts were obtained from the repeated-measures mixed model, comparing improvement from Pretreatment to Day 28. P-values and simultaneous 95% confidence intervals were adjusted for the three treatment-vs-Control comparisons using the multivariate t method. Vehicle had the largest estimated improvement relative to Control, but its adjusted confidence interval included zero."
            )
          )
        ),
        
        col_widths = c(
          6,
          6
        )
      ),
      
      
      # ------------------------------------------------------
      # Full model tests + client recommendation
      # ------------------------------------------------------
      
      layout_columns(
        
        card(
          
          card_header(
            tags$strong(
              "Complete Type III Tests"
            )
          ),
          
          card_body(
            
            tableOutput(
              "model_results_table"
            ),
            
            tags$div(
              class = "plot-note",
              
              tags$p(
                tags$strong("Test details: "),
                "The table reports Type III F-tests for the fixed effects in the repeated-measures linear mixed model. ",
                "NumDF is the numerator degrees of freedom, and DenDF is the denominator degrees of freedom estimated using the Satterthwaite method."
              ),
              
              tags$p(
                tags$strong("Interpretation: "),
                "The primary interpretation focuses on Treatment, Time, and Treatment × Time. ",
                "Row and Column were included to account for variation associated with the documented field layout."
              )
            )
          )
        ),
        
        
        card(
          
          card_header(
            tags$strong(
              "Client Recommendation"
            )
          ),
          
          card_body(
            
            div(
              class = "recommendation-box",
              
              tags$p(
                tags$strong(
                  "Most promising treatment for further evaluation: Vehicle"
                )
              ),
              
              tags$p(
                paste0(
                  "Focused comparison: A model-based contrast from the repeated-measures mixed model estimated that Vehicle improved by about ",
                  sprintf(
                    "%.2f",
                    vehicle_estimate
                  ),
                  " severity units more than Control from Pretreatment to Day 28."
                )
              ),
              
              tags$p(
                paste0(
                  "The simultaneous 95% confidence interval was ",
                  sprintf(
                    "%.2f",
                    vehicle_lower_ci
                  ),
                  " to ",
                  sprintf(
                    "%.2f",
                    vehicle_upper_ci
                  ),
                  ", with an adjusted p-value of ",
                  sprintf(
                    "%.3f",
                    vehicle_adjusted_p
                  ),
                  ". Inference was adjusted across the three treatment-vs-Control comparisons using the multivariate t method."
                )
              ),
              
              tags$p(
                "Because the confidence interval includes zero and the adjusted p-value is above 0.05, the evidence is suggestive rather than definitive."
              ),
              
              tags$p(
                tags$strong(
                  "Recommendation: "
                ),
                "Prioritize Vehicle for further evaluation, but do not describe it as a confirmed winner based on this study alone."
              )
            )
          )
        ),
        
        col_widths = c(
          5,
          7
        )
      ),
      
      
      # ------------------------------------------------------
      # Recommended next steps
      # ------------------------------------------------------
      
      card(
        
        card_header(
          tags$strong(
            "Recommended Next Steps"
          )
        ),
        
        card_body(
          
          layout_columns(
            
            div(
              class = "next-step-box",
              
              tags$strong(
                "Increase replication"
              ),
              
              tags$p(
                "Use more independent plots per treatment in a future study to improve precision and power. See the Future Study Planning tab for exploratory pilot-based power scenarios."
              )
            ),
            
            div(
              class = "next-step-box",
              
              tags$strong(
                "Validate with plant-level data"
              ),
              
              tags$p(
                "Use the original plant-level ordinal ratings in an ordinal mixed model to check whether the main conclusions are consistent."
              )
            ),
            
            div(
              class = "next-step-box",
              
              tags$strong(
                "Clarify sampling and prespecify comparisons"
              ),
              
              tags$p(
                "Clarify how plants were sampled within plots and prespecify the key treatment-vs-Control comparisons in a future study."
              )
            ),
            
            col_widths = c(
              4,
              4,
              4
            )
          )
        )
      ),
      
      
      takeaway_box(
        "Vehicle showed the most promising improvement by Day 28, but the adjusted evidence was not definitive. I would prioritize Vehicle for further evaluation, but I would not describe it as a confirmed winner based on this study alone."
      )
    )
  ),
  
  
  # ==========================================================
  # TAB 6: Future Study Planning
  # ==========================================================
  
  nav_panel(
    "Future Study Planning",
    
    div(
      class = "tab-container",
      
      question_box(
        "How many independent plots per treatment might be needed in a future study to achieve adequate power?"
      ),
      
      
      # ------------------------------------------------------
      # Planning context
      # ------------------------------------------------------
      
      layout_columns(
        
        value_box(
          title = "Current Design",
          value = paste0(
            n_plots / n_treatments,
            " plots / treatment"
          ),
          
          theme = value_box_theme(
            bg = "#DCE8F3",
            fg = "#0B2E59"
          )
        ),
        
        value_box(
          title = "Pilot Vehicle Effect",
          value = paste0(
            "~",
            sprintf(
              "%.2f",
              pilot_power_effect
            ),
            " units"
          ),
          
          theme = value_box_theme(
            bg = "#FFF0EA",
            fg = "#8A351A"
          )
        ),
        
        value_box(
          title = "Planning Target",
          value = paste0(
            round(
              100 * power_target
            ),
            "% power"
          ),
          
          theme = value_box_theme(
            bg = "#E7F3EC",
            fg = "#215C3A"
          )
        ),
        
        value_box(
          title = "Exploratory Simulations",
          value = paste0(
            power_iterations,
            " / scenario"
          ),
          
          theme = value_box_theme(
            bg = "#F0F2F5",
            fg = "#536273"
          )
        ),
        
        col_widths = c(
          3,
          3,
          3,
          3
        )
      ),
      
      
      # ------------------------------------------------------
      # How power and sample size are determined
      # ------------------------------------------------------
      
      card(
        
        card_header(
          tags$strong(
            "How Are Power and Sample Size Determined?"
          )
        ),
        
        card_body(
          
          div(
            class = "power-definition-box",
            
            tags$p(
              tags$strong(
                "What does sample size mean here? "
              ),
              
              "Sample size refers to the number of independent plots per treatment because treatment was assigned at the plot level. ",
              "Repeated measurements from the same plot do not count as additional independent experimental units. ",
              "For example, 12 plots per treatment means 48 independent plots in total across the four treatment groups."
            ),
            
            tags$p(
              tags$strong(
                "What is power? "
              ),
              
              "Power is the probability that the planned analysis detects the Vehicle-vs-Control improvement when the assumed true effect is present. ",
              "Because this study uses repeated measurements and a mixed model, power was estimated by simulation rather than by a simple closed-form sample-size formula."
            ),
            
            div(
              class = "power-formula-box",
              
              tags$strong(
                "Estimated power = "
              ),
              
              "number of simulated studies in which the multiplicity-adjusted Vehicle-vs-Control p-value is below 0.05 ",
              "÷ total number of simulated studies."
            ),
            
            tags$p(
              tags$strong(
                "How is the planning sample size chosen? "
              ),
              
              "For each assumed effect size, candidate designs of 4, 8, 12, 16, and 20 independent plots per treatment were evaluated. ",
              "The exploratory planning benchmark is the smallest evaluated design with estimated power of at least 80%."
            ),
            
            div(
              class = "power-example-box",
              
              tags$strong(
                "Example: "
              ),
              
              "Under the moderate assumed effect of 0.70 severity units, 12 plots per treatment produced 77.5% estimated power, while 16 plots per treatment produced 90.5%. ",
              "Therefore, among the evaluated designs, 16 plots per treatment was the first design to exceed the 80% planning target."
            ),
            
            div(
              class = "mc-interval-box",
              
              tags$p(
                tags$strong(
                  "What is the Approx. 95% Monte Carlo Interval? "
                ),
                
                "Because each planning scenario uses a finite number of simulation runs, the estimated power itself has Monte Carlo, or simulation, uncertainty. ",
                "The interval summarizes uncertainty in the estimated power caused by using only 200 simulated studies."
              ),
              
              tags$p(
                tags$strong(
                  "How is it calculated? "
                ),
                
                "The Monte Carlo standard error is approximated as sqrt[p × (1 - p) / N], where p is the estimated power and N is the number of simulation runs. ",
                "The displayed interval is approximately p ± 1.96 × Monte Carlo standard error, limited to the 0% to 100% range."
              ),
              
              tags$p(
                tags$strong(
                  "Example: "
                ),
                
                "If estimated power is 77.5% from 200 simulations, the approximate Monte Carlo interval is 71.7% to 83.3%. ",
                "This means 77.5% is the simulation estimate, but there is noticeable numerical uncertainty from using only 200 runs."
              ),
              
              tags$p(
                tags$strong(
                  "Important distinction: "
                ),
                
                "This is not a confidence interval for the treatment effect and it is not the simultaneous confidence interval used for the Vehicle-vs-Control treatment comparison. ",
                "It reflects only Monte Carlo uncertainty in the estimated power."
              ),
              
              tags$p(
                tags$strong(
                  "Boundary note: "
                ),
                
                "If all 200 simulations are significant, this simple normal approximation can display 100% to 100%. ",
                "That should not be interpreted as proof that the true power is exactly 100%; it only means that all 200 simulated studies were successful in this exploratory run."
              )
            )
          )
        )
      ),
      
      
      # ------------------------------------------------------
      # Interactive explorer
      # ------------------------------------------------------
      
      layout_columns(
        
        card(
          
          card_header(
            tags$strong(
              "Interactive Planning Explorer"
            )
          ),
          
          card_body(
            
            div(
              class = "power-control-box",
              
              selectInput(
                inputId = "power_effect_scenario",
                label = "Assumed true Vehicle advantage",
                choices = c(
                  "Conservative — 0.50 severity units" =
                    "Conservative (0.50)",
                  "Moderate — 0.70 severity units" =
                    "Moderate (0.70)",
                  "Pilot estimate — 0.89 severity units" =
                    "Pilot estimate (0.89)"
                ),
                selected = "Moderate (0.70)"
              ),
              
              selectInput(
                inputId = "power_plots_per_treatment",
                label = "Sample size: independent plots per treatment",
                choices = c(
                  4,
                  8,
                  12,
                  16,
                  20
                ),
                selected = 12
              ),
              
              tags$p(
                class = "plot-note",
                
                tags$strong(
                  "What does the assumed effect mean? "
                ),
                
                "It is the assumed true additional improvement of Vehicle relative to Control from Pretreatment to Day 28, measured in severity units. A larger assumed effect generally requires fewer independent plots to achieve the same power."
              ),
              
              tags$p(
                class = "plot-note",
                
                tags$strong(
                  "Sample-size unit: "
                ),
                
                "The selected number is the number of independent plots in each treatment group, not the number of plants and not the number of repeated plot-time observations."
              ),
              
              tags$p(
                class = "plot-note",
                
                tags$strong(
                  "Interactive behavior: "
                ),
                
                "The selections display precomputed exploratory results. Changing the inputs does not rerun the simulation."
              )
            )
          )
        ),
        
        
        card(
          
          card_header(
            tags$strong(
              "Selected Scenario"
            )
          ),
          
          card_body(
            uiOutput(
              "selected_power_summary"
            ),
            
            uiOutput(
              "selected_power_interpretation"
            )
          )
        ),
        
        col_widths = c(
          4,
          8
        )
      ),
      
      
      # ------------------------------------------------------
      # Power curve + simulation method
      # ------------------------------------------------------
      
      layout_columns(
        
        card(
          
          card_header(
            tags$strong(
              "Power Across Planning Scenarios"
            )
          ),
          
          card_body(
            
            plotOutput(
              "power_curve_plot",
              height = "440px"
            ),
            
            tags$p(
              class = "plot-note",
              
              tags$strong(
                "How to read this plot: "
              ),
              
              "Each line represents an assumed true Vehicle-vs-Control improvement. The x-axis is sample size, defined here as independent plots per treatment. Larger effects generally require fewer independent plots to achieve the same power. The dashed line marks the 80% planning target, and the highlighted point corresponds to the selected dropdown scenario. Error bars show approximate 95% Monte Carlo intervals, which reflect simulation uncertainty in the estimated power caused by using 200 simulation runs. They are not confidence intervals for the treatment effect."
            )
          )
        ),
        
        
        card(
          
          card_header(
            tags$strong(
              "How the Simulation Works"
            )
          ),
          
          card_body(
            
            div(
              class = "power-method-step",
              tags$strong("1. Define future design"),
              tags$br(),
              "Evaluate 4, 8, 12, 16, and 20 independent plots per treatment by repeating the documented balanced 4 × 4 field layout."
            ),
            
            div(
              class = "power-method-step",
              tags$strong("2. Specify plausible effects"),
              tags$br(),
              "Evaluate Vehicle advantages of 0.50, 0.70, and the pilot estimate of approximately 0.89 severity units."
            ),
            
            div(
              class = "power-method-step",
              tags$strong("3. Simulate repeated measurements"),
              tags$br(),
              "Generate plot-level repeated outcomes using the pilot-based treatment pattern and estimated plot and residual variability."
            ),
            
            div(
              class = "power-method-step",
              tags$strong("4. Refit the analysis"),
              tags$br(),
              "Fit the repeated-measures mixed model to each simulated study, accounting for the field layout and the plot random intercept. When multiple 4 × 4 layouts are used, the field replicate is also included as a blocking factor."
            ),
            
            div(
              class = "power-method-step",
              tags$strong("5. Repeat the focused comparison"),
              tags$br(),
              "Compare Pretreatment-to-Day-28 improvement for each treatment with Control and apply the same multivariate-t multiplicity adjustment."
            ),
            
            div(
              class = "power-method-step",
              tags$strong("6. Estimate power and Monte Carlo uncertainty"),
              tags$br(),
              paste0(
                "For each design and effect-size scenario, ",
                power_iterations,
                " future studies were simulated. Estimated power is the number of simulated studies with a multiplicity-adjusted Vehicle-vs-Control p-value below 0.05 divided by ",
                power_iterations,
                ". The Monte Carlo standard error is approximated by sqrt[p × (1 - p) / N]. The displayed approximate 95% Monte Carlo interval is p ± 1.96 × MCSE, limited to the 0% to 100% range."
              )
            ),
            
            div(
              class = "power-method-step",
              tags$strong("7. Identify the planning benchmark"),
              tags$br(),
              "For each assumed effect size, the smallest evaluated number of independent plots per treatment with estimated power of at least 80% is used as the exploratory sample-size benchmark."
            )
          )
        ),
        
        col_widths = c(
          8,
          4
        )
      ),
      
      
      # ------------------------------------------------------
      # Preliminary findings
      # ------------------------------------------------------
      
      card(
        
        card_header(
          tags$strong(
            "Preliminary Planning Findings"
          )
        ),
        
        card_body(
          
          div(
            class = "power-finding-box",
            
            tags$p(
              tags$strong(
                "If the true effect is close to the pilot estimate (~0.89): "
              ),
              
              paste0(
                "8 plots per treatment produced approximately ",
                sprintf(
                  "%.1f",
                  100 * pilot_power_at_8
                ),
                "% simulated power."
              )
            ),
            
            tags$p(
              tags$strong(
                "For a moderate effect of 0.70: "
              ),
              
              paste0(
                "12 plots per treatment produced approximately ",
                sprintf(
                  "%.1f",
                  100 * moderate_power_at_12
                ),
                "% power, while 16 plots per treatment produced approximately ",
                sprintf(
                  "%.1f",
                  100 * moderate_power_at_16
                ),
                "% power."
              )
            ),
            
            tags$p(
              tags$strong(
                "For a conservative effect of 0.50: "
              ),
              
              paste0(
                "even 20 plots per treatment produced approximately ",
                sprintf(
                  "%.1f",
                  100 * conservative_power_at_20
                ),
                "% power, so the 80% target was not reached within the evaluated range."
              )
            )
          ),
          
          tags$div(
            class = "power-disclaimer",
            
            tags$strong(
              "Important: "
            ),
            
            paste0(
              "These results are exploratory, pilot-based planning estimates using ",
              power_iterations,
              " simulations per scenario. The current effect and variance estimates come from only four independent plots per treatment. The reported sample-size benchmarks are therefore exploratory rather than exact requirements. The Monte Carlo intervals shown here quantify only simulation uncertainty in the estimated power, not uncertainty in the treatment effect. For a formal future-study design, I would increase the number of simulation iterations, consider additional plausible effect sizes and variance assumptions, and refine the future field design with the client."
            )
          )
        )
      ),
      
      
      # ------------------------------------------------------
      # Complete exploratory results
      # ------------------------------------------------------
      
      card(
        
        card_header(
          tags$strong(
            "All Evaluated Planning Scenarios"
          )
        ),
        
        card_body(
          
          tableOutput(
            "power_results_table"
          ),
          
          tags$p(
            class = "plot-note",
            
            tags$strong(
              "Interpretation note: "
            ),
            
            "The table summarizes the same precomputed 200-simulation results shown in the power curve. Plots / Treatment is the sample-size measure used here, and Total Plots equals plots per treatment multiplied by the four treatment groups. Approx. 95% MC Interval is the approximate Monte Carlo interval for the estimated power. It reflects numerical simulation uncertainty from using 200 runs and is calculated from the Monte Carlo standard error; it is not a confidence interval for the treatment effect. These values are useful for comparing planning scenarios, but they should not be interpreted as exact sample-size requirements."
          )
        )
      ),
      
      
      takeaway_box(
        "Here, sample size means independent plots per treatment. The number of plots needed depends strongly on the effect size the future study is designed to detect: smaller effects require more independent plots to achieve the same power. These pilot-based simulations provide an exploratory planning benchmark, not a definitive sample-size requirement."
      )
    )
  )
)

# ============================================================
# 18. Server
# ============================================================

server <- function(input, output, session) {
  
  
  # ==========================================================
  # Interactive data preview
  # ==========================================================
  
  output$data_preview_table <- renderDT({
    
    preview_df <- data_preview %>%
      
      mutate(
        Severity = round(
          Severity,
          2
        ),
        
        Temperature = round(
          Temperature,
          2
        )
      ) %>%
      
      rename(
        `Plot ID` = Plot,
        `Field Row` = Row,
        `Field Column` = Column,
        `Mean Severity` = Severity,
        `Plants Sampled` = N_plants,
        `Temperature (°C)` = Temperature
      )
    
    
    datatable(
      preview_df,
      
      rownames = FALSE,
      
      filter = "top",
      
      options = list(
        pageLength = 10,
        lengthMenu = c(
          10,
          20,
          40,
          80
        ),
        autoWidth = TRUE,
        scrollX = TRUE
      )
    )
  })
  
  
  # ==========================================================
  # Workflow highlight
  # ==========================================================
  
  output$workflow_ui <- renderUI({
    
    current_tab <- input$main_nav
    
    
    workflow_step <- function(label) {
      
      active_class <- if (
        identical(
          current_tab,
          label
        )
      ) {
        
        "workflow-step active"
        
      } else {
        
        "workflow-step"
      }
      
      
      span(
        class = active_class,
        label
      )
    }
    
    
    div(
      class = "workflow-box",
      
      workflow_step(
        "Study & Data"
      ),
      
      span(
        class = "workflow-arrow",
        "→"
      ),
      
      workflow_step(
        "EDA"
      ),
      
      span(
        class = "workflow-arrow",
        "→"
      ),
      
      workflow_step(
        "Model"
      ),
      
      span(
        class = "workflow-arrow",
        "→"
      ),
      
      workflow_step(
        "Diagnostics"
      ),
      
      span(
        class = "workflow-arrow",
        "→"
      ),
      
      workflow_step(
        "Results & Recommendation"
      ),
      
      span(
        class = "workflow-arrow",
        "→"
      ),
      
      workflow_step(
        "Future Study Planning"
      )
    )
  })
  
  
  # ==========================================================
  # Future Study Planning: selected precomputed scenario
  # ==========================================================
  
  selected_power_row <- reactive({
    
    req(
      input$power_effect_scenario,
      input$power_plots_per_treatment
    )
    
    selected_row <- power_results %>%
      filter(
        as.character(Scenario) == input$power_effect_scenario,
        PlotsPerTreatment == as.integer(
          input$power_plots_per_treatment
        )
      )
    
    validate(
      need(
        nrow(selected_row) == 1,
        "The selected planning scenario is not available."
      )
    )
    
    selected_row
  })
  
  
  output$selected_power_summary <- renderUI({
    
    selected_row <- selected_power_row()
    
    selected_power <- selected_row$Power
    
    status_text <- if (
      selected_power >= power_target
    ) {
      "Meets target"
    } else {
      "Below target"
    }
    
    status_bg <- if (
      selected_power >= power_target
    ) {
      "#E7F3EC"
    } else {
      "#FFF0EA"
    }
    
    status_fg <- if (
      selected_power >= power_target
    ) {
      "#215C3A"
    } else {
      "#8A351A"
    }
    
    
    layout_columns(
      
      value_box(
        title = "Estimated Power",
        value = paste0(
          sprintf(
            "%.1f",
            100 * selected_power
          ),
          "%"
        ),
        
        theme = value_box_theme(
          bg = "#DCE8F3",
          fg = "#0B2E59"
        )
      ),
      
      value_box(
        title = "Total Independent Plots",
        value = selected_row$TotalPlots,
        
        theme = value_box_theme(
          bg = "#F0F2F5",
          fg = "#536273"
        )
      ),
      
      value_box(
        title = "80% Target",
        value = status_text,
        
        theme = value_box_theme(
          bg = status_bg,
          fg = status_fg
        )
      ),
      
      col_widths = c(
        4,
        4,
        4
      )
    )
  })
  
  
  output$selected_power_interpretation <- renderUI({
    
    selected_row <- selected_power_row()
    
    scenario_rows <- power_results %>%
      filter(
        as.character(Scenario) ==
          input$power_effect_scenario
      )
    
    eligible_designs <- scenario_rows %>%
      filter(
        Power >= power_target
      ) %>%
      arrange(
        PlotsPerTreatment
      )
    
    
    if (
      nrow(
        eligible_designs
      ) > 0
    ) {
      
      smallest_target_design <-
        eligible_designs$PlotsPerTreatment[1]
      
      target_message <- paste0(
        "Within the designs evaluated here, the smallest design reaching at least 80% estimated power was ",
        smallest_target_design,
        " independent plots per treatment."
      )
      
    } else {
      
      target_message <- paste0(
        "The 80% target was not reached within the evaluated range of up to ",
        max(
          scenario_rows$PlotsPerTreatment
        ),
        " independent plots per treatment."
      )
    }
    
    
    selected_status <- if (
      selected_row$Power >= power_target
    ) {
      
      "This selected design exceeds the 80% planning target."
      
    } else {
      
      "This selected design is below the 80% planning target."
    }
    
    
    div(
      class = "power-selected-note",
      
      tags$p(
        tags$strong(
          "Selected assumption: "
        ),
        
        paste0(
          "Vehicle has a true additional improvement of ",
          sprintf(
            "%.2f",
            selected_row$TargetEffect
          ),
          " severity units relative to Control, with ",
          selected_row$PlotsPerTreatment,
          " independent plots per treatment."
        )
      ),
      
      tags$p(
        tags$strong(
          "Exploratory estimate: "
        ),
        
        paste0(
          "Estimated power = ",
          sprintf(
            "%.1f",
            100 * selected_row$Power
          ),
          "%, with an approximate 95% Monte Carlo interval of ",
          sprintf(
            "%.1f",
            100 * selected_row$ApproxPowerLower95
          ),
          "% to ",
          sprintf(
            "%.1f",
            100 * selected_row$ApproxPowerUpper95
          ),
          "%. This interval reflects simulation uncertainty in the estimated power from using a finite number of simulation runs; it is not a confidence interval for the treatment effect."
        )
      ),
      
      tags$p(
        tags$strong(
          "How that power was calculated: "
        ),
        
        paste0(
          "Approximately ",
          round(
            selected_row$Power *
              selected_row$SuccessfulAnalyses
          ),
          " of the ",
          selected_row$SuccessfulAnalyses,
          " successfully analyzed simulated studies had a multiplicity-adjusted Vehicle-vs-Control p-value below 0.05. ",
          "That proportion is the estimated power for this selected scenario."
        )
      ),
      
      tags$p(
        tags$strong(
          "Planning interpretation: "
        ),
        selected_status,
        " ",
        target_message
      )
    )
  })
  
  
  output$power_curve_plot <- renderPlot({
    
    selected_row <- selected_power_row()
    
    
    plot_data <- power_results %>%
      mutate(
        Scenario = factor(
          Scenario,
          levels = power_scenario_levels
        )
      )
    
    
    selected_plot_data <- selected_row %>%
      mutate(
        Scenario = factor(
          Scenario,
          levels = power_scenario_levels
        )
      )
    
    
    ggplot(
      plot_data,
      
      aes(
        x = PlotsPerTreatment,
        y = Power,
        color = Scenario,
        group = Scenario
      )
    ) +
      
      geom_hline(
        yintercept = power_target,
        linetype = "dashed",
        color = "#8C8C8C",
        linewidth = 0.9
      ) +
      
      geom_errorbar(
        aes(
          ymin = ApproxPowerLower95,
          ymax = ApproxPowerUpper95
        ),
        width = 0.35,
        linewidth = 0.65,
        alpha = 0.45
      ) +
      
      geom_line(
        linewidth = 1.15
      ) +
      
      geom_point(
        size = 3
      ) +
      
      geom_point(
        data = selected_plot_data,
        aes(
          x = PlotsPerTreatment,
          y = Power,
          color = Scenario
        ),
        inherit.aes = FALSE,
        shape = 21,
        fill = "white",
        size = 5.5,
        stroke = 1.4
      ) +
      
      scale_color_manual(
        values = power_scenario_colors,
        drop = FALSE
      ) +
      
      scale_x_continuous(
        breaks = sort(
          unique(
            power_results$PlotsPerTreatment
          )
        )
      ) +
      
      scale_y_continuous(
        limits = c(
          0,
          1
        ),
        breaks = seq(
          0,
          1,
          by = 0.1
        ),
        labels = scales::label_percent(
          accuracy = 1
        )
      ) +
      
      labs(
        x = "Independent plots per treatment",
        y = "Estimated power",
        color = "Assumed Vehicle advantage",
        subtitle = "Vehicle vs Control: difference in Pretreatment-to-Day-28 improvement"
      ) +
      
      theme_minimal(
        base_size = 13
      ) +
      
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        
        plot.subtitle = element_text(
          color = "#6C757D",
          size = 11
        )
      )
  })
  
  
  output$power_results_table <- renderTable({
    
    power_results %>%
      
      transmute(
        Scenario = as.character(
          Scenario
        ),
        
        `Assumed Effect` = sprintf(
          "%.2f",
          TargetEffect
        ),
        
        `Plots / Treatment` =
          PlotsPerTreatment,
        
        `Total Plots` =
          TotalPlots,
        
        `Estimated Power` = paste0(
          sprintf(
            "%.1f",
            100 * Power
          ),
          "%"
        ),
        
        `Approx. 95% MC Interval` = paste0(
          sprintf(
            "%.1f",
            100 * ApproxPowerLower95
          ),
          "% – ",
          sprintf(
            "%.1f",
            100 * ApproxPowerUpper95
          ),
          "%"
        )
      )
    
  },
  striped = TRUE,
  bordered = FALSE,
  spacing = "s"
  )
  
  
  # ==========================================================
  # EDA: Pretreatment severity
  # ==========================================================
  
  output$pretreatment_plot <- renderPlot({
    
    ggplot(
      pretreatment_summary,
      
      aes(
        x = Treatment,
        y = Mean,
        fill = Treatment
      )
    ) +
      
      geom_col(
        width = 0.65
      ) +
      
      geom_errorbar(
        aes(
          ymin = Mean - SD,
          ymax = Mean + SD
        ),
        width = 0.15,
        linewidth = 0.8
      ) +
      
      geom_text(
        aes(
          label = sprintf(
            "%.2f",
            Mean
          )
        ),
        vjust = -0.8,
        fontface = "bold",
        size = 4
      ) +
      
      scale_fill_manual(
        values = treatment_colors
      ) +
      
      scale_y_continuous(
        limits = c(
          0,
          5
        ),
        breaks = 0:5
      ) +
      
      labs(
        x = NULL,
        y = "Pretreatment mean severity",
        subtitle = "Mean ± SD"
      ) +
      
      theme_minimal(
        base_size = 13
      ) +
      
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        
        plot.subtitle = element_text(
          color = "#6C757D"
        )
      )
  })
  
  
  # ==========================================================
  # EDA: Mean severity over time
  # ==========================================================
  
  output$mean_trajectory_plot <- renderPlot({
    
    ggplot(
      severity_summary,
      
      aes(
        x = Time,
        y = Mean,
        group = Treatment,
        color = Treatment
      )
    ) +
      
      geom_line(
        linewidth = 1.2
      ) +
      
      geom_point(
        size = 3
      ) +
      
      scale_color_manual(
        values = treatment_colors
      ) +
      
      scale_y_continuous(
        limits = c(
          0,
          5
        ),
        breaks = 0:5
      ) +
      
      labs(
        x = NULL,
        y = "Mean thrips severity",
        color = "Treatment"
      ) +
      
      theme_minimal(
        base_size = 13
      ) +
      
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
      )
  })
  
  
  # ==========================================================
  # EDA: Individual plot trajectories
  # ==========================================================
  
  output$individual_trajectory_plot <- renderPlot({
    
    ggplot(
      model_data,
      
      aes(
        x = Time,
        y = Severity,
        group = Plot,
        color = Treatment
      )
    ) +
      
      geom_line(
        linewidth = 0.8,
        alpha = 0.8
      ) +
      
      geom_point(
        size = 2
      ) +
      
      facet_wrap(
        ~ Treatment,
        ncol = 2
      ) +
      
      scale_color_manual(
        values = treatment_colors
      ) +
      
      scale_y_continuous(
        limits = c(
          0,
          5
        ),
        breaks = 0:5
      ) +
      
      labs(
        x = NULL,
        y = "Plot mean severity"
      ) +
      
      theme_minimal(
        base_size = 13
      ) +
      
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        
        strip.text = element_text(
          face = "bold",
          size = 12
        )
      )
  })
  
  
  # ==========================================================
  # EDA: Plant counts
  # ==========================================================
  
  output$plant_count_plot <- renderPlot({
    
    ggplot(
      model_data,
      
      aes(
        x = Time,
        y = N_plants
      )
    ) +
      
      geom_boxplot(
        width = 0.55,
        fill = "#DCE8F3",
        color = "#1F4E79",
        outlier.shape = NA
      ) +
      
      geom_jitter(
        width = 0.12,
        alpha = 0.55,
        size = 1.8
      ) +
      
      labs(
        x = NULL,
        y = "Number of plants sampled"
      ) +
      
      theme_minimal(
        base_size = 13
      ) +
      
      theme(
        panel.grid.minor = element_blank()
      )
  })
  
  
  # ==========================================================
  # EDA: Temperature pattern
  # ==========================================================
  
  output$temperature_plot <- renderPlot({
    
    ggplot(
      model_data,
      
      aes(
        x = Time,
        y = Temperature
      )
    ) +
      
      geom_point(
        position = position_jitter(
          width = 0.08
        ),
        size = 2.5,
        alpha = 0.65,
        color = "#1F4E79"
      ) +
      
      stat_summary(
        fun = mean,
        geom = "line",
        aes(
          group = 1
        ),
        linewidth = 1.1,
        color = "#FA4616"
      ) +
      
      stat_summary(
        fun = mean,
        geom = "point",
        size = 3,
        color = "#FA4616"
      ) +
      
      labs(
        x = NULL,
        y = "Temperature (°C)",
        subtitle = paste0(
          "Exploratory categorical-Time model: R² = ",
          sprintf("%.2f", temperature_time_r2),
          " (",
          sprintf("%.1f", temperature_time_percent),
          "% of observed temperature variation)"
        )
      ) +
      
      theme_minimal(
        base_size = 13
      ) +
      
      theme(
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(
          color = "#6C757D",
          size = 11
        )
      )
  })
  
  output$model_variance_table <- renderTable({
    
    tibble(
      Component = c(
        "Plot random intercept",
        "Residual"
      ),
      
      `Standard Deviation` = c(
        round(
          plot_random_sd,
          3
        ),
        
        round(
          residual_sd,
          3
        )
      ),
      
      Meaning = c(
        "Remaining between-plot variability in overall severity level",
        "Remaining observation-level variability after fixed effects and plot differences are accounted for"
      )
    )
    
  },
  striped = TRUE,
  bordered = FALSE,
  spacing = "s"
  )
  
  
  # ==========================================================
  # Diagnostics: Residuals vs Fitted
  # ==========================================================
  
  output$residual_fitted_plot <- renderPlot({
    
    ggplot(
      diagnostic_data,
      aes(
        x = Fitted,
        y = StdResidual
      )
    ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "#8C8C8C"
      ) +
      
      geom_point(
        color = "#1F4E79",
        size = 2.3,
        alpha = 0.7
      ) +
      
      geom_smooth(
        method = "loess",
        formula = y ~ x,
        se = FALSE,
        color = "#E4572E",
        linewidth = 0.9
      ) +
      
      labs(
        x = "Fitted severity",
        y = "Standardized residual"
      ) +
      
      theme_minimal(
        base_size = 13
      ) +
      
      theme(
        panel.grid.minor = element_blank()
      )
  })
  
  
  # ==========================================================
  # Diagnostics: Normal Q-Q Plot
  # ==========================================================
  
  output$qq_plot <- renderPlot({
    
    ggplot(
      diagnostic_data,
      aes(
        sample = StdResidual
      )
    ) +
      
      stat_qq(
        color = "#1F4E79",
        size = 2.3,
        alpha = 0.75
      ) +
      
      stat_qq_line(
        color = "#E4572E",
        linewidth = 0.9
      ) +
      
      labs(
        x = "Theoretical quantiles",
        y = "Standardized residual quantiles"
      ) +
      
      theme_minimal(
        base_size = 13
      ) +
      
      theme(
        panel.grid.minor = element_blank()
      )
  })
  
  
  # ==========================================================
  # Diagnostics: Residuals by Time
  # ==========================================================
  
  output$residual_time_plot <- renderPlot({
    
    ggplot(
      diagnostic_data,
      aes(
        x = Time,
        y = StdResidual
      )
    ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "#8C8C8C"
      ) +
      
      geom_boxplot(
        width = 0.55,
        fill = "#DCE8F3",
        color = "#1F4E79",
        outlier.shape = NA
      ) +
      
      geom_jitter(
        width = 0.10,
        size = 1.8,
        alpha = 0.55
      ) +
      
      labs(
        x = NULL,
        y = "Standardized residual"
      ) +
      
      theme_minimal(
        base_size = 13
      ) +
      
      theme(
        panel.grid.minor = element_blank()
      )
  })
  
  
  # ==========================================================
  # Diagnostics: Observed vs Fitted
  # ==========================================================
  
  output$observed_fitted_plot <- renderPlot({
    
    ggplot(
      diagnostic_data,
      aes(
        x = Fitted,
        y = Severity,
        color = Treatment
      )
    ) +
      
      geom_abline(
        intercept = 0,
        slope = 1,
        linetype = "dashed",
        color = "#8C8C8C",
        linewidth = 0.8
      ) +
      
      geom_point(
        size = 2.5,
        alpha = 0.75
      ) +
      
      scale_color_manual(
        values = treatment_colors
      ) +
      
      scale_x_continuous(
        limits = c(0, 5),
        breaks = 0:5
      ) +
      
      scale_y_continuous(
        limits = c(0, 5),
        breaks = 0:5
      ) +
      
      coord_equal() +
      
      labs(
        x = "Fitted severity",
        y = "Observed severity",
        color = "Treatment"
      ) +
      
      theme_minimal(
        base_size = 13
      ) +
      
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
      )
  })
  
  
  # ==========================================================
  # Results: Complete Type III tests
  # ==========================================================
  
  output$model_results_table <- renderTable({
    
    model_test_results %>%
      
      mutate(
        Effect = recode(
          Effect,
          "Treatment:Time" = "Treatment × Time"
        ),
        
        NumDF = round(
          NumDF,
          1
        ),
        
        DenDF = round(
          DenDF,
          1
        ),
        
        `F value` = round(
          F_value,
          3
        ),
        
        `p-value` = map_chr(
          p_value,
          format_p_value
        )
      ) %>%
      
      select(
        Effect,
        NumDF,
        DenDF,
        `F value`,
        `p-value`
      )
    
  },
  striped = TRUE,
  bordered = FALSE,
  spacing = "s"
  )
  
  
  # ==========================================================
  # Results: Observed change Pretreatment to Day 28
  # Horizontal bar chart to match the presentation slide
  # ==========================================================
  
  output$observed_change_plot <- renderPlot({
    
    plot_change_data <- observed_change_summary %>%
      mutate(
        Treatment = factor(
          Treatment,
          levels = c(
            "Manual",
            "Vehicle",
            "Pesticide",
            "Control"
          )
        )
      )
    
    ggplot(
      plot_change_data,
      
      aes(
        x = Treatment,
        y = Change,
        fill = Treatment
      )
    ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "#8C8C8C"
      ) +
      
      geom_col(
        width = 0.65
      ) +
      
      geom_text(
        aes(
          label = sprintf(
            "%.2f",
            Change
          )
        ),
        hjust = -0.15,
        color = "#202124",
        fontface = "bold",
        size = 4
      ) +
      
      scale_fill_manual(
        values = treatment_colors
      ) +
      
      scale_y_continuous(
        limits = c(
          -1.75,
          0.15
        ),
        breaks = seq(
          -1.75,
          0,
          by = 0.25
        )
      ) +
      
      coord_flip() +
      
      labs(
        x = NULL,
        y = "Observed change in mean severity (Day 28 - Pretreatment)",
        subtitle = "Negative values indicate improvement"
      ) +
      
      theme_minimal(
        base_size = 13
      ) +
      
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        
        plot.subtitle = element_text(
          color = "#6C757D"
        )
      )
  })
  
  
  # ==========================================================
  # Results: Treatment improvement relative to Control
  # ==========================================================
  
  output$contrast_forest_plot <- renderPlot({
    
    ggplot(
      contrast_results,
      
      aes(
        x = estimate,
        y = Treatment,
        color = Treatment
      )
    ) +
      
      geom_vline(
        xintercept = 0,
        linetype = "dashed",
        color = "#8C8C8C"
      ) +
      
      geom_segment(
        aes(
          x = lower.CL,
          xend = upper.CL,
          y = Treatment,
          yend = Treatment
        ),
        linewidth = 1.2
      ) +
      
      geom_point(
        size = 4
      ) +
      
      scale_color_manual(
        values = treatment_colors
      ) +
      
      scale_y_discrete(
        limits = rev(
          c(
            "Pesticide",
            "Vehicle",
            "Manual"
          )
        )
      ) +
      
      labs(
        x = "Additional improvement vs Control",
        y = NULL,
        subtitle = "Estimate with simultaneous 95% confidence interval"
      ) +
      
      theme_minimal(
        base_size = 13
      ) +
      
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        
        plot.subtitle = element_text(
          color = "#6C757D"
        )
      )
  })
  
}


# ============================================================
# 19. Run App
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
