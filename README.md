# Chilli Thrips Treatment Analysis

An interactive R Shiny dashboard for evaluating treatment effectiveness in a repeated-measures field experiment.

## Overview

This project analyzes four chilli thrips treatments over time using a repeated-measures mixed-effects modeling approach.

The dashboard provides an interactive way to explore the data, statistical results, model diagnostics, treatment comparisons, and future-study planning.

## Dashboard Features

- Exploratory data analysis
- Treatment severity trajectories over time
- Linear mixed-effects modeling
- Model diagnostics
- Treatment-by-time interaction
- Treatment-vs-Control comparisons
- Pretreatment-to-Day-28 improvement analysis
- Simulation-based power and sample-size planning
- Monte Carlo uncertainty for estimated power

## Statistical Approach

The primary model is a linear mixed-effects model:

`Severity ~ Treatment * Time + Row + Column + (1 | Plot)`

The plot is treated as the experimental unit because treatments were assigned at the plot level, with repeated measurements collected over time.

## Interactive Dashboard

You can explore the live dashboard here:

https://rentalsurvey.shinyapps.io/chilli-thrips-treatment-analysis/

## Tools

- R
- Shiny
- lme4
- lmerTest
- emmeans
- ggplot2

## Notes

The analysis is intended to demonstrate a statistical consulting workflow, including exploratory analysis, mixed-effects modeling, interpretation, diagnostics, and exploratory future-study planning.

## Author

Yu-Chun Wang  
Statistical Science | Data Analysis | R | Python
