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
- Treatment-by-time interaction analysis
- Treatment-vs-Control comparisons
- Pretreatment-to-Day-28 improvement analysis
- Simulation-based power and sample-size planning
- Monte Carlo uncertainty for estimated power

## Statistical Approach

The primary analysis uses a linear mixed-effects model:

`Severity ~ Treatment * Time + Row + Column + (1 | Plot)`

The plot is treated as the experimental unit because treatments were assigned at the plot level, with repeated measurements collected over time.

The model accounts for:

- Treatment differences
- Changes over time
- Treatment-by-time interaction
- Row and column effects
- Correlation among repeated measurements from the same plot

## Interactive Dashboard

[View Interactive Dashboard](https://rentalsurvey.shinyapps.io/chilli-thrips-treatment-analysis/)

The interactive dashboard provides additional details on exploratory analysis, model results, diagnostics, treatment comparisons, and exploratory future-study planning.

## Future Study Planning

The dashboard includes an exploratory simulation-based power analysis to examine how the number of independent plots per treatment may affect statistical power in a future study.

Because treatment was assigned at the plot level, sample size refers to the number of independent plots per treatment rather than the number of individual plants or repeated measurements.

The power analysis is intended as an exploratory planning tool rather than a formal sample-size recommendation.

## Tools

- R
- Shiny
- lme4
- lmerTest
- emmeans
- ggplot2

## Project Purpose

This project demonstrates a statistical consulting workflow, including:

- Understanding the experimental design
- Identifying the appropriate experimental unit
- Exploring the data before modeling
- Selecting and fitting a repeated-measures model
- Evaluating model assumptions
- Interpreting treatment effects and interactions
- Communicating statistical uncertainty
- Planning future studies

## Author

**Yu-Chun Wang**  
Ph.D. in Statistical Science  
George Mason University
