# XAI Partial Dependence Analysis

## Authors: Adrià Aguilar, Santiago Font, Pablo Gandía

This repository contains an ordered solution for the Partial Dependence Plot assignment. The main R script follows the exercise sequence from the PDF:

1. One-dimensional PDPs for daily bike rental demand.
2. Two-dimensional PDP for normalized temperature and humidity.
3. One-dimensional PDPs for King County house prices.

## Repository Structure

- `data/`: supplied assignment PDF and source datasets.
- `src/main.R`: ordered R script, written by exercise.
- `outputs/figures/`: generated PDP figures.
- `outputs/tables/`: generated model metrics, feature importance, and PDP tables.
- `report/main.tex`: LaTeX report source.
- `report/main.pdf`: compiled report.

## Run the Analysis

From the repository root:

```bash
Rscript src/main.R
```

The report can be obtained by uploading or copying the full repository into Overleaf and compiling `report/main.tex`.
