# Sampling-Techniques-
STA405-6

# Sampling Techniques – Unbiasedness of Sample Statistics

This repository contains R code for studying sampling techniques and validating the unbiasedness of common sample statistics (sample mean, sample variance, and sample proportion) under Simple Random Sampling Without Replacement (SRSWOR). The main script, `sampling_2.R`, builds a small finite population, draws repeated samples, and compares empirical results with theoretical properties from sampling theory.

## Files

- `sampling_2.R`: Main R script that
  - Creates a finite population with production values, certification status, and department labels.
  - Computes true population parameters (mean, variance, proportion).
  - Draws many SRSWOR samples to estimate the sampling distribution of sample mean, variance, and proportion.
  - Compares simulated expectations with the true population parameters to illustrate (approximate) unbiasedness.
  - Calculates and compares theoretical and empirical variances of the sample mean.
  - Produces basic visualizations (histograms and convergence plots) for the sample statistics.

## Requirements

- R (version X.X.X or later)
- Recommended R packages:
  - `combinat`
  - `dplyr`


