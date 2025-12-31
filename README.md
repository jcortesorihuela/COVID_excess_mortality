# COVID-19 Excess Mortality Replication Files

This repository contains the replication files for the working paper titled *"The Unwarranted and Misleading Precision of Excess Mortality Estimates"* by Javier Cortes, Juan Diaz, Eduardo Engel, Ivan Gutierrez, and Alejandro Jofre.

## Repository Structure

The repository is organized into the following folders:

-   **Code**: Includes all scripts required to reproduce the paper's results, ranging from data wrangling to generating tables and figures.
-   **Figures**: Stores all the figures included in the paper.
-   **Input Data**: Contains pre-processed data on death counts and reported COVID-19 deaths.
-   **Output Data**: This folder contains:
    1.  Processed death count data and reported COVID-19 deaths.
    2.  Predictions, bootstrapped and resampled values from the cross-validation procedure, and excess death computations.
-   **Tables**: Stores all tables presented in the paper.

## Replication Instructions

To replicate the results of the paper, follow the steps below in the suggested order:

### 1. Data Wrangling

-   Run all scripts located in the `/Code/Data_wrangling/` folder.
-   These scripts transform the pre-processed death count and reported death data for further analysis.

### 2. Cross-Validation

-   Execute all scripts from the `/Code/Cross-validation/STAN` folder.
    -   These scripts runs both the cross-validation and the excess death calculations for the STAN models.
    -   Copy the relevant STAN outputs to `/Output data/Cross-validation data/` and `/Output data/Excess death data/` (The current repository already has the relevant files in place, look for the `ntbl_stan_sample_x.rds` files)
-   Execute all scripts from the `/Code/Cross-validation/` folder.
    -   These scripts run (or extract in the case of the STAN model) the cross-validation procedure of each model, saving results for each iteration in the `/Output_data/Cross-validation_data/` folder.

### 3. Excess Death Calculation

-   Run the scripts from the `/Code/Excess_death/` folder.
-   These scripts compute (or extract in the case of the STAN model) the excess death estimates, saving the results in the `/Output_data/Excess_death_data/` folder.

### 4. Tables

-   Execute the scripts from the `/Code/Tables_and_statistics/` folder to generate the tables used in the paper.

### 5. Figures

-   Run the scripts from the same `/Code/Figures/` folder to produce the figures presented in the paper.

### Extra: On the undercoverage of the WHO methodology

-   We investigate this issue in the folders `/Code/Cross-validation/Extra` which run the cross-validation procedure described in the final two paragraphs of section 5 from the paper.
-   To obtain summary results, run `Code/Tables and statistics/Extra`

------------------------------------------------------------------------

Following these steps will allow you to fully replicate the analysis and results presented in the paper.
