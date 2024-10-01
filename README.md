# COVID-19 Excess Mortality Replication Files

This repository contains the replication files for the working paper titled *"The Unwarranted and Misleading Precision of Excess Mortality Estimates"* by Javier Cortes, Juan Diaz, Eduardo Engel, and Alejandro Jofre.

## Repository Structure

The repository is organized into the following folders:

- **Code**: Includes all scripts required to reproduce the paper's results, ranging from data wrangling to generating tables and figures.
- **Figures**: Stores all the figures included in the paper.
- **Input Data**: Contains pre-processed data on death counts and reported COVID-19 deaths.
- **Output Data**: This folder contains:
  1. Processed death count data and reported COVID-19 deaths.
  2. Predictions, bootstrapped and resampled values from the cross-validation procedure, and excess death computations.
- **Tables**: Stores all tables presented in the paper.

## Replication Instructions

To replicate the results of the paper, follow the steps below in the suggested order:

### 1. Data Wrangling
- Run all scripts located in the `/Code/Data_wrangling/` folder.
- These scripts transform the pre-processed death count and reported death data for further analysis.

### 2. Cross-Validation
- Execute all scripts from the `/Code/Cross-validation/` folder.
- These scripts run the cross-validation procedure, saving results for each iteration in the `/Output_data/Cross-validation_data/` folder.

### 3. Excess Death Calculation
- Run the scripts from the `/Code/Excess_death/` folder.
- These scripts compute the excess death estimates, saving the results in the `/Output_data/Excess_death_data/` folder.

### 4. Tables
- Execute the scripts from the `/Code/Tables_and_statistics/` folder to generate the tables used in the paper.

### 5. Figures
- Run the scripts from the same `/Code/Tables_and_statistics/` folder to produce the figures presented in the paper.

---

Following these steps will allow you to fully replicate the analysis and results presented in the paper.
