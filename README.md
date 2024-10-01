# COVID_excess_mortality
Replication files for the working paper titled "The unwarranted and misleading precision of excess mortality estimates" by Javier Cortes, Juan Diaz, Eduardo Engel, and Alejandro Jofre.

There are four folders in the repository:

- Code: Contains all code to reproduce the results from the paper, from the data wrangling to the tables.
- Figures: Contains the figures in the paper.
- Input data: Contains all pre-processed death count and reported death data.
- Output data: Contains
  1. All the processed death counts and reported COVID deaths.
  2. All the predictions, bootstrapped and resampled values from the cross-validation procedure and from the excess death computations.
- Tables: Contains the tables presented in the paper.

The suggested order to replicate the findings of the paper is as follows:
1. Run every script from the /Code/Data wrangling/ folder.
   - These scripts transform the pre-processed death count and reported death data.
2. Run every script from the /Code/Cross-validation/ folder.
   - These scripts perform the cross-validation procedure, saving the results from each iteration into the /Output data/Cross-validation data/ folder.
3. Run every script from the /Code/Excess death/ folder.
   - These scripts perform the excess death computation, saving the results into the /Output data/Excess death data/ folder.
4. Run every script from the /Code/Tables and statistics/ folder.
   - These scripts produce the tables from the paper.
5. Run every script from the /Code/Tables and statistics/ folder.
   - These scripts produce the figures from the paper.
