# Replication of Numbers Game
- In this R project, I replicate the analysis of [Bird, Karolyi, and Ruchti (BKR; 2019, JAE)](https://www.sciencedirect.com/science/article/pii/S0165410119300370). Using this code, anyone who has earnings surprise data and associated stock returns can conduct BKR analysis.
- The code is roughly divided to two parts:
  1. Simulate the empirical earnings surprise distribution using the optimizing behavior and estimated parameters described in BKR.
  2. Estimate the parameter by BKR method, using the simulated data in Step 1 as "observed" data.

# Files
- `rmd/Replication.Rmd` is the main script. The script is exported as `html` file, which is named `rmd/Replication.html`.
- **Download `rmd/Replication.html` and open the file with a broser (ex. Chrome)**.
    - All the original functions that appear in `Replication.Rmd` are stored in `R` directory.
- `NumbersGame_Summary.pdf` is a summary of BKR's estimation method.
- If you have any comments or corrections, please post on Issues or email me (contact info. appears below).

