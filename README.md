# Replication of Numbers Game
- In this R project, I replicate the analysis of Summary of Bird, Karolyi, and Ruchti (BKR; 2019, JAE). Using this code, anyone who has earnings surprise data can conduct BKR analysis.
- The code is roughly divided to two parts:
  1. Simulate the empirical earnings surprise distribution using the optimizing behavior and estimated parameters described in BKR.
  2. Estimate the parameter by BKR method, using the simulated data in Step 1 as "observed" data.
- I currently finished Step 1.

# Files
- `rmd/Replication.Rmd` is the main script. The script is exported as `html` file, which is named `rmd/Replication.html`.
    - All the original functions that appear in `Replication.Rmd` is stored in `R` directory.
- `SummaryBKR.pdf` is a summary of BKR's estimation method.
- If you have any comments or corrections, please post on Issues or email me (contact info. appears below).

# Contact Information
- Name: Shunsuke Matsuno
- Affiliation: The University of Tokyo, Graduate School of Economics, Second-year master's student
- Contact: [smatsuno@g.ecc.u-tokyo.ac.jp](mailto:smatsuno@g.ecc.u-tokyo.ac.jp)
