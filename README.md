# Researcher_Bias_Methods

This repository contains scripts and tutorials for implementing the approaches described in the article ["Protecting against researcher bias in secondary data analysis: Challenges and potential solutions"](https://psyarxiv.com/md5pe/). Scripts are provided in .Rmd and .md formats (.md format is recommended for viewing).

Scripts are provided as follows:

1. *Data_Scrambling_Tutorial* 

This is a tutorial for data scrambling in R. Data scrambling is a way to blind your data in order to trial and refine analyses before pre-registering a study. Data scrambling was originally proposed by MacCoun and Perlmutter (2017); for more information, please see their [article](https://onlinelibrary.wiley.com/doi/pdf/10.1002/9781119095910.ch15?saml_referrer) for a detailed description and simulated examples. If you have suggested edits to this tutorial, please submit a pull request or contact Jessie Baldwin (j.baldwin@ucl.ac.uk).

2. *Holdout_script*

This script provides code to randomly split a dataset into a holdout and training sample in R.

3. *Simulation_subsample_trial_analyses*

This script reproduces the simulation reported in Box 1 of the article. This simulation examines what an appropriate sample size should be for a subset of data to test and refine analyses on, before pre-registering. In particular, it examines the trade-off between a large enough sample size to sufficiently examine characteristics of the data versus small enough sample size to avoid insight into the statistical significance of the findings. 
