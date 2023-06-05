## Project title: "Deterrent Effects of SQFs on Arrests in New York City: A Spatial Analysis for 2006"

**Author: Jonas Frost (frostjonas@web.de)**

This project tries to estimate deterring effects of Stop-Question-and-Frisks (SQFs) conducted by the
NYPD in New York in 2006. 
The final paper can be found here: [/Final_Paper.pdf](/Final_Paper.pdf)

The project was conducted as part of a masters course on spatial econometrics at the University of Leipzig.

This README will explain the files used in this project and the file structure of the directory.


## Important files

The root directory of this repository includes 3 subdirectories and several files. [/Dashboard_Idea.html](/Dashboard_Idea.html), 
[/Slides_results.pdf](/Slides_results.pdf) and [/Final_Paper.pdf](/Final_Paper.pdf) include reports of the results at different stages of the analysis. 
While the dashboard was created to introduce the project idea, the slides present the research design and some
results. The final paper lays out the whole research project, its methods and results.

## /Data (Not pushed to GitHub, but available upon request!)

The Data folder is divided into two subdirectories: Data/Raw and Data/Clean

**/Data/Raw/:**
This folder holds the raw downloaded data on SQFs and arrests, as well as a few shape files. 
The arrest and SQF data and documentation can also be found on [New Yorks open data platform](https://opendata.cityofnewyork.us/).
The arrest data is saved as Data/Raw/NYPD_Arrests_data__Historic_.csv .
All other files in this directory are shape files of New York. These and the raw data files
are loaded in Code/load_data.R.

**/Data/Clean/:**
This sub-directory holds the cleaned data and saved results. 
- /Data/Clean/data_arrest.R and Data/Clean/data_sqf.R hold the cleaned data sets for all years. 
- /Data/Clean/n_sqf_all.R is a save of the numbers of SQFs per year before cleaning the data set.
- /Data/Clean/data_final.Rds and /Data/Clean/data_final_long.Rds include the generated arrest
  counts for each SQF in 2006 in two formats.
- /Data/Clean/Ks.R and Data/Clean/Kcross_save.R are saved results of the computed K-statistics.
- /Data/Clean/quadcounts.R saves the results of /Code/quadcount.R.
- /Data/Clean/nyc_shape_ll.R and /Data/Clean/nyc_zip_shap_ll.R are saves of transformed shapefiles with
  longitude and lattitude as reference system. 
- /Data/Clean/lmer_dat_simple.Rds holds a simplified data set used for a few regression analyses.
- /Data/Clean/regressions_simple.Rda holds the results of these regression alnalyses. 	  


## [/Code](/Code)

This directory holds the R-scripts that were written during the analysis. Most importantly, the makefile.

**[/Code/Makefile.R](/Code/Makefile.R):** 
This script loads all relevant packages and sources the other scripts in order. The makefile is sourced at the 
beginning of the final paper generating file. In order for it to work correctly, the package "needs" has to be installed.
Using this package, all other relevant R-packages are installed (if needed) and loaded. 

**[/Code/load_data.R](/Code/load_data.R):**
This script loads the raw data from the Data/Raw folder, if needed. It combines the data sets of arrests and SQFs from 2006 
to 2011 into two master data sets. It also loads shapefiles of New York City.

**[/Code/quadcount.R](/Code/quadcount.R):**
This script conducts a raster count of arrests and SQFs for all years and tests for correlation. If the saved file
Data/Clean/quadcounts.R exists, the computation is skipped, and the file is loaded. This is because the computation
takes some time.

**[/Code/Cluster_Analysis.R](/Code/Cluster_Analysis.R):**
This script computes K-functions for SQFs and arrests for each year. After, it computes the cross K-statistics.
If the saved file Data/Clean/Ks.R exists, the computation is skipped and the file is loaded. This is because the computation 
takes some time.

**[/Code/make_data.R](/Code/make_data.R):**
This script conducts the computation of arrests counts for each SQF on the 10 days before and after, count the number of 
other SQFs in the area and repeats the process for the three different radii. It generates the random control events and repeats
the counting process for those. This file is sourced in the makefile, if [Data/Clean/data_final.Rds](Data/Clean/data_final.Rds) and 
[Data/Clean/data_final_long.Rds](Data/Clean/data_final_long.Rds) cannot be loaded. 

**[/Code/Regressions.R](/Code/Regressions.R):**
This file runs different regression models to estimate the effects of SQFs on arrests in 2006.


## [/Writing](/Writing)

This folder includes the written outputs of the project. [/Writing/Literatur.bib](/Writing/Literatur.bib) holds the referenced biblographies. 
[/Writing/Logo.png](/Writing/Logo.png) and [/Writing/Logo_small.png](/Writing/Logo_small.png) are two versions of the university logo.
[/Writing/zfs.csl](/Writing/zfs.csl) is a csl-file used to define the referencing style used in the writing. 
The subdirectories Poster, Proposal, Slides and Term_Paper each include a .Rmd-file that generates the output and a .pdf or .html-file 
that is the latest version of the output. All other files in these subdirectories are caching-files.


Note: 
At the beginning of the project, it was planned to conduct an analysis for the years from
2006 to 2011. In the process, the scale of the project had to be reduced to only 2006. This is why
some scripts still compute statistics for all years. Later on, only 2006 is of interest. It was decided
not to remove the parts also applicable for the other years, as the code might be of interest when 
rerunning the analysis for all years.
