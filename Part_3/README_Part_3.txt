Replication instructions for "Balancing external vs. internal validity: An application of causal forest in finance" by Huseyin Gulen, Candace E. Jens, and T. Beau Page
====================================================================================================================

This file contains a description of the steps needed to replicate all results in Section 5, which is the application.

====================================================================================================================
SOFTWARE used to run the code:
    - Operating system(s): Windows 11 for R code 
    - R version 4.3.2.  
        - Required packages (R scripts will load packages, but you need to pre-install them): 
                - lubridate_1.9.3, forcats_1.0.0,   stringr_1.5.1,   dplyr_1.1.4, 
 			purrr_1.0.2,     readr_2.1.4,     tidyr_1.3.0,    tibble_3.2.1,  
			tidyverse_2.0.0,  ggplot2_3.4.4,  xtable_1.8-4, Formula_1.2-5, 
			AER_1.2-14, lmtest_0.9-40, zoo_1.8-12, sandwich_3.1-0, stargazer_5.2.3,
			glmnet_4.0-2, Matrix_1.6-1.1, fastDummies_1.7.4, data.table_1.14-10,
			scales_1.3.0, ggpubr_0.6.0, gridExtra_2.3, hash_2.2.6.3, grf_1.1.0, 			rdd_0.57, rdrobust_2.2
    - Rtools 43 to install grf_1.1.0
====================================================================================================================
REPLICATION INSTRUCTIONS: 
1. Install all software mentioned in the section above 
2. In R, open Project Part_3.  The Data folder contains all publicly available datasets, and the Data cleaning folder includes files to replicate some third party data.  Other third party data must be downloaded directly from the source.  All final results save to the output folder. 
3. To compile data, run the following scripts in the Data cleaning folder in order.  
	- dealscan.R
		downloads Dealscan data
		outputs "dealscan.Rdata" to Data folder	(Note:  our sample is from January 2019.)
	- funda.R
		downloads annual Compustat data
		outputs "funda.Rdata" to Data folder (Note:  our sample is from January 2019.)
	- fundq.R
		downloads quarterly Compustat data
		outputs "fundq.Rdata" to Data folder (Note:  our sample is from March 2021.)
	- load_data.R
		loads raw data files, combines them into one Rdata file for easier access
		loads: cpi.csv, sec_covenant_violations_24_Sep_2012.xlsx, dealscan_compustat_link.xlsx, dealscan.Rdata, fundq.Rdata, funda.Rdata
		outputs "rawdata.Rdata" to Data folder
	- merge_data.R
		adds "gvkey" to Dealscan tables so they can be merged with Compustat
		loads: rawdata.Rdata 
		outputs "merge_data.Rdata" to Data folder
	- extend_compustat_data.R
		transforms raw Compustat quarterly data into useful form (e.g., quarterly versions of year-to-date only variables, compute ratios)
		loads: rawdata.Rdata, ratings.csv
		outputs "extend_compustat_data.Rdata" to Data folder
	- link_dealscan_fundq_with_dates.R
		merge quarterly Compustat and Dealscan data, compute covenant ratios
		loads: merge_data.Rdata, extend_compustat_data.Rdata, lenders.csv, 
		outputs "link_dealscan_fundq_with_dates.Rdata" to Data folder
	- collect_processed_data.R
		combines files into one Rdata file for easier access
		loads: rawdata.Rdata, extend_compustat_data.Rdata, link_dealscan_fundq_with_dates.Rdata
		outputs "collect_processed_data.Rdata" to Data folder
4. To recreate our sample of analysis, from the Different samples folder, run NWallnoloan.  NWallnoloan loads collect_processed_data.Rdata, finds the loan with minimum distance to default (for net worth covenants only), determines if a loan is in default, drops financial and regulated firms, winsorizes variables of interest, drops firms with non-positive leverage, and saves "NWallnoloan.Rdata" to the "Samples" folder.

The Different samples folder also contains two files, Bothallnoloan and CRallnoloan, that produce samples we used for previous versions of the paper.  CRallnoloan creates a sample of firms with current ratio covenants and Bothallnoloan creates a sample of firms with either a current ratio or net worth covenant.  

5. To replicate Tables 6 and 7, run the following scripts from the RDD code folder:
	- Table_6_Panel_A.R loads NWallnoloan.Rdata and saves "Table_6_Panel_A.txt" to the output folder.
	- Table_6_Panel_B.R loads NWallnoloan.Rdata and saves "Table_6_Panel_B.txt" to the output folder.
	- Table_6_Panel_C.R loads NWallnoloan.Rdata and saves "Table_6_Panel_C.txt" to the output folder.
	- Table_6_Panel_D.R loads NWallnoloan.Rdata and saves "Table_6_Panel_D.txt" to the output folder.
		(Note:  apart from the panel names, the only difference between panels is the subsample of analysis, defined around lines 36-38 in each file.)
	- Table_7_Panel_A.R 
		Presents regressions of investment on bind (= 1 if a firm is in technical default, = 0 otherwise) with firm and year-quarter fixed effects, with and without controls and different polynomials of distance to default for the net worth covenant sample (NWallnoloan.Rdata) as in Table 7, Panel A.  The script saves "Table_7_Panel_A.tex" to the output folder using the Stargazer package.

	- Table_7_Panel_B.R
		Presents regressions of investment on bind (= 1 if a firm is in technical default, = 0 otherwise) with firm and year-quarter fixed effects, controls, and different polynomials of distance to default interacted with bind for the net worth covenant sample (NWallnoloan.Rdata) as in Table 7, Panel B. The script saves "Table_7_Panel_B.tex" to the output folder using the Stargazer package.

	- Table_7_Panel_C.R
		Presents regressions of investment on bind (= 1 if a firm is in technical default, = 0 otherwise) with firm and year-quarter fixed effects, with and without controls and different polynomials of distance to default for observations within a bandwidth around the default threshold for the net worth covenant sample (NWallnoloan.Rdata) as in Table 7, Panel C. The script saves "Table_7_Panel_C.tex" to the output folder using the Stargazer package.

6. To replicate our causal forest analysis, run the following script from the Estimate causal forests folder:
	- NWallnoloan_cf.R:
		firm-level fixed effects using LASSO, classification forest for default status, regression forest for investment, and causal forest. Save the 3 forests to the "Causal Forest Output/Forests" and the data used for each forest to the "Causal Forest Output/Forest Data" folders.  
This script requires grf_1.1.0 and cannot run with more recent versions of grf. Lines 5-6 of NWallnoloan_cf downloads the correct version of grf but requires the pre-installation of Rtools.  Note that on a normal laptop, this might run for 15-20 minutes.
		loads: NWallnoloan.Rdata
		outputs NWallnoloan_forest.Rdata to the Causal Forest Output/Forests subfolder and NWallnoloan_forest_data.Rdata to the Causal Forest Output/Forest Data

7. To replicate causal forest results (Figure 5, Figure 6, Figure 7, Table 3, Table 4, and Table 5), run the following from the Causal forest results folder:
	- Figure_5:
		loads NWallnoloan_forest.Rdata from Causal Forest Output/Forests
		saves Figure_5.pdf to the output folder.  Note that the statistics given in the figure are from Table IA.6 in the internet appendix.
	- Figure_6:
		Figure 6 presents comparative statistics and requires grf_1.1.0.
		loads NWallnoloan_forest_data.Rdata from Causal Forest Output/Forest Data and NWallnoloan_forest.Rdata from Causal Forest Output/Forests
		saves Figure_6.pdf to output folder.
	- Figure_7: 
		loads NWallnoloan_forest_data.Rdata from Causal Forest Output/Forest Data
		saves Figure_7.pdf to output folder.
	- Table 3:
		Table 3 presents variable importance and requires grf_1.1.0.  Note that on a normal laptop, this might run for 8-10 hours.
		loads NWallnoloan_forest_data.Rdata from Causal Forest Output/Forest Data and NWallnoloan_forest.Rdata from Causal Forest Output/Forests
		saves Table_3.tex to output folder.
	- Table 4:
		loads NWallnoloan_forest_data.Rdata from Causal Forest Output/Forest Data and NWallnoloan_forest.Rdata from Causal Forest Output/Forests
		saves Table_4.tex to output folder.	
	- Table 5:
		loads NWallnoloan_forest_data.Rdata from Causal Forest Output/Forest Data and NWallnoloan_forest.Rdata from Causal Forest Output/Forests
		saves Table_5.tex to output folder
		
OPTIONAL ADDITIONAL REPLICATION:
	We left in the R Project additional scripts to replicate key results that are left to the appendix:
	- Table_IA_5 reproduces the summary statistics.
	- Table_IA_6 reproduces the average treatment effect table.
	- Table_IA_7 reproduces a table that presents conditional average treatment effects on different subsamples.
	- Figure_IA_4 plots the hte dot plots against covariates.
	- Figure_IA_7 plots the propensity to be in default around the threshold.


========================================================================================
DATASETS

Below are datasets we used in our study. All datasets should be placed in the "Data" folder.

The datasets described in the "THIRD-PARTY" section are omitted from the replication package since they are proprietary. 

The datasets in the "PUBLIC" section are freely available, so we included them in full. Note that we made these downloads between 2018 and 2023, so the data may have changed in the meantime. 

THIRD-PARTY: 
==============================
To load dealscan, fundq, and funda data, we use the WRDS ODBC interface for R.  See details here: https://wrds-www.wharton.upenn.edu/pages/about/3-ways-use-wrds/
lenders.csv and ratings.csv were downloaded separately. 


	dealscan.Rdata
		financial covenant and net worth covenant tables from Dealscan (see dealscan.R for how we downloaded this data)
	funda.Rdata
		annual firm data from Compustat (see funda.R for how we downloaded this data)
	fundq.Rdata
		quarterly firm data from Compustat (see fundq.R for how we downloaded this data)
	lenders.csv
		FacilityIDs downloaded from WRDS/Dealscan used to identify syndicate size (Note:  our sample is from April 2019.)
	ratings.csv
		S&P credit ratings from Compustat (downloaded from WRDS)  (Note:  our sample is from April 2019.)

PUBLIC:
==============================

	cpi.csv
		CPI data downloaded from BLS (Note:  our sample is from April 2019.)
	dealscan_compustat_link.xlsx
		downloaded from Michael Roberts' website in December 2018.  Updated files are available at: https://finance.wharton.upenn.edu/~mrrobert/research.html. 
	sec_covenant_violations_24_Sep_2012.xlsx
		downloaded from Michael Roberts' website in December 2018.  Updated files are available at: https://finance.wharton.upenn.edu/~mrrobert/research.html. 

