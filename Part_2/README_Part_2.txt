Replication instructions for "Balancing external vs. internal validity: An application of causal forest in finance" by Huseyin Gulen, Candace E. Jens, and T. Beau Page
====================================================================================================================

This file contains a description of the steps needed to replicate all results in Section 4, which is the Monte Carlo simulations.  Below are two sets of instructions.  The first set of instructions is to run the simulations themselves.  The simulations were run on the OrangeGrid HTC system at Syracuse University (for details, see here: https://researchcomputing.syr.edu/resources/orange-grid/). The second set of instructions is to replicate the tables and figures in the paper.  Because the data are simulated, all data files are included in the replication package, so replicating the final figures and tables does not require re-running the simulations.

====================================================================================================================

INSTRUCTIONS BELOW ARE FOR REPLICATING THE SIMULATED DATA:

====================================================================================================================
SOFTWARE used to run the code:
    - Condor is the job scheduler on OrangeGrid:
		Condor version:
		$CondorVersion: 9.7.0 2022-03-13 BuildID: Debian-9.7.0-1.1 PackageID: 9.7.0-1.1 Debian-9.7.0-1.1 $
		$CondorPlatform: X86_64-Ubuntu_20.04 $
    - Singularity is the "container" system that holds R:
		singularity version 3.7.1
    - R packages: 
	majorMinor 4.3, RcppEigen 0.3.3.9.3, grf 2.3.1, rdd 0.57, syMisc 1.2.3, logging 0.10.108, dplyr 1.1.3

====================================================================================================================
REPLICATION INSTRUCTIONS: 
The code in the simulation code folder was run on a cluster running the HTCondor job management software.  The R interpreter was submitted to the cluster inside of a singularity container along with the R scripts driver.R and mc_for_loop_6.R and the configuration files simulation.sub and singularity_wrapper.sh.

The code can be run sequentially on a local installation of R using driver-local.R instead of driver.R.  Running the simulations sequentially will only require the R scripts driver-local.R and mc_for_loop_6.R.  You will have to run the driver-local.R script repeatedly, modifying the top several lines to change RUN_NUMBER and BATCH_NUMBER.  RUN_NUMBER will have to be set to each integer from 0 to 10 and BATCH_NUMBER to each integer from 1 to 19.  The 209 iterations can be run in any order except that, for each BATCH_NUMBER, RUN_NUMBER 10 will have to be run last to collect the files created with RUN_NUMBER 0-9.

====================================================================================================================

INSTRUCTIONS BELOW ARE FOR REPLICATING THE FINAL TABLES AND FIGURES FROM THE SIMULATED DATA:

====================================================================================================================
SOFTWARE used to run the code:
    - Operating system(s): Windows 11 for R code 
    - R version 4.3.2.  
        - Required packages (R scripts will load packages, but you need to pre-install them): 
                - lubridate_1.9.3, forcats_1.0.0,   stringr_1.5.1,   dplyr_1.1.4, 
 			purrr_1.0.2,     readr_2.1.4,     tidyr_1.3.0,    tibble_3.2.1,  
			tidyverse_2.0.0,  ggplot2_3.4.4,  xtable_1.8-4    
====================================================================================================================
REPLICATION INSTRUCTIONS: 
1. Install all software mentioned in the section above 
2. In R:
	- Open Project Part_2.  All results save to the output folder.  The other folders in the directory (apart from the simulation code folder) include the results from different simulation specifications:
		- sample_sizes varies sample sizes with no manipulation (pman = 0) and no latency (slope = 0)
		- hetero varies latency (slope) but holds manipulation at zero (pman = 0)
		- pman varies manipulation (pman) but holds latency at zero (slope = 0)
		- pmanhetero varies both manipulation and latency
		- interactions contains specifications that interact variables. These results are only presented in the internet appendix.  Internet appendix replication details are included below under "optional additional replication". 
	- Each folder has two sets of results, raw results and results where the most extreme 20 simulation results for each estimator have been removed.  The latter results are denoted with "_alt".  To create the "_alt" files, in each folder, open and run alternate_sims_and_bias.R.  These files will read in all results in the folder, remove the 20 most extreme simulation results for each estimator, and save the results with the ending "_alt".  We have included both the raw and _alt files in the folder, so that the final tables and figures can be replicated without this step.  
	- Run Figure_3_sample_size to replicate both Figure 3 and internet appendix Figure IA.1, which is an expanded version of Figure 3.  Figure_3_samples_size reads in _alt results from the sample_sizes folder and saves samplesize_MSresubmit.pdf and samplesize_abridged_MSresubmit.pdf to the output folder.
	- Run Figure_4_latency to replicate Figure 4.  Figure_4_latency reads in _alt results from the latency folder and saves latency_MSresubmit.pdf to the output folder.
	- To replicate Table 2, run:
		- Table_2_Panel_A_pman.R reads in _alt files from the pman folder.
		- Table_2_Panel_B_hetero.R reads in _alt files from the hetero folder.
		- Table_2_Panel_C_part1.R reads in _alt files from the pmanhetero folder.
		- Table_2_Panel_C_part2.R reads in _alt files from the pmanhetero folder.
		- Table_2_Panel_C_part3.R reads in _alt files from the pmanhetero folder.
		- Table_2_Panel_C_part4.R reads in _alt files from the pmanhetero folder.
		- Table_2_Panel_C_part5.R reads in _alt files from the pmanhetero folder.
		 Each script saves a .txt document with the same name as the R script.


OPTIONAL ADDITIONAL REPLICATION:
	We left some files to replicate extra results, including results in the appendix, in the R Project.  In the interactions folder, interaction_plot replicates internet appendix Figure IA.2.  This script reads in _alt files from the interactions folder and saves interaction_MSresubmit.pdf to the output folder.  Additionally, Table_IA_2.R reads in files from the sample_sizes folder and saves .tex files for each panel of internet appendix Table IA.2 to the output folder. 
