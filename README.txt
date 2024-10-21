Replication instructions for "Balancing external vs. internal validity: An application of causal forest in finance" by Huseyin Gulen, Candace E. Jens, and T. Beau Page
====================================================================================================================

This file provides an overview of the pieces of the replication package.  The replication package is split into three parts, each of which has its own README file in its own subfolder.

Part_1 replicates all results presented in Sections 2 and 3 of our paper.  The Part_1 subfolder includes R scripts that replicate Figure 1 and Figure 2.   

Part_2 replicates all results presented in Section 4 of our paper. The Part_2 subfolder includes R scripts that replicate: Figure 3, Figure 4, and Table 2. Note that there is no code to replicate Table 1, which presents the correlations between w and x_4 and the slope on x_4 (kappa) that our simulations consider.  The corresponding lambda and gamma presented in the table are calculated with Equations 5 and 7, respectively. 

Part_3 replicates all results presented in Section 5 of our paper. The Part_3 subfolder includes R scripts that replicate: Figure 5, Figure 6, Figure 7, Table 3, Table 4, Table 5, Table 6, and Table 7.  

Each part of the replication is structured as an RStudio project. 

If you are not using the RStudio project, you will need to define the top level of the directory as the working directory for each file.  In addition, for each file you will need to add code specifying the part of the project you are replicating.  For example, for part 3, specify: 	setwd("C:/YOURDIRECTORY/GulenJensPage_replication/Part_3")
	In which YOURDIRECTORY should be replaced with the location of the "GulenJensPage_replication/Part_3" folder.


