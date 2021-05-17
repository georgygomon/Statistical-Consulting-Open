Dear user,

In this git repository you can find the R-code used by Ella and Georgy during our Statistical Consulting Project.
To set up the analysis please carry-out the following steps:

1. Obtain the encrypted data-file from PSYTREC and save it as 'My_version.sav' in the folder with all .R files.
2. Open Stat_Consulting.Rproj: this will automatically set your working directory to the project directory.
3. Open the desired .R file. 
4. Each separate .R file calls Creating_data.R. In Creating_data.R all necessary libraries are loaded (and if necessary, installed) and the data is loaded.


The following .R files are present:
1. LMM_1.R: Fits a Linear Mixed Model with as outcome the depression severity as measured by the QIDS
2. GLMM_1.R: Fits a Binomial Generalized Linear Mixed Model with as binary outcome depression on the MINI
3. RCI_1.R: Patients classified as Recovered, Improved, Unchaged or Deteriorated of MDD on basis of the Reliable Change Index 
 	    and Clinical Significance. Based on the QIDS-SR 16
4. LMM_2.R: Fits a Linear Mixed Model with as outcome PTSD severity as measured by the CAPS
5. RCI_1.R: Patients classified as Recovered, Improved, Unchaged or Deteriorated of PTSD on basis of the Reliable Change Index 
 	    and Clinical Significance. Based on the CAPS
6. Creating_data.R: This file is called by all other files. Here all necessary libraries are loaded (and if necessary, installed) and the data is loaded.

If there are any questions please feel free to email g.gomon@umail.leidenuniv.nl or e.a.hoogendoorn@umail.leidenuniv.nl.

Good luck!
Ella & Georgy
