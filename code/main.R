#Performs all analyses for the project. Conventions we adopt:
#1) Make the R working directory be "code" before running this or any of the work package scripts listed below;
#2) All work packages start by clearing the workspace and setting the seed;
#3) All work packages end by saving their output in results.
#These conventions should minimize dependencies between packages.

#Run tests
testthat::test_dir(".")

#Import and clean the data and save in results as dataList.Rds: (Jasmin)
source("CleanDat.R")

#Work package 1 (Jasmin/Adeola): Plotting Vcom, Vsym, phiLdM, etc versus diversity, and related work
source("DivStab.R"); source("phiLdMfig.R"); source("DSRfig.R")

#Getting to know the data: Plotting community and species-level abundance and metadata histograms (Jasmin)
source("AbundanceFigs.R"); source("metaHistograms.R")

#Work package 2 (Adeola/Vadim) Taylor's law
source("Overyielding.R")


#Work package 3: (Nat/Jasmin) Under/overyielding



#Work package 4: (Dan/Nat, also Lei later) Parallel analysis to Lei's ms
source("LeiAnalysis.R")
