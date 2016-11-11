# cytofNormalizeR
R implementation of bead normalization for CyTOF data


#Installation


## Install required R packages

You need to install the devtools package, available from CRAN, and the flowCore package from Bioconductor. The rest of the dependencies for cytofNormalizeR will be automatically installed

#### Devtools

Open an R session, type the following command and select a CRAN mirror when prompted.

`install.packages("devtools")`

#### FlowCore

Open an R session and type the following commands

```
source("http://bioconductor.org/biocLite.R")
biocLite("flowCore")
```

## Install cytofNormalizeR

Once you have succesfully completed the steps above, start an R session and type the following commands

```
library(devtools)
install_github("ParkerICI/cytofNormalizeR")
```

This will install the SCAFFoLD R package together with all the required dependencies. If evertyhing was successful you should be able to start cytofNormalizeR by typing the following commands

```
library(cytofNormalizeR)
cytofNormalizeR.run()
```
to stop cytofNormalizeR simply hit the "ESC" key in your R session.

*Note*: the latest version of devtools seems to be occasionally having problems installing dependencies on windows. If the installation of cytofNormalizeR fails for a missing package, please install the offending packages manually, using the R *install.packages* function


# Usage


The normalization workflow involves the following steps:

1. Beads identification through gating
2. Data normalization
3. Beads removal (optional)

Assuming the working directory is called *working_directory* and  contains two FCS files called *A.fcs* and *B.fcs*, at the end of the workflow the following directory structure and output files will be generated

```
working_directory
|--- A.fcs
|--- B.fcs
|--- normed
     |--- A_normalized.fcs
     |--- B_normalized.fcs
     |--- beads_before_and_after.pdf
     |--- beads_removed
          |--- A_normalized_beadsremoved.fcs
          |--- B_normalized_beadsremoved.fcs
          |--- beads
               |--- A_beads.fcs
               |--- B_beads.fcs
```

- *A_normalized.fcs*: contains the normalized data, with an added parameter called *beadDist* representing the square root of the Mahalanobis distance of each event from the centroid of the beads population
- *beads_before_and_after.pdf*: a plot of the median intensities of the beads channels before and after normalization
- *A_normalized_beadsremoved.fcs*: the normalized data with the beads events removed
- *A_beads.fcs*: the beads events that have been removed

The GUI is organized in two tabs:
- *Normalize data*: used for beads gating and data normalization 
- *Remove beads*: used for beads removal

## *Normalize data* tab

This panel contains the following controls:

-*Select beads type*: select the type of normalization beads that have been used for the experiment. Most users will select the default *Fluidigm Beads (140, 151, 153, 165, 175)*. These are the beads [sold](https://www.fluidigm.com/reagents/proteomics/201078-eq-four-element-calibration-beads--100ml) by Fluidigm. The numbers indicate the beads channels used for normalization.


#Differences with the Matlab Normalizer

