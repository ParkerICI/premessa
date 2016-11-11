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


#Usage


The normalization workflow involves the following steps:

1) Beads identification through gating
2) Data normalization
3) Beads removal (optional)



The GUI has two tabs *Normalize data* and *Remove beads*.


#Differences with the Matlab Normalizer

