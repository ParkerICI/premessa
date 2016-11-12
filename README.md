# cytofNormalizeR

R implementation of bead normalization for CyTOF data. The idea behind the method is described in [this](https://www.ncbi.nlm.nih.gov/pubmed/23512433) publication. cytofNormalizeR is an R re-implementation of the [original](https://github.com/nolanlab/bead-normalization) normalization software developed for Matlab.


# Installation


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

This will install the cytofNormalizeR R package together with all the required dependencies.

*Note*: the latest version of devtools seems to be occasionally having problems installing dependencies on Windows. If the installation of cytofNormalizeR fails for a missing package, please install the offending packages manually, using the R *install.packages* function


# Usage

**---> Make sure to have a backup copy of your data before you use the software! <---**

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
          |--- removed_events
               |--- A_normalized_removedEvents.fcs
               |--- B_normalized_removedEvents.fcs
     |--- beads
          |--- A_beads.fcs
          |--- B_beads.fcs
```

- *A_normalized.fcs*: contains the normalized data, with an added parameter called *beadDist* representing the square root of the Mahalanobis distance of each event from the centroid of the beads population
- *beads_before_and_after.pdf*: a plot of the median intensities of the beads channels before and after normalization
- *A_normalized_beadsremoved.fcs*: the normalized data with the beads events removed
- *A_normalized_removedEevents.fcs*: the events that have been removed from the normalized data based on the Mahalanobis distance cutoff 
- *A_beads.fcs*: the beads events, as identified by gating

## Selecting the working directory

You can start the cytofNormalizeR GUI by typing the following commands in your R session

```
library(cytofNormalizeR)
cytofNormalizeR.run()
```
This will open a new web browser window, which is used for displaying the GUI. Upon starting, a file selection window will also appear from your R session. You should use this window to navigate to the directory containing the data you want to normalize, and select any file in that directory. The directory itself will then become the working directory for the software.

To stop the software simply hit the "ESC" key in your R session.

The GUI is organized in two tabs:
- *Normalize data*: used for beads gating and data normalization 
- *Remove beads*: used for beads removal

## *Normalize data* panel

This panel contains the following controls:

- *Select beads type*: select the type of normalization beads that have been used for the experiment. Most users will select the default *Fluidigm Beads (140, 151, 153, 165, 175)*. These are the beads [sold](https://www.fluidigm.com/reagents/proteomics/201078-eq-four-element-calibration-beads--100ml) by Fluidigm. The numbers indicate the beads channels used for normalization.
- *Select FCS file*: the FCS  that is currently being visualized for gating. This dropdown will contain all the FCS files located in the working directory. The gating plots will appear under the row of buttons.
- *Select baseline for normalization*: the baseline beads intensities to be used for normalization. You can either use the median beads intensities of the FCS files that you are currently using for normalization (*Current files* option), or the median intensities of an existing set of beads files (*Existing folder of beads files*, see below).
- *Identify beads*: clicking this button will color in red the events that are recognized as beads events in the gating plots.
- *Apply current gates to all files*: applies the current gates to all the files.
- *Normalize*: starts the normalization routine.

The workflow involves cycling through all the files and adjusting the beads gates in the plot, in order to identify the beads. Only events that are included in *all* the beads gates are identified as beads. As detailed in the dialog box that is above the row of buttons, only files for which the gates have been defined will be used as input for normalization.

You can cycle back and forth between different files, as the GUI will remember the gates you have selected for each file.

If you want to use existing beads files as the baseline for normalization, a file dialog window will pop-up when you hit the *Normalize* button. Use the window to navigate to a directory containing FCS files containing beads events only (for instance the *A_beads.fcs* file in the above example) and select one of the files. The software will then load *all* the files contained in the same directory as the file you selected.

## *Remove beads* panel

This panel has the following controls

- *Select beads type*: same as for the *Normalize data* panel: select the type of normalization beads that have been used for the experiment. 
- *Select FCS file*: select the FCS file for plotting. The dropdown will contain all the FCS files located in the *normed* sub-folder of the working directory. The plots will appear below the row of buttons. See below for a description of what the plots represent
- *Cutoff for bead removal*: the Mahalanobis distance cutoff to be used for bead removal (see below).
- *Remove beads (current file)*: use the current cutoff to remove beads from the currently selected file
- *Remove beads (all files)*: use the current cutoff to remove beads from all the files in the folder (i.e. all the files that are listed in the *Select FCS file* dropdown).
 
The bead removal procedure is based on the idea of looking at the distance between each event and the centroid of the beads population, and removing all the events that are closer than a given threshold to the beads population, and therefore are likely to represent beads as opposed to true cells.

To this end, during the normalization the software calculates the square root of the Mahalanobis distance of each event from the centroid of the beads population, and records this information in the *beadDist* parameter in the FCS file with the normalized data (i.e. the *_normalized.fcs* files in the *normed* sub-folder).

During the beads removal step, all the events whose *beadDist* is less or equal than the *Cutoff for bead removal* parameter are removed from the FCS. The removed events are saved in the *rmeoved_events* sub-folder (see above).

The plots in the bottom half of the panel help you select an appropriate cutoff. They display all the pairs of beads channels. Beads should appear as a population in the upper right corner (as they will be double-positives for all the channel pairs). The color of the points represent the distance from the beads population. You should choose a cutoff so that most of the bead events are below the cutoff, and most of the non-beads events are above it. The legend for the color scale is located above the plots.

# Differences with the Matlab Normalizer

The normalization algorithm is exactly identical to the on used in the original Matlab implementation of the normalizer. The only differences relate to the way the GUI manages the workflow, and to the organization of the output directory structure.

With the Matlab implementation, when you do gating and beads removal, you process one file at the time, and there is no way to look back at previously analyzed files, for instance for adjusting the gates.

cytofNormalizeR separates the two steps of the workflow. First you setup the gates, moving back and forth between the input files as needed. This is useful for instance if you want to use the exact same gates for all the files, because you need to visualize all the data, before you can identify gates that will work across all the files. Once the gates have been setup, all the files are normalized and the results are saved. You then switch to the beads removal step, once again visualizing the files back and forth as needed, until you have selected an appropriate cutoff. You can then either remove beads from a single file, or from all the files simultaneously.
