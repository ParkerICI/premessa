# cytofNormalizeR

R implementation of bead normalization for CyTOF data. 

**---> Make sure to have a backup copy of your data before you use the software! <---**


# Installation


## Install required R packages

You need to install the devtools package, available from CRAN, and the flowCore package from Bioconductor. The rest of the dependencies for cytofNormalizeR will be automatically installed

#### devtools

Open an R session, type the following command and select a CRAN mirror when prompted.

```
install.packages("devtools")
```

#### flowCore

Open an R session and type the following commands

```
source("http://bioconductor.org/biocLite.R")
biocLite("flowCore")
```

## Install cytofNormalizeR

Once you have succesfully completed the steps above, you have to create a Github token by following [these instructions.](https://help.github.com/articles/creating-an-access-token-for-command-line-use/) (This won't be necessary anymore when the repository goes public).
Copy the token, start an R session and type the following commands, substituing your Github token

```
library(devtools)
install_github("ParkerICI/cytofNormalizeR", auth_token = "YOUR TOKEN HERE")
```

This will install the cytofNormalizeR R package together with all the required dependencies.

*Note*: the latest version of devtools seems to be occasionally having problems installing dependencies on Windows. If the installation of cytofNormalizeR fails due to a missing package, please install the offending packages manually, using the R *install.packages* function


# Usage

## Starting the GUI and selecting the working directory

You can start the cytofNormalizeR GUI by typing the following commands in your R session

```
library(cytofNormalizeR)
cytofNormalizeR.run()
```
This will open a new web browser window, which is used for displaying the GUI. Upon starting, a file selection window will also appear from your R session. You should use this window to navigate to the directory containing the data you want to analyze, and select any file in that directory. The directory itself will then become the working directory for the software.

To stop the software simply hit the "ESC" key in your R session.



## De-barcoding

The idea behind the method is described in [this](https://www.ncbi.nlm.nih.gov/pubmed/25612231) publication. This software represents an R re-implementation of the [original](https://github.com/nolanlab/single-cell-debarcoder) debarcoding software developed for Matlab.

Sample data for testing is available [here](https://github.com/nolanlab/single-cell-debarcoder/tree/master/sample_files) (you only need the *.csv* and *.fcs* files)

Upon launching the GUI you will have access to the following controls:

- *Select FCS file*: the FCS file you want to debarcode. The dropdown menu is populated with the list of FCS files present in the working directory
- *Select barcode key*: the CSV file containing the barcode key. The dropdown is populated with the list of *.csv* files present in the working directory. Upon selecting both the FCS file and the key, the preliminary debarcoding process will start immediately. After a few seconds a number of diagnostic plots will appear in the right portion of the window (see [below](#plot-types))
- *Minimum separation*: the minimum seperation between the positive and negative barcode channels that an event needs to have in order to be assigned to a sample. Events where the separation is less than this threshold are left unassigned. This filtering is done after rescaling the intensity of the barcode channels, and therefore the threshold should be a number between 0 and 1
- *Maximum Mahalanobis distance*: the maximum distance between a single cell event, and the centroid of the sample the event has been assigned to. Events with distance greather than the threshold are left unassigned. The distance is capped at 30, so the default value of this option does not apply a filter based on Mahalanobis distance.
- *Plot type*: selects the type of plot to be displayed. Please see [below](#plot-types) for a description of the plots. Depending on the plot type, a few additional controls may be displayed:
  - *Select sample*: select a specific sample for plotting. Sample names are taken from the barcode key
  - *Select x axis*: select the channel to be displayed on the x axis
  - *Select y axis*: select the channel to be displayed on the y axis
- *Save files*: hitting this button will apply the current settings, performed the debarcoding, and save the resulting output files

Assuming the working directory is called *working_directory* and contains an FCS file called *barcoded_data.fcs* and a barcode key called *barcode_key.csv* that defines 3 barcoded populations (A, B, C), the following directories and output files will be created at the end of the debarcoding process:

```
working_directory
|--- barcoded_data.fcs
|--- barcode_key.csv
|--- debarcoded
     |--- barcoded_data_A.fcs
     |--- barcoded_data_B.fcs
     |--- barcoded_data_C.fcs
     |--- barcoded_data_Unassigned.fcs
```



### Plot types

There are four types of visualization that allow you to inspect the results of the debarcoding process, and choose the optimal seperation and Mahalanobis distance thresholds. Each plot window is subdivided into a top and bottom section, as described below:

- *Separation*
  - *Top*: A histogram of the separation between the positive and negative barcode channels for all the events
  - *Bottom*: Barcode yields as a function of the separation threshold. As the threshold increases, the number of cells assigned to each sample decreases, and more events are left unassigned. The currently selected threshold is displayed as a vertical red line.
- *Event*
  - *Top*: Bargraph or cell yields for each sample after debarcoding, given the current settings
  - *Bottom*: Scatterplot showing the barcode channel intensities for each event, ordered on the x axis. The left plot displays the original data, arcsinh transformed. The plot on the right displays the data rescaled between 0 and 1, which is actually used for debarcoding. Both plots only displays data for the selected sample (use the *Select sample* dropdown to pick the active sample)
- *Single biaxial*
  - *Top*: Bargraph of cell yields for each sample after debarcoding, given the current settings
  - *Bottom*: Scatterplot of barcode channel intensities for the selected sample. The channels displayed on the x and y axis are selected using the dropdown menus on the left (*Select x axis*, *Select y axis*). The points are colored according to the Mahalanobis distance from the centroid of the population. The color scale is displayed on top of the graph. The values plotted are arcsinh  transformed.
- *All barcode biaxials*
  - *Top*: Bargraph of cell yields for each sample after debarcoding, given the current setting
  - *Bottom*: a matrix of scatterplots of barcode channel intensities for the selected sample. All possible combinations are displayed, and the channels represented on the rows and columns are identified by the names on the axes of the bottom and left-most plots. The plots on the diagonal display the distribution of intensity values for the corresponding channel

### De-barcoding without the GUI

All the R functions necessary to perform debarcoding can be accessed directly, and documentation is available in the standard R documentation format, accessible through the R session. The main wrapper function that executes the entire procedure is called `debarcode_fcs`. Inspecting the code of this function should give you an idea of what are the steps involved, and which functions perform each one.

## Bead-based normalization

The idea behind the method is described in [this](https://www.ncbi.nlm.nih.gov/pubmed/23512433) publication. This software represents an R re-implementation of the [original](https://github.com/nolanlab/bead-normalization) normalization software developed for Matlab.

Sample data for testing is available [here](https://github.com/nolanlab/bead-normalization/tree/master/sample_data) (only download the FCS in the top level directory, not the contents of the beads and normed sub-folders).

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

The GUI is organized in two tabs:
- *Normalize data*: used for beads gating and data normalization 
- *Remove beads*: used for beads removal

### *Normalize data* panel

This panel contains the following controls:

- *Select beads type*: select the type of normalization beads that have been used for the experiment. Most users will select the default *Fluidigm Beads (140, 151, 153, 165, 175)*. These are the beads [sold](https://www.fluidigm.com/reagents/proteomics/201078-eq-four-element-calibration-beads--100ml) by Fluidigm. The numbers indicate the beads channels used for normalization.
- *Select FCS file*: the FCS  that is currently being visualized for gating. This dropdown will contain all the FCS files located in the working directory. The gating plots will appear under the row of buttons.
- *Select baseline for normalization*: the baseline beads intensities to be used for normalization. You can either use the median beads intensities of the FCS files that you are currently using for normalization (*Current files* option), or the median intensities of an existing set of beads files (*Existing folder of beads files*). If you select the latter a file dialog window will pop-up when you select the option. Use the window to navigate to a directory containing FCS files containing beads events only (for instance the *A_beads.fcs* file in the above example) and select one of the files. The software will then load *all* the files contained in the same directory as the file you selected. The currently selected folder will be displayed in a text box on the right. 
- *Identify beads*: clicking this button will color in red the events that are recognized as beads events in the gating plots.
- *Apply current gates to all files*: applies the current gates to all the files.
- *Normalize*: starts the normalization routine.

The workflow involves cycling through all the files and adjusting the beads gates in the plot, in order to identify the beads. Only events that are included in *all* the beads gates are identified as beads. As detailed in the dialog box that is above the row of buttons, only files for which the gates have been defined will be used as input for normalization.

You can cycle back and forth between different files, as the GUI will remember the gates you have selected for each file.

 
### *Remove beads* panel

This panel has the following controls

- *Select beads type*: same as for the *Normalize data* panel: select the type of normalization beads that have been used for the experiment. 
- *Select FCS file*: select the FCS file for plotting. The dropdown will contain all the FCS files located in the *normed* sub-folder of the working directory. The plots will appear below the row of buttons. See below for a description of what the plots represent
- *Cutoff for bead removal*: the Mahalanobis distance cutoff to be used for bead removal (see below).
- *Remove beads (current file)*: use the current cutoff to remove beads from the currently selected file
- *Remove beads (all files)*: use the current cutoff to remove beads from all the files in the folder (i.e. all the files that are listed in the *Select FCS file* dropdown).
 
The bead removal procedure is based on the idea of looking at the distance between each event and the centroid of the beads population, and removing all the events that are closer than a given threshold to the beads population, and therefore are likely to represent beads as opposed to true cells.

To this end, during normalization, the software calculates the square root of the Mahalanobis distance of each event from the centroid of the beads population, and records this information in the *beadDist* parameter in the FCS file with the normalized data (i.e. the *_normalized.fcs* files in the *normed* sub-folder).

During the beads removal step, all the events whose *beadDist* is less or equal than the *Cutoff for bead removal* parameter are removed from the FCS. The removed events are saved in the *removed_events* sub-folder (see above).

The plots in the bottom half of the panel help you select an appropriate cutoff. They display all the pairs of beads channels. Beads should appear as a population in the upper right corner (as they will be double-positives for all the channel pairs). The color of the points represent the distance from the beads population. You should choose a cutoff so that most of the bead events are below the cutoff, and most of the non-beads events are above it. The legend for the color scale is located above the plots.

### Differences with the Matlab Normalizer

The normalization algorithm is exactly identical to the one used in the original Matlab implementation of the normalizer. The only differences relate to the way the GUI manages the workflow, and to the organization of the output directory structure.

With the Matlab implementation, when you do gating and beads removal, you process one file at the time, and there is no way to look back at previously analyzed files, for instance for adjusting the gates.

cytofNormalizeR separates the two steps of the workflow. First you setup the gates, moving back and forth between the input files as needed. This is useful if, for instance, you want to use the exact same gates for all the files, because you need to visualize all the data, before you can identify gates that will work across all the files. Once the gates have been setup, all the files are normalized and the results are saved. 

You then switch to the beads removal step, once again visualizing the files back and forth as needed, until you have selected an appropriate cutoff. You can then either remove beads from a single file, or from all the files simultaneously. Because the intermediate normalization results have been saved, you can repeat the beads removal step if needed.
