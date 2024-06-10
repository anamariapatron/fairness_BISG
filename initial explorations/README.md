# Tutorial: Statistical Methods for Imputing Race and Ethnicity

## Overview

The purpose of this tutorial is to demonstrate how to use some of the methods discussed in "Statistical Methods for Imputing Race and Ethnicity" in Python and R. The tutorial scripts can be run to reproduce the figures in "Appendix D: Sample Tutorial Output" in the paper.

The data used for this tutorial is simulated, and does not reflect the true distribution of names, locations, races/ethnicities, or any other characteristic. The intent of this tutorial is purely to show how the code could be written and does not produce meaningful results.

## Getting Started

The main scripts demonstrating the use of various race imputation methods are the Python and R scripts whose names start with "tutorial". To demonstrate possible metrics for examining the performance of each method, the tutorial scripts import functions from the "metrics" Python and R scripts. In order to run these scripts, a few setup steps are required, including installing the required software and packages, and setting up a Census API key (for `wru_tutorial.r`).

### Python setup

The Python tutorials were developed under Python 3.11, which can be downloaded from the [Python website](https://www.python.org/downloads/release/python-3110/). Downloads of Python from the Python website include the pip package manager, which can be used to install Python packages.

The required Python packages and their version numbers are listed in `requirements.txt`. See the [requirements.txt](#requirementstxt) section for more information.

### R setup

The R tutorials were developed under R 4.3.3, which can be downloaded from [CRAN](https://cran.r-project.org/).

If using a Windows machine, [Rtools](https://cran.r-project.org/bin/windows/Rtools/) needs to be installed manually from CRAN before installing packages, since one of the required packages relies on Rtools to be built.

The required R packages and their version numbers are listed in `renv.lock`. See the [renv.lock, .Rprofile, renv/activate.R, and renv/settings.json](#renvlock-rprofile-renvactivater-and-renvsettingsjson) section for more information.

#### Census API Key

A Census API key is a hash that authenticates users to download data using the Census API. Users can request a Census API key here: https://api.census.gov/data/key_signup.html.

See the [tutorial_wru.r](#tutorial_wrur) section for more details.

## Descriptions of files

The main scripts to run are  `tutorial_ethnicolr.py` and `tutorial_surgeo.py` in Python, and `tutorial_birdie.r` and `tutorial_wru.r` in R. All four tutorial scripts are independent of each other, but do depend on having the necessary packages installed, as well as importing the metrics scripts. The remainder of this section describes the purpose and usage of each file.

### Setup

#### requirements.txt

This file contains a list of required packages (including version numbers) for the Python tutorials and metrics. Note that the code was developed under Python 3.11, the newest available version at the time we began work on this tutorial. Thus, the packages in the requirements file are compatible with Python 3.11 but may not be compatible with future releases of Python.

Package installers such as pip can install the correct packages by reading the requirements file, for example by running
```
pip install -r requirements.txt
```

Alternatively, the user can install packages manually using pip, conda, or another package manager, for example by running
```
# using pip
pip install pandas==2.1.1
# using conda
conda install pandas==2.1.1
# etc
```

#### renv.lock, .Rprofile, renv/activate.R, and renv/settings.json

Like the `requirements.txt` file for Python, the `renv.lock` file contains required packages (including version numbers) for the R tutorials and metrics. Note that the code was developed under R 4.3.3, the newest available version at the time we began work on this tutorial. Thus, the packages in the requirements file are compatible with R 4.3.3, but may not be compatible with future releases of R.

The `.Rprofile` script runs `renv/activate.R` to activate a `renv` environment, which will allow R to install the correct packages from the `renv.lock` file. If `.Rprofile` is in the working directory, it should run itself automatically. If this does not happen, `renv/activate.R` can also be run by hand. `renv/settings.json` contains additional settings for the `renv` environment and does not need to be run by hand.

Once the `renv` environment is activated, the required packages can be installed by running `renv::restore()`; this command is included at the top of each R tutorial script.

Alternatively to relying on `renv`, the user can install packages manually using the function `devtools::install_version()`. This function works for packages on both CRAN and GitHub. Only the packages explicitly referenced in the `library` calls of the R scripts would need to be installed manually, since they install their own dependencies. Specifically, the user could run the following code to install the required packages if `renv` is not working correctly:

```
install.packages("devtools")
library(devtools)

# packages from CRAN
devtools::install_version("tidyverse", version = "2.0.0")  # includes dplyr, ggplot2, etc
devtools::install_version("wru", version = "3.0.1")
devtools::install_version("pROC", version = "1.18.5")
devtools::install_version("MLmetrics", version = "1.1.1")

# packages from GitHub
devtools::install_version("CoryMcCartan/birdie@0b37f4dc27b89cbcd53f0ebd000f2ff5")
```

Note that on Windows machines, [Rtools](https://cran.r-project.org/bin/windows/Rtools/) needs to be installed manually from CRAN first, since one of the required packages relies on Rtools to be built.

### Metrics

These scripts define functions that can be used to assess the performance of each method. They are imported into the tutorial scripts, and do not need to be run separately.

#### metrics.py

Python script to define metrics functions. The main function is `run_metrics`, which produces the following outputs:
- AUC (area under the ROC curve)
- Average probability of predicted self-reported cohort, by self-reported cohort
- Average probability of predicted white cohort, by self-reported cohort
- Precision
- Recall
- Specificity
- Positive and negative likelihood ratios
- Calibration curves
- Histograms of predicted probabilities, by self-reported cohort

#### metrics.r

R script to define metrics functions. The main function is `run_metrics`, which produces the same list of outputs as the Python version, excluding calibration curves.

### Tutorials

These are the main scripts to run and reference to learn how to use the race imputation methods described in the paper. Note that file paths for inputs and outputs may need to be changed to refer to the correct location on your computer. The relevant lines of code are called out with comments beginning with "TODO".

#### tutorial_ethnicolr.py

`tutorial_ethnicolr.py` demonstrates using two pre-trained long short-term memory models from the `ethnicolr` package. One model uses full names, while the other uses just surnames. Both models were trained on Florida voter registration data. The tutorial also produces metrics for each model.

The `ethnicolr` documentation can be found here: https://pypi.org/project/ethnicolr/

#### tutorial_surgeo.py

`tutorial_surgeo.py` demonstrates Bayesian Improved Surname Geocoding (BISG) and Bayesian Improved First Name Surname Geocoding (BIFSG) using the `surgeo` package. It also produces metrics for each model.

The `surgeo` documentation can be found here: https://github.com/theonaunheim/surgeo

#### tutorial_birdie.r and birdie_calibration_curves.py

`tutorial_birdie.r` demonstrates BIRDiE using the `birdie` package, with Surname Analysis (SA), Bayesian Improved Surname Geocoding (BISG), and Fully Bayesian Improved Surname Geocoding (fBISG) as input probabilities. It also produces metrics, excluding calibration curves, for each BIRDiE model.

We created calibration curves in Python using the function `sklearn.calibration.calibration_curve`, for which we could not find a good equivalent in R. `birdie_calibration_curves.py` runs the Python metrics, including calibration curves, on the BIRDiE model predictions which were run in R.

The `birdie` documentation can be found here: https://corymccartan.com/birdie/


#### tutorial_wru.r

`tutorial_wru.r` demonstrates Bayesian Improved Surname Geocoding (BISG) and Fully Bayesian Improved Surname Geocoding (fBISG) using the `wru` package. It also produces metrics, excluding calibration curves, for each method.

Note that several of the functions in the `wru` package require a Census API key to download data using the Census API. Lines 14-17 of `tutorial_wru.r` show how to use the `.Renviron` file to set up the tutorial script to use a Census API key, without including the key in the tutorial script. This is useful because it allows multiple users to share the script used for an analysis without sharing their keys, which should be kept secret. The line `usethis::edit_r_environ()` should pull up the `.Renviron` file for the user to edit; the user should add the following line to the file (not including the <> brackets):
```
CENSUS_API_KEY=<your key here>
```
It might be necessary to restart R after editing the `.Renviron` file for the changes to take effect.

The line `Sys.getenv("CENSUS_API_KEY")` simply prints the key saved in the `.Renviron` file for the user's information. By default, `wru` functions requiring a Census API key run the same line of code under the hood to retrieve the key.

The `wru` documentation can be found here: https://github.com/kosukeimai/wru
