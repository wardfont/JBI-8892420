#  Revealing uncertainty from past range shifts in species distribution modelling: Code and Data

This repository contains the necessary data and code to reproduce the results presented in the research article. The Worldclim data used in the analysis can be downloaded using the provided code. The eBird data should be seperately downloaded (https://science.ebird.org/en/use-ebird-data/download-ebird-data-products). The code contains a reproducible workflow based on the `targets` R package. More info on the package and its syntax can be found [here](https://books.ropensci.org/targets/).

The `targets` workflow assumes the following file structure and might throw errors if some of these locations do not exist on your system:

```bash
.
├── data
│   ├── ebird
│   │   ├── ebd
│   │   ├── readme.txt
│   │   └── sed
│   └── worldclim
├── LICENSE
├── output
│   ├── bioclim
│   ├── ebird
│   ├── future
│   ├── hist
│   ├── random_sp
│   ├── temp
│   └── worldclim
├── plot
├── R
│   ├── balance.R
│   ├── calc_bioclim.R
│   ├── case-geo-plot.R
│   ├── centroid_vs_range.R
│   ├── check_gams.R
│   ├── extract_gbif.R
│   ├── get_ebird.R
│   ├── get_info_scen.R
│   ├── get_scen_presence_absence.R
│   ├── packages.R
│   ├── plots.R
│   ├── sp_to_pa.R
│   └── worldclim_monthly.R
├── README.md
├── _targets
└── _targets.R
```

The workflow can be visualised by running the following R code in the root directory:

```R
library(targets)
tar_visnetwork()
```

and can be run using the following R code:

```R
library(targets)
tar_make()
```

Running the complete workflow will take considerable time and requires at least 30GB of free disk space.

All analysis is contained within the `_targets.R` file and the function in the the R-files in the `./R` folder.
