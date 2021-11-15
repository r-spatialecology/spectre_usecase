  <!-- badges: start -->
  [![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/r-spatialecology/spectre_usecase/master?urlpath=rstudio)
  <!-- badges: end -->

# spectre_usecase

Use case and analysis repository of the [`spectre` package](https://github.com/r-spatialecology/spectre).
The main idea of this repository is to accompany the `spectre` R package repository with detailed analyses and additional utility functions that would not fit in the context of the `spectre` package itself. This repository also contains all data and analysis used in the `spectre` manuscript.

## Contents

```bash
./
├── data # all data calculated during the analysis
│   ├── BCI_tree8_MSP.rds
│   ├── Fig4_data
│   ├── virtualspecies_02_advanced_data.rds
│   ├── virtualspecies_04_complete_versionB.rds
│   ├── virtualspecies_benchmark_parallel.rds
│   └── virtualspecies_benchmark.rds
├── DESCRIPTION # meta data about this analysis
├── figures
│   ├── Fig2.png
│   ├── Fig3.png
│   ├── Fig4.png
│   ├── S-Fig1.png
│   └── virtualspecies_complete_versionB
│       ├── mae.png
│       ├── mc.png
│       ├── min_error.png
│       ├── species_richness_map_10.png
│       ├── species_richness_map_20.png
│       └── species_richness_map_30.png
├── LICENSE.md
├── man
│   ├── calculate_mae.Rd
│   ├── calculate_mc.Rd
│   ├── calculate_rce.Rd
│   ├── generate_data_virtualspecies.Rd
│   ├── generate_data_virtualspecies_to_solution.Rd
│   ├── virtualspecies_plot.Rd
│   └── virtualspecies_simfun.Rd
├── NAMESPACE
├── R # all R functions used in the analysis
│   ├── calculate_mae.R
│   ├── calculate_mc.R
│   ├── calculate_rce.R
│   ├── generate_data_virtualspecies.R
│   ├── generate_data_virtualspecies_to_solution.R
│   ├── virtualspecies_plot.R
│   └── virtualspecies_simfun.R
├── raw_data # read-only data used in the analysis
│   ├── BCI_prepare_tree8_data.R
│   └── bci.tree8.rdata
├── README.md # this file
├── Rscripts # all Rscripts used for this analysis
│   ├── Fig2_plot_error.R
│   ├── Fig3_virt_species_scaling_run.R
│   ├── Fig4_BCI_known_sites_plots.R
│   └── Fig4_BCI_known_sites_run.R
└── spectre_usecase.Rproj
```

## Analyses

This analysis is packaged into an R-package. To use thr analysis either just click [![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/r-spatialecology/spectre_usecase/master?urlpath=rstudio) or 

```R
...
```