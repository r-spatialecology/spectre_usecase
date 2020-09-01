# spectre_usecase

Use case and analysis repository of the [spectre package](https://github.com/r-spatialecology/spectre).
The main idea of this repository is to accompany the spectre R package repository with detailed analyses and additional utility functions that would not fit in the context of the spectre package itself.

## Contents

Here is a quick overview of the folder structure of this repository

* R
    * *R script files containing functions only (.R)*
    * *Please only put functions here that can be sourced without throwing errors*
    * *Sourcing in Rmd files can be done with:* `sapply(list.files("../R", full.names = TRUE), source)`
    * `plot_virtualspecies.R`
        * utility function for plotting virtualspecies raster stacks in different formats
* Rmd
    * *R Markdown scripts containing analyses, drafts, ...*
    * `Virtualspecies_01_simple.Rmd`
        * Simple example on how to generate virtual species data and run that through the spectre package
    * `virtualspecies_02_advanced.Rmd`
        * Advanced example on how to run more complex experiments with virtual species and execution on the GWDG HPC
* data
    * *Place to store survey data, generated data, or processed data (.rds)*
    * `virtualspecies_02_advanced_data.rds`
        * results of the `virtualspecies_02_advanced.Rmd` example, executed on the GWDG HPC
* figures
    * *Place to store all kind of figures, plots, etc. (.png, .svg, ...)*
    * `virtualspecies_02_advanced_01.png, virtualspecies_02_advanced_02.png, virtualspecies_02_advanced_03.png`
        * Analyses result plots of the `virtualspecies_02_advanced.Rmd` example
        

## Analyses

This section gives you a short overview of the analyses that are currently part of this repository

#### Rmd/Virtualspecies_01_simple.html

The [virtualspecies package](https://github.com/Farewe/virtualspecies) allows to generate species distributions from habitat suitability maps. By generating these maps through the [NLMR package](https://github.com/ropensci/NLMR), we keep control over spatial autocorrelation which allows us to test the efficiency and accuracy of spectre under various different settings.

The first virtualspecies example shows how to use the virtualspecies functions of spectre.


#### Rmd/Virtualspecies_02_advanced.html

The second, more detailed virtualspecies example not only runs one parameterization, but shows how to setup complete experiments with different parameterizations, and how to distribute these calculations to the GWDG HPC.






