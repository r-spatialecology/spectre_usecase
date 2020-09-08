library(spectre)
library(tidyverse)
library(future)
library(virtualspecies)
library(furrr)
library(clustermq)
library(landscapetools)

## Source spectre analysis functions (R folder)
sapply(list.files("../R", full.names = TRUE), source)
set.seed(3562347)
exec <- "HPC" # "HPC"
dir.hpc <- file.path("/home/uni08/jsaleck/spectre")
dir.cloud <- file.path("/home/jan/netlogo_jan/spectre_usecase")

## Constant parameters:
replicates <- 1
max_runs <- 100
energy_threshold <- 0
beta <- 0.75

## Variable parameters:
landscape_size <- c(10,25,50,75,100)
corr_within <- 0.5
corr_among <- 0.1
gamma <- c(100,200,300,400)
random_seeds <- round(runif(replicates) * 100000)

## Generate parameter matrix:
parameters <- expand.grid(landscape_size = landscape_size, 
                          corr_within = corr_within,
                          gamma = gamma, 
                          beta = beta,
                          corr_among = corr_among,
                          random_seeds = random_seeds)

if (exec == "local") {
  plan(multisession)
  results <- furrr::future_map_dfr(seq(nrow(parameters)), function(x) {
    virtualspecies_simfun(siminputrow = x, 
                          parameters = parameters,
                          max_runs = max_runs,
                          energy_threshold = energy_threshold,
                          writeRDS = FALSE,
                          outdir = dir.cloud)
  })
}
if (exec == "HPC") {
  
  maxjobs.hpc <- 2000
  njobs <- min(nrow(parameters), maxjobs.hpc)
  jobIDs <- seq(nrow(parameters))
  
  results <- clustermq::Q(fun = virtualspecies_simfun, 
                          siminputrow = jobIDs,
                          const = list(parameters = parameters,
                                       max_runs = max_runs,
                                       energy_threshold = energy_threshold,
                                       writeRDS = TRUE,
                                       outdir = dir.hpc),
                          export = list(), 
                          seed = 42, 
                          n_jobs = njobs, 
                          template = list(job_name = "spectre", # define jobname
                                          log_file = "spectre.log", # define logfile name
                                          queue = "medium",  # define HPC queue
                                          service = "normal", # define HPC service
                                          walltime = "48:00:00", # define walltime
                                          mem_cpu = "4000")) # define memory per cpu   
  results <- dplyr::bind_rows(results)
}

#### Restore results from rds files if neccessary:
#results <- purrr::map_dfr(list.files(dir.cloud, pattern = "rds", full.names = TRUE), function(x) {
#  res.x <- readRDS(x)
#  return(res.x)
#})

#### STORE RESULTS:
saveRDS(results, file=file.path(dir.cloud, "data", "virtualspecies_benchmark.rds"))


#### Examine results:
#
#
results <- readRDS("data/virtualspecies_benchmark.rds")

## Projected time for 50000 iterations:

bench<- results %>% 
  dplyr::select(gamma, landscape_size, benchmark) %>% 
  dplyr::mutate(benchmark_proj_50k = ((((benchmark / 100) * 50000) / 60) / 60))



### Time:
ggplot(bench, aes(x=gamma, y=benchmark_proj_50k, color=factor(landscape_size), group=landscape_size)) +
  geom_line(size=1) +
  geom_point(size=2) +
  #geom_bar(stat="identity", position="dodge") +
  xlab("Gamma") +
  ylab("Time per 50k iterations [hrs]") +
  #guides(color=guide_legend(title="Landscape size")) +
  ggsci::scale_color_jco() +
  ggthemes::theme_tufte(base_size = 12)
