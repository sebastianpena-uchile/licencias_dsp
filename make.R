#' licencias_dsp: A Research Compendium
#' 
#' @description 
#' A paragraph providing a full description of the project and describing each 
#' step of the workflow.
#' 
#' @author Sebastian Pena-Frias \email{sebastianpena@uchile.cl}
#' 
#' @date 2025/08/07



## Install Dependencies (listed in DESCRIPTION) ----

devtools::install_deps(upgrade = "never")


## Load Project Addins (R Functions and Packages) ----

devtools::load_all(here::here())


## Global Variables ----

# You can list global variables here (or in a separate R script)


## Run Project ----

# List all R scripts in a sequential order and using the following form:
source(here::here("R", "1.extraer_data.R"))
source(here::here("R", "2.prepare_data.R"))
source(here::here("R", "3.crear_variables.R"))
source(here::here("R", "4.crear_db.R"))
