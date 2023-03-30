library(targets)
#library(tarchetypes)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "bookdown", "omxr", "nhts2017", "rgdal", "sf", "ggthemes", "lhs", "foreign", "imputeTS"))

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/functions.R")


# Targets necessary to build your data and run your model
data_targets <- list(
  #read in Skims
  tar_target(skims_file, "data/rvtpo_skims.omx", format = "file"),
  tar_target(skims, readskims(skims_file)),
  
  #read in Land Use
  tar_target(landuse_file, "data/se.csv", format = "file"),
  tar_target(land_use, readCSV(landuse_file)),
  
  #HH productions
  tar_target(productions_file, "data/HH_PROD.csv", format = "file"),
  tar_target(productions, readCSV(productions_file)),
  
  # purposes
  tar_target(HBW, purpose <- "HBW"),
  tar_target(HBO, purpose <- "HBO"),
  tar_target(NHB, purpose <- "NHB"),
  
  # productions by purpose
  tar_target(hbw_productions, productions_purpose(productions, HBW)),
  tar_target(hbo_productions, productions_purpose(productions, HBO)),
  tar_target(nhb_productions, productions_purpose(productions, NHB)),
  
  # read in mc / dc utilities
  tar_target(mc_coeff_file, "data/MC_coeff.csv", format = "file"),
  tar_target(mc_const_file, "data/MC_constants.csv", format = "file"),
  tar_target(dc_coeff_file, "data/DESTCHOICE_PARAMETERS.DBF", format = "file"),
  tar_target(mc_coeff, readCSV(mc_coeff_file)),
  tar_target(mc_const, readCSV(mc_const_file)),
  tar_target(dc_coeff, as_tibble(foreign::read.dbf(dc_coeff_file, as.is = TRUE))),
  
  #generate MC coeffs
  tar_target(hbw_mc_coeff_lists_100, generate_mc_coeff(HBW, mc_coeff, mc_const, 100)),
  tar_target(hbo_mc_coeff_lists_100, generate_mc_coeff(HBO, mc_coeff, mc_const, 100)), 
  tar_target(nhb_mc_coeff_lists_100, generate_mc_coeff(NHB, mc_coeff, mc_const, 100)),
  
  tar_target(hbw_mc_coeff_lists_600, generate_mc_coeff(HBW, mc_coeff, mc_const, 600)),
  tar_target(hbo_mc_coeff_lists_600, generate_mc_coeff(HBO, mc_coeff, mc_const, 600)), 
  tar_target(nhb_mc_coeff_lists_600, generate_mc_coeff(NHB, mc_coeff, mc_const, 600)),
  
  # DC coeff
  tar_target(hbw_dc_coeff_lists_100, generate_dc_coeff(HBW, dc_coeff, 100)),
  tar_target(hbo_dc_coeff_lists_100, generate_dc_coeff(HBO, dc_coeff, 100)), 
  tar_target(nhb_dc_coeff_lists_100, generate_dc_coeff(NHB, dc_coeff, 100)),
  
  tar_target(hbw_dc_coeff_lists_600, generate_dc_coeff(HBW, dc_coeff, 600)),
  tar_target(hbo_dc_coeff_lists_600, generate_dc_coeff(HBO, dc_coeff, 600)), 
  tar_target(nhb_dc_coeff_lists_600, generate_dc_coeff(NHB, dc_coeff, 600)),
  
  #write out CSVs
#  tar_target(write_mc_lists, mcparam_csv(hbw_mc_coeff_lists_100, hbo_mc_coeff_lists_100, nhb_mc_coeff_lists_100, mc_coeff)),
#  tar_target(write_dc_lists, dcparam_csv(hbw_dc_coeff_lists_100, hbo_dc_coeff_lists_100, nhb_dc_coeff_lists_100, dc_coeff)),
  
  # run loop for logsums
  tar_target(hbw_full_loop_100, full_loop(skims, hbw_mc_coeff_lists_100, hbw_dc_coeff_lists_100, land_use, 100)),
  tar_target(hbo_full_loop_100, full_loop(skims, hbo_mc_coeff_lists_100, hbo_dc_coeff_lists_100, land_use, 100)),
  tar_target(nhb_full_loop_100, full_loop(skims, nhb_mc_coeff_lists_100, nhb_dc_coeff_lists_100, land_use, 100)),
  
  tar_target(hbw_full_loop_600, full_loop(skims, hbw_mc_coeff_lists_600, hbw_dc_coeff_lists_600, land_use, 600)),
  tar_target(hbo_full_loop_600, full_loop(skims, hbo_mc_coeff_lists_600, hbo_dc_coeff_lists_600, land_use, 600)),
  tar_target(nhb_full_loop_600, full_loop(skims, nhb_mc_coeff_lists_600, nhb_dc_coeff_lists_600, land_use, 600)),

  # pull tibbles
  tar_target(hbw_mclogsum_tibble,      pull_tibbles(hbw_full_loop_100, "ModeChoice_Logsum")),
  tar_target(hbw_dcutility_tibble,     pull_tibbles(hbw_full_loop_100, "Destination_Utility")),
  tar_target(hbw_dclogsum_tibble,      pull_tibbles(hbw_full_loop_100, "Destination_Logsum")),
  tar_target(hbw_mcprobability_tibble, pull_tibbles(hbw_full_loop_100, "ModeChoice_Probability")),
  tar_target(hbw_dcprobability_tibble, pull_tibbles(hbw_full_loop_100, "Destination_Probability")),
  
  tar_target(hbo_mclogsum_tibble,      pull_tibbles(hbo_full_loop_100, "ModeChoice_Logsum")),
  tar_target(hbo_dcutility_tibble,     pull_tibbles(hbo_full_loop_100, "Destination_Utility")),
  tar_target(hbo_dclogsum_tibble,      pull_tibbles(hbo_full_loop_100, "Destination_Logsum")),
  tar_target(hbo_mcprobability_tibble, pull_tibbles(hbo_full_loop_100, "ModeChoice_Probability")),
  tar_target(hbo_dcprobability_tibble, pull_tibbles(hbo_full_loop_100, "Destination_Probability")),

  tar_target(nhb_mclogsum_tibble,      pull_tibbles(nhb_full_loop_100, "ModeChoice_Logsum")),
  tar_target(nhb_dcutility_tibble,     pull_tibbles(nhb_full_loop_100, "Destination_Utility")),
  tar_target(nhb_dclogsum_tibble,      pull_tibbles(nhb_full_loop_100, "Destination_Logsum")),
  tar_target(nhb_mcprobability_tibble, pull_tibbles(nhb_full_loop_100, "ModeChoice_Probability")),
  tar_target(nhb_dcprobability_tibble, pull_tibbles(nhb_full_loop_100, "Destination_Probability")),

  tar_target(hbw_mclogsum_tibble_600,      pull_tibbles(hbw_full_loop_600, "ModeChoice_Logsum")),
  tar_target(hbo_mclogsum_tibble_600,      pull_tibbles(hbo_full_loop_600, "ModeChoice_Logsum")),
  tar_target(nhb_mclogsum_tibble_600,      pull_tibbles(nhb_full_loop_600, "ModeChoice_Logsum")),

  tar_target(hbw_stats_100, process_stats(hbw_mclogsum_tibble)),
  tar_target(hbo_stats_100, process_stats(hbo_mclogsum_tibble)),
  tar_target(nhb_stats_100, process_stats(nhb_mclogsum_tibble)),

  tar_target(hbw_stats_600, process_stats(hbw_mclogsum_tibble_600)),
  tar_target(hbo_stats_600, process_stats(hbo_mclogsum_tibble_600)),
  tar_target(nhb_stats_600, process_stats(nhb_mclogsum_tibble_600)),
  
  #trips
  tar_target(hbw_trips, total_trips(hbw_productions, hbw_dcprobability_tibble, hbw_mcprobability_tibble)),
  tar_target(hbo_trips, total_trips(hbo_productions, hbo_dcprobability_tibble, hbo_mcprobability_tibble)),
  tar_target(nhb_trips, total_trips(nhb_productions, nhb_dcprobability_tibble, nhb_mcprobability_tibble)),

  #write trips to omx
#  tar_target(omx_trips, omx_write(hbw_trips, hbo_trips, nhb_trips)),

  # plots by purpose
  tar_target(hbw_plots, tibbleplots(hbw_mclogsum_tibble, hbw_dcutility_tibble, hbw_dclogsum_tibble, hbw_mcprobability_tibble, hbw_dcprobability_tibble)),
  tar_target(hbo_plots, tibbleplots(hbo_mclogsum_tibble, hbo_dcutility_tibble, hbo_dclogsum_tibble, hbo_mcprobability_tibble, hbo_dcprobability_tibble)),
  tar_target(nhb_plots, tibbleplots(nhb_mclogsum_tibble, nhb_dcutility_tibble, nhb_dclogsum_tibble, nhb_mcprobability_tibble, nhb_dcprobability_tibble)),

  #network data
  tar_target(networks, network_data("data/sensitivity_out")),



  tar_target(dummy, 2+2)
  
  )


# Targets necessary to build the book / article
book_targets <- list(

)



# run all targets
list(
  data_targets, 
  book_targets
)
