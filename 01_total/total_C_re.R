
library("tidyverse")
library("foreach")
library("doParallel")
library("nnet")
library("SuperLearner")

options("scipen" = 100, "digits" = 4)

# start timer
start.time <- Sys.time()

# load functions
source("0_functions.R")

# load data
load("data_final.Rdata")

# select sample
data_final <- data_final %>% filter(year %in% c(1991:2020))

# define outcome
data_final$outcome <- ifelse(data_final$employment_status_next_year %in% c("Retired"), 1, 0)

# define treatment model
formula_treatment <- formula("eseg_class ~ age + as.numeric(year) + region + urban_residence + education_level + disability + marital_status + hh_size + n_children + foreign_born + lm_expr_ft + lm_expr_pt")

# select variables for outcome model
preds_outcome <- c("eseg_class", "year", "age", "urban_residence", "region", "education_level", "disability", "marital_status", "hh_size", "n_children", "foreign_born", "lm_expr_ft", "lm_expr_pt")

# select models
sl_models <- c("SL.glm", "SL.ranger", "SL.xgboost")

# set number of bootstrap iterations
bs_iterations <- 1000

# clear memory
gc()

# set seed
set.seed(999)

# set up and start parallel processing
cluster <- makeCluster(30)

print(paste("Cores:", detectCores()))

# start parallel processing
registerDoParallel(cluster)

# run decomposition iterations in parallel
outputs <- foreach(i = 1:bs_iterations, .combine = "rbind", .packages = c("tidyverse", "nnet", "SuperLearner")) %dopar% {
  
  # randomly draw individuals
  ids <- data_final %>% select(orgpid) %>% unique() %>% ungroup() %>% slice_sample(prop = 1, replace = T) %>% ungroup()
  
  # draw all observations from individuals (block bootstrapping)
  data <- ids %>% left_join(data_final, by = "orgpid", relationship = "many-to-many") %>% ungroup()
  
  # run decomposition and store results
  estimates <- decompose_dr_xfit_intC(data, formula_treatment, preds_outcome, sl_models)
  
  # save number of bootstrap iteration
  estimates$bs_iteration <- i
  
  # return bootstrap results
  return(estimates)
  
}

# close parallel processing
stopCluster(cluster)

# check results
get_results(outputs)

# compile results
results <- get_results(outputs)
results$intervention <- "C"
results$outcome <- "Retired"
results$group <- "Total population"

# end timer
end.time <- Sys.time()

# save results
save.image("/home/sozialwiss/einhofja/Dokumente/analysis_decomp/output_total_c_re.RData")
