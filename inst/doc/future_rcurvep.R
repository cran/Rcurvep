## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning=FALSE, message=FALSE--------------------------------------
library(future)
library(furrr)
library(dplyr)
library(purrr)
#library(microbenchmark) # it is needed if recalculating the comparison
library(Rcurvep)

## -----------------------------------------------------------------------------
data("zfishdev_all") # two endpoints, each endpoint includes 32 chemicals/curves
data("zfishdev_act") # simulated curves based on zfishdev_all, each chemical has 100 simulated curves
data("zfishdev") # four endpoints, each endpoint includes 3 chemicals/curves

## -----------------------------------------------------------------------------

# sequential run
seq_run_multi <- expression(
  
  # set up the plan
  future::plan(sequential),
  
  # calculation
  combi_run_rcurvep(
    zfishdev_all, 
    n_sample = 10,
    TRSH = seq(5, 95, by = 5),
    RNGE = 1000000,
    keep_sets = "act_set",
    seed = 300
  )
)


# parallel run
par_run_multi <- expression(
  
  # set up the plan
  future::plan(multisession, workers = 10),
  
  # calculation
  combi_run_rcurvep(
    zfishdev_all, 
    n_sample = 10,
    TRSH = seq(5, 95, by = 5),
    RNGE = 1000000,
    keep_sets = "act_set",
    seed = 300
  ),
  
  # re-set the plan back
  future::plan(sequential)
)


## ----eval=FALSE---------------------------------------------------------------
#  
#  run_speed_multi_rcurvep <- microbenchmark(
#    eval(seq_run_multi),
#    eval(par_run_multi),
#    times = 10
#  )

## -----------------------------------------------------------------------------
#> run_speed_multi_rcurvep 
#Unit: seconds
#                expr      min       lq     mean   median       uq      max neval cld
# eval(seq_run_multi) 70.97373 71.06332 73.85309 74.02031 75.58980 77.31413    10  a 
# eval(par_run_multi) 22.73920 23.86592 24.80765 25.09510 25.36901 27.39045    10   b

## -----------------------------------------------------------------------------
bmr_out <- estimate_dataset_bmr(zfishdev_act, plot = FALSE)
bmrd <- bmr_out$outcome

## -----------------------------------------------------------------------------
inp_tb <- bmrd |>
  nest_join(
    zfishdev_all, by = c("endpoint"), keep = TRUE, name = "data"
  ) |>
  select(RNGE, endpoint, bmr_exp, data)
# input_data for combi_run_rcurvep
rmarkdown::paged_table(inp_tb)

## -----------------------------------------------------------------------------
# sequential run
seq_run_bmr <- expression(
  future::plan(sequential),
  pmap(inp_tb, ~ combi_run_rcurvep(..4, TRSH = ..3, RNGE = ..1, n_samples = 100, seed = 300, keep_sets = c("act_set")))
)

# parallel run
par_run_bmr1 <- expression(
  
  future::plan(multisession, workers = 10),
  
  # calculation with no additional future_pmap
  pmap(inp_tb, ~ combi_run_rcurvep(..4, TRSH = ..3, RNGE = ..1, n_samples = 100, seed = 300, keep_sets = c("act_set"))),

  future::plan(sequential)
)


# parallel run
par_run_bmr2 <- expression(
  
  future::plan(multisession, workers = 10),
  
  # calculation with additional future_pmap
  future_pmap(inp_tb, ~ combi_run_rcurvep(..4, TRSH = ..3, RNGE = ..1, n_samples = 100, seed = 300, keep_sets = c("act_set")), .options = furrr_options(seed = 300)),

  future::plan(sequential)
)


## ----eval=FALSE---------------------------------------------------------------
#  run_speed_bmr_rcurvep <- microbenchmark(
#    eval(seq_run_bmr),
#    eval(par_run_bmr1),
#    eval(par_run_bmr2),
#    times = 10
#  )

## -----------------------------------------------------------------------------

#> run_speed_bmr_rcurvep
#Unit: seconds
#               expr      min       lq     mean   median       uq      max neval cld
#  eval(seq_run_bmr) 39.29451 40.34472 42.08456 40.92504 44.08229 47.01601    10 a  
# eval(par_run_bmr1) 17.66869 18.38648 18.89080 18.80669 19.20098 20.97384    10  b 
# eval(par_run_bmr2) 24.38823 25.35811 25.46403 25.48099 25.83589 26.54293    10   c

## -----------------------------------------------------------------------------

# sequential run
seq_fit_hill_boot <- expression(
  
  future::plan(sequential),
  run_fit(create_dataset(zfishdev), hill_pdir = 1, n_samples = 100, modls = "hill")

)

# parallel run
seq_fit_hill_boot <- expression(
  
  future::plan(multisession, workers = 10),
  
   # calculation
  run_fit(create_dataset(zfishdev), hill_pdir = 1, n_samples = 100, modls = "hill"),
  future::plan(sequential)
)

## ----eval=FALSE---------------------------------------------------------------
#  run_speed_fit_hill_boot <- microbenchmark(
#    eval(seq_fit_hill_boot),
#    eval(par_fit_hill_boot),
#    times = 10
#  )

## -----------------------------------------------------------------------------
#> run_speed_fit_hill_boot
#Unit: seconds
#               expr      min       lq     mean   median       uq      max neval
# eval(seq_fit_hill_boot) 60.75111 63.42611 64.02762 63.97692 65.46656 66.83198    10
# eval(par_fit_hill_boot) 34.81743 36.88777 37.43076 37.41199 38.88405 39.40711    10

## -----------------------------------------------------------------------------

# make the incidence data as response data
inp_tb_resp <- inp_tb |> mutate(data = map(data, create_dataset))

# sequential run
seq_fit_hill_multi <- expression(
  future::plan(sequential),
  set.seed(2003),
  pmap(inp_tb_resp, ~ run_fit(..4, modls = "hill", hill_pdir = ifelse(..3 < 0, -1, 1), n_samples = 100, keep_sets = c("fit_set")))
)

# parallel run
para_fit_hill_multi1 <- expression(
  future::plan(multisession, workers = 10), 
  set.seed(2003),
  # no future_pmap
  pmap(inp_tb_resp, ~ run_fit(..4, modls = "hill", hill_pdir = ifelse(..3 < 0, -1, 1), n_samples = 100, keep_sets = c("fit_set"))),
  
  future::plan(sequential)
)
  
# parallel run
para_fit_hill_multi2 <- expression(
  future::plan(multisession, workers = 10), 
  
  # with future_pmap
  future_pmap(inp_tb_resp, ~ run_fit(..4, modls = "hill", hill_pdir = ifelse(..3 < 0, -1, 1), n_samples = 100, keep_sets = c("fit_set")), .options = furrr_options(seed = 2023)),
  
  future::plan(sequential)
)


## ----eval=FALSE---------------------------------------------------------------
#  run_speed_fit_hill_multi <- microbenchmark(
#    eval(seq_fit_hill_multi),
#    eval(para_fit_hill_multi1),
#    eval(para_fit_hill_multi2),
#    times = 10
#  )

## -----------------------------------------------------------------------------

#> run_speed_fit_hill_multi
#Unit: seconds
#                       expr       min        lq      mean    median        uq       max neval cld
# eval(seq_fit_hill_multi) 210.70736 211.45879 214.57743 212.88469 217.75672 222.96850    10 a  
# eval(para_fit_hill_multi1)  95.43516  95.98258  96.56978  96.65905  97.00355  97.55148    10  b 
# eval(para_fit_hill_multi2) 121.67944 122.25914 122.66318 122.80333 123.16349 123.24216    10   c

## ----eval=FALSE---------------------------------------------------------------
#  
#  # sequential run
#  seq_fit_hill_ori <- expression(
#  
#    future::plan(sequential),
#    run_fit(respd_1,  modls = "hill")
#  
#  )
#  
#  # parallel run
#  par_fit_hill_ori <- expression(
#  
#    future::plan(multisession, workers = 5),
#    run_fit(respd_1,  modls = "hill"),
#    future::plan(sequential)
#  )

## ----eval=FALSE---------------------------------------------------------------
#  run_speed_hit_hill_ori <- microbenchmark(
#    eval(seq_fit_hill_ori),
#    eval(par_fit_hill_ori),
#    times = 5
#  )

## -----------------------------------------------------------------------------
#> run_speed_hit_hill_ori
#Unit: seconds
#                   expr       min        lq      mean    median       uq       max neval
# eval(seq_fit_hill_ori) 112.12160 112.30511 112.35622 112.40213 112.4308 112.52144     5
# eval(par_fit_hill_ori)  62.92156  63.04108  63.51158  63.60182  63.9130  64.08043     5

## ----eval=FALSE---------------------------------------------------------------
#  
#  # sequential run
#  seq_fit_cc2 <- expression(
#  
#    future::plan(sequential),
#    run_fit(respd_1,  modls = "cc2")
#  
#  )
#  
#  # parallel run
#  par_fit_cc2 <- expression(
#  
#    future::plan(multisession, workers = 5),
#    run_fit(respd_1,  modls = "cc2"),
#    future::plan(sequential)
#  )

## ----eval=FALSE---------------------------------------------------------------
#  run_speed_fit_cc2 <- microbenchmark(
#    eval(seq_fit_cc2),
#    eval(par_fit_cc2),
#    times = 5
#  )

## -----------------------------------------------------------------------------
#> run_speed_fit_cc2
#Unit: seconds
#              expr      min       lq     mean   median       uq      max neval
# eval(seq_fit_cc2) 68.37777 68.39599 68.46936 68.45011 68.45746 68.66547     5
# eval(par_fit_cc2) 58.07689 58.31388 59.17766 59.01783 59.07421 61.40546     5

## ----include=FALSE, eval=FALSE------------------------------------------------
#  res <- ls() |> str_match("run_speed.*") |> na.omit()
#  l1 <- map(res, get) |> set_names(res)
#  saveRDS(l1, here("data-raw", "future_rcurvep_output.rds"))

