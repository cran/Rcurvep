## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning=FALSE, message=FALSE--------------------------------------
library(future)
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
#                expr      min       lq     mean   median       uq      max neval
# eval(seq_run_multi) 61.06341 61.12504 61.79744 61.43494 62.17374 63.99470    10
# eval(par_run_multi) 18.87097 19.36093 19.74137 19.50655 20.42096 20.97378    10

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
par_run_bmr <- expression(
  
  future::plan(multisession, workers = 10),
  
  # calculation
  # there is no need to use future_pmap here 
  pmap(inp_tb, ~ combi_run_rcurvep(..4, TRSH = ..3, RNGE = ..1, n_samples = 100, seed = 300, keep_sets = c("act_set"))),

  future::plan(sequential)
)



## ----eval=FALSE---------------------------------------------------------------
#  run_speed_bmr_rcurvep <- microbenchmark(
#    eval(seq_run_bmr),
#    eval(par_run_bmr),
#    times = 10
#  )

## -----------------------------------------------------------------------------
#> run_speed_bmr_rcurvep
#Unit: seconds
#              expr      min       lq     mean   median       uq      max neval
# eval(seq_run_bmr) 35.51327 35.59489 35.79890 35.81629 35.88001 36.28173    10
# eval(par_run_bmr) 14.78751 15.52596 16.19503 16.14672 16.50997 17.82284    10

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
  
  pmap(inp_tb_resp, ~ run_fit(..4, modls = "hill", hill_pdir = ifelse(..3 < 0, -1, 1), n_samples = 100, keep_sets = c("fit_set")), .options = furrr_options(seed = 2023))
)
  
# parallel run
para_fit_hill_multi <- expression(
  future::plan(multisession, workers = 10), 
  
  # calculation, no need to use future_pmap
  pmap(inp_tb_resp, ~ run_fit(..4, modls = "hill", hill_pdir = ifelse(..3 < 0, -1, 1), n_samples = 100, keep_sets = c("fit_set")), .options = furrr_options(seed = 2023)),
  future::plan(sequential)
)


## ----eval=FALSE---------------------------------------------------------------
#  run_speed_fit_hill_multi <- microbenchmark(
#    eval(seq_fit_hill_multi),
#    eval(para_fit_hill_multi),
#    times = 10
#  )

## -----------------------------------------------------------------------------
#> run_speed_fit_hill_multi
#Unit: seconds
#                      expr       min        lq      mean   median       uq      max neval
#  eval(seq_fit_hill_multi) 219.04359 220.59658 222.32948 222.2282 224.4493 225.2394    10
# eval(para_fit_hill_multi)  97.04507  98.85834  99.67153 100.1014 100.9218 101.0854    10

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

