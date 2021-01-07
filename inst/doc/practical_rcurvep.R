## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
#https://github.com/rstudio/rmarkdown/issues/1268 has to move project rmd_knit_tem

## ---- eval=FALSE--------------------------------------------------------------
#  # the development version from GitHub:
#  # install.packages("devtools")
#  devtools::install_github("moggces/Rcurvep")
#  devtools::install_github("moggces/Rcurvep", build_vignettes = TRUE)

## ----setup--------------------------------------------------------------------
library(Rcurvep)

## -----------------------------------------------------------------------------
# More details of the dataset can be found ?zfishdev.
data(zfishdev) 
str(zfishdev)

## -----------------------------------------------------------------------------
curvep_defaults()
#For details of parameters, see ?curvep.


## -----------------------------------------------------------------------------
identical(
  combi_run_rcurvep(zfishdev, RNGE = 1000000),
  combi_run_rcurvep(create_dataset(zfishdev), RNGE = 1000000)
)

## -----------------------------------------------------------------------------
out <- combi_run_rcurvep(
  zfishdev,
  TRSH = 25, # BMR = 25
  RNGE = 1000000 # increasing direction
)
out
out$config

## -----------------------------------------------------------------------------
sum_out <- summarize_rcurvep_output(out)
sum_out

## -----------------------------------------------------------------------------
set.seed(300)
out <- combi_run_rcurvep(
  zfishdev,
  n_samples = 10, # often 1000 samples are preferred
  TRSH = 25,
  RNGE = 1000000,
  keep_sets = "act_set"
)
sum_out <- summarize_rcurvep_output(out)
sum_out

## ----eval = FALSE-------------------------------------------------------------
#  # The combi_run_rcurvep() can be used for a combination of Curvep parameters.
#  # finishing the code will take some time.
#  
#  set.seed(300)
#  data(zfishdev_all)
#  
#  zfishdev_act <- combi_run_rcurvep(
#  
#    zfishdev_all,
#    n_samples = 100,
#    keep_sets = c("act_set"),
#    TRSH = seq(5, 95, by = 5), # test all candidates, 5 to 95
#    RNGE = 1000000,
#    CARR = 20
#  )
#  

## -----------------------------------------------------------------------------
data(zfishdev_act)
bmr_out <- estimate_dataset_bmr(zfishdev_act, plot = FALSE)
bmr_out$outcome

## ---- message=FALSE, warning = FALSE, fig.align = "center", fig.width = 6-----
plot(bmr_out)

## -----------------------------------------------------------------------------
fitd <- run_fit(create_dataset(zfishdev), hill_pdir = 1, modls = "hill")
fitd

## -----------------------------------------------------------------------------
fitd_sum_out <- summarize_fit_output(fitd, thr_resp  = 25)
fitd_sum_out

## -----------------------------------------------------------------------------
fitd <- run_fit(create_dataset(zfishdev), hill_pdir = 1, n_samples = 10, modls = "hill")

