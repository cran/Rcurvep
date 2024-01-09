## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
#https://github.com/rstudio/rmarkdown/issues/1268 has to move project rmd_knit_tem

## ----eval=FALSE---------------------------------------------------------------
#  # the development version from GitHub:
#  # install.packages("devtools")
#  devtools::install_github("moggces/Rcurvep")
#  devtools::install_github("moggces/Rcurvep", dependencies = TRUE, build_vignettes = TRUE)

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

## ----message=FALSE, warning = FALSE, fig.align = "center", fig.width = 6------
plot(bmr_out)

## -----------------------------------------------------------------------------
# set the preferred direction as increasing hill_pdir = 1
# this is to use the 3-parameter hill 
fitd1 <- run_fit(create_dataset(zfishdev), hill_pdir = 1, modls = "hill")
fitd1

# can also use the curve class2 4-parameter hill with classification SD as 5% 
# please ?fit_cc2_modl to understand curve classification
fitd2 <- run_fit(create_dataset(zfishdev), cc2_classSD = 5, modls = "cc2")
fitd2 

## -----------------------------------------------------------------------------

# thr_resp will get BMC10% and perc_resp will get EC20%


# hill with 3-parameter 
fitd_sum_out1 <- summarize_fit_output(fitd1, thr_resp  = 10, perc_resp = 20, extract_only = TRUE)

# cc2 (hill with 4-parameter + curve classification)
fitd_sum_out2 <- summarize_fit_output(fitd2, thr_resp  = 10, perc_resp = 20, extract_only = TRUE)


## -----------------------------------------------------------------------------
#EC20% concordance (when both methods provide values)
cor(fitd_sum_out1$result$act_set$ECxx, fitd_sum_out2$result$act_set$ECxx, use = "pairwise.complete.obs")

#BMC10% (when both methods provide values)
cor(fitd_sum_out1$result$act_set$POD, fitd_sum_out2$result$act_set$POD, use = "pairwise.complete.obs")

#EC50 (when both methods provide values)
cor(fitd_sum_out1$result$act_set$EC50, fitd_sum_out2$result$act_set$EC50, use = "pairwise.complete.obs")


# check number of curves consider as active by both
sum(fitd_sum_out1$result$act_set$hit == 0) # no fit
sum(fitd_sum_out2$result$act_set$hit == 4) # cc2 = 4 (inactive)


## -----------------------------------------------------------------------------
fitd <- run_fit(create_dataset(zfishdev), hill_pdir = 1, n_samples = 10, modls = "hill")

