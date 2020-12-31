<!-- badges: start -->
[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/diogo-almeida/TuckerIdrissiAlmeida2020/main?urlpath=rstudio)
[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/diogo-almeida/TuckerIdrissiAlmeida2020/master?urlpath=rstudio)
<!-- badges: end -->

# Reproducible analysis for Tucker, Idrissi & Almeida (2020). Attraction effects for verbal gender and number are similar but not identical: self-paced reading evidence from Modern Standard Arabic. Frontiers in Psychology, Language Sciences. doi: 10.3389/fpsyg.2020.586464

## Running the scripts

In order to run the scripts, you will need:

* R (tested with version 4.0.3)
* Rstudio (tested with version 1.3.1093)

1. With these in place, if you click on the "AnalysisScripts.Rproj" file, it should open an RStudio session.

2. Open the file "00-ReproduceAnalyses-Tucker_et_al_2020.R"

3. Run the "Source" command from the RStudio menu

The analysis scripts will create a folder for each experiment as well as meta-analysis. Within each folder, you will find the original datasets used in the analysis as well as the reproduced figures and tables.

## Notes

1. It may be possible to run the scripts without RStudio. I have no idea on how to do it, though, so I cannot offer any support for it.

2. The bootstrapped CIs reported in the paper will differ a little from the ones generated from these scripts. We forgot to set a reproducible seed on the original analysis scripts, so the exact bootstrapped CIs reported in the paper are not reproducible, but the ones from the scripts are.

