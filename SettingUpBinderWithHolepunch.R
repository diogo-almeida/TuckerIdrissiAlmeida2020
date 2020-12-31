## Attempting to create research compendium with Binder

library(holepunch)
write_compendium_description(package = "TuckerIdrissiAlmeida2020", 
                             description = "Reproducibility package for the 
                             research reported in Tucker, Idrissi, & Almeida 
                             (2020).Attraction effects for verbal gender and 
                             number are similar but not identical: self-paced 
                             reading evidence from Modern Standard Arabic. 
                             Frontiers in Psychology, Language Sciences. 
                             doi: 10.3389/fpsyg.2020.586464")

write_dockerfile(maintainer = "diogo-almeida", branch = "main", 
                 r_date = "2020-04-03") 
# To write a Dockerfile. It will automatically pick the date of the last 
# modified file, match it to that version of R and add it here. You can 
# override this by passing r_date to some arbitrary date
# (but one for which a R version exists).

generate_badge() # This generates a badge for your readme.

# ----------------------------------------------
# At this time 🙌 push the code to GitHub 🙌
# ----------------------------------------------

# And click on the badge or use the function below to get the build 
# ready ahead of time.
build_binder()
# 🤞🚀
