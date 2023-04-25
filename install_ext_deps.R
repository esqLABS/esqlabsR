install.packages('https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v0.9.2/rClr_0.9.2.zip', repos = NULL, type = 'binary')

# Install suggested dependencies
install.packages(c("clipr",
                   "knitr",
                   "openxlsx",
                   "rmarkdown",
                   "showtext", # to pass snaps tests
                   "shiny",
                   "shinyjs",
                   "shinytest2",
                   "testthat",
                   "vdiffr",
                   "withr"),
                 repos = 'http://cran.us.r-project.org')
