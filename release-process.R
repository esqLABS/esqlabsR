library(usethis)

###### Dev -> Release ######

## Open a new PR with new version
new_version <- usethis:::choose_version("What should the new version be?")

usethis::pr_init(branch = paste("release", new_version, sep="-"))

## Replace ospsuite dependencies with latest releases versions
desc::desc_clear_remotes()

# INFO: Open the folowing links and copy/paste the url to the .tar.gz files
# available in the "Assets" section.
#   - https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils/releases/latest
#   - https://github.com/Open-Systems-Pharmacology/TLF-library/releases/latest
#   - https://github.com/Open-Systems-Pharmacology/OSPSuite-R/releases/latest
#   - https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/releases/latest

desc::desc_set_remotes(
  c(paste(
    "url::",
    c("https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils/releases/download/v1.4.23/ospsuite.utils_1.4.23.tar.gz",
      "https://github.com/Open-Systems-Pharmacology/TLF-Library/releases/download/v1.5.125/tlf_1.5.125.tar.gz",
      "https://github.com/Open-Systems-Pharmacology/OSPSuite-R/archive/refs/tags/v11.2.251.tar.gz"
      # "https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/releases/download/v1.1.0/ospsuite.parameteridentification_1.1.0.9002.tar.gz" # Update this when 1.2 is "release" in repo
    ),
    collapse = ",\n\t\t",
    sep = ""),
    "Open-Systems-Pharmacology/OSPSuite.ParameterIdentification" # delete this when 1.2 is released for PI
  )
)

# INFO: choose patch, minor or major and accept the commit suggestion.
usethis::use_version(which = labels(new_version))

## Push branch to remote
pr_push()

## Wait for maintainers to approve and merge PR

## Close PR
pr_finish()
git2r::pull()

## Create a draft release
usethis::use_github_release()



###### Release -> Dev ######

## Replace ospsuite dependencies remotes for development branches
desc::desc_clear_remotes()

usethis::use_dev_package(package = "ospsuite.utils",
                         type = "Imports",
                         remote = "Open-Systems-Pharmacology/OSPSuite.RUtils")

usethis::use_dev_package(package = "tlf",
                         type = "Imports",
                         remote = "Open-Systems-Pharmacology/TLF-Library")

usethis::use_dev_package(package = "ospsuite",
                         type = "Depends",
                         remote = "Open-Systems-Pharmacology/OSPSuite-R")

usethis::use_dev_package(package = "ospsuite.parameteridentification",
                         type = "Imports",
                         remote = "Open-Systems-Pharmacology/OSPSuite.ParameterIdentification")

## Add .9000 to version number
usethis::use_dev_version()

# Push
system(command = "git push")
