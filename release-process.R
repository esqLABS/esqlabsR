require(usethis)
require(gert)
require(desc)
require(pkgdown)
require(yaml)

###### Dev -> Release ######

# Choose the new version
new_version <- usethis:::choose_version("What should the new version be?")

# Open a new PR with new version
usethis::pr_init(branch = paste("release", new_version, sep="-"))

# Update version in Appveyor
appveyor_config <- yaml::read_yaml(file = "appveyor.yml")

appveyor_config$environment$app_version <- new_version

# Make sure branches$only is a list so the right format is kept when writing
# the yml file.
if (is.character(appveyor_config$branches$only)) {
  appveyor_config$branches$only <- list(appveyor_config$branches$only)
}

yaml::write_yaml(appveyor_config, file = "appveyor.yml")


# Update DESCRIPTION
## Update OSPS dependencies remotes

### Replace ospsuite dependencies with latest release versions
desc::desc_clear_remotes()

desc::desc_set_remotes(
  # When latest released version of dependencies contains all necessary features, use url::

  # Open the folowing links and copy/paste the url to the .tar.gz files
  # available in the "Assets" section.
  #   - https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils/releases/latest
  #   - https://github.com/Open-Systems-Pharmacology/TLF-library/releases/latest
  #   - https://github.com/Open-Systems-Pharmacology/OSPSuite-R/releases/latest
  #   - https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/releases/latest

  # c(
  #   "url::https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils/releases/download/v1.4.23/ospsuite.utils_1.4.23.tar.gz",
  #   "url::https://github.com/Open-Systems-Pharmacology/TLF-Library/releases/download/v1.5.125/tlf_1.5.125.tar.gz",
  #   "url::https://github.com/Open-Systems-Pharmacology/OSPSuite-R/archive/refs/tags/v11.2.251.tar.gz",
  #   "url::https://github.com/Open-Systems-Pharmacology/OSPSuite.ParameterIdentification/releases/download/v1.1.0/ospsuite.parameteridentification_1.1.0.9002.tar.gz"
  # )

  # When necessary features are not available in release versions of dependencies, use commit id
  c(
    "Open-Systems-Pharmacology/OSPSuite.RUtils@aa497333f5d1c2e7c1ba2787fbc5a4a517008936",
    "Open-Systems-Pharmacology/TLF-Library@d206f8519891df0e3717c91aa4e796903812e3d0",
    "Open-Systems-Pharmacology/OSPSuite-R@b191bc7178285b3b0ac3d0cb5f7956c87e6d96bd",
    "Open-Systems-Pharmacology/ospsuite.parameteridentification@c5c6975519afe5cf4d0176bc51301499c546e27e"
  )
)

gert::git_add(files = "appveyor.yml")
gert::git_add(files = "release-process.R")
gert::git_commit("update version in appveyor config and dependencies' commit references")

## Update Version string (accept commit suggestion)
usethis::use_version(which = labels(new_version))


# Update Documentation
## Build documentation
devtools::document()

## Commit and push docs
gert::git_add(files = "man")
gert::git_commit("devtools::document()")

## Build pkgdown site
pkgdown::build_site(devel = FALSE)

## Commit and push docs
gert::git_add(files = "docs")
gert::git_commit("pkgdown::build_site(devel = FALSE)")

## Push branch to remote
usethis::pr_push()

## Wait for maintainers to approve and merge PR

## Close PR
usethis::pr_finish()
git2r::pull()

## Create a draft release
usethis::use_github_release()



###### Release -> Dev ######



dev_version <- usethis:::choose_version(which = "dev")

usethis::pr_init(branch = paste0("switch-to-dev-",dev_version))


# Update version in Appveyor
appveyor_config <- yaml::read_yaml(file = "appveyor.yml")

appveyor_config$environment$app_version <- dev_version

yaml::write_yaml(appveyor_config, file = "appveyor.yml")

# Update DESCRIPTION
## Update OSPS dependencies remotes to latest development versions
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
                         remote = "Open-Systems-Pharmacology/ospsuite.parameteridentification")

## Update version to dev (accept commit suggestions)
usethis::use_dev_version()


# Update Documentation
## Build documentation
devtools::document()

## Commit and push docs
gert::git_add(files = "man")
gert::git_commit("devtools::document()")

## Build pkgdown site
pkgdown::build_site(devel = TRUE)

## Commit and push docs
gert::git_add(files = "docs/dev")
gert::git_commit("pkgdown::build_site(devel = TRUE)")

## Push to main branch directly
usethis::pr_push()
