require(usethis)
require(gert)
require(desc)
require(pkgdown)
require(yaml)

###### Dev -> Release ######

# Choose the new version
new_version <- usethis:::choose_version("What should the new version be?")

# Open a new PR with new version
usethis::pr_init(branch = paste("release", new_version, sep = "-"))

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
  # Update the commits id after the `@` in the folowing vector with the latest
  # commits identifiers that passed checks in main development branches
  c(
    "Open-Systems-Pharmacology/OSPSuite.RUtils@8790585",
    "Open-Systems-Pharmacology/TLF-Library@033b7f7",
    "Open-Systems-Pharmacology/OSPSuite-R@4ed57da",
    "Open-Systems-Pharmacology/ospsuite.parameteridentification@0f675cc"
  )
)

# Make sure you install these versions of the dependencies and run the tests using:
# restart session
#   devtools::install_local(dependencies = TRUE)
#   devtools::test()
# If all tests are ok, you can proceed.


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

usethis::pr_init(branch = paste0("switch-to-dev-", dev_version))


# Update version in Appveyor
appveyor_config <- yaml::read_yaml(file = "appveyor.yml")

appveyor_config$environment$app_version <- dev_version

# Make sure branches$only is a list so the right format is kept when writing
# the yml file.
if (is.character(appveyor_config$branches$only)) {
  appveyor_config$branches$only <- list(appveyor_config$branches$only)
}

yaml::write_yaml(appveyor_config, file = "appveyor.yml")

gert::git_add(files = "appveyor.yml")
gert::git_commit("update version in appveyor config file")


# Update DESCRIPTION
## Update OSPS dependencies remotes to latest development versions
desc::desc_clear_remotes()

usethis::use_dev_package(
  package = "ospsuite.utils",
  type = "Imports",
  remote = "Open-Systems-Pharmacology/OSPSuite.RUtils"
)

usethis::use_dev_package(
  package = "tlf",
  type = "Imports",
  remote = "Open-Systems-Pharmacology/TLF-Library"
)

usethis::use_dev_package(
  package = "ospsuite",
  type = "Depends",
  remote = "Open-Systems-Pharmacology/OSPSuite-R"
)

usethis::use_dev_package(
  package = "ospsuite.parameteridentification",
  type = "Imports",
  remote = "Open-Systems-Pharmacology/ospsuite.parameteridentification"
)

## Update version to dev (accept commit suggestions)
usethis::use_dev_version()


# Update Documentation
## Build documentation
devtools::document()

## Commit and push docs
gert::git_add(files = "man")
gert::git_commit("devtools::document()")

## Push to main branch directly
usethis::pr_push()
