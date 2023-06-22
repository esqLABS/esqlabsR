require(usethis)
require(gert)
require(desc)
require(pkgdown)

###### Dev -> Release ######

## Open a new PR with new version
new_version <- usethis:::choose_version("What should the new version be?")

usethis::pr_init(branch = paste("release", new_version, sep="-"))

## Replace ospsuite dependencies with latest releases versions
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
    "Open-Systems-Pharmacology/OSPSuite.RUtils@88152af44e32e714a1d5c80237c239105ed85120",
    "Open-Systems-Pharmacology/TLF-Library@03e1acfc38c283536e1841b681a04f2eb42e08f9",
    "Open-Systems-Pharmacology/OSPSuite-R@7e08e5603982f11ae72d5c8998f3d998e8a37650",
    "Open-Systems-Pharmacology/OSPSuite.ParameterIdentification@3c934d74d95d3745b81dc1741b7c7f4d0ef00e7b"
    )
)

# INFO: Accept the commit suggestion when running this
usethis::use_version(which = labels(new_version))

pkgdown::build_site(devel = FALSE)

gert::git_add(files = "docs/.")
gert::git_commit_all("pkgdown::build_site(devel = FALSE)")

## Push branch to remote
usethis::pr_push()

## Wait for maintainers to approve and merge PR

## Close PR
usethis::pr_finish()
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

pkgdown::build_site(devel = TRUE)

gert::git_add(files = "docs/.")
gert::git_commit_all("pkgdown::build_site(devel = FALSE)")

gert::git_push()
