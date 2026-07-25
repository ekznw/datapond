# Development launcher for Data Pond.
#
# Installing the package and calling datapond::run_data_pond() is the
# supported way to run the application. This file remains so that "Run App"
# also works from a source checkout.

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop(
    "Package 'pkgload' is required to run Data Pond from its source tree. ",
    "Install the package and call datapond::run_data_pond() instead."
  )
}

pkgload::load_all(export_all = FALSE, helpers = FALSE)
datapond::run_data_pond()
