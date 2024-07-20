if (FALSE) {
	### basic dev cycle
	load_all()
	document()
	check()
	install()

	# install.packages("roxygen2")

	### declare files to be ignored during build
	use_build_ignore("dev_code.R")
	use_build_ignore("data-raw")
	# use_git_ignore("dev_code.R")

	### declare intent to use GITHUB
	use_git()

	### set package licence
	use_mit_license()

	### declare use of RMARKDOWN for help files
	use_roxygen_md()

	### generate a readme file for GITHUB
	use_readme_rmd()

	### update the readme
	devtools::build_readme()

	### declare use of tests
	use_testthat()
	use_test("xxx")

	### declare dependencies
	use_package("cli")
	use_package("dplyr")
	use_package("future")
	use_package("future.apply")
	use_package("ggplot2")
	use_package("lubridate")
	use_package("magrittr")
	use_package("progressr")
	use_package("purrr")
	use_package("readr")
	use_package("stringr")
	use_package("tibble")
	use_package("tidyr")
	use_package("terra")
	use_package("Rfes2014")

	### create and edit a new R file
	use_r("xxx")

	### GENERATE EXAMPLE DATA
	# run code in "data-raw/pulse_data.R"
}
