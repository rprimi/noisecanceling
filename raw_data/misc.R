use_package("psych")
use_package("dplyr")
use_package("purrr")
use_package( "plyr")
use_package("openxlsx")
use_package("xlsx")
use_package("stringr")
usethis::use_package("magrittr")



devtools::build_readme()
use_pipe(export = TRUE)
library(tidyverse)

usethis::use_r("recoding_functions.R")
