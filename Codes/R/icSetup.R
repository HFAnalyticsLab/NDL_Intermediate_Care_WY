need_packages <- c(
  'config',
  'tidyverse',
  'lubridate',
  'readxl',
  'tidyr',
  'tibble',
  'openxlsx',
  'odbc',
  'janitor',
  'cowplot',
  'ggpubr',
  'RcppAlgos',
  'furrr',
  'tictoc',
  'moments'
)

installed <- need_packages %in% installed.packages()
if(length(need_packages[!installed]) > 0) install.packages(need_packages[!installed], type = 'binary')
lapply(need_packages, library, character.only = TRUE)

options(stringsAsFactors = FALSE)

config <- get()

# Load custom functions
invisible(
  lapply(
    Sys.glob('user_defined_functions/*.R'),
    function(x) source(x)
  )
)
