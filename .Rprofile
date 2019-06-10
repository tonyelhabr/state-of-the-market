
path_r_profile <- "~/.Rprofile"
if(file.exists(path_r_profile)) {
  source(path_r_profile)
}
rm("path_r_profile")

# Reference: https://github.com/yihui/xfun/R/packages.R
.library_silently <- function(...) {
  suppressWarnings(suppressPackageStartupMessages(base::library(...)))
}

.library_silently("tidyverse")
.library_silently("rlang")
rm(".library_silently")

invisible(R.utils::sourceDirectory(file.path("R", "functions"), recursive = FALSE))