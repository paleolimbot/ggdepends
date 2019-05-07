
library(tidyverse)

target_pkg <- "ggplot2"
target_repo <- "tidyverse/ggplot2"

packages <- tibble(
  target_pkg = target_pkg,
  target_repo = target_repo,
  foreign_pkg = c("ggspatial", "ggplot2", "lime", "ggstance"),
  foreign_repo = c("cran/ggspatial", "tidyverse/ggplot2", "thomasp85/lime", "lionel-/ggstance")
)

for(pkg in packages$foreign_pkg) {
  message("Finding **", target_pkg, "** dependencies in **", pkg, "**")
  system(paste("Rscript", "find_usage.R", target_pkg, pkg), intern = TRUE)
}

write_csv(packages, "data/_collect-params.csv")
