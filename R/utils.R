`%!in%` <- Negate(`%in%`)

# Defensive installation of missing packages
library <- function(pkg, ...) {
  pkg_name <- deparse1(substitute(pkg)) # get symbol as char
  
  if (pkg_name %!in% rownames(installed.packages())) {
    install.packages(pkg_name, ..., dependencies = TRUE)
  }
  
  base::library(pkg_name, ..., character.only = TRUE)
}


