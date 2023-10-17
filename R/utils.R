`%!in%` <- `%not.in%` <- Negate(`%in%`)
is.not.na <- Negate(is.na)
is.not.null <- Negate(is.null)
`%||%` <- default <- function(a,b) if (is.not.null(a)) a else b

keep <- dplyr::filter

.cap <- function(s) { sub("^(.*)", "\\L\\1", s, perl = TRUE) }
.CAP <- function(s) { sub("^(.*)", "\\U\\1", s, perl = TRUE) }
.Cap <- function(s) { sub("^(.)(.*)", "\\U\\1\\L\\2", s, perl = TRUE) }
.CaP <- function(s) { paste(sapply(unlist(strsplit(s, " ")), .Cap), collapse = " ") }

# x <- "al Pha Bra vo"
# .cap(x)  # "al pha bra vo"
# .CAP(x)  # "AL PHA BRA VO"
# .Cap(x)  # "Al pha bra vo"
# .CaP(x)  # "Al Pha Bra Vo"

English <- function(num) {
  print(.Cap(english::words(num)))
  invisible(num)
}

# Defensive installation of missing packages
library <- function(pkg, ..., pkg_name = deparse1(substitute(pkg))) {
  if (pkg_name %!in% rownames(installed.packages())) {
    install.packages(pkg_name, ..., dependencies = TRUE)
  }
  if (pkg_name %in% rownames(installed.packages())) {
    base::library(pkg_name, ..., character.only = TRUE)
  }
}

