# Please download and install
#   A Plain Text Editor: Notepad++ for Windows or BBEdit for Mac work well
#   R 4.x
#   RStudio

# In the RStudio Console pane:

`%!in%` <- Negate(`%in%`)

ip <- function(pkgs) {
  pkgs <- pkgs[pkgs %!in% rownames(installed.packages())]
  install.packages(pkgs, dependencies = TRUE)
}

ip(c(
  "tidyverse", "remotes",
  "here", "janitor", "hexView",
  "DiagrammeR", "wordcloud", "igraph", "ggraph", "incidentally", "ggforce",
  "tm", "topicmodels", "widyr", "XML",
  "tidytext", "textrecipes", "tidylda", "quanteda",
  "janeaustenr", "aRxiv",
  "udpipe", "NLP", "openNLP", "openNLPdata",
  "sotu", "corporaexplorer", "humaniformat"
))

remotes::install_github("https://github.com/ropensci/gutenbergr", dependencies = TRUE)

remotes::install_url("http://datacube.wu.ac.at/src/contrib/openNLPmodels.en_1.5-1.tar.gz")

udpipe::udpipe_download_model(language = "english-ewt", model_dir = "udpipe_models")

# You may want to update.packages(ask = FALSE)

