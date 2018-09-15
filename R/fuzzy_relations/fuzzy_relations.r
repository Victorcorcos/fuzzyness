require(tidyr)

# Main functions
reload <- function () {
  source('/Users/victorcosta/Desktop/fuzzyness/R/fuzzy_relations/fuzzy_relations.r')
}

x <- data.frame(x = c("a", "b", "c"))
y <- data.frame(y = c(1, 2, 3))

crossing(x, y)


# Criando matrizes
# matrix(rnorm(30), nrow=5, ncol=6)

# PCA 3D R
# https://cran.r-project.org/web/packages/plot3D/vignettes/plot3D.pdf
# https://stackoverflow.com/questions/43228379/cartesian-product-with-dplyr-r?noredirect=1&lq=1

# Moodle
# https://www.moodle.ufba.br/pluginfile.php/616508/mod_resource/content/1/04.Rela%C3%A7%C3%B5es%20fuzzy.pdf