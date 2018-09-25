require(tidyr)

reload <- function () {
  source('/Users/victorcosta/Desktop/Fuzzy/R/fuzzy_relations/fuzzy_relations.r')
}

print_fuzzy_sets <- function () {
  plot(fuzzy_set_A_domain, fuzzy_set_A_image, type = 'l', col = 'blue', xlim = c(0, 10), ylim = c(0, 1), main = 'Fuzzy Sets (A and B)', xlab = 'Domain', ylab = 'Membership Functions')
  lines(fuzzy_set_B_domain, fuzzy_set_B_image, type = 'l', col = 'red')
}

# Possible input names!
#
# === t_norms ===
# minimo
# produto
# diferenca_limitada
# interseccao_drastica
#
# === s_norms ===
# maximo
# soma_algebrica
# soma_limitada
# uniao_drastica
relation_norm <- function (name) {
  norm = eval(parse(text = name))
  x = fuzzy_set_A_domain
  y = fuzzy_set_B_domain
  z = outer(fuzzy_set_A_image, fuzzy_set_B_image, function(a, b) { mapply(norm, a, b) })
  persp(x, y, z,
    main = paste('Fuzzy relation operation:', name),
    xlab = 'Relation A',
    ylab = 'Relation B',
    zlab = 'Membership Functions',
    col = 'lightskyblue',
    theta = -25,
    phi = 30,
    r = 6,
    ticktype = 'detailed')
}

# ================ T-Norms ================
minimo <- function (x, y) {
  min(x, y)
}

produto <- function (x, y) {
  x * y
}

diferenca_limitada <- function (x, y) {
  max(0, (x + y - 1))
}

interseccao_drastica <- function (x, y) {
  if(x == 1) {
    return(y)
  } else if(y == 1) {
    return(x)
  } else {
    return(0)
  }
}

# ================ S-Norms ================
maximo <- function (x, y) {
  if(x > y) {
    return(x)
  } else {
    return(y)
  }
}

soma_algebrica <- function (x, y) {
  x + y - (x * y)
}

soma_limitada <- function (x, y) {
  min(1, x+y)
}

uniao_drastica <- function (x, y) {
  if(x == 0) {
    return(y)
  } else if(y == 0) {
    return(x)
  } else {
    return(1)
  }
}

# # =========================================
# t_norms <- function () {
#   domain = seq(0, 10, 0.1)

#   par(mfrow = c(3,3))
#   print_fuzzy_sets()
#   plot(domain, apply_t_norm('minimo', fuzzy_set_A_image, fuzzy_set_B_image), type = 'l', col = 'green', xlim = c(0, 10), ylim = c(0, 1), main = 'T-norm (Minimo)', xlab = 'Domain', ylab = 'Membership Functions')
#   plot(domain, apply_t_norm('produto', fuzzy_set_A_image, fuzzy_set_B_image), type = 'l', col = 'lightskyblue', xlim = c(0, 10), ylim = c(0, 1), main = 'T-norm (Produto Algebrico)', xlab = 'Domain', ylab = 'Membership Functions')
#   plot(domain, apply_t_norm('diferenca limitada', fuzzy_set_A_image, fuzzy_set_B_image), type = 'l', col = 'orange2', xlim = c(0, 10), ylim = c(0, 1), main = 'T-norm (Lukasiewics)', xlab = 'Domain', ylab = 'Membership Functions')
#   plot(domain, apply_t_norm('interseccao drastica', fuzzy_set_A_image, fuzzy_set_B_image), type = 'l', col = 'yellow3', xlim = c(0, 10), ylim = c(0, 1), main = 'T-norm (Interseccao Drastica)', xlab = 'Domain', ylab = 'Membership Functions')
# }


# apply_t_norm <- function (type, imageA, imageB) {
#   if(type == 'minimo') {
#     image = mapply(minimo, imageA, imageB)
#   } else if(type == 'produto') {
#     image = mapply(produto, imageA, imageB)
#   } else if(type == 'diferenca limitada') {
#     image = mapply(diferenca_limitada, imageA, imageB)
#   } else if(type == 'interseccao drastica') {
#     image = mapply(interseccao_drastica, imageA, imageB)
#   } else {
#     warning('invalid t_norm! Valid ones: minimo, produto algebrico, diferenca limitada, interseccao drastica)')
#   }

#   return(image)
# }

# s_norms <- function () {
#   domain = seq(0, 10, 0.1)

#   par(mfrow = c(3,3))
#   print_fuzzy_sets()
#   plot(domain, apply_s_norm('maximo', domain, fuzzy_set_A_image, fuzzy_set_B_image), type = 'l', col = 'green', xlim = c(0, 10), ylim = c(0, 1), main = 'S-norm (Maximo)', xlab = 'Domain', ylab = 'Membership Functions')
#   plot(domain, apply_s_norm('soma algebrica', domain, fuzzy_set_A_image, fuzzy_set_B_image), type = 'l', col = 'lightskyblue', xlim = c(0, 10), ylim = c(0, 1), main = 'S-norm (Soma Algebrica)', xlab = 'Domain', ylab = 'Membership Functions')
#   plot(domain, apply_s_norm('soma limitada', domain, fuzzy_set_A_image, fuzzy_set_B_image), type = 'l', col = 'orange2', xlim = c(0, 10), ylim = c(0, 1), main = 'S-norm (Lukasiewics)', xlab = 'Domain', ylab = 'Membership Functions')
#   plot(domain, apply_s_norm('uniao drastica', domain, fuzzy_set_A_image, fuzzy_set_B_image), type = 'l', col = 'yellow3', xlim = c(0, 10), ylim = c(0, 1), main = 'S-norm (Uniao Drastica)', xlab = 'Domain', ylab = 'Membership Functions')
# }

# apply_s_norm <- function (type, domain, imageA, imageB) {
#   if(type == 'maximo') {
#     image = mapply(maximo, imageA, imageB)
#   } else if(type == 'soma algebrica') {
#     image = mapply(soma_algebrica, imageA, imageB)
#   } else if(type == 'soma limitada') {
#     image = mapply(soma_limitada, imageA, imageB)
#   } else if(type == 'uniao drastica') {
#     image = mapply(uniao_drastica, imageA, imageB)
#   } else {
#     warning('invalid s_norm! Valid ones: maximo, soma algebrica, soma limitada, uniao drastica)')
#   }

#   return(image)
# }

triangular_membership_function <- function (x, a, m, b) {
  if (x >= a && x < m) {
    y = (x - a) / (m - a)
  } else if (x >= m && x < b) {
    y = (b - x) / (b - m)
  } else {
    y = 0
  }
  y
}

# ============== Main ============== #
fuzzy_set_A_domain = seq(0, 10, 0.1)
fuzzy_set_A_image =  sapply(fuzzy_set_A_domain, triangular_membership_function, a = 1.0, m = 3.5, b = 6.0) # seq(0, 1, length = 101)

fuzzy_set_B_domain = seq(0, 10, 0.1)
fuzzy_set_B_image =  sapply(fuzzy_set_B_domain, triangular_membership_function, a = 3.0, m = 5.5, b = 7.0) # seq(0, 1, length = 101)


# Good functions!
#
# help.search("concatenate")
# matrix(rnorm(30), nrow=5, ncol=6)
# unlist(list(1, 2, 3))
# sapply(list(1, 2, 3), function(x) { x })

# PCA 3D R
# https://www.datamentor.io/r-programming/3d-plot/ # outer() function => WOW
# https://cran.r-project.org/web/packages/plot3D/vignettes/plot3D.pdf
# https://stackoverflow.com/questions/43228379/cartesian-product-with-dplyr-r?noredirect=1&lq=1

# Moodle
# https://www.moodle.ufba.br/pluginfile.php/616508/mod_resource/content/1/04.Rela%C3%A7%C3%B5es%20fuzzy.pdf
