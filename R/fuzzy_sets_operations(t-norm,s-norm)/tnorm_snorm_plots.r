require(dplyr)
require(purrr)

# Main functions
reload <- function () {
  source('/Users/victorcosta/Desktop/Fuzzy/R/fuzzy_sets_operations(t-norm,s-norm)/tnorm_snorm_plots.r')
}

print_fuzzy_sets <- function () {
  plot(fuzzy_set_A_domain, fuzzy_set_A_image, type = 'l', col = 'blue', xlim = c(0, 10), ylim = c(0, 1), main = 'Fuzzy Sets (A and B)', xlab = 'Domain', ylab = 'Membership Functions')
  lines(fuzzy_set_B_domain, fuzzy_set_B_image, type = 'l', col = 'red')
}

# Possible t-norms
# minimo, produto, diferenca limitada, interseccao drastica
t_norms <- function () {
  domain = seq(0, 10, 0.1)

  par(mfrow = c(3,3))
  print_fuzzy_sets()
  plot(domain, apply_t_norm('minimo', fuzzy_set_A_image, fuzzy_set_B_image), type = 'l', col = 'green', xlim = c(0, 10), ylim = c(0, 1), main = 'T-norm (Minimo)', xlab = 'Domain', ylab = 'Membership Functions')
  plot(domain, apply_t_norm('produto', fuzzy_set_A_image, fuzzy_set_B_image), type = 'l', col = 'lightskyblue', xlim = c(0, 10), ylim = c(0, 1), main = 'T-norm (Produto Algebrico)', xlab = 'Domain', ylab = 'Membership Functions')
  plot(domain, apply_t_norm('diferenca limitada', fuzzy_set_A_image, fuzzy_set_B_image), type = 'l', col = 'orange2', xlim = c(0, 10), ylim = c(0, 1), main = 'T-norm (Lukasiewics)', xlab = 'Domain', ylab = 'Membership Functions')
  plot(domain, apply_t_norm('interseccao drastica', fuzzy_set_A_image, fuzzy_set_B_image), type = 'l', col = 'yellow3', xlim = c(0, 10), ylim = c(0, 1), main = 'T-norm (Interseccao Drastica)', xlab = 'Domain', ylab = 'Membership Functions')

  # 1. T-NORMAS (intersecção)
  # Mínimo: min(x, y)
  # Produto algébrico: x * y
  # Diferença limitada (Lukasiewics): max(0, x+y-1)
  # Intersecção drástica: y se x=1; x se y=1; 0 c.c
  # Yager’s intersection: 1 - min[1, ((1 - x)^w + (1 - y)^w)^(1/w)], where w belongs to (0, infinity)
}

apply_t_norm <- function (type, imageA, imageB) {
  if(type == 'minimo') {
    image = mapply(min, imageA, imageB)
  } else if(type == 'produto') {
    image = mapply(prod, imageA, imageB)
  } else if(type == 'diferenca limitada') {
    image = mapply(diferenca_limitada, imageA, imageB)
  } else if(type == 'interseccao drastica') {
    image = mapply(interseccao_drastica, imageA, imageB)
  } else {
    warning('invalid t_norm! Valid ones: minimo, produto algebrico, diferenca limitada, interseccao drastica)')
  }

  return(image)
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

# Possible s-norms
# maximo, soma algebrica, soma limitada, uniao drastica
s_norms <- function () {
  domain = seq(0, 10, 0.1)

  par(mfrow = c(3,3))
  print_fuzzy_sets()
  plot(domain, apply_s_norm('maximo', domain, fuzzy_set_A_image, fuzzy_set_B_image), type = 'l', col = 'green', xlim = c(0, 10), ylim = c(0, 1), main = 'S-norm (Maximo)', xlab = 'Domain', ylab = 'Membership Functions')
  plot(domain, apply_s_norm('soma algebrica', domain, fuzzy_set_A_image, fuzzy_set_B_image), type = 'l', col = 'lightskyblue', xlim = c(0, 10), ylim = c(0, 1), main = 'S-norm (Soma Algebrica)', xlab = 'Domain', ylab = 'Membership Functions')
  plot(domain, apply_s_norm('soma limitada', domain, fuzzy_set_A_image, fuzzy_set_B_image), type = 'l', col = 'orange2', xlim = c(0, 10), ylim = c(0, 1), main = 'S-norm (Lukasiewics)', xlab = 'Domain', ylab = 'Membership Functions')
  plot(domain, apply_s_norm('uniao drastica', domain, fuzzy_set_A_image, fuzzy_set_B_image), type = 'l', col = 'yellow3', xlim = c(0, 10), ylim = c(0, 1), main = 'S-norm (Uniao Drastica)', xlab = 'Domain', ylab = 'Membership Functions')

  # 2. T-CONORMAS (união)
  # Máximo: max(x, y)
  # Soma algébrica (probabilística): x + y - x*y
  # Soma limitada (Likasiewicz): min(1, x+y)
  # União drástica: x se y=0; y se x=0; 1 c.c
  # Yager’s union: min[1, (x^w + y^w)^(1/w)], where w belongs to (0, infinity)
}

apply_s_norm <- function (type, domain, imageA, imageB) {
  if(type == 'maximo') {
    image = mapply(max, imageA, imageB)
  } else if(type == 'soma algebrica') {
    image = mapply(soma_algebrica, imageA, imageB)
  } else if(type == 'soma limitada') {
    image = mapply(soma_limitada, imageA, imageB)
  } else if(type == 'uniao drastica') {
    image = mapply(uniao_drastica, imageA, imageB)
  } else {
    warning('invalid s_norm! Valid ones: maximo, soma algebrica, soma limitada, uniao drastica)')
  }

  return(image)
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
fuzzy_set_A_image = sapply(fuzzy_set_A_domain, triangular_membership_function, a = 1.0, m = 3.5, b = 6.0)

fuzzy_set_B_domain = seq(0, 10, 0.1)
fuzzy_set_B_image = sapply(fuzzy_set_B_domain, triangular_membership_function, a = 3.0, m = 5.5, b = 7.0)




# R Apply family (apply(), sapply(), mapply(), tapply() and so on)
# https://www.datacamp.com/community/tutorials/r-tutorial-apply-family?utm_source=adwords_ppc&utm_campaignid=1455363063&utm_adgroupid=65083631748&utm_device=c&utm_keyword=&utm_matchtype=b&utm_network=g&utm_adpostion=1t1&utm_creative=278443377077&utm_targetid=dsa-473406586995&utm_loc_interest_ms=&utm_loc_physical_ms=1001533&gclid=CjwKCAjw2_LcBRBYEiwA_XVBU-8G9basGQa4sgLIUlojB4rsG-PqwhX32aGhwOOBJ1q7Xik9BaddshoCdX0QAvD_BwE

################## Aplicar... ###################
#
# ============== Operações Padrões ==============
# União => max(A(x), B(x))
# Intersecção => min(A(x), B(x))
# Complemento => A’(x) = 1 – A(x)
#
#
# =========== Operações generalizadas ===========
#
# 1. T-NORMAS (intersecção)
# Mínimo: min(x, y)
# Produto algébrico: x * y
# Diferença limitada (Lukasiewics): max(0, x+y-1)
# Intersecção drástica: y se x=1; x se y=1; 0 c.c
#
# 2. T-CONORMAS (união)
# Máximo: max(x, y)
# Soma algébrica (probabilística): x + y - x*y
# Soma limitada (Lukasiewicz): min(1, x+y)
# União drástica: x se y=0; y se x=0; 1 c.c
#
################################################

###################### R REMINDERS ###################### 
#
#
# plot(fuzzy_set_B_domain, fuzzy_set_B_image, type = 'l')
#
# A => a = 1, m = 3.5, b = 6
# B => a = 3, m = 5.5, b = 7
#
# source('/Users/victorcosta/Desktop/R/lets plot.r')
# ?plot
# typeof(2)
# clear <- function() cat(rep("\n", 50))
#
#=========================================================
