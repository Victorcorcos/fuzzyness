# require(dplyr)
# require(purrr)

# Main functions
reload <- function () {
  source('/Users/victor/Desktop/Fuzzy/R/fuzzy_system/fuzzy_system.r')
}

print_fuzzy_sets <- function () {
  par(mfrow = c(3,3))
  plot(V_domain,  V_setA, type = 'l', col = 'blue', xlim = c(-100, 100), ylim = c(0, 1), main = 'Variacao de vendas', xlab = 'Domain', ylab = 'Membership Functions')
  lines(V_domain, V_setB, type = 'l', col = 'red')
  lines(V_domain, V_setC, type = 'l', col = 'green')
  legend('left', cex = 0.5, title = 'Linguistic Terms', c('Diminuindo','Estavel','Aumentando'), fill = c('blue', 'red', 'green'))
  plot(S_domain,  S_setA, type = 'l', col = 'blue', xlim = c(0, 100), ylim = c(0, 1), main = 'Sobrecarga de servicos', xlab = 'Domain', ylab = 'Membership Functions')
  lines(S_domain, S_setB, type = 'l', col = 'red')
  lines(S_domain, S_setC, type = 'l', col = 'green')
  legend('left', cex = 0.5, title = 'Linguistic Terms', c('Baixo','Media','Alta'), fill = c('blue', 'red', 'green'))
  plot(I_domain,  I_setA, type = 'l', col = 'blue', xlim = c(0, 100), ylim = c(0, 1), main = 'Sobrecarga de servicos', xlab = 'Domain', ylab = 'Membership Functions')
  lines(I_domain, I_setB, type = 'l', col = 'red')
  lines(I_domain, I_setC, type = 'l', col = 'green')
  legend('left', cex = 0.5, title = 'Linguistic Terms', c('Ruim','Medio','Bom'), fill = c('blue', 'red', 'green'))
  plot(R_domain,  R_setA, type = 'l', col = 'blue', xlim = c(0, 100), ylim = c(0, 1), main = 'Sobrecarga de servicos', xlab = 'Domain', ylab = 'Membership Functions')
  lines(R_domain, R_setB, type = 'l', col = 'red')
  lines(R_domain, R_setC, type = 'l', col = 'green')
  legend('left', cex = 0.5, title = 'Linguistic Terms', c('Leve','Media','Forte'), fill = c('blue', 'red', 'green'))
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
    image = mapply(minimo, imageA, imageB)
  } else if(type == 'produto') {
    image = mapply(produto, imageA, imageB)
  } else if(type == 'diferenca limitada') {
    image = mapply(diferenca_limitada, imageA, imageB)
  } else if(type == 'interseccao drastica') {
    image = mapply(interseccao_drastica, imageA, imageB)
  } else {
    warning('invalid t_norm! Valid ones: minimo, produto algebrico, diferenca limitada, interseccao drastica)')
  }

  return(image)
}

# T-norms
minimo <- function (x, y) { min(x, y) }
produto <- function (x, y) { x * y }
diferenca_limitada <- function (x, y) { max(0, (x + y - 1)) }
interseccao_drastica <- function (x, y) { if(x == 1) return(y) else if(y == 1) return(x) else return(0) }

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
    image = mapply(maximo, imageA, imageB)
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

# S-norms
maximo <- function (x, y) { if(x > y) return(x) else return(y) }
soma_algebrica <- function (x, y) { x + y - (x * y) }
soma_limitada <- function (x, y) { min(1, x+y) }
uniao_drastica <- function (x, y) { if(x == 0) return(y) else if(y == 0) return(x) else return(1) }

# Implication operators
implication_lukasiewics <- function (x, y) { min(1, 1-x+y) }
implication_kleene <- function (x, y) { max(1-x, y) }
implication_reichenbach <- function (x, y) { 1 - x + x*y }
implication_zadeh <- function (x, y) { max(1-x, min(x, y)) }
implication_gaines <- function (x, y) { if(x<=y) return(1) else return(0) }
implication_godel <- function (x, y) { if (x <= y) return(1) else return(y) }
implication_goguen <- function (x, y) { if (x <= y) return(1) else return(y/x) }
implication_kliryuan <- function (x, y) { 1 - x + x^2 * y }

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

trapezoidal_membership_function <- function (x, a, m, n, b) {
  if (x >= a && x < m) {
    y = (x - a) / (m - a)
  } else if(x >= m && x < n) {
    y = 1
  } else if(x >= n && x < b) {
    y = (b - x) / (b - n)
  } else {
    y = 0
  }
}

# ============== Main ============== #

# Inputs
V_domain = seq(-100, 100, 0.1)
V_setA = sapply(V_domain, trapezoidal_membership_function, a = -100, m = -100, n = -50, b = 0)
V_setB = sapply(V_domain, triangular_membership_function, a = -50, m = 0, b = 50)
V_setC = sapply(V_domain, trapezoidal_membership_function, a = 0, m = 50, n = 100, b = 100)

S_domain = seq(0, 100, 0.1)
S_setA = sapply(S_domain, triangular_membership_function, a = 0,  m = 0,   b = 50)
S_setB = sapply(S_domain, triangular_membership_function, a = 0,  m = 50,  b = 100)
S_setC = sapply(S_domain, triangular_membership_function, a = 50, m = 100, b = 100)

I_domain = seq(0, 100, 0.1)
I_setA = sapply(I_domain, triangular_membership_function, a = 0,  m = 0,   b = 50)
I_setB = sapply(I_domain, triangular_membership_function, a = 0,  m = 50,  b = 100)
I_setC = sapply(I_domain, triangular_membership_function, a = 50, m = 100, b = 100)


# Output
R_domain = seq(0, 100, 0.1)
R_setA = sapply(R_domain, triangular_membership_function, a = 0,  m = 0,   b = 50)
R_setB = sapply(R_domain, triangular_membership_function, a = 0,  m = 50,  b = 100)
R_setC = sapply(R_domain, triangular_membership_function, a = 50, m = 100, b = 100)


# Rules
# Se V está aumentando e S é alta e I é bom então R é forte.
# Se V está aumentando e S é média e I é bom então R é média.
# Se V está aumentando e S é baixa e I é bom então R é leve.
# Se V está aumentando e S é média e I é ruim então R é forte.

# apply_t_norm('minimo', fuzzy_set_A_image, fuzzy_set_B_image

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
