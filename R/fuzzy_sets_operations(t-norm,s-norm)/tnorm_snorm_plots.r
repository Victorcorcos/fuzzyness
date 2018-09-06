require(dplyr)
require(purrr)

# Main functions
reload <- function () {
  source('/Users/victorcosta/Desktop/fuzzyness/R/fuzzy_sets_operations(t-norm,s-norm)/tnorm_snorm_plots.r')
}

print_fuzzy_sets <- function () {
  plot(fuzzy_set_A_domain, fuzzy_set_A_image, type = 'l', col = 'blue', xlim = c(0, 10), ylim = c(0, 1), main = 'Fuzzy Sets (A and B)', xlab = 'Domain', ylab = 'Membership Functions')
  lines(fuzzy_set_B_domain, fuzzy_set_B_image, type = 'l', col = 'red')
}

# Possible t-norms
# minimo, produto, diferenca limitada, interseccao drastica
t_norms <- function () {
  domain = c(0:100)/10

  par(mfrow = c(3,3))
  print_fuzzy_sets()
  plot(domain, apply_t_norm('minimo', domain, fuzzy_set_A_image, fuzzy_set_B_image), type = 'l', col = 'green', xlim = c(0, 10), ylim = c(0, 1), main = 'T-norm (Minimo)', xlab = 'Domain', ylab = 'Membership Functions')
  plot(domain, apply_t_norm('produto', domain, fuzzy_set_A_image, fuzzy_set_B_image), type = 'l', col = 'lightskyblue', xlim = c(0, 10), ylim = c(0, 1), main = 'T-norm (Produto Algebrico)', xlab = 'Domain', ylab = 'Membership Functions')
  plot(domain, apply_t_norm('diferenca limitada', domain, fuzzy_set_A_image, fuzzy_set_B_image), type = 'l', col = 'orange2', xlim = c(0, 10), ylim = c(0, 1), main = 'T-norm (Lukasiewics)', xlab = 'Domain', ylab = 'Membership Functions')
  plot(domain, apply_t_norm('interseccao drastica', domain, fuzzy_set_A_image, fuzzy_set_B_image), type = 'l', col = 'yellow3', xlim = c(0, 10), ylim = c(0, 1), main = 'T-norm (Interseccao Drastica)', xlab = 'Domain', ylab = 'Membership Functions')

  # 1. T-NORMAS (intersecção)
  # Mínimo: min(x, y)
  # Produto algébrico: x * y
  # Diferença limitada (Lukasiewics): max(0, x+y-1)
  # Intersecção drástica: y se x=1; x se y=1; 0 c.c
}

apply_t_norm <- function (type, domain, imageA, imageB) {
  image = c()

  if(type == 'minimo') {
    for(i in 1:length(domain)) {
      image[i] = min(cast(imageA[i]), cast(imageB[i]))
    }
  } else if(type == 'produto') {
    for(i in 1:length(domain)) {
      image[i] = cast(imageA[i]) * cast(imageB[i])
    }
  } else if(type == 'diferenca limitada') {
    for(i in 1:length(domain)) {
      image[i] = max(0, cast(imageA[i])+cast(imageB[i])-1)
    }
  } else if(type == 'interseccao drastica') {
    for(i in 1:length(domain)) {
      if(cast(imageA[i]) == 1) {
        image[i] = cast(imageB[i])
      } else if(cast(imageB[i]) == 1) {
        image[i] = cast(imageA[i])
      } else {
        image[i] = 0
      }
    }
  } else {
    warning('invalid t_norm! Valid ones: minimo, produto algebrico, diferenca limitada, interseccao drastica)')
  }

  return(image)
}

# Possible s-norms
# maximo, soma algebrica, soma limitada, uniao drastica
s_norms <- function () {
  domain = c(0:100)/10

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
}

apply_s_norm <- function (type, domain, imageA, imageB) {
  image = c()

  if(type == 'maximo') {
    for(i in 1:length(domain)) {
      image[i] = max(cast(imageA[i]), cast(imageB[i]))
    }
  } else if(type == 'soma algebrica') {
    for(i in 1:length(domain)) {
      image[i] = cast(imageA[i]) + cast(imageB[i]) - (cast(imageA[i]) * cast(imageB[i]))
    }
  } else if(type == 'soma limitada') {
    for(i in 1:length(domain)) {
      image[i] = min(1, cast(imageA[i])+cast(imageB[i]))
    }
  } else if(type == 'uniao drastica') {
    for(i in 1:length(domain)) {
      if(cast(imageA[i]) == 0) {
        image[i] = cast(imageB[i])
      } else if(cast(imageB[i]) == 0) {
        image[i] = cast(imageA[i])
      } else {
        image[i] = 1
      }
    }
  } else {
    warning('invalid s_norm! Valid ones: maximo, soma algebrica, soma limitada, uniao drastica)')
  }

  return(image)
}


# ============== Auxiliar functions ============== #
calculate_image <- function (domain, a, m, b) {
  image = c()
  for(i in 1:length(domain)) {
    image[i] = get_y(domain[i], a, m, b)
  }
  image
  # map(domain, function (x) { get_y(x, a, m, b) })
}

get_y <- function (x, a, m, b) {
  if (x >= a && x < m) {
    result = (x - a) / (m - a)
  } else if (x >= m && x < b) {
    result = (b - x) / (b - m)
  } else {
    result = 0
  }
  result
}

cast <- function (value) {
  if (is.na(value) || length(value) == 0) { # when the first or out of bounds index is requested
    return(0)
  } else {
    return(value)
  }
}


# ============== Main ============== #
fuzzy_set_A_domain = c(0:100)/10
fuzzy_set_A_image = calculate_image(fuzzy_set_A_domain, 1.0, 3.5, 6.0) # a = 1.0, m = 3.5, b = 6.0

fuzzy_set_B_domain = c(0:100)/10
fuzzy_set_B_image  = calculate_image(fuzzy_set_B_domain, 3.0, 5.0, 7.0) # a = 3.0, m = 5.5, b = 7.0






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
# Soma limitada (Likasiewicz): min(1, x+y)
# União drástica: x se y=0; y se x=0; 1 c.c
#
################################################

#==================== R REMINDERS ====================
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
