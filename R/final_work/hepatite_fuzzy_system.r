# require(dplyr)
# require(purrr)

# Main functions
reload <- function () {
  source('/Users/victor/Desktop/Fuzzy/R/final_work/hepatite_fuzzy_system.r')
}

# =============== T-norms ===============
minimo <- function (x, y) { min(x, y) }
produto <- function (x, y) { x * y }
diferenca_limitada <- function (x, y) { max(0, (x + y - 1)) }
interseccao_drastica <- function (x, y) { if(x == 1) return(y) else if(y == 1) return(x) else return(0) }

# =============== S-norms ===============
maximo <- function (x, y) { if(x > y) return(x) else return(y) }
soma_algebrica <- function (x, y) { x + y - (x * y) }
soma_limitada <- function (x, y) { min(1, x+y) }
uniao_drastica <- function (x, y) { if(x == 0) return(y) else if(y == 0) return(x) else return(1) }

# =============== Implication operators ===============
mamdani <- function (x, y) { min(x, y) }
larsen <- function (x, y) { x * y }
lukasiewics <- function (x, y) { min(1, 1-x+y) }
kleene <- function (x, y) { max(1-x, y) }
reichenbach <- function (x, y) { 1 - x + x*y }
zadeh <- function (x, y) { max(1-x, min(x, y)) }
gaines <- function (x, y) { if(x <= y) return(1) else return(0) }
godel <- function (x, y) { if (x <= y) return(1) else return(y) }
goguen <- function (x, y) { if (x <= y) return(1) else return(y/x) }
kliryuan <- function (x, y) { 1 - x + x^2 * y }


# =============== Membership Functions ===============
triangular_membership_function <- function (x, a, m, b) {
  if (x == m && m == b) {
    y = 1
  } else if (x >= a && x < m) {
    y = (x - a) / (m - a)
  } else if (x >= m && x <= b) {
    y = (b - x) / (b - m)
  } else {
    y = 0
  }
  y
}

trapezoidal_membership_function <- function (x, a, m, n, b) {
  if (x == n && n == b) {
    y = 1
  } else if (x >= a && x < m) {
    y = (x - a) / (m - a)
  } else if(x >= m && x < n) {
    y = 1
  } else if(x >= n && x <= b) {
    y = (b - x) / (b - n)
  } else {
    y = 0
  }
  y
}

gaussian_membership_function <- function (x, m, o) { # m = 2, o = 30
  exp(-((x - m)^2)/(o^2))
}

# =============== Inference Engine =============== #
get_membership_value <- function (value, set, domain) {
  index = match(c(value), domain)
  return(set[index])
}

get_lowest_membership_value <- function (V_set, S_set, I_set) {
  V_mf_value = get_membership_value(input_v, V_set, V_domain)
  S_mf_value = get_membership_value(input_s, S_set, S_domain)
  I_mf_value = get_membership_value(input_i, I_set, I_domain)

  return(min(V_mf_value, S_mf_value, I_mf_value))
}

infer <- function (V_set, S_set, I_set, R_set) {
  minimum = get_lowest_membership_value(V_set, S_set, I_set)

  # Inferences :)
  relation_result = sapply(R_set, global$implication, y = minimum)
  return(relation_result)
}


# =============== Rule aggregation method =============== #
aggregate <- function (snorm) {
  partial = mapply(snorm, rule_1_result, rule_2_result)
  partial = mapply(snorm, partial, rule_3_result)
  partial = mapply(snorm, partial, rule_4_result)
  return(partial)
}


# =============== Defuzzification Methods =============== #
maximum_center <- function () {
  max_value = max(aggregation)
  max_indexes = which(aggregation %in% c(max_value))
  first_index = max_indexes[1]
  last_index = tail(max_indexes, n = 1)
  # Detalhe => first = first_index - 1
  # Detalhe => last = last_index - 1
  first = R_domain[first_index]
  last = R_domain[last_index]
  return((first + last)/2)
}

area_center <- function () {
  sum(mapply(produto, aggregation, R_domain)) / sum(aggregation)
}


# =============== Auxiliar Print function =============== #
print_dotted_lines <- function (domain, set, value) {
  index = match(c(value), domain)
  mf_value = set[index]
  cut_line = rep(mf_value, length(domain))
  lines(cut_line, lty = 2, col = 'red')
}


# =============== Print functions =============== #
print_fuzzy_sets <- function () {
  par(mfrow = c(3,3), mar = c(5, 3, 2, 2)+0.01)
  plot(V_domain,  V_setDiminuindo, type = 'l', col = 'blue', xlim = c(-100, 100), ylim = c(0, 1), main = 'Variacao de vendas', xlab = 'Domain')
  lines(V_domain, V_setEstavel, type = 'l', col = 'red')
  lines(V_domain, V_setAumentando, type = 'l', col = 'green')
  legend('left', cex = 0.5, title = 'Linguistic Terms', c('Diminuindo','Estavel','Aumentando'), fill = c('blue', 'red', 'green'))
  plot(S_domain,  S_setBaixo, type = 'l', col = 'blue', xlim = c(0, 100), ylim = c(0, 1), main = 'Sobrecarga de Servicos', xlab = 'Domain')
  lines(S_domain, S_setMedia, type = 'l', col = 'red')
  lines(S_domain, S_setAlta, type = 'l', col = 'green')
  legend('left', cex = 0.5, title = 'Linguistic Terms', c('Baixo','Media','Alta'), fill = c('blue', 'red', 'green'))
  plot(I_domain,  I_setRuim, type = 'l', col = 'blue', xlim = c(0, 100), ylim = c(0, 1), main = 'Nivel de Informatizacao', xlab = 'Domain')
  lines(I_domain, I_setMedio, type = 'l', col = 'red')
  lines(I_domain, I_setBom, type = 'l', col = 'green')
  legend('left', cex = 0.5, title = 'Linguistic Terms', c('Ruim','Medio','Bom'), fill = c('blue', 'red', 'green'))
  plot(R_domain,  R_setLeve, type = 'l', col = 'blue', xlim = c(0, 100), ylim = c(0, 1), main = 'Recomendacao de Investimento', xlab = 'Domain')
  lines(R_domain, R_setMedia, type = 'l', col = 'red')
  lines(R_domain, R_setForte, type = 'l', col = 'green')
  legend('left', cex = 0.5, title = 'Linguistic Terms', c('Leve','Media','Forte'), fill = c('blue', 'red', 'green'))
}

print_rules <- function () {
  par(mfrow = c(5, 5), mar = c(5, 3, 2, 2))
  plot(V_setAumentando, ylim = c(0, 1), type = 'l', col = 'orange', main = 'Vendas', xlab = 'Aumentando', ylab = 'Rule 1')
  print_dotted_lines(V_domain, V_setAumentando, input_v)
  plot(S_setAlta, ylim = c(0, 1), type = 'l', col = 'green', main = 'Servicos', xlab = 'Alta')
  print_dotted_lines(S_domain, S_setAlta, input_s)
  plot(I_setBom, ylim = c(0, 1), type = 'l', col = 'black', main = 'Informatizacao', xlab = 'Bom')
  print_dotted_lines(I_domain, I_setBom, input_i)
  plot(R_setForte, ylim = c(0, 1), type = 'l', col = 'purple', main = 'Investimento', xlab = 'Forte')
  plot(rule_1_result, ylim = c(0, 1), type = 'l', col = 'blue', main = 'Resultado', xlab = '')

  plot(V_setAumentando, ylim = c(0, 1), type = 'l', col = 'orange', main = 'Vendas', xlab = 'Aumentando', ylab = 'Rule 2')
  print_dotted_lines(V_domain, V_setAumentando, input_v)
  plot(S_setMedia, ylim = c(0, 1), type = 'l', col = 'green', main = 'Servicos', xlab = 'Media')
  print_dotted_lines(S_domain, S_setMedia, input_s)
  plot(I_setBom, ylim = c(0, 1), type = 'l', col = 'black', main = 'Informatizacao', xlab = 'Bom')
  print_dotted_lines(I_domain, I_setBom, input_i)
  plot(R_setMedia, ylim = c(0, 1), type = 'l', col = 'purple', main = 'Investimento', xlab = 'Media')
  plot(rule_2_result, ylim = c(0, 1), type = 'l', col = 'blue', main = 'Resultado', xlab = '')

  plot(V_setAumentando, ylim = c(0, 1), type = 'l', col = 'orange', main = 'Vendas', xlab = 'Aumentando', ylab = 'Rule 3')
  print_dotted_lines(V_domain, V_setAumentando, input_v)
  plot(S_setBaixo, ylim = c(0, 1), type = 'l', col = 'green', main = 'Servicos', xlab = 'Baixo')
  print_dotted_lines(S_domain, S_setBaixo, input_s)
  plot(I_setBom, ylim = c(0, 1), type = 'l', col = 'black', main = 'Informatizacao', xlab = 'Bom')
  print_dotted_lines(I_domain, I_setBom, input_i)
  plot(R_setLeve, ylim = c(0, 1), type = 'l', col = 'purple', main = 'Investimento', xlab = 'Leve')
  plot(rule_3_result, ylim = c(0, 1), type = 'l', col = 'blue', main = 'Resultado', xlab = '')

  plot(V_setAumentando, ylim = c(0, 1), type = 'l', col = 'orange', main = 'Vendas', xlab = 'Aumentando', ylab = 'Rule 4')
  print_dotted_lines(V_domain, V_setAumentando, input_v)
  plot(S_setMedia, ylim = c(0, 1), type = 'l', col = 'green', main = 'Servicos', xlab = 'Media')
  print_dotted_lines(S_domain, S_setMedia, input_s)
  plot(I_setRuim, ylim = c(0, 1), type = 'l', col = 'black', main = 'Informatizacao', xlab = 'Ruim')
  print_dotted_lines(I_domain, I_setRuim, input_i)
  plot(R_setForte, ylim = c(0, 1), type = 'l', col = 'purple', main = 'Investimento', xlab = 'Forte')
  plot(rule_4_result, ylim = c(0, 1), type = 'l', col = 'blue', main = 'Resultado', xlab = '')
}

print_aggregation <- function () {
  par(mfrow = c(3, 4), mar = c(5, 3, 2, 2))
  plot(rule_1_result, ylim = c(0, 1), type = 'l', col = 'blue',  main = 'Regra 1',   xlab = '', ylab = '')
  plot(rule_2_result, ylim = c(0, 1), type = 'l', col = 'green', main = 'Regra 2',   xlab = '', ylab = '')
  plot(rule_3_result, ylim = c(0, 1), type = 'l', col = 'black', main = 'Regra 3',   xlab = '', ylab = '')
  plot(rule_4_result, ylim = c(0, 1), type = 'l', col = 'red',   main = 'Regra 4',   xlab = '', ylab = '')
  plot(aggregation,   ylim = c(0, 1), type = 'l', col = 'red',   main = 'Agregacao', xlab = '', ylab = '')
}



# ============== Main ============== #
input_v = 55
input_s = 60
input_i = 85

precision = 1.0

global = new.env()
global$implication = mamdani

# Inputs
V_domain = seq(-100, 100, precision)
V_setDiminuindo = sapply(V_domain, trapezoidal_membership_function, a = -100, m = -100, n = -50, b = 0)
V_setEstavel = sapply(V_domain, triangular_membership_function,     a = -50,  m = 0,    b = 50)
V_setAumentando = sapply(V_domain, trapezoidal_membership_function, a = 0,    m = 50,   n = 100, b = 100)

S_domain = seq(0, 100, precision)
S_setBaixo = sapply(S_domain, triangular_membership_function, a = 0,  m = 0,   b = 50)
S_setMedia = sapply(S_domain, triangular_membership_function, a = 0,  m = 50,  b = 100)
S_setAlta = sapply(S_domain, triangular_membership_function,  a = 50, m = 100, b = 100)

I_domain = seq(0, 100, precision)
I_setRuim = sapply(I_domain, triangular_membership_function,  a = 0,  m = 0,   b = 50)
I_setMedio = sapply(I_domain, triangular_membership_function, a = 0,  m = 50,  b = 100)
I_setBom = sapply(I_domain, triangular_membership_function,   a = 50, m = 100, b = 100)


# Output
R_domain = seq(0, 100, precision)
R_setLeve = sapply(R_domain, triangular_membership_function,  a = 0,  m = 0,   b = 50)
R_setMedia = sapply(R_domain, triangular_membership_function, a = 0,  m = 50,  b = 100)
R_setForte = sapply(R_domain, triangular_membership_function, a = 50, m = 100, b = 100)


# Rules
# 1) Se V está aumentando e S é alta e I é bom então R é forte.
rule_1_result = infer(V_setAumentando, S_setAlta, I_setBom, R_setForte)
# 2) Se V está aumentando e S é média e I é bom então R é média.
rule_2_result = infer(V_setAumentando, S_setMedia, I_setBom, R_setMedia)
# 3) Se V está aumentando e S é baixa e I é bom então R é leve.
rule_3_result = infer(V_setAumentando, S_setBaixo, I_setBom, R_setLeve)
# 4) Se V está aumentando e S é média e I é ruim então R é forte.
rule_4_result = infer(V_setAumentando, S_setMedia, I_setRuim, R_setForte)


# Aggregation
aggregation = aggregate(maximo)

# Defuzificacao
centro_de_maximos = maximum_center()
centro_de_area = area_center()
cat('Metodos de Defuzificacao...\n')
cat('Centro de maximos:', centro_de_maximos, '\n')
cat('Centro de area:', centro_de_area, '\n')



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
