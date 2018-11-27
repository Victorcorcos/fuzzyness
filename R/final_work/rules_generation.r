reload <- function () {
  source('/Users/victor/Desktop/Fuzzy/R/final_work/rules_generation.r')
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
implication_lukasiewics <- function (x, y) { min(1, 1-x+y) }
implication_kleene <- function (x, y) { max(1-x, y) }
implication_reichenbach <- function (x, y) { 1 - x + x*y }
implication_zadeh <- function (x, y) { max(1-x, min(x, y)) }
implication_gaines <- function (x, y) { if(x <= y) return(1) else return(0) }
implication_godel <- function (x, y) { if (x <= y) return(1) else return(y) }
implication_goguen <- function (x, y) { if (x <= y) return(1) else return(y/x) }
implication_kliryuan <- function (x, y) { 1 - x + x^2 * y }


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

# =============== Print functions =============== #
print_fuzzy_sets <- function () {
  par(mfrow = c(3,3), mar = c(5, 3, 2, 2)+0.01)
  for(col in 1:ncol(global$linguistic_variable_sets)) {
    linguistic_variable = colnames(global$linguistic_variable_sets)[col]
    sets_domain = get_domain(linguistic_variable)
    fuzzy_sets = get_sets(linguistic_variable)
    for(index in 1:length(fuzzy_sets)) {
      if (index == 1) {
        plot(constantize(sets_domain), constantize(fuzzy_sets[index]), type = 'l', col = get_color(index), xlim = c(-100, 100), ylim = c(0, 1), main = linguistic_variable, xlab = 'Domain')
      } else {
       lines(constantize(sets_domain), constantize(fuzzy_sets[index]), type = 'l', col = get_color(index)) 
      }
    }
    legend('left', cex = 0.5, title = 'Linguistic Terms', fuzzy_sets)
  }

  # plot(V_domain,  V_Diminuindo, type = 'l', col = 'blue', xlim = c(-100, 100), ylim = c(0, 1), main = 'Variacao de vendas', xlab = 'Domain')
  # lines(V_domain, V_Estavel, type = 'l', col = 'red')
  # lines(V_domain, V_Aumentando, type = 'l', col = 'green')
  # legend('left', cex = 0.5, title = 'Linguistic Terms', c('Diminuindo','Estavel','Aumentando'), fill = c('blue', 'red', 'green'))
  # plot(R_domain,  R_Leve, type = 'l', col = 'blue', xlim = c(0, 100), ylim = c(0, 1), main = 'Recomendacao de Investimento', xlab = 'Domain')
  # lines(R_domain, R_Media, type = 'l', col = 'red')
  # lines(R_domain, R_Forte, type = 'l', col = 'green')
  # legend('left', cex = 0.5, title = 'Linguistic Terms', c('Leve','Media','Forte'), fill = c('blue', 'red', 'green'))
}

# =============== Inference Engine =============== #
get_membership_value <- function (value, set, domain) {
  index = match(c(value), domain)
  return(set[index])
}

# =============== Axiliar Functions =============== #
create_matrix <- function (inputs, labels) {
  matrix(inputs, ncol = length(labels), byrow = TRUE, dimnames = list(NULL, labels))
}

create_blank_matrix <- function (labels) {
  matrix(, ncol = length(labels), dimnames = list(NULL, labels))[-1,]
}

add_linguistic_variable <- function (linguistic_variable, sets) {
  sets_and_nas = rep(NA, global$max_granularity + 1) # +1 for the Domain
  for(index in 1:length(sets_and_nas)) {
    sets_and_nas[index] = sets[index]
  }

  if (length(global$linguistic_variable_sets) == 0) {
    global$linguistic_variable_sets = matrix(sets_and_nas, ncol = 1, dimnames = list(NULL, linguistic_variable))
  } else {
    global$linguistic_variable_sets = cbind(global$linguistic_variable_sets, sets_and_nas)
    colnames(global$linguistic_variable_sets)[ncol(global$linguistic_variable_sets)] = linguistic_variable
  }
}

constantize <- function (name) {
  eval(parse(text = name))
}

get_sets <- function(linguistic_variable) {
  na.omit(global$linguistic_variable_sets[,c(linguistic_variable)][-1])
}

get_domain <- function(linguistic_variable) {
  global$linguistic_variable_sets[,c(linguistic_variable)][1]
}

get_color <- function(number) {
  colors = c('blue', 'red', 'chartreuse3', 'chocolate3', 'blueviolet', 'gold3', 'khaki', 'navy', 'slateblue1')
  return(colors[number])
}

# =============== Rules Generation =============== #
generate_rules <- function () {
  rule_line_values = c()
  rule_line_sets = c()

  for(row in 1:nrow(dataset)) {
    for(col in 1:ncol(dataset)) {
      linguistic_variable = names(dataset[row, col])
      linguistic_sets = get_sets(linguistic_variable)
      linguistic_domain = get_domain(linguistic_variable)

      current_sets = c()
      current_mf_values = c()

      for(i in 1:length(linguistic_sets)) {
        current_sets = append(current_sets, c(linguistic_sets[i]))
        mf = get_membership_value(dataset[row, col], constantize(linguistic_sets[i]), constantize(linguistic_domain))
        current_mf_values = append(current_mf_values, mf)
      }

      max_membership_value = max(current_mf_values)
      max_fuzzy_set = current_sets[match(max_membership_value, current_mf_values)]

      rule_line_values = append(rule_line_values, max_membership_value)
      rule_line_sets = append(rule_line_sets, max_fuzzy_set)
    }
    add_rule_by_strength(rule_line_values, rule_line_sets)

    rule_line_values = c()
    rule_line_sets = c()
  }
}

# This function add rules if they have higher strength
# If the rule is added, TRUE is returned. Otherwise, FALSE is returned
add_rule_by_strength <- function (rule_line_values, rule_line_sets) {
  if (length(global$rules_values) == 0) {
    add_rule(rule_line_values, rule_line_sets)
  } else {
    for(row in 1:nrow(global$rules_sets)) {
      if (same_antecendent(row, rule_line_sets)) {
        if (prod(rule_line_values) > strength(row)) {
          remove_rule(row)
          add_rule(rule_line_values, rule_line_sets)
          return(TRUE)
        } else {
          return(FALSE)
        }
      }
    }
    add_rule(rule_line_values, rule_line_sets)
    return(TRUE)
  }
}

same_antecendent <- function (rule_sets_row, rule_line_sets) {
  database_rule_antecedent = global$rules_sets[rule_sets_row, -ncol(global$rules_sets)]
  new_rule_antecedent = rule_line_sets[-length(rule_line_sets)]
  all(database_rule_antecedent == new_rule_antecedent)
}

strength <- function (rule_values_row) {
  prod(global$rules_values[rule_values_row,])
}

add_rule <- function (rule_line_values, rule_line_sets) {
  global$rules_values = rbind(global$rules_values, rule_line_values)
  global$rules_sets = rbind(global$rules_sets, rule_line_sets)
}

remove_rule <- function (rule_row) {
  global$rules_values = global$rules_values[-rule_row,]
  global$rules_sets = global$rules_sets[-rule_row,]
}

# ====================================================

global = new.env()

global$linguistic_variable_sets = c() # THE MOST IMPORTANT GLOBAL VARIABLE!
global$max_granularity = 9
domain_precision = 1.0

# Rule Input 1 (Vendas)
V_domain = seq(-100, 100, domain_precision)
V_Diminuindo = sapply(V_domain, trapezoidal_membership_function, a = -100, m = -100, n = -50, b = 0)
V_Estavel = sapply(V_domain, triangular_membership_function,     a = -50,  m = 0,    b = 50)
V_Aumentando = sapply(V_domain, trapezoidal_membership_function, a = 0,    m = 50,   n = 100, b = 100)
add_linguistic_variable('Vendas', c('V_domain', 'V_Diminuindo', 'V_Estavel', 'V_Aumentando'))

# Can be more!!


# Rule Output (Recomendacao)
R_domain = seq(0, 100, domain_precision)
R_Leve = sapply(R_domain, triangular_membership_function,  a = 0,  m = 0,   b = 50)
R_Media = sapply(R_domain, triangular_membership_function, a = 0,  m = 50,  b = 100)
R_Forte = sapply(R_domain, triangular_membership_function, a = 50, m = 100, b = 100)
add_linguistic_variable('Recomendacao', c('R_domain', 'R_Leve', 'R_Media', 'R_Forte'))

#  Vendas   Recomendacao
#    10   |     100
#    20   |     60
#  -100   |     0
entries = c(10, 100, 20, 60, -100, 0)
labels = colnames(global$linguistic_variable_sets)
dataset = create_matrix(entries, labels)

global$rules_values = create_blank_matrix(labels)
global$rules_sets = create_blank_matrix(labels)

generate_rules()


# Real work data below...
#
# 49 colunas
# attribute_labels = c('ANO_NOTIF', 'Idade', 'SEXO', 'GESTANTE', 'RACA_COR', 'ESCOLARIDADE', 'ZONA_RESID', 'UF_NOTIF', 'UF_RESID', 'REGIAO_NOTIF', 'REGIAO_RESIDE', 'PAIS_RESIDE', 'SUSPDE', 'VAC_HEPA', 'VAC_HEPB', 'INSTITUCIONAL', 'AGR_ASS_HIV', 'AGR_ASS_DST', 'CON_SEXUAL', 'CON_DOMICILIAR', 'CON_OCUPACIONAL', 'MED_INJET', 'TATUA_PIERC', 'AC_MAT_BIOL', 'DR_INAL_CRACK', 'ACUNPUT', 'TRANSF_SAN', 'DR_INJETAV', 'TRAT_CIRUR', 'AGUA_AL_CONT', 'TR_DENTARIO', 'TRES_PARC_SEX', 'HEMOD', 'TRANSPLANTE', 'E_OUTRAS', 'R_ANTIHAVIGM', 'R_ANTIHBS', 'R_ANTIHDVIGM', 'R_HBSAG', 'R_HBEAG', 'R_ANTIHEVIGM', 'R_ANTIHBCIGM', 'R_ANTIHBE', 'R_ANTIHCV', 'R_ANTIHBCTOTAL', 'R_ANTIHDVTOTAL', 'R_HCVRNA', 'FONTE_INFEC', 'CF11')

# inputs = c(1,31,1,6,9,9,1,33,33,3,3,1,3.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,4.0,2.0,4.0,1.0,1.0,4.0,1.0,2.0,4.0,1.0,4.0,4.0,99.0,1,
# 1,30,2,6,4,9,1,33,33,3,3,1,1.0,3.0,9.0,9.0,9.0,9.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,2.0,2.0,4.0,1.0,4.0,4.0,4.0,4.0,2.0,3.0,4.0,4.0,99.0,1,
# 1,27,2,5,4,3,1,51,51,5,5,1,2.0,3.0,3.0,8.0,2.0,2.0,3.0,3.0,3.0,2.0,2.0,3.0,3.0,3.0,3.0,3.0,2.0,3.0,2.0,2.0,3.0,3.0,3.0,4.0,4.0,4.0,1.0,4.0,4.0,4.0,4.0,4.0,1.0,4.0,4.0,1.0,1,
# 1,57,1,6,3,1,1,52,52,5,5,1,2.0,3.0,3.0,8.0,9.0,9.0,9.0,9.0,9.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,2.0,3.0,2.0,3.0,9.0,3.0,3.0,4.0,4.0,4.0,1.0,4.0,4.0,1.0,4.0,4.0,1.0,4.0,4.0,99.0,1,
# 1,58,1,6,1,3,1,52,52,5,5,1,3.0,3.0,3.0,8.0,9.0,9.0,9.0,9.0,9.0,3.0,3.0,3.0,2.0,3.0,3.0,2.0,3.0,3.0,3.0,2.0,3.0,3.0,3.0,4.0,1.0,4.0,2.0,4.0,4.0,4.0,4.0,1.0,1.0,4.0,1.0,99.0,1,
# 1,32,2,5,1,2,1,42,42,4,4,1,2.0,3.0,3.0,8.0,2.0,2.0,2.0,3.0,3.0,1.0,2.0,3.0,3.0,3.0,3.0,3.0,2.0,3.0,2.0,2.0,3.0,3.0,3.0,4.0,2.0,4.0,1.0,4.0,4.0,4.0,4.0,2.0,4.0,4.0,4.0,99.0,1,
# 1,51,1,6,1,2,1,42,42,4,4,1,2.0,3.0,3.0,8.0,2.0,2.0,3.0,3.0,3.0,3.0,2.0,3.0,3.0,3.0,3.0,2.0,2.0,3.0,3.0,1.0,3.0,3.0,3.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,1.0,4.0,4.0,1.0,3.0,1,
# 1,22,2,9,1,6,1,42,42,4,4,1,3.0,3.0,3.0,8.0,9.0,9.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,2.0,3.0,3.0,3.0,3.0,4.0,4.0,4.0,2.0,4.0,4.0,4.0,4.0,1.0,4.0,4.0,2.0,1.0,1,
# 1,28,1,6,2,3,1,31,31,3,3,1,2.0,3.0,3.0,8.0,2.0,1.0,9.0,2.0,9.0,1.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,9.0,2.0,1.0,3.0,3.0,3.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,1.0,1,
# 1,6,1,6,2,10,1,31,31,3,3,1,1.0,9.0,1.0,8.0,2.0,2.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,9.0,3.0,3.0,3.0,3.0,3.0,1.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,99.0,1,
# 1,21,1,6,1,4,1,31,31,3,3,1,3.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,1.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,9.0,1,
# 1,13,1,6,1,3,1,31,31,3,3,1,3.0,9.0,1.0,9.0,9.0,9.0,3.0,3.0,3.0,3.0,3.0,9.0,3.0,3.0,3.0,3.0,3.0,9.0,2.0,3.0,3.0,3.0,9.0,1.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,9.0,1,
# 1,35,2,3,1,4,1,35,35,3,3,1,2.0,9.0,9.0,8.0,2.0,2.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,4.0,4.0,4.0,1.0,1.0,4.0,2.0,4.0,2.0,1.0,4.0,4.0,99.0,1,
# 1,16,1,6,1,5,1,26,26,2,2,1,1.0,3.0,1.0,7.0,2.0,2.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,2.0,1.0,1.0,3.0,3.0,3.0,3.0,1.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,11.0,1,
# 1,28,1,6,9,9,1,35,35,3,3,1,2.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,3.0,9.0,3.0,3.0,9.0,3.0,2.0,3.0,3.0,9.0,9.0,3.0,9.0,9.0,4.0,2.0,4.0,1.0,4.0,4.0,4.0,1.0,2.0,1.0,4.0,4.0,99.0,1,
# 1,7,2,6,1,2,1,23,23,2,2,1,1.0,3.0,1.0,2.0,2.0,2.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,1.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,99.0,1,
# 1,18,1,6,4,3,2,23,23,2,2,1,1.0,3.0,1.0,9.0,2.0,2.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,1.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,99.0,1,
# 1,7,1,6,4,3,1,23,23,2,2,1,1.0,3.0,1.0,2.0,2.0,2.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,1.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,99.0,1,
# 1,46,2,5,1,4,1,42,42,4,4,1,3.0,3.0,3.0,8.0,2.0,2.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,2.0,1.0,4.0,2.0,2.0,4.0,1.0,1.0,2.0,1.0,4.0,4.0,99.0,1,
# 1,67,1,6,9,9,1,33,33,3,3,1,2.0,3.0,3.0,8.0,2.0,2.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,2.0,3.0,3.0,3.0,3.0,3.0,3.0,2.0,2.0,4.0,1.0,4.0,4.0,2.0,4.0,2.0,4.0,4.0,4.0,8.0,1,
# 1,7,1,6,4,2,1,32,32,3,3,1,3.0,3.0,1.0,8.0,2.0,2.0,3.0,3.0,3.0,3.0,2.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,2.0,3.0,3.0,3.0,3.0,1.0,4.0,4.0,2.0,4.0,4.0,2.0,4.0,2.0,4.0,4.0,4.0,99.0,1,
# 1,20,1,6,4,1,2,15,15,1,1,1,3.0,9.0,9.0,9.0,2.0,2.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,7.0,1,
# 1,6,1,6,5,10,2,26,26,2,2,1,1.0,1.0,2.0,9.0,2.0,2.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,4.0,4.0,1.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,9.0,1,
# 1,42,1,6,2,2,1,35,35,3,3,1,1.0,3.0,3.0,9.0,9.0,9.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,9.0,2.0,3.0,3.0,3.0,9.0,4.0,2.0,4.0,2.0,2.0,4.0,2.0,1.0,2.0,1.0,4.0,4.0,1.0,1,
# 1,32,2,4,3,9,1,35,35,3,3,1,2.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,9.0,4.0,4.0,4.0,1.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,99.0,1,
# 1,54,1,6,1,0,1,29,29,2,2,1,3.0,9.0,9.0,9.0,2.0,2.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,2.0,1.0,4.0,4.0,4.0,4.0,4.0,4.0,4.0,1.0,4.0,4.0,99.0,1,
# 1,37,1,6,4,4,1,14,14,1,1,1,2.0,3.0,3.0,9.0,2.0,2.0,2.0,2.0,2.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,3.0,4.0,2.0,4.0,2.0,4.0,4.0,4.0,4.0,2.0,1.0,4.0,4.0,99.0,1)

# dataset = matrix(inputs, ncol = 49, byrow = TRUE, dimnames = list(NULL, attribute_labels))

### Hints
# * Create a rule matrix
# rule_domains <- matrix(c(10, 20, 30), ncol = 3, dimnames = list(c('Rules'), c('InputA', 'InputB', 'Output')))
# rule_labels  <- matrix(c('A_Baixo', 'B_Medio', 'O_Alto'), ncol = 3, dimnames = list(c('Rules'), c('InputA', 'InputB', 'Output')))
# rule_values  <- matrix(c(0.2, 0.23, 0.1), ncol = 3, dimnames = list(c('Rules'), c('InputA', 'InputB', 'Output')))
# rules
# Define uma função que dado uma entrada, cria uma row pra regra daquela entrada (Atualizando as 3 variaveis)
# Define uma função que deleta uma regra (Pelo numero) e deleta todos esses valores dessas variaveis auxiliares

# * Add column with its label
# 

# * Access elements
# => rule_values[1, c('Output')]

# * Access an row
# => rule_values[2,]

# * Remove the output column
# => rule_values[,-ncol(rule_values)]

# * Discover the Fuzzy Set
# rule_line[match(max(rule_line_values), rule_line_values)]

# * Access column names
# => colnames(rule_values)

# * Add row
# => rule_values = rbind(rule_values, c(4, 5, 6))

# * Delete rows
# => rule_values = rule_values[-2,]
# => rule_values = rule_values[c(-2:-6),]

# * Converting List
# do.call(rbind, list(1, 2)) # Matrix
# unlist(list(1, 2))         # Vector


# * List current variables
# ls()