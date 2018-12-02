reload <- function () {
  source('/Users/victor/Desktop/Fuzzy/R/final_work/fuzzy_system.r')
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

# =============== Defuzzification Methods =============== #
maximum_center <- function () {
  max_value = max(global$aggregated)
  max_indexes = which(global$aggregated %in% c(max_value))
  first_index = max_indexes[1]
  last_index = tail(max_indexes, n = 1)

  consequent_domain = consequent_domain()
  first = consequent_domain[first_index]
  last = consequent_domain[last_index]
  return((first + last)/2)
}

area_center <- function () {
  if (sum(global$aggregated) == 0) return(0)
  consequent_domain = consequent_domain()
  sum(mapply(produto, global$aggregated, consequent_domain)) / sum(global$aggregated)
}

# =============== Membership Functions ===============
triangular <- function (x, a, m, b) {
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

trapezoidal <- function (x, a, m, n, b) {
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

gaussian <- function (x, m, o) { # m = 2, o = 30
  exp(-((x - m)^2)/(o^2))
}

# =============== Inference Engine =============== #

# Makes the whole inference process by a provided test sample
# The output is the value after the defuzification!

inference <- function (test_sample) {
  # Example of test_sample => c(8, -2)
  rule_precendents = precedent_matrix()
  rule_consequent  = consequent_vector()
  global$rule_inferences = c()

  # global$test_data # All inputs needed to test
  for(row in 1:nrow(rule_precendents)) {
    for(col in 1:ncol(rule_precendents)) {
      input = test_sample[col] # 20
      set = rule_precendents[row, col] # "V_Estavel"
      domain = get_domain(colnames(rule_precendents)[col]) # "V_domain"
      current_mf = get_membership_value(input, constantize(set), constantize(domain))

      # T-norm on all antecedents
      if (col == 1) {
        rule_strength = current_mf
      } else {
        rule_strength = global$tnorm(rule_strength, current_mf)
      }
    }
    output_set = constantize(rule_consequent[row])
    if (row == 1) {
      # Use the implication method
      aggregated = sapply(output_set, global$implication, y = rule_strength)
      global$rule_inferences = cbind(global$rule_inferences, aggregated)
    } else {
      # Aggregate the inferences by the S-norm
      new_consequent = sapply(output_set, global$implication, y = rule_strength)
      global$rule_inferences = cbind(global$rule_inferences, new_consequent)
      aggregated = mapply(global$snorm, aggregated, new_consequent)
    }
  }

  global$aggregated = aggregated

  return(global$defuzification())
}

precedent_matrix <- function () {
  precedents = global$rules[,-ncol(global$rules)]

  if (is.null(dim(precedents))) { # Needs to convert to matrix if R converts to a vector
    precedents = matrix(data=precedents, nrow=length(precedents), dimnames=list(NULL, colnames(global$dataset)[1]))
  }

  return(precedents)
}

consequent_vector <- function () {
  global$rules[,ncol(global$rules)]
}

get_membership_value <- function (value, set, domain) {
  index = match(value, domain)
  return(set[index])
}

# =============== Rules Generation =============== #
generate_rules <- function () {
  rule_line_values = c()
  rule_line_sets = c()

  for(row in 1:nrow(global$training_data)) {
    for(col in 1:ncol(global$training_data)) {
      linguistic_variable = names(global$training_data[row, col])
      linguistic_sets = get_sets(linguistic_variable)
      linguistic_domain = get_domain(linguistic_variable)

      current_sets = c()
      current_mf_values = c()

      for(i in 1:length(linguistic_sets)) {

        current_sets = append(current_sets, c(linguistic_sets[i]))
        mf = get_membership_value(global$training_data[row, col], constantize(linguistic_sets[i]), constantize(linguistic_domain))
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
    for(row in 1:nrow(global$rules)) {
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
  database_rule_antecedent = global$rules[rule_sets_row, -ncol(global$rules)]
  new_rule_antecedent = rule_line_sets[-length(rule_line_sets)]
  all(database_rule_antecedent == new_rule_antecedent)
}

strength <- function (rule_values_row) {
  prod(global$rules_values[rule_values_row,])
}

add_rule <- function (rule_line_values, rule_line_sets) {
  global$rules_values = rbind(global$rules_values, rule_line_values)
  global$rules = rbind(global$rules, rule_line_sets)
}

remove_rule <- function (rule_row) {
  global$rules_values = global$rules_values[-rule_row,]
  global$rules = global$rules[-rule_row,]
}

initialize_dataset <- function () {
  # === Expected dataset entry (example) ===
  # Speed, Angle, Power
  # 1, -5, 0.3
  # 2, 5, 0.3
  # 3, -2, 0.5
  # 1, 2, 0.5
  # 2, 0, 0.7
  # 6, -5, 0.5
  # 7, 5, 0.5
  # 6, -2, 0.3
  # 7, 2, 0.3
  # 6, 0, 0.7
  # 8, -5, 0.5
  # 9, 5, 0.5
  # 10, -2, 0.3
  # 8, 2, 0.3
  # 9, 0, 0.5

  dataset <- read.csv('/Users/victor/Desktop/Fuzzy/R/final_work/qualidade_da_agua_bahia.csv', sep=',')
  dataset_matrix <- as.matrix(dataset)
  global$dataset = dataset_matrix

  labels = colnames(global$dataset)
  global$rules_values = create_blank_matrix(labels)
  global$rules = create_blank_matrix(labels)
}

# =============== Separate into Training and Test data =============== #
# training_percentage: The percentage of the dataset designed for training data
create_training_and_test <- function (training_percentage) {
  total = nrow(global$dataset)
  training = round(total * training_percentage)
  test = training + 1

  if (training == nrow(global$dataset)) {
    stop("There is no data for test! Please provide another percentage for training data.")
  } else if (training == 0) {
    stop("There is no data for training! Please provide another percentage for training data.")
  }

  global$training_data = global$dataset[1:training,]
  global$test_data = global$dataset[test:total,]

  # When there is just one element, R transforms the output to a vector instead of a matrix...
  # We NEED the matrix for further calculations, so I'm converting here!
  if (is.null(dim(global$training_data))) {
    global$training_data = matrix(data=global$training_data, ncol=length(global$training_data), dimnames=list(NULL, colnames(global$dataset)))
  }
  if (is.null(dim(global$test_data))) {
    global$test_data = matrix(data=global$test_data, ncol=length(global$test_data), dimnames=list(NULL, colnames(global$dataset)))
  }
}

# =============== Accuracy Function =============== #

library(FuzzyR) # Used just for accuracy measurement

measure_accuracy <- function () {
  global$aggregated = c()
  observeds = global$test_data[,ncol(global$test_data)]
  forecasteds = c()

  for(row in 1:nrow(global$test_data)) {
    forecasted = inference(global$test_data[,-ncol(global$test_data)][row,])
    forecasteds = append(forecasteds, forecasted)
  }

  global$newest_accuracy = accuracy(forecasteds, observeds)
  return(global$newest_accuracy)
}

# forecasting: A vector of forecasting values produced by the Fuzzy System inference.
# observed: A vector of observed values.
accuracy <- function (forecasting, observed) {
  fuzzyr.accuracy(forecasting, observed)
}

# =============== Auxiliar Functions =============== #
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
  number = (number %% 8) + 1
  colors = c('blue', 'red', 'chartreuse3', 'chocolate3', 'blueviolet', 'gold3', 'khaki', 'navy', 'slateblue1')
  return(colors[number])
}

consequent_domain <- function () {
  constantize(get_domain(colnames(global$dataset)[ncol(global$dataset)]))
}

add_new_accuracy <- function (tnorm, snorm, implication, defuzification) {
  if (is.null(global$accuracies)) {
    row_name = make.names(paste(tnorm, snorm, implication, defuzification))
    global$accuracies = matrix(nrow=1, ncol=length(global$newest_accuracy), dimnames=list(row_name, NULL))
  } else {
    global$accuracies = rbind(global$accuracies, global$newest_accuracy)
    newrow_name = make.names(paste(tnorm, snorm, implication, defuzification))
    rownames(global$accuracies)[length(rownames(global$accuracies))] = newrow_name
  }
}

sort_accuracies <- function () {
  global$accuracies = global$accuracies[order(global$accuracies[,1], decreasing=FALSE),]
}

get_set_names <- function () {
  if (global$fuzzy_sets_quantity == 3) {
    return(c('Pouco', 'Medio', 'Muito'))
  } else if (global$fuzzy_sets_quantity == 5) {
    return(c('MuitoPouco', 'Pouco', 'Medio', 'Muito', 'Muitissimo'))
  } else if (global$fuzzy_sets_quantity == 7) {
    return(c('ExtremamentePouco', 'MuitoPouco', 'Pouco', 'Medio', 'Muito', 'Muitissimo', 'ExtremamenteMuito'))
  } else if (global$fuzzy_sets_quantity == 9) {
    return(c('QuaseNada', 'ExtremamentePouco', 'MuitoPouco', 'Pouco', 'Medio', 'Muito', 'Muitissimo', 'ExtremamenteMuito', 'Completamente'))
  }
}

# =============== Print functions =============== #
print_fuzzy_sets <- function () {
  par(mfrow = c(3,3), mar = c(5, 3, 2, 2)+0.01)
  for(col in 1:ncol(global$linguistic_variable_sets)) {
    linguistic_variable = colnames(global$linguistic_variable_sets)[col]
    sets_domain = get_domain(linguistic_variable)
    fuzzy_sets = get_sets(linguistic_variable)
    for(index in 1:length(fuzzy_sets)) {
      domain = constantize(sets_domain)
      if (index == 1) {
        plot(domain, constantize(fuzzy_sets[index]), type = 'l', col = get_color(index), xlim = c(min(domain), max(domain)), ylim = c(0, 1), main = linguistic_variable, xlab = 'Domain')
      } else {
       lines(domain, constantize(fuzzy_sets[index]), type = 'l', col = get_color(index), xlim = c(min(domain), max(domain)))
      }
    }
    legend('left', cex = 0.5, title = 'Conjuntos Fuzzy', fuzzy_sets)
  }
}

print_aggregation <- function () {
  par(mfrow = c(4, 6), mar = c(5, 3, 2, 2))

  domain = consequent_domain()
  for(col in 1:ncol(global$rule_inferences)) {
    plot(global$rule_inferences[,col], ylim = c(0, 1), type = 'l', col = get_color(col), xlim = c(min(domain), max(domain)),  main = paste('Regra', col),   xlab = '', ylab = '')
  }
  plot(global$aggregated, ylim = c(0, 1), type = 'l', col = get_color(sample(1:8, 1)), xlim = c(min(domain), max(domain)), main = 'Agregacao', xlab = '', ylab = '')
}

# ============ Here starts the code after executing source() or reload() ===============

global = new.env()

global$max_granularity = 13
global$fuzzy_sets_quantity = 9
global$fuzzy_sets_function = 'triangular'

global$linguistic_variable_sets = c()

initialize_dataset()

# Assign Attribute_Domain and Attribute_FuzzySets to an Attribute
for(col in 1:ncol(global$dataset)) {
  attribute = colnames(global$dataset)[col]
  minimum = min(global$dataset[,col])
  maximum = max(global$dataset[,col])

  # Creating domain
  domain = paste(attribute, '_Domain', sep='')
  precision = 1
  assign(domain, seq(minimum, maximum, precision))

  # Creating Fuzzy Sets dynamically (The complex part)
  linguistic_terms = c(domain)
  set_names = get_set_names()

  if (global$fuzzy_sets_function == 'triangular' || global$fuzzy_sets_function == 'gaussian') {
    step = (maximum - minimum) / (length(set_names) - 1)
  } else if (global$fuzzy_sets_function == 'trapezoidal') {
    step = (maximum - minimum) / ((length(set_names)*2) - 1)
  }

  set_limit = minimum

  for(set_i in 1:length(set_names)) {
    mf_function = constantize(global$fuzzy_sets_function)
    attribute_set = paste(attribute, set_names[set_i], sep='_')
    domain_var = constantize(domain)
    linguistic_terms = append(linguistic_terms, attribute_set)

    if(global$fuzzy_sets_function == 'triangular') {
      if (set_i == 1) {
        set_values = sapply(domain_var, mf_function, a = minimum, m = minimum, b = minimum + step)
        assign(attribute_set, set_values)
      } else if (set_i == length(set_names)) {
        set_values = sapply(domain_var, mf_function, a = maximum - step, m = maximum, b = maximum)
        assign(attribute_set, set_values)
      } else {
        set_limit = set_limit + step
        set_values = sapply(domain_var, mf_function, a = set_limit - step, m = set_limit, b = set_limit + step)
        assign(attribute_set, set_values)
      }
    } else if (global$fuzzy_sets_function == 'trapezoidal') {
      if (set_i == 1) {
        set_values = sapply(domain_var, mf_function, a = minimum, m = minimum, n = minimum + step, b = minimum + 2*step)
        assign(attribute_set, set_values)
      } else if (set_i == length(set_names)) {
        set_values = sapply(domain_var, mf_function, a = maximum - 2*step, m = maximum - step, n = maximum, b = maximum)
        assign(attribute_set, set_values)
      } else {
        set_limit = set_limit + 2*step
        set_values = sapply(domain_var, mf_function, a = set_limit - step, m = set_limit, n = set_limit + step, b = set_limit + 2*step)
        assign(attribute_set, set_values)
      }
    } else if (global$fuzzy_sets_function == 'gaussian') {
      sigma = (step/length(set_names)) * 2
      if (set_i == 1) {
        set_values = sapply(domain_var, mf_function, m = minimum, o = sigma)
        assign(attribute_set, set_values)
      } else if (set_i == length(set_names)) {
        set_values = sapply(domain_var, mf_function, m = maximum, o = sigma)
        assign(attribute_set, set_values)
      } else {
        set_limit = set_limit + step
        set_values = sapply(domain_var, mf_function, m = set_limit, o = sigma)
        assign(attribute_set, set_values)
      }
    }
  }

  add_linguistic_variable(attribute, linguistic_terms)
}


# Above we have the dynamic method to create Fuzzy Sets and distribute them in the domain
# Below we have some example of simple creations of Fuzzy Sets. (In comments)


# # Rule Input 1 (Vendas)
# V_domain = seq(-100, 100, domain_precision)
# V_Diminuindo = sapply(V_domain, trapezoidal, a = -100, m = -100, n = -50, b = 0)
# V_Estavel = sapply(V_domain, triangular,     a = -50,  m = 0,    b = 50)
# V_Aumentando = sapply(V_domain, trapezoidal, a = 0,    m = 50,   n = 100, b = 100)
# add_linguistic_variable('Vendas', c('V_domain', 'V_Diminuindo', 'V_Estavel', 'V_Aumentando'))

# # Rule Input 2 (Servicos)
# S_domain = seq(0, 100, domain_precision)
# S_Baixo = sapply(S_domain, triangular, a = 0,  m = 0,   b = 50)
# S_Media = sapply(S_domain, triangular, a = 0,  m = 50,  b = 100)
# S_Alta = sapply(S_domain, triangular,  a = 50, m = 100, b = 100)
# add_linguistic_variable('Servicos', c('S_domain', 'S_Baixo', 'S_Media', 'S_Alta'))


# # Rule Output (Recomendacao)
# R_domain = seq(0, 100, domain_precision)
# R_Leve = sapply(R_domain, triangular,  a = 0,  m = 0,   b = 50)
# R_Media = sapply(R_domain, triangular, a = 0,  m = 50,  b = 100)
# R_Forte = sapply(R_domain, triangular, a = 50, m = 100, b = 100)
# add_linguistic_variable('Recomendacao', c('R_domain', 'R_Leve', 'R_Media', 'R_Forte'))

create_training_and_test(0.8) # 80% for training (Wang & Mendel), 20% for test (Inference and Accuracy measurement)

generate_rules() # Runs Wang & Mendel by the training samples

tnorms = c('minimo', 'produto', 'diferenca_limitada', 'interseccao_drastica')
snorms = c('maximo', 'soma_algebrica', 'soma_limitada', 'uniao_drastica')
implications = c('mamdani', 'larsen', 'lukasiewics', 'kleene', 'reichenbach', 'zadeh', 'gaines', 'godel', 'goguen', 'kliryuan')
defuzifications = c('maximum_center', 'area_center')

global$accuracies = c()

print('Generating results for...')

for (tnorm in tnorms) {
  for (snorm in snorms) {
    for (implication in implications) {
      for (defuzification in defuzifications) {
        global$tnorm = constantize(tnorm)
        global$snorm = constantize(snorm)
        global$implication = constantize(implication)
        global$defuzification = constantize(defuzification)
        measure_accuracy() # Makes the inference for all test samples and calculate the accuracy
        add_new_accuracy(tnorm, snorm, implication, defuzification)
        print(paste('T-norm: ', tnorm, 'S-norm: ', snorm, 'Implication: ', implication, 'Defuzification: ', defuzification))
      }
    }
  }
}

sort_accuracies()

print(global$accuracies) # Print the accuracies (tnorm, snorm, implication and defuzification)
