
# Estatísticas suficientes para a log-verossimilhança ZIP
resumir_contagens <- function(y) {
  n0 <- sum(y == 0)

  list(
    n0 = n0,
    n_positivos = length(y) - n0,
    soma = sum(y)
  )
}

# Log-verossimilhança observada, sem constante
logver_zip <- function(p, lambda, resumo) {
  prob_zero <- p + (1 - p) * exp(-lambda)

  resumo$n0 * log(prob_zero) +
    resumo$n_positivos * log1p(-p) -
    resumo$n_positivos * lambda +
    resumo$soma * log(lambda)
}
