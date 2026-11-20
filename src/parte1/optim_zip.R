
# ME524 — Laboratório 2
# Estimação do modelo ZIP por máxima verossimilhança
# utilizando optim() com múltiplos pontos iniciais.


# Ajusta o modelo ZIP para um conjunto de observações.
ajustar_zip_optim <- function(y, p_inicial, lambda_inicial) {

  resumo <- resumir_contagens(y)

  # Transformar os valores iniciais para a escala irrestrita
  inicio <- c(
    alpha = qlogis(p_inicial),
    beta = log(lambda_inicial)
  )

  # Função objetivo: negativo da log-verossimilhança
  objetivo <- function(theta) {
    p <- plogis(theta[1])
    lambda <- exp(theta[2])

    -logver_zip(p, lambda, resumo)
  }

  # Minimização numérica
  ajuste <- optim(
    par = inicio,
    fn = objetivo,
    method = "BFGS",
    control = list(maxit = 1000)
  )

  # Resultados na escala original dos parâmetros
  data.frame(
    p_inicial = p_inicial,
    lambda_inicial = lambda_inicial,
    p = unname(plogis(ajuste$par[1])),
    lambda = unname(exp(ajuste$par[2])),
    logver = -ajuste$value,
    convergencia = ajuste$convergence,
    avaliacoes = unname(ajuste$counts[["function"]])
  )
}


# Repete a estimação utilizando diferentes pontos iniciais.
multiplos_inicios_zip <- function(y) {

  inicios <- expand.grid(
    p = c(0.1, 0.4, 0.7),
    lambda = c(0.5, 2, 5)
  )

  resultados <- lapply(seq_len(nrow(inicios)), function(i) {
    ajustar_zip_optim(
      y = y,
      p_inicial = inicios$p[i],
      lambda_inicial = inicios$lambda[i]
    )
  })

  do.call(rbind, resultados)
}
