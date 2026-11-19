# Algoritmo EM
em_zip <- function(
  y,
  p_inicial = 0.2,
  lambda_inicial = mean(y),
  tol_ll = 1e-10,
  tol_param = 1e-8,
  max_iter = 10000
) {
  
  resumo <- resumir_contagens(y)
  log_fatoriais <- sum(lgamma(y + 1))

  p <- p_inicial
  lambda <- lambda_inicial
  ll <- logver_zip(p, lambda, resumo) - log_fatoriais

  historico <- tibble::tibble(iteracao = 0, p = p, lambda = lambda, logLik = ll)
  convergiu <- FALSE

  for (iter in seq_len(max_iter)) {
    #passo E: calculo dos pesos dos zeros estruturais
    w <- dplyr::if_else(y == 0, p / (p + (1 - p) * exp(-lambda)), 0)

    #passo M: atualizacao de p e lambda
    p_novo <- mean(w)
    lambda_novo <- sum((1 - w) * y) / sum(1 - w)
    ll_nova <- logver_zip(p_novo, lambda_novo, resumo) - log_fatoriais

    historico <- dplyr::bind_rows(
      historico,
      tibble::tibble(iteracao = iter, p = p_novo, lambda = lambda_novo, logLik = ll_nova)
    )

    #criterio de convergencia
    convergiu <- abs(ll_nova - ll) <= tol_ll * (1 + abs(ll)) &&
      abs(p_novo - p) <= tol_param * (1 + abs(p)) &&
      abs(lambda_novo - lambda) <= tol_param * (1 + abs(lambda))

    p <- p_novo
    lambda <- lambda_novo
    ll <- ll_nova

    if (convergiu) break
  }

  if (!convergiu) {
    print("EM não convergiu")
  }

  list(
    p = p,
    lambda = lambda,
    logLik = ll,
    iteracoes = iter,
    convergiu = convergiu,
    historico = historico
  )
}
