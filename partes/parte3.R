# ME524 - Lab 2, Parte 3: Gibbs Sampler para o modelo ZIP
# Priori: p ~ Beta(1,1); lambda ~ Gamma(shape=1, rate=0.01)

# 1. Amostrador de Gibbs para UM perfil

gibbs_zip <- function(y, B = 10000L, burnin = 2000L,
                      p_inicial = 0.5, lambda_inicial = NULL) {
  if (!is.numeric(y) || length(y) == 0L || anyNA(y) ||
      any(y < 0) || any(y != floor(y))) {
    stop("y deve ser um vetor numerico nao vazio de contagens inteiras >= 0.")
  }
  if (B < 2L || burnin < 0L || burnin >= B) {
    stop("Use B >= 2 e 0 <= burnin < B.")
  }
  if (p_inicial <= 0 || p_inicial >= 1) {
    stop("p_inicial deve estar entre 0 e 1 (exclusivos).")
  }
  if (is.null(lambda_inicial)) lambda_inicial <- max(mean(y), 0.1)
  if (!is.finite(lambda_inicial) || lambda_inicial <= 0) {
    stop("lambda_inicial deve ser positivo e finito.")
  }
  
  n <- length(y)
  idx_zero <- which(y == 0)
  p <- numeric(B)
  lambda <- numeric(B)
  p[1] <- p_inicial
  lambda[1] <- lambda_inicial
  z <- integer(n)
  
  for (t in 2:B) {
    # (a) z | y, p, lambda: somente observacoes iguais a zero sao aleatorias
    z[] <- 0L
    if (length(idx_zero) > 0L) {
      # plogis(qlogis(p) + lambda) equivale a p/[p+(1-p)*exp(-lambda)]
      prob_estrutural <- plogis(qlogis(p[t - 1L]) + lambda[t - 1L])
      z[idx_zero] <- rbinom(length(idx_zero), size = 1L,
                            prob = prob_estrutural)
    }
    
    # (b) p | z ~ Beta(1+n1, 1+n0)
    n1 <- sum(z)
    n0 <- n - n1
    p[t] <- rbeta(1L, shape1 = 1 + n1, shape2 = 1 + n0)
    
    # (c) lambda | y,z ~ Gamma(1+soma das contagens no componente 0,
    #                          rate=0.01+n0)
    soma_poisson <- sum(y[z == 0L])
    lambda[t] <- rgamma(1L, shape = 1 + soma_poisson,
                        rate = 0.01 + n0)
  }
  
  idx <- (burnin + 1L):B
  posterior <- data.frame(p = p[idx], lambda = lambda[idx])
  resumo <- data.frame(
    parametro = c("p", "lambda"),
    media = vapply(posterior, mean, numeric(1)),
    mediana = vapply(posterior, median, numeric(1)),
    LI_95 = vapply(posterior,
                   function(v) unname(quantile(v, 0.025)), numeric(1)),
    LS_95 = vapply(posterior,
                   function(v) unname(quantile(v, 0.975)), numeric(1)),
    row.names = NULL
  )
  list(cadeias = data.frame(iteracao = 1:B, p = p, lambda = lambda),
       posterior = posterior, resumo = resumo,
       burnin = burnin)
}