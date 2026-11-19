# Ajustes EM por perfil de risco
ajustar_em_por_perfil <- function(visitas_por_perfil) {
  ajustes <- lapply(visitas_por_perfil, em_zip)

  resumo <- purrr::map_dfr(ajustes, function(ajuste) {
    tibble::tibble(
      p = ajuste$p,
      lambda = ajuste$lambda,
      logLik = ajuste$logLik,
      iteracoes = ajuste$iteracoes,
      convergiu = ajuste$convergiu,
      menor_incremento = min(diff(ajuste$historico$logLik))
    )
  }, .id = "perfil")

  historico <- purrr::map_dfr(ajustes, function(ajuste) {
    ajuste$historico
  }, .id = "perfil")

  historico$perfil <- factor(historico$perfil, levels = names(visitas_por_perfil))

  list(
    resumo = resumo,
    historico = historico
  )
}
