calcGera <- function(Pot = PDisp, CVU = 250, Inflex = 0, dadosCMO = TabCMO) {
  mutate(dadosCMO, Geração = if_else(CMO >= CVU, Pot, Inflex)) # Se CMO é maior ou igual, gera PDisp, senão, gera inflexibilidade.
}
calcGF <- function(Pot = PDisp, CVU = 250, Inflex = 0, Arredonda = TRUE, dadosCMO = TabCMO) {
  resultado <- calcGera(PDisp, CVU, Inflex, dadosCMO) %>%
    mutate(GFtemp = CMO * Geração) %>% group_by(SSist) %>% summarise(GF = sum(GFtemp) / sum(CMO), GFsemPond = mean(Geração))
  if (Arredonda) {
    resultado <- mutate(resultado, GF = round(GF, 1), GFsemPond = round(GFsemPond, 1))    
  }
  resultado
}

calcK <- function(Pot = PDisp, CVU = 250, Inflex = 0, CalculaGF = TRUE, UsaPond = TRUE, 
                  ArredondaGF = TRUE, GF = 50, NhoraMesReal = TRUE, dadosCMO = TabCMO) { # NhoraMesReal: usa número de horas reais de cada mês 
  if (CalculaGF) {
    GFtib <- calcGF(Pot, CVU, Inflex, ArredondaGF, dadosCMO)
  } else {
    GFtib <- tibble(SSist = unique(dadosCMO$SSist), GF = GF, GFsemPond = GF)
  }
  calcGera(Pot, CVU, Inflex, dadosCMO) %>% left_join(GFtib) %>% 
    mutate(GFu = ifelse(UsaPond, GF, GFsemPond), PLD = pmax(pmin(CMO, PLDmax), PLDmin), 
           HorasMes = (NhoraMesReal * days_in_month(month(data)) * 24) + ((!NhoraMesReal) * 730), # Define número de horas a cada mês
           COPserie = (Geração - Inflex) * CVU * HorasMes, # Geração líquida vezes CVU vezes número de horas no mês. 
           CECserie = (GFu - Geração) * PLD * HorasMes, LACEserie = Geração * CMO * HorasMes) %>% 
    group_by(SSist) %>% 
    summarise(Potência = Pot, CVU = CVU, Pond = UsaPond, GF = mean(GFu), COP = mean(COPserie / GFu) / 8760 * 12, 
              CEC = mean(CECserie / GFu) / 8760 * 12, k = COP + CEC, COPano = sum(COPserie) / n_distinct(Série) / n_distinct(data) * 12, 
              CECano = sum(CECserie) / n_distinct(Série) / n_distinct(data) * 12, 
              LACE = mean(LACEserie / GFsemPond) / 8760 * 12, LACEano = sum(LACEserie) / n_distinct(Série) / n_distinct(data) * 12,
              COPpot = COPano / Pot / 1000, CECpot = CECano / Pot / 1000, LACEpot = LACEano / Pot / 1000)
}

CVaRsort <- function(x, probs = 0.5, UpperTail = TRUE) {
  names(probs) <- paste0(probs * 100, "%")
  sapply(probs, function(y) mean(sort(x, decreasing = UpperTail)[1:((1 - y) * length(x))]))
}

CalcCVaR <- function(x, probs = 0.5, UpperTail = TRUE) {
  # Based on http://www-iam.mathematik.hu-berlin.de/~romisch/SP01/Uryasev.pdf
  sapply(probs, function(prob) {
    if (!UpperTail) x <- -x
    VaR <- quantile(x, prob, type = 1)
    psi <- mean(x <= VaR)
    lambda <- (psi - prob) / (1 - prob)
    CVaRp <- mean(x[x > VaR])
    if (is.nan(CVaRp)) {
      lambda <- 1
      CVaRp <- 0
    }
    (lambda * VaR + (1 - lambda) * CVaRp) * 
      ifelse(UpperTail, 1, -1)
  })
}

