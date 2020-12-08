calcGera <- function(Pot = PDisp, CVU = 250, Inflex = 0, dadosCMO = CMO) {
  mutate(dadosCMO, Geração = if_else(CMO >= CVU, Pot, Inflex)) # Se CMO é maior ou igual, gera PDisp, senão, gera inflexibilidade.
}
calcGF <- function(Pot = PDisp, CVU = 250, Inflex = 0, Arredonda = TRUE, dadosCMO = CMO) {
  resultado <- calcGera(PDisp, CVU, Inflex, dadosCMO) %>%
    mutate(GFtemp = CMO * Geração) %>% group_by(SSist) %>% summarise(GF = sum(GFtemp) / sum(CMO), GFsemPond = mean(Geração))
  if (Arredonda) {
    resultado <- mutate(resultado, GF = round(GF, 1), GFsemPond = round(GFsemPond, 1))    
  }
  resultado
}

calcK <- function(Pot = PDisp, CVU = 250, Inflex = 0, CalculaGF = TRUE, UsaPond = TRUE, 
                  ArredondaGF = TRUE, GF = 50, NhoraMesReal = TRUE, dadosCMO = CMO) { # NhoraMesReal: usa número de horas reais de cada mês 
  if (CalculaGF) {
    GFtib <- calcGF(Pot, CVU, Inflex, ArredondaGF)
  } else {
    GFtib <- tibble(SSist = c("SE", "S", "NE", "N"), GF = GF, GFsemPond = GF)
  }
  calcGera(Pot, CVU, Inflex) %>% left_join(GFtib) %>% 
    mutate(GFu = ifelse(UsaPond, GF, GFsemPond), PLD = pmax(pmin(CMO, PLDmax), PLDmin), 
           HorasMes = (NhoraMesReal * days_in_month(month(data)) * 24) + ((!NhoraMesReal) * 730), # Define número de horas a cada mês
           COPserie = (Geração - Inflex) * CVU * HorasMes, # Geração líquida vezes CVU vezes número de horas no mês. 
           CECserie = (GFu - Geração) * PLD * HorasMes) %>% group_by(SSist) %>% 
    summarise(Potência = Pot, CVU = CVU, Pond = UsaPond, GF = mean(GFu), COP = mean(COPserie / GFu) / 8760 * 12, 
              CEC = mean(CECserie / GFu) / 8760 * 12, k = COP + CEC, COPano = sum(COPserie) / n_distinct(Série) / n_distinct(data) * 12, 
              CECano = sum(CECserie) / n_distinct(Série) / n_distinct(data) * 12)
}
