library(readxl)
library(lubridate)
library(tidyverse)
library(furrr)
NomeArquivo <- "CMO_LEN_A5-2021_rv1.xlsx"
# Lê CMO
TabCMO <- bind_rows("SE" = read_xlsx(NomeArquivo, "CMO_Sudeste", skip = 1),
                    "S" = read_xlsx(NomeArquivo, "CMO_Sul", skip = 1),
                    "NE" = read_xlsx(NomeArquivo, "CMO_Nordeste", skip = 1),
                    "N" = read_xlsx(NomeArquivo, "CMO_Norte", skip = 1),
                    .id = "SSist")
TabCMO <- rename(TabCMO, Série = CMO)
TabCMO <- pivot_longer(TabCMO, !any_of(c("Série", "SSist")), names_to = "datatexto", values_to = "CMO") %>% 
  mutate(data = parse_date(datatexto, "%b-%y", locale = locale("pt")), .after = datatexto)

# Limites de PLD
PLDmin <- 49.77
PLDmax <- 583.88
#Dados da usina (exceto CVU e inflex.)
PNom <- 100
TEIF <- 0
IP <- 0
FCmax <- 100

PDisp <- PNom * (FCmax / 100) * (1 - TEIF / 100) * (1 - IP / 100)
source("funções.R")

# Calcula para CVU de 100 a 300 R$/MWh, de 50 em 50, com inflexibilidade de 40 MW. 
map_dfr(c(100, 150, 200, 250, 300), ~calcK(Pot = PDisp, CVU = ., Inflex = 40)) %>% suppressMessages()

map_dfr(c(100, 150, 200, 250, 300), ~calcK(Pot = PDisp, CVU = ., Inflex = 0)) %>% 
  mutate(k / CVU)

TabCMOse <- TabCMO %>% filter(SSist == "SE")
plan(multisession, workers = 9)
#plan(sequential)
p <- future_map_dfr(seq(0, 1000, length.out = 1000), ~calcK(Pot = PDisp, CVU = ., Inflex = 0, dadosCMO = TabCMOse),
                    .progress = TRUE) %>% 
  suppressMessages() %>% filter(SSist == "SE")
ggplot(p, aes(x = CVU)) + 
  geom_line(aes(y = k), color = "orange") + 
  geom_line(aes(y = GF)) + 
  geom_line(aes(y = COP), color = "green") + 
  geom_line(aes(y = CEC), color = "yellow")

Pteto <- 365

ggplot(p, aes(x = CVU)) + 
  geom_line(aes(y = (Pteto - k) * GF * 8.760 / Potência), color = "orange") + 
  geom_line(aes(y = LACEpot- COPpot)) +
  labs(y = "Receita fixa anual máxima por kW para ser competitivo")

p %>% mutate(BENpot = LACEpot- COPpot, BENICBpot = (Pteto - k) * GF * 8.760 / Potência,
             DeltaBENpot = -(BENpot - lag(BENpot)) / (CVU - lag(CVU)),
             DeltaBENICBpot = -(BENICBpot - lag(BENICBpot)) / (CVU - lag(CVU))) %>% 
  pivot_longer(c(DeltaBENpot, DeltaBENICBpot), names_to = "Índice", values_to = "Valor") %>% 
  filter(CVU > 2) %>% 
  ggplot(aes(x = CVU)) + geom_line(aes(y = Valor, color = Índice)) +
  labs(y = "Redução da receita por kW para cada R$/MWh de aumento do CVU")

plan(sequential)

# CMO ---------------------------------------------------------------------
TabCMO %>% group_by(SSist) %>% summarise(VaR10 = quantile(CMO, 0.9))
TabCMO %>% mutate(Mês = month(data)) %>% group_by(SSist, Mês) %>% mutate(VaR10 = quantile(CMO, 0.9)) %>% 
  filter(CMO >= VaR10) %>% summarise(CVaR10 = mean(CMO))
TabCMO %>% mutate(Mês = month(data)) %>% group_by(SSist, Mês) %>% arrange(desc(CMO)) %>% 
  slice_head(prop = 0.1) %>% summarise(CVaR10 = mean(CMO))
TabCMO %>% mutate(Mês = month(data)) %>% group_by(SSist, Mês) %>% summarise(VaR10 = quantile(CMO, 0.9), CVaR10 = CVaRsort(CMO, 0.9))
TabCMO %>% mutate(Mês = month(data)) %>% group_by(SSist, Mês) %>% summarise(VaR10 = quantile(CMO, 0.9), CVaR10 = CalcCVaR(CMO, 0.9))

TabCMO %>% group_by(SSist) %>% summarise(mean(CMO))

ggplot(TabCMO, aes(x = SSist, y = CMO)) + geom_boxplot() + geom_violin(alpha = 0.5) + scale_y_log10()

