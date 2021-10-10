library(readxl)
library(lubridate)
library(tidyverse)
library(furrr)
NomeArquivo <- "CMO_A4-2020.xlsx"
# Lê CMO
CMO <- bind_rows("SE" = read_xlsx(NomeArquivo, "CMO_Sudeste", skip = 1),
                 "S" = read_xlsx(NomeArquivo, "CMO_Sul", skip = 1),
                 "NE" = read_xlsx(NomeArquivo, "CMO_Nordeste", skip = 1),
                 "N" = read_xlsx(NomeArquivo, "CMO_Norte", skip = 1),
                 .id = "SSist")
CMO <- rename(CMO, Série = CMO)
CMO <- pivot_longer(CMO, !any_of(c("Série", "SSist")), names_to = "datatexto", values_to = "CMO") %>% 
  mutate(data = parse_date(datatexto, "%b-%y", locale = locale("pt")), .after = datatexto)

# Limites de PLD
PLDmin <- 39.68
PLDmax <- 559.75
#Dados da usina (exceto CVU e inflex.)
PNom <- 100
TEIF <- 0
IP <- 0
FCmax <- 100

PDisp <- PNom * (FCmax / 100) * (1 - TEIF / 100) * (1 - IP / 100)
source("funções.R")

# Calcula para CVU de 100 a 300 R$/MWh, de 50 em 50, com inflexibilidade de 40 MW. 
map_dfr(c(100, 150, 200, 250, 300), ~calcK(Pot = PDisp, CVU = ., Inflex = 40)) %>% suppressMessages()

#plan(multisession, workers = 6)
plan(sequential)
p <- future_map_dfr(seq(0, 1000, length.out = 500), ~calcK(Pot = PDisp, CVU = ., Inflex = 0)) %>% 
  suppressMessages() %>% filter(SSist == "SE")
ggplot(p, aes(x = CVU)) + 
  geom_line(aes(y = k), color = "orange") + 
  geom_line(aes(y = GF)) + 
  geom_line(aes(y = COP), color = "green") + 
  geom_line(aes(y = CEC), color = "yellow")
plan(sequential)
