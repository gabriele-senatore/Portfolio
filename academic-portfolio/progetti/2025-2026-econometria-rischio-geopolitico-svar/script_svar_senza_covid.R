# =========================================================================
# VAR Geopolitical Risk - Script corretto
# Fix applicati rispetto alla versione originale:
#   1. na.omit() solo DOPO aver selezionato le colonne di interesse
#   2. RPZTEXP invece di PZTEXP (variabile corretta usata nel paper)
#   3. Trasformazione log + detrend lineare (come nel paper), non differenza prima
#   4. Aggiunta World GDP e World Inflation come variabili di controllo
#   5. Bande di confidenza al 90% (come nel paper) invece di 68%
#   6. Controllo esplicito su range di date e multicollinearità GPR/GPRA/GPRT
# =========================================================================

library(tidyquant)
library(tidyverse)
library(lubridate)
library(readxl)
library(zoo)
library(tseries)
library(vars)
library(ggplot2)
library(dplyr)

data_raw <- read.csv("vardata_replication.csv")

# -------------------------------------------------------------------------
# FIX 1: selezioniamo PRIMA le colonne, poi facciamo na.omit
# Così non perdiamo osservazioni a causa di NA in colonne che non usiamo
# -------------------------------------------------------------------------

df <- data_raw %>%
  dplyr::select(dates, GPR, GPRT, GPRA, LNGPR, LNGPRT, LNGPRA,
                CCI_OECDE, RPZTEXP,                    # FIX 2: RPZTEXP non PZTEXP
                MWRDPPP_GDP_DET, GFDWLDINF) %>%         # FIX 4: World GDP + World Inflation
  mutate(dates = as.Date(dates, format = "%m/%d/%Y")) %>%
  filter(dates <= "2020-01-01") %>%
  na.omit()

# CONTROLLO ESPLICITO: verifica sempre il campione effettivo dopo na.omit
cat("Range di date nel campione:\n")
print(range(df$dates))
cat("Numero di osservazioni:", nrow(df), "\n\n")
# Il paper usa 1974-01 a 2023-12 (600 osservazioni). Se il tuo numero è molto
# diverso, stai lavorando su un campione differente dal paper.

# -------------------------------------------------------------------------
# Plot esplorativi
# -------------------------------------------------------------------------
ggplot(df, aes(x = dates, y = GPR)) + geom_line() + labs(title = "GPR") + xlab('Data')
ggplot(df, aes(x = dates, y = GPRT)) + geom_line() + labs(title = "GPRT") + xlab('Data')
ggplot(df, aes(x = dates, y = GPRA)) + geom_line() + labs(title = "GPRA") + xlab('Data')
ggplot(df, aes(x = dates, y = MWRDPPP_GDP_DET)) + geom_line() + labs(title = "PIL Mondiale (PPP)") + xlab('Data')
ggplot(df, aes(x = dates, y = GFDWLDINF)) + geom_line() + labs(title = "Inflazione Mondiale") + xlab('Data')
ggplot(df, aes(x = dates, y = CCI_OECDE)) + geom_line() + labs(title = "Indice di fiducia dei consumatori (CCI)") + xlab('Data')
ggplot(df, aes(x = dates, y = RPZTEXP)) + geom_line() + labs(title = "Real oil price") + xlab('Data')

# -------------------------------------------------------------------------
# FIX 3: trasformazione corretta per RPZTEXP
# Nel paper questa variabile è in "log + detrend lineare", non differenza prima.
# Il detrend lineare rimuove solo il trend deterministico di lungo periodo,
# mantenendo la persistenza ciclica che la differenza prima distruggerebbe.
# -------------------------------------------------------------------------

adf.test(df$RPZTEXP)
# Se rifiuta la stazionarietà sui livelli, ricorda che il paper non usa
# differenza prima ma detrend lineare sul logaritmo. Replichiamo qui:

t_index <- seq_len(nrow(df))
log_rpztexp <- log(df$RPZTEXP)
trend_model <- lm(log_rpztexp ~ t_index)
detrended_log_rpztexp <- residuals(trend_model) * 100  # *100 come nel codice MATLAB originale

df$RPZTEXP_detrend <- detrended_log_rpztexp

ggplot(df, aes(x = dates, y = RPZTEXP_detrend)) +
  geom_line() +
  labs(title = "RPZTEXP: log + detrend lineare (replica trasformazione paper)")

# Verifica stazionarietà della serie trasformata
adf.test(df$RPZTEXP_detrend)

# -------------------------------------------------------------------------
# Controllo multicollinearità tra le tre varianti GPR
# -------------------------------------------------------------------------
cat("\nCorrelazione tra GPR, GPRA, GPRT:\n")
print(cor(df[, c("GPR", "GPRA", "GPRT")]))
# GPR è costruito come media di GPRA e GPRT: aspettati correlazioni alte.
# Per questo motivo i tre modelli vanno SEMPRE stimati separatamente
# (come fai correttamente), mai nello stesso VAR.


# =========================================================================
# MODELLO 4 (ROBUSTEZZA): GPR con World GDP e World Inflation
# Aggiunto per testare quanto cambiano le IRF includendo i canali macro
# principali del paper, che nei modelli 1-3 erano omessi
# =========================================================================

var_data_4 <- data.frame(
  GPR = df$LNGPR,              # 1. Esogena pura (Geopolitica)
  OIL = df$RPZTEXP_detrend,  # 2. Prezzo del Petrolio (Mercato Commodity)
  CCI = df$CCI_OECDE,        # 3. Canale aspettative / Fiducia
  GDP = df$MWRDPPP_GDP_DET,  # 4. Attività reale (PIL Mondiale)
  INF = df$GFDWLDINF         # 5. Prezzi (Inflazione Mondiale)
)

lag_selection_4 <- VARselect(var_data_4, lag.max = 24, type = "const")
print(lag_selection_4$selection)

var_model_4 <- VAR(var_data_4, p = 5, type = "const")
serial.test(var_model_4, lags.pt = 16, type = "PT.asymptotic")

# 1. Generiamo le IRF complete al 90%
irf_all_90 <- irf(var_model_4, impulse = "GPR", 
                  n.ahead = 24, ortho = TRUE, boot = TRUE, runs = 1000, ci = 0.90)

# 2. Generiamo le IRF complete al 68%
irf_all_68 <- irf(var_model_4, impulse = "GPR", 
                  n.ahead = 24, ortho = TRUE, boot = TRUE, runs = 1000, ci = 0.68)

# Funzione riscritta per calcolare il periodo basandosi sulle righe reali della matrice
build_df_variable <- function(obj_90, obj_68, var_name) {
  
  # Calcoliamo il numero di righe della matrice dei risultati (sarà pari a n.ahead + 1)
  n_steps <- nrow(obj_90$irf$GPR)
  
  data.frame(
    Periodo  = 0:(n_steps - 1),  # Crea la sequenza corretta da 0 a 24
    Variabile = var_name,
    Risposta = obj_90$irf$GPR[, var_name],
    Lower_90 = obj_90$Lower$GPR[, var_name],
    Upper_90 = obj_90$Upper$GPR[, var_name],
    Lower_68 = obj_68$Lower$GPR[, var_name],
    Upper_68 = obj_68$Upper$GPR[, var_name]
  )
}

# 4. Estraiamo i dataset per le singole variabili senza cicli for o apply
df_gdp <- build_df_variable(irf_all_90, irf_all_68, "GDP")
df_inf <- build_df_variable(irf_all_90, irf_all_68, "INF")
df_cci <- build_df_variable(irf_all_90, irf_all_68, "CCI")
df_oil <- build_df_variable(irf_all_90, irf_all_68, "OIL")

# 5. Uniamo i dati
df_plot_all <- rbind(df_gdp, df_inf, df_cci, df_oil)

# 6. Grafico a pannelli con ggplot2
ggplot(df_plot_all, aes(x = Periodo)) +
  geom_ribbon(aes(ymin = Lower_90, ymax = Upper_90), fill = "steelblue", alpha = 0.15) +
  geom_ribbon(aes(ymin = Lower_68, ymax = Upper_68), fill = "steelblue", alpha = 0.35) +
  geom_line(aes(y = Risposta), color = "darkblue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick1", size = 0.6) +
  facet_wrap(~ Variabile, scales = "free_y", ncol = 2) +
  labs(title = "Funzioni di Risposta all'Impulso (IRF) a uno Shock del GPR",
       subtitle = "Intervalli di confidenza al 68% e 90%",
       x = "Mesi successivi allo shock",
       y = "Risposta della variabile") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "gray30", hjust = 0.5),
    strip.text = element_text(face = "bold", size = 11),
    panel.spacing = unit(1.5, "lines")
  )

# =========================================================================
# MODELLO 5 (ROBUSTEZZA): GPR con World GDP e World Inflation
# Aggiunto per testare quanto cambiano le IRF includendo i canali macro
# principali del paper, che nei modelli 1-3 erano omessi
# =========================================================================

var_data_5 <- data.frame(
  GPRT = df$LNGPRT,              # 1. Esogena pura (Geopolitica)
  OIL = df$RPZTEXP_detrend,  # 2. Prezzo del Petrolio (Mercato Commodity)
  CCI = df$CCI_OECDE,        # 3. Canale aspettative / Fiducia
  GDP = df$MWRDPPP_GDP_DET,  # 4. Attività reale (PIL Mondiale)
  INF = df$GFDWLDINF         # 5. Prezzi (Inflazione Mondiale)
)

lag_selection_5 <- VARselect(var_data_5, lag.max = 12, type = "const")
print(lag_selection_5$selection)


var_model_5 <- VAR(var_data_5, p = 5, type = "const")
serial.test(var_model_5, lags.pt = 16, type = "PT.asymptotic")

# 1. Generiamo le IRF complete al 90%
irf_all_90 <- irf(var_model_5, impulse = "GPRT", 
                  n.ahead = 24, ortho = TRUE, boot = TRUE, runs = 1000, ci = 0.90)

# 2. Generiamo le IRF complete al 68%
irf_all_68 <- irf(var_model_5, impulse = "GPRT", 
                  n.ahead = 24, ortho = TRUE, boot = TRUE, runs = 1000, ci = 0.68)

# Funzione riscritta per calcolare il periodo basandosi sulle righe reali della matrice
build_df_variable <- function(obj_90, obj_68, var_name) {
  
  # Calcoliamo il numero di righe della matrice dei risultati (sarà pari a n.ahead + 1)
  n_steps <- nrow(obj_90$irf$GPR)
  
  data.frame(
    Periodo  = 0:(n_steps - 1),  # Crea la sequenza corretta da 0 a 24
    Variabile = var_name,
    Risposta = obj_90$irf$GPR[, var_name],
    Lower_90 = obj_90$Lower$GPR[, var_name],
    Upper_90 = obj_90$Upper$GPR[, var_name],
    Lower_68 = obj_68$Lower$GPR[, var_name],
    Upper_68 = obj_68$Upper$GPR[, var_name]
  )
}

# 4. Estraiamo i dataset per le singole variabili senza cicli for o apply
df_gdp <- build_df_variable(irf_all_90, irf_all_68, "GDP")
df_inf <- build_df_variable(irf_all_90, irf_all_68, "INF")
df_cci <- build_df_variable(irf_all_90, irf_all_68, "CCI")
df_oil <- build_df_variable(irf_all_90, irf_all_68, "OIL")

# 5. Uniamo i dati
df_plot_all <- rbind(df_gdp, df_inf, df_cci, df_oil)

# 6. Grafico a pannelli con ggplot2
ggplot(df_plot_all, aes(x = Periodo)) +
  geom_ribbon(aes(ymin = Lower_90, ymax = Upper_90), fill = "steelblue", alpha = 0.15) +
  geom_ribbon(aes(ymin = Lower_68, ymax = Upper_68), fill = "steelblue", alpha = 0.35) +
  geom_line(aes(y = Risposta), color = "darkblue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick1", size = 0.6) +
  facet_wrap(~ Variabile, scales = "free_y", ncol = 2) +
  labs(title = "Funzioni di Risposta all'Impulso (IRF) a uno Shock del GPR Threats",
       subtitle = "Intervalli di confidenza al 68% e 90%",
       x = "Mesi successivi allo shock",
       y = "Risposta della variabile") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "gray30", hjust = 0.5),
    strip.text = element_text(face = "bold", size = 11),
    panel.spacing = unit(1.5, "lines")
  )

# Confronta queste IRF con quelle del Modello 1 (senza GDP/Inflation):
# se la forma o l'ampiezza della risposta di CCI o FX a GPR cambia
# sostanzialmente, è la controprova diretta dell'omitted variable bias
# discusso: il modello a 3 variabili sta stimando uno shock "diverso".

var_data_6 <- data.frame(
  GPRA  = df$LNGPRA,
  OIL   = df$RPZTEXP_detrend,
  CCI  = df$CCI_OECDE,
  GDP  = df$MWRDPPP_GDP_DET,
  INF  = df$GFDWLDINF
)

lag_selection_6 <- VARselect(var_data_6, lag.max = 12, type = "const")
print(lag_selection_6$selection)

var_model_6 <- VAR(var_data_6, p = 5, type = "const")
serial.test(var_model_6, lags.pt = 16, type = "PT.asymptotic")

# 1. Generiamo le IRF complete al 90%
irf_all_90 <- irf(var_model_6, impulse = "GPRA", 
                  n.ahead = 24, ortho = TRUE, boot = TRUE, runs = 1000, ci = 0.90)

# 2. Generiamo le IRF complete al 68%
irf_all_68 <- irf(var_model_6, impulse = "GPRA", 
                  n.ahead = 24, ortho = TRUE, boot = TRUE, runs = 1000, ci = 0.68)

# Funzione riscritta per calcolare il periodo basandosi sulle righe reali della matrice
build_df_variable <- function(obj_90, obj_68, var_name) {
  
  # Calcoliamo il numero di righe della matrice dei risultati (sarà pari a n.ahead + 1)
  n_steps <- nrow(obj_90$irf$GPR)
  
  data.frame(
    Periodo  = 0:(n_steps - 1),  # Crea la sequenza corretta da 0 a 24
    Variabile = var_name,
    Risposta = obj_90$irf$GPR[, var_name],
    Lower_90 = obj_90$Lower$GPR[, var_name],
    Upper_90 = obj_90$Upper$GPR[, var_name],
    Lower_68 = obj_68$Lower$GPR[, var_name],
    Upper_68 = obj_68$Upper$GPR[, var_name]
  )
}

# 4. Estraiamo i dataset per le singole variabili senza cicli for o apply
df_gdp <- build_df_variable(irf_all_90, irf_all_68, "GDP")
df_inf <- build_df_variable(irf_all_90, irf_all_68, "INF")
df_cci <- build_df_variable(irf_all_90, irf_all_68, "CCI")
df_oil <- build_df_variable(irf_all_90, irf_all_68, "OIL")

# 5. Uniamo i dati
df_plot_all <- rbind(df_gdp, df_inf, df_cci, df_oil)

# 6. Grafico a pannelli con ggplot2
ggplot(df_plot_all, aes(x = Periodo)) +
  geom_ribbon(aes(ymin = Lower_90, ymax = Upper_90), fill = "steelblue", alpha = 0.15) +
  geom_ribbon(aes(ymin = Lower_68, ymax = Upper_68), fill = "steelblue", alpha = 0.35) +
  geom_line(aes(y = Risposta), color = "darkblue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick1", size = 0.6) +
  facet_wrap(~ Variabile, scales = "free_y", ncol = 2) +
  labs(title = "Funzioni di Risposta all'Impulso (IRF) a uno Shock del GPR Acts",
       subtitle = "Intervalli di confidenza al 68% e 90%",
       x = "Mesi successivi allo shock",
       y = "Risposta della variabile") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "gray30", hjust = 0.5),
    strip.text = element_text(face = "bold", size = 11),
    panel.spacing = unit(1.5, "lines")
  )



## Sezione Forecast Error Variance Decomposition

fevd_results <- fevd(var_model_5, n.ahead = 20)
horizons <- c(1, 4, 8, 20)
fevd_rgdp <- round(fevd_results$GDP[horizons,]*100,0)
fevd_cci <- round(fevd_results$CCI[horizons,]*100,0)
fevd_inf <- round(fevd_results$INF[horizons,]*100,0)
fevd_oil <- round(fevd_results$OIL[horizons,]*100, 0)

# Aggiungiamo una colonna che indica l'orizzonte temporale (i mesi ahead)
stampa_fevd <- function(fevd_matrix, nome_var) {
  df <- as.data.frame(fevd_matrix)
  df <- cbind(Mese = horizons, df)
  cat("\n--- FEVD per Variable:", nome_var, "---\n")
  print(df)
  return(df)
}

# Visualizza i risultati nella console di R
gdp_tab <- stampa_fevd(fevd_rgdp, "PIL Mondiale (GDP)")
inf_tab <- stampa_fevd(fevd_inf, "Inflazione (INF)")
cci_tab <- stampa_fevd(fevd_cci, "Fiducia Consumatori (CCI)")
oil_tab <- stampa_fevd(fevd_oil, "Prezzo Petrolio (OIL)")