# Time Series Analysis — A.A. 2025/2026

Progetto d'esame di **Analisi delle Serie Storiche**, a cura di **Francesco P.** e **Gabriele S.**

Il progetto è composto da tre analisi univariate indipendenti, ciascuna delle quali segue lo stesso workflow metodologico: analisi esplorativa, decomposizione, verifica di stazionarietà, identificazione e stima di un modello SARIMA, diagnostica dei residui e confronto di previsori.

## Struttura del repository

```
2025-2026-time-series-analysis/
├── apple-google-trends/       # Ricerche Google per "Apple" (Gen 2004 – Ott 2025, mensile)
│   ├── DatiApple.csv
│   ├── TS-AnalisiApple-ITA.Rmd
│   └── TS-AnalisiApple-ITA.pdf
├── el-nino/                   # Anomalie mensili di temperatura globale, fenomeno El Niño (1980–2024)
│   ├── Nino.csv
│   ├── El_nino.Rmd
│   └── TS-El_Nino.pdf
└── nocollege-employment/      # Occupati USA senza formazione universitaria (2000 Q1 – 2025 Q4, trimestrale)
    ├── NoCollege.csv
    ├── TS-AnalisiNoCollege-ITA.Rmd
    └── TS-AnalisiNoCollege-ITA.pdf
```

Ogni cartella contiene:
- il **dataset** (`.csv`) utilizzato per l'analisi;
- il **codice sorgente** (`.Rmd`) con l'intero workflow in R;
- il **report finale** (`.pdf`) compilato, con grafici, output e commento dei risultati.

## Sintesi delle analisi

### 1. Apple — Google Trends
Serie mensile delle ricerche Google per "Apple" (2004–2025). Presenta trend crescente e forte stagionalità (picco a settembre, in coincidenza con gli eventi di lancio prodotti). Decomposizione additiva STL, stazionarietà raggiunta con log-trasformazione e differenziazione stagionale a lag 12. Modello selezionato: **SARIMA(1,0,1)(0,1,1)₁₂**. In fase di forecasting il previsore basato su decomposizione STL risulta il più accurato.

### 2. El Niño — Anomalie di temperatura
Serie mensile delle anomalie di temperatura globale legate al fenomeno El Niño (1980–2024, rispetto alla baseline 1991–2020). Trend crescente, assenza di stagionalità (tipica delle serie di anomalie). Stazionarietà ottenuta con differenziazione prima. Modello selezionato: **ARMA(2,1)**. Il modello **ARIMA(2,1,1)** risulta il previsore più accurato nel confronto finale.

### 3. NoCollege — Tasso di occupazione USA
Serie trimestrale del numero di occupati senza formazione universitaria negli USA (2000–2025). Presenta shock legati alla crisi 2008-2009 e alla pandemia COVID-19, stagionalità debole. Stazionarietà ottenuta con log-trasformazione e differenziazione prima. Modello selezionato: **SARMA(1,1)(1,0)₄**. In fase di forecasting il modello **SARIMA(1,1,1)(1,0,0)₄** risulta il più accurato.

## Strumenti utilizzati
R, con i pacchetti principali: `forecast`, `tseries`, `fpp3`/`fable`, `ggplot2`.
