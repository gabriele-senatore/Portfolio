# Rischio geopolitico, pandemia e attività economica globale: evidenze da un modello SVAR

**Corso**: Econometria · **Anno**: 2025-2026
**Autore**: Gabriele Senatore

## Obiettivo

Analizzare l'impatto degli shock di rischio geopolitico sulle principali variabili macroeconomiche globali (PIL, inflazione, fiducia dei consumatori, prezzo del petrolio) attraverso un modello SVAR a 5 variabili, confrontando i risultati con e senza il periodo COVID-19.

## Dati

- **Periodo**: 1973-01 – 2023-12, dati mensili
- **Fonti**: Geopolitical Risk Index — GPR, GPR Acts, GPR Threats (Caldara & Iacoviello); PIL mondiale PPP, inflazione mondiale, indice di fiducia dei consumatori OCSE (CCI), prezzo reale del petrolio (WTI)
- File dati inclusi: [`vardata_replication.csv`](./vardata_replication.csv) (dataset compilato per la replica), [`data_gpr_export.xls`](./data_gpr_export.xls) (estrazione GPR)

## Metodologia

- Modello VAR in forma ridotta → modello SVAR, identificazione tramite decomposizione di Cholesky
- Ordinamento strutturale: GPR (Acts/Threats) → prezzo petrolio → fiducia consumatori → PIL mondiale → inflazione
- Tre specificazioni alternative della variabile d'impulso: GPR aggregato, GPR Threats, GPR Acts
- Analisi delle Impulse Response Functions (IRF) su un orizzonte di 24 mesi, con bande di confidenza al 68% e 90%
- Forecast Error Variance Decomposition (FEVD) per quantificare il contributo di ciascuna variabile
- Confronto sistematico tra campione completo e campione che esclude il periodo COVID-19

## Risultati principali

- Uno shock di rischio geopolitico genera un pattern coerente con la letteratura: calo della fiducia dei consumatori, contrazione del PIL mondiale, aumento persistente dell'inflazione
- Il pattern qualitativo è robusto sia con sia senza il periodo COVID, ma l'**ampiezza** dell'effetto cambia molto: nel campione completo l'effetto recessivo sembra guidato quasi interamente dalle "Threats", mentre escludendo il 2020 emergono anche gli "Acts" come componente significativa
- La FEVD mostra che il rischio geopolitico spiega solo una quota contenuta (generalmente sotto il 5%) della varianza delle variabili macro, agendo soprattutto in modo indiretto: tramite la fiducia dei consumatori verso il PIL, e tramite il petrolio verso l'inflazione
- L'inclusione del periodo COVID amplifica artificialmente l'interconnessione tra fiducia, petrolio e PIL, mascherando il legame strutturale di lungo periodo tra ciclo economico mondiale e prezzo del petrolio

## File

- [`report.pdf`](./report.pdf) — relazione completa del progetto
- [`script_svar_con_covid.R`](./script_svar_con_covid.R) — pipeline SVAR sul campione completo (incluso il periodo COVID)
- [`script_svar_senza_covid.R`](./script_svar_senza_covid.R) — stessa pipeline sul campione che esclude il periodo COVID (filtro `dates <= 2020-01-01`)
- [`vardata_replication.csv`](./vardata_replication.csv), [`data_gpr_export.xls`](./data_gpr_export.xls) — dati utilizzati

## Librerie R utilizzate

`tidyquant`, `tidyverse`, `lubridate`, `readxl`, `zoo`, `tseries`, `vars`, `ggplot2`, `dplyr`
