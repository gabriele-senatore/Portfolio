# Utilizzo di metodi ensemble per valutare l'efficacia di campagne marketing

**Corso**: Apprendimento Statistico (ASL) · **Anno**: 2025-2026
**Autore**: Gabriele Senatore — Corso di laurea in Scienze Statistiche per la Finanza, Università degli Studi di Salerno

## Obiettivo

Costruire un modello predittivo capace di identificare i clienti più propensi ad aderire a una campagna marketing, confrontando un albero decisionale singolo (CART) con tre tecniche di ensemble: Bagging, Random Forest e AdaBoost.

## Dataset

- **Fonte**: [Marketing Campaign — Kaggle](https://www.kaggle.com/datasets/rodsaldanha/arketingcampaign/data) (Rod Saldanha, 2021)
- 2212 osservazioni, 23 variabili dopo preprocessing e feature engineering
- Variabile target `Response` fortemente sbilanciata (85% No / 15% Sì)
- Il dataset **non è incluso nel repo** (fonte pubblica esterna): scaricalo dal link sopra e posizionalo in `dataset/marketing_campaign.csv` per eseguire lo script.

## Metodo

- Feature engineering: aggregazione delle campagne precedenti accettate, anzianità cliente, volume totale acquisti, tasso di conversione web
- Split 80/20 train/validation, tuning via 10-fold cross validation
- Valutazione con metriche robuste per classi sbilanciate: PR-AUC, MCC, F1-Score, oltre a Sensitivity/Specificity
- Procedura di **threshold tuning** (ottimizzazione della soglia decisionale via F1-Score) su Random Forest e AdaBoost

## Risultati principali

| Modello | AUC | AUC-PR | Sensitivity (soglia 0.5) |
|---|---|---|---|
| CART (pruned) | 0.780 | 0.479 | 0.409 |
| Bagging | 0.843 | 0.552 | 0.409 |
| Random Forest | 0.843 | 0.574 | 0.364 |
| AdaBoost | **0.881** | **0.634** | 0.455 |

Dopo l'ottimizzazione della soglia, il **Random Forest** (soglia 0.25) offre il miglior compromesso operativo per il business con una sensitività del 69.7%, mentre l'**AdaBoost** (soglia 0.41) resta il più affidabile nell'individuare i clienti realmente non interessati. Le variabili più rilevanti sono risultate `total_acc_cmp`, `Recency` e `Customer_Tenure`.

## File

- [`script.R`](./script.R) — pipeline completa: preprocessing, feature engineering, training dei quattro modelli, threshold tuning, variable importance
- [`report.pdf`](./report.pdf) — relazione completa del progetto

## Librerie R utilizzate

`tidyverse`, `caret`, `rpart` / `rpart.plot`, `ipred`, `randomForest`, `gbm`, `pROC`, `ROCit`, `PRROC`, `ggcorrplot`, `patchwork`, `vip`
