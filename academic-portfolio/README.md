# Portfolio Progetti Accademici — Gabriele Senatore

Raccolta dei progetti svolti durante il corso di laurea in Scienze Statistiche per la Finanza, Università degli Studi di Salerno.

Ogni cartella in [`progetti/`](./progetti) contiene un progetto autonomo, con codice, report e una breve descrizione.

## Indice progetti

| Progetto | Corso | Anno | Argomento | Report |
|---|---|---|---|---|
| [Marketing campaign ensemble](./progetti/2025-2026-ASL-marketing-campaign-ensemble) | Apprendimento Statistico | 2025-2026 | CART, Bagging, Random Forest, AdaBoost su dati sbilanciati | [PDF](./progetti/2025-2026-ASL-marketing-campaign-ensemble/report.pdf) |

## Struttura di una cartella progetto

```
progetti/<anno>-<corso>-<nome-breve>/
├── README.md      # Sintesi: obiettivo, metodo, risultati
├── script.R        # (o script.py) codice sorgente
├── report.pdf       # relazione/paper del progetto
└── dataset/         # (opzionale, escluso da .gitignore se pesante o non ridistribuibile)
```

## Convenzioni

- **Nome cartella**: `AAAA-AAAA-nome-corso-argomento-breve`, tutto minuscolo, parole separate da trattini.
- **Un README per progetto**: 5-10 righe che spiegano obiettivo, metodo e risultato principale, così chi visita il repo capisce il contenuto senza aprire il PDF.
- **Nessun file temporaneo**: `.Rhistory`, `.RData`, cache di RStudio/VSCode non vengono versionati (vedi `.gitignore`).
- **Dataset**: se il dataset è di terze parti (es. Kaggle) o troppo pesante, non va incluso nel repo — si linka la fonte nel README del progetto.

## Come aggiungere un nuovo progetto

1. Crea la cartella `progetti/<anno>-<corso>-<argomento>/`
2. Copia dentro script/notebook e report finale
3. Scrivi un README breve seguendo il template sopra
4. Aggiungi la riga nella tabella indice qui sopra
