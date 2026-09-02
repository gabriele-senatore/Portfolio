####################################################################
##  Progetto in ASL
##  Gabriele Senatore
##  Matricola: SE22400038
###################################################################


## Librerie necessarie
library(tidyverse)
library(corrplot)
library(caret)
library(rpart)
library(rpart.plot)
library(ipred)
library(randomForest)
library(ggcorrplot)
library(gbm)
library(pROC)
library(ROCit)
library(PRROC)
library(ggplot2) ## Grafici
library(patchwork) ## Combina più grafici

## Obiettivo del progetto
## Costruire un modello che sia capace di prevedere quali consumatori siano
## più propensi ad aderire ad una campagna marketing. 

# ================================
# DESCRIZIONE DEL DATASET
# ================================
# Numero osservazioni: 2212 (dopo il preprocessing e controllo missing values)
# Numero variabili: 29
# ================================
# 
# Descrizione delle variabili
# AcceptedCmp1 - 1 se il cliente ha accettato l'offerta nella 1ª campagna, 0 altrimenti
# AcceptedCmp2 - 1 se il cliente ha accettato l'offerta nella 2ª campagna, 0 altrimenti
# AcceptedCmp3 - 1 se il cliente ha accettato l'offerta nella 3ª campagna, 0 altrimenti
# AcceptedCmp4 - 1 se il cliente ha accettato l'offerta nella 4ª campagna, 0 altrimenti
# AcceptedCmp5 - 1 se il cliente ha accettato l'offerta nella 5ª campagna, 0 altrimenti
# Response (target) - 1 se il cliente ha accettato l'offerta nell'ultima campagna, 0 altrimenti
# Complain - 1 se il cliente ha sporto reclamo negli ultimi 2 anni
# DtCustomer - Data di iscrizione del cliente presso l'azienda
# Education - Livello di istruzione del cliente
# Marital - Stato civile del cliente
# Kidhome - Numero di bambini piccoli nel nucleo familiare
# Teenhome - Numero di adolescenti nel nucleo familiare
# Income - Reddito annuo familiare del cliente
# MntFishProducts - Importo speso in prodotti ittici negli ultimi 2 anni
# MntMeatProducts - Importo speso in prodotti a base di carne negli ultimi 2 anni
# MntFruits - Importo speso in prodotti ortofrutticoli negli ultimi 2 anni
# MntSweetProducts - Importo speso in prodotti dolciari negli ultimi 2 anni
# MntWines - Importo speso in prodotti vinicoli negli ultimi 2 anni
# MntGoldProds - Importo speso in prodotti "gold" negli ultimi 2 anni
# NumDealsPurchases - Numero di acquisti effettuati con sconti
# NumCatalogPurchases - Numero di acquisti effettuati tramite catalogo
# NumStorePurchases - Numero di acquisti effettuati direttamente in negozio
# NumWebPurchases - Numero di acquisti effettuati tramite sito web
# NumWebVisitsMonth - Numero di visite al sito web nell'ultimo mese
# Recency - Numero di giorni trascorsi dall'ultimo acquisto

dat <- read_delim("dataset/marketing_campaign.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

## Controllo missing values

sum(is.na(dat))
dat <- na.omit(dat)

## Pre processing dei dati

dat <- dat %>% 
  filter(!Marital_Status %in% c("YOLO", "Absurd"))

# Feature engineering dei dati

# Trasformo 5 variabili che indicano dopo quante campagne il cliente
# ha accettato l'offerta, in una singola variabile che indica quanto il
# cliente risulta essere attivo rispetto alle campagne in generale. 

## Mi aspetto che un cliente che abbia un total_acc_cmp >0 sia più 
## propenso ad accettare un'offerta nell'ultima campagna

dat <- dat %>%
  mutate(total_acc_cmp = AcceptedCmp1 + AcceptedCmp2 + AcceptedCmp3 + AcceptedCmp4 + AcceptedCmp5)

## Dt_customer trasformo fino ad oggi in giorni, per avere un valore dell'anzianità. 

data_rif <- as.Date("2026-01-27") 

dat <- dat %>%
  mutate(Customer_Tenure = as.numeric(difftime(data_rif, Dt_Customer, units = "days")))

## Total purchase

dat <- dat %>%
  mutate(Total_Purchases = NumWebPurchases + NumCatalogPurchases + NumStorePurchases)

## Tasso di conversione dalle visite Web

dat <- dat %>%
  mutate(Web_Conversion_Rate = ifelse(NumWebVisitsMonth > 0, 
                                      NumWebPurchases / NumWebVisitsMonth, 
                                      0))

## Ricodifico le variabili

dat <- dat %>% mutate(Education = as_factor(Education),
                      Marital_Status = droplevels(as_factor(Marital_Status)),
                      Complain = as_factor(Complain), 
                      Response = as_factor(Response))

dat <- dat %>% select(-Z_CostContact, -Z_Revenue, -ID, -Year_Birth, -Dt_Customer,
                      -AcceptedCmp1,-AcceptedCmp2, -AcceptedCmp3, -AcceptedCmp4,
                      -AcceptedCmp5)

levels(dat$Response) <- c('No', 'Si')
str(dat)

## Vediamo com'è distribuita la variabile target (table 2 del progetto)

prop.table(table(dat$Response))

## Ho uno sbilanciamento 85-15

## Vedo graficamente le correlazioni

# Selezioniamo solo le variabili numeriche
# Escludiamo date che non servono per la correlazione
df_num <- dat %>% select(where(is.numeric))

# Calcoliamo la matrice di correlazione
corr_matrix <- cor(df_num, use = "complete.obs")

## Grafico che non ho inserito nel progetto
ggcorrplot(corr_matrix, 
           hc.order = TRUE,   # Ordina le variabili simili vicine
           type = "lower",    # Mostra solo metà triangolo (più leggibile)
           lab = TRUE,        # Mostra i numeri (se non sono troppe variabili)
           lab_size = 3,
           method = "circle", 
           colors = c("#E46726", "white", "#6D9EC1"), 
           title = "Matrice di Correlazione")

## Non sembra esserci nessun problema di multicollinearità tra le variabili
## in esame. Alcune variabili come recency e customer_tenure hanno un legame
## variabile debole con le altre.

# Per adaboost (si veda dopo)
dat$Response01 <- ifelse(dat$Response=="No", 0, 1)  

## Classificazione
## Split dati in training e test

set.seed(777)
u <- createDataPartition(dat$Response, p=0.8, times= 1, list= TRUE)
idx <- u$Resample1
dat_train <- dat[idx,]
dat_val <- dat[-idx,]

m0 <- formula('Response ~ . -Response01')
mob <- formula('Response01 ~ . -Response')
## Imposto il train control
## Imposto una repeated kfold cross validation per avere una stima
## accurata dell'errore.


set_cv <- trainControl(method = "cv", 
                       number = 10, 
                       classProbs = TRUE,
                       savePredictions = "all")

# CART Semplice

# Combinazioni del parametro di cost complexity, prendo il valore
# che massimizza l'accuracy e poi faccio un plot dell'albero, con
# il valore di cost complexity ottimale. 

set.seed(1234)

cp_grid <- expand.grid(cp = seq(0, 0.2, length = 50))

fit_0 <- train(m0,
               data = dat_train, 
               trControl = set_cv,
               tuneGrid = cp_grid,              
               method = "rpart")

## Il valore di cost complexity scelto è:

cp_best <-  as.numeric(fit_0$bestTune)
cp_best

plot(fit_0)

## Calcoliamo errore di previsione

pred_0 <- predict(fit_0, newdata = dat_val, type = "raw") ## attenzione a type
err_cart_semplice <- mean(pred_0 != dat_val$Response )

## Calcoliamo valore AUC

pred_tree_prob <- predict(fit_0, newdata = dat_val, type = 'prob')
roc_obj_tree <- roc(dat_val$Response, pred_tree_prob$Si, levels = c("No", "Si"))
auc_cart <- auc(roc_obj_tree)
auc_cart

# Plot albero finale potato
# pruned_tree <- prune(fit_tree_completo, cp = cp_best)  

## Grafico albero cart potato,
# rpart.plot(pruned_tree)
rpart.plot(fit_0$finalModel)

## Matrice di confusione cart
matrice_cart <- confusionMatrix(pred_0, dat_val$Response, positive = 'Si')
matrice_cart$table


## Bagging, in cross validation
## -----------------------------------------------------------------------------
set.seed(5555)

fit_bag <- train(m0,
                data = dat_train,
                method = "treebag", # Il modello si chiama treebag. 
                keepX = TRUE, # Tieni le x ogni volta
                importance = TRUE, # Importance
                trControl = set_cv) 


## Errore calcolato sul validation set

pred <- predict(fit_bag, newdata = dat_val, type = "raw")
err_bagging  <- mean( pred != dat_val$Response )

## Matrice di confusione 

matrice_bagging <- confusionMatrix(pred, dat_val$Response, positive = 'Si')

tpr <- matrice_bagging$byClass["Sensitivity"]  
precision <- matrice_bagging$byClass["Precision"]
f1 <- matrice_bagging$byClass["F1"]

prob_pred <- predict(fit_bag, dat_val, type = "prob")

# Creo l'oggetto roc (usiamo la colonna 'Si')
roc_obj <- roc(dat_val$Response, prob_pred$Si, levels = c("No", "Si"))

# Estraggo il valore AUC
auc_val_bagging <- auc(roc_obj)
cat("Valore AUC:", auc_val_bagging)

performance_report_bagging <- data.frame(
  Metrica = c("Accuracy", "Sensitivity (TPR)", "Specificity", "Precision", "F1-Score", "AUC"),
  Valore = c(
    matrice_bagging$overall["Accuracy"],
    matrice_bagging$byClass["Sensitivity"],
    matrice_bagging$byClass["Specificity"],
    matrice_bagging$byClass["Precision"],
    matrice_bagging$byClass["F1"],
    auc_val_bagging
  )
)

performance_report_bagging

## Random Forests 
## -------------------------------------

## tuning
## -----
## ntree: corrisponde a B (numero di trees)
## mtry:  corrisponde q = numero di variabili campionate in ogni split.


## fissiamo una griglia di tunings 
tunegrid <- expand.grid(mtry  = c(4,5,6))

## Fissiamo più valori di q, cioè di variabili scelte a caso.

set.seed(1277272)
fit_rf  <- train(m0,
                data = dat_train,
                method='rf', 
                tuneGrid=tunegrid, 
                trControl=set_cv)
fit_rf

## Figura 4 del progetto
plot(fit_rf)
## --------------------

## Numero di alberi
fit_rf$finalModel

## errore previsione sul validation set 
pred_rf <- predict(fit_rf, newdata = dat_val, type = "raw")
err_rf  <- mean( pred_rf != dat_val$Response )
err_rf

matrice_random_forest <- confusionMatrix(pred_rf, dat_val$Response, positive = 'Si')

prob_pred_rf <- predict(fit_rf, dat_val, type = "prob")

# Creo l'oggetto roc (usiamo la colonna 'Si')
roc_obj_rf <- roc(dat_val$Response, prob_pred$Si, levels = c("No", "Si"))

# Estraggo il valore AUC
auc_val_rf <- auc(roc_obj)
cat("Valore AUC:", auc_val_rf)

performance_report_rf <- data.frame(
  Metrica = c("Accuracy", "Sensitivity (TPR)", "Specificity", "Precision", "F1-Score", "AUC"),
  Valore = c(
    matrice_random_forest$overall["Accuracy"],
    matrice_random_forest$byClass["Sensitivity"],
    matrice_random_forest$byClass["Specificity"],
    matrice_random_forest$byClass["Precision"],
    matrice_random_forest$byClass["F1"],
    auc_val_rf
  ))


performance_report_rf


## Adaboost
## ------------------------------------------------------------------------------

set.seed(579)
fit_ada <-  gbm(mob,
              data = dat_train,
              distribution = "adaboost",
              n.trees = 400,
              cv.folds = 20) ## k per la KFCV per stimare l'errore      

fit_ada

# Figura 3 progetto ------------------------------------------------------------

gbm.perf(fit_ada)

## ---------------------------------------------------------------------------
best_ntrees <- gbm.perf(fit_ada)


pred_ada <-  predict.gbm(object = fit_ada,
                     newdata = dat_val,
                     n.trees = best_ntrees,
                     type = "response")

pred_class <- ifelse(pred_ada>=0.5, 1, 0)

# Ricodifichiamo dato cha abbiamo i posterior. 

## errore sull'external validation set 
err_adaboost <- mean( pred_class != dat_val$Response01 )
err_adaboost 

matrice_adaboost <- confusionMatrix(as_factor(pred_class), 
                                    as_factor(dat_val$Response01),
                                    positive = '1')
matrice_adaboost


auc_ada <- rocit(pred_ada, dat_val$Response01)

performance_report_ada <- data.frame(
  Metrica = c("Accuracy", "Sensitivity (TPR)", "Specificity", "Precision", "F1-Score", "AUC"),
  Valore = c(
    matrice_adaboost$overall["Accuracy"],
    matrice_adaboost$byClass["Sensitivity"],
    matrice_adaboost$byClass["Specificity"],
    matrice_adaboost$byClass["Precision"],
    matrice_adaboost$byClass["F1"],
    auc_ada$AUC
  ))

performance_report_ada

## ---------------------------------------------------------------------------

## Tabella finale di confronto

## Siccome abbiamo la presenza di uno sbilanciamento nei nostri dati per la 
## classe positiva risulta utile oltre il confronto rispetto al
## tasso di errore anche il Valore AUC. 

Risultati <- data.frame('AUC' = c(auc_cart, auc_val_bagging, auc_val_rf, auc_ada$AUC),
           'Err' = c(err_cart_semplice, err_bagging, err_rf, err_adaboost))
rownames(Risultati) <- c('Cart pruned', 'Bagging', 'Random Forest', 'Adaboost')

Risultati


## Tuttavia implementando il classificatore ottimale bayesiano otteniamo i
## seguenti risultati relativi a Specificity e Sensitivity.

Risultati_metriche <- data.frame('Sensitivity(TPR)' = c(matrice_cart$byClass['Sensitivity'],
                                                         matrice_bagging$byClass['Sensitivity'],
                                                         matrice_random_forest$byClass['Sensitivity'],
                                                         matrice_adaboost$byClass['Sensitivity']),
                                 'Specificity(TNR)' = c(matrice_cart$byClass['Specificity'],
                                                         matrice_bagging$byClass['Specificity'],
                                                         matrice_random_forest$byClass['Specificity'],
                                                         matrice_adaboost$byClass['Specificity']))

rownames(Risultati_metriche) <- c('Cart pruned', 'Bagging', 'Random Forest', 'Adaboost')

Risultati_metriche

## In questo caso dai valori di sensitività e specificità concludiamo che i modelli
## sono precisi nel riconoscere la classe negativa tuttavia sono imprecisi nel riconoscere
## la classe positiva. Potrebbe essere un problema che riflette lo sbilanciamento
## iniziale nel dataset. 


## Nella pratica potrebbe interessarci avere una maggiore sensitività rispetto
## alla specificità. Quindi classificare più correttamente i consumatori che 
## sono più propensi a reagire ad una campagna marketing. 


# Verifica finale con la curva PR
pr_plot_rf <- pr.curve(scores.class0 = prob_pred_rf[dat_val$Response == "Si", 2],
                    scores.class1 = prob_pred_rf[dat_val$Response == "No", 2],
                    curve = TRUE)
plot(pr_plot_rf, main = "PR – Random Forest")

pr_plot_cart <- pr.curve(scores.class0 = pred_tree_prob[dat_val$Response == "Si", 2],
                         scores.class1 = pred_tree_prob[dat_val$Response == "No", 2],
                         curve = TRUE)

plot(pr_plot_cart, main = "PR – CART")

pr_plot_adaboost <- pr.curve(scores.class0 = pred_ada[dat_val$Response == "Si"],
                             scores.class1 = pred_ada[dat_val$Response == "No"],
                             curve = TRUE)
plot(pr_plot_adaboost, main = "PR – AdaBoost")

pr_plot_bagging <- pr.curve(scores.class0 = prob_pred[dat_val$Response == "Si", 2],
                            scores.class1 = prob_pred[dat_val$Response == "No", 2],
                            curve = TRUE)

plot(pr_plot_bagging, main = "PR – Bagging")


## TABLE 3 progetto

auc_err <- data.frame(
  Modello = c("Cart pruned", "Bagging", "Random Forest", "Adaboost"),
  AUC = c(auc_cart, auc_val_bagging, auc_val_rf, auc_ada$AUC),
  Err = c(err_cart_semplice, err_bagging, err_rf, err_adaboost)
)

## -----------------------------------------------------------------------------
sens_spec <- data.frame(
  Modello = c("Cart pruned", "Bagging", "Random Forest", "Adaboost"),
  TPR = c(matrice_cart$byClass['Sensitivity'], matrice_bagging$byClass['Sensitivity'],
          matrice_random_forest$byClass['Sensitivity'], matrice_adaboost$byClass['Sensitivity']),
  TNR = c(matrice_cart$byClass['Specificity'], matrice_bagging$byClass['Specificity'],
          matrice_random_forest$byClass['Specificity'], matrice_adaboost$byClass['Specificity'])
)
auc_pr <- data.frame(
  Modello = c("Cart pruned", "Bagging", "Random Forest", "Adaboost"),
  AUC_PR = c(pr_plot_cart$auc.integral, pr_plot_bagging$auc.integral, 
             pr_plot_rf$auc.integral, pr_plot_adaboost$auc.integral)
)

tabella_finale <- auc_err %>%
  left_join(sens_spec, by = "Modello") %>%
  left_join(auc_pr, by = "Modello")

tabella_finale
## -------------------------------------------------------------------------------------


## Dopo aver individuato i modelli con le migliori prestazioni complessive in 
## termini di AUC-PR, è stata analizzata la scelta della soglia decisionale. 
## Sebbene il punto di Youden fornisca una soglia che bilancia 
## sensibilità e specificità, 
## tale criterio può risultare subottimale in presenza di classi sbilanciate.
## Pertanto, la soglia finale è stata selezionata massimizzando l’F1-score, 
# che rappresenta un compromesso tra precisione e recall, 
# più coerente con l’obiettivo di individuare correttamente la classe minoritaria.

# Probabilità sul TRAINING

# Recupera i valori predetti per la CV, sarebbero gli half-log-odds 
cv_f_values <- fit_ada$cv.fitted

# Si trasformano in probabilità (usando la funzione logistica)
cv_probs <- 1 / (1 + exp(-2*cv_f_values))

# Si calcola ora l'F1-Score per ogni soglia
thresholds <- seq(0, 1, by = 0.01)

f1_cv_ada <- sapply(thresholds, function(t) {
  # Trasforma probabilità in classi (0/1 o No/Si)
  pred_t <- ifelse(cv_probs >= t, 1, 0)
  
  # Crea la matrice di confusione (assicurati che dat_train$Response sia 0/1)
  # o converti entrambi i fattori
  actual <- ifelse(dat_train$Response == "Si", 1, 0)
  
  # Calcolo F1
  cm <- confusionMatrix(factor(pred_t, levels = c(0, 1)), 
                        factor(actual, levels = c(0, 1)), 
                        positive = "1")
  return(cm$byClass["F1"])
})

t_best_f1 <- thresholds[which.max(na.omit(f1_cv_ada))]


## Valutazione finale con soglia ottimale

pred_final <- ifelse(pred_ada >= t_best_f1, "Si", "No")
pred_final <- factor(pred_final, levels = levels(dat_val$Response))

# Confusion matrix
cm <- confusionMatrix(pred_final, dat_val$Response, positive = "Si")
cm

# Sensitivity e Specificity
sens <- cm$byClass["Sensitivity"]
spec <- cm$byClass["Specificity"]

# G-Mean
gmean <- sqrt(sens * spec)
gmean

f1 <- cm$byClass["F1"]
f1

# MCC manuale
TP <- cm$table[2,2]
TN <- cm$table[1,1]
FP <- cm$table[1,2]
FN <- cm$table[2,1]

mcc <- (TP*TN - FP*FN) / sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
mcc

Risultati_end_ada <- data.frame('Sensitivity' = sens,
                               'Specificity' = spec,
                               'F1' = f1,
                               'MCC' = mcc)
row.names(Risultati_end_ada) <- 'Risultati'

Risultati_end_ada

## -------------------------------------------------------------------------
## Grafico ADAboost F1 al variare della soglia 
## -------------------------------------------------------------------------

# Generazione Grafico sul VALIDATION 
# Calcolo probabilità sul validation set
prob_val_ada <- predict(fit_ada, newdata = dat_val, n.trees = best_ntrees, type = "response")
f1_val_ada <- sapply(thresholds, function(t) {
  pred <- factor(ifelse(prob_val_ada >= t, "Si", "No"), levels = c("No", "Si"))
  confusionMatrix(pred, dat_val$Response, positive = "Si")$byClass["F1"]
})

df_plot_ada <- data.frame(threshold = thresholds, F1 = f1_val_ada)

p_ada <- ggplot(df_plot_ada, aes(x = threshold, y = F1)) +
  geom_line() +
  geom_vline(xintercept = t_best_f1, linetype = "dashed", color = "red") +
  annotate("text", x = t_best_f1, y = max(f1_val_ada, na.rm=T), 
           label = paste0("Soglia Train: ", t_best_f1), vjust = -1, color = "red") +
  theme_minimal() +
  ggtitle('AdaBoost: F1 su Validation (Soglia da Train)')



## -------------------------------------------------------------------------

## Dopo aver fissato la soglia finale (basata sulla massimizzazione dell’F1-score),
## il modello è stato valutato tramite metriche adatte a dataset sbilanciati. 

## -------------------------------------------------------------------------
## Per il random forest

##Prendo le probabilità dalla cross validation

cv_results <- fit_rf$pred %>%
  filter(mtry == fit_rf$bestTune$mtry)

# Griglia di valori 
thresholds <- seq(0, 1, by = 0.01)

f1_cv <- sapply(thresholds, function(t) {
  # Applichiamo la soglia t alla colonna della classe positiva 
  pred_t <- ifelse(cv_results$Si >= t, "Si", "No")
  
  # Creiamo la matrice di confusione confrontando con i valori reali (obs)
  cm <- confusionMatrix(factor(pred_t, levels = c("Si", "No")), 
                        cv_results$obs, ## Quelle attuali
                        positive = "Si")
  
  return(cm$byClass["F1"])
})


## Prendo la soglia che massimizza l'f1-score medio della CV
t_best_f1_rf <- thresholds[which.max(na.omit(f1_cv))]
t_best_f1_rf

## Valutazione finale con soglia ottimale

pred_final <- ifelse(prob_pred_rf[,2] >= t_best_f1_rf, "Si", "No")
pred_final <- factor(pred_final, levels = levels(dat_val$Response))

# Confusion matrix
cm <- confusionMatrix(pred_final, dat_val$Response, positive = "Si")
cm

# Sensitivity e Specificity
sens <- cm$byClass["Sensitivity"]
spec <- cm$byClass["Specificity"]

f1 <- cm$byClass["F1"]
f1

# MCC manuale
TP <- cm$table[2,2]
TN <- cm$table[1,1]
FP <- cm$table[1,2]
FN <- cm$table[2,1]

mcc <- (TP*TN - FP*FN) / sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
mcc

Risultati_end_rf <- data.frame('Sensitivity' = sens,
                                'Specificity' = spec,
                                'F1' = f1,
                                'MCC' = mcc )
row.names(Risultati_end_rf) <- 'Risultati'

Risultati_end_rf


## Individuando la treshold ottimale il random forest ottiene risultati di 
## sensitività maggiori. 

## -------------------------------------------------------------------------
## Grafico Random Forest F1 al variare della soglia
## -------------------------------------------------------------------------

# --- Generazione dati Grafico sul VALIDATION ---
prob_val_rf <- predict(fit_rf, newdata = dat_val, type = "prob")
f1_val_rf <- sapply(thresholds, function(t) {
  pred <- factor(ifelse(prob_val_rf[,2] >= t, "Si", "No"), levels = c("No", "Si"))
  confusionMatrix(pred, dat_val$Response, positive = "Si")$byClass["F1"]
})



df_plot_rf <- data.frame(threshold = thresholds, F1 = f1_val_rf)

p_rf <- ggplot(df_plot_rf, aes(x = threshold, y = F1)) +
  geom_line() +
  geom_vline(xintercept = t_best_f1_rf, linetype = "dashed", color = "blue") +
  annotate("text", x = t_best_f1_rf, y = max(f1_val_rf, na.rm=T), 
           label = paste0("Soglia Train: ", t_best_f1_rf), vjust = -1, color = "blue") +
  theme_minimal() +
  ggtitle('Random Forest: F1 su Validation (Soglia da Train)')


## -------------------------------------------------------------------------

## Grafico Figura 4 e Table 4

p_ada + p_rf


## Costruzione tabella
Risultati_end_rf$Modello <- "Random Forest"
Risultati_end_ada$Modello <- "Adaboost"

Risultati_end <- bind_rows(Risultati_end_rf, Risultati_end_ada)
row.names(Risultati_end) <- NULL
Risultati_end

## -------------------------------------------------------------------------


## Articolo di riferimento: https://doi.org/10.1016/j.aci.2018.08.003
## A differenza della curva ROC (dove il caso casuale è sempre 0.5),
## nella curva Precision-Recall la baseline è pari alla prevalenza della classe positiva.

# Baseline = 0.15 (random classifier)

## Variable importance dei due modelli 
## ----------------------------------------------------------------------------

## ---------------------------------------------------------------------------
## Figura 5

u_rf <- vip::vip(fit_rf)
plot(u_rf)

## ---------------------------------------------------------------------------

## ---------------------------------------------------------------------------
## Figura 6

u <- vip::vip(fit_ada)
plot(u)

## ---------------------------------------------------------------------------

## END