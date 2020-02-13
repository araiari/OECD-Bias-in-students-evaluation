# READ ME

# In questa cartella sono contenuti gli script per il modello lineare.

# La risposta del modello � il voto in matematica degli studenti,
# mentre le covariate disponibili riguardano alcune informazioni di base, la loro condizione socio-economica, le loro abitudini e i loro interessi.

# Abbiamo selezionato le covariate pi� significative utilizzando i metodi di SSVS e di Elastic Net.
# Si � scelto di utilizzare il criterio di Hard Shrinkage controllando se 0 sta nei credible intervals dei coefficienti.
# Tutti i metodi sono stati implementati tramite MCMC con JAGS.

# Il file SSVS_Variable_Selection.R � lo script per implementare il metodo SSVS. 
# Sono utilizzati diversi iperparametri e i diversi risultati sono confrontati alla fine dello script.

# Nel file Confronto_LM_con_WAIC.R sono confrontati i diversi LM con le covariate pi� significative e pi� o meno covariate dubbie. 
# E' calcolato l'indice WAIC dei LM ed � scelto il modello con il maggior WAIC.

# La cartella contiene anche i files .bug utilizzati da JAGS nei vari script sopra descritti.

# E' inoltre aggiunta un'ulteriore cartella, Elastic_net. Contiene gli script che implementano il metodo di regressione penalizzata EN.

