In questa cartella sono contenuti gli script con cui si è analizzata la differenza dei voti 
nelle diverse regioni e i risutati dei test di ipotesi in merito.

Sempre, si è considerato un modello con risposta bivariata, corrispondente al vettore di [voti math,
voti reaging]. E' stato utilizzato JAGS per campionare dai modelli.

Si è proceduto costruendo due modelli differenti. Le loro descrizioni sono contenute all'inizio dei rispettivi
file ANOVA1.R e ANOVA2.R, script utilizzati per campionare con JAGS e poi per analizzare le densità a posteriori.
I files modello_biv_anova1.bug e modello_biv_anova2.bug contengono i files utilizzati da JAGS negli script.

Nel file BF_ANOVA_confronto_regioni.R vengono fatti alcuni testi di ipotesi per "ordinare" le regioni
dalla migliore alla peggiore in base ai voti ottenuto dagli studenti. 

---------------------------------------------------------------------------------------------

This folder holds all the script to analyse the difference of the students' evaluation between different regions 
and the results of the relates hypothesis tests.

We always considered the bivariate model with bidimensional response [math evaluation, reading evaluation].
We used JAGS to sample from the model.

Two additional ANOVA models were built. Files ANOVA1.R and ANOVA2.R contain the description of the models 
and the code to sample with JAGS and analyze posterior distribution. 
Files modello_biv_anova1.bug and modello_biv_anova2.bug were used by JAGS in the previous scripts.

In BF_ANOVA_confronto_regioni.R we performed some hypothesis test, aiming at ordering the regions 
from the best to the worst.
