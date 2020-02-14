# Pisa-BurzacchiFalcoTeodori
L'obiettivo del progetto è capire quali sono i fattori che impattano maggiormente sui voti degli studenti italiani.
A partire dai dati dell'OCSE, che forniscono numerose caratterstiche sugli studenti e sulle scuole da loro frequentate,
si vuole costruire un modello gerarchico che abbia come risposta i voti degli studenti.
Data la numerosità delle covariate presenti (più di 1000) si è prima proceduto con una scrematura iniziate.
L'analisi vera e propria scremerà ulteriormente le covariate.
L'analisi è stata effettuata rimuovendo le righe con dati mancanti,
in quanto le informazioni presenti non sono state ritenute sufficienti per imputare i dati mancanti in modo soddisfacente.
Ogni analisi è stata trattata con un approccio bayesiano.


La cartella dataset contiene i diversi dataset utilizzati.

Per i modelli lineari guardare solo la cartella modelli_finali, il resto sono codici/risultati vecchi.
Al suo interno è presente l'analisi del modello univariato (risposta: voto_matematica) e del modello bivariato (risposta: voto_matematica, voto_reading). Ogni sottocartella contiene un file read.me che descrive il contenuto della cartella stessa.

Nella cartella sono contenuti gli script con cui si è analizzata la differenza dei voti 
nelle diverse regioni (Campania, Tentino Alto Adige e Lombardia, le uniche di cui si aveva informazione) e i risutati dei test di ipotesi in merito.
