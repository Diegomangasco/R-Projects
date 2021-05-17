VIQ - Laboratorio 7
===================

*13 maggio 2021*

### Obiettivo

- Applicare principi e tecniche base per la costruzione di app Shiny

### Strumento

-  R Studio 

### Dataset

*Dataset del Laboratorio 1, Esercizio 1* :

Classifica Marcatori Serie A, Campionato 2020/21, 27ª giornata

-   URL: <http://softeng.polito.it/courses/VIQ/datasets/Marcatori2021.csv>
-   Scaricata a partire dai dati pubblicati da [La Gazzetta dello Sport](https://www.gazzetta.it/calcio/serie-a/marcatori/).

**Istruzioni per creare un'app Shiny con il *project wizard* R Studio**

- Utilizzare il menù *File* e sceglie *New Project...* e quindi *New Directory* 
- Nella finestra di scelta del tipo di progetto, selezionare *Shiny Web Application* 
- Indicare il nome della cartella (es. MarcatoriSerieA-app1) e il percorso del file system dove creare il progetto 
- creare la sottocartella *data* e copiare al suo interno il file di dati `Marcatori2021.csv`
- Aprire il file `app.R` (creato automaticamente dal wizard) per avviare l'esercizio


--- 

### App 1: statistiche generali

1.  **Da Lab1, Es1, Task2**:  Calcolare e mostrare servendosi di *textOutput()* e/o *verbatimTextOutput()* gli indici sommari della distribuzione dei gol:
    1.   Tendenza centrale: media, mediana, midrange, moda
    2.   Dispersione: deviazione standard, MAD, IQR, range


2.  **Da Lab1, Es1, Task3**: Aggiungere alla pagina la tabella di distribuzione delle frequenze per il numero di gol (inserire il numero di gol come etichetta di riga) in due modi:
    1.  utilizzando la funzione *TableOutput()*
    2.  utilizzando la funzione *dataTableOutput()*
        
3. Immettere un campo di input legato alla tabella statica, in cui inserire il numero massimo di gol da visualizzare utilizzando *numericInput()* e fare in modo che la tabella si aggiorni automaticamente 

4. Aggiungere un pulsante (tramite *actionButton()*) e fare in modo che la tabella si aggiorni solo quando il bottone viene schiacciato (usare *eventReactive()*)

5.  Immettere un un campo di input legato alla tabella dinamica, in cui inserire un range di gol da visualizzare utilizzando *SliderInput()* e fare in modo che la tabella si aggiorni automaticamente quando viene schiacciato un apposito pulsante. Gli estremi dello slider possono essere fissati staticamente.

6. Utilizzare *fluidRow()* e *columns()* per organizzare la pagina in modo visivamente fruibile e armonioso

7. Disegnare il reactive graph dell'app realizzata

--- 

### App 2: marcatori

#### *Istruzioni per caricare un csv direttamente dall'app Shiny*

0. Istruire l'app per fare l'upload di `Marcatori2021.csv` seguendo le seguenti istruzioni:
   -  Utilizzare questa stringa nella funzione della UI, che permette di fare l'upload: `fileInput("file", "Load csv file Marcatori Serie A", accept = c(".csv"))`
   -  Utilizzare il seguente codice dentro la funzione del server, che rende disponibili i dati come dataframe *df* :
   
      ```
      df<- reactive({req(input$file)
      read.csv(input$file$datapath, header = TRUE, sep=",")})
      ```
      
    - *N.B.: queste istruzioni sono semplificate perché non vengono fatti controlli per validare l'input*
  
1. **Da Lab1, Es1, Task4**: Costruire una tabella di distribuzione delle frequenze che riporti, per ciascuna squadra, il numero di giocatori che hanno segnato almeno un gol per quella squadra. La tabella deve essere ordinata per numero di marcatori *decrescente* . Utilizzare esclusivamente *TableOutput()*.

2. Creare un menù con *SelectInput()* in cui si possa selezionare il ruolo (valori possibili: *Tutti, Difensore, Centrocampista, Attaccante*) e fare in modo che la tabella si aggiorni mostrando solo il numerdo di giocatori che ricoprono quel ruolo
   - N.B. 1: per verificare i risultati, modificare la tabella Pivot del file `Lab1_1_sol.xlsx` (tab `Freq.squadre`) in modo che venga tenuto in conto nelle righe anche il ruolo
   - N.B. 2: la tabella, ad ogni aggiornamento, deve continuare as essere ordinata per numero di marcatori *decrescente* 

3. Modificare *SelectInput()* con l'opzione *multiple=TRUE* 
e fare in modo che la tabella si aggiorni mostrando solo le squadre selezionate
    - N.B.: la tabella, ad ogni aggiornamento, deve continuare as essere ordinata per numero di marcatori *decrescente* 

4. Disegnare il reactive graph dell'app realizzata, e verificare se sia opportuno apportare cambiamenti

--- 

### App 3: grafico

0. Copiare tutto il codice dell'App 2

1. Utilizzare il *sidebarLayout()* in modo da avere gli input nel pannello laterale e gli output nel pannello principale

2. Nel pannello principale, creare due tab:
   - il primo tab ospita la tabella di distribuzione delle frequenze 
   - il secondo tab ospita un grafico che rappresenta la tabella di distribuzione dei marcatori per squadra (ad es. grafico a barre), facendo in modo che il grafico si aggiorni automaticamente con la scelta dei ruoli dal pannello laterale

3. Disegnare il reactive graph dell'app realizzata, e verificare se sia opportuno apportare cambiamenti 

