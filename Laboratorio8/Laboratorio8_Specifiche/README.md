# "VIQ - Laboratorio 8"

*Date: 2021-05-20*

### Obiettivi

1.  Applicare principi e tecniche avanzate per la realizzazione di grafici interattivi in app Shiny, in particolare:
    - validare l'input dell'utente
    - costruire elementi grafici dinamici 
    - gestire gli eventi del mouse 

### Strumento

-  R Studio e Shiny

**Dataset**

Il dataset è lo stesso del Lab 06: `covid_19-iss.xlsx`. 

**Tasks**

Sviluppare un'app Shiny che visualizzi l'andamento dei casi Covid, rendendo interattivo il grafico sviluppo in `Lab6_sol.Rmd`, sezione *"Andamento temporale"*.
Il grafico è il seguente:

![](figure/andamento-casi.png)

I requisiti sono i seguenti.

1. Il file deve essere caricato dall'utente, e i seguenti controlli devono essere eseguiti:
   - controllo sull'estensione "xlsx"
   - controllo sulla presenza dei tab *"casi_inizio_sintomi"* e *"casi_prelievo_diagnosi"*, per il quale si consiglia l'uso di `need()`(verificare la documentazione di `validate()`e di `tryCatch()` :
   
      ```
      result <- tryCatch(
          {
              # Codice che si vuole girare, che può lanciare errore o warning
          }, warning = function(war) {
              # Codice eseguito se viene lanciato un warning 
          }, error = function(err) {
              # # Codice eseguito se viene lanciato un errore 
          }
      )
      ```
    - N.B.1: I messaggi di errore visualizzati all'utente devono essere di almeno due tipi, ovvero bisogna differenziare il caso dell'estensione errata da quello del tab mancante 
    - N.B.2: Il grafico non deve essere visualizzato se il file non è caricato (suggerimento)
    - N.B.3: la funzione `read_xlsx( )` necessita un percorso.  Si può accedere al percorso del file caricato con `input$issfile$datapath`

2. Fare in modo che l'utente possa selezionare se visualizzare nel grafico:
   - solo la linea dei casi conteggiati con i sintomi `Tipo == "SINTOMI"`
   - solo la linea dei casi conteggiati con la diagnosi `Tipo == "DIAGNOSI"`
   - entrambe le linee
   - nessuna delle due, e in questo caso il grafico non viene visualizzato, ma viene mostrato all'utente un messaggio informativo 

3. Fare in modo che l'utente possa selezionare due date che ridefiniscono l'arco temporale del grafico (asse X)

4.  Fare in modo che l'utente possa selezione (*`brush`*) una porzione dei dati sul grafico. Una volta selezionati, le seguenti azioni avvengono:
   - una tabella interattiva appare accanto al grafico, contenente i dati selezionati 
   - sul grafico appare l'indicazione della media dei casi per la selezione: se la selezione contiene entrambe le linee (SINTOMI e DIAGNOSI), allora entrambe le indicazioni devono apparire; altrimenti solamente quella del tipo conteggio selezionata.
   
   - N.B.1: in caso di selezione vuota, né la tabella né le indicazioni sul grafico dovranno apparire, bensì un messaggio informativo che indichi all'utente che non ha selezionato dati.
   - N.B.2: Entrambe le selezioni del *`brush`* devono tener conto delle precedenti selezioni sulle date e sul tipo di conteggio dei casi.  
   - N.B.3: per semplicità, se le date e il tipo di conteggio dei casi vengono selezionati dopo una selezione *`brush`* , allora quest'ultima viene annullata
 
   
E' fortemente consigliato partire con il design dell'applicazione tramite grafo reattivo, e solo successivamente implementare il codice seguendo il grafo (ed eventualmente correggendelo in caso di modifiche). 

Le seguenti screenshot sono esempi di come potrebbe apparire il grafico e di interattività atteso.

- Schermata iniziale:

![](figure/schermata-iniziale.png)


- Inserimento file con estensione errata:

![](figure/estensione-errata.png)

- Inserimento file con tab *casi_prelievo_diagnosi* mancante:

![](figure/tab.diagnosi.mancante.png)

- Inserimento file con tab *casi_inizio_sintomi* mancante:
  
![](figure/tab.sintomi.mancante.png)

- Caricamento file corretto e visualizzazione grafico:
  
![](figure/caricamento-file.png)

- Nessun tipo di conteggio selezionato:

![](figure/nessun-conteggio.png)

- Selezione di date e tipo di conteggio:
 
![](figure/selezioni-date-conteggio.png)

- Successiva selezione con brush:

![](figure/brush-presel.png)

- Selezione brush completa:
  
![](figure/brush-completa.png)

- Selezione brush con solo conteggi per diagnosi
  
![](figure/brush-diagnosi.png)
