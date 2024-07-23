# coding=utf8
import pandas as pd
import os
import datetime
from selenium import webdriver
import time
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.chrome.service import Service
from webdriver_manager.chrome import ChromeDriverManager

class Croupier:
    def __init__(self, name):
        self.name = name

def main():
    #inizializzazione:
    print("\n\n**********************************")
    print("Benvenuto nella versione .project di ROULexe.\nQuesta è una versione demo, si prega di NON usare il programma per scopi diversi a quelli didattici.\n\nIl programma è stato scritto da Giacomo Filippin.")
    
    wheel = [0, 32, 15, 19, 4, 21, 2, 25, 17, 34, 6, 27, 13, 36, 11, 30, 8, 23, 10, 5, 24, 16, 33, 1, 20, 14, 31, 9, 22, 18, 29, 7, 28, 12, 35, 3, 26,
             0, 32, 15, 19, 4, 21, 2, 25, 17, 34, 6, 27, 13, 36, 11, 30, 8, 23, 10, 5, 24, 16, 33, 1, 20, 14, 31, 9, 22, 18, 29, 7, 28, 12, 35, 3, 26,
             0, 32, 15, 19, 4, 21, 2, 25, 17, 34, 6, 27, 13, 36, 11, 30, 8, 23, 10, 5, 24, 16, 33, 1, 20, 14, 31, 9, 22, 18, 29, 7, 28, 12, 35, 3, 26,]
    # wheel definisce la ruota ordinata della roulette, la ruota è ripetuta tre volte in modo da permettere il calcolo della distanza di ogni coppia di numeri su di essa.
    
    if os.path.isfile("Credentials.txt"):
        mail, psw = credRead()
    else:
        config()
        mail, psw = credRead()
    # qui devono essere inserite le credenziali dell'account betfair che si desidera utilizzare per le operaizoni di scraping e puntata automatica.
    
    #avvio della pseudo-interfaccia utente 
    menu(wheel)
    
    print("****ARRIVEDERCI****\n\n**********************************")
    
def menu(N):  #  stampa le diverse funzioni del programma e lancia la funzione scelta; permette di eseguire funzioni in serie finchè non si sceglie di uscire.
    while True:  # il ciclo while permette di restare nel programma una volta eseguita un'operazione.
        print("\n\n**********************************")
        print("\nMenù: \n\n1: START ROULexe \n2: RESET (necessario al primo avvio)/(se presenti dati raccolti,formatta tutto) \n3: Modifica credenziali \nAltro: Esci")
        Chosen = str(input())     # permette di selezionare l'operazione da eseguire.
    
        if Chosen == '1':    # START ROULexe
            print("\n\n**********************************\nHai selezionato: 'START ROULexe'.\n\n**********************************\n")
            mail, psw = credVerify()
            print("\n\n**********************************")
            oggi = datetime.datetime.now()
            ieri = oggi - datetime.timedelta(days=1)
            ieri_d = ieri.strftime("%Y-%m-%d")
            print("\n\n**********************************")
            dataset = int(input("vuoi usare i dati completi o quelli degli ultimi tre giorni?\n(0:completi, 1: ultimi tre giorni)\n"))
            if dataset == 0:
                dati = pd.read_csv('Data.txt', header=0, sep=' ')
                choice = 'full_dataset'
            elif dataset == 1:
                dati_orig = pd.read_csv('Data.txt', header=0, sep=' ', parse_dates=['data'], dayfirst=True)
                dati_orig['data'] = pd.to_datetime(dati_orig['data']).dt.date
                oggi = datetime.datetime.now().date()
                due_giorni_fa = oggi - datetime.timedelta(days=2)  # tre giorni fa
                dati = dati_orig[(dati_orig['data'] >= due_giorni_fa)]    
                choice = 'last_3_days'
            # Stampa le prime righe del DataFrame
            if dati.empty:
                distsToPlay = [35,36,0,1,2]
            else:
                dati['date'] = pd.to_datetime(dati['date'])

                bestsects = [None] * 37
                tabella_frequenza = dati[dati['date'] == ieri_d]['distance'].value_counts().sort_index()
                for j in range(0, 33):
                    bestsects[j+2] = tabella_frequenza.loc[[j,j+1,j+2,j+3,j+4]].sum()
                bestsects[35] = tabella_frequenza.loc[[33, 34, 35, 36, 0]].sum()
                bestsects[36] = tabella_frequenza.loc[[34, 35, 36, 0, 1]].sum()
                bestsects[0] = tabella_frequenza.loc[[35, 36, 0, 1, 2]].sum()
                bestsects[1] = tabella_frequenza.loc[[36, 0, 1, 2, 3]].sum()
                bestdist = bestsects.index(max(bestsects))
                if bestdist == 35:
                    distsToPlay = [33,34,35,36,0]
                elif bestdist == 36:
                    distsToPlay = [34,35,36,0,1]
                elif bestdist == 0: 
                    distsToPlay = [35,36,0,1,2]
                elif bestdist == 1:
                    distsToPlay = [36,0,1,2,3]
                else:
                    distsToPlay = [bestdist-2,bestdist-1,bestdist,bestdist+1,bestdist+2]

            print('il centroide migliore di ieri è stato: ', distsToPlay[3], '\n\n')
            bet = input("si desidera anche giocare?: (y/n)\n")
            print("\n\n...CARICAMENTO IN CORSO, ATTENDERE PREGO.")
            autobet_andScrape(distsToPlay, N, mail, psw, bet)
                      
        elif Chosen == '2':  # RESET:
            print("\n\n**********************************\n(re)inizializzazione in corso.\n\n**********************************\n")
            config()
        elif Chosen == '3':  # permette di sovrascrivere le credenziali
            print("\n\n**********************************\nSovrascrittura delle credenziali.\n\n**********************************\n")
            credChange()
        else:
            break

def config():  # crea (o pulisce se già esistenti) i file 'Croupiers.txt' e 'Data.txt', necessari alla memorizzazione dei dati raccolti.
               # N.B: viene inserito di default un croupier 'AA.test' per eventuali prove di inserimento dati in modo che sia facilmente individuabile e rimuovibile da analisi future.

    file = open('Data.txt', 'w')
    file.write("number distance verse croupier time date betting won delta launch_n")
    file.write("\n")
    file.close() # crea il file che conterrà il dataset.
    
    file = open('Credentials.txt', 'w')
    file.write("NA")
    file.write('\n')
    file.write("NA")
    file.close() # crea il file che conterrà le credenziali betfair.  .

def credRead():  # legge dal file 'Credentials.txt' le credenziali memorizzate.
    file = open('Credentials.txt', 'r')
    lines = file.readlines()
    return str(lines[0]), str(lines[1])

def credInsert():  # chiede in input le credenziali betfar.
    print("inserire le credenziali Betfair da utilizzare:\n")
    mail = str(input('mail: '))
    psw = str(input('psw: '))
    return mail, psw

def credWrite(mail, psw):  # memorizza le credenziali nel file Credentials.txt.
    file = open('Credentials.txt', 'w')  
    file.write(mail)
    file.write('\n')
    file.write(psw)
    file.close()      

def credVerify():  # verifica che le credenziali siano state inserite, altrimenti le chiede in input.
    mail, psw = credRead()
    if mail == "NA" or psw == "NA":
        print("prima di utilizzare questa funzione,") # il resto del testo è contenuto in credInsert.
        mail, psw = credInsert()
        credWrite(mail,psw) # memorizza le nuove credenziali.
    else:
        print('\n\nCredenziali verificate con successo.')
    return mail, psw

def credChange():  # sovrascrive le vecchie credenziali con credenziali nuove da input.
    print('CAMBIO CREDENZIALI:\n')
    mail, psw = credInsert() # chiede da input le nuove credenziali.
    credWrite(mail,psw) # sovrascrive le credenziali vecchie.

def distance(n1, n2, N):  # data la ruota e due numeri su di essa, calcola la distanza tra questi su di essa (intesa come numero di caselle che separano i due numeri.)
                          # N.B: la distanza viene calcolata sempre in senso orario, non come il minimo tra la distanza oraria e quella antioraria.
    if n1 == n2:
        return 0 # se i due numeri sono uguali la distanza è nero (è un caso limite e si può anticipare al resto per aumentare l'efficienza in caso esso si verifichi.
    for i in N: # si trovano gli indici dei due numeri nella ruota, per poi sottrarli e trovare la loro distanza.
                # N.B: l'indice del secondo numero va trovato solo tra i numeri successivi al primo, in modo da trovare la distanza in senso orario (per questo la ruota è ripetuta due volte)
        if N[i] == n1:
            for j in range (i+1, len(N)):
                if N[j] == n2:
                    return (j - i)

def commonGround(mail, psw):  # esegue lo scraping delle ultime 4 ore e mezza di lanci; il verso viene calcolato a partire dall'ultimo lancio.
    # CARICAMENTO DELLA PAGINA E DEFINIZIONE DEI BOTTONI DI PUNTATA
    print("\n\n\nLOGIN IN CORSO...")
    options = webdriver.ChromeOptions()
    options.add_argument("start-maximized")
    options.add_argument("--headless")
    options.add_argument("user-agent=Mozilla/5.0 (Windows NT 11.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.150 Safari/537.36")
    options.add_experimental_option("excludeSwitches", ["enable-automation"])
    options.add_experimental_option('useAutomationExtension', False)
    
    print("\n\n\nLOGIN IN CORSO...")
    driver = webdriver.Chrome()
    driver.get("https://casino.betfair.it/p/casino-live") # apre il sito di Betfair nella sezione dedicata al casinò live.
    WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.ID, "ssc-liu"))) # aspetta che la pagina si carichi 
    driver.find_element(By.ID, "ssc-lipw").send_keys(psw) # inserisce la psw nello spazio indicato.
    time.sleep(0.1) 
    driver.find_element(By.ID, "ssc-liu").send_keys(mail) # inserisce la mail nello spazio indicato
    WebDriverWait(driver, 5).until(EC.element_to_be_clickable((By.ID, "ssc-lis")))    
    driver.find_element(By.ID, "ssc-lis").send_keys(Keys.ENTER) # finalizza la procedura di login
    time.sleep(1)
    capcha = input('è stato superato il capcha?')
    stake = driver.find_element(By.CLASS_NAME, "ssc-wla").text
    time.sleep(1)
    print("\n\n\nCONNESSIONE ALLA LOBBY...")
    driver.get("https://launcher.betfair.it/?gameId=live-betfair-roulette-italiana-cptl&returnURL=https%3A%2F%2Fcasino.betfair.it%2Fp%2Fcasino-live&launchProduct=casino&RPBucket=casino&mode=real&dataChannel=casino&switchedToPopup=true")
    WebDriverWait(driver, 40).until(EC.element_to_be_clickable((By.CLASS_NAME, "modal-button--XBKpB")))    
    driver.find_element(By.CLASS_NAME, "modal-button--XBKpB").click() # conferma l'inserimento dei soldi (necessari due click).
    time.sleep(2)
    driver.find_element(By.CLASS_NAME, "modal-button--XBKpB").click()
    WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.CLASS_NAME, "sidebar-buttons__item")))    
    driver.find_elements(By.CLASS_NAME, "sidebar-buttons__item")[4].click() # dopo aver aspettato che lo storico sia disponibile, lo apre (necessari due click, sulla sezione e poi sullo storico in sé).
    
    # qui termina la parte comune a scraping ed autobet.
    return driver, stake

def print_col(stringa, colore):
    colore_reset = "\033[0m"
    print(colore + stringa + colore_reset)

def writeData(fileName, record):  # aggiunge un record dato in input al dataset scelto.
    file = open(fileName, 'a')
    file.write(record)
    file.write("\n")
    file.close()

def autobet_andScrape(dist_toPlay, wheel, mail, psw, bet):  # punta sul settore indicato il budget indicato finche il programma non viene stoppato
    # CARICAMENTO DELLA PAGINA E DEFINIZIONE DEI BOTTONI DI PUNTATA
    driver, stake = commonGround(mail, psw)
    all_singlebets = driver.find_elements(By.CLASS_NAME, "table-cell__bg-main--NI0hh")
    playables = [all_singlebets[41]]
    # playables contiene tutti i bottoni corrispondenti alle puntate dirette sui numeri singoli da 0 a 36.    
    for i in range(54, 90):
        playables.append(all_singlebets[i])
    # AUTOBETTING:
    toPlay = []
    gains = 0    # gains servirà poi per tenere traccia dei guadagni.
    gains_ = gains
    start = input('premi INVIO dopo un lancio antiorario per iniziare a giocare.')
    verso = 0
    antiverso = 1
    print('\n\n******************************\n\nINIZIALIZZAZIONE...\n\n')
    launch_count = 0

    while True:
        # ricava l'ultimo numero estratto e lo dupluca.
        elements = driver.find_elements(By.CSS_SELECTOR, '.roulette-history-item--ei7kI .roulette-history-item__value--CjA_R .roulette-history-item__value-text--siwxW')
        last_numbers = []
        last_number = elements[0].text
        old_last_number = elements[1].text
        for item in elements[0:5]:
            last_numbers.append(item.text)
        old_last_numbers = last_numbers
        while last_numbers == old_last_numbers: # controlla ogni secondo se c'è stato un nuovo numero estratto confrontando last_number aggiornato con old_last_number non aggiornato.
            old_last_numbers = last_numbers
            last_numbers = []
            elements = driver.find_elements(By.CSS_SELECTOR, '.roulette-history-item--ei7kI .roulette-history-item__value--CjA_R .roulette-history-item__value-text--siwxW')
            for item in elements[0:5]:
                last_numbers.append(item.text)
            last_number = elements[0].text
            old_last_number = elements[1].text
            while last_number == '':
                elements = driver.find_elements(By.CSS_SELECTOR, '.roulette-history-item--ei7kI .roulette-history-item__value--CjA_R .roulette-history-item__value-text--siwxW')
                last_number = elements[0].text
                old_last_number = elements[1].text
        print('Delta: ', round(gains,2))
        print("...IN ATTESA DEL PROSSIMO NUMERO.")
        print('\n*****************************')
        ora = int(datetime.datetime.now().time().hour)
        if launch_count != 0 and ((bet != 'n' and verso == 1) or (bet == 'n' and launch_count % 15 == 0)  or (18 <= ora <= 23 and verso == 1)):
            if int(last_number) in toPlay: # se il numero uscito è tra quelli giocati, incrementa il credito disponibile di 7.20 euro.
                gains = gains + 7.20
                won = 1
                print_col("HAI VINTO! SI SBOCCIA!", "\033[92m")
            else:
                won = 0
                print_col("RIP BOZO.", "\033[91m")
        won = 'NA'
        toPlay = []
        for number in wheel: # calcola, in base al numero uscito precedentemente, che numeri bisogna giocare, ricavando prima i margini del settore e poi trovando i numeri al suo interno.
            if distance(int(last_number), int(number), wheel) in dist_toPlay and number not in toPlay:
                toPlay.append(int(number))
        print('numero uscito: ',last_number)
        if (bet != 'n' and verso == 0) or (bet == 'n' and launch_count % 15 == 0):
            print('nuova puntata: ', toPlay)
            wait = WebDriverWait(driver, 5)
            wait.until(EC.invisibility_of_element_located((By.CSS_SELECTOR, "div.modal-overlay")))
            for number in toPlay: # gioca i numeri da giocare e sottrae le puntate al credito disponibile.
                try:
                    time.sleep(0.20)
                    playables[int(number)].click()
                    gains = gains - 0.20
                except Exception as e:
                    print("\n*************************\nErrore nella puntata del numero \n", number, '\n*************************\n')
                    pass
            print('Delta: ', round(gains,2)) # stampa i numeri giocati per fare verifiche a schermo della corrispondenza tra numeri giocati e numeri effettivamente da giocare.                 
        launch_count += 1
        dist = distance(int(old_last_number), int(last_number), wheel)
        data = time.strftime("%d/%m/%Y") # ricava la data autonomamente.    
        ora = datetime.datetime.now().time()
        ora = ora.strftime("%H:%M:%S.%f")[:-3]
        croupier = driver.find_element(By.CLASS_NAME, "header__dealer-name").text
        record = str(last_number)+' '+str(dist)+' '+str(verso)+' '+str(croupier)+' '+str(ora)+' '+str(data)+' '+str(bet)+' '+str(won)+' '+str(round(gains_,2))+' '+str(launch_count)
        writeData('DataNew.txt', record) # inserisce ciascun record nel file DataNew.txt.
        gains_ = gains
        verso, antiverso = antiverso, verso
    driver.close()




#inizio
main()
