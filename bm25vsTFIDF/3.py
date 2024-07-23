#--- importazione di interi moduli
import sys
import json
import numpy as np 
import matplotlib.pyplot as plt
#import pytrec_eval
import csv
import re

from elasticsearch import Elasticsearch as esserver

def res(results,a_file, query = "1", n = 10, tag = "tag"):
    rank = 0
    for hit in results:
        rank += 1
        docid = hit["_source"]['id']
        score = hit['_score']
        print(query,"Q0",docid,rank,score,tag,sep=' ', file=a_file)

# Metodo che permette di compiere la batch search e restituire le varie run list
# date le diverse configurazioni

#q_list sono le queries
#file è il nome del file dove salvare i risultati
def batch_search(q_list, file, esclient, INDEXNAME, SIZE):
    run_file = open(file, "w")
    r={}
    print(file)
    for query in q_list:
        num = int(query['number'])
        text = query['title']
        query_dict={
            'bool':{
                'should':[
                    {'match':{'content':text}}
                    ]
                }
            }
        response = esclient.search(index=INDEXNAME,
                                   query=query_dict,
                                   size=SIZE, )
        r[str(num)] = res(response['hits']['hits'],a_file = run_file,query = num, n = SIZE)
    run_file.close()
    return r

#Funzione per impostare i settings (BM25 o TFIDF)
def new_conf(settings, esclient, INDEXNAME):
    esclient.indices.close(index=INDEXNAME)
    # Aggiorna le impostazioni dell'indice
    esclient.indices.put_settings(index=INDEXNAME, body=settings)
    # Riapri l'indice dopo aver applicato le modifiche
    esclient.indices.open(index=INDEXNAME)

def readSettings(es, indice):
    settings = es.indices.get_settings(index=indice)

    # Estrai le configurazioni delle similitudini personalizzate
    similarity_settings = settings[indice]['settings']['index']['similarity']

    # Verifica le configurazioni delle similitudini personalizzate
    if "custom_tfidf" in similarity_settings:
        tfidf_config = similarity_settings["custom_tfidf"]
        print("\nConfigurazione in uso :TF-IDF\n")
        print('dettagli: ', tfidf_config)
    if "custom_bm25" in similarity_settings:
        bm25_config = similarity_settings["custom_bm25"]
        print("\nConfigurazione in uso: BM25\n")
        print('dettagli: ', bm25_config)

def main():
    #--- variabili globali: indice e dimensione del campione di documenti da prelevare ---#
    INDEXNAME = 'robust6'
    SIZE      = 100
    #Inizializzazione server
    esclient = esserver(['http://localhost:9200'])

# Definisci le impostazioni dell'indice con TF-IDF come analizzatore predefinito
    settings_tf = {
        "settings": {
            "similarity": {
                "custom_tfidf": {
                    "type": "scripted",
                    "script": {
                        "source": "double tf = Math.sqrt(doc.freq); double norm = 1/Math.sqrt(doc.length); return weight * tf * norm;"
                    }
                }
            }
        }
    }  
    fs  = [ "title", "content"]

    config = {}
    config["settings"] = {}
    config["settings"]["similarity"] = {}
    config["mappings"] = {}
    config["mappings"]["properties"] = {}

    for f in fs:
        fieldname = "_".join([f])
        fieldname = fieldname.replace('.','')
        weightname = "_".join(["custom_tfidf"])
        weightname = fieldname.replace('.','')
        config["settings"]["similarity"][weightname] = {}
        config["settings"]["similarity"][weightname]["type"] = "scripted"
        config["settings"]["similarity"][weightname]["type"]["script"]["source"] = Math.sqrt(doc.freq); double norm = 1/Math.sqrt(doc.length); return weight * tf * norm;
        config["mappings"]["properties"][f] = {}
        config["mappings"]["properties"][f]["type"] = "text"
        config["mappings"]["properties"][f]["copy_to"] = []
        config["mappings"]["properties"][f]["copy_to"].append(fieldname)
        config["mappings"]["properties"][fieldname] = {}
        config["mappings"]["properties"][fieldname]["type"] = "text"
        config["mappings"]["properties"][fieldname]["similarity"] = weightname  

    settings_bm = {
    "settings": {
        "similarity": {
            "custom_bm25": {
                "type": "BM25",
                "b": 0.75,  # Imposta il parametro 'b' di BM25 (valore di esempio)
                "k1": 1.2   # Imposta il parametro 'k1' di BM25 (valore di esempio)
                }
            }
        }
    }

    #Prelievo delle varie query da svolgere
    infile = open("robust2004_topics.txt",'r')
    content = infile.read()
    
    query_blocks = re.findall(r'<top>(.*?)<\/top>', content, re.DOTALL)

    queries_list = []

    for query_block in query_blocks:
        query_info = {}
        match_num = re.search(r'<num> Number: (\d+)', query_block)
        match_title = re.search(r'<title>([\s\S]+)', query_block)
        match_desc = re.search(r'<desc> Description: (.+)', query_block)
        match_narr = re.search(r'<narr> Narrative: (.+)', query_block)

        if match_num:
            query_info['number'] = match_num.group(1)
        if match_title:
            query_info['title'] = match_title.group(1)
        if match_desc:
            query_info['description'] = match_desc.group(1)
        if match_narr:
            query_info['narrative'] = match_narr.group(1)

        queries_list.append(query_info)
    
    new_conf(settings_tf, esclient, INDEXNAME)
    readSettings(esclient, INDEXNAME)
    risultato_tfidf = batch_search(queries_list, 'TF.txt', esclient, INDEXNAME, SIZE)

    new_conf(settings_bm, esclient, INDEXNAME)    
    readSettings(esclient, INDEXNAME)   
    risultato_BM25 = batch_search(queries_list, 'BM.txt', esclient, INDEXNAME, SIZE)
    
if __name__ == "__main__":
    main()