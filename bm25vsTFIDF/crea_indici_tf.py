from elasticsearch import Elasticsearch

# Connessione a Elasticsearch
es = Elasticsearch(['http://localhost:9200'])

#es.indices.delete(index='tfcorti')
#es.indices.delete(index='tfmedio-corti')
#es.indices.delete(index='tfmedio-lunghi')
#es.indices.delete(index='tflunghi')

# Nomi degli indici
indici_destinazione = ['tfcorti', 'tfmedio-corti', 'tfmedio-lunghi', 'tflunghi']

# Creazione degli indici
for indice in indici_destinazione:
    es.indices.create(index=indice)

# Query per ottenere i documenti da "robust2_agg"
query = {
    "query": {
        "match_all": {}
    }
}

# Indice di origine
indice_origine = "rtf"

# Ottieni i documenti da "robust2_agg"
response = es.search(index=indice_origine, body=query, size=10000)

# Copia i documenti nei 4 indici in base alla lunghezza
for hit in response['hits']['hits']:
    documento = hit["_source"]
    lunghezza = documento.get("lunghezza_cont", 0)

    if lunghezza < 1500:
        es.index(index="tfcorti", body=documento)
    elif 1500 <= lunghezza <= 2250:
        es.index(index="tfmedio-corti", body=documento)
    elif 2251 <= lunghezza <= 2750:
        es.index(index="tfmedio-lunghi", body=documento)
    elif lunghezza > 2750:
        es.index(index="tflunghi", body=documento)

print("Copia dei documenti completata.")
