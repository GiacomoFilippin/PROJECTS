from elasticsearch import Elasticsearch as esserver


def main():
    INDICE_INIZIALE = 'robust2'
    NUOVO_INDICE = 'rbm'
    #Inizializzazione server
    esclient = esserver(['http://localhost:9200'])
    esclient.indices.delete(index=NUOVO_INDICE)
    aggregation_settings = {
        "settings": {
            "number_of_shards": 1,
            "number_of_replicas": 0,
            "analysis": {
                "analyzer": {
                    "custom_analyzer": {
                        "type": "custom",
                        "tokenizer": "standard",
                        "filter": ["lowercase", "asciifolding"]
                    }
                }
            },
            "number_of_shards": 1,
                    'similarity':{
                        'default':{
                            'type':'BM25',
                        }
                    }
                },
        "mappings": {
            "properties": {"content": {
                    "type": "text"},
                "lunghezza_cont": {
                    "type": "integer"
                }
            }
        }
    }

    esclient.indices.create(index=NUOVO_INDICE, body=aggregation_settings)

    # Recupera tutti i documenti dall'indice iniziale
    risultati_query = esclient.search(index=INDICE_INIZIALE, body={"query": {"match_all": {}}, "size": 10000})
    documenti_da_importare = risultati_query['hits']['hits']

    for documento in documenti_da_importare:
        documento_sorgente = documento['_source']
        lunghezza_testo = len(documento_sorgente['content'])
        documento_sorgente['lunghezza_cont'] = lunghezza_testo
        esclient.index(index=NUOVO_INDICE, body=documento_sorgente)

if __name__ == "__main__":
    main()