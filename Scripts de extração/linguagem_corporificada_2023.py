# -*- coding: utf-8 -*-
"""Linguagem Corporificada 2023

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1eh84h0tkdwISFbR9DO8BO0BvZZ1t3Rjp

# 1. Montar o Google Drive
Neste momento, precisamos ligar o Drive ao Notebook, de modo com que possamos acessar os arquivos do Drive aqui. Ao executar a função, uma janela será aberta para que você acesse a conta Google na qual você está usando o Google Colab, que é a mesma a qual você utilizará o Drive.
"""

from google.colab import drive
drive.mount('/content/drive')

"""# 2. Instalar as bibliotecas
Neste momento, são instaladas e importadas todas as bibliotecas e componentes necessários para funcionamento dos códigos.
"""

!pip install -U spacy

!pip install -U spacy-lookups-data

!python -m spacy download pt_core_news_lg

!pip install pandas

!pip install wasabi

import spacy
nlp = spacy.load('pt_core_news_lg')

"""# 3. Carregar os arquivos .txt do Google Drive
Para tanto, é necessário:
- Criar uma pasta no Google Drive contendo os arquivos .txt que você deseja processar.
- Determinar o caminho para a pasta no Google Drive. Por exemplo, se a pasta estiver localizada em "Meu Drive/Minha Pasta", o caminho será "/content/drive/MyDrive/Minha Pasta".
- Execute o seguinte código para carregar os arquivos .txt.
"""

nomes_arq = !ls '/content/drive/MyDrive/LinguagemCorporificada2023/'

"""# 4. Extração de informação dos arquivos
O seguinte código é utilizado para a extração de informações presentes na própria entrevista, como nome do arquivo, que é o que desejamos, uma vez que o nome possui informações sobre o falante (gênero, idade, deslocamento e tempo no curso) e sobre a amostra (São Cristóvão 2019, São Cristóvão 2020 ou Itabaiana).

Código para extrair informações dos nomes das entrevistas
"""

def extrai_num_ent(nome_arquivo):
    num_ent = nome_arquivo[0:65]
    return num_ent

"""# 5. Extração dos fenômenos
A partir daqui, lidamos com a extração das ocorrências dos fenômenos, cada um dos fenômenos isoladamente. Para tanto, esta seção apresenta:
- Código de busca e classificação
- Armazenamento dos resultados em arquivo .xlsx

## 5.1 Determinantes possessivos antecedendo nomes
"""

import spacy
from spacy.matcher import Matcher
from wasabi import Printer
import pandas as pd

colunas = ['Numero_ent', 'Contexto anterior', 'Ocorrencia','Contexto Seguinte', 'Contexto']
linhas = []

nlp = spacy.load('pt_core_news_lg')

nomes_arq = !ls '/content/drive/MyDrive/LinguagemCorporificada2023'

for nome in nomes_arq:
  nome_refatorado = nome.replace("'", "").strip() # remove leading/trailing spaces
  arq =f'/content/drive/MyDrive/LinguagemCorporificada2023/{nome_refatorado}'
  corpus = open(arq).read()
  doc = nlp(corpus)
  numero_ent = extrai_num_ent(nome)

  matcher = Matcher(vocab=nlp.vocab)
  detso = [{'LEMMA': {'NOT_IN': ['meu','teu', 'seu', 'nosso']}},
           {'POS': 'DET', 'MORPH':{'IS_SUPERSET': ['PronType=Prs']}},
           {'POS': 'NOUN'}]
  matcher.add('detso', [detso])
  detrex = matcher(doc, as_spans=True)
  matches = matcher(doc)

  match = Printer()
  for match_id, start, end in matches:
    ocorrencia = doc[start:end]
    contexto = doc[start-20:end+20]
    contexto_parte1 = doc[start-20:start]
    contexto_parte2 = doc[end: end+20]
    linhas.append ([numero_ent, contexto_parte1, ocorrencia,contexto_parte2, contexto])

dataframe = pd.DataFrame(linhas, columns = colunas)
dataframe.to_excel("det2023.xlsx")

"""## 5.2 Pronomes pessoais de 2PS"""

import spacy
from spacy.matcher import Matcher
from wasabi import Printer
import pandas as pd

colunas = ['Numero_ent', 'Contexto anterior', 'Ocorrencia','Contexto Seguinte', 'Contexto']
linhas = []

nlp = spacy.load('pt_core_news_lg')

nomes_arq = !ls '/content/drive/MyDrive/LinguagemCorporificada2023'

for nome in nomes_arq:
  nome_refatorado = nome.replace("'", "").strip() # remove leading/trailing spaces
  arq =f'/content/drive/MyDrive/LinguagemCorporificada2023/{nome_refatorado}'
  corpus = open(arq).read()
  doc = nlp(corpus)
  numero_ent = extrai_num_ent(nome)
  matcher = Matcher(vocab=nlp.vocab)
  detso = [{'ORTH': {'IN': ['você', 'cê', 'tu']}}, {'POS': 'VERB', 'MORPH': {'IS_SUPERSET': ['VerbForm=Fin']}}]
  inte = [{'ORTH': {'IN': ['você', 'cê', 'tu']}}, {}, {'POS': 'VERB', 'MORPH': {'IS_SUPERSET': ['VerbForm=Fin']}}] # com material interveniente

  matcher.add('detso', [detso])
  matcher.add('inte', [inte])
  detrex = matcher(doc, as_spans=True)
  matches = matcher(doc)
  match = Printer()
  for match_id, start, end in matches:
        ocorrencia = doc[start:end]
        contexto = doc[start-20:end+20]
        contexto_parte1 = doc[start-20:start]
        contexto_parte2 = doc[end: end+20]
        linhas.append ([numero_ent, contexto_parte1, ocorrencia,contexto_parte2, contexto])

dataframe = pd.DataFrame(linhas, columns = colunas)
dataframe.to_excel("pro2023.xlsx")

"""## 5.3 Clíticos de 2PS"""

import spacy
from spacy.matcher import Matcher
from wasabi import Printer
import pandas as pd

colunas = ['Numero_ent', 'Contexto anterior', 'Ocorrencia','Contexto Seguinte', 'Contexto']
linhas = []

nlp = spacy.load('pt_core_news_lg')

nomes_arq = !ls '/content/drive/MyDrive/LinguagemCorporificada2023'

for nome in nomes_arq:
  nome_refatorado = nome.replace("'", "").strip() # remove leading/trailing spaces
  arq =f'/content/drive/MyDrive/LinguagemCorporificada2023/{nome_refatorado}'
  corpus = open(arq).read()
  doc = nlp(corpus)
  numero_ent = extrai_num_ent(nome)
  matcher = Matcher(vocab=nlp.vocab)
  matcher = Matcher(vocab=nlp.vocab)
  detso = [{'ORTH': {'IN': ['te', 'lhe']}}, {'POS': 'VERB'}]
  clitc= [{'POS': 'VERB'}, {'ORTH': {'IN': ['te', 'lhe']}}]
  encli= [{'POS': 'VERB'}, {'IS_PUNCT': True}, {'ORTH': {'IN': ['te', 'lhe']}}]

  matcher.add('detso', [detso])
  matcher.add('clitc', [clitc])
  matcher.add('encli', [encli])
  detrex = matcher(doc, as_spans=True)
  matches = matcher(doc)
  match = Printer()
  for match_id, start, end in matches:
        ocorrencia = doc[start:end]
        contexto = doc[start-20:end+20]
        contexto_parte1 = doc[start-20:start]
        contexto_parte2 = doc[end: end+20]
        linhas.append ([numero_ent, contexto_parte1, ocorrencia,contexto_parte2, contexto])

dataframe = pd.DataFrame(linhas, columns = colunas)
dataframe.to_excel("cli2023.xlsx")

"""## 5.4 Possessivos de 2PS"""

import spacy
from spacy.matcher import Matcher
from wasabi import Printer
import pandas as pd

colunas = ['Numero_ent', 'Contexto anterior', 'Ocorrencia','Contexto Seguinte', 'Contexto']
linhas = []

nlp = spacy.load('pt_core_news_lg')

nomes_arq = !ls '/content/drive/MyDrive/LinguagemCorporificada2023'

for nome in nomes_arq:
  nome_refatorado = nome.replace("'", "").strip() # remove leading/trailing spaces
  arq =f'/content/drive/MyDrive/LinguagemCorporificada2023/{nome_refatorado}'
  corpus = open(arq).read()
  doc = nlp(corpus)
  numero_ent = extrai_num_ent(nome)

  matcher = Matcher(vocab=nlp.vocab)
  pre = [{"TEXT": {"IN":["seu", "sua", "seus", "suas", "teu", "tua", "teus", "tuas"]}}, {"POS": "NOUN"}]
  pos = [{"POS": "NOUN"}, {"TEXT": {"IN":["seu", "sua", "seus", "suas", "teu", "tua", "teus", "tuas"]}}]

  matcher.add('pre', [pre])
  matcher.add('pos', [pos])
  detrex = matcher(doc, as_spans=True)
  matches = matcher(doc)
  match = Printer()
  for match_id, start, end in matches:
    ocorrencia = doc[start:end]
    contexto = doc[start-20:end+20]
    contexto_parte1 = doc[start-20:start]
    contexto_parte2 = doc[end: end+20]
    linhas.append ([numero_ent, contexto_parte1, ocorrencia,contexto_parte2, contexto])

dataframe = pd.DataFrame(linhas, columns = colunas)
dataframe.to_excel("pos2023.xlsx")