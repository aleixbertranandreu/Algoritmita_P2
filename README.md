# Partició òptima d’equips a l’illa

Aquest programa resol el problema de dividir 2k nois en dos equips de k membres cadascun de manera que **es maximitzi la distància mínima entre qualsevol parella de membres de diferents equips**.

## Funcionament

L’algorisme:
- Calcula totes les distàncies entre punts (matriu de distàncies).
- Obté possibles llindars de distància a partir del MST (arbre generador mínim).
- Fa una cerca binària sobre aquests llindars per trobar el màxim que permet dividir els nois en dos equips viables.
- Comprova la factibilitat de cada llindar amb un algorisme de components connexes i programació dinàmica.

## Ús

```bash
python3 batalla.py [input_file] [output_file]
```

Si no es proporcionen fitxers, llegeix de l’entrada estàndard i escriu a la sortida estàndard.

### Format d’entrada

- Una línia amb l’enter `k` (mida de cada equip).
- `2k` línies amb dos enters `x y`, les coordenades de cada noi.

**Exemple:**
```
2
0 0
1 1
0 1
1 0
```

### Format de sortida

- Una línia amb la distància mínima entre equips (amb precisió de 6 decimals).
- Una línia amb els índexs (1-based) d’un dels equips, separats per espais i ordenats.

**Exemple:**
```
1.000000
1 2
```

## Fitxers

- `batalla.py`: Codi principal de resolució del problema.
- `README.md`: Documentació del projecte.

## Requisits

- Python 3.6+
- Llibreries usades: només estàndard (`math`, `sys`, `collections`)

## Notes d’implementació

- La partició es basa en components connexos respecte a un llindar de distància.
- La factibilitat de formar dos equips s’avalua amb una DP de subconjunts.
- L’algorisme funciona en temps **polinòmic** per la mida del problema esperada a la pràctica.

## Autor

Aleix Bertran Andreu (48251646S) — Grau en Enginyeria Informàtica  
Bru Pallàs Vargués (48251999T)  — Grau en Enginyeria Informàtica  

Universitat de Lleida — Algorísmia i Complexitat (2024/2025)
