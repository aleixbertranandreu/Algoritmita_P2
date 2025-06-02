# Partició òptima d’equips a l’illa

Aquest programa resol el problema de dividir 2k nois en dos equips de k membres cadascun de manera que **es maximitzi la distància mínima entre qualsevol parella de membres de diferents equips**.

## Funcionament

L’algorisme implementat segueix aquests passos:

* Calcula totes les distàncies entre punts (matriu de distàncies).
* Obté possibles llindars de distància a partir dels pesos d’un MST (arbre generador mínim).
* Realitza una cerca binària sobre aquests llindars per trobar el valor màxim que permet dividir els nois en dos equips viables.
* Comprova la factibilitat de cada llindar mitjançant un algorisme de components connexos i programació dinàmica per subconjunts.

Hi ha dues versions disponibles:

* **Versió iterativa:** `batalla_iter.py` (comprova components amb BFS i cua).
* **Versió recursiva:** `batalla_rec.py` (comprova components amb DFS recursiu).

## Ús

```bash
python3 batalla_iter.py [input_file] [output_file]
python3 batalla_rec.py [input_file] [output_file]
```

Si no es proporcionen fitxers, llegeix de l’entrada estàndard i escriu a la sortida estàndard.

### Format d’entrada

* Una línia amb l’enter `k` (mida de cada equip).
* `2k` línies amb dos enters `x y`, les coordenades de cada noi.

**Exemple:**

```
2
0 0
1 1
0 1
1 0
```

### Format de sortida

* Una línia amb la distància mínima entre equips (amb precisió de 6 decimals).
* Una línia amb els índexs (1-based) d’un dels equips, separats per espais i ordenats.

**Exemple:**

```
1.000000
1 2
```

## Fitxers

* `batalla_iter.py`: Versió principal, resolució iterativa amb BFS.
* `batalla_rec.py`: Versió alternativa, resolució recursiva amb DFS.
* `README.md`: Documentació del projecte.
* `Generator.py`: Script auxiliar per generar exemples de prova.
* `Checker.py`: Script per validar la sortida dels programes amb exemples oficials.
* `Makefile`: Automatitza la compilació, execució i validació (inclou suport per Haskell i múltiples versions Python).

## Requisits

* Python 3.6 o superior.
* Llibreries utilitzades: només estàndard (`math`, `sys`, `time`, `itertools`, `collections`, `functools`).

## Notes d’implementació

* La divisió en equips es basa en components connexos respecte a un llindar de distància.
* La factibilitat de formar equips es comprova amb una **DP de tipus subset-sum**, seguint una traça binària.
* La cerca òptima del llindar es fa mitjançant **cerca binària**, millorant el rendiment respecte a una cerca lineal.
* La distància mínima es calcula a partir de la **matriu de distàncies simètrica** construïda amb `math.hypot`.

### Decorador de mesura de temps

S’ha implementat un decorador anomenat `@timeit` per mesurar el temps d’execució de la funció principal. Aquesta funcionalitat s’utilitza per avaluar l’eficiència real de l’algorisme durant el desenvolupament i proves.

```python
@timeit
def solve(k, pts):
    ...
```

### Generador de combinacions

Tot i que no s’utilitza al programa final, es va implementar un **generador de combinacions** mitjançant `itertools.combinations`, útil per validar manualment solucions a mida petita.

```python
def generate_teams(total, k):
    return itertools.combinations(range(total), k)
```

## Autors

* **Aleix Bertran Andreu** (48251646S) — Grau en Enginyeria Informàtica
* **Bru Pallàs Vargués** (48251999T) — Grau en Enginyeria Informàtica

**Universitat de Lleida — Algorísmia i Complexitat (2024/2025)**
