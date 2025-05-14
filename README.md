# Max Min Team Partition 🧠⚖️

Algorisme per dividir 2n persones en dos equips de n membres, **maximitzant la mínima distància entre membres del mateix equip**.

---

## 🎯 Objectiu

Donats `2n` punts en el pla, es vol construir dos equips iguals de manera que la **distància mínima entre qualsevol parella del mateix equip sigui tan gran com sigui possible**.

---

## 🛠️ Enfocament

Aquesta solució **no usa força bruta**. S'utilitzen tècniques avançades per garantir eficiència:

* **Matriu de distàncies** precomputada: `O((2n)^2)`
* **Minimum Spanning Tree (Prim)** per extreure llindars rellevants
* **Cerca binària** sobre llindars candidats
* **BFS** per identificar components que no poden separar-se amb arestes curtes
* **Programació Dinàmica (subset-sum amb bitset)** per veure si és possible formar un equip de mida `n` amb components

### Complexitat

* Total: `O((2n)^2 * log n)`
* Optimitzat per a `n` relativament grans (fins a 20 o més)

---

## 📁 Fitxers

* `partition.py`: Codi principal completament comentat
* `input.txt` (opcional): Fitxer d'exemple amb format d’entrada
* `output.txt` (opcional): Fitxer amb resultat

---

## 📅 Format d'entrada

Primer una línia amb `n`. Després `2n` línies amb coordenades:

```
3
0 0
0 1
0 2
10 0
10 1
10 2
```

---

## 📄 Format de sortida

Una línia amb la millor distància mínima trobada (amb 6 decimals), i una segona línia amb els índexs (1-based) d'un dels equips:

```
1.000000
1 2 3
```

---

## 🚀 Com executar

```bash
python3 partition.py < input.txt > output.txt
```

O bé passant els fitxers com arguments:

```bash
python3 partition.py input.txt output.txt
```

---

## ✅ Exemple

**Entrada:**

```
2
0 0
0 2
10 0
10 2
```

**Sortida:**

```
2.000000
1 2
```

---

## 🧠 Notes finals

* Aquesta implementació **prioritza la claredat, comentaris i eficiència**.
* Cap part del codi empra enumeració de combinacions ni força bruta.
* Ideal per competicions o projectes amb restriccions exigents.

---

💡 Desenvolupat amb passió per l'algorítmica eficient.
