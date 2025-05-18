import sys
import math
import itertools

def llegir_entrada(nom_fitxer):
    with open(nom_fitxer, 'r') as f:
        n = int(f.readline())
        punts = [tuple(map(int, línia.split())) for línia in f]
    return n, punts

def escriure_sortida(nom_fitxer, distancia, equip1, equip2):
    with open(nom_fitxer, 'w') as f:
        f.write(f"{distancia:.6f}\n")
        f.write(' '.join(map(str, equip1)) + '\n')
        #f.write(' '.join(map(str, equip2)) + '\n')

def dist(p1, p2):
    return math.hypot(p1[0] - p2[0], p1[1] - p2[1])

def maximitza_distancia(punts):
    millor_distancia = -1
    millor_equip1, millor_equip2 = [], []
    n = len(punts) // 2

    for equip1 in itertools.combinations(range(len(punts)), n):
        equip1 = set(equip1)
        equip2 = set(range(2 * n)) - equip1
        min_dist = min(
            dist(punts[i], punts[j]) for i in equip1 for j in equip2
        )
        if min_dist > millor_distancia:
            millor_distancia = min_dist
            millor_equip1 = sorted([i + 1 for i in equip1])
            millor_equip2 = sorted([j + 1 for j in equip2])

    return millor_distancia, millor_equip1, millor_equip2

def main():
    if len(sys.argv) >= 3:
        fitxer_entrada = sys.argv[1]
        fitxer_sortida = sys.argv[2]
    else:
        fitxer_entrada = sys.stdin
        fitxer_sortida = sys.stdout

    n, punts = llegir_entrada(fitxer_entrada)
    #print("Executant l'algorisme de selecció d'equips...")
    distancia, equip1, equip2 = maximitza_distancia(punts)
    escriure_sortida(fitxer_sortida, distancia, equip1, equip2)

if __name__ == "__main__":
    main()
