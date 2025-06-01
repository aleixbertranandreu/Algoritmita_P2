"""Partició en dos equips maximitzant la mínima distància"""
import sys
import math
import time
import functools
import itertools
from collections import deque


def timeit(func):
    """Decorador per mesurar el temps d'execució d'una funció."""
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        start = time.time()
        result = func(*args, **kwargs)
        end = time.time()
        print(f"[INFO] Temps d'execució de {func.__name__}: {end - start:.6f} segons")
        return result
    return wrapper


def generate_teams(total, k):
    """Generador de combinacions d'equips de mida k d'entre total nois."""
    return itertools.combinations(range(total), k)


def read_input(source):
    """Llegeix k i 2k punts"""
    with open(source, encoding='utf-8') if isinstance(source, str) else source as f:
        k = int(f.readline())
        pts = [tuple(map(int, line.split())) for line in f]
    return k, pts


def write_output(dest, dist, team):
    """Escriu la distància i l'equip triat"""
    with open(dest, 'w', encoding='utf-8') if isinstance(dest, str) else dest as f:
        f.write(f"{dist:.6f}\n")
        f.write(" ".join(map(str, sorted(team))) + "\n")


def compute_dist_matrix(pts):
    """Crea matriu de distàncies"""
    size = len(pts)
    dist = [[0.0] * size for _ in range(size)]
    for i in range(size):
        xi, yi = pts[i]
        for j in range(i + 1, size):
            d = math.hypot(xi - pts[j][0], yi - pts[j][1])
            dist[i][j] = dist[j][i] = d
    return dist


def compute_mst_thresholds(dist):
    """Calcula pesos d'arbre generador mínim"""
    size = len(dist)
    used = [False] * size
    key = [float('inf')] * size
    key[0] = 0.0
    thresholds = []
    for _ in range(size):
        u = min((i for i in range(size) if not used[i]), key=lambda x: key[x])
        used[u] = True
        if key[u] != 0.0:
            thresholds.append(key[u])
        for v in range(size):
            if not used[v] and dist[u][v] < key[v]:
                key[v] = dist[u][v]
    return sorted(set(thresholds))


def is_feasible(threshold, pts, adj, k):
    """Verifica si es pot fer una partició amb distància >= threshold"""
    total = len(pts)
    seen = [False] * total
    comps = []

    for i in range(total):
        if not seen[i]:
            seen[i] = True
            queue = deque([i])
            comp = [i]
            while queue:
                u = queue.popleft()
                for v, d in adj[u]:
                    if not seen[v] and d < threshold:
                        seen[v] = True
                        queue.append(v)
                        comp.append(v)
            comps.append(comp)

    dp = 1
    trace = [0] * len(comps)
    sizes = [len(c) for c in comps]
    for idx, sz in enumerate(sizes):
        shifted = dp << sz
        newdp = dp | shifted
        trace[idx] = newdp ^ dp
        dp = newdp

    if not (dp >> k) & 1:
        return False, None

    team = []
    rem = k
    for i in range(len(comps) - 1, -1, -1):
        if rem >= sizes[i] and (trace[i] >> rem) & 1:
            team.extend(comps[i])
            rem -= sizes[i]
    return True, [x + 1 for x in team]


def binary_search_threshold(thresholds, pts, adj, k):
    """Fa cerca binària sobre els llindars per trobar el millor"""
    lo = 0
    hi = len(thresholds) - 1
    best_d = 0.0
    best_team = []

    while lo <= hi:
        mid = (lo + hi) // 2
        thresh = thresholds[mid]
        ok, team = is_feasible(thresh, pts, adj, k)
        if ok:
            best_d = thresh
            best_team = team
            lo = mid + 1
        else:
            hi = mid - 1

    return best_d, best_team


@timeit
def solve(k, pts):
    """Retorna distància i equip òptim"""
    total = 2 * k
    dist = compute_dist_matrix(pts)
    adj = [[(j, dist[i][j]) for j in range(total) if i != j] for i in range(total)]
    thresholds = compute_mst_thresholds(dist)
    return binary_search_threshold(thresholds, pts, adj, k)


if __name__ == "__main__":
    infile = sys.argv[1] if len(sys.argv) > 1 else sys.stdin
    outfile = sys.argv[2] if len(sys.argv) > 2 else sys.stdout
    k_val, pts_list = read_input(infile)
    best_dist, team_result = solve(k_val, pts_list)
    write_output(outfile, best_dist, team_result)
