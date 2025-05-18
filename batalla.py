"""Partició en dos equips maximitzant la mínima distància"""
import sys
import math
from collections import deque

def read_input(file):
    """
    Llegeix n i 2n punts des d'arxiu o stdin.
    Retorna n (tamany d'equip) i llista de punts [(x,y),...].
    """
    f = open(file) if isinstance(file, str) else file
    n = int(f.readline())
    pts = [tuple(map(int, line.split())) for line in f]
    return n, pts

def write_output(file, dist, team):
    """
    Escriu la distància (6 decimals) i la llista d'índexs de l'equip seleccionat.
    """
    f = open(file, 'w') if isinstance(file, str) else file
    f.write(f"{dist:.6f}\n")
    f.write(' '.join(map(str, sorted(team))) + '\n')

def solve(n, pts):
    """
    Retorna (millor_dist, equip_indices) amb:
      - equip_indices: n índexs (1-based) d'un dels equips
      - millor_dist: la màxima mínima distància entre equips.
    """
    n2 = 2 * n
    # 1) Matriu completa de distàncies
    dist_mat = [[0.0] * n2 for _ in range(n2)]
    for i in range(n2):
        xi, yi = pts[i]
        for j in range(i+1, n2):
            d = math.hypot(xi - pts[j][0], yi - pts[j][1])
            dist_mat[i][j] = dist_mat[j][i] = d

    # 2) Prim per MST
    in_mst = [False] * n2
    key = [float('inf')] * n2
    key[0] = 0.0
    mst_weights = []
    for _ in range(n2):
        # extreu v no inclòs amb mínima key
        u = min((v for v in range(n2) if not in_mst[v]), key=lambda x: key[x])
        in_mst[u] = True
        # si no és el primer node, guardem pes de l'arista
        if key[u] != 0.0:
            mst_weights.append(key[u])
        # relaxa veïns
        for v in range(n2):
            if not in_mst[v] and dist_mat[u][v] < key[v]:
                key[v] = dist_mat[u][v]

    # llindars candidats = pesos únics de l'MST ordenats
    thresholds = sorted(set(mst_weights))

    # adjacency global (per BFS ràpid)
    full_adj = [[(j, dist_mat[i][j]) for j in range(n2) if i != j] for i in range(n2)]

    def feasible(T):
        """
        Determina si existeix partició amb distància mínima >= T.
        Retorna (True, equip_indices) o (False, None).
        """
        seen = [False] * n2
        comps = []  # llista de components connexes
        # BFS per components amb arestes < T
        for i in range(n2):
            if not seen[i]:
                seen[i] = True
                q = deque([i])
                comp = [i]
                while q:
                    u = q.popleft()
                    for v, d in full_adj[u]:
                        if not seen[v] and d < T:
                            seen[v] = True
                            q.append(v)
                            comp.append(v)
                comps.append(comp)
        # DP subset-sum: cada component enter
        dp = 1  # bitset: dp[x]==1 si podem fer suma x
        trace = [0] * len(comps)  # bits nous per cada idx
        sizes = [len(c) for c in comps]
        for idx, sz in enumerate(sizes):
            shifted = dp << sz
            newdp = dp | shifted
            trace[idx] = newdp ^ dp
            dp = newdp
        # si no podem fer suma n, invàlid
        if not ((dp >> n) & 1):
            return False, None
        # reconstrucció equip: iterem components inversament
        team = []
        rem = n
        for i in range(len(comps)-1, -1, -1):
            if rem >= sizes[i] and ((trace[i] >> rem) & 1):
                team.extend(comps[i])
                rem -= sizes[i]
        # convertim a 1-based i retornem només n
        return True, [x+1 for x in team]

    # 4) Cerca binària sobre thresholds
    best_d = 0.0
    best_team = []
    lo, hi = 0, len(thresholds) - 1
    while lo <= hi:
        mid = (lo + hi) // 2
        T = thresholds[mid]
        ok, team = feasible(T)
        if ok:
            best_d, best_team = T, team
            lo = mid + 1
        else:
            hi = mid - 1
    return best_d, best_team

if __name__ == '__main__':
    inp = sys.argv[1] if len(sys.argv) > 1 else sys.stdin
    out = sys.argv[2] if len(sys.argv) > 2 else sys.stdout
    n, points = read_input(inp)
    distance, team = solve(n, points)
    write_output(out, distance, team)
