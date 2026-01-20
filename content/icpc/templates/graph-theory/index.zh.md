---
title: "Graph Theory"
language: zh
canonical: true
translationKey: icpc-templates-graph-theory
math: true
toc: true
draft: false
---


# 最短路

## Dijkstra

```cpp
long long int dist[VERTEX_SIZE];
bool vis[VERTEX_SIZE];
void dijkstra(int startPt) {
    for (int i = 0; i < vertexNum; i++)
        vis[i] = false, dist[i] = LLONG_MAX;
    priority_queue<pair<long long int, int> > pq;
    pq.push(make_pair(0, startPt));
    dist[startPt] = 0;
    while (!pq.empty()) {
        int cntPt = pq.top().second; pq.pop();
        if (vis[cntPt])
            continue;
        vis[cntPt] = true;
        for (int i = head[cntPt]; i != -1; i = edges[i].next) {
            int nextPt = edges[i].to;
            if (dist[nextPt] > dist[cntPt] + edges[i].len) {
                dist[nextPt] = dist[cntPt] + edges[i].len;
                pq.push(make_pair(-dist[nextPt], nextPt));
            }
        }
    }
}
```

# Steiner Tree

求权值和最小的边集使得给定的 $k$ 个点联通。

$dp[st][i]$ 代表以 $i$ 为根，必选点联通状态为 $st$ 时最小边权和。

不变根的状态转移（线性 DP）：

$$
dp[st][i] = \min\limits_{s' \in st} \{ dp[s'][i] + dp[st - s'][i] \}
$$

变根的状态转移（图上 DP）：

$$
dp[st][u] = \min\limits_{u \rightarrow v} \{ dp[st][v] + dis(u, v) \}
$$

```cpp
priority_queue<
    pair<int, int>,
    vector<pair<int, int> >,
    greater<pair<int, int> >
> pq;

void dijkstra(int st) {
    while (!pq.empty()) {
        auto p = pq.top(); pq.pop();
        if (p.first > dp[st][p.second])
            continue;
        for (int i = head[p.second]; i != -1; i = edges[i].next) {
            int nextPt = edges[i].to;
            if (dp[st][nextPt] > dp[st][p.second] + edges[i].cost) {
                dp[st][nextPt] = dp[st][p.second] + edges[i].cost;
                pq.push(make_pair(dp[st][nextPt], nextPt));
            }
        }
    }
}

// Key points: Vertex labeled [0 ~ keyNum - 1] 
for (int st = 0; st < (1 << keyNum); st++)
    fill(dp[st] + 0, dp[st] + vertexNum, INT_MAX);
    for (int i = 0; i < keyNum; i++)
        dp[1 << i][i] = 0;

for (int st = 0; st < (1 << keyNum); st++) {
    for (int i = 0; i < vertexNum; i++) {
        for (int subst = st & (st - 1); subst > 0; subst = st & (subst - 1))
            if (dp[subst][i] != INT_MAX && dp[st ^ subst][i] != INT_MAX)
                dp[st][i] = min(dp[st][i], dp[subst][i] + dp[st ^ subst][i]);
        if (dp[st][i] != INT_MAX)
            pq.push(make_pair(dp[st][i], i));
    }
    dijkstra(st);
}
```

如果要求斯坦纳森林，则需要再 DP 一次。记 $f[st] = \min \{ dp[st][i] \}$，然后再按题意对 $f$ 进行合并即可。例如：必须保证相同频率的频道相连，就可以对于每个 $st$ 枚举 $subst$，然后分别检查 $subst$ 和 $st \oplus subst$ 是否合法。如果合法，则进行合并。

# 最大团 / 极大团

## 最大团

```cpp
bool arr[SIZE][SIZE];
int path[SIZE], dp[SIZE]; // dp[i]: number of vertices in maximum clique of G{i ~ N}
int vertexNum, ans;	// Size of maximum clinque

void dfs(int cntPt, int cntNum) {
    ans = max(ans, cntNum);
    if (ans >= pickNum)
        return;
    if (cntNum + vertexNum - cntPt - 1 <= ans)
        return;
    for (int i = cntPt + 1; i < vertexNum; i++) {
        if (dp[i] + cntNum <= ans)
            return;
        if (canAdd(i, cntNum)) {
            path[cntNum] = i; dfs(i, cntNum + 1);
        }
        if (ans >= pickNum)
            return;
    }
    return;
}

void maximumClinque() {
    ans = 1; dp[vertexNum - 1] = 1;
    for (int i = vertexNum - 2; i >= 0; i--) {
        path[0] = i; dfs(i, 1); dp[i] = ans;
    }
}
```

## 极大团

```cpp
bool arr[SIZE][SIZE];
int all[SIZE][SIZE], some[SIZE][SIZE], none[SIZE][SIZE];
long long int ans;	// Number of maximal clinques

void dfs(int depth, int allNum, int someNum, int noneNum) {
    if (someNum == 0 && noneNum == 0)
        ans++;
    int pivot = some[depth][0];
    for (int i = 0; i < someNum; i++) {
        int cntVertex = some[depth][i];
        if (arr[pivot][cntVertex])
            continue;
        for (int j = 0; j < allNum; j++)
            all[depth + 1][j] = all[depth][j];
        all[depth + 1][allNum] = cntVertex;
        int nxtSomeNum = 0, nxtNoneNum = 0;
        for (int j = 0; j < someNum; j++)
            if (some[depth][j] != -1 && arr[cntVertex][some[depth][j]])
                some[depth + 1][nxtSomeNum++] = some[depth][j];
        for (int j = 0; j < noneNum; j++)
            if (arr[cntVertex][none[depth][j]])
                none[depth + 1][nxtNoneNum++] = none[depth][j];

        dfs(depth + 1, allNum + 1, nxtSomeNum, nxtNoneNum);
        some[depth][i] = -1;
        none[depth][noneNum++] = cntVertex;
    }
}

void bronKerbosch(int vertexNum) {
    for (int i = 0; i < vertexNum; i++)
        some[0][i] = i;
    ans = 0; dfs(0, 0, vertexNum, 0);
}
```

# 拓补排序

```cpp
void bfs() {
    priority_queue<int> pq;
    for (int i = 0; i < vertexNum; i++)
        if (inDeg[i] == 0)
            pq.push(i);
    ansPt = 0;
    while (!pq.empty()) {
        int cntPt = pq.top(); pq.pop();
        ans[ansPt++] = cntPt;
        int edgePt = head[cntPt];
        while (edgePt != -1) {
            inDeg[edges[edgePt].to]--;
            if (inDeg[edges[edgePt].to] == 0)
                pq.push(edges[edgePt].to);
            edgePt = edges[edgePt].next;
        }
        head[cntPt] = -1;
    }
}
```

# 差分约束

- 最大解：形如 $x_i - x_j \le c_k$，连边 $j \rightarrow i$ 权值 $c_k$，并且用 SPFA 算最短路；
- 最小解：形如 $x_i - x_j \ge c_k$，连边 $j \rightarrow i$ 权值 $c_k$，并且用 SPFA 算最长路；
- 若需要常数，可令一项为常数项并对其进行特殊限制或 check。

```cpp
int dist[VERTEX_NUM], cnt[VERTEX_NUM], vertexNum;
bool inQueue[VERTEX_NUM];
// Shortest path variant
bool spfa() {
    queue<int> q;
    for (int i = 0; i < vertexNum; i++) {
        dist[i] = 0;	// Lower or upper limit for each x
        cnt[i] = 0; q.push(i); inQueue[i] = true;
    }

    while (!q.empty()) {
        int cntPt = q.front(); q.pop();
        inQueue[cntPt] = false;
        for (int i = head[cntPt]; i != -1; i = edges[i].next) {
            int nextPt = edges[i].to;
            if (dist[nextPt] > dist[cntPt] + edges[i].len) {
                dist[nextPt] = dist[cntPt] + edges[i].len;
                cnt[nextPt] = cnt[cntPt] + 1;
                if (cnt[nextPt] >= vertexNum)
                    return false;
                if (!inQueue[nextPt]) {
                    q.push(nextPt), inQueue[nextPt] = true;
                }
            }
        }
    }
    return true;
}
```

# 2-SAT

## 建图

对命题 $i$ 假/真 拆点成 $P_i, \ P_i'$；若 $i$ 为真则 $j$ 为真建有向边：$P_i \rightarrow P_j$。

本模板中 `FALSE: (i << 1); TRUE: (i << 1 | 1)`。

## 字典序最小/大解

复杂度：$\mathcal{O}(N^2)$

```cpp
// Lexicographically smallest variant
bool sel[VERTEX_SIZE];
int vertexNum; stack<int> stk; 

bool dfs(int cntPt) {
    if (sel[cntPt ^ 1])
        return false;
    if (sel[cntPt])
        return true;
    sel[cntPt] = true; stk.push(cntPt);
    for (int i = head[cntPt]; i != -1; i = edges[i].next) {
        int nextPt = edges[i].to;
        if (!dfs(nextPt))
            return false;
    }
    return true;
}

bool twoSat() {
    fill(sel + 0, sel + vertexNum, false);
    for (int i = 0; i < vertexNum; i += 2) {
        if (!sel[i] && !sel[i ^ 1]) {
            while (!stk.empty())
                stk.pop();
            if (!dfs(i)) {
                while (!stk.empty()) {
                    int cntTop = stk.top();
                    stk.pop();
                    sel[cntTop] = false;
                }
                if (!dfs(i ^ 1))
                    return false;
            }
        }
    }
    return true;
}
```

## 任意解

建正图和反图，在正图中跑 Tarjan SCC，同一 SCC 中的所有点必然同时取或不取。如果同一命题的两种状态在同一 SCC 中说明无解。若有解则在反图中按照拓补序构造方案（取正图中拓补序较大者为真）。

复杂度：$\mathcal{O}(N)$

```cpp
class Edge {
public:
    int to, next;
};
Edge edges[2][SIZE << 2];
int head[2][SIZE], edgesPt[2], vertexNum;
int quit[SIZE], sccId[SIZE], cntTime; bool vis[SIZE];

void addEdge(int id, int from, int to) {
    edges[id][edgesPt[id]] = {to, head[id][from]};
    head[id][from] = edgesPt[id]++;
}

void dfs1(int cntPt) {
    vis[cntPt] = true;
    for (int i = head[0][cntPt]; i != -1; i = edges[0][i].next) {
        int nextPt = edges[0][i].to;
        if (!vis[nextPt])
            dfs1(nextPt);
    }
    quit[cntTime++] = cntPt;
}

void dfs2(int cntPt, int sccPt) {
    vis[cntPt] = true; sccId[cntPt] = sccPt;
    for (int i = head[1][cntPt]; i != -1; i = edges[1][i].next) {
        int nextPt = edges[1][i].to;
        if (!vis[nextPt])
            dfs2(nextPt, sccPt);
    }
}

bool twoSat() {
    fill(vis + 0, vis + vertexNum, false);
    for (int i = 0; i < vertexNum; i++)
        if (!vis[i])
            dfs1(i);
    fill(vis + 0, vis + vertexNum, false);
    int sccPt = 0;
    for (int i = cntTime - 1; i >= 0; i--)
        if (!vis[quit[i]])
            dfs2(quit[i], sccPt++);
    for (int i = 0; i < vertexNum; i += 2)
        if (sccId[i] == sccId[i ^ 1]) 
            return false;
    return true;
}

// Output solution
for (int i = 0; i < len; i++)
    cout << (sccId[i << 1] > sccId[i << 1 | 1] ? "false" : "true");
```

# 连通性 (Tarjan)

## 无向图双连通分量

### e-DCC

```cpp
int dfn[VERTEX_SIZE], low[VERTEX_SIZE], cntTime;
int edccId[VERTEX_SIZE], edccNum;
bool isBridge[EDGE_SIZE]; int bridgeNum;

void tarjan(int cntPt, int edgeId) {
    dfn[cntPt] = cntTime; low[cntPt] = cntTime; cntTime++;
    for (int i = head[cntPt]; i != -1; i = edges[i].next) {
        int nextPt = edges[i].to;
        if (dfn[nextPt] == -1) {
            tarjan(nextPt, i);
            low[cntPt] = min(low[cntPt], low[nextPt]);
            if (low[nextPt] > dfn[cntPt]) {
                isBridge[i] = true; isBridge[i ^ 1] = true;
                bridgeNum++;
            }
        } else if (i != (edgeId ^ 1)) {
            low[cntPt] = min(low[cntPt], dfn[nextPt]);
        }
    }
}

void dfs(int cntPt) {
    edccId[cntPt] = edccNum;
    for (int i = head[cntPt]; i != -1; i = edges[i].next) {
        int nextPt = edges[i].to;
        if (edccId[nextPt] > -1 || isBridge[i])
            continue;
        dfs(nextPt);
    }
}

memset(dfn, -1, sizeof(dfn));
memset(edccId, -1, sizeof(edccId));
memset(isBridge, false, sizeof(isBridge));
cntTime = 0, edccNum = 0, bridgeNum = 0;
for (int i = 0; i < vertexNum; i++)
    if (dfn[i] == -1)
        tarjan(i, 0);
for (int i = 0; i < vertexNum; i++) 
    if (edccId[i] == -1)
        dfs(i), edccNum++;
```

### v-DCC

**Mind that `VERTEX_SIZE` and `EDGE_SIZE` should be 2x larger if constructing new graph is necessary.**

```cpp
int dfn[VERTEX_SIZE], low[VERTEX_SIZE], cntTime;
stack<int> stk;
vector<int> vdcc[VERTEX_SIZE];
int cntRoot, vdccNum;

bool isCut[VERTEX_SIZE];
int vdccId[VERTEX_SIZE], newId[VERTEX_SIZE], edgeVdccId[EDGE_SIZE];

void tarjan(int cntPt) {
    dfn[cntPt] = cntTime; low[cntPt] = cntTime; cntTime++; 
    stk.push(cntPt);
    if (cntPt == cntRoot && head[cntPt] == -1) {
        vdcc[vdccNum++].push_back(cntPt);
        return;
    }

    bool isFirst = true;
    for (int i = head[cntPt]; i != -1; i = edges[i].next) {
        int nextPt = edges[i].to;
        if (dfn[nextPt] == -1) {
            tarjan(nextPt);
            low[cntPt] = min(low[cntPt], low[nextPt]);
            if (low[nextPt] >= dfn[cntPt]) {
                if (cntPt != cntRoot || !isFirst) 
                    isCut[cntPt] = true;
                isFirst = false;
                while (!stk.empty()) {
                    int cntTop = stk.top(); stk.pop();
                    vdcc[vdccNum].push_back(cntTop);
                    if (cntTop == nextPt)
                        break;
                }
                vdcc[vdccNum++].push_back(cntPt);
            }
        } else {
            low[cntPt] = min(low[cntPt], dfn[nextPt]);
        }
    }
}

memset(head, -1, sizeof(head));
memset(dfn, -1, sizeof(dfn));
memset(vdccId, -1, sizeof(vdccId));
memset(isCut, false, sizeof(isCut));
cntTime = 0, vdccNum = 0;
edgePt = 0;
for (int i = 0; i < vertexNum; i++)
    vdcc[i].clear();
for (int i = 0; i < vertexNum; i++)
    if (dfn[i] == -1)
        cntRoot = i, tarjan(i);


// Construct new graph (tree)
int newVertexNum = vdccNum;
for (int i = 0; i < vertexNum; i++)
    if (isCut[i])
        newId[i] = newVertexNum++;
for (int i = 0; i < vdccNum; i++) {
    for (const auto & j : vdcc[i]) {
        if (isCut[j]) {
            addEdge(1, i, newId[j]);
            addEdge(1, newId[j], i);
        }
        vdccId[j] = i;
    }

    for (const auto & j : vdcc[i]) {
        for (int k = head[0][j]; k != -1; k = edges[0][k].next) {
            int nextPt = edges[0][k].to;
            if (vdccId[nextPt] == i) {
                edgeVdccId[k >> 1] = i;
            }
        }
    }
}
```

## 有向图强连通分量

```cpp
int dfn[VERTEX_SIZE], low[VERTEX_SIZE], cntTime;
bool inStack[VERTEX_SIZE]; stack<int> stk;
int sccId[VERTEX_SIZE], sccSize[VERTEX_SIZE], sccNum;

void tarjan(int cntPt) {
    dfn[cntPt] = cntTime; low[cntPt] = cntTime; cntTime++;
    stk.push(cntPt); inStack[cntPt] = true;
    for (int i = head[cntPt]; i != -1; i = edges[i].next) {
        int nextPt = edges[i].to;
        if (dfn[nextPt] == -1) {
            tarjan(nextPt);
            low[cntPt] = min(low[cntPt], low[nextPt]);
        } else if (inStack[nextPt]) {
            low[cntPt] = min(low[cntPt], dfn[nextPt]);
        }
    }
    if (dfn[cntPt] == low[cntPt]) {
        while (!stk.empty()) {
            int cntTop = stk.top(); stk.pop();
            sccId[cntTop] = sccNum;
            sccSize[sccNum]++;
            inStack[cntTop] = false;
            if (cntTop == cntPt)
                break;
        }
        sccNum++;
    }
}

fill(dfn + 0, dfn + vertexNum, -1);
fill(inStack + 0, inStack + vertexNum, false);
fill(sccSize + 0, sccSize + vertexNum, 0);
cntTime = 0, sccNum = 0;
for (int i = 0; i < vertexNum; i++)
    if (dfn[i] == -1)
        tarjan(i);
```

# 图的匹配

## 二分图

### 无权 Hungarian

```cpp
bool arr[SIZE][SIZE], sndVisited[SIZE];
int sndMatch[SIZE];
int fstNum, sndNum;

bool canFind(int fstId) {
    for (int i = 0; i < sndNum; i++) {
        if (arr[fstId][i] && !sndVisited[i]) {
            sndVisited[i] = true;
            if (sndMatch[i] == -1 || canFind(sndMatch[i])) {
                sndMatch[i] = fstId;
                return true;
            }
        }
    }
    return false;
}

int hungarian() {
    int ret = 0; memset(sndMatch, -1, sizeof(sndMatch));
    for (int i = 0; i < fstNum; i++) {
        memset(sndVisited, false, sizeof(sndVisited));
        ret += canFind(i);
    }
    return ret;
}
// DON'T FORGET TO SET fstNum and sndNum
```

### 带权 KM (必须完备匹配): Based on BFS

```cpp
int arr[SIZE][SIZE], fstNum, sndNum;
int fstEx[SIZE], sndEx[SIZE];
int sndMatch[SIZE], sndNeed[SIZE], pre[SIZE];
bool sndVisited[SIZE];

void bfs(int fstId) {
    for (int i = 0; i < sndNum; i++) {
        sndVisited[i] = false;
        sndNeed[i] = INT_MAX;
        pre[i] = -1;
    }
    int cntSnd = -1;
    while (cntSnd == -1 || sndMatch[cntSnd] != -1) {
        int cntFst;
        if (cntSnd == -1) {
            cntFst = fstId;
        } else {
            cntFst = sndMatch[cntSnd];
            sndVisited[cntSnd] = true;
        }
        int minDelta = INT_MAX, minSnd = -1;
        for (int i = 0; i < sndNum; i++) {
            if (!sndVisited[i]) {
                if (sndNeed[i] > fstEx[cntFst] + sndEx[i] - arr[cntFst][i]) {
                    sndNeed[i] = fstEx[cntFst] + sndEx[i] - arr[cntFst][i];
                    pre[i] = cntSnd;
                }
                if (sndNeed[i] < minDelta) {
                    minDelta = sndNeed[i];
                    minSnd = i;
                }
            }
        }

        fstEx[fstId] -= minDelta;
        for (int i = 0; i < sndNum; i++) {
            if (sndVisited[i]) {
                fstEx[sndMatch[i]] -= minDelta;
                sndEx[i] += minDelta;
            } else {
                sndNeed[i] -= minDelta;
            }
        }
        cntSnd = minSnd;
    }

    while (cntSnd != -1) {
        if (pre[cntSnd] == -1)
            sndMatch[cntSnd] = fstId;
        else
            sndMatch[cntSnd] = sndMatch[pre[cntSnd]];
        cntSnd = pre[cntSnd];
    }
}

int hungarian() {
    for (int i = 0; i < sndNum; i++) {
        sndMatch[i] = -1;
        sndEx[i] = 0;
    }
    for (int i = 0; i < fstNum; i++) {
        fstEx[i] = arr[i][0];
        for (int j = 1; j < sndNum; j++)
            fstEx[i] = max(fstEx[i], arr[i][j]);
    }
    for (int i = 0; i < sndNum; i++)
        bfs(i);

    int ans = 0;
    for (int i = 0; i < sndNum; i++)
        if (sndMatch[i] != -1)
            ans += arr[sndMatch[i]][i];
    return ans;
}
```

### Hopcroft-Karp

```cpp
class Edge {
public:
    int to, next;
};
Edge edges[SIZE << 1];
int head[SIZE << 1], edgesPt; 
int fstMatch[SIZE], sndMatch[SIZE], fstDist[SIZE], sndDist[SIZE];
int fstNum, sndNum; bool sndVis[SIZE], vis[SIZE << 1];

void addEdge(int from, int to) {
    edges[edgesPt] = {to, head[from]};
    head[from] = edgesPt++;
}

bool canFind(int fstPt) {
    for (int i = head[fstPt]; i != -1; i = edges[i].next) {
        int sndPt = edges[i].to - fstNum; assert(sndPt >= 0);
        if (sndVis[sndPt] || sndDist[sndPt] != fstDist[fstPt] + 1)
            continue;
        sndVis[sndPt] = true;
        if (sndMatch[sndPt] == -1 || canFind(sndMatch[sndPt])) {
            fstMatch[fstPt] = sndPt;
            sndMatch[sndPt] = fstPt;
            return true;
        }
    }
    return false;
}

bool findAugPath() {
    bool flag = false; queue<int> q;
    fill(fstDist + 0, fstDist + fstNum, 0);
    fill(sndDist + 0, sndDist + sndNum, 0);
    for (int i = 0; i < fstNum; i++)
        if (fstMatch[i] == -1)
            q.push(i);
    while (!q.empty()) {
        int fstPt = q.front(); q.pop();
        for (int i = head[fstPt]; i != -1; i = edges[i].next) {
            int sndPt = edges[i].to - fstNum; assert(sndPt >= 0);
            if (sndDist[sndPt] != 0)
                continue;
            sndDist[sndPt] = fstDist[fstPt] + 1;
            if (sndMatch[sndPt] != -1) {
                fstDist[sndMatch[sndPt]] = sndDist[sndPt] + 1;
                q.push(sndMatch[sndPt]);
            } else {
                flag = true;
            }
        }
    }
    return flag;
}

int hopcroftKarp() {
    fill(fstMatch + 0, fstMatch + fstNum, -1);
    fill(sndMatch + 0, sndMatch + sndNum, -1);
    int ret = 0;
    while (findAugPath()) {
        fill(sndVis + 0, sndVis + sndNum, false);
        for (int i = 0; i < fstNum; i++)
            ret += (fstMatch[i] == -1 && canFind(i));
    }
    return ret;
}
```

## 一般图

```cpp
/* 带花树 */
/* 咕咕咕 */
```

# 网络流

## 最大流 Dinic

```cpp
class Edge {
public:
    int to, next, cap;
};
 
Edge edges[EDGE_SIZE];
int head[VERTEX_SIZE], edgesPt;
int depth[VERTEX_SIZE], lastVis[VERTEX_SIZE], vertexNum;
 
void addEdge(int from, int to, int cap){
    edges[edgesPt] = {to, head[from], cap};
    head[from] = edgesPt++;
    edges[edgesPt] = {from, head[to], 0};
    head[to] = edgesPt++;
}
 
bool updateDepth(int startPt, int endPt) {
    memset(depth, -1, sizeof(depth));
    queue<int> q;
    depth[startPt] = 0;
    q.push(startPt);
    while (!q.empty()) {
        int cntPt = q.front();
        q.pop();
        for (int i = head[cntPt]; i != -1; i = edges[i].next) {
            int nextPt = edges[i].to;
            if (depth[nextPt] == -1 && edges[i].cap > 0) {
                depth[nextPt] = depth[cntPt] + 1;
                if (nextPt == endPt)
                    return true;
                q.push(nextPt);
            }
        }
    }
    return false;
}
 
int findAugPath(int cntPt, int endPt, int minCap) {
    if (cntPt == endPt)
        return minCap;
    int cntFlow = 0;
    for (int & i = lastVis[cntPt]; i != -1; i = edges[i].next) {
        int nextPt = edges[i].to;
        if (depth[nextPt] == depth[cntPt] + 1 && edges[i].cap > 0) {
            int flowInc = findAugPath(nextPt, endPt, min(minCap - cntFlow, edges[i].cap));
            if (flowInc == 0) {
                depth[nextPt] = -1;
            } else {
                edges[i].cap -= flowInc;
                edges[i ^ 1].cap += flowInc;
                cntFlow += flowInc;
                if (cntFlow == minCap)
                    break;
            }
        }
    }
    return cntFlow;
}
 
int dinic(int srcPt, int snkPt) {
    int ret = 0;
    while (updateDepth(srcPt, snkPt)) {
        for (int i = 0; i < vertexNum; i++)
            lastVis[i] = head[i];
        int flowInc =  findAugPath(srcPt, snkPt, INT_MAX);
        if (flowInc == 0)
            break;
        ret += flowInc;
    }
    return ret;
}

// DON'T FORGET TO SET vertexNum
int ans = dinic(srcPt, snkPt);
```

## 最小费用最大流 EK

```cpp
typedef struct _Edge {
    int to, next;
    int cap, cost;
} Edge;

Edge edges[EDGE_SIZE];
int head[VERTEX_SIZE], edgePt;
int dist[VERTEX_SIZE], minCap[VERTEX_SIZE], pre[VERTEX_SIZE];
bool inQueue[VERTEX_SIZE];
int vertexNum, edgeNum, expFlow;

void addEdge(int from, int to, int cap, int cost) {
    edges[edgePt] = {to, head[from], cap, cost};
    head[from] = edgePt++;
    edges[edgePt] = {from, head[to], 0, -cost};
    head[to] = edgePt++;
}

void updCap(int startPt, int endPt) {
    int cntPt = endPt;
    while (cntPt != startPt) {
        int edgePt = pre[cntPt];
        edges[edgePt].cap -= minCap[endPt];
        edges[edgePt ^ 1].cap += minCap[endPt];
        cntPt = edges[edgePt ^ 1].to;
    }
}

bool findAugPath(int startPt, int endPt) {
    for (int i = 0; i < vertexNum; i++)
        dist[i] = INT_MAX, inQueue[i] = false;

    queue<int> q; q.push(startPt);
    dist[startPt] = 0;
    minCap[startPt] = INT_MAX;
    inQueue[startPt] = true;

    while (!q.empty()) {
        int cntPt = q.front(); q.pop();
        inQueue[cntPt] = false;

        int edgePt = head[cntPt];
        while (edgePt != -1) {
            if (edges[edgePt].cap > 0 && dist[cntPt] != INT_MAX && dist[edges[edgePt].to] > dist[cntPt] + edges[edgePt].cost) {
                dist[edges[edgePt].to] = dist[cntPt] + edges[edgePt].cost;
                minCap[edges[edgePt].to] = min(minCap[cntPt], edges[edgePt].cap);
                pre[edges[edgePt].to] = edgePt;

                if (!inQueue[edges[edgePt].to]) {
                    q.push(edges[edgePt].to);
                    inQueue[edges[edgePt].to] = true;
                }
            }
            edgePt = edges[edgePt].next;
        }
    }

    if (dist[endPt] == INT_MAX)
        return false;
    return true;
}

void edmondsKarp(int startPt, int endPt, int & flow, int & cost) {
    while (findAugPath(startPt, endPt)) {
        updCap(startPt, endPt);
        flow += minCap[endPt];
        cost += dist[endPt] * minCap[endPt];
    }
}
// DON'T FORGET TO SET vertexNum
int flow = 0, cost = 0;
edmondsKarp(0, vertexNum - 1, flow, cost);
```

## 最小费用最大流 zkw

```cpp
typedef struct _Edge {
    int to, next;
    int cap, cost;
} Edge;

Edge edges[EDGE_SIZE];
int head[VERTEX_SIZE], edgePt;
int dist[VERTEX_SIZE], lastVisitedEdge[VERTEX_SIZE]; 
bool inQueue[VERTEX_SIZE], vis[VERTEX_SIZE];
int vertexNum;

void addEdge(int from, int to, int cap, int cost) {
    edges[edgePt] = {to, head[from], cap, cost};
    head[from] = edgePt++;
    edges[edgePt] = {from, head[to], 0, -cost};
    head[to] = edgePt++;
}

bool isFullFlow(int startPt, int endPt) {
    for (int i = 0; i < vertexNum; i++)
        dist[i] = INT_MAX, inQueue[i] = false;
    dist[startPt] = 0;
    queue<int> q; q.push(startPt);
    inQueue[startPt] = true;

    while (!q.empty()) {
        int cntPt = q.front();
        q.pop();
        inQueue[cntPt] = false;
        for (int i = head[cntPt]; i != -1; i = edges[i].next) {
            int nextPt = edges[i].to;
            if (edges[i ^ 1].cap != 0 && dist[nextPt] > dist[cntPt] - edges[i].cost) {
                dist[nextPt] = dist[cntPt] - edges[i].cost;
                if (!inQueue[nextPt]) {
                    inQueue[nextPt] = true;
                    q.push(nextPt);
                }
            }
        }
    }
    return dist[endPt] != INT_MAX;
}

int findAugPath(int cntPt, int endPt, int minCap, int & cost) {
    if (cntPt == endPt) {
        vis[endPt] = true;
        return minCap;
    }

    int cntFlow = 0;
    vis[cntPt] = true;
    for (int & i = lastVisitedEdge[cntPt]; i != -1; i = edges[i].next) {
        int nextPt = edges[i].to;
        if (vis[nextPt])
            continue;
        if (dist[cntPt] - edges[i].cost == dist[nextPt] && edges[i].cap > 0) {
            int flowInc = findAugPath(nextPt, endPt, min(minCap - cntFlow, edges[i].cap), cost);
            if (flowInc != 0) {
                cost += flowInc * edges[i].cost;
                edges[i].cap -= flowInc;
                edges[i ^ 1].cap += flowInc;
                cntFlow += flowInc;
            }
            if (cntFlow == minCap)
                break;
        }
    }
    return cntFlow;
}

void zkw(int startPt, int endPt, int & flow, int & cost) {
    flow = 0, cost = 0;
    while (isFullFlow(endPt, startPt)) {
        vis[endPt] = true;
        while (vis[endPt]) {
            for (int i = 0; i < vertexNum; i++)
                lastVisitedEdge[i] = head[i], vis[i] = false;
            flow += findAugPath(startPt, endPt, INT_MAX, cost);
        }
    }
}
// DON'T FORGET TO SET vertexNum
int flow = 0, cost = 0;
zkw(0, vertexNum - 1, flow, cost);
```

# 仙人掌上找环

```cpp
stack<int> stk;
bool vis[VERTEX_SIZE], inStack[VERTEX_SIZE];

void dfs(int cntPt, int prevPt) {
    vis[cntPt] = true;
    stk.push(cntPt); inStack[cntPt] = true;
    for (int i = head[cntPt]; i != -1; i = edges[i].next) {
        int nextPt = edges[i].to;
        if (nextPt == prevPt || (vis[nextPt] && !inStack[nextPt]))
            continue;
        if (vis[nextPt]) {
            // Cycle found
            while (!stk.empty()) {
                int cntTop = stk.top();
                stk.pop();
                inStack[cntTop] = false;
                if (cntTop == nextPt)
                    break;
            }
        } else {
            dfs(nextPt, cntPt);
        }
    }
    if (!stk.empty() && stk.top() == cntPt) {
        stk.pop();
        inStack[cntPt] = false;
    }
}
```

# 三元环计数

## 无向图三元环计数

- 记 $d_i$ 为点 $i$ 的度数。对于无向边 $\langle u, v \rangle$，改连成 $d$ 较小的连向 $d$ 较大的，若相等则标号小的连向标号大的；
- 对于每个点 $u$：
  - 先标记其所有相邻点 $v$：$mark_v = u$；
  - 对于每个相邻点 $v$ 的相邻点 $w$ 判断是否满足 $mark_w = u$；
- 复杂度：$\mathcal{O}(E \sqrt{E})$，考虑每条有向边 $u \rightarrow v$：
  - 若 $deg_v \le \sqrt{E}$，则枚举 $v$ 连出的点不会超过 $\mathcal{O}(\sqrt{E})$，处理这些点复杂度至多 $\mathcal{O}(E\sqrt{E})$；
  - 若 $deg_v > \sqrt{E}$，则 $deg_u \ge \deg_v > \mathcal{O}(\sqrt{E})$，又由于 $\sum deg_i = 2E$，因此这样的点不会超过 $\sqrt{E}$ 个，因此处理这些点复杂度至多 $\mathcal{O}(E\sqrt{E})$。

## 有向图三元环计数

- 当成无向图算，然后暴力检验。

# 树上倍增

## LCA

```cpp
int depth[SIZE], dist[SIZE], anc[SIZE][20];
int vertexNum, maxDepth;

// BFS variant
void bfs(int startPt) {
    memset(depth, -1, sizeof(depth));
    memset(dist, 0, sizeof(dist));
    memset(anc, -1, sizeof(anc));
    queue<int> q;
    q.push(startPt);
    depth[startPt] = 0;

    while (!q.empty()) {
        int cntPt = q.front();
        q.pop();

        for (int i = head[cntPt]; i != -1; i = edges[i].next) {
            int nextPt = edges[i].to;
            if (depth[nextPt] != -1)
                continue;
            depth[nextPt] = depth[cntPt] + 1;
            dist[nextPt] = dist[cntPt] + edges[i].len;
            anc[nextPt][0] = cntPt;
            // minLen[nextPt][0] = edges[i].len;
            for (int j = 1; j <= maxDepth; j++) {
                if (anc[nextPt][j - 1] != -1) {
                    anc[nextPt][j] = anc[anc[nextPt][j - 1]][j - 1];
                    // minLen[i][j] = min(minLen[i][j - 1], minLen[anc[i][j - 1]][j - 1]);
                }
            }
            q.push(nextPt);
        }
    }
}

// DFS variant
void dfs(int cntPt) {
    for (int i = head[cntPt]; i != -1; i = edges[i].next) {
        int nextPt = edges[i].to;
        if (depth[nextPt] != -1)
            continue;
        depth[nextPt] = depth[cntPt] + 1;
        anc[nextPt][0] = cntPt;
        // minLen[nextPt][0] = edges[i].len;
        dfs(nextPt);
    }
}

void st() {
    for (int j = 1; j <= maxDepth; j++) {
        for (int i = 0; i < vertexNum; i++) {
            if (anc[i][j - 1] != -1) {
                anc[i][j] = anc[anc[i][j - 1]][j - 1];
                // minLen[i][j] = min(minLen[i][j - 1], minLen[anc[i][j - 1]][j - 1]);
            }
        }
    }
}

int getLca(int fstPt, int sndPt) {
    if (depth[fstPt] < depth[sndPt])
        swap(fstPt, sndPt);
    for (int i = maxDepth; i >= 0; i--)
        if (anc[fstPt][i] != -1 && depth[anc[fstPt][i]] >= depth[sndPt])
            fstPt = anc[fstPt][i];
    if (fstPt == sndPt)
        return fstPt;
    for (int i = maxDepth; i >= 0; i--)
        if (anc[fstPt][i] != -1 && anc[sndPt][i] != -1 && anc[fstPt][i] != anc[sndPt][i])
            fstPt = anc[fstPt][i], sndPt = anc[sndPt][i];
    return anc[fstPt][0];
}

maxDepth = log2(vertexNum);
```

# 剖分

## 轻重链剖分

```cpp
int father[SIZE], depth[SIZE], siz[SIZE];
int top[SIZE], hson[SIZE], id[SIZE], orig[SIZE], cntId;

void dfs1(int cntPt) {
    siz[cntPt] = 1;
    for (int i = head[cntPt]; i != -1; i = edges[i].next) {
        int nextPt = edges[i].to;
        if (depth[nextPt] != -1)
            continue;
        father[nextPt] = cntPt;
        depth[nextPt] = depth[cntPt] + 1;
        dfs1(nextPt);
        siz[cntPt] += siz[nextPt];
        if (hson[cntPt] == -1 || siz[nextPt] > siz[hson[cntPt]])
            hson[cntPt] = nextPt;
    }
}

void dfs2(int cntPt, int cntTop) {
    top[cntPt] = cntTop;
    id[cntPt] = ++cntId;
    orig[cntId] = cntPt;
    if (hson[cntPt] == -1)
        return;
    dfs2(hson[cntPt], cntTop);

    for (int i = head[cntPt]; i != -1; i = edges[i].next) {
        int nextPt = edges[i].to;
        if (nextPt == hson[cntPt] || nextPt == father[cntPt])
            continue;
        dfs2(nextPt, nextPt);
    }
}

int queryRoute(int fstPt, int sndPt) {
    int ans = 0;
    while (top[fstPt] != top[sndPt]) {
        if (depth[top[fstPt]] < depth[top[sndPt]])
            swap(fstPt, sndPt);
        ans += querySum(1, id[top[fstPt]], id[fstPt]);
        fstPt = father[top[fstPt]];
    }

    if (depth[fstPt] > depth[sndPt])
        swap(fstPt, sndPt);
    ans += querySum(1, id[fstPt], id[sndPt]);
    return ans;
}

void updateRoute(int fstPt, int sndPt, int val) {
    while (top[fstPt] != top[sndPt]) {
        if (depth[top[fstPt]] < depth[top[sndPt]])
            swap(fstPt, sndPt);
        rangeAdd(1, id[top[fstPt]], id[fstPt], val);
        fstPt = father[top[fstPt]];
    }

    if (depth[fstPt] > depth[sndPt])
        swap(fstPt, sndPt);
    rangeAdd(1, id[fstPt], id[sndPt], val);
}

int querySubtree(int rootPt) {
    return querySum(1, id[rootPt], id[rootPt] + siz[rootPt] - 1);
}

void updateSubtree(int rootPt, int val) {
    rangeAdd(1, id[rootPt], id[rootPt] + siz[rootPt] - 1, val);
}

memset(depth, -1, sizeof(depth));
memset(hson, -1, sizeof(hson));
father[rootPt] = 0, depth[rootPt] = 0; cntId = 0;
dfs1(0); dfs2(0, 0);
```

**基环树？**用并查集找出环上的一条边，把它拿出来另外维护，记其为 $\langle u, v \rangle$。
查询 $\langle s, t \rangle$ 的时候，先查询不经过这条边的答案 `ans1`，再查询经过这条边的答案 `ans2`：$\langle s, v \rangle + \langle u, t \rangle$ 或者 $\langle s, u \rangle + \langle v, t \rangle$。

# 动态树

## 代码

```cpp
class LinkCutTree {
public:
    int val, sum; // Value on vertex, sum of route to root
    bool revLazy; int father, son[2];
};
LinkCutTree lct[SIZE]; stack<int> stk; 
const auto node = [](int rt) -> LinkCutTree & { return lct[rt]; };
const auto father = [](int rt) -> LinkCutTree & { return lct[node(rt).father]; };
const auto lson = [](int rt) -> LinkCutTree & { return lct[node(rt).son[0]]; };
const auto rson = [](int rt) -> LinkCutTree & { return lct[node(rt).son[1]]; };

void pushUp(int rt) {
    node(rt).sum = node(rt).val;
    for (int i = 0; i < 2; i++)
        if (node(rt).son[i] != 0)
            node(rt).sum ^= node(node(rt).son[i]).sum;
}

void pushDown(int rt) {
    if (!node(rt).revLazy)
        return;
    swap(node(rt).son[0], node(rt).son[1]);
    for (int i = 0; i < 2; i++)
        if (node(rt).son[i] != 0)
            node(node(rt).son[i]).revLazy ^= 1;
    node(rt).revLazy = false;
}

bool isRoot(int cntPt) {
    return father(cntPt).son[0] != cntPt && father(cntPt).son[1] != cntPt;
}

int whichSon(int cntPt) {
    return father(cntPt).son[1] == cntPt;
}

void rotate(int cntPt) {
    int fatherPt = node(cntPt).father, grandPt = node(fatherPt).father;
    int sonPt = node(cntPt).son[whichSon(cntPt) ^ 1];

    if (!isRoot(fatherPt))
        node(grandPt).son[whichSon(fatherPt)] = cntPt;
    if (whichSon(cntPt) == 0)
        node(cntPt).son[1] = fatherPt, node(fatherPt).son[0] = sonPt;
    else
        node(cntPt).son[0] = fatherPt, node(fatherPt).son[1] = sonPt;
    node(cntPt).father = grandPt;
    node(fatherPt).father = cntPt;
    if (sonPt != 0)
        node(sonPt).father = fatherPt;
    pushUp(fatherPt);
}

void splay(int cntPt) {
    stk.push(cntPt);
    while (!isRoot(stk.top()))
        stk.push(node(stk.top()).father);
    while (stk.size())
        pushDown(stk.top()), stk.pop();
    while (!isRoot(cntPt)) {
        if (!isRoot(node(cntPt).father))
            rotate(whichSon(cntPt) == whichSon(node(cntPt).father) ? node(cntPt).father : cntPt);
        rotate(cntPt);
    }
    pushUp(cntPt);
}

void expose(int cntPt) {    // Create a solid path that starts from root and ends at cntPt
    int sonPt = 0;
    while (cntPt != 0) {
        splay(cntPt);
    	node(cntPt).son[1] = sonPt;
        pushUp(cntPt);
       	sonPt = cntPt; cntPt = node(cntPt).father;
    }
}

void evert(int cntPt) {     // Make cntPt as the root (in the original tree)
    expose(cntPt); splay(cntPt);
    node(cntPt).revLazy ^= 1;
}

int root(int cntPt) {       // Query root (in the original tree)
    expose(cntPt); splay(cntPt);
    while (node(cntPt).son[0] != 0) {
        pushDown(cntPt);
        cntPt = node(cntPt).son[0];
    }
    splay(cntPt);
    return cntPt;
}

void link(int from, int to) {   // Link an edge
    if (root(from) == root(to))
        return;
    evert(to); node(to).father = from;
}

void cut(int from, int to) {    // Cut an edge
    evert(from); expose(to); splay(to);
    if (node(to).son[0] == from) {
        node(to).son[0] = 0;
        node(from).father = 0;
    }
}

int query(int from, int to) {   // Query info stored on path
    evert(from); expose(to); splay(to);
    return node(to).sum;
}

void update(int cntPt, int val) {   // Update info stored on vertex
    splay(cntPt);
    node(cntPt).val = val;
    pushUp(cntPt);
}

// Note that index 0 is the blank node, and index of vertices starts from 1
fill(lct + 0, lct + vertexNum + 1, LinkCutTree{0, 0, 0, 0, {0, 0}});
```

## 高级应用

### 维护边上信息

对于边 $u \rightarrow v$，新建记录其信息的点 $p'$ ，并连边 $u \rightarrow p' \rightarrow v$。

### 维护动态 MST

下面举两个例子。

#### 最小差值生成树

最小化最大权值与最小权值之差：将所有边按边权从大到小排序，同时 LCT 里维护最大权边。由于从大到小加边，因此加入的边一定是当前的最小权值。如果加边前两端已经联通 （即：`root(e.from) == root(e.to)`），则 `cut` 掉权值最大的边再加入当前边就好了。

```cpp
class LinkCutTree {
public:
    int val, maxv, maxPt; // Store maximum value and maxmium vertex's index
    bool revLazy; int father, son[2];
};

void pushUp(int rt) {
    node(rt).maxv = node(rt).val; node(rt).maxPt = rt;
    for (int i = 0; i < 2; i++)
        if (node(rt).son[i] != 0)
            if (node(rt).maxv < node(node(rt).son[i]).maxv)
                node(rt).maxv = node(node(rt).son[i]).maxv, node(rt).maxPt = node(node(rt).son[i]).maxPt;
}

/* ... */

pair<int, int> query(int from, int to) {
    evert(from); expose(to); splay(to);
    return make_pair(node(to).maxv, node(to).maxPt);
}

class Edge {
public:
    int from, to, val; bool vis;
    bool operator > (const Edge & snd) const {
        return this -> val > snd.val;
    }
};

Edge edges[SIZE];

int main() {
    int vertexNum, edgeNum; cin >> vertexNum >> edgeNum;
    fill(lct + 0, lct + vertexNum + edgeNum + 1, LinkCutTree{0, 0, 0, 0, 0, {0, 0}});
    for (int i = 0; i < edgeNum; i++)
        cin >> edges[i].from >> edges[i].to >> edges[i].val, edges[i].vis = false;
    sort(edges + 0, edges + edgeNum, greater<Edge>());

    int ans = INT_MAX, cntEdge = 0; queue<int> q;
    for (int i = 0; i < edgeNum; i++) {
        Edge & e = edges[i];
        if (e.from == e.to)
            continue;
        if (root(e.from) == root(e.to)) {
            pair<int, int> ret = query(e.from, e.to); int cutPt = ret.second - vertexNum - 1;
            cut(edges[cutPt].from, ret.second); cut(ret.second, edges[cutPt].to);           
            edges[cutPt].vis = false; cntEdge--;
        }
        int pt = vertexNum + i + 1;
        update(pt, e.val);
        link(e.from, pt); link(pt, e.to);
        q.push(i); cntEdge++; e.vis = true;
        if (cntEdge == vertexNum - 1) {
            while (q.size() && !edges[q.front()].vis)
                q.pop();
            ans = min(ans, edges[q.front()].val - e.val);
        }
    }

    cout << ans << '\n';
    return 0;
}
```

#### 双权值最小生成树

每条边有两个权值 $\langle a_i, b_i \rangle$，需最小化 $\max\{a_i\} + \max\{b_i\}$：将边先按照 $a$ 维从小到大排序，同时 LCT 里维护 $b$ 的最大值。由于按 $a$ 从小到大加边，因此加入的边一定是当前最大的 $a$。如果加边前两端已经联通，并且当前边的 $b$ 还大于等于树中最大 $b$，那还加个卵，否则就加。

```cpp
/* Maintain the same values as the previous example in LCT */

class Edge {
public:
    int from, to, fst, snd;
    bool operator < (const Edge & snd) const {
        return this -> fst < snd.fst;
    }
};

Edge edges[SIZE];

int main() {
    int vertexNum, edgeNum; cin >> vertexNum >> edgeNum;
    fill(lct + 0, lct + vertexNum + edgeNum + 1, LinkCutTree{0, 0, 0, 0, 0, {0, 0}});
    for (int i = 0; i < edgeNum; i++)
        cin >> edges[i].from >> edges[i].to >> edges[i].fst >> edges[i].snd;
    sort(edge + 0, edge + edgeNum);

    int ans = INT_MAX;
    for (int i = 0; i < edgeNum; i++) {
        Edge & e = edges[i];
        if (root(e.from) == root(e.to)) {
            auto ret = query(e.from, e.to);
            if (edges[i].snd >= ret.first)
                continue;
            cut(edges[ret.second - vertexNum - 1].from, ret.second);
            cut(ret.second, edges[ret.second - vertexNum - 1].to);
        }
        int pt = vertexNum + i + 1;
        update(pt, e.snd);
        link(e.from, pt); link(pt, e.to);
        if (root(1) == root(vertexNum))
            ans = min(ans, e.fst + query(1, vertexNum).first);
    }

    cout << (ans == INT_MAX ? -1 : ans) << '\n';
    return 0;
}
```
