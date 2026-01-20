---
title: "Offline Algorithms"
language: zh
canonical: true
translationKey: icpc-templates-offline-algorithms
math: true
toc: true
draft: false
---


# 莫队算法

## 普通莫队

```cpp
typedef struct _Query {
    int id, qLeftPt, qRightPt;
} Query;

Query qArr[SIZE];
int arr[SIZE], ans[SIZE], len, qNum, pos[SIZE];

int main() {
    /* Some code here... */
    // Partition
    int blockSize = sqrt(len);
    for (int i = 0; i < len; i++)
        pos[i] = i / blockSize;
    // Sort
    sort(qArr + 0, qArr + qNum, [](const Query & fst, const Query & snd) {
        if (pos[fst.qLeftPt] == pos[snd.qLeftPt])
            return fst.qRightPt < snd.qRightPt;
        return pos[fst.qLeftPt] < pos[snd.qLeftPt];
    });

    int cntLeftPt = 0, cntRightPt = 0, cntAns = 0;
    for (int i = 0; i < qNum; i++) {
        while (qArr[i].qLeftPt < cntLeftPt) {
            // Add left
            cntLeftPt--;
            int cntPt = cntLeftPt;
            // Process insertion of cntPt
        }
        while (qArr[i].qRightPt > cntRightPt) {
            // Add right
            cntRightPt++;
            int cntPt = cntRightPt;
            // Process insertion of cntPt
        }
        while (qArr[i].qLeftPt > cntLeftPt) {
            // Delete left
            int cntPt = cntLeftPt;
            // Process removal of cntPt
            cntLeftPt++;
        }
        while (qArr[i].qRightPt < cntRightPt) {
            // Delete right
            int cntPt = cntRightPt;
            // Process removal of cntPt
            cntRightPt--;
        }
        ans[qArr[i].id] = cntAns;
    }
    /* Some code here... */
}
```

# CDQ 分治

$solve(l, r)$：$\forall k \in [l, r]$，若第 $k$ 项操作是查询，则计算 $[l, k - 1]$ 中修改对其造成的影响：

- $mid = \frac{1}{2}(l + r)$；
- $solve(l, mid)$；
- $solve(mid + 1, r)$；
- 计算第 $[l, mid]$ 项操作中所有修改对 $[mid + 1, r]$ 中所有查询造成的影响。

动态问题 $\rightarrow$ 静态问题。

## 三维偏序

```cpp
void divideConquer(int headPt, int tailPt) {
    if (headPt == tailPt)
        return;
    int midPt = (headPt + tailPt) >> 1;
    divideConquer(headPt, midPt);
    divideConquer(midPt + 1, tailPt);

    int j = headPt;
    for (int i = midPt + 1; i <= tailPt; i++) {
        for (; j <= midPt && arr[j].b <= arr[i].b; j++)
            add(arr[j].c, arr[j].num);
        arr[i].ans += prefixSum(arr[i].c);
    }

    for (int i = headPt; i < j; i++)
        add(arr[i].c, -arr[i].num);

    inplace_merge(arr + headPt, arr + midPt + 1, arr + tailPt + 1, cmpSnd);
}
```

## 四维偏序

```cpp
void divideConquer2(int headPt, int tailPt) {
    if (headPt == tailPt)
        return;
    int midPt = (headPt + tailPt) >> 1;
    divideConquer2(headPt, midPt);
    divideConquer2(midPt + 1, tailPt);
    int j = headPt;
    for (int i = midPt + 1; i <= tailPt; i++) {
        if (arr[i].type == LEFT)
            continue;
        for (; j <= midPt && arr[j].b < arr[i].b; j++)
            if (arr[j].type == LEFT)
                add(arr[j].c, 1);
        ans += prefixSum(arr[i].c);
    }
    for (int i = headPt; i < j; i++)
        if (arr[i].type == LEFT)
            add(arr[i].c, -1);
    inplace_merge(arr + headPt, arr + midPt + 1, arr + tailPt + 1, cmpSnd);
}

void divideConquer(int headPt, int tailPt) {
    if (headPt == tailPt)
        return;
    int midPt = (headPt + tailPt) >> 1;
    divideConquer(headPt, midPt);
    divideConquer(midPt + 1, tailPt);

    for (int i = headPt; i <= midPt; i++)
        arr[i].type = LEFT;
    for (int i = midPt + 1; i <= tailPt; i++)
        arr[i].type = RIGHT;

    inplace_merge(arr + headPt, arr + midPt + 1, arr + tailPt + 1, cmpFst);
    copy(arr + headPt, arr + tailPt + 1, tmp + headPt);
    divideConquer2(headPt, tailPt);
    copy(tmp + headPt, tmp + tailPt + 1, arr + headPt);
}
```

## Luogu P1903：数颜色

```cpp
class Query {
public:
    int type;   // 0: Query+, 1: Query-, 2: Modify
    int x, y, val;
    bool operator < (const _Query & snd) const {
        if (x != snd.x)
            return x < snd.x;
        if (y != snd.y)
            return y < snd.y;
        return type > snd.type;
    }
};

Query qArr[SIZE << 3]; 
set<int> st[VAL_SIZE];
int arr[SIZE], pre[SIZE], last[VAL_SIZE], bit[SIZE], ans[SIZE];

int lowbit(int n) { 
    return n & -n;
}

void add(int pos, int val) {
    pos += 3;
    for (int i = pos; i < SIZE; i += lowbit(i))
        bit[i] += val;
}

int prefixSum(int pos) {
    int ret = 0; pos += 3;
    for (int i = pos; i > 0; i -= lowbit(i))
        ret += bit[i];
    return ret;
}

void divideConquer(int headPt, int tailPt) {
    if (headPt == tailPt)
        return;
    int midPt = (headPt + tailPt) >> 1;
    divideConquer(headPt, midPt);
    divideConquer(midPt + 1, tailPt);

    int j = headPt;
    for (int i = midPt + 1; i <= tailPt; i++) {
        if (qArr[i].type == 2)
            continue;
        for (; j <= midPt && qArr[j].x <= qArr[i].x; j++)
            if (qArr[j].type == 2)
                add(qArr[j].y, qArr[j].val);
        ans[qArr[i].val] += (1 - (qArr[i].type << 1)) * prefixSum(qArr[i].y);
    }

    for (int i = headPt; i < j; i++)
        if (qArr[i].type == 2)
            add(qArr[i].y, -qArr[i].val);

    inplace_merge(qArr + headPt, qArr + midPt + 1, qArr + tailPt + 1);
}

int main() {
    memset(last, -1, sizeof(last));
    memset(bit, 0, sizeof(bit));
    int len, qNum, ansPt = 0, qPt = 0;
    cin >> len >> qNum;
    for (int i = 0; i < len; i++) {
        cin >> arr[i];
        pre[i] = last[arr[i]];
        qArr[qPt++] = {2, i, pre[i], 1};
        last[arr[i]] = i;
        st[arr[i]].insert(i);
    }

    for (int i = 0; i < qNum; i++) {
        char op; int x, y;
        cin >> op >> x >> y;
        x--;
        if (op == 'R') {  // Modify
            // Erase original
            qArr[qPt++] = {2, x, pre[x], -1};
            auto it = st[arr[x]].upper_bound(x);
            if (it != st[arr[x]].end()) {
                qArr[qPt++] = {2, *it, pre[*it], -1};
                pre[*it] = pre[x];
                qArr[qPt++] = {2, *it, pre[*it], 1};
            }
            st[arr[x]].erase(x);
            // Insert new
            it = st[y].upper_bound(x);
            if (it != st[y].end()) {
                pre[x] = pre[*it];
                qArr[qPt++] = {2, x, pre[x], 1};
                qArr[qPt++] = {2, *it, pre[*it], -1};
                pre[*it] = x;
                qArr[qPt++] = {2, *it, pre[*it], 1};
            } else {
                if (st[y].empty()) {
                    pre[x] = -1;
                    qArr[qPt++] = {2, x, pre[x], 1};
                } else {
                    it = prev(it);
                    pre[x] = *it;
                    qArr[qPt++] = {2, x, pre[x], 1}; 
                }
            }
            st[y].insert(x);
            arr[x] = y;

        } else {    // Query
            y--;
            qArr[qPt++] = {0, y, x - 1, ansPt};
            qArr[qPt++] = {1, x - 1, x - 1, ansPt};
            ansPt++;
        }
    }
    divideConquer(0, qPt - 1);
    for (int i = 0; i < ansPt; i++)
        cout << ans[i] << '\n';
    return 0;
}
```

## Luogu P4169：天使玩偶

```cpp
class Query {
public:
    int id, x, y;
    bool operator < (const struct _Query & snd) const {
        if (x != snd.x)
            return x < snd.x;
        if (y != snd.y)
            return y < snd.y;
        return id < snd.id;
    }
};
Query qArr[Q_SIZE], orig[Q_SIZE];
int bit[SIZE], ans[Q_SIZE];

int lowbit(int n) {
    return n & -n;
}

void addMax(int pos, int val) {
    pos++;
    for (int i = pos; i < SIZE; i += lowbit(i))
        bit[i] = max(bit[i], val);
}

void clear(int pos) {
    pos++;
    for (int i = pos; i < SIZE; i += lowbit(i))
        bit[i] = INT_MIN;
}

int prefixMax(int pos) {
    int ret = INT_MIN; pos++;
    for (int i = pos; i > 0; i -= lowbit(i))
        ret = max(ret, bit[i]);
    return ret;
}

void divideConquer(int headPt, int tailPt) {
    if (headPt == tailPt)
        return;
    int midPt = (headPt + tailPt) >> 1;
    divideConquer(headPt, midPt);
    divideConquer(midPt + 1, tailPt);

    int j = headPt;
    for (int i = midPt + 1; i <= tailPt; i++) {
        if (qArr[i].id == -1)
            continue;
        for (; j <= midPt && qArr[j].x <= qArr[i].x; j++)
            if (qArr[j].id == -1)
                addMax(qArr[j].y, qArr[j].x + qArr[j].y);
        int ret = prefixMax(qArr[i].y);
        if (ret != INT_MIN)
            ans[qArr[i].id] = min(ans[qArr[i].id], qArr[i].x + qArr[i].y - ret);
    }

    for (int i = headPt; i < j; i++)
        if (qArr[i].id == -1)
            clear(qArr[i].y);

    inplace_merge(qArr + headPt, qArr + midPt + 1, qArr + tailPt + 1);
}

int main() {
    int len, qNum; cin >> len >> qNum;
    for (int i = 0; i < len; i++) {
        cin >> qArr[i].x >> qArr[i].y;
        qArr[i].id = -1;
    }
    int qPt = len, idPt = 0;
    for (int i = 0; i < qNum; i++) {
        int op; cin >> op >> qArr[qPt].x >> qArr[qPt].y;
        qArr[qPt++].id = (op == 1 ? -1 : idPt++);
    }
    fill(bit + 0, bit + SIZE, INT_MIN);
    fill(ans + 0, ans + idPt, INT_MAX);
    copy(qArr + 0, qArr + qPt, orig + 0);
    // (x1 + y1) - (x2 + y2)
    divideConquer(0, qPt - 1);
    // (x1 - y1) + (x2 - y2)
    for (int i = 0; i < qPt; i++) {
       qArr[i] = orig[i];
       qArr[i].y = SIZE - 1 - qArr[i].y;
    }
    divideConquer(0, qPt - 1);
    // (-x1 + y1) + (-x2 + y2)
    for (int i = 0; i < qPt; i++) {
        qArr[i] = orig[i];
        qArr[i].x = SIZE - 1 - qArr[i].x;
    }
    divideConquer(0, qPt - 1);
    // (-x1 - y1) + (-x2 - y2)
    copy(orig + 0, orig + qPt, qArr + 0);
    for (int i = 0; i < qPt; i++) {
        qArr[i] = orig[i];
        qArr[i].x = SIZE - 1 - qArr[i].x;
        qArr[i].y = SIZE - 1 - qArr[i].y;
    }
    divideConquer(0, qPt - 1);
    for (int i = 0; i < idPt; i++)
        cout << ans[i] << '\n';
    return 0;
}
```

# 线段树分治

## BZOJ 4025

给定一张 $n$ 个点的图，在 $T$ 时间内一些边会在 $s_i$ 时刻出现，并在 $t_i$ 时刻消失。询问每一个时刻该图是否是二分图。

---

$solve(l, r, Q)$：对于时刻 $[l, r]$，操作集为 $Q$，满足 $\forall q \in Q$，其存活时间区间与当前分治的时间区间交集非空；而这个函数的作用为对于 $[l, r]$ 内每个时刻，判断此时图是否是二分图。操作集 $Q$ 中元素 $q$ 带有 $4$ 个属性：$\langle u, v, l, r \rangle$。其中，$u, v$ 代表边的两个端点，而 $l, r$ 代表出现和消失时刻。我们可以考虑如下算法：

- $mid = \frac{1}{2} (l + r)$，并新建两个空操作集 $Q_L, Q_R$；
- 遍历 $q \in Q$：
  - 若 $q.l = l \land q.r = r$（说明这一操作对其子树中所有叶子节点都有影响，那么直接加边）：
    - 并查集中合并 $q.u$ 和 $q.v$，并判断是否出现奇环；
    - 存在奇环？说明时间区间 $[l, r]$ 都凉了，均标记答案为否！**撤销对并查集的操作并回溯**（所以我们需要可撤销并查集，其实也很简单，就搞一个栈记录一下操作之前值的情况然后撤销的时候弹栈就好了）；
  - 若 $q.l \le mid$：令 $q.r = \min⁡(q.r, mid)$，并把 $q$ 放入 $Q_L$；
  - 若 $q.r > mid$：$q.l = \max⁡(q.l, mid + 1)$，并把 $q$ 放入 $Q_R$；
- 若 $l = r$，标记答案为是，**撤销对并查集的操作并回溯**；
- $solve(l, mid, Q_L)$；
- $solve(mid + 1, r, Q_R)$；
- **撤销对并查集的操作并回溯**。

---

```cpp
class Query {
public:
    int from, to, leftPt, rightPt;
};

vector<Query> ques;
int ans[SIZE];
stack<pair<int *, int> > stk;
int parent[SIZE], siz[SIZE], dep[SIZE];

pair<int, int> getParent(int cntPt) {
    if (parent[cntPt] == cntPt)
        return make_pair(cntPt, 0);
    pair<int, int> ret = getParent(parent[cntPt]);
    ret.second ^= dep[cntPt];
    return ret;
}

bool merge(int fstPt, int sndPt) {
    pair<int, int> fst = getParent(fstPt), snd = getParent(sndPt);
    fstPt = fst.first, sndPt = snd.first;
    if (fstPt == sndPt)
        return false;
    if (siz[fstPt] < siz[sndPt])
        swap(fstPt, sndPt);

    stk.push(make_pair(parent + sndPt, parent[sndPt])); parent[sndPt] = fstPt;
    stk.push(make_pair(siz + fstPt, siz[fstPt])); siz[fstPt] += siz[sndPt];
    stk.push(make_pair(dep + sndPt, dep[sndPt])); dep[sndPt] = fst.second ^ snd.second ^ 1;
    return true;
}

void undo() {
    if (stk.empty())
        return;
    *stk.top().first = stk.top().second; stk.pop();
}

void divideConquer(int leftPt, int rightPt, const vector<Query> & vec) {
    int midPt = (leftPt + rightPt) >> 1, stkSiz = stk.size();
    vector<Query> leftVec, rightVec;
    for (int i = 0; i < (int)vec.size(); i++) {
        const Query & q = vec[i];
        if (q.leftPt == leftPt && q.rightPt == rightPt) {
            if (!merge(q.from, q.to) && getParent(q.from).second == getParent(q.to).second) {
                for (int i = leftPt; i <= rightPt; i++)
                    ans[i] = false;
                while ((int)stk.size() != stkSiz)
                    undo();
                return;    
            }
        } else {
            if (q.leftPt <= midPt)
                leftVec.push_back(Query{q.from, q.to, q.leftPt, min(q.rightPt, midPt)});
            if (q.rightPt > midPt)
                rightVec.push_back(Query{q.from, q.to, max(q.leftPt, midPt + 1), q.rightPt});
        }
    }

    if (leftPt == rightPt) {
        ans[leftPt] = true;
        while ((int)stk.size() > stkSiz)
            undo();
        return;
    }

    divideConquer(leftPt, midPt, leftVec);
    divideConquer(midPt + 1, rightPt, rightVec);
    while ((int)stk.size() > stkSiz)
        undo();
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(0); cout.tie(0);

    int vertexNum, qNum, timeLen;
    cin >> vertexNum >> qNum >> timeLen;
    for (int i = 0; i < vertexNum; i++)
        parent[i] = i, dep[i] = 0, siz[i] = 1;
    while (qNum--) {
        int from, to, leftPt, rightPt;
        cin >> from >> to >> leftPt >> rightPt; 
        from--; to--; leftPt++;
        if (leftPt > rightPt)
            continue;
        ques.push_back(Query{from, to, leftPt, rightPt});
    }

    divideConquer(1, timeLen, ques);
    for (int i = 1; i <= timeLen; i++)
        cout << (ans[i] ? "Yes\n" : "No\n");
    return 0;
}
```

## Codeforces 1217F

给定一个初始无边的含有 $n$ 个点的无向图（标号从 $1$ 开始），共 $q$ 次操作。操作有两种形式（其中 $last$ 代表上一次 $2$ 类操作的结果）：

- $1 \ x \ y \ (1 \le x, y \le n, \ x \neq y)$：在点 $(x + last - 1) \bmod (n + 1)$ 和点 $(y + last - 1) \bmod (n + 1)$ 间加一条无向边；
- $2 \ x \ y \ (1 \le x, y \le n, \ x \neq y)$：查询点 $(x + last - 1) \bmod (n + 1)$ 和点 $(y + last - 1) \bmod (n + 1)$ 是否连通。

```cpp
stack<pair<int *, int> > stk;
int parent[SIZE], siz[SIZE];

int getParent(int cntPt) {
    if (parent[cntPt] == cntPt)
        return cntPt;
    return getParent(parent[cntPt]);
}

bool merge(int fstPt, int sndPt) {
    fstPt = getParent(fstPt), sndPt = getParent(sndPt);
    if (fstPt == sndPt)
        return false;
    if (siz[fstPt] < siz[sndPt])
        swap(fstPt, sndPt);
    stk.push(make_pair(parent + sndPt, parent[sndPt])); parent[sndPt] = fstPt;
    stk.push(make_pair(siz + fstPt, siz[fstPt])); siz[fstPt] += siz[sndPt];
    return true;
}

void undo() {
    if (stk.empty())
        return;
    *stk.top().first = stk.top().second; stk.pop();
}

class Edge {
public:
    int op, from, to;
};
Edge edges[SIZE];

class Query {
public:
    int from, to, leftPt, rightPt;
};
vector<Query> ques; int vertexNum, last;
unordered_map<long long int, int> mp;
const auto p2ll = [](int x, int y) {
    tie(x, y) = make_tuple(min(x, y), max(x, y));
    return 1ll * x * SIZE + y; 
};

void divideConquer(int leftPt, int rightPt, const vector<Query> & vec) {
    int midPt = (leftPt + rightPt) >> 1, stkSiz = stk.size();
    const auto undoAll = [stkSiz]() { while ((int)stk.size() != stkSiz) undo(); };
    vector<Query> leftVec, rightVec;

    for (const auto & q : vec) {
        if (q.leftPt == leftPt && q.rightPt == rightPt) {
            if (!mp[p2ll(q.from, q.to)])
                continue;
            merge(q.from, q.to);
        } else {
            if (q.leftPt <= midPt)
                leftVec.push_back(Query{q.from, q.to, q.leftPt, min(q.rightPt, midPt)});
            if (q.rightPt > midPt)
                rightVec.push_back(Query{q.from, q.to, max(q.leftPt, midPt + 1), q.rightPt});
        }
    }
    
    if (leftPt == rightPt) {
        auto & e = edges[leftPt];
        e.from = (e.from + last) % vertexNum, e.to = (e.to + last) % vertexNum;
        
        if (e.op == 2) {
            last = (getParent(e.from) == getParent(e.to));
            cout << last;
        } else {
            mp[p2ll(e.from, e.to)] ^= 1;
        }
        undoAll();
        return;
    }

    divideConquer(leftPt, midPt, leftVec);
    divideConquer(midPt + 1, rightPt, rightVec);
    undoAll();
}

int main() {
    int qNum; cin >> vertexNum >> qNum;
    for (int i = 0; i < vertexNum; i++)
        parent[i] = i, siz[i] = 1;
    for (int i = 0; i < qNum; i++) {
        auto & e = edges[i]; cin >> e.op >> e.from >> e.to; e.from--; e.to--;
        const auto addQue = [i, qNum](int from, int to) {
            if (mp.find(p2ll(from, to)) == mp.end()) {
                if (i + 1 > qNum - 1)
                    return;
                mp[p2ll(from, to)] = ques.size();
                ques.push_back(Query{from, to, i + 1, qNum - 1});
            } else {
                ques[mp[p2ll(from, to)]].rightPt = max(ques[mp[p2ll(from, to)]].leftPt, i - 1);                    
                if (i + 1 > qNum - 1)
                    return;
                mp[p2ll(from, to)] = ques.size();
                ques.push_back(Query{from, to, i + 1, qNum - 1});
            }
        };
        if (e.op == 1) {
            int from = e.from, to = e.to;
            addQue(from, to);
            from = (from + 1) % vertexNum, to = (to + 1) % vertexNum;
            addQue(from, to);
        }
    }
    mp.clear(); last = 0;
    divideConquer(0, qNum - 1, ques);
    return 0;
}
```

# 整体二分

## 静态区间第 $k$ 小

```cpp
// POJ 2104
#include <bits/stdc++.h>
using namespace std;

#define SIZE 100100

typedef struct _Node {
    int val, id;
    bool operator < (const struct _Node & snd) const {
        return val < snd.val;
    }
} Node;

Node arr[SIZE];

typedef struct _Query {
    int qLeftPt, qRightPt, k;
    int id, cnt;
} Query;

Query qArr[SIZE], bkt[SIZE];
int ans[SIZE], bit[SIZE], len, qNum;

int lowbit(int n) {
    return n & -n;
}

void add(int pos, int val) {
    for (int i = pos; i <= len; i += lowbit(i))
        bit[i] += val;
}

int getPrefixSum(int pos) {
    int ret = 0;
    for (int i = pos; i > 0; i -= lowbit(i))
        ret += bit[i];
    return ret;
}

void divideConquer(int headPt, int tailPt, int leftPt, int rightPt) {
    if (leftPt == rightPt) {
        for (int i = headPt; i <= tailPt; i++)
            ans[qArr[i].id] = rightPt;
        return;
    }
    int midPt = (leftPt + rightPt) >> 1;
    // First element that is not smaller than leftPt
    int pos = lower_bound(arr + 0, arr + len, Node{leftPt, -1}) - arr;
    // Update BIT
    for (int i = pos; i < len && arr[i].val <= midPt; i++)
        add(arr[i].id + 1, 1);
	// Calculate cnt
    for (int i = headPt; i <= tailPt; i++)
        qArr[i].cnt = getPrefixSum(qArr[i].qRightPt + 1) - getPrefixSum(qArr[i].qLeftPt);
    // Restore BIT
    for (int i = pos; i < len && arr[i].val <= midPt; i++)
        add(arr[i].id + 1, -1);
	// Divide And Conquer
    int bktHeadPt = headPt, bktTailPt = tailPt;
    for (int i = headPt; i <= tailPt; i++) {
        if (qArr[i].cnt >= qArr[i].k) {
            bkt[bktHeadPt++] = qArr[i];
        } else {
            qArr[i].k -= qArr[i].cnt;
            bkt[bktTailPt--] = qArr[i];
        }
    }
    for (int i = headPt; i <= tailPt; i++)
        qArr[i] = bkt[i];
    if (bktHeadPt != headPt)
        divideConquer(headPt, bktHeadPt - 1, leftPt, midPt);
    if (bktTailPt != tailPt)
        divideConquer(bktTailPt + 1, tailPt, midPt + 1, rightPt);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(0); cout.tie(0);
    while (cin >> len >> qNum) {
        memset(bit, 0, sizeof(bit));
        int minVal = INT_MAX, maxVal = INT_MIN;
        for (int i = 0; i < len; i++) {
            cin >> arr[i].val;
            minVal = min(minVal, arr[i].val);
            maxVal = max(maxVal, arr[i].val);
            arr[i].id = i;
        }
        sort(arr + 0, arr + len);
        for (int i = 0; i < qNum; i++) {
            cin >> qArr[i].qLeftPt >> qArr[i].qRightPt >> qArr[i].k;
            qArr[i].qLeftPt--; qArr[i].qRightPt--;
            qArr[i].id = i; qArr[i].cnt = 0;
        }
        divideConquer(0, qNum - 1, minVal, maxVal);
        for (int i = 0; i < qNum; i++)
            cout << ans[i] << '\n';
    }
    return 0;
}
```

## 动态区间第 $k$ 小

```cpp
// ZOJ 2112
#include <bits/stdc++.h>
using namespace std;

#define SIZE 50010
#define QUE_SIZE 10010
#define OPR_SIZE 70010

// If k == 0: modify;
// qLeftPt = pos, qRightPt = val, cnt = type (-1: delete | 1: add)
typedef struct _Opr {
    int qLeftPt, qRightPt, k;   // pos, val, 0
    int qId, cnt;            // -1, type
} Opr;

Opr oprArr[OPR_SIZE], fstBkt[OPR_SIZE], sndBkt[OPR_SIZE];
int arr[SIZE], ans[QUE_SIZE], bit[SIZE], len;

int lowbit(int n) {
    return n & -n;
}

void add(int pos, int val) {
    for (int i = pos; i <= len; i += lowbit(i))
        bit[i] += val;
}

int prefixSum(int pos) {
    int ret = 0;
    for (int i = pos; i > 0; i -= lowbit(i))
        ret += bit[i];
    return ret;
}

void divideConquer(int headPt, int tailPt, int leftPt, int rightPt) {
    if (leftPt == rightPt) {
        for (int i = headPt; i <= tailPt; i++) {
            if (oprArr[i].k == 0)   // Skip modifications
                continue;
            ans[oprArr[i].qId] = rightPt;
        }
        return;
    }
    int midPt = (leftPt + rightPt) >> 1;
    // Update BIT && Calculate cnt
    for (int i = headPt; i <= tailPt; i++) {
        if (oprArr[i].k > 0)    // query
            oprArr[i].cnt = prefixSum(oprArr[i].qRightPt + 1) - prefixSum(oprArr[i].qLeftPt);
        else if (oprArr[i].qRightPt <= midPt)   // modify
            add(oprArr[i].qLeftPt + 1, oprArr[i].cnt);
    }
    // Restore BIT
    for (int i = headPt; i <= tailPt; i++)
        if (oprArr[i].k == 0 && oprArr[i].qRightPt <= midPt)
            add(oprArr[i].qLeftPt + 1, -oprArr[i].cnt);
    // Divide And Conquer
    int fstPt = 0, sndPt = 0;
    for (int i = headPt; i <= tailPt; i++) {
        if (oprArr[i].k > 0) {
            // query
            if (oprArr[i].k <= oprArr[i].cnt) {
                fstBkt[fstPt++] = oprArr[i];
            } else {
                oprArr[i].k -= oprArr[i].cnt;
                sndBkt[sndPt++] = oprArr[i];
            }
        } else {
            // modify
            if (oprArr[i].qRightPt <= midPt)
                fstBkt[fstPt++] = oprArr[i];
            else
                sndBkt[sndPt++] = oprArr[i];
        }
    }
    for (int i = 0; i < fstPt; i++)
        oprArr[headPt + i] = fstBkt[i];
    for (int i = 0; i < sndPt; i++)
        oprArr[headPt + fstPt + i] = sndBkt[i];
    if (fstPt > 0)
        divideConquer(headPt, headPt + fstPt - 1, leftPt, midPt);
    if (sndPt > 0)
        divideConquer(headPt + fstPt, tailPt, midPt + 1, rightPt);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(0); cout.tie(0);
    int caseNum;
    cin >> caseNum;
    while (caseNum--) {
        memset(bit, 0, sizeof(bit));
        int qNum, oprPt = 0, qPt = 0, minVal = INT_MAX, maxVal = 0;
        cin >> len >> qNum;
        for (int i = 0; i < len; i++) {
            // treat original array as insertions
            cin >> arr[i];
            minVal = min(minVal, arr[i]);
            maxVal = max(maxVal, arr[i]);
            oprArr[oprPt++] = {i, arr[i], 0, -1, 1};
        }
        for (int i = 0; i < qNum; i++) {
            char opr; cin >> opr;
            if (opr == 'Q') {	// Query
                int qLeftPt, qRightPt, k;
                cin >> qLeftPt >> qRightPt >> k;
                qLeftPt--, qRightPt--;
                oprArr[oprPt++] = {qLeftPt, qRightPt, k, qPt++, 0};
            } else if (opr == 'C') {	// Change
                int pos, val;
                cin >> pos >> val;
                pos--;
                // delete
                oprArr[oprPt++] = {pos, arr[pos], 0, -1, -1};
                // insert
                oprArr[oprPt++] = {pos, val, 0, -1, 1};
                arr[pos] = val;
                minVal = min(minVal, val);
                maxVal = max(maxVal, val);
            }
        }
        divideConquer(0, oprPt - 1, minVal, maxVal);
        for (int i = 0; i < qPt; i++)
            cout << ans[i] << '\n';
    }
    return 0;
}
```

# 分治解决动态 MST

$n$ 个点，$m$ 条有权边，$q$ 次操作。每次操作修改一条边的边权，动态维护 MST 大小。

```cpp
#include <bits/stdc++.h>
using namespace std;

#define VERTEX_SIZE 20010
#define EDGE_SIZE 50010

typedef struct _Query {
    int id, val;
} Query;

Query qArr[EDGE_SIZE];

typedef struct _Edge {
    int id, from, to, len;
    bool operator < (const struct _Edge & snd) const {
        return len < snd.len;
    }
} Edge;

vector<Edge> edge[30], ret, tmp;

int id[EDGE_SIZE], cntLen[EDGE_SIZE];
long long int ans[EDGE_SIZE];

/* Disjoint Set */
int parent[VERTEX_SIZE], siz[VERTEX_SIZE];

int getParent(int cntPt) {
    if (parent[cntPt] == cntPt)
        return cntPt;
    parent[cntPt] = getParent(parent[cntPt]);
    return parent[cntPt];
}

bool merge(int fstPt, int sndPt) {
    fstPt = getParent(fstPt); sndPt = getParent(sndPt);
    if (fstPt == sndPt)
        return false;
    if (siz[fstPt] < siz[sndPt])
        swap(fstPt, sndPt);
    parent[sndPt] = fstPt;
    siz[fstPt] += siz[sndPt];
    return true;
}

void clear(const vector<Edge> & vec) {
    for (const auto & e : vec) {
        parent[e.from] = e.from;
        parent[e.to] = e.to;
        siz[e.from] = 1; siz[e.to] = 1;
    }
}

/* Divide And Conquer */
void relabel(vector<Edge> & vec) {
    int pt = 0;
    for (const auto & e : vec)
        id[e.id] = pt++;
}

void reduction() { 
    tmp.clear(); clear(ret); 
    sort(ret.begin(), ret.end());
    for (const auto & e : ret)
        if (e.len == INT_MAX || merge(e.from, e.to))
            tmp.push_back(e);

    relabel(tmp); ret = tmp;
}

void contraction(long long int & val) { 
    // Reduction
    tmp.clear(); clear(ret);
    sort(ret.begin(), ret.end());
    for (const auto & e : ret)
        if (merge(e.from, e.to)) 
            tmp.push_back(e);
    // Contraction
    clear(tmp); sort(tmp.begin(), tmp.end());
    for (const auto & e : tmp)
        if (e.len != INT_MIN && merge(e.from, e.to))
            val += e.len;
    tmp.clear();
    for (const auto & e : ret) {
        int from = getParent(e.from), to = getParent(e.to);
        tmp.push_back({e.id, from, to, e.len});
    }
    relabel(tmp); ret = tmp;
} 

void divideConquer(int headPt, int tailPt, int dep, long long int val) {
    if (headPt == tailPt)
        cntLen[qArr[headPt].id] = qArr[headPt].val; 
    // Recover
    ret = edge[dep]; relabel(ret);
    for (auto & e : ret)
        e.len = cntLen[e.id];
    if (headPt == tailPt) {
        ans[headPt] = val; clear(ret);
        sort(ret.begin(), ret.end());
        for (const auto & e : ret)
            if (merge(e.from, e.to))
                ans[headPt] += e.len;
        return;
    }
    for (int i = headPt; i <= tailPt; i++)
        ret[id[qArr[i].id]].len = INT_MIN;
    contraction(val);
    for (int i = headPt; i <= tailPt; i++)
        ret[id[qArr[i].id]].len = INT_MAX;
    reduction();
    // Pushdown
    edge[dep + 1] = ret;
    int midPt = (headPt + tailPt) >> 1;
    divideConquer(headPt, midPt, dep + 1, val);
    divideConquer(midPt + 1, tailPt, dep + 1, val);
} 

int main() {
    int vertexNum, edgeNum, qNum;
    cin >> vertexNum >> edgeNum >> qNum;
    for (int i = 0; i < edgeNum; i++) {
        int from, to, len; cin >> from >> to >> len;
        from--; to--; cntLen[i] = len;
        edge[0].push_back({i, from, to, len});
    }
    for (int i = 0; i < qNum; i++) {
        cin >> qArr[i].id >> qArr[i].val;
        qArr[i].id--;
    }

    divideConquer(0, qNum - 1, 0, 0);
    for (int i = 0; i < qNum; i++)
        cout << ans[i] << '\n';
    return 0;
}
```
