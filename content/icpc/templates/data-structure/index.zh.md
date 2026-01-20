---
title: "Data Structure"
language: zh
canonical: true
translationKey: icpc-templates-data-structure
math: true
toc: true
draft: false
---


## ST 表

### 朴素 RMQ

```cpp
pair<long long int, long long int> stArr[SIZE][20];	// Min, Max

pair<long long int, long long int> minMax(const pair<long long int, long long int> & fst, const pair<long long int, long long int> & snd) {
    return make_pair(min(fst.first, snd.first), max(fst.second, snd.second));
}

void initSt() {
    int logLim = log2(len) + 1;
    for (int i = 0; i < len; i++)
        stArr[i][0] = make_pair(arr[i], arr[i]);
    for (int j = 1; j <= logLim; j++) {
        for (int i = 0; i < len; i++) {
            if (i + (1 << j) - 1 >= len)
                continue;
            stArr[i][j] = minMax(stArr[i][j - 1], stArr[i + (1 << (j - 1))][j - 1]);
        }
    }
}

pair<long long int, long long int> queryMinMax(int qLeftPt, int qRightPt) {
    int lenLog = log2(qRightPt - qLeftPt + 1);
    return minMax(stArr[qLeftPt][lenLog], stArr[qRightPt - (1 << lenLog) + 1][lenLog]);
}
```

### 约束 RMQ

对于序列 $a$，满足 $\forall i \in [2, n], \ |a_i - a_{i - 1}| = 1$。此时预处理部分可以优化至 $\mathcal{O}(1)$。大致思想是分块，块内分别搞一次 RMQ，块间最值再搞一次 RMQ。

设块大小 $b = \frac{1}{2} \log{n}$，则块数 $d = \lceil \frac{n}{b} \rceil$。由于相邻两个数只可能 $\pm 1$，故块内情况只有 $2^b = \sqrt{n}$ 种不同情况。预处理时直接对所有情况进行预处理，复杂度不会超过线性。处理好后带上 offset 搞就好了。询问的时候左右不完整块内部查询，中间块外部查询。

### 随机数据时的优化

块大小为 $b$，块内预处理前缀 $\max$ 和后缀 $\max$，预处理的复杂度为 $\mathcal{O}(n + \frac{n}{b} \log{\frac{n}{b}})$。若询问两端点跨不同区间则可 $\mathcal{O}(1)$ 得到（块内前后缀，块间 ST 表），而若两端跨相同区间最坏 $\mathcal{O}(b)$ 得到。考虑到询问完全随机时两端在同一区间内概率为 $\frac{b}{n}$，则期望复杂度是 $\mathcal{O}(\frac{b^2}{n})$。

总复杂度：$\mathcal{O}(n + \frac{n}{b}\log{\frac{n}{b}} + q + q\frac{b^2}{n})$

- $b$ 至少为 $\mathcal{O}(\log{n})$ 时，预处理不超过 $\mathcal{O}(n)$；
- $b$ 至多为 $\mathcal{O}(\sqrt{n})$ 时，询问不超过 $\mathcal{O}(q)$；
- 如果数据不随机，大致取 $2\sqrt{\frac{n}{q}\log{n}}$ 可以让期望复杂度为 $\mathcal{O}(n\sqrt{\log{n}})$；
- 调参即可~

```cpp
int arr[SIZE], pos[SIZE], blockSiz, blockNum, len;
int pfx[SIZE], sfx[SIZE], logs[SIZE], twoPows[30];
int stArr[BLK_NUM][LG_SIZE];

inline void initLogs() {
    logs[0] = -1;
    for (int i = 1; i < BLK_NUM; i++)
        logs[i] = logs[i >> 1] + 1;
    twoPows[0] = 1;
    for (int i = 1; i < 30; i++)
        twoPows[i] = twoPows[i - 1] << 1;
}

inline void initSt() {
    for (int b = 0, l = 0; l < len; b++, l += blockSiz) {
        int r = min(l + blockSiz, len) - 1;
        pfx[l] = arr[l]; sfx[r] = arr[r];
        for (int i = l + 1; i <= r; i++)
            pfx[i] = max(pfx[i - 1], arr[i]);
        for (int i = r - 1; i >= l; i--)
            sfx[i] = max(sfx[i + 1], arr[i]);
        stArr[b][0] = sfx[l];
    }

    int logLim = logs[blockNum];
    for (int j = 1; j <= logLim; j++) {
        for (int i = 0; i < blockNum; i++) {
            if (i + twoPows[j] - 1 >= blockNum)
                continue;
            stArr[i][j] = max(stArr[i][j - 1], stArr[i + twoPows[j - 1]][j - 1]);
        }
    }
}

inline int queryMax(int qLeftPt, int qRightPt) {
    if (pos[qLeftPt] == pos[qRightPt]) {
        int ret = arr[qLeftPt];
        for (int i = qLeftPt + 1; i <= qRightPt; i++)
            ret = max(ret, arr[i]);
        return ret;
    }

    int ret = max(sfx[qLeftPt], pfx[qRightPt]);
    qLeftPt = pos[qLeftPt] + 1, qRightPt = pos[qRightPt] - 1;
    if (qLeftPt > qRightPt)
        return ret; 
    int lenLog = logs[qRightPt - qLeftPt + 1];
    return max({ret, stArr[qLeftPt][lenLog], stArr[qRightPt - (1 << lenLog) + 1][lenLog]});
}

int main() {
    /* Some inits... */
    blockSiz = max(1, (int)sqrt(len)); blockNum = len / blockSiz + (len % blockSiz > 0);
    for (int i = 0; i < len; i++)
        cin >> arr[i], pos[i] = i / blockSiz;
    initSt();
	/* Some queries... */
}
```

## 树状数组

### 一维前缀和/最大值

```cpp
int lowbit(int n) {
    return n & (-n);
}

void add(int pos, int val) {
    for (int i = pos; i < SIZE; i += lowbit(i)) {
        bit[i] += val;
        // bit[i] = max(bit[i], val);
    }
}

int prefixSum(int pos) {
    int ans = 0;
    for (int i = pos; i > 0; i -= lowbit(i)) {
        ans += bit[i];
        // ans = max(ans, bit[i]);
    }
    return ans;
}
```

### 一维区间最大值

复杂度： $\mathcal{O}(\log^2{n})$

```cpp
int lowbit(int n) {
    return n & -n;
}

void update(int pos, int val) {
    arr[pos] = val; // Original Array
    for (int i = pos; i <= num; i += lowbit(i)) {
        bit[i] = val;
        for (int j = 1; j < lowbit(i); j <<= 1) {
            bit[i] = max(bit[i], bit[i - j]);
        }
    }
}

int query(int leftPt, int rightPt) {
    int ans = INT_MIN;
    while (rightPt >= leftPt) {
        ans = max(ans, arr[rightPt]);
        rightPt--;
        while (rightPt - lowbit(rightPt) >= leftPt) {
            ans = max(ans, bit[rightPt]);
            rightPt -= lowbit(rightPt);
        }
    }
    return ans;
}
```

### 二维前缀和

```cpp
int lowbit(int n) {
    return n & -n;
}

void add(const pair<int, int> & pos, int val) {
    for (int i = pos.first; i < SIZE; i += lowbit(i)) {
        for (int j = pos.second; j < SIZE; j += lowbit(j)) {
            bit[i][j] += val;
        }
    }
}

int getSum(const pair<int, int> & pos) {
    int ans = 0;
    for (int i = pos.first; i > 0; i -= lowbit(i)) {
        for (int j = pos.second; j > 0; j -= lowbit(j)) {
            ans += bit[i][j];
        }
    }
    return ans;
}

int getRangeSum(const pair<int, int> & upperLeftPt, const pair<int, int> & lowerRightPt) {
    int ans = getSum(lowerRightPt);
    if (upperLeftPt.first > 0)
        ans -= getSum(make_pair(upperLeftPt.first - 1, lowerRightPt.second));
    if (upperLeftPt.second > 0)
        ans -= getSum(make_pair(lowerRightPt.first, upperLeftPt.second - 1));
    if (upperLeftPt.first > 0 && upperLeftPt.second > 0)
        ans += getSum(make_pair(upperLeftPt.first - 1, upperLeftPt.second - 1));
    return ans;
}
```

## 线段树

### 普通线段树

```cpp
#define LEFT_SON (segPt << 1)
#define RIGHT_SON (segPt << 1 | 1)

typedef struct _SegNode {
    int leftPt, rightPt;
    int sum;
} SegNode;
SegNode segTree[SIZE << 2];

void pushUp(int segPt) {
    segTree[segPt].sum = segTree[LEFT_SON].sum + segTree[RIGHT_SON].sum;
}

void build(int segPt, int leftPt, int rightPt) {
    segTree[segPt].leftPt = leftPt;
    segTree[segPt].rightPt = rightPt;

    if (leftPt == rightPt) {
        segTree[segPt].sum = 0;
        return;
    }

    int midPt = (leftPt + rightPt) >> 1;
    build(LEFT_SON, leftPt, midPt);
    build(RIGHT_SON, midPt + 1, rightPt);
    pushUp(segPt);
}

void update(int segPt, int cntPt, int val) {
    if (segTree[segPt].leftPt == segTree[segPt].rightPt) {
        segTree[segPt].sum = val;
        return;
    }

    int midPt = (segTree[segPt].leftPt + segTree[segPt].rightPt) >> 1;
    if (cntPt <= midPt)
        update(LEFT_SON, cntPt, val);
    else
        update(RIGHT_SON, cntPt, val);
    pushUp(segPt);
}

int querySum(int segPt, int qLeftPt, int qRightPt) {
    if (segTree[segPt].leftPt >= qLeftPt && segTree[segPt].rightPt <= qRightPt) {
        return segTree[segPt].sum;
    }

    int ans = 0;
    int midPt = (segTree[segPt].leftPt + segTree[segPt].rightPt) >> 1;
    if (qLeftPt <= midPt)
        ans += querySum(LEFT_SON, qLeftPt, qRightPt);
    if (qRightPt > midPt)
        ans += querySum(RIGHT_SON, qLeftPt, qRightPt);
    return ans;
}
```

### 带 lazy 标记的线段树

```cpp
#define LEFT_SON (segPt << 1)
#define RIGHT_SON (segPt << 1 | 1)

typedef struct _SegNode {
    int leftPt, rightPt;
    int sum, lazy;
} SegNode;
SegNode segTree[SIZE << 2];

void pushUp(int segPt) {
    segTree[segPt].sum = segTree[LEFT_SON].sum + segTree[RIGHT_SON].sum;
}

void pushDown(int segPt) {
    if (segTree[segPt].lazy != 0) {
        segTree[LEFT_SON].sum += segTree[segPt].lazy * (segTree[LEFT_SON].rightPt - segTree[LEFT_SON].leftPt + 1);
        segTree[RIGHT_SON].sum += segTree[segPt].lazy * (segTree[RIGHT_SON].rightPt - segTree[RIGHT_SON].leftPt + 1);

        segTree[LEFT_SON].lazy += segTree[segPt].lazy;
        segTree[RIGHT_SON].lazy += segTree[segPt].lazy;

        segTree[segPt].lazy = 0;
    }
}

void build(int segPt, int leftPt, int rightPt) {
    segTree[segPt].leftPt = leftPt;
    segTree[segPt].rightPt = rightPt;
    segTree[segPt].lazy = 0;
    if (leftPt == rightPt) {
        segTree[segPt].sum = 0;
        return;
    }
    int midPt = (leftPt + rightPt) >> 1;
    build(LEFT_SON, leftPt, midPt);
    build(RIGHT_SON, midPt + 1, rightPt);
    pushUp(segPt);
}

void addRange(int segPt, int qLeftPt, int qRightPt, int val) {
    if (segTree[segPt].leftPt >= qLeftPt && segTree[segPt].rightPt <= qRightPt) {
        segTree[segPt].sum += val * (segTree[segPt].rightPt - segTree[segPt].leftPt + 1);
        segTree[segPt].lazy += val;
        return;
    }
    pushDown(segPt);
    int midPt = (segTree[segPt].leftPt + segTree[segPt].rightPt) >> 1;
    if (qLeftPt <= midPt)
        addRange(LEFT_SON, qLeftPt, qRightPt, val);
    if (qRightPt > midPt)
        addRange(RIGHT_SON, qLeftPt, qRightPt, val);
    pushUp(segPt);
}

int querySum(int segPt, int qLeftPt, int qRightPt) {
    if (segTree[segPt].leftPt >= qLeftPt && segTree[segPt].rightPt <= qRightPt) {
        return segTree[segPt].sum;
    }
    pushDown(segPt);
    int ans = 0;
    int midPt = (segTree[segPt].leftPt + segTree[segPt].rightPt) >> 1;
    if (qLeftPt <= midPt)
        ans += querySum(LEFT_SON, qLeftPt, qRightPt);
    if (qRightPt > midPt)
        ans += querySum(RIGHT_SON, qLeftPt, qRightPt);
    return ans;
}
```

## 可持久化线段树

### 普通可持久化线段树

```cpp
#define LSON(x) segTree[x].leftSon
#define RSON(x) segTree[x].rightSon

typedef struct _SegNode {
    int sum;
    int leftSon, rightSon;
} SegNode;
SegNode segTree[SIZE * 40];
int rootArr[SIZE], cntPt;

void pushUp(int segPt) {
    segTree[segPt].sum = segTree[LSON(segPt)].sum + segTree[RSON(segPt)].sum;
}

void build(int & segPt, int leftPt, int rightPt) {
    segPt = ++cntPt;
    segTree[segPt].sum = 0;
    if (leftPt == rightPt)
        return;

    int midPt = (leftPt + rightPt) >> 1;
    build(LSON(segPt), leftPt, midPt);
    build(RSON(segPt), midPt + 1, rightPt);
}

void update(int & segPt, int prevPt, int leftPt, int rightPt, int pos, int val) {
    segPt = ++cntPt;
    LSON(segPt) = LSON(prevPt);
    RSON(segPt) = RSON(prevPt);

    if (leftPt == rightPt) {
        segTree[segPt].minPt = val;
        return;
    }

    int midPt = (leftPt + rightPt) >> 1;
    if (pos <= midPt)
        update(LSON(segPt), LSON(prevPt), leftPt, midPt, pos, val);
    else
        update(RSON(segPt), RSON(prevPt), midPt + 1, rightPt, pos, val);
    pushUp(segPt);
}

int query(int segPt, int leftPt, int rightPt, int qLeftPt, int qRightPt) {
    if (qLeftPt <= leftPt && qRightPt >= rightPt)
        return segTree[segPt].sum;

    int ans = 0, midPt = (leftPt + rightPt) >> 1;
    if (qLeftPt <= midPt)
        ans += query(LSON(segPt), leftPt, midPt, qLeftPt, qRightPt);
    if (qRightPt > midPt)
        ans += query(RSON(segPt), midPt + 1, rightPt, qLeftPt, qRightPt);
    return ans;
}

cntPt = 0;
build(rootArr[0], 1, len);
```

### 带 lazy 标记的可持久化线段树

```cpp
#define LSON(x) segTree[x].leftSon
#define RSON(x) segTree[x].rightSon

typedef struct _SegNode {
    int sum, lazy;
    int leftSon, rightSon;
} SegNode;
SegNode segTree[SIZE * 40];
int rootArr[SIZE], cntPt;

void pushUp(int segPt, int len) {
    segTree[segPt].sum = segTree[LSON(segPt)].sum + segTree[RSON(segPt)].sum + len * segTree[segPt].lazy;
}

void build(int & segPt, int leftPt, int rightPt) {
    segPt = ++cntPt;
    segTree[segPt].lazy = 0;
    if (leftPt == rightPt) {
        segTree[segPt].sum = arr[leftPt];
        return;
    }

    int midPt = (leftPt + rightPt) >> 1;
    build(segTree[segPt].leftSon, leftPt, midPt);
    build(segTree[segPt].rightSon, midPt + 1, rightPt);
    pushUp(segPt, 0);
}

void rangeAdd(int & segPt, int prevPt, int leftPt, int rightPt, int qLeftPt, int qRightPt, int val) {
    segPt = ++cntPt;
    segTree[segPt] = segTree[prevPt];

    if (leftPt >= qLeftPt && rightPt <= qRightPt) {
        segTree[segPt].sum += val * (rightPt - leftPt + 1);
        segTree[segPt].lazy += val;
        return;
    }

    int midPt = (leftPt + rightPt) >> 1;
    if (qLeftPt <= midPt)
        rangeAdd(LSON(segPt), LSON(prevPt), leftPt, midPt, qLeftPt, qRightPt, val);
    if (qRightPt > midPt)
        rangeAdd(RSON(segPt), RSON(prevPt), midPt + 1, rightPt, qLeftPt, qRightPt, val);
    pushUp(segPt, rightPt - leftPt + 1);
}

int querySum(int segPt, int leftPt, int rightPt, int qLeftPt, int qRightPt, int lazy = 0) {
    // push down when querying
    if (leftPt >= qLeftPt && rightPt <= qRightPt) {
        return segTree[segPt].sum + lazy * (rightPt - leftPt + 1);
    }

    int ans = 0, midPt = (leftPt + rightPt) >> 1;
    if (qLeftPt <= midPt)
        ans += querySum(LSON(segPt), leftPt, midPt, qLeftPt, qRightPt, lazy + segTree[segPt].lazy);
    if (qRightPt > midPt)
        ans += querySum(RSON(segPt), midPt + 1, rightPt, qLeftPt, qRightPt, lazy + segTree[segPt].lazy);
    return ans;
}

cntPt = 0;
build(rootArr[0], 1, len);
```

### 区间第 k 大

```cpp
int query(int leftPt, int rightPt, int qLeftPt, int qRightPt, int k) {
    int ans = 0;
    while (leftPt < rightPt) {
        int midPt = (leftPt + rightPt) >> 1;
        if (k <= midPt) {
            qLeftPt = segTree[qLeftPt].leftSon;
            qRightPt = segTree[qRightPt].leftSon;
            rightPt = midPt;
        } else {
            ans += segTree[segTree[qRightPt].leftSon].sum - segTree[segTree[qLeftPt].leftSon].sum;
            qLeftPt = segTree[qLeftPt].rightSon;
            qRightPt = segTree[qRightPt].rightSon;
            leftPt = midPt + 1;
        }
    }
    if (k >= leftPt)
        ans += segTree[qRightPt].sum - segTree[qLeftPt].sum;
    return ans;
}

// Discretize first
for (int i = 1; i <= len; i++) {
    int pos = lower_bound(dsc + 1, dsc + dscLen + 1, arr[i]) - dsc;
    add(rootArr[i], rootArr[i - 1], 1, dscLen, pos, 1);
}
k = upper_bound(dsc + 1, dsc + dscLen + 1, k) - dsc - 1;
cout << query(1, dscLen, rootArr[qLeftPt - 1], rootArr[qRightPt], k) << endl;
```

## 平衡树

### 无旋转 treap

#### 普通平衡树

```cpp
#include <bits/stdc++.h>
using namespace std;

#define SIZE 100010

mt19937 rng(chrono::steady_clock::now().time_since_epoch().count()); 
uniform_int_distribution<int> unifInt;

class Treap {
public:
    int val, rnd, siz, son[2];
};

Treap trp[SIZE]; int trpPt;

const auto node = [](int rt) -> Treap & { return trp[rt]; };
const auto lson = [](int rt) -> Treap & { return trp[trp[rt].son[0]]; };
const auto rson = [](int rt) -> Treap & { return trp[trp[rt].son[1]]; };

void maintain(int rt) {
    node(rt).siz = lson(rt).siz + rson(rt).siz + 1;
}

int newNode(int val) {
    int cntPt = trpPt++;
    if (vec.size()) {
        cntPt = vec.back();
        vec.pop_back(); trpPt--;
    }
    trp[cntPt] = {val, unifInt(rng), 1, {0, 0}};
    return cntPt;
}

int merge(int fstRt, int sndRt) {
    if (fstRt == 0)
        return sndRt;
    if (sndRt == 0)
        return fstRt;

    if (node(fstRt).rnd < node(sndRt).rnd) {
        node(fstRt).son[1] = merge(node(fstRt).son[1], sndRt);
        maintain(fstRt); return fstRt;
    } else {
        node(sndRt).son[0] = merge(fstRt, node(sndRt).son[0]);
        maintain(sndRt); return sndRt;
    }
}

void split(int rt, int k, int & fstRt, int & sndRt) {
    if (rt == 0) {
        fstRt = 0; sndRt = 0;
        return;
    }

    if (k <= lson(rt).siz) {
        sndRt = rt; split(node(rt).son[0], k, fstRt, node(rt).son[0]);
    } else {
        fstRt = rt; split(node(rt).son[1], k - lson(rt).siz - 1, node(rt).son[1], sndRt);
    }
    maintain(rt);
}

void splitByVal(int rt, int val, int & fstRt, int & sndRt) {
    if (rt == 0) {
        fstRt = 0; sndRt = 0;
        return;
    }

    if (node(rt).val > val)
        sndRt = rt, splitByVal(node(rt).son[0], val, fstRt, node(rt).son[0]);
    else
        fstRt = rt, splitByVal(node(rt).son[1], val, node(rt).son[1], sndRt);        
    maintain(rt);
}

int queryRank(int rt, int val) {
    if (rt == 0)
        return 1;
    if (val <= node(rt).val)
        return queryRank(node(rt).son[0], val);
    return queryRank(node(rt).son[1], val) + lson(rt).siz + 1;
}

int queryByRank(int rt, int k) {
    int fstRt = 0, sndRt = 0, thdRt = 0;
    split(rt, k - 1, fstRt, sndRt);
    split(sndRt, 1, sndRt, thdRt);
    int ret = node(sndRt).val;
    rt = merge(fstRt, merge(sndRt, thdRt));
    return ret;
}

void insert(int & rt, int val) {
    int k = queryRank(rt, val);
    int fstRt = 0, sndRt = 0;
    split(rt, k - 1, fstRt, sndRt);
    rt = merge(fstRt, merge(newNode(val), sndRt));
}

void remove(int & rt, int val) {
    int k = queryRank(rt, val);
    int fstRt = 0, sndRt = 0, thdRt = 0;
    split(rt, k - 1, fstRt, sndRt); 
    split(sndRt, 1, sndRt, thdRt);
    rt = merge(fstRt, thdRt); vec.push_back(sndRt);
}

int queryPrev(int & rt, int val) {
    int fstRt = 0, sndRt = 0;
    splitByVal(rt, val - 1, fstRt, sndRt);
    int ret = queryByRank(fstRt, node(fstRt).siz);
    rt = merge(fstRt, sndRt);
    return ret;
}

int queryNext(int & rt, int val) {
    int fstRt = 0, sndRt = 0;
    splitByVal(rt, val, fstRt, sndRt);
    int ret = queryByRank(sndRt, 1);
    rt = merge(fstRt, sndRt);
    return ret;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(0); cout.tie(0);
    vec.clear();
    trpPt = 0; trp[trpPt++] = {0, 0, 0, {0, 0}};

    int rt = 0, qNum; cin >> qNum;
    while (qNum--) {
        int op, cnt; cin >> op >> cnt;
        if (op == 1)
            insert(rt, cnt);
        else if (op == 2)
            remove(rt, cnt);
        else if (op == 3)
            cout << queryRank(rt, cnt) << '\n';
        else if (op == 4)
            cout << queryByRank(rt, cnt) << '\n';
        else if (op == 5)
            cout << queryPrev(rt, cnt) << '\n';
        else if (op == 6)
            cout << queryNext(rt, cnt) << '\n';
    }

    return 0;
}

```

### 文艺平衡树

```cpp
#include <bits/stdc++.h>
using namespace std;

mt19937 rng(chrono::steady_clock::now().time_since_epoch().count()); 
uniform_int_distribution<int> unifInt(0, 1e9);

class Treap {
public:
    int val, rnd, siz; bool lazy;
    Treap * son[2];

    Treap (int v) : val (v) {
        this -> rnd = unifInt(rng); this -> siz = 1; this -> lazy = false;
        this -> son[0] = nullptr; this -> son[1] = nullptr;
    }

    void maintain() {
        this -> siz = 1;
        for (int i = 0; i < 2; i++)
            if (this -> son[i] != nullptr)
                this -> siz += this -> son[i] -> siz;
    }

    void pushDown() {
        if (!this -> lazy)
            return;
        swap(this -> son[0], this -> son[1]);
        for (int i = 0; i < 2; i++)
            if (this -> son[i] != nullptr)
                this -> son[i] -> lazy ^= 1;
        this -> lazy = false; return;
    }
};

auto siz = [](Treap * rt) {
    return rt == nullptr ? 0 : rt -> siz;
};

Treap * merge(Treap * fstRt, Treap * sndRt) {
    if (fstRt == nullptr)
        return sndRt;
    if (sndRt == nullptr)
        return fstRt;

    if (fstRt -> rnd < sndRt -> rnd) {
        fstRt -> pushDown();
        fstRt -> son[1] = merge(fstRt -> son[1], sndRt);
        fstRt -> maintain(); return fstRt;
    } else {
        sndRt -> pushDown();
        sndRt -> son[0] = merge(fstRt, sndRt -> son[0]);
        sndRt -> maintain(); return sndRt;
    }
}

void split(Treap * rt, int k, Treap * & fstRt, Treap * & sndRt) {
    if (rt == nullptr) {
        fstRt = nullptr; sndRt = nullptr;
        return;
    }

    rt -> pushDown();
    if (k <= siz(rt -> son[0])) {
        split(rt -> son[0], k, fstRt, rt -> son[0]);
        rt -> maintain(); sndRt = rt;
    } else {
        split(rt -> son[1], k - siz(rt -> son[0]) - 1, rt -> son[1], sndRt);
        rt -> maintain(); fstRt = rt;
    }
}

int getRank(Treap * rt, int val) {
    if (rt == nullptr)
        return 0;
    if (val <= rt -> val)
        return getRank(rt -> son[0], val);
    return getRank(rt -> son[1], val) + siz(rt -> son[0]) + 1;
}

void insert(Treap * & rt, int val) {
    int k = getRank(rt, val);
    Treap * fstRt = nullptr, * sndRt = nullptr;
    split(rt, k, fstRt, sndRt);
    rt = merge(fstRt, merge(new Treap(val), sndRt));
}

void remove(Treap * & rt , int val) {
    int k = getRank(rt, val) ;
    Treap * fstRt = nullptr, * sndRt = nullptr, * thdRt = nullptr;
    split(rt, k - 1, fstRt, sndRt); 
    split(sndRt, 1, sndRt, thdRt);
    rt = merge(fstRt, thdRt); delete sndRt;
}

void reverse(Treap * & rt, int qLeftPt, int qRightPt) {
    Treap * fstRt = nullptr, * sndRt = nullptr, * thdRt = nullptr;
    split(rt, qLeftPt - 1, fstRt, sndRt); 
    split(sndRt, qRightPt - qLeftPt + 1, sndRt, thdRt);
    sndRt -> lazy = true;
    rt = merge(fstRt, merge(sndRt, thdRt));
}

void print(Treap * rt) {
    if (rt == nullptr)
        return;
    if (rt -> lazy)
        rt -> pushDown();
    print(rt -> son[0]);
    cout << rt -> val << " ";
    print(rt -> son[1]);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(0); cout.tie(0);
    int len, qNum; cin >> len >> qNum;
    Treap * rt = nullptr;
    for (int i = 1; i <= len; i++)
        insert(rt, i);
    while (qNum--) {
        int qLeftPt, qRightPt; cin >> qLeftPt >> qRightPt;
        reverse(rt, qLeftPt, qRightPt);
    }
    print(rt);
    return 0;
}
```

## 字典树

```cpp
typedef struct _TrieNode {
    bool isWord;
    int nextArr[CHAR_SIZE];
} TrieNode;
int triePt;

int newTrieNode() {
    trieArr[triePt].isWord = false;
    for (int i = 0; i < CHAR_SIZE; i++)
        trieArr[triePt].nextArr[i] = -1;
    return triePt++;
}

void insertName(string & str) {
    int cntPt = 0, len = str.size();
    for (int i = 0; i < len; i++) {
        int cnt = str[i] - 'a';
        if (trieArr[cntPt].nextArr[cnt] == -1)
            trieArr[cntPt].nextArr[cnt] = newTrieNode();
        cntPt = trieArr[cntPt].nextArr[cnt];
    }
    trieArr[cntPt].wordId = true;
}

triePt = 0;
newTrieNode();
```

## policy_based_data_structures

### Heap

```cpp
#include<ext/pb_ds/priority_queue.hpp>
using namespace __gnu_pbds;

__gnu_pbds::priority_queue<
    int,
    less<int>,   // Smaller value at top, Big -> Small when enumerating using iterator 
    pairing_heap_tag // or: binary_heap_tag, binomial_heap_tag, rc_binomial_heap_tag, thin_heap_tag
> pq;

fst.join(snd);
```

### Set

```cpp
#include <ext/pb_ds/assoc_container.hpp>
#include <ext/pb_ds/tree_policy.hpp>
// using namespace __gnu_pbds;

__gnu_pbds::tree<
    int,
    __gnu_pbds::null_type,
    less<int>,  // less_equal<int>: lower_bound -> upper_bound
    __gnu_pbds::rb_tree_tag,
    __gnu_pbds::tree_order_statistics_node_update
> st;

st.order_of_key(val);
st.find_by_order(val);

fst.join(snd); // Join two trees (RANGE of key value should not intersect)
fst.split(val, snd);
```
