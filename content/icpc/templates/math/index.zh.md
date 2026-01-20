---
title: "Math"
language: zh
canonical: true
translationKey: icpc-templates-math
math: true
toc: true
draft: false
---


# 基础运算

## 快速幂

```cpp
long long int fastPow(long long int a, long long int n, long long int mod) {
    long long int ret = 1; a %= mod;
    while (n > 0) {
        if (n & 1)
            ret = ret * a % mod;
        a = a * a % mod; n >>= 1;
    }
    return ret;
}
```

## 防爆乘法

对于无法使用 `__int128` 的情况：

```cpp
long long int dblMul(long long int a, long long int b, long long int mod) {
    return (a * b - (long long int)((long double) a / mod * b) * mod + mod) % mod;
}

long long int slowMul(long long int a, long long int b, long long int mod) {
    long long int ret = 0; a %= mod;
    while (b > 0) {
        if (b & 1)
            ret = (ret + b) % mod;
        b = (b << 1) % mod; n >>= 1;
    }
    return ret;
}
```

# 线性递推式

## 矩阵快速幂

可以构造矩阵来加速线性递推式第 $n$ 项的计算。

```cpp
class Matrix {
public:
    int row, col;
    long long int arr[MAT_SIZE][MAT_SIZE];
    Matrix(int r, int c) {
        row = r; col = c;
        memset(arr, 0, sizeof(arr));
    }
    
    void operator += (const Matrix & b) {
        // assert(row == b.row && col == b.col);
        for (int i = 0; i < row; i++)
            for (int j = 0; j < col; j++)
                arr[i][j] = (arr[i][j] + b.arr[i][j]) % mod;
    }
    
    Matrix operator * (const Matrix & snd) const {
        // assert(col == snd.row);
        Matrix ret(row, snd.col);
        for (int i = 0; i < row; i++) {
            for (int k = 0; k < col; k++) {
                long long int cnt = arr[i][k];
                for (int j = 0; j < col; j++)
                    ret.arr[i][j] = (ret.arr[i][j] + cnt * snd.arr[k][j]) % mod;
            }
        }
        return ret;
    }
    
    Matrix & operator = (const Matrix & snd) {
        row = snd.row; col = snd.col;
        memcpy(arr, snd.arr, sizeof(snd.arr));
        return *this;
    }
};

Matrix matFastPow(Matrix mat, long long int n) {
    // assert(mat.row == mat.col);
    Matrix ret(mat.row, mat.col);
    for (int i = 0; i < mat.row; i++)
        ret.arr[i][i] = 1;
    while (n > 0) {
        if (n & 1)
            ret = ret * mat;
        mat = mat * mat; n >>= 1;
    }
    return ret;
}
```

## 特征方程求通项公式

如果是在模 $p$ 意义下求第 $n$ 项的值，且通项公式中无理数或复数均可用二次剩余代替时可考虑直接公式求解。

这里只讨论如下二阶线性递推式：

$$
f_n =
\begin{cases}
a & n \le 0 \\
b & n = 1 \\
c_1f_{n - 1} + c_2f_{n - 2} & n \ge 2
\end{cases}
$$

将其化成一个等比数列，设 $\exists \ r, s, \text{ s.t. } f_{n} - rf_{n - 1} = s(f_{n - 1} - rf_{n - 2})$，则联立原式得：

$$
\begin{cases}
c_ 1 = s + r \\
c_2 = -rs
\end{cases}
$$

消去 $s$ 即可得到*特征方程*：

$$
r^2 = c_1r + c_2
$$

记该方程的两个根为 $x_1, x_2$，则有：

$$
f_n = c_1{x_1}^n+c_2{x_2}^n
$$

代入 $f_0 = a, \ f_1 = b$ 即可得到通项公式。

# 数论分块

```cpp
/* n only */
for (int l = 1, r; l <= n; l = r + 1) {
    r = n / (n / l);
    // range = [l, r]
}
/* n and m */
tie(n, m) = make_tuple(min(n, m), max(n, m));
for (int l = 1, r; l <= n; l = r + 1) {
    r = min(n / (n / l), m / (m / l));
    // range = [l, r]
}
```

# 素数

## 线性筛

```cpp
/* Variant #1 */
int primes[SIZE], minFac[SIZE], primesPt;
void initPrimes() {
    memset(minFac, 0, sizeof(minFac)); primesPt = 0;
    for (int i = 2; i < SIZE; i++) {
        if (minFac[i] == 0)
            minFac[i] = i, primes[primesPt++] = i;
        for (int j = 0; j < primesPt && primes[j] <= min(minFac[i], (SIZE - 1) / i); j++)
            minFac[i * primes[j]] = primes[j];
    }
}
/* Variant #2 */
int primes[SIZE], primesPt; bool sieved[SIZE];
void initPrimes() {
    memset(sieved, false, sizeof(sieved)); primesPt = 0;
    for (int i = 2; i < SIZE; i++) {
        if (!sieved[i])
            primes[primesPt++] = i;
        for (int j = 0; j < primesPt; j++) {
            if (primes[j] * i > SIZE - 1)
                break;
            sieved[i * primes[j]] =  true;
            if (i % primes[j] == 0)
                break;
        }
    }
}
```

**拓展**：$\text{minFac}(x)$  记录 $x$ 最小质因子，若要对所有数快速分解质因子，则一直除其最小质因子即可。

## Miller-Robin 素性测试

代码见 Pollard-Rho 质因数分解一节。

- 对于 $n < 3.4 \times 10^{14}$，选取 $\{ 2, 3, 5, 7, 11, 13, 17 \}$ 即可保证正确性；
- 对于 $n < 3.8 \times 10^{18}$，选取 $\{ 2, 3, 5, 7, 11, 13, 17, 19, 23 \}$ 即可保证正确性；
- 对于 $n < 3.1 \times 10^{23}$，选取 $\{ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37\}$ 即可保证正确性。

## Pollard-Rho 质因数分解

```cpp
#define MR_SIZE 9
const int mr[MR_SIZE] = {2, 3, 5, 7, 11, 13, 17, 19, 23};

long long int fastPow(__int128 a, long long int n, long long int mod) {
    __int128 ret = 1; a %= mod;
    while (n > 0) {
        if (n & 1)
            ret = ret * a % mod;
        a = a * a % mod; n >>= 1;
    }
    return ret;
}

bool millerRobin(long long int n, int p) {
    for (long long int k = n - 1; k > 0; k >>= 1) {
        long long int cnt = fastPow(p, k, n);
        if (cnt != 1 && cnt != n - 1)
            return false;
        if ((k & 1) == 1 || cnt == n - 1)
            return true;
    }
    return true;
}

bool isPrime(long long int n) {
    if (n < 2)
        return false;
    for (int i = 0; i < MR_SIZE; i++)
        if (n % mr[i] == 0)
            return n == mr[i];
    bool ret = true;
    for (int i = 0; i < MR_SIZE && ret; i++)
        ret &= millerRobin(n, mr[i]);
    return ret;
}

long long int func(long long int x, long long int c, long long int md) {
    return ((__int128)x * x + c) % md;
}

long long int findFac(long long int n) {
    for (int i = 0; i < MR_SIZE; i++)
        if (n % mr[i] == 0)
            return mr[i];
    long long int c = rand() % (n - 1) + 1;
    long long int s = 0, t = 0, val = 1;
    for (long long int lim = 2; lim <<= 1; s = t, val = 1) {
        for (long long int i = 1; i <= lim; i++) {
            t = func(t, c, n);
            val = (__int128)val * abs(t - s) % n;
            if (i % 127 == 0) {
                long long int gcd = __gcd(val, n);
                if (gcd > 1)
                    return gcd;
            }
        }
        long long int gcd = __gcd(n, val);
        if (gcd > 1)
            return gcd;
    }
    assert(false); return n;
}

unordered_map<long long int, int> mp;
void pollardRho(long long int n) {
    if (n < 2)
        return;
    if (isPrime(n)) {
        mp[n]++;
        return;
    }
    long long int p = findFac(n);
    while (p >= n)
        p = findFac(p);
    pollardRho(p); pollardRho(n / p);
}

int main() {
    srand(time(0));
    mp.clear(); pollardRho(n);
}
```


# 数论函数

**注**：以下默认 $n = \prod\limits_{i = 1}^{k} p_i^{e_i}$，其中 $p_i \in \text{PRIMES}$（$n \geq 2$）。

## 欧拉函数 $\varphi(n)$

$$
\varphi(n) = n\prod\limits_{i = 1}^{k}(1 - \frac{1}{p_i})
$$

### 朴素

```cpp
long long int phi(long long int num) {
    long long int ret = num;
    for (int i = 2; 1ll * i * i <= num; i++) {
        if (num % i != 0)
            continue;
        ret -= ret / i;
        while (num % i == 0)
            num /= i;
    }
    if (num > 1)
        ret -= ret / num;
    return ret;
}
```

### 线性筛

```cpp
int minFac[SIZE], primes[SIZE], phi[SIZE], primesPt;
void initPhi() {
    memset(minFac, 0, sizeof(minFac));
    primesPt = 0; phi[1] = 1;
    for (int i = 2; i < SIZE; i++) {
        if (minFac[i] == 0) {
            phi[i] = i - 1;
            minFac[i] = i, primes[primesPt++] = i;
        }
        for (int j = 0; j < primesPt && primes[j] <= min(minFac[i], (SIZE - 1) / i); j++) {
            minFac[i * primes[j]] = primes[j];
            if (minFac[i] == primes[j]) {
                phi[i * primes[j]] = phi[i] * primes[j];
            } else {
                phi[i * primes[j]] = phi[i] * (primes[j] - 1);
            }
        }
    }
}
```

### 性质（欧拉反演基础）

$$
\varphi(ij) = \frac{\varphi(i)\varphi(j)\gcd(i, j)}{\varphi(\gcd(i, j))}
$$

$$
\sum\limits_{d \mid n} \varphi(d) = n
$$

$$
\sum\limits_{d \mid n} \frac{\mu(d)}{d} = \frac{\varphi(n)}{n}
$$

## 莫比乌斯函数 

$$
\mu(n) =
\begin{cases}
1 & n = 1 \\
0 & \exists \ e_i > 1 \\
(-1)^k & \text{otherwise}
\end{cases}
$$

### 朴素

```cpp
int mu(long long int num) {
    int ret = 1;
    for (int i = 2; 1ll * i * i <= num; i++) {
        if (num % i != 0)
            continue;
        if (num % (1ll * i * i) == 0)
            return 0;
        ret *= -1, num /= i;
    }
    if (num > 1)
        ret *= -1;
    return ret;
}
```

### 线性筛

```cpp
int minFac[SIZE], primes[SIZE], mu[SIZE], primesPt;
void initMu() {
    memset(minFac, 0, sizeof(minFac));
    primesPt = 0;
    mu[1] = 1;
    for (int i = 2; i < SIZE; i++) {
        if (minFac[i] == 0) {
            mu[i] = -1;
            minFac[i] = i, primes[primesPt++] = i;
        }
        for (int j = 0; j < primesPt && primes[j] <= min(minFac[i], (SIZE - 1) / i); j++) {
            minFac[i * primes[j]] = primes[j];
            if (minFac[i] == primes[j]) {
                mu[i * primes[j]] = 0;
            } else {
                mu[i * primes[j]] = -mu[i];
            }
        }
    }
}
```

### 性质（莫比乌斯反演基础）

$$
\sum\limits_{d \mid n} \mu(d) = 
\begin{cases}
1 & n = 1 \\
0 & \text{otherwise}
\end{cases}
$$

### 反演

$$
\begin{aligned}
F(n) & = \sum\limits_{d \mid n}f(d) \Leftrightarrow f(n) = \sum\limits_{d \mid n} \mu(d)F(\frac{n}{d}) \\
F(n) & = \sum\limits_{n \mid d}f(d) \Leftrightarrow f(n) = \sum\limits_{n \mid d} \mu(\frac{d}{n})F(d)
\end{aligned}
$$

## 约数个数 $d(n)$

$$
d(n) = \prod\limits_{i = 1}^{k}(e_i + 1)
$$

### 相关套路

$$
\begin{aligned}
\sum\limits_{i = 1}^{x} d(i) & = \sum\limits_{i = 1}^{x} \left\lfloor \frac{x}{i} \right\rfloor \\
d(xy) & = \sum\limits_{i \mid x}\sum\limits_{j \mid y} [(i, j) = 1] \\
d(xyz) & = \sum\limits_{i \mid x}\sum\limits_{j \mid y}\sum\limits_{k \mid z} [(i, j) = 1][(i, k) = 1][(j, k) = 1]
\end{aligned}
$$

### 线性筛

```cpp
int primes[SIZE], minFac[SIZE], numArr[SIZE], d[SIZE], primesPt;
void initD() {
    memset(minFac, 0, sizeof(minFac));
    primesPt = 0; d[1] = 1;
    for (int i = 2; i < SIZE; i++) {
        if (minFac[i] == 0) {
            numArr[i] = 1, d[i] = 2;
            minFac[i] = i, primes[primesPt++] = i;
        }
        for (int j = 0; j < primesPt && primes[j] <= min(minFac[i], (SIZE - 1) / i); j++) {
            minFac[i * primes[j]] = primes[j];
            if (minFac[i] == primes[j]) {
                numArr[i * primes[j]] = numArr[i] + 1;
                d[i * primes[j]] = d[i] / (numArr[i] + 1) * (numArr[i * primes[j]] + 1);
            } else {
                numArr[i * primes[j]] = 1;
                d[i * primes[j]] = d[i] * d[primes[j]];
            }
        }
    }
}
```

## 约数和 $\sigma(n)$

$$
\sigma(n) = \prod\limits_{i = 1}^{k}\sum\limits_{j = 0}^{e_i}p_i^j
$$

### 线性筛

```cpp
int primes[SIZE], minFac[SIZE], primesPt;
long long int dSum[SIZE], facSqrSum[SIZE];
void initDSum() {
    memset(minFac, 0, sizeof(minFac));
    primesPt = 0; facSqrSum[1] = 1; dSum[1] = 1;
    for (int i = 2; i < SIZE; i++) {
        if (minFac[i] == 0) {
            facSqrSum[i] = 1 + i, dSum[i] = 1 + i;
            minFac[i] = i, primes[primesPt++] = i;
        }
        for (int j = 0; j < primesPt && primes[j] <= min(minFac[i], (SIZE - 1) / i); j++) {
            minFac[i * primes[j]] = primes[j];
            if (minFac[i] == primes[j]) {
                facSqrSum[i * primes[j]] = facSqrSum[i] * primes[j] + 1;
                dSum[i * primes[j]] = dSum[i] / facSqrSum[i] * facSqrSum[i * primes[j]];
            } else {
                facSqrSum[i * primes[j]] = 1 + primes[j];
                dSum[i * primes[j]] = dSum[i] * dSum[primes[j]];
            }
        }
    }
}
```

## 狄利克雷卷积

记 $\mathbb{S}$ 代表全体数论函数，则对 $f, g \in \mathbb{S}$，定义其卷积：

$$
(f \cdot g)(n) = \sum\limits_{d \mid n}f(d)g(\frac{n}{d})
$$

$\lang \mathbb{S}, +, \cdot \rang $ 构成一个**可交换环**。其中单位元为 $\epsilon(n) = [n = 1]$。

记 $1(n) = 1, \ N(n) = n$：

- $\mu \cdot 1 = \epsilon$
- $\varphi \cdot 1 = N$ 
- $d = 1 \cdot 1$
- $\sigma = N \cdot 1$

**注意**：

- 数论函数不一定是积性函数；
- 积性函数的卷积仍是积性函数。

## 杜教筛

### 简介

构造 $f, g, h \in \mathbb{S}, \text{ s.t. } f \cdot g = h$，且其中 $\sum{h}$ 可被快速计算。记 $S(n) = \sum\limits_{i = 1}^{n} f(i)$，则有： 

$$
g(1)S(n) = \sum\limits_{i = 1}^{n}h(i) - \sum\limits_{d = 2}^{n}g(d)S(\left\lfloor \frac{n}{d} \right\rfloor)
$$

设置阈值 $t$，若 $n \le t$ 可用线性筛解决，大于阈值的则用上式递归解决。为了加速可将已求得的值放入 Hash Map 中。

**注意**：$f, g, h$ **不一定**是积性函数。

### 常见应用

- $f(i) = \varphi(i), \ S(n) = \sum\limits_{i = 1}^{n} \varphi(i)$：
  
  由 $\varphi \cdot 1 = N$ （对应 $f \cdot g = h$）：
  $$
  S(n) = \sum\limits_{i = 1}^{n}i - \sum\limits_{d = 2}^{n} S(\left\lfloor \frac{n}{d} \right\rfloor)
  $$

- $f(i) = \mu(i), \ S(n) = \sum\limits_{i = 1}^{n} \mu(i)$：

  由 $\mu \cdot 1 = \epsilon$（对应 $f \cdot g = h$）：

  $$
  S(n) = 1 - \sum\limits_{d = 2}^{n} S(\left\lfloor \frac{n}{d} \right\rfloor)
  $$

- $f(i) = i \varphi(i), \ S(n) = \sum\limits_{i = 1}^{n} i  \varphi(i)$：

   $$
   \begin{aligned}
   h= f \cdot g & = \sum\limits_{d \mid n}f(d)g(\frac{n }{d}) \\
   & = \sum\limits_{d \mid n} d \varphi(d)g(\frac{n}{d}) \\
   & \text{let } g = N \\ 
   & = n\sum\limits_{d \mid n}\varphi(d) \\
   & = n^2
   \end{aligned}
   $$

   $$
   S(n) = \sum\limits_{i = 1}^{n}i^2 - \sum\limits_{d = 2}^{n} dS(\left\lfloor \frac{n}{d} \right\rfloor)
   $$
   
- $f(i) = i^2 \varphi(i), \ S(n) = \sum\limits_{i = 1}^{n} i^2 \varphi(i)$：

   $$
   \begin{aligned}
   h = f \cdot g & = \sum\limits_{d \mid n} f(d)g(\frac{n}{d}) \\
   & = \sum\limits_{d \mid n}d^2\varphi(d)g(\frac{n}{d}) \\
   & \text{let } g = N^2 \\
   & = n^2\sum\limits_{d \mid n} \varphi(d) \\
   & = n^3
   \end{aligned}
   $$

   $$
   S(n) = \sum\limits_{i = 1}^{n}i^3 - \sum\limits_{d = 2}^{n}d^2S(\left\lfloor \frac{n}{d} \right\rfloor)
   $$

预处理 $\mathcal{O}(n^\frac{2}{3})$ 左右的数据则复杂度 $\mathcal{O}(n^{\frac{2}{3}})$；若不预处理是 $\mathcal{O}(n^\frac{3}{4})$？要记忆化！

## min25 筛

> - 当 $n \ge 10^{12}$ 时首先想想有没有其他做法！
> - 如果是求区间和注意 $R - L$ 有没有范围！

### 核心思想

下文默认记 $\mathbb{P}$ 为素数集，$P_i$ 代表其中第 $i$ 大的素数并默认 $p \in \mathbb{P}$。若函数 $f(x)$ 满足：

- $f(p)$ 为多项式；
- $f(p^e)$ 可被快速计算；
- 若 $a \perp b$，则 $f(ab) = f(a)f(b)$。

min25 筛可在 $\mathcal{O}(\frac{n^\frac{3}{4}}{\log{n}})$ 的复杂度内计算 $\sum\limits_{i = 1}^{n} f(i)$，其核心思想即将 $[1, n]$ 分为素数、$1$ 和剩余数三个部分进行讨论：

$$
\begin{aligned}
\sum\limits_{i = 1}^{n} f(i) & = \sum\limits_{i = 1}^{n} \left[i \in \mathbb{P} \right] f(i) + \sum\limits_{i = 1}^{n} \left[i \not \in \mathbb{P} \right] f(i) \\
& = \sum\limits_{i = 1}^{n} \left[i \in \mathbb{P} \right] f(i) + f(1) + \sum\limits_{p \in \mathbb{P}}\sum\limits_{1 \le p^e \le n}f(p^e)\sum\limits_{i = 1}^{\left\lfloor \frac{n}{p^e} \right\rfloor}f(i)
\end{aligned}
$$

**注**：上面提到的限制条件是充分不必要条件。

### 筛素数答案

考虑筛出不大于 $\sqrt{n}$ 的所有素数作为 $\mathbb{P}$。记 $minp(x)$ 代表 $x$ 的最小质因子，令：

$$
g(n, j) = \sum\limits_{i = 2}^{n} \left[i \in \mathbb{P} \text{ or } minp(i) > P_j \right] f(i)
$$

则有：

$$
\sum\limits_{i = 1}^{n} \left[i \in \mathbb{P} \right] f(i) = g(n, |P|)
$$

借助于埃氏筛类似的思想（考虑每次筛去了哪些数），可对 $g$ 进行如下状态转移：

$$
g(n, j) = 
\begin{cases}
g(n, j - 1) - f(P_j)\left[ g(\left\lfloor \frac{n}{P_j} \right\rfloor, j - 1) - \sum\limits_{i = 1}^{j - 1}f(P_i) \right] & {P_j}^2 \le n \\
g(n, j - 1) & \text{otherwise} 
\end{cases}
$$

对于初值，有 $g(n, 0) = \sum\limits_{i = 2}^{n}f(i)$。在实现时第一维可以滚动以节约空间。

### 筛非素数答案

令：

$$
s(n, j) = \sum\limits_{i = 2}^{n} \left[ minp(i) \ge P_j \right] f(i)
$$

则有：

$$
\sum\limits_{i = 1}^{n}f(i) = s(n, 1) + f(1)
$$

可对 $s$ 进行如下状态转移：

$$
\begin{aligned}
s(n, j) & = \sum\limits_{i = P_j + 1}^{n} \left[ i \in \mathbb{P} \right]f(i) + \sum\limits_{i = P_j + 1}^{n} \left[i \not \in \mathbb{P} \right] f(i) \\
& = g(n, |\mathbb{P}|) - \sum\limits_{k = 1}^{j - 1}f(P_k) + \sum\limits_{k = j}^{|\mathbb{P}|}\sum\limits_{{P_k}^{e + 1} \le n} \left[ f({P_k}^e) \cdot s(\left\lfloor \frac{n}{{P_k}^{e}} \right\rfloor, k + 1) + f({P_k}^{e + 1}) \right]
\end{aligned}
$$

直接递归搜即可，不需要记忆化复杂度也是对的（~~证明不来~~），如果有多个状态可以考虑开个结构体一起转移。

### 代码

【洛谷模板】积性函数： $f(p^e) = p^e (p^e - 1)$

注：`SIZE` 应设置为 $2\sqrt{n}$ 左右。

```cpp
#define SIZE 200020
#define EXP_SIZE 2
const int mod = 1e9 + 7;

int primes[SIZE], minFac[SIZE], primesPt, lim;
long long int g[SIZE][EXP_SIZE], dsc[SIZE], pfx[SIZE][EXP_SIZE], inv2, inv3;
pair<int, int> indx[SIZE];  // indx[x]: index of <x, n / x>
const pair<int, int> csts[EXP_SIZE] = {make_pair(-1, 1), make_pair(1, 2)};

long long int fastPow(long long int a, long long int n, long long int mod) {
    long long int ret = 1; a %= mod;
    while (n > 0) {
        if (n & 1)
            ret = (ret * a) % mod;
        a = (a * a) % mod; n >>= 1;
    }
    return ret;
}

void initPrimes(int siz) {
    inv2 = fastPow(2, mod - 2, mod); inv3 = fastPow(3, mod - 2, mod);
    fill(minFac + 0, minFac + siz + 1, 0); primesPt = 0;
    for (int i = 2; i <= siz; i++) {
        if (minFac[i] == 0)
            minFac[i] = i, primes[primesPt++] = i;
        for (int j = 0; j < primesPt && primes[j] <= min(minFac[i], siz / i); j++)
            minFac[i * primes[j]] = primes[j];
    }

    for (int e = 0; e < EXP_SIZE; e++)
        pfx[0][e] = fastPow(primes[0], csts[e].second, mod);
    for (int i = 1; i < primesPt; i++)
        for (int e = 0; e < EXP_SIZE; e++)
            pfx[i][e] = (pfx[i - 1][e] + fastPow(primes[i], csts[e].second, mod)) % mod;
}

const auto f = [](long long int p) {
    long long int ret = 0;
    for (int e = 0; e < EXP_SIZE; e++)
        ret = (ret + csts[e].first * fastPow(p, csts[e].second, mod)) % mod;
    return ret;
};

const auto sum = [](long long int n, long long int exp) {
    n %= mod;
    if (exp == 0)
        return n;
    long long int ret = n * (n + 1) % mod * inv2 % mod;
    if (exp == 2)
        return ret * ((n << 1) + 1) % mod * inv3 % mod;
    return ret;
};

long long int sieve(long long int x, int pt, long long int n) {
    if (x <= 1 || primes[pt] > x)
        return 0;
    int k = x <= lim ? indx[x].first : indx[n / x].second;
    long long int ret = 0;
    for (int e = 0; e < EXP_SIZE; e++)
        ret = (ret + csts[e].first * (g[k][e] - (pt == 0 ? 0 : pfx[pt - 1][e])) % mod + mod) % mod;

    for (int i = pt; i < primesPt && 1ll * primes[i] * primes[i] <= x; i++) {
        long long int pk = primes[i], pk1 = 1ll * primes[i] * primes[i];
        for (int e = 1; pk1 <= x; pk = pk1, pk1 *= primes[i], e++)
            ret = (ret + f(pk) * sieve(x / pk, i + 1, n) % mod + f(pk1)) % mod;
    }

    return (ret + mod) % mod;
}

long long int min25(long long int n) {
    lim = sqrt(n); initPrimes(lim + 1); int dscPt = 0;
    for (long long int l = 1, r; l <= n; l = r + 1) {
        r = n / (n / l); long long int v = n / l; dsc[dscPt] = v;
        for (int e = 0; e < EXP_SIZE; e++)
            g[dscPt][e] = sum(dsc[dscPt], csts[e].second) - 1;
        v <= lim ? indx[v].first = dscPt : indx[n / v].second = dscPt; dscPt++;
    }

    for (int i = 0; i < primesPt && primes[i] <= lim; i++) {
        for (int j = 0; j < dscPt && 1ll * primes[i] * primes[i] <= dsc[j]; j++) {
            long long int v = dsc[j] / primes[i];
            int k = v <= lim ? indx[v].first : indx[n / v].second;
            for (int e = 0; e < EXP_SIZE; e++)
                g[j][e] = (g[j][e] - fastPow(primes[i], csts[e].second, mod) * (g[k][e] - (i == 0 ? 0 : pfx[i - 1][e]) + mod) % mod + mod) % mod;
        }
    }
    return (sieve(n, 0, n) + 1) % mod;
}

int main() {
    long long int num; cin >> num;
    cout << min25(num) << '\n';
}
```

### 灵活变化

#### $f(p)$ 是分段函数？

Nowcoder 887K

$$
f(p^k) = 
\begin{cases}
3k + 1 & p \equiv 1 \pmod 4 \\
1 & \text{otherwise}
\end{cases}
$$

求：$\sum\limits_{i = 1}^{n} f(i)$

- 把 $2$ 从素数中 “除名” 并不影响结果（即使得 $minp(x) \ge 3$）；
- 模 $4$ 余 $1, 3$ 的素数需要分别计算，因此考虑修改 $g$ 的定义与状态转移：
  $$
  g(n, j, k) = \sum\limits_{i = 1}^{n} \left[ i \in \mathbb{P}, \ minp(p) > P_j, 
  \begin{cases}
  i \equiv 1 \pmod 4 & k = 0 \\
  i \equiv 3 \pmod 4 & k = 1
  \end{cases}
  \ \right] f(i)
  $$
  状态转移是根据余数再分别进行转移即可；对于 $s$ 最后不能忘记考虑 $2$。

#### $f$ 不是积性函数？

对于最后一条限制，其实只要待筛函数 $f$ 能够在对 $s, g$ 进行转移时被正确处理就可以了。若记 $h = \sum f(a_i)$，且 $\forall i, \ p \not\mid a_i$，若能快速计算出 $h' = \sum f(a_ip)$，则一般可用 min25 筛出答案。如：$\forall a \perp b, \ f(ab) = f(a) + f(b)$。

## 乱搞套路

$$
f(n) = \sum\limits_{d \mid n} g(d)
$$

```cpp
for (int i = 1; i <= n; i++)
    g[i] = f[i];
for (int i = 1; i <= n; i++)
    for (int j = i + 1; j <= n; j += i)
        g[j] -= g[i];
```

---

$$
\begin{aligned}
\sum\limits_{x \mid d} \left\lfloor \frac{A}{d} \right\rfloor
& = \sum\limits_{t = 1}^{\left\lfloor \frac{A}{x} \right\rfloor} \left\lfloor \frac{A}{xt} \right\rfloor \\
& = \sum\limits_{t = 1}^{\left\lfloor \frac{A}{x} \right\rfloor} \left\lfloor \frac{\left\lfloor \frac{A}{x} \right\rfloor}{t} \right\rfloor \\
& = \sum\limits_{t = 1}^{\left\lfloor \frac{A}{x} \right\rfloor} d(t)
\end{aligned}
$$

---

$$
\begin{aligned}
\sum\limits_{i = 1}^{n}\sum\limits_{d \mid i}f(d) & = \sum\limits_{d = 1}^{n}\sum\limits_{i = 1}^{n}f(d) \cdot [\gcd(d, i) = d] \\
& = \sum\limits_{d = 1}^{n} f(d )\sum\limits_{i = 1}^{\left\lfloor \frac{n}{d} \right\rfloor}[\gcd(d, i) = 1] \\
& = \sum\limits_{d = 1}^{n}f(d) \left\lfloor \frac{n}{d} \right\rfloor
\end{aligned}
$$

如果有多层和式，可以一层一层按照上面的套路化简。

---

1. 遇到形如 $\left\lfloor \frac{n}{td} \right\rfloor$ 的式子时有套路：令 $T = td$ 并枚举 $T$；
2. 经过上面一堆操作很可能化出形如 $\sum\limits_{d \mid n} \mu(d)f^k(\frac{T}{d})$ 的式子，此时往迪利克雷卷积的方向考虑即 $f \cdot \mu$，说不定利用一下 $\mu \cdot 1 = e$ 再配合杜教筛乱搞一下就搞出来了，而杜教筛中的那个前缀和说不定就能用 min25 筛乱搞；
3. 或者我们可以利用 $\mu$ 的性质，考虑 $d$ 取何值才能使 $\mu(d) \neq 0$，说不定就能发现什么；
4. 看到 $\gcd(i, j)$ 考虑直接枚举其值然后再套上莫比乌斯反演；
5. 带权的直接欧拉反演少走弯路；
6. 有时可考虑对求和进行拆分，可能会有意想不到的效果（在有 $\gcd$ 时贴近 $\varphi$ 的定义）：
   $$
   \sum\limits_{i = 0}^{n}\sum\limits_{j = 0}^{n} = \sum\limits_{i = 0}^{n}\sum\limits_{j = 0}^{i - 1} + \sum\limits_{i = 0}^{n}[j = i] + \sum\limits_{j = 0}^{n}\sum\limits_{i = 0}^{j - 1}
   $$

# 逆元与降幂

## 欧拉定理

若 $\gcd(a, p) = 1$，则：
$$
a^{\varphi(p)} \equiv 1 \pmod p
$$


## 欧拉降幂

$$
a^b = 
\begin{cases}
a^{b \bmod \varphi(p)} & \gcd(a, p ) = 1 \\
a^b & \gcd(a, p) \neq 1, \ b < \varphi(p) \\
a^{(b \bmod \varphi(p)) + \varphi(p)} & \gcd(a, p) \neq 1, \ b \ge \varphi(p)
\end{cases}
$$

## 拓展欧几里得

```cpp
long long int inv(long long int a, long long int mod) {
    long long int x, y; extEuclid(a, mod, x, y);
    return (x + mod) % mod;
}
```

## 线性预处理

```cpp
long long int invs[SIZE];
void initInv() {
    invs[1] = 1;
    for (int i = 2; i < SIZE; i++)
        invs[i] = (mod - mod / i) * invs[mod % i] % mod;
}
```

# 线性同余

## 拓展欧几里得

$$
ax + by = \gcd(x, y)
$$

```cpp
/* ax + by = gcd(x, y) */
long long int extEuclid(long long int a, long long int b, long long int & x, long long int & y) {
    if (b == 0) {
        x = 1, y = 0;
        return a;
    }
    long long int gcd = extEuclid(b, a % b, x, y), tmp = x;
    x = y; y = tmp - y * (a / b);
    return gcd;
}
```

## 一次同余方程

$$
ax \equiv b \pmod p
$$

```cpp
 /* ax = b (mod p) */
long long int linCon(long long int a, long long int b, long long int p) {
    long long int x, y, gcd = extEuclid(a, p, x, y);
    a /= gcd; p /= gcd;
    if (b % gcd != 0)
        return -1;
    long long int ret = b / gcd * x;
    return (ret % p + p) % p;
}
```

## 中国剩余定理

对于线性同余方程组：
$$
\begin{cases}
x \equiv b_1 & \pmod {p_1} \\
x \equiv b_2 & \pmod {p_2} \\
\dots & \\
x_n \equiv b_n & \pmod {p_n}
\end{cases}
$$

若 $p_1, p_2, \dots p_n$ 两两互素，则可使用中国剩余定理。令 $M = \prod\limits_{i = 1}^{n} p_i, \ m_i = \frac{M}{p_i}, \ m_im_i' \equiv 1 \pmod {p_i}$，则方程组有解：

$$
x \equiv \sum\limits_{i = 1}^{n} b_im_im_i' \pmod M
$$

## 拓展中国剩余定理

```cpp
// x % mods[i] = csts[i]
long long int mods[SIZE], csts[SIZE];
long long int crt(int equNum) {
    long long int modProd = mods[0], ans = csts[0];
    for (int i = 1; i < equNum; i++) {
        long long int x, y, a = modProd, b = mods[i];
        long long int c = (csts[i] - ans % b + b) % b;
        long long int gcd = extEuclid(a, b, x, y), bg = b / gcd;
        if (c % gcd != 0)
            return -1;
        long long int cntAns = (c / gcd % bg * x % bg + bg) % bg;
        // long long int cntAns = fastMul(x, c / gcd, bg);
        ans += modProd * cntAns;
        modProd *= bg;
    }
    return (ans % modProd + modProd) % modProd;
}
```

# 二次剩余

$$
x^2 \equiv n \pmod p
$$

## 解的存在性

对于 $p$ 为奇素数的情形：

$$
\newcommand{\legendre}[2]{\genfrac{(}{)}{}{}{#1}{#2}}
\begin{aligned}
\legendre{n}{p} & \equiv n^{\frac{p - 1}{2}} \pmod p \\ 
& \equiv 
\begin{cases}
1 & \pmod p & \text{such solution exists} \\
-1 & \pmod p & \text{otherwise}
\end{cases}
\end{aligned}
$$

## Tonelli–Shanks 算法

适用于模数为 $p^k$ 的情形，若为合数则考虑用 CRT 合并。

```cpp
/* 见 Claris 模板 */
```

# 离散对数

$$
a^x \equiv b \pmod p
$$

## BSGS 算法

令 $x = it + j, \text{ s.t. } i \in [0, \frac{p}{t}], \ j \in [0, t)$。问题转换为：

$$
(a^t)^i = a^{-j}b \pmod p
$$

借助 Hash 表 meet in the middle 即可。**为了求稳可考虑求出指数以防止出错！**

```cpp
// k * a ^ x = b (mod p), returns minimum non-negative solution
long long int bsgs(long long int k, long long int a, long long int b, long long int p) {  
    a %= p; b %= p;
    if (a == 0)
        return b == 0 ? 1 : -1;
    unordered_map<long long int, int> mp;
    int t = sqrt(p) + 1; long long int tmp = k % p;
    for (int j = 0; j < t; j++) {
        if (mp.find(tmp) != mp.end())
            break;
        mp[tmp] = j; tmp = tmp * a % p;
    }
    // phip could be replaced by ord(a, p)
    long long int phip = phi(p), inv = fastPow(a, phip - t % phip, p); tmp = b;
    for (int i = 0; i <= t; i++) {
        int j = mp.find(tmp) == mp.end() ? -1 : mp[tmp];
        if (j != -1) 
            return 1ll * i * t + j; 
        tmp = tmp * inv % p;
    }
    return -1;
}
long long int extBsgs(long long int a, long long int b, long long int p) {
    a %= p; b %= p;
    if (b == 1)
        return 0;
    long long int offset = 0, fac = 1;
    while (true) {
        if (fac == b) 
            return offset;
        long long int gcd = __gcd(a, p);
        if (b % gcd != 0)
            return -1;
        if (gcd == 1)
            break;
        offset++; p /= gcd; b /= gcd; 
        fac = a / gcd * fac % p;
    }
    long long int ret = bsgs(fac, a, b, p);
    return ret == -1 ? ret : ret + offset;
}
```

## Pohlig–Hellman 算法

```cpp
/* Baby step Giant step */
long long int bsgs(long long int a, long long int b, long long int p, long long int ord) { 
    // Replace phip with ord
}
/* Pohlig-Hellman Algorithm */
long long int getOrd(long long int a, long long int p) {
    // Calculate order: ord(a, p)
}
long long int pohligHellman(long long int a, long long int b, long long int mod, 
                            long long int ord, long long int p, int e) {    // ord = p ^ e
    long long int ret = 0;
    long long int y = fastPow(a, ord / p, mod), p1 = ord / p, p2 = 1;
    for (int k = 0; k < e; k++) {
        long long int inv = fastPow(fastPow(a, ord - 1, mod), ret, mod);
        long long int bk = fastPow((__int128)inv * b % mod, p1, mod); p1 /= p;
        long long int dk = bsgs(y, bk, mod, p); // The last p stands for order of the group
        if (dk == -1)
            return -1;
        ret = (ret + p2 * dk % ord) % ord; p2 *= p;
    }
    return ret;
}
int primes[2] = {2, 3}, primesPt = 2;   // Prime factors of phi(p)
long long int hiCon(long long int a, long long int b, long long int p) {
    long long int ord = getOrd(a, p), tmp = ord; int equNum = 0;
    for (int i = 0; i < primesPt; i++) {
        long long int prime = primes[i];
        if (tmp % prime != 0)
            continue;
        int exp = 1; long long int cnt = prime; tmp /= prime;
        while (tmp % prime == 0)
            exp++, cnt *= prime, tmp /= prime;
        long long int ai = fastPow(a, ord / cnt, p), bi = fastPow(b, ord / cnt, p);
        csts[equNum] = pohligHellman(ai, bi, p, cnt, prime, exp);
        if (csts[equNum] == -1)
            return -1;
        mods[equNum++] = cnt;
    }
    return crt(equNum); // Chinese Remainder Theorem
}
```

# 二项式系数

## 简介与计算

$$
\binom{n}{m} = \frac{n!}{m! (n - m)!} \quad (0 \le m \le n)
$$

### 递推预处理

$$
\binom{n}{m} = \binom{n - 1}{m - 1} + \binom{n - 1}{m}
$$

```cpp
void initBinom() {
    binom[0][0] = 1;
    for (int i = 1; i < SIZE; i++) {
        binom[i][0] = 1; binom[i][i] = 1;
        for (int j = 1; j < i; j++)
            binom[i][j] = binom[i - 1][j - 1] + binom[i - 1][j];
    }
}
```

### 取模意义下:     $\mathcal{O}(1)$ 

```cpp
long long int binom(int n, int m) {
    return m > n ? 0 : factorials[n] * invFactorials[m] % mod * invFactorials[n - m] % mod;
}
long long int permu(int n, int m) {
    return m > n ? 0 : factorials[n] * invFactorials[n - m] % mod;
}
```

### 朴素:    $\mathcal{O}(n\log{n})$ 

```cpp
long long int binom(long long int n, long long int m) {
    long long int ret = 1;
    for (int i = 1; i <= n; i++) {
        long long int a = (m + i - n) % mod, b = i % mod;
        ans = ans * (a * fastPow(b, mod - 2) % mod) % mod;
    }
    return ret;
}
```

## 性质

$$
\begin{aligned}
m \cdot \binom{n}{m} & = n \cdot \binom{n - 1}{m - 1} \\
\sum\limits_{i = 1}^{n} i\binom{n}{i} & = n2^{n - 1} \\
\sum\limits_{i = 1}^{n} i^2\binom{n}{i} & = n(n + 1)2^{n - 2} \\
\sum\limits_{i = 0}^{n} \binom{n}{i}^2 & = \binom{2n}{n} \\
\sum\limits_{i = 0}^{n} \binom{m + i}{m} & = \binom{m + n + 1}{m + 1} \\
\sum\limits_{i = 0}^{k}(-1)^i \binom{n}{i} & = (-1)^k\binom{n - 1}{k}
\end{aligned}
$$

## Lucas 定理

$$
\binom{n}{m} \bmod p = \binom{n \bmod p}{m \bmod p} \cdot \binom{\left\lfloor\frac{n}{p}\right\rfloor}{\left\lfloor\frac{m}{p}\right\rfloor}
$$

### 素数

```cpp
long long int lucas(long long int n, long long int m) {
    return n == 0 ? 1 : binom(n % mod, m % mod) * lucas(n / mod, m / mod) % mod;
}
```

### 非素数

模数固定，多次询问。

```cpp
class Fac {
public:
    long long int p, pk;
    vector<long long int> factorials, invFactorials;
};
/* Initializations */
vector<Fac> facs;
void factorize(long long int num) {
    facs.clear(); facs.reserve(20);
    for (int i = 0; i < primesPt && primes[i] * primes[i] <= num; i++) {
        if (num % primes[i] != 0)
            continue;
        Fac f = Fac{primes[i], primes[i], vector<long long int>(), vector<long long int>()};
        num /= primes[i];
        while (num % primes[i] == 0)
            f.pk *= primes[i], num /= primes[i];
        facs.push_back(f);
    }
    if (num > 1)
        facs.push_back(Fac{num, num, vector<long long int>(), vector<long long int>()});
}
void initFactorials() {
    for (auto & f : facs) {
        auto & v = f.factorials; v.reserve(f.pk); v.push_back(1);
        for (int i = 1; i < f.pk; i++)
            v.push_back(v.back() * (i % f.p == 0 ? 1 : i) % f.pk);
        auto & vi = f.invFactorials; vi.reserve(f.pk); vi.push_back(inv(v.back(), f.pk));
        for (int i = f.pk; i >= 1; i--)
            vi.push_back(vi.back() * (i % f.p == 0 ? 1 : i) % f.pk);
        reverse(vi.begin(), vi.end());
    }
}
long long int calc(long long int n, const Fac & f) {
    if (n == 0)
        return 1;
    long long int ret = fastPow(f.factorials[f.pk - 1], n / f.pk, f.pk);
    return ret * f.factorials[n % f.pk] % f.pk * calc(n / f.p, f) % f.pk;
}
long long int calcInv(long long int n, const Fac & f) {
    if (n == 0)
        return 1;
    long long int ret = fastPow(f.invFactorials[f.pk - 1], n / f.pk, f.pk);
    return ret * f.invFactorials[n % f.pk] % f.pk * calcInv(n / f.p, f) % f.pk;
}
/* Primes */
long long int comb1(long long int n, long long int m, const Fac & f) {
    if (n == 0)
        return 1;
    const auto comb = [f](long long int n, long long int m) {
        return m > n ? 0 : f.factorials[n] * f.invFactorials[m] % f.p * f.invFactorials[n - m] % f.p;
    };
    return comb(n % f.p, m % f.p) * comb1(n / f.p, m / f.p, f) % f.p;
}
/* Power of primes */
long long int comb2(long long int n, long long int m, const Fac & f) {
    long long int occ = 0;
    for (long long int i = n; i > 0; i /= f.p)
        occ += i / f.p;
    for (long long int i = m; i > 0; i /= f.p)
        occ -= i / f.p;
    for (long long int i = n - m; i > 0; i /= f.p)
        occ -= i / f.p;
    return fastPow(f.p, occ, f.pk) * calc(n, f) % f.pk * calcInv(m, f) % f.pk * calcInv(n - m, f) % f.pk;
}
long long int extLucas(long long int n, long long int m) {
    int equNum = 0;
    for (const auto & f : facs) {
        mods[equNum] = f.pk; 
        csts[equNum] = f.p == f.pk ? comb1(n, m, f) : comb2(n, m, f);
        equNum++;
    }
    return crt(equNum);
}
int main() {
    long long int n, m, p; cin >> n >> m >> p;
    initPrimes(); factorize(p); initFactorials();
    cout << extLucas(n, m) << '\n';
}
```

## 二项式反演

$$
f(n) = \sum\limits_{i = 0}^{n} (-1)^i \cdot \binom{n}{i}\cdot g(i) \Leftrightarrow g(n) = \sum\limits_{i = 0}^{n}(-1)^i \cdot \binom{n}{i} \cdot f(i)
$$

更常用的形式：
$$
f(n) = \sum\limits_{i = 0}^{n} \binom{n}{i} \cdot g(i) \Leftrightarrow g(n) = \sum\limits_{i = 0}^{n}(-1)^{n - i} \cdot \binom{n}{i} \cdot f(i)
$$

### 错排问题

$n$ 封信对应 $n$ 封信封，求**恰好**所有信都装错的方案数。

令 $f(i)$ 代表恰好装错 $i$ 封的方案数，$g(i)$ 代表至多装错 $i$ 封的方案数。显然 $g(i) = i!$，且：

$$
g(n) = \sum\limits_{i = 0}^{n} \binom{n}{i} \cdot f(i)
$$

由二项式反演：

$$
\begin{aligned}
f(n) & = \sum\limits_{i = 0}^{n} (-1)^{n - i} \cdot \binom{n}{i} \cdot g(i) \\
& = \sum\limits_{i = 0}^{n} (-1)^{n - i} \cdot \frac{n!}{i!(n - i)!} \cdot i! \\
&= n!\sum\limits_{i = 0}^{n} \frac{(-1)^i}{i!}
\end{aligned}
$$

### 球染色问题

用 $k$ 种颜色给 $n$ 个球进行染色，要求相邻两球颜色不同，且每种颜色至少用一次。

令 $f(i)$ 代表恰好使用 $i$ 种颜色的方案数，$g(i)$ 代表至多使用 $i$ 种颜色的方案数。显然 $g(i) = i(i - 1)^{n - 1}$，且：

$$
g(k) = \sum\limits_{i = 0}^{k} \binom{k}{i} \cdot f(i)
$$

由二项式反演：

$$
f(k) = \sum\limits_{i = 0}^{k} (-1)^{k - i} \cdot \binom{k}{i} \cdot g(i)
$$

# 高斯二项式系数

$$
\binom{n}{m}_q = \frac{(1 - q ^ n)(1 - q ^ {n - 1}) \cdots (1 - q^{n - m + 1})}{(1 - q)(1 - q ^ 2) \cdots (1 - q ^ m)} \quad (0 \le m \le n)
$$

记：

$$
\left[ k \right]_q = \sum\limits_{i = 0}^{k - 1} q ^ i =
\begin{cases}
\frac{1 - q^k}{1 - q} & k \neq 1 \\
k  & k = 1
\end{cases}
$$

则：

$$
\binom{n}{m}_q = \frac{\left[ n \right]_q!}{\left[m\right]_q! \left[n - m\right]_q!}
$$

**组合意义**：对于长度为 $n$ 的序列 $a$， $a_i \in \{0, 1\}$，其中 $0$ 出现 $m$ 次，$1$ 出现 $(n - m)$ 次。若在计算其全排列个数时对每一个排列乘上系数 $q^d$，其中 $d$ 为该排列种逆序对的对数，得到的答案即：

$$
\frac{\left[ n \right]_q!}{\left[m\right]_q! \left[n - m\right]_q!} = \binom{n}{m}_q
$$

这一结论可以推广至 $a_i \in S, \ |S| = k$ 且 $S$ 种第 $i$ 个元素出现 $c_i$ 次的情形：

$$
\frac{\left[n\right]_q!}{\prod\limits_{i = 1}^{k}\left[c_i\right]_q!}
$$

```cpp
void init(int len, int q) {
    numbers[0] = 0; numbers[1] = 1; long long int cnt = q;
    for (int i = 2; i <= len; i++) {
        numbers[i] = (numbers[i - 1] + cnt) % mod;
        cnt = cnt * q % mod;
    }
    factorials[0] = 1;
    for (int i = 1; i <= len; i++)
        factorials[i] = factorials[i - 1] * numbers[i] % mod;
    invFactorials[len] = fastPow(factorials[len], mod - 2, mod);
    for (int i = len - 1; i >= 0; i--)
        invFactorials[i] = invFactorials[i + 1] * numbers[i + 1] % mod;
}
// The remaining part is similar to the case with ordinary binomial coefficient
```

---

$$
\begin{aligned}
\binom{n}{m}_q & = \binom{n - 1}{m - 1}_q + q ^ m \binom{n - 1}{m}_q \\
& = q^{n - m}\binom{n - 1}{m - 1}_q + \binom{n - 1}{m}_q \\
\lim\limits_{q \rightarrow 1} \binom{n}{m}_q & = \binom{n}{m} \\
\lim\limits_{n \rightarrow +\infty} \binom{n}{m} & = \frac{1}{\left[ m \right]_q!(1 - q)^m} \quad (|q| < 1)
\end{aligned}
$$

# 类欧几里得

## 常见形式

$$
\begin{aligned}
f(a, b, c, n) & = \sum\limits_{i = 0}^{n} {\left\lfloor \frac{ai + b}{c} \right\rfloor} \\
g(a, b, c, n) & = \sum\limits_{i = 0}^{n} {i \left\lfloor \frac{ai + b}{c} \right\rfloor} \\
h(a, b, c, n) & = \sum\limits_{i = 0}^{n} {\left\lfloor \frac{ai + b}{c} \right\rfloor}^2
\end{aligned}
$$

---

$$
\begin{aligned}
f(a, b, c, n) = &
\begin{cases}
\left\lfloor \frac{a}{c} \right\rfloor \frac{n(n + 1)}{2} + \left\lfloor \frac{b}{c} \right\rfloor (n + 1) + f(a \bmod c, b \bmod c, c, n) & a \ge c \lor b \ge c \\
nm - f(c, c - b - 1, a, m - 1) & \text{otherwise}
\end{cases}
\\ \\
g(a, b, c, n) = &
\begin{cases}
\left\lfloor \frac{a}{c} \right\rfloor \frac{1}{6}n(n + 1)(2n + 1) + \left\lfloor \frac{b}{c} \right\rfloor \frac{1}{2}n(n + 1) + g(a \bmod c, b \bmod c, c, n) & a \ge c \lor b \ge c \\
\frac{1}{2} \left[ mn(n + 1) - h(c, c - b - 1, a, m - 1) - f(c, c - b - 1, a, m - 1) \right] & \text{otherwise}
\end{cases}
\\ \\
h(a, b, c, n) = &
\begin{cases}
\left\lfloor \frac{a}{c} \right\rfloor^2 \frac{1}{6}n(n + 1)(2n + 1) + \left\lfloor \frac{a}{c} \right\rfloor \left\lfloor \frac{b}{c} \right\rfloor 2n(n + 1) + \left\lfloor \frac{b}{c} \right\rfloor^2(n + 1) & \\
\ + 2\left\lfloor \frac{a}{c} \right\rfloor g(a \bmod c, b \bmod c, c, n) + 2\left\lfloor \frac{b}{c} \right\rfloor f(a \bmod c, b \bmod c, c, n) & \\
\ + h(a \bmod c, b \bmod c, c, n) & a \ge c \lor b \ge c \\
nm^2 - 2g(c, c - b - 1, a, m - 1) - f(c, c - b - 1, a, m - 1) & \text{otherwise}
\end{cases}
\end{aligned}
$$

---

```cpp
/* Just f */
long long int quasiEuclidean(long long int a, long long int b, long long int c, long long int n) {
    if (n < 0)
        return 0;
    if (a == 0)
        return (n + 1) * (b / c);
    if (a >= c || b >= c) {
        long long int tmp = (n & 1) ? ((n + 1) >> 1) * n : (n >> 1) * (n + 1);
        return (a / c) * tmp + (b / c) * (n + 1) + quasiEuclidean(a % c, b % c, c, n);
    }
    long long int m = (a * n + b) / c;
    return n * m - quasiEuclidean(c, c - b - 1, a, m - 1);
}
/* Huge Data: __int128 is preferred! */
long long int quasiEuclidean(long long int a, long long int b, long long int c, long long int n) {
    if (n < 0)
        return 0;
    if (a == 0)
        return (n + 1) % mod * ((b / c) % mod) % mod;
    if (a >= c || b >= c) {
        long long int tmp = ((n & 1) ? ((n + 1) >> 1) % mod * (n % mod) : (n >> 1) % mod * ((n + 1) % mod)) % mod;
        return ((a / c) % mod * (tmp % mod) % mod + (b / c) % mod * ((n + 1) % mod) % mod + quasiEuclidean(a % c, b % c, c, n) % mod) % mod;
    }
    long long int m = ((__int128)a * n + b) / c;
    return (n % mod * (m % mod) % mod - quasiEuclidean(c, c - b - 1, a, m - 1) % mod + mod) % mod;
}
/* f, g, h (Untested!!!) */
class Node {
public:
    int f, g, h;
};
long long int getSum(long long int n) {
    return (n & 1) ? ((n + 1) >> 1) * n : (n >> 1) * (n + 1);
}
long long int getSqrSum(long long int n) {
    long long int ans = getSqrSum(n);
    return (ans % 3 == 0) ? ans / 3 * ((n << 1) + 1) : ((n << 1) + 1) / 3 * ans;
}
Node quasiEuclidean(long long int a, long long int b, long long int c, long long int n) {
    if (n < 0)
        return 0;
    if (a == 0 && b < c)
        return Node{0, 0, 0};
    if (a >= c || b >= c) {
        long long int lfa = a / c, lfb = b / c;
        Node ret = quasiEuclidean(a % c, b % c, c, n);
        ret.h += lfa * lfa * getSqrSum(n) + lfa * lfb * (getSum(n) << 2) + lfb * lfb * (n + 1);
        ret.h += lfa * (ret.g << 1) + lfb * (ret.f << 1);
        ret.f += lfa * getSum(n) + lfb * (n + 1);
        ret.g += lfa * getSqrSum(n) + lfb * getSum(n);
        return ret;
    }
    long long int m = (a * n + b) / c;
    Node ret = Node{0, 0, 0}, last = quasiEuclidean(c, c - b - 1, a, m - 1);
    ret.f = n * m - last.f;
    ret.g = m * getSum(n) - ((last.f + last.h) >> 1);
    ret.h = n * m * m - (last.g << 1) - last.f;
    return ret;
}
```

## BZOJ  - 3817

$$
f(a, b, c, n) = \sum\limits_{i = 0}^{n} \left\lfloor i \cdot \frac{at + b}{c} \right\rfloor
$$

其中 $t$ 为非整数的实数（若为整数需要另外讨论）。

$$
f(a, b, c, n) =
\begin{cases}
\left\lfloor \frac{at + b}{c} \right\rfloor \frac{1}{2}n(n + 1) + f(a, b - c \left\lfloor \frac{at + b}{c} \right\rfloor, c, n) & \frac{at + b}{c} \ge 1 \\
nm - f(ac, -bc, a^2t^2 - b^2, m) & \frac{at + b}{c} < 1
\end{cases}
$$

## BZOJ -  2187

给定 $a, b, c, d$，求解 $p, q$ 满足：

$$
\frac{a}{b} < \frac{p}{q} < \frac{c}{d}
$$

优先最小化 $q$，其次最小化 $p$。

---

记原问题的解为 $f(a, b, c, d) = \langle p, q \rangle$，并先讨论两种可以直接得出答案的情况：

1. 如果 $\frac{a}{b}$ 和 $\frac{c}{d}$ 间存在正整数，显然取 $q = 1$；即满足以下条件时取 $p = \left\lfloor \frac{a}{b} \right\rfloor + 1, q = 1$ 为解。

$$
\begin{cases}
\left\lfloor \frac{c}{d} \right\rfloor - \left\lfloor \frac{a}{b} \right\rfloor \ge 1 & c \not\equiv 0 \pmod d \\
\frac{c}{d} - 1 - \left\lfloor \frac{a}{b} \right\rfloor \ge 1 & c \equiv 0 \pmod d
\end{cases}
$$

2. 若 $a = 0$，则取 $p = 1, q = \left\lfloor \frac{c}{d} \right\rfloor + 1$ 为解。

对于剩下不能直接得出答案的情况中，若 $a \le b$ 且 $c \le d$ ，对问题进行转换：

$$
\frac{a}{b} < \frac{p}{q} < \frac{c}{d} \Leftrightarrow \frac{d}{c} < \frac{q}{p} < \frac{b}{a}
$$

由此可以对子问题 $f(d, c, b, a) = \langle p', q' \rangle$ 进行求解，并在回溯时令 $p = q', q = p'$ 。

接下来就只需考虑 $c > d$ 的情况。如果此时出现了上面所说的 $\frac{a}{b}$ 和 $\frac{c}{d}$ 间存在正整数，那么直接回溯就好了。如果不存在，考虑对问题进行如下变换从而尽量缩小 $a$：

$$
\begin{aligned}
& \frac{a}{b} < \frac{p}{q} < \frac{c}{d} \\
& \Leftrightarrow \frac{a}{b} - \left\lfloor \frac{a}{b} \right\rfloor < \frac{p}{q} -\left\lfloor \frac{a}{b} \right\rfloor < \frac{c}{d} - \left\lfloor \frac{a}{b} \right\rfloor \\
& \Leftrightarrow \frac{a - b\left\lfloor \frac{a}{b} \right\rfloor}{b} < \frac{p - q\left\lfloor \frac{a}{b} \right\rfloor}{q} < \frac{c - d\left\lfloor \frac{a}{b} \right\rfloor}{d} \\
& \Leftrightarrow \frac{a \bmod b}{b} < \frac{p - q\left\lfloor \frac{a}{b} \right\rfloor}{q} < \frac{c - d\left\lfloor \frac{a}{b} \right\rfloor}{d}
\end{aligned}
$$

由此一来，可以对子问题 $f(a \bmod b, b, c - d\left\lfloor \frac{a}{b} \right\rfloor, d) = \langle p'', q'' \rangle$ 求解，并在回溯时令 $p = p'' + q''\left\lfloor \frac{a}{b} \right\rfloor, q = q''$ 。

---

```cpp
void simplify(long long int & a, long long int & b) {
    long long int gcd = __gcd(a, b);
    a /= gcd; b /= gcd;
}
pair<long long int, long long int> solve(long long int a, long long int b, long long int c, long long int d) {
    simplify(a, b); simplify(c, d);
    long long int left = a / b + 1, right = (c - 1) / d;
    if (left <= right)
        return make_pair(left, 1);
    if (a == 0)
        return make_pair(1, d / c + 1);
    if (a <= b && c <= d) {
        pair<long long int, long long int> ans = solve(d, c, b, a);
        swap(ans.first, ans.second);
        return ans;
    }

    long long int t = a / b;
    pair<long long int, long long int> ans = solve(a % b, b, c - t * d, d);
    ans.first = ans.first + ans.second * t;
    return ans;
}
```

---

**应用**：对于 $ab^{-1} \equiv x \pmod p$，给定 $x, p$（$p$ 为质数），求解最小的满足 $0 < a < b$ 的 $a, b$。

有 $a = bx - pt \ (t > 0)$，因此：
$$
\begin{aligned}
& 0 < bx - pt < b \\
\Rightarrow & \frac{p}{x} < \frac{b}{t} < \frac{p}{x - 1} \\
\end{aligned}
$$
应用上述算法即可求出 $b, t$，进而求出 $a$。

# 多项式

## 积分

```cpp
class Polyn {
public:
    vector<int> vec;
    Polyn(int siz = 0) { vec.resize(siz); }
    Polyn operator * (const Polyn & snd) const {
        Polyn ret(vec.size() + snd.vec.size() - 1); 
        for (int i = 0; i < (int)vec.size(); i++)
            for (int j = 0; j < (int)snd.vec.size(); j++)
                ret.vec[i + j] = (ret.vec[i + j] + 1ll * vec[i] * snd.vec[j] % mod) % mod;
        return ret;
    }
    Polyn integrate() {
        Polyn ret(vec.size() + 1);
        for (int i = 0; i < (int)vec.size(); i++)
            ret.vec[i + 1] = vec[i] * fastPow(i + 1, mod - 2, mod) % mod;
        return ret;
    }
    long long int calc(long long int x) {
        long long int ret = 0, xe = 1;
        for (int i = 0; i < (int)vec.size(); i++)
            ret = (ret + xe * vec[i] % mod) % mod, xe = xe * x % mod;
        return ret;
    }
};
```

## 卷积

记 $A(x) = \sum\limits_{i = 0}^{d} a_i x^i, \ B(x) = \sum\limits_{i = 0}^{d}b_ix^i$，则其卷积：

$$
(A \cdot B)(x) = \sum\limits_{i = 0}^{2d}(\sum\limits_{j = 0}^{i}a_j b_{i - j})x^i
$$

### FFT

```cpp
const double pi = acos(-1.0);
class Complex {
public:
    double real, imag;
    Complex operator + (const Complex & snd) & { return Complex{real + snd.real, imag + snd.imag}; };
    Complex operator - (const Complex & snd) & { return Complex{real - snd.real, imag - snd.imag}; };
    Complex operator * (const Complex & snd) & { return Complex{real * snd.real - imag * snd.imag, real * snd.imag + imag * snd.real}; };
    Complex conj() & { return Complex{real, -imag}; }
};
Complex fst[SIZE], snd[SIZE], omg[SIZE], inv[SIZE];
int pos[SIZE], fstLen, sndLen, len, lim;
void init() {
    len = 1, lim = 0; double cnt = 0;
    while (len < fstLen + sndLen - 1)
        len <<= 1, lim++;
    for (int i = 0; i < len; i++, cnt += pi + pi) {
        omg[i] = {cos(cnt / len), sin(cnt / len)};
        inv[i] = omg[i].conj(); pos[i] = (pos[i >> 1] >> 1) | ((i & 1) << (lim - 1)) ;
    }
}
void fft(Complex * arr, Complex * omg) {
    for (int i = 0; i < len; i++) 
        if (i < pos[i])
            swap(arr[i], arr[pos[i]]);
    for (int l = 2; l <= len; l <<= 1) {
        int midPt = l >> 1, step = len / l;
        for (Complex * p = arr; p != arr + len; p += l) {
            for (int i = 0, cnt = 0; i < midPt; i++, cnt += step) {
                Complex t = omg[cnt] * p[i + midPt];
                p[i + midPt] = p[i] - t; p[i] = p[i] + t;
            }
        }
    }
}
void multiply() {
    init(); fft(fst, omg); fft(snd, omg);
    for (int i = 0; i < len; i++)
        fst[i] = fst[i] * snd[i];
    fft(fst, inv);
}
int main() {
    cin >> fstLen >> sndLen; fstLen++; sndLen++;
    for (int i = 0; i < fstLen; i++) {
        int cnt; cin >> cnt; fst[i].real = cnt;
    }
    for (int i = 0; i < sndLen; i++) {
        int cnt; cin >> cnt; snd[i].real = cnt;
    }
    multiply();
    cout << (int)(fst[0].real / len + 0.5);
    for (int i = 1; i < fstLen + sndLen - 1; i++)
        cout << ' ' << (int)(fst[i].real / len + 0.5);
    cout << '\n';
    return 0;   
}
```

### NTT

```cpp
const int mod = 998244353, pr = 3;
long long int fst[SIZE], snd[SIZE], omg[SIZE], inv[SIZE];
int pos[SIZE], fstLen, sndLen, len, lim;
long long int fastPow(long long int a, long long int n, long long int mod) {
    long long int ret = 1; a %= mod;
    while (n > 0) {
        if (n & 1)
            ret = (ret * a) % mod;
        a = (a * a) % mod; n >>= 1;
    }
    return ret;
}
void init() {
    len = 1, lim = 0;
    while (len < fstLen + sndLen - 1)
        len <<= 1, lim++;
    omg[0] = 1; inv[0] = 1; 
    long long int w =  fastPow(pr, (mod - 1) / len, mod);
    for (int i = 1; i < len; i++) {
        omg[i] = omg[i - 1] * w % mod;
        inv[len - i] = omg[i];
        pos[i] = (pos[i >> 1] >> 1) | ((i & 1) << (lim - 1));
    }
}
void ntt(long long int * arr, long long int * omg) {
    for (int i = 0; i < len; i++) 
        if (i < pos[i])
            swap(arr[i], arr[pos[i]]);
    for (int l = 2; l <= len; l <<= 1) {
        int midPt = l >> 1, step = len / l;
        for (long long int * p = arr; p != arr + len; p += l) {
            for (int i = 0, cnt = 0; i < midPt; i++, cnt += step) {
                long long int t = omg[cnt] * p[i + midPt] % mod;
                p[i + midPt] = (p[i] - t + mod) % mod; 
                p[i] = (p[i] + t) % mod;
            }
        }
    }
}
void multiply() {
    init(); int lenInv = fastPow(len, mod - 2, mod);
    ntt(fst, omg); ntt(snd, omg);
    for (int i = 0; i < len; i++)
        fst[i] = fst[i] * snd[i] % mod;
    ntt(fst, inv);
    for (int i = 0; i < fstLen + sndLen - 1; i++)
        fst[i] = fst[i] * lenInv % mod;
}
int main() {
    cin >> fstLen >> sndLen; fstLen++; sndLen++;
    for (int i = 0; i < fstLen; i++)
        cin >> fst[i];
    for (int i = 0; i < sndLen; i++)
        cin >> snd[i];
    multiply();
    long long int lenInv = fastPow(len, mod - 2, mod);
    cout << fst[0];
    for (int i = 1; i < fstLen + sndLen - 1; i++)
        cout << ' ' << fst[i];
    cout << '\n';
    return 0;   
}
```

---

**任意模数 NTT**：选 $3$ 个大模数然后 CRT 合并答案，常取 $998244353, \ 1004535809, \ 469762049$ 因为它们原根都是 $3$。

为了防止爆 `long long`，可首先对前两项进行合并（可能需要快速乘），得到如下式子：

$$
\begin{cases}
x \equiv A & \pmod M \\
x \equiv b_3 & \pmod {p_3}
\end{cases}
$$

直接令答案 $x = kM + A$，则有：

$$
kM + a \equiv b_3 \pmod {p_3} \Rightarrow k \equiv (b_3 - A)M^{-1} \pmod {p_3}
$$

直接求 $k$，然后求 $x$ 即可。

---

`SIZE` 建议设置为最高幂次的 $3 \sim 4$ 倍，如果有多个式子相乘则可以类似启发式合并将复杂度降低为 $\mathcal{O}(\log{n})$。

## 插值

求作次数 $\le d$ 的多项式 $P_d(x) = \sum\limits_{i = 0}^{d} a_ix$，使其满足 $(d + 1)$ 对条件：$P_d(x_i) = y_i$。插值法可以不高于 $\mathcal{O}(d^2)$ 预处理，并以不高于 $\mathcal{O}(d \log{d})$ 的复杂度回答询问 $P_d(x)$。

### 拉格朗日插值

令基函数：

$$
l_i(x) = \prod\limits_{j = 0, \ j \neq i}^{d} \frac{x - x_j}{x_i - x_j}
$$

则多项式可被表示为：

$$
P_d(x) = \sum\limits_{i = 0}^d l_i(x)y_i
$$

---

可以引入重心以对拉格朗日插值进行优化：
$$
P_d(x) = \sum\limits_{i = 0}^{d}y_i \cdot \frac{\prod\limits_{j = 0 \ j \neq i}^{d}(x - x_j)}{\prod\limits_{j = 0, \ j \neq i}^{d}(x_i - x_j)}
$$

令：

$$
w(x) = \prod\limits_{j = 0}^{d}(x - x_j)
$$

$$
t_i = \frac{y_i}{\prod\limits_{j = 0, \ j \neq i}^{d} (x_i - x_j)}
$$

则有：

$$
P_d(x) = w(x)\sum\limits_{i = 0}^{d}\frac{t_i}{x - x_i}
$$

这样一来，在插入新点时，只需 $\mathcal{O}(d)$ 更新 $t_i$ 即可。

```cpp
pair<int, int> arr[SIZE]; long long int lag[SIZE]; 
void initLag(int num) {    // num = highest exp
    for (int i = 0; i < num; i++) {
        lag[i] = arr[i].second % mod;
        for (int j = 0; j < num; j++) {
            if (i == j)
                continue;
            lag[i] = lag[i] * fastPow(arr[i].first - arr[j].first , mod - 2) % mod;
        }
    }
}
long long int lagrange(int x, int num) {    // retrieve f(x)
    long long int ret = 0;
    for (int i = 0; i < num; i++)
        ret = (ret + lag[i] * fastPow(x - arr[i].first, mod - 2)) % mod;
    for (int i = 0; i < num; i++)
        ret = ret * (x - arr[i].first) % mod;
    return (ret + mod) % mod;
}
```

### 线性插值

已知 $d$ 次多项式 $F(x)$ 在整点 $0, 1, \dots, d$ 处的值，我们可以 $\mathcal{O}(d)$ 时间复杂度求出 $F(x)$ 或以 $\mathcal{O}(d \log d)$ 复杂度求出原多项式 $F(x)$。

$$
\begin{aligned}
P_d(x) & = \sum\limits_{i = 0}^{d}y_i\prod\limits_{j = 0, \ j \neq i}^{d} \frac{x - x_j}{x_i - x_j} \\
& = \sum\limits_{i = 0}^{d}y_i \frac{\prod\limits_{j = 0, \ j \neq i}^{d} (x - j)}{\prod\limits_{j = 0, \ j \neq i}^{d} (i - j)}
\end{aligned}
$$

考虑 $\mathcal{O}(n)$ 预处理：

$$
\begin{cases}
pre_i & = \prod\limits_{j = 0}^{i} (x - j) \\
suf_i & = \prod\limits_{j = i}^{d} (x - j) \\
fac_i & = i!
\end{cases}
$$

则：

$$
P_d(x) = \sum\limits_{i = 0}^{d}y_i \cdot (-1)^{d - i} \cdot \frac{pre_{i - 1} \cdot suf_{i + 1}}{fac_{i} \cdot fac_{d - i}}
$$

这样就可以把预处理的复杂度降低到 $\mathcal{O}(n)$。

```cpp
long long int val[SIZE], pre[SIZE], suf[SIZE], rem[SIZE];
long long int factorials[SIZE], invFactorials[SIZE];

// Initialize factorials + f(0) -> f(exp)
void init() {}

// exp stands for highest exponent, y stands for val array
long long int lagrangeCons(long long int x, int exp, long long int * y) {
    x %= mod; pre[0] = x; suf[exp] = x - exp;
    for (int i = 1; i <= exp; i++) 
        pre[i] = pre[i - 1] * (x - i) % mod;
    for (int i = exp - 1; i >= 0; i--)
        suf[i] = suf[i + 1] * (x - i) % mod;

    long long int ret = 0;
    for (int i = 0; i <= exp; i++) {
        long long int cnt = y[i] * (i == 0 ? 1ll : pre[i - 1]) % mod * (i == exp ? 1ll : suf[i + 1]) % mod;
        cnt = cnt * invFactorials[i] % mod * invFactorials[exp - i] % mod;
        cnt = ((exp + i) & 1) ? -cnt : cnt;
        ret = (ret + cnt + mod) % mod;
    }
    return ret;
}
```

---

我们也可以借助二项式反演得到这一结论。注意到 $\binom{x}{0}, \binom{x}{1}, \dots \binom{x}{d}$ 次数不同，并且线性无关。不妨令：
$$
F(x) = \sum\limits_{i = 0}^{d} \binom{x}{i} \cdot a_i
$$

由二项式反演：

$$
\begin{aligned}
a_k & = \sum\limits_{i = 0}^{k} (-1)^{k - i} \binom{k}{i} \cdot F(i) \\
\frac{p_k}{k!} & = \sum\limits_{i = 0}^{k} \frac{F(i)}{i!} \cdot \frac{(-1)^{k - i}}{(k - i)!}
\end{aligned}
$$

可以构造两个多项式：

$$
A(x) = \sum\limits_{i = 0}^{d}\frac{F(i)}{i!}x^i, \ B(x) = \sum\limits_{i = 0}^{d} \frac{(-1)^i}{i!}x^i
$$

两者 FFT 卷积一下就可以求出每一项系数，复杂度 $\mathcal{O}(d \log d)$。

---

对于 $x > d$ 的 $F(x)$，有：

$$
\begin{aligned}
F(x) 
& = \sum\limits_{i = 0}^{d} \binom{x}{i} \cdot a_i \\
& = \sum\limits_{i = 0}^{d} \binom{x}{i} \left[ \sum\limits_{j = 0}^{i} (-1)^{i - j} \binom{i}{j}F(j) \right] \\
& = \sum\limits_{j = 0}^{d} F(j) \sum\limits_{i = j}^{d} (-1)^{i - j} \binom{x}{i} \binom{i}{j} \\
\end{aligned}
$$

其中：

$$
\begin{aligned}
\sum\limits_{i = j}^{d}(-1)^{i - j} \cdot \binom{x}{i}\binom{i}{j}
& = \sum\limits_{i = j}^{d} (-1)^{i - j} \frac{x!}{(x - i)!i!} \cdot \frac{i!}{j!(i - j)!} \\
& = \sum\limits_{i = j}^{d} (-1)^{i - j}\frac{x!}{j!(x - j)!} \cdot \frac{(x - j)!}{(x - i)!(i - j)!} \\
& = \sum\limits_{i = j}^{d} (-1)^{i - j} \binom{x}{j}\binom{x - j}{i - j} \\
& = \binom{x}{j} \sum\limits_{k = 0}^{d - j} (-1)^k \binom{x - j}{k}
\end{aligned}
$$

又由于：

$$
\sum\limits_{i = 0}^{k} (-1)^i \binom{n}{i} = (-1)^k \binom{n - 1}{k}
$$

因此：

$$
\begin{aligned}
F(x) & = \sum\limits_{j = 0}^{d}F(j)\binom{x}{j}(-1)^{d - j} \binom{x - j - 1}{d - j} \\
& = \sum\limits_{j = 0}^{d}(-1)^{d - j}F(j) \frac{x!(x - j - 1)!}{j!(x - j)!(x - d - 1)!(d - j)!} \\
& = \sum\limits_{j = 0}^{d} (-1)^{d - j} F(j) \frac{x(x - 1) \dots (x - d)}{(d - j)!j!(x - j)}
\end{aligned}
$$

于是我们得到了跟最开始一样的结论。

### 牛顿插值

拉格朗日插值法缺陷：每增加一个插值节点就需要重新计算基函数 $l_i(x)$。

定义差商：

- $f[x_i, x_j] = \frac{f(x_i) - f(x_j)}{x_i - x_j} \ (i \neq j)$ 称为 $f(x)$ 在点 $x_i, \ x_j$ 处的一阶差商； 
- $f[x_i, x_j, x_k] = \frac{f[x_i, x_j] - f[x_j], x_k}{x_i - x_k} \ (i \neq j \neq k)$ 称为 $f(x)$ 在点 $x_i, \ x_j, \ x_k$ 处的二阶差商；
- $\dots$
-  $f[x_0, \dots, x_d] = \frac{f[x_0, \dots, x_{d - 1}] - f[x_1, \dots x_d]}{x_0 - x_d}$ 即 $f(x)$ 的 $d$ 阶差商。

将后项不断带入前项，即可得到牛顿插值公式：

$$
\begin{aligned}
f(x) = f(x_0) & + f[x_0, x_1](x - x_0) + \dots \\ 
& + f[x_0, x_1, \dots x_d](x - x_0)(x - x_1) \dots (x - x_{d - 1}) \\
& + f[x, x_0, \dots, x_d](x - x_0)(x - x_1) \dots (x - x_d)
\end{aligned}
$$

最后一项带有未知数 $x$，故当作余项并舍弃。

下例代码中，给定 $n$ 个点，$q$ 次询问，每次询问要求使用第 $[l, r]$ 个点进行插值。

```cpp
pair<int, int> arr[SIZE];
long long int delta[SIZE][SIZE], inv[INV_SIZE];
/* Initialize inversions ... */
int main() {
    int num;
    while (cin >> num) {
        for (int i = 0; i < num; i++)
            cin >> arr[i].first >> arr[i].second;
        for (int i = 0; i < num; i++)
            delta[i][i] = arr[i].second;
        for (int d = 1; d < num; d++)
            for (int i = 0; i + d < num; i++)
                delta[i][i + d] = (delta[i][i + d - 1] - delta[i + 1][i + d]) * getInv(arr[i].first - arr[i + d].first) % mod;
        int qNum; cin >> qNum;
        while (qNum--) {
            int l, r, x; cin >> l >> r >> x; l--; r--;
            long long int ans = 0, prod = 1;
            for (int i = l; i <= r; i++) {
                ans = (ans + prod * delta[l][i]) % mod;
                prod = prod * (x - arr[i].second) % mod;
            }
            cout << (ans + mod) % mod << '\n';
        }
    }
    return 0;
}
```

# 数位 DP

```cpp
/* An example: Codeforces 628D (Partial) */
long long int dfs(int pos, int rem, bool isZero, bool hasLim) {
    if (pos < 0)
        return !isZero && rem == 0;
    if (!hasLim && dp[pos][rem] != -1)
        return dp[pos][rem];
    long long int ans = 0; int lim = hasLim ? cntArr[pos] : 9;
    for (int i = 0; i <= lim; i++) {
        /* Your code here...
        int nextRem = (rem * 10 + i) % m;
        ans = (ans + dfs(pos - 1, nextRem, isZero && i == 0, hasLim && i == lim)) % mod;
        */
    }
    if (!hasLim)
        dp[pos][rem] = ans;
    return ans;
}
// Int -> array
long long int work(long long int cnt) {
    int len = 0;
    while (cnt) {
        cntArr[len++] = cnt % 10;
        cnt /= 10;
    }
    cntArr[len] = 0;
    return dfs(len - 1, 0, true, true);
}
// String -> array
long long int work(string & str) {
    int len = str.size();
    for (int i = 0; i < len; i++)
        cntArr[i] = str[len - i - 1] - '0';
    return dfs(len - 1, 0, true, true);
}
```

# 置换群

## 运算

### 整数幂

若置换 $T$ 可表示为 $1$ 个长度为 $n$ 的轮换 $c$，可用如下算法求出 $T^e$ 轮换乘积形式：

- $T^e$ 可表示为 $\gcd(n, e)$ 个长度为 $\frac{n}{\gcd(n, e)}$ 的轮换 $c'$；
- 对于第 $i$ 个轮换 $c'_i$：$c'_i[j] = c[(ej + i) \bmod n]$。

## 计数

### Burnside 定理

有限群 $G$ 作用于有限集合 $S$，轨道数为：

$$
O = \frac{1}{|G|} \sum\limits_{g \in G} |Fix(g)|
$$

其中：

$$
Fix(g) = \{s \in S \mid g \circ s = s \}
$$

### Pólya 定理

$$
O = \frac{1}{|G|} \sum\limits_{g \in G} m^{c(g)}
$$

其中 $m$ 为颜色数，$c(g)$ 为置换 $g$ 分解成轮换的个数。即：若把每个轮换内的元素都染成相同颜色，就成了不动点。

---

**Example 1**：长为 $n$ 的环，$m$ 种颜色，可旋转可翻转。

*旋转*：考虑循环置换群 $\mid G \mid = n$，$g_i \in G$ 代表逆时针旋转 $\frac{2\pi i}{n}$，则有 $g_i = {g_1}^i$。依据置换群幂的性质：

$$
c(g_i) = \gcd(n, i)
$$

可以通过枚举 $\gcd$ 的值将复杂度降至 $\mathcal{O}(\sqrt{n})$：

$$
\begin{aligned}
\frac{1}{|G|}\sum\limits_{i = 1}^{n}m^{\gcd(n, i)} & = \frac{1}{|G|}\sum\limits_{t | n}\varphi(\frac{n}{t}) \cdot m^t
\end{aligned}
$$

另外，旋转 $t$ 位的置换下，第 $i$ 个元素一定出现于第 $(i \bmod \gcd(n, t)) + 1$ 个轮换中。而不动点的充要条件即轮换内所有元素颜色相同，故只需考虑前 $\gcd(n, t)$ 个元素的染色方案即为不动点个数。因此若带有额外限制可用 DP 解决。

*翻转*：

* $n$ 为偶数：
  * 过点的对称轴：$c(g) = \frac{1}{2}n + 1$
  * 过边的对称轴：$c(g) = \frac{1}{2}n$
* $n$ 为奇数：
  * 既过点又过边的对称轴：$c(g) = \frac{1}{2}(n - 1) + 1$

```cpp
getFac(n);  // Store all factors of n into vector<int>::fac
long long int ans = 0;
// Rotation
for (const auto & i : facs)
    ans = (ans + phi(n / i) * fastPow(m, i) % mod) % mod;
// Inversion
if (n & 1)
    ans = (ans + n * fastPow(m, ((n - 1) >> 1) + 1) % mod) % mod;
else
    ans = (ans + (n >> 1) * (fastPow(m, (n >> 1) + 1) + fastPow(m, n >> 1)) % mod) % mod;
ans = ans * fastPow(n << 1, mod - 2) % mod;
```

---

**Example 2**：边长为 $n$ 正方形，$m$ 种颜色，可旋转可翻转。

*旋转*：置换群 $G = \{ 0^{\circ}, 90^{\circ}, 180^{\circ}, 270^{\circ} \}$， 轮换个数 $c(g_i)$：

- $n$ 是奇数：

    $$
    \begin{aligned}
    c(0^{\circ}) & = n^2 \\
    c(90^{\circ} \text{or } 270^{\circ}) & = \frac{1}{4}(n^2 - 1) + 1 \\
    c(180^{\circ}) & = \frac{1}{2}(n^2 - 1) + 1
    \end{aligned}
    $$
    
- $n$ 是偶数：

    $$
    \begin{aligned}
    c(0^{\circ}) & = n^2 \\
    c(90^{\circ} \text{or } 270^{\circ}) & = \frac{1}{4}n^2 \\
    c(180^{\circ}) & = \frac{1}{2}n^2
    \end{aligned}
    $$
    

*翻转*：

- $n$ 为偶数：
  - 过对角线对称轴：$c(g) = \frac{1}{2}(n^2 - n) + n$
  - 过边中点的对称轴：$c(g) = \frac{1}{2}n^2$
- $n$ 为奇数：$c(g) = \frac{1}{2}(n^2 - n) + n$

---

**Example 3**：边长为 $n$ 正方体，$m$ 种颜色，可旋转。

- 保持不动：$8$ 个长为 $1$ 的轮换：$1$ 种；
- 绕对立面中心转：
  - $90^{\circ}, 270^{\circ}$：$2$ 个长为 $4$ 的轮换：$3 \times 2 = 6$ 种；
  - $180^{\circ}$：$4$ 个长为 $2$ 的轮换：$3$ 种；
- 绕对立边中心转 $180^{\circ}$：$4$ 个长为 $2$ 的轮换： $6 \times 1 = 6$ 种；
- 绕对角点转 $120^{\circ}, 240^{\circ}$：$4$ 个长度为 $2$ 的轮换： $4 \times 2 = 8$ 种。

综上：$O = \frac{1}{24} \left[m^8 + (3 + 6 + 8)m^4 + 6m^2\right]$ 

---

**Example 4**：$n$ 个点无向完全图，$m$ 种颜色，求本质不同无向完全图个数。

两张图若对点重标号后可以重合即为同构。对点的置换群即 $n$ 阶对称群 $S_n$，$|S_n| = n!$。每个点置换可以被表示成若干点轮换的乘积，而该点置换下，边两端的两点有两种情况：在同一个轮换里，或在两个不同轮换里（注意接下来考虑的是轮换，那么对应的操作就是循环右移）。

- 两点在同一轮换里的边：记点轮换大小为 $x$，则可构成 $\left\lfloor \frac{x}{2} \right\rfloor$ 个边轮换。

  考虑 $x$ 个点构成的完全图，将点等距离放在圆周上，则显然长度相等的线段构成一个边轮换，共 $\left\lfloor \frac{x}{2} \right\rfloor$ 个。

- 两点在不同轮换里的边：记点轮换大小分别为 $x, y$，则可构成 $\gcd(x, y)$ 个边轮换。

  需要移 $\text{lcm}(x, y)$ 次才能转回原图形，发现每个轮换大小均为 $\text{lcm}(x, y)$。共 $xy$ 条边，则轮换个数 $\frac{xy}{\text{lcm}(x, y)} = \gcd(x, y)$ 个。 

由于置换群规模较大，故考虑剪枝。考虑对 $n$ 的每一种拆分方案：$n = \sum\limits_{i = 1}^{k} t_i a_i$，其中 $a_i$ 代表点轮换大小，$t_i$ 为该大小的点轮换个数。则满足上述条件的点置换个数为：

$$
\frac{n!}{\prod\limits_{i = 1}^{k} t_i! \cdot a_i^{t_i}}
$$

其中除去 $a_i^{t_i}$ 的原因是因为每个大小为 $a_i$ 的点轮换会被重复算 $a_i$ 次。

爆搜所有的拆分方案即可，同一边轮换内的边都应当同色，运用一下 Pólya 定理即可。

# 高斯整数

- $\Z [i] = \{a + bi \  \mid \ a, b \in \Z \} \ \text{ where }i^2 = -1$，构成可交换环。
- $x = a + bi$ 的范 $N(x) = (a + bi)(a - bi) = a^2 + b^2$；
- 若 $\exist t \text{ s.t. } xt = 1$，则 $t$ 是高斯整数中的可逆元。$x$ 是可逆元的充要条件为 $N(x) = 1$，只有 $4$ 个：$\pm 1, \ \pm i$；
- 范是积性的：$N(zw) = N(z)N(w)$，且一定非负。
- $x = a + bi$ 为素数当且仅当：
  - $a, b$ 中有一个是 $0$，且另一个数的绝对值是形如 $4n + 3$ 的素数；
  - $a, b$ 均不为 $0$，且 $a^2 + b^2$ 是素数。

# 自适应辛普森积分

用二次函数来拟合，其中二次函数通过 $l, mid, r$ 三点确定：

$$
\int_{l}^{r} f(x)dx \approx \frac{r - l}{6}\left[f(l) + 4f(\frac{l + r}{2}) + f(r)\right]
$$

直接拟合精度糟糕，因此考虑通过分治缩小拟合规模多次拟合从而提升精度。$eps$ 往死里面设！

```cpp
double simpson(double l, double r) {
    double mid = (l + r) / 2;
    return (r - l) / 6 * (f(l) + 4 * f(mid) + f(r));
}
double adaptiveSimpson(double l, double r, double sp) {
    double mid = (l + r) / 2;
    double lsp = simpson(l, mid), rsp = simpson(mid, r);
    if (abs(sp - lsp - rsp) < eps)
        return lsp + rsp;
    return adaptiveSimpson(l, mid, lsp) + adaptiveSimpson(mid, r, rsp);
}
adaptiveSimpson(l, r, simpson(l, r));
```

# 杂项

## 计算星期几

```cpp
// 1582AD+, Returns [1, 7]: Monday -> Sunday
int getDay(Date d) {
    if (d.month < 3)
        d.month += 12, d.year--;
    return (d.day + ((d.month + 1) * 26) / 10 + d.year + d.year / 4 + 6 * (d.year / 100) + d.year / 400 + 5) % 7 + 1;
}
```

## 康托展开

$\text{permutation} \rightarrow \text{rank}$

```cpp
int id(const vector<int> & vec) {
    int ret = 0, fac = 1, len = vec.size();
    for (int i = len - 1; i >= 0; i--) {
        int cnt = 0;
        for (int j = i + 1; j < len; j++)
            cnt += (vec[i] > vec[j]);
        ret += cnt * fac; fac *= (len - i);
    }
    return ret;
}
```

## 勒让德定理

在正数 $n!$ 的素因子标准分解式中，素数 $p$ 的最高指数：

$$
L_p(n!) = \sum\limits_{k \ge 1} \left\lfloor \frac{n}{p^k} \right\rfloor
$$

## 威尔逊定理

当且仅当 $p$ 是素数时，有：

$$
(p - 1)! = -1 \pmod p
$$

## 费马平方和定理

奇素数 $p$ 能被表示成 $p = a^2 + b^2$ 的充要条件为 $p \equiv 1 \pmod 4$。 

## 快速计算 $(2x - 1)!! \bmod 2^k$

$$
(2x - 1)!! = \prod\limits_{i = 1}^{x} (2i - 1)
$$

令 $P_k(x) = \prod\limits_{i = 1}^{k}(2i + 2x - 1)$，则所求即为 $P_x(0)$。利用下面三个性质即可：

$$
\begin{aligned}
P_{1}(x) &= 2x + 1 \\
P_{2k}(x) & = P_k(x) \cdot P_k(x + k) \\
P_{k + 1}(x) & = (2x + 2k + 1)P_k(x) \\
\end{aligned}
$$

由于 $x$ 的系数是 $2$，所以只需要维护前 $k$ 项就好了。

```cpp
#define SIZE 64
class Polyn {
public:
    vector<unsigned long long int> vec;
    Polyn(int siz = 0) { vec.resize(siz); }
    Polyn operator * (const Polyn & snd) const {
        Polyn ret(min(SIZE, (int)(vec.size() + snd.vec.size() - 1)));
        for (int i = 0; i < min(SIZE, (int)vec.size()); i++)
            for (int j = 0; j < min(SIZE - i, (int)snd.vec.size()); j++)
                ret.vec[i + j] += vec[i] * snd.vec[j];
        return ret;
    }
    Polyn xAdd(unsigned long long int inc) {
        Polyn ret(vec.size());
        for (int i = 0; i < (int)vec.size(); i++)
            for (int j = 0; j <= i; j++)
                ret.vec[j] += vec[i] * binom[i][j] * fastPow(inc, i - j);
        return ret;
    }
};

Polyn solve(unsigned long long int x) {
    if (x == 0) {
        Polyn ret; ret.vec.push_back(1);
        return ret;
    }
    if (x & 1) {
        Polyn qaq; 
        qaq.vec.push_back((x << 1) - 1);
        qaq.vec.push_back(2);
        return qaq * solve(x - 1);
    }
    auto ret = solve(x >> 1);
    return ret * ret.xAdd(x >> 1);
}
```

## 欧拉级数

$$
\sum\limits_{n \ge 1} \frac{1}{n^2} = \frac{\pi^2}{6}
$$

**任意两个正整数互质概率**：记 $p_i$ 为第 $i$ 大素数，则任意正整数可被 $p_i$ 整除概率为 $\frac{1}{p_i}$，任选两个正整数都被 $p_i$ 整除概率 $\frac{1}{p_i^2}$，则两个正整数互质概率：

$$
\begin{aligned}
\prod\limits_{i \in \mathbb{N}^{+}} (1 - \frac{1}{p_i^2}) 
& = \frac{1}{\prod\limits_{i \in \mathbb{N}^{+}} \frac{1}{1 -\frac{1}{p_i^2}}} \\
& = \frac{1}{\prod\limits_{i \in \mathbb{N}^{+}}(1 + \frac{1}{p_i^2} + \frac{1}{p_i^4} + \dots) }\\
& = \frac{1}{\sum\limits_{n \in \mathbb{N}^{+}} \frac{1}{n^2}} = \frac{6}{\pi^2}
\end{aligned}
$$

## 因数集的最大互质子集

令 $n = \prod\limits_{i = 1}^{k} {p_i}^{e_i}$，求 $n$ 的因数集 $\mathbb{F}$ 的最大互质子集的大小。

记 $\deg(n) = \sum\limits_{i = 1}^{k} e_i$，构造集合 $\mathbb{S} = \{ d \mid d \in \mathbb{F} \ \land \ \deg(d) = \left\lfloor \frac{\deg(n)}{2} \right\rfloor \}$，则 $\mathbb{S}$ 则为所求作集合。

## Fibonacci 与 GCD

记 $F_i$ 代表 Fibonacci 数列的第 $i$ 项，则有：

$$
\gcd(F_n, F_m) = F_{\gcd(n, m)}
$$

## n-维球面上的整点个数

记 $f(d)$ 代表 $n$ 维坐标系中离原点距离为 $d$ 的整点个数，则 $\frac{f(d)}{2n}$ 为积性函数。 

## 异或和

$$
\oplus_{i = 1}^{n} i =
\begin{cases}
n & i \equiv 0 \pmod 4 \\
1 & i \equiv 1 \pmod 4 \\
n + 1 & i \equiv 2 \pmod 4 \\
0 & i \equiv 3 \pmod 4
\end{cases}
$$

---

另外可以按位考虑，记 $\oplus_{i = 1}^{n} a_i$ 的二进制表示下第 $k$ 低位上的值为 $f(n, k)$，则：

$$
f(n, k) = \sum\limits_{i = 1}^{n} \left\lfloor \frac{a_i}{2^k} \right\rfloor \pmod 2
$$

## $x^i $ 关于 $i$ 的前缀和

$$
\sum\limits_{i = 0}^{k}x^i = \frac{x^{i + 1} - 1}{x - 1}
$$
