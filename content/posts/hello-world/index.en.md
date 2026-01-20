---
title: "Hello World... Again!"
date: 2019-10-15
language: en
canonical: false
translationKey: hello-world
tags:
  - life
  - hello
math: true
toc: false
draft: false
---


Hello World... Again!

After days of work I have finally managed to migrate my blog from Hexo to Blogdown + Hugo, and it actually went harder than I expected. Now with the power of Hugo, I shall enjoy faster rendering speed and much better i18n support. Plus, with the power of RMarkdown, it is now possible to create graphs and diagram in a more elegant way. 

The new blog is themed with a modified version of [Hermit](https://github.com/Track3/Hermit), which is a tremendous theme and I would highly recommend to dark theme enthusiasts. I tuned its styles to match my own flavor and performed some dirty little hacks for better compatibility with Blogdown. Anyway it looks quite good to me now.

Like before, the source of this blog will be open-sourced in the `source` branch of its [repo](https://github.com/codgician/codgician.github.io). Check it out if you are interested. You can also leave your comment below on what you think.

--- 

Check out below to see the power of this new combination! You can view table of contents by clicking its icon in the menu bar if you are on desktop.

# A grand title

## Some code

```haskell
putStrLn "Hello World... Again!"
```

## Some math

$$
\begin{aligned}
\sum\limits_{i = 1}^{n} f(i) & = \sum\limits_{i = 1}^{n} \left[i \in \mathbb{P} \right] f(i) + \sum\limits_{i = 1}^{n} \left[i \not \in \mathbb{P} \right] f(i) \\
& = \sum\limits_{i = 1}^{n} \left[i \in \mathbb{P} \right] f(i) + f(1) + \sum\limits_{p \in \mathbb{P}}\sum\limits_{1 \le p^e \le n}f(p^e)\sum\limits_{i = 1}^{\left\lfloor \frac{n}{p^e} \right\rfloor}f(i)
\end{aligned}
$$

## Some fun

(R Markdown chart removed during migration to Hakyll)