______________________________________________________________________

title: "Hello World... Again!"
date: 2019-10-15T14:28:28+08:00
math: true
utterances: 1
categories: Life
tags:

- Hello
  output:
  blogdown::html_page:
  toc: true
  highlight: zenburn

______________________________________________________________________

Hello World... Again!

After days of work I have finally managed to migrate my blog from Hexo to Blogdown + Hugo, and it actually went harder than I expected. Now with the power of Hugo, I shall enjoy faster rendering speed and much better i18n support. Plus, with the power of RMarkdown, it is now possible to create graphs and diagram in a more elegant way.

The new blog is themed with a modified version of [Hermit](https://github.com/Track3/Hermit), which is a tremendous theme and I would highly recommend to dark theme enthusiasts. I tuned its styles to match my own flavor and performed some dirty little hacks for better compatibility with Blogdown. Anyway it looks quite good to me now.

Like before, the source of this blog will be open-sourced in the `source` branch of its [repo](https://github.com/codgician/codgician.github.io). Check it out if you are interested. You can also leave your comment below on what you think.

______________________________________________________________________

Check out below to see the power of this new combination! You can view table of contents by clicking its icon in the menu bar if you are on desktop.

# A grand title

## Some code

```haskell
putStrLn "Hello World... Again!"
```

## Some math

$$
\\begin{aligned}
\\sum\\limits\_{i = 1}^{n} f(i) & = \\sum\\limits\_{i = 1}^{n} \\left[i \\in \\mathbb{P} \\right] f(i) + \\sum\\limits\_{i = 1}^{n} \\left[i \\not \\in \\mathbb{P} \\right] f(i) \\
& = \\sum\\limits\_{i = 1}^{n} \\left[i \\in \\mathbb{P} \\right] f(i) + f(1) + \\sum\\limits\_{p \\in \\mathbb{P}}\\sum\\limits\_{1 \\le p^e \\le n}f(p^e)\\sum\\limits\_{i = 1}^{\\left\\lfloor \\frac{n}{p^e} \\right\\rfloor}f(i)
\\end{aligned}
$$

## Some fun

```{r pie, fig.cap='What is inside me?', tidy=TRUE}
par(bg = "gray", mar = c(0, 1, 0, 1))
pie(
  c(90, 5, 5),
  c('git', 'fit', 'wit'),
  col = gray(seq(0.3, 0.6, length = 3))
)
```
