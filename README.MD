# Welcome to my R repository

This repository will provide some examples from my daily R workloads and parts of my PhD studies. Like everyone in the field of educational psychology, I dealed with challenging statistical problems. I will report about them.

## Specifying a multi-group longitudinal CFA

This example shows you how to use [`lavaan`](http://lavaan.ugent.be/tutorial/cfa.html), [`knitr`](https://yihui.name/knitr/), [`semTools`](https://cran.r-project.org/web/packages/semTools/index.html), [`kableExtra`](https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html) and [`psych`](https://cran.r-project.org/web/packages/psych/index.html) to generate a multi-group longitudinal CFA. Additionally, the examples shows you how to automize the necessary table production for the results of the [measurement invariances testing](https://github.com/Zirkonium88/R-Stuff/blob/master/Dissertation/Long_MG_CFA.R). It was used as a part of thesis, written with [`bookdown`](https://bookdown.org). The output table looks similar to this:

![inline](/Dissertation/Sample_Table.png)

## Specifying a multi-group latent growth modell

This [example](https://github.com/Zirkonium88/R-Stuff/blob/master/Dissertation/Growth_MG.R) how latent growth models can be established with [`lavaan`](http://lavaan.ugent.be/tutorial/cfa.html). The model specification is based on:

- Little, T. D. (2013). Longitudinal Structural Equation Modeling. New York: Guilford Press.
- Grimm, K. J., Ram, N. & Estabrook, R. (2016). Growth modeling: Structural equation and multilevel modeling approaches. Guilford Publications.
- Wu, A. D., Liu, Y., Gadermann, A. M. & Zumbo, B. D. (2010). Multiple-indicator mul- tilevel growth model: A solution to multiple methodological challenges in longitudinal studies. Social Indicators Research, 97 (2), 123–142. doi:10.1007/s11205-009-9496-8

## Dealing with DIF-analysis

The following [example](https://github.com/Zirkonium88/R-Stuff/blob/master/Dissertation/DIF_Analysis.R) shows, how I used [`pairwise`](https://cran.r-project.org/web/packages/pairwise/index.html) for my DIF analysis.
In a first step, I recoded my Likert scales with [`car`](https://cran.r-project.org/web/packages/car/index.html). The scales need to start with zero. In the example, these are five. In addition, the example shows the automated export of four DIF analyzes in a combined graph in pdf format and calling the export using [`R-markdown`](https://rmarkdown.rstudio.com) or [`bookdown`](https://bookdown.org) combined with [`knitr`](https://yihui.name/knitr/).

