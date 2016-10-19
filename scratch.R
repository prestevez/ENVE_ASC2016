
packages <- c("foreign",
              "ggplot2",
              "Cairo",
              "knitr",
              "lme4",
              "classInt",
              "car",
              "texreg",
              "xtable",
              "lmtest",
              "pscl",
              "sjstats",
              "sjPlot",
              "rmarkdown",
              "arm")


packages %in% rownames(installed.packages())
