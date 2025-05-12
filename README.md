# sktmap
- R package for SK Telecom T Map API interface. Some examples and description will be added soon.
- The character processing part is assuming UTF-8 locale. Tested only on Linux and MacOS. Windows-based system may cause trouble due to its CP-949 or EUC-KR locale dafault
- R package file to use SK Telecom's T Map API.
- Based on UTF-8 encoding, and tested on Linux and MacOS environments. Character encoding may cause some trouble on Windows because of CP-949/EUC-KR character set.

Assuming that [devtools](https://cran.r-project.org/web/packages/devtools/index.html) package is installed on the system, it can be installed by
```{r}
devtools::install_github("ydhwang/sktmap")
library(sktmap)
```

