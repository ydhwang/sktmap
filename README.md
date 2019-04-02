# sktmap
- R package for SK Telecom T Map API interface. Some examples and description will be added soon.
- The character processing part is assuming UTF-8 locale. Tested only on Linux and MacOS. Windows-based system may cause trouble due to its CP-949 or EUC-KR locale dafault
- SK Telecom의 T Map API 를 사용하기 위한 R 패키지 파일. 예제와 설명이 후에 추가될 예정임.
- UTF-8 encoding 기반으로 되어 있음. Linux 와 MacOS에서만 테스트 되었음. Windows 에서는 윈도의 CP-949/EUC-KR 의 기본값 설정으로 인해서 문자열 처리 문제가 발생할 수 있음.

[devtools](https://cran.r-project.org/web/packages/devtools/index.html) package 가 설치되었다는 전제 하에 
```{r}
devtools::install_github("ydhwang/sktmap")
library(sktmap)
```
으로 설치/사용준비가 가능함. package dependency 등은 추후 업데이트 예정.
