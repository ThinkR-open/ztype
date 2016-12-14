# ztype

How fast can you type R functions on your keyboard ?

Find out by running a zty.pe game : export R functions as instructions to type to destroy oponnents vessels.

This game was originally written by Dominic Szablewski with music from Andreas Loesch. Its original URL is
<http://www.phoboslab.org/ztype/>


## Installation & usage

```R
# install.packages("devtools")
devtools::install_github("ThinkRstat/ztype")

library(ztype)
ztype() # dplyr, ggplot2 and lubridate

require(magrittr)
c("lubridate") %>% ztype()


```


![alt tag](https://raw.githubusercontent.com/ThinkRstat/ztype/master/screen.png)

