<!-- README.md is generated from README.Rmd. Please edit that file -->
creditriskau - Australian credit risk capital tools
===================================================

You will be able to calculated credit risk capital related quantities using this package. These reflect the standards outlined in [APRA's credit risk prudential standards](http://www.apra.gov.au/adi/PrudentialFramework/Pages/prudential-standards-and-guidance-notes-for-adis.aspx), and consequently reflects the Australian implementation of the [Basel III capital standards](http://www.bis.org/bcbs/basel3.htm).

Retail exposures
----------------

The following illustrates the capital ratios associated with retail exposures with different credit risk characteristics.

``` r
library(tidyverse)
#> Loading tidyverse: ggplot2
#> Loading tidyverse: tibble
#> Loading tidyverse: tidyr
#> Loading tidyverse: readr
#> Loading tidyverse: purrr
#> Loading tidyverse: dplyr
#> Conflicts with tidy packages ----------------------------------------------
#> filter(): dplyr, stats
#> lag():    dplyr, stats
library(creditriskau)
x <- seq(0.01, 0.99, by = 0.03)
pd <- rep(x, 3)
lgd <- 0.20
sub_class <- rep(c("mortgage", "qrr", "other"), each = length(x))
k <- retail_capital(pd, lgd, sub_class)
df <- tibble(pd, lgd, sub_class, k)
ggplot(df, aes(x = pd, y = k, colour = sub_class)) + 
  geom_point() +
  theme_minimal() +
  labs(x = "PD", y = "Capital ratio", colour = "Sub-asset class")
```

![](README-retail_example-1.png)

Non-retail exposures
--------------------

The following illustrates the capital ratios associated with non-retail exposures with different credit risk characteristics.

``` r
size <- rep(c(1, 10, 100), each = length(x))
k <- non_retail_capital(pd, lgd, size, 1, FALSE)
df <- tibble(pd, lgd, size, k)
ggplot(df, aes(x = pd, y = k, colour = as.character(size))) + 
  geom_point() +
  theme_minimal() +
  labs(x = "PD", y = "Capital ratio", colour = "Size (A$m)")
```

![](README-non_retail_example-1.png)

Capital ratios for specialised lending facilities subject to slotting can be accessed using the `slotting_capital()` function.

Counterparty credit risk exposures
----------------------------------

To be completed.
