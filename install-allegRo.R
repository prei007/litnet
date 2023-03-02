# install AllegRo from hard disk

install.packages("~/Documents/misc/r-stuff/allegRo", 
                 repos = NULL,
                 type = "source")

library(allegRo)

# Install AllegRo from github (preferred)

# install.packages("devtools") - if not installed already
library(devtools)
install_github("baasman/allegRo")