## 
# Learning httr
###

# Mainly from https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html


# The response
library(httr)
r <- GET("http://httpbin.org/get")
r

status_code(r)
http_status(r)
# For overview of status code see 
# https://www.flickr.com/photos/girliemac/sets/72157628409467125

# The request
r <- GET("http://httpbin.org/get", 
         query = list(key1 = "value1", key2 = "value2")
)
r

r <- POST("http://httpbin.org/post", body = list(a = 1, b = 2, c = 3))
r

url <- "http://httpbin.org/post"
body <- list(a = 1, b = 2, c = 3)
POST(url, body = body, encode = "multipart", verbose())

body <- "I am a long message"
POST(url, body = body, encode = "form", verbose())


