azHeaders <- function(additional){
  main <- c(
    `User-Agent` = "R",
    # `Content-Type` = "application/octet-stream Charset=UTF-8",
    `Content-Type` = "text/plain; charset=UTF-8",
    `x-ms-date` = format(Sys.time(), format = "%a, %d %b %Y %H:%M:%S %Z"),
    # `x-ms-version` = "2014-02-14"
    `x-ms-version` = "2015-02-21"
    # `x-ms-client-request-id` = "NA"
  )
  if(!missing("additional") && !is.null(additional)){
    c(main, additional)
  } else {
    main
  }
}



# Construct canonical headers for the Azure API.
# 
# @param x headers
# @keywords Internal
# @return character vector of length 1
# @references https://msdn.microsoft.com/en-us/library/azure/dd179428.aspx
canonicalizedHeaders <- function(x){
  y <- x[grep("^x-ms-", names(x))]
  # browser()
  names(y) <- tolower(names(y))
  z <- y[order(names(y))]
  paste(sprintf("%s:%s\n", names(z), unname(z)), collapse = "", sep = "")
}



# Get Container Metadata
# GET http://myaccount.blob.core.windows.net/mycontainer?restype=container&comp=metadata 
# CanonicalizedResource:
#   /myaccount/mycontainer\ncomp:metadata\nrestype:container
canonicalizedResource <- function(uri, account = gsub(".*//(.*?)\\..*", "\\1", uri)){
  spl <- strsplit(uri, split = "\\?")[[1]]
  ptn <- "(http|https)://.*?\\.(.*)"
  encodedUri <- gsub(ptn, "\\2", spl[1])
  
  if(length(spl) == 2){
    parms <- spl[2]
    parms <- gsub("=", ":", parms)
    splitParms <- strsplit(parms, split = "&")[[1]]
    combineParms <- function(x){
      if(length(x) == 1) {
        x
      } else {
        m <- matrix(unlist(strsplit(x, ":")), ncol = 2, byrow = TRUE)
        r <- split(m[, 2], m[, 1])
        zz <- sapply(r, paste, collapse = ",")
        paste(names(zz), unname(zz), sep=":", collapse = "\n")
      }
    }
    combinedParms <- combineParms(sort(tolower(splitParms)))
    resource <- gsub("\\?.*", "", encodedUri)
    resource <- gsub(".*?/", "", resource)
  } else {
    combinedParms <- ""
    resource <- gsub(".*?/(.*)", "\\1", encodedUri)
  }
  # browser()
  accountResource <- paste(account, resource, sep = "/")
  accountResource <- gsub("/$", "", accountResource)
  
  if(combinedParms != "")
    sprintf("/%s/\n%s", accountResource, combinedParms)
  else
    sprintf("/%s", accountResource)
}



#' @importFrom digest hmac
#' @importFrom base64enc base64encode base64decode
azure.encode <- function(x, key){
  z <- iconv(x, "ASCII", to = "UTF-8")
  base64encode(
    hmac(base64decode(key), z, algo = "sha256", raw = TRUE)
  )
}



.orEmpty <- function(x){
  if(!is.null(x)) x else ""
}

azure.signature <- function(sharedKey,
                            uri,
                            `Content-Encoding` = NULL,
                            `Content-Language` = NULL,
                            `Content-Length` = .orEmpty(uri$headers["Content-Length"]),
                            `Content-MD5` = NULL,
                            # `Content-Type` = "application/octet-stream Charset=UTF-8",
                            `Content-Type` = "text/plain; charset=UTF-8",
                            `Date` = NULL,
                            `If-Modified-Since` = NULL,
                            `If-Match` = NULL,
                            `If-None-Match` = NULL,
                            `If-Unmodified-Since` = NULL,
                            `Range` = NULL,
                            #                             `CanonicalizedHeaders` = NULL,
                            #                             `CanonicalizedResource` = NULL,
                            encode = TRUE
){
  VERB <- uri$verb
  CanonicalizedHeaders <- canonicalizedHeaders(azHeaders(uri$headers))
  # browser()
  CanonicalizedResource = canonicalizedResource(uri$uri)
  stringToSign <- paste0(VERB, "\n",
                         `Content-Encoding`, "\n",
                         `Content-Language`, "\n",
                         `Content-Length`, "\n",
                         `Content-MD5`, "\n",
                         `Content-Type`, "\n",
                         `Date`, "\n",
                         `If-Modified-Since`, "\n",
                         `If-Match`, "\n",
                         `If-None-Match`, "\n",
                         `If-Unmodified-Since`, "\n",
                         `Range`, "\n",
                         CanonicalizedHeaders,
                         CanonicalizedResource
  )
  if(encode)
    azure.encode(x = stringToSign, key = sharedKey)
  else
    stringToSign
}


#' @importFrom xml2 read_xml xml_find_all xml_contents
azParseContent <- function(x, xpath){
  x <- rawToChar(req$content)
  Encoding(x) <- "UTF-8"
  xml <- xml2::read_xml(x)
  if(!missing("xpath") && !is.null(xpath)){
    z <- xml_find_all(xml, xpath)
    as.character(xml_contents(z))
  } else
    xml
}


