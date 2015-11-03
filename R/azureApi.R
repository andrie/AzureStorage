#' @export
azListContainers <- function(account){
  uri <- "https://myaccount.blob.core.windows.net/?comp=list"
  verb <- "GET"
  uri <- gsub("myaccount", account, uri)
  list(uri = uri,
       account = account,
       verb = verb,
       canonicalizedResource = canonicalizedResource(uri, account)
  )
}

#' The Get Blob operation reads or downloads a blob from the system, including its metadata and properties. You can also call Get Blob to read a snapshot.
#' 
#' @references https://msdn.microsoft.com/en-us/library/azure/dd179440.aspx
#' @export
azGetBlob <- function(account){
  uri <- "https://myaccount.blob.core.windows.net/mycontainer/myblob"
  verb <- "GET"
  uri <- gsub("myaccount", account, uri)
  list(uri = uri,
       account = account,
       verb = verb,
       headers = NULL,
       canonicalizedResource = canonicalizedResource(uri)
  )
}


#' The Put Blob operation creates a new block, page, or append blob, or updates the content of an existing block blob.
#' 
#' @references https://msdn.microsoft.com/en-us/library/azure/dd179451.aspx
azPutBlob <- function(account, container, blob, value){
  uri <- "https://myaccount.blob.core.windows.net/mycontainer/myblob"
  verb <- "PUT"
  uri <- gsub("myaccount", account, uri)
  uri <- gsub("mycontainer", container, uri)
  uri <- gsub("myblob", blob, uri)
  # browser()
  body <- value
  contentLength <- nchar(body)
  list(uri = uri,
       account = account,
       verb = verb,
       headers = c(
         `Content-Length` = contentLength, # Required. The length of the request.
         `x-ms-blob-type` = "BlockBlob",     # Optional. Set the blob’s content type.
         `x-ms-blob-content-disposition` = "attachment; filename=\"fname.ext\""
#          `x-ms-blob-content-type` = "",     # Optional. Set the blob’s content type.
#          `x-ms-blob-content-encoding` = "", # Optional. Set the blob’s content encoding.
#          `x-ms-blob-content-language` = "", # Optional. Set the blob's content language.
#          `x-ms-blob-content-md5` = "",      # Optional. Set the blob’s MD5 hash.
#          `x-ms-blob-cache-control` = ""     # Optional. Sets the blob's cache control.
       ),
       canonicalizedResource = canonicalizedResource(uri),
       body = body # blob goes in request body
  )
}

#' Send the request to Azure.
#' 
#' @importFrom httr GET PUT DELETE add_headers verbose
azSendRequest <- function(uri, key, verbose = TRUE){
  if(verbose) message(azure.signature(key, uri, encode = FALSE))
  Signature <- azure.signature(key, uri, encode = TRUE)
  headers <- azHeaders(c(
               uri$headers,
               c(`Authorization`= sprintf("SharedKey %s:%s", AccountName, Signature)))
  )
  # verbosity <- if(verbose) httr::verbose(TRUE) else httr::verbose(FALSE)
  verbosity <- if(verbose) httr::verbose(TRUE) else NULL
  # browser()
  switch(uri$verb, 
         GET = GET(uri$uri, add_headers(headers), verbosity),
         PUT = PUT(uri$uri, add_headers(headers), verbosity, body = uri$body, httr::content_type("text/plain; charset=UTF-8"))
  )
}
