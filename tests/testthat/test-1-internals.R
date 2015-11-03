if(interactive()) library("testthat")

context("canonical resource")

test_that("canonicalizedResource", {
  expect_identical(
    canonicalizedResource("https://apdevries.blob.core.windows.net/?comp=list"),
    "/apdevries\ncomp:list"
  )
  
  expect_identical(
    canonicalizedResource("http://myaccount.blob.core.windows.net/mycontainer?restype=container&comp=metadata"),
    "/myaccount/mycontainer\ncomp:metadata\nrestype:container"
  )
  
  
  expect_identical(
    canonicalizedResource("http://myaccount.blob.core.windows.net/mycontainer?restype=container&comp=list&include=snapshots&include=metadata&include=uncommittedblobs"),
    "/myaccount/mycontainer\ncomp:list\ninclude:metadata,snapshots,uncommittedblobs\nrestype:container"
  )
  
  expect_identical(
    canonicalizedResource("https://myaccount-secondary.blob.core.windows.net/mycontainer/myblob"),
    "/myaccount/mycontainer/myblob"
  )   
  
  expect_identical(
    canonicalizedResource("https://myaccount.blob.core.windows.net/mycontainer/myblob"),
    "/myaccount/mycontainer/myblob"
  )
})

context("canonical header")


test_that("canonicalizedHeader", {
  uri <- azGetBlob("myaccount")
  
  # exp <- "GET\n\n\n\n\n\n\n\n\n\n\n\nx-ms-date:Sun, 11 Oct 2009 21:49:13 GMT\nx-ms-version:2009-09-19\n/myaccount/mycontainer\ncomp:metadata\nrestype:container\n"
  exp <- "GET\n\n\n\n\n\n\n\n\n\n\n\nx-ms-date:Sun, 11 Oct 2009 21:49:13 GMT\nx-ms-version:2009-09-19\n/myaccount/mycontainer/myblob"
  
  ret <- azure.signature(primaryKey, uri, encode = FALSE)
  expect_identical(nchar(ret), nchar(exp))
})
  