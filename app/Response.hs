module Response where

import Network.HTTP.Server

headers message contentType =
  [ Header HdrContentLength (show $ length message)
  , Header HdrContentType $ contentType ++ "; charset=utf-8"
  ]

responseWith contentType body =
  Response { rspCode = (2,0,0)
           , rspBody = body
           , rspHeaders = headers body contentType
           , rspReason = "OK"
           }

notFound message =
  Response { rspCode = (4,0,4)
           , rspBody = message
           , rspHeaders = headers message "text/plain"
           , rspReason = "Not found."
           }

badRequest message =
  Response { rspCode = (4,0,0)
           , rspBody = message
           , rspHeaders = headers message "text/plain"
           , rspReason = "Bad request."
           }
