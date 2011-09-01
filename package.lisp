(defpackage #:machina-policy
  (:use #:cl)
  (:import-from #:alexandria #:rcurry)
  (:import-from #:cl-ppcre
                #:scan
                #:register-groups-bind
                #:split
                #:create-scanner)
  (:import-from #:puri
                #:uri
                #:render-uri
                #:merge-uris
                #:enough-uri
                #:uri-scheme
                #:uri-host
                #:uri-port)
  (:export ;; Parsing and query the robots.txt file as a particular user-agent
           #:parse-robots.txt
           #:agent-policy
           #:uri-allowed-p

           ;; Additional info
           #:crawl-delay))
