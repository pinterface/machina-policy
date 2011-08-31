;;;; This file implements a parser for the robots.txt file syntax.  It doesn't
;;;; use anything fancy like esrap to avoid being really slow.  Also, to be more
;;;; forgiving to the files likely to exist in the wild, which almost certainly
;;;; won't conform to the BNF in the spec[1].
;;;; 
;;;; [1] http://www.robotstxt.org/norobots-rfc.txt

(in-package #:robots.txt)

(defgeneric parse (thing)
  (:method ((string string)) (with-input-from-string (stream string) (parse-stream stream)))
  (:method ((path pathname)) (with-open-file (stream path :direction :input) (parse-stream stream)))
  (:method ((stream stream)) (parse-stream stream)))

(defun parse-stream (stream)
  (loop :for line = (read-line stream nil nil)
        :while line
        :when (starts-record-p line)
          :collect (parse-record line stream)))

(defun comment-line-p (line)
  (scan "^[ 	]*#" line))

(defun ignorable-p (line)
  (or (scan "^[ 	]*$" line)
      (comment-line-p line)))

(defun starts-record-p (line)
  (scan "^[ 	]*(?i:user-agent)[ 	]*:" line))

(defun user-agent (line)
  (register-groups-bind (agent)
      ("^[ 	]*(?i:user-agent)[ 	]*:[ 	]*([^#]+?)[ 	]*(?:#.+?)?$" line)
    agent))

(defun parse-record (first-line stream)
  (let* ((agents (list (user-agent first-line)))
         (in-controls nil)
         (controls nil))
    (loop :for line = (read-line stream nil nil)
          :while line
          :do (cond
                ((comment-line-p line) nil)
                ((ignorable-p line) (return))
                ((not (starts-record-p line))
                 (setf in-controls t)
                 (push (parse-acl-line line) controls))
                ((and (not in-controls)
                      (starts-record-p line))
                 (push (user-agent line) agents))
                ((starts-record-p line)
                 (cerror "Ignore line" "User-agent specification ~s out of order" line))
                (t (cerror "Ignore line" "Unable to parse line ~s" line))))
    (cons (nreverse agents) (nreverse controls))))

(defun starts-acl-p (line)
  (and (not (starts-record-p line))
       (scan "^[ 	]*(?i:disallow|allow|crawl-delay|[a-z]+)[ 	]*:" line)))

(defun parse-acl-line (line)
  (register-groups-bind (key value)
      ("^[ 	]*((?i)[a-z-]+)[ 	]*:[ 	]*([^#]*?)[ 	]*(?:#.+?)?$" line)
    (cond
      ((string-equal "disallow"    key) (cons :disallow    (parse-path value)))
      ((string-equal "allow"       key) (cons :allow       (parse-path value)))
      ((string-equal "crawl-delay" key) (cons :crawl-delay (parse-integer value)))
      (t (cons key value)))))

(defun parse-path (path)
  (cond
    ((string= path "") nil)
    ((wild-uri-p path)
     (make-instance 'wild-uri :raw path))
    (t (puri:parse-uri path))))


;; blankcomment = 1*(blank | commentline)
;; commentblank = *commentline blank *(blankcomment)
;; blank        = *space CRLF
;; CRLF         = CR LF
;; record       = *commentline agentline *(commentline | agentline)
;;                1*ruleline *(commentline | ruleline)
;; agentline    = "User-agent:" *space agent  [comment] CRLF
;; ruleline     = (disallowline | allowline | extension)
;; disallowline = "Disallow" ":" *space path [comment] CRLF
;; allowline    = "Allow" ":" *space rpath [comment] CRLF
;; extension    = token : *space value [comment] CRLF
;; value        = <any CHAR except CR or LF or "#">
;; commentline  = comment CRLF
;; comment      = *blank "#" anychar
;; space        = 1*(SP | HT)
;; rpath        = "/" path
;; agent        = token
;; anychar      = <any CHAR except CR or LF>
;; CHAR         = <any US-ASCII character (octets 0 - 127)>
;; CTL          = <any US-ASCII control character
;;                     (octets 0 - 31) and DEL (127)>
;; CR           = <US-ASCII CR, carriage return (13)>
;; LF           = <US-ASCII LF, linefeed (10)>
;; SP           = <US-ASCII SP, space (32)>
;; HT           = <US-ASCII HT, horizontal-tab (9)>
;; token        = 1*<any CHAR except CTLs or tspecials>
;; tspecials    = "(" | ")" | "<" | ">" | "@"
;;              | "," | ";" | ":" | "\" | <">
;;              | "/" | "[" | "]" | "?" | "="
;;              | "{" | "}" | SP | HT
;; path        = fsegment *( "/" segment )
;; fsegment    = 1*pchar
;; segment     =  *pchar
;; pchar       = uchar | ":" | "@" | "&" | "="
;; uchar       = unreserved | escape
;; unreserved  = alpha | digit | safe | extra
;; escape      = "%" hex hex
;; hex         = digit | "A" | "B" | "C" | "D" | "E" | "F" |
;;                      "a" | "b" | "c" | "d" | "e" | "f"
;; alpha       = lowalpha | hialpha
;; lowalpha    = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" |
;;               "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" |
;;               "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
;; hialpha     = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" |
;;               "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" |
;;               "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"
;; digit       = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" |
;;               "8" | "9"
;; safe        = "$" | "-" | "_" | "." | "+"
;; extra       = "!" | "*" | "'" | "(" | ")" | ","

#+(or)
(parse
"
# /robots.txt for http://www.fict.org/
# comments to webmaster@fict.org

User-agent: unhipbot
Disallow: /

User-agent: commentedbot # a comment!
Disallow: /comments  # mo' commentage

User-agent: webcrawler
User-agent: excite
Disallow: 

User-agent: *
Disallow: /org/plans.html
Allow: /org/
Allow: /serv
Allow: /~mak
Disallow: /
crawl-delay: 5

User-agent: wildcards
# a wild comment appears!
Disallow: /*?
Disallow: /*.bak$
")
