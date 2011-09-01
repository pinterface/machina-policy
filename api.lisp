(in-package #:machina-policy)

(defclass robots.txt ()
  ((default-policy :accessor %default-policy :initarg :default-rules)
   (policies :accessor %policies :initarg :rules :documentation "bot-name => policy pairs")))

(defgeneric parse-robots.txt (input &optional agent)
  (:documentation "Parses a given ROBOTS.TXT.  If the optional AGENT is
  specified, ignores anything not applicable to AGENT.")
  (:method ((input string) &optional agent)
    (with-input-from-string (stream input)
      (parse-robots.txt stream agent)))
  (:method ((input stream) &optional agent)
    (let ((rules (parse input)))
      (flet ((find-rules-for (agent)
               (find agent rules :key #'car :test (rcurry #'member :test #'string-equal)))
             (make-policy (rules)
               (cons (car rules)
                     (let ((rules (rest rules)))
                       (when rules
                         (make-instance 'policy
                                        :crawl-delay (cdr (assoc :crawl-delay rules))
                                        :rules (remove :crawl-delay rules :key #'car))))))
             (split-policy (rules)
               "Given ((<agents>+) . <policy>), returns ((<agent> . <policy>)+)."
               (mapcar (rcurry #'cons (cdr rules)) (car rules))))
        (make-instance 'robots.txt
                       :default-rules (cdr (make-policy (find-rules-for "*")))
                       :rules (cond
                                (agent (and (find-rules-for agent)
                                            (list (cons agent (cdr (make-policy (find-rules-for agent)))))))
                                (t (mapcan #'split-policy (mapcar #'make-policy rules)))))))))

#+(or)
(agent-policy "user-agent: bar
user-agent: bat
allow: /foo/bar
disallow: /foo
crawl-delay: 1

user-agent: *
disallow: 

user-agent: some-other
"
              "bar")

(defgeneric agent-policy (robots.txt agent)
  (:documentation "Returns the policy applicable to AGENT as specified by the
  given ROBOTS.TXT.  As a second value, returns T if the policy applies to the
  given agent specifically, or NIL if the policy returned was the wildcard
  policy (*).")
  (:method ((robots.txt pathname) agent)
    (agent-policy (parse-robots.txt robots.txt agent) agent))
  (:method ((robots.txt string) agent)
    (agent-policy (parse-robots.txt robots.txt agent) agent))
  (:method ((robots.txt robots.txt) agent)
    (let ((policy (assoc agent (%policies robots.txt) :test #'string-equal)))
      (cond
        (policy (values (cdr policy) t))
        (t (values (%default-policy robots.txt) nil))))))

(defclass policy ()
  ((crawl-delay :initarg :crawl-delay :reader crawl-delay)
   (access-rules :initarg :rules)))

(defgeneric uri-allowed-p (policy uri)
  (:documentation "General boolean indicating whether URI is allowed access
  under the given POLICY.  Defaults to T.")
  (:method ((policy null) uri)
    t)
  (:method ((policy policy) (uri string))
    (uri-allowed-p policy (uri uri)))
  (:method ((policy policy) (uri uri))
    (loop :for (what . where) :in (slot-value policy 'access-rules)
      :do (case what
            (:disallow (when (uri-match where uri) (return nil)))
            (:allow    (when (uri-match where uri) (return t))))
      :finally (return t))))

(defun host-relative-uri (uri)
  (enough-uri uri (make-instance 'uri
                                 :scheme (uri-scheme uri)
                                 :host (uri-host uri)
                                 :port (uri-port uri))))
(defun host-relative-uri-string (uri)
  (render-uri (host-relative-uri uri) nil))

(defgeneric uri-match (uri-1 uri-2)
  (:method ((uri-1 null) uri-2)
    nil)
  (:method ((uri-1 string) (uri-2 string))
    (= (length uri-1) (or (mismatch uri-1 uri-2) (length uri-1))))
  (:method (uri-1 (uri-2 string))
    (uri-match uri-1 (uri uri-2)))
  (:method ((uri-1 uri) (uri-2 uri))
    (uri-match (host-relative-uri-string uri-1) (host-relative-uri-string uri-2)))
  (:method ((uri-1 wild-uri) (uri-2 uri))
    (scan (slot-value uri-1 'scanner) (host-relative-uri-string uri-2))))

#+(or) (uri-match (uri "/ad/") (uri "http://kepibu.org/ad/"))
#+(or) (uri-match (make-instance 'wild-uri :raw "/foo/baz$") "/foo/baz")

#+(or)
(let* ((robots.txt (parse-robots.txt (drakma:http-request "http://web.kepibu.org/robots.txt")))
       (policy (agent-policy robots.txt "*")))
  (uri-allowed-p policy "http://web.kepibu.org/ham"))
