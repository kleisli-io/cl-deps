(defpackage lack/middleware/cors
  (:nicknames :lack.middleware.cors)
  (:use :cl)
  (:export :*lack-middleware-cors*))
(in-package :lack/middleware/cors)

;;; Minimal CORS middleware — vendored because Lack ships none.
;;;
;;; Two responsibilities:
;;;   1. OPTIONS preflight: short-circuit with 204 and the four CORS
;;;      response headers (Access-Control-Allow-Origin / -Methods /
;;;      -Headers, Access-Control-Max-Age).
;;;   2. Other methods: run the wrapped app, then append
;;;      Access-Control-Allow-Origin and Vary: Origin to the response so
;;;      browsers will accept cross-origin reads.
;;;
;;; Defaults are tuned for an open public docs corpus + an MCP transport:
;;;   - Origin "*"      — fully open; the corpus is read-only public.
;;;   - Methods         — covers GET/HEAD/POST/DELETE/OPTIONS.
;;;   - Headers         — Content-Type + Accept (negotiation) +
;;;                       Mcp-Session-Id (mcp-http transport).
;;;   - Max-Age 86400   — 24h preflight cache reduces OPTIONS load.

(defparameter *lack-middleware-cors*
  (lambda (app &key
                 (origin "*")
                 (methods "GET, HEAD, POST, DELETE, OPTIONS")
                 (headers "Content-Type, Accept, Mcp-Session-Id")
                 (max-age "86400"))
    (lambda (env)
      (cond
        ((eq (getf env :request-method) :options)
         (list 204
               (list :access-control-allow-origin origin
                     :access-control-allow-methods methods
                     :access-control-allow-headers headers
                     :access-control-max-age max-age)
               nil))
        (t
         (let ((response (funcall app env)))
           (cond
             ((and (consp response) (>= (length response) 2))
              (list (first response)
                    (append (second response)
                            (list :access-control-allow-origin origin
                                  :vary "Origin"))
                    (if (>= (length response) 3) (third response) nil)))
             (t response)))))))
  "Vendored Lack-style CORS middleware. Short-circuits OPTIONS preflight
   with 204 + CORS headers; appends Access-Control-Allow-Origin + Vary:
   Origin to all other responses.")
