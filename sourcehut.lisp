;; sourcehut.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :github-to-sourcehut)

(defun sourcehut-get (type url &rest args)
  "Make an authenticated GET request to the SourceHut API."
  (let ((url (concatenate 'string
                          "https://"
                          type
                          ".sr.ht/api/"
                          (apply #'format nil url args))))
    (multiple-value-bind (body response)
        (handler-bind ((dex:http-request-failed 'dex:ignore-and-continue))
          (dex:get url
                 :headers (list (cons "Authorization"
                                      (concatenate 'string
                                                   "token "
                                                   (getjso "sh_token" *config*))))))
      (list url (read-json-from-string body) response))))

(defun sourcehut-post (type data url &rest args)
  "Make an authenticated POST request to the SourceHut API."
  (let ((url (concatenate 'string
                          "https://"
                          type
                          ".sr.ht/api/"
                          (apply #'format nil url args))))
    (multiple-value-bind (body response)
        (handler-bind ((dex:http-request-failed 'dex:ignore-and-continue))
          (dex:post url
                    :content (st-json:write-json-to-string data)
                    :headers (list (cons "content-type"
                                         "application/json")
                                   (cons "Authorization"
                                         (concatenate 'string
                                                      "token "
                                                      (getjso "sh_token" *config*))))))
      (list url (read-json-from-string body) response))))

(defun sourcehut-put (type data url &rest args)
  "Make an authenticated PUT request to the SourceHut API."
  (let ((url (concatenate 'string
                          "https://"
                          type
                          ".sr.ht/api/"
                          (apply #'format nil url args))))
    (multiple-value-bind (body response)
        (handler-bind ((dex:http-request-failed 'dex:ignore-and-continue))
          (dex:put url
                   :content (st-json:write-json-to-string data)
                   :headers (list (cons "content-type"
                                         "application/json")
                                   (cons "Authorization"
                                         (concatenate 'string
                                                      "token "
                                                      (getjso "sh_token" *config*))))))
      (list url (read-json-from-string body) response))))

(defun sourcehut-delete (type url &rest args)
  "Make an authenticated DELETE request to the SourceHut API."
  (let ((url (concatenate 'string
                          "https://"
                          type
                          ".sr.ht/api/"
                          (apply #'format nil url args))))

    (multiple-value-bind (body response)
        (handler-bind ((dex:http-request-failed 'dex:ignore-and-continue))
          (dex:delete url
                      :headers (list (cons "Authorization"
                                           (concatenate 'string
                                                        "token "
                                                        (getjso "sh_token" *config*))))))
      (declare (ignorable body))
      (list url nil response))))

(defun sourcehut-repo-exists-p (repo)
  "Return t if the repo exists on SourceHut."
  (= 200 (caddr (sourcehut-get "git" "repos/~a" (getjso "name" repo)))))

(defun delete-all-srht-repos ()
  "Delete all of the users repos on SourceHut.
This is a very dangerous function..."
  (lparallel.kernel-util:with-temp-kernel (8 :name "git-clone")
    (loop
      :for response = (cadr (sourcehut-get "git" "repos"))
      :while (> (length (getjso "results" response)) 0)
      :do
         (lparallel:pmapcar
          (lambda (repo)
            (sourcehut-delete "git" "repos/~a" (getjso "name" repo)))
          (getjso "results" response)))))

(defun create-sourcehut-repo (repo &key description (visibility "public"))
  "Create a new repo on sourcehut."
  (let ((content-body
          (st-json:jso-from-alist
           (if description
               (list
                (cons "name" (getjso "name" repo))
                (cons "description" description)
                (cons "visibility" visibility))
               (list
                (cons "name" (getjso "name" repo))
                (cons "visibility" visibility))))))
    (sourcehut-post "git"
             content-body
             "repos/")))
