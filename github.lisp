;; github.lisp
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

(defun github-get (url &rest args)
  "Make an authenticated GET request to the GitHub API."
  (let ((url (concatenate 'string
                          "https://api.github.com"
                          (apply #'format nil url args))))
    (format t "Fetching ~a~%" url)
    (multiple-value-bind (body response)
        (handler-bind ((dex:http-request-failed 'dex:ignore-and-continue))
          (dex:get url
                   :basic-auth
                   (cons (getjso "gh_username" *config*)
                         (getjso "gh_token" *config*))))
      (when (/= 200 response)
        (error "Got non-200 response ~a" response))
      (list url (read-json-from-string body) response))))

(defun all-user-github-repos (username)
  "Return a list of all of a user's GitHub repos."
  (loop
    :with per-page = 50
    :for page :from 1
    :for next-page = (cmd-output (github-get
                            "/users/~a/repos?sort=updated&direction=DECs&per_page=~a&page=~a"
                            username per-page page))
    :for page-count = (length next-page)
    :while (> page-count 0)
    :appending next-page :into results
    :until (< page-count per-page)
    :finally (return results)))
