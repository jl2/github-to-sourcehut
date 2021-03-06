;; package.lisp
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

(in-package :cl-user)
(defpackage :github-to-sourcehut.test
  (:use :cl
        :fiveam
        :alexandria
        :github-to-sourcehut))

(in-package :github-to-sourcehut.test)

(def-suite :github-to-sourcehut)
(in-suite :github-to-sourcehut)

(test config
      (let ((config (load-config)))
        (is-true (not (null (st-json:getjso "gh_username" config))))
        (is-true (not (null (st-json:getjso "gh_token" config))))
        (is-true (not (null (st-json:getjso "sh_username" config))))
        (is-true (not (null (st-json:getjso "sh_token" config))))))
