;; gh-to-sourcehut.lisp
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

(in-package :gh-to-sourcehut)

(defparameter *config-path*
  (asdf:system-relative-pathname :gh-to-sourcehut ".config"))

(defun load-config ()
  (with-input-from-file (ins *config-path*)
    (st-json:read-json ins)))

(defparameter *config* (load-config))

(defparameter *gh-cache* (make-hash-table :test 'equal :size 10))

(defun clear-gh-cache ()
  (setf *gh-cache* (make-hash-table :test 'equal :size 10)))

(defun get-gh-cache (url)
  (gethash url *gh-cache* nil))

(defun add-gh-cache (url data)
  (setf (gethash url *gh-cache*) data))

(defun gh-get (url &rest args)
  (let ((url (concatenate 'string
                          "https://api.github.com"
                          (apply #'format nil url args))))
    (cond ((get-gh-cache url)
           (format t "Using cached ~a~%" url)
           (values (get-gh-cache url) 200))
          (t
           (format t "Fetching ~a~%" url)
           (multiple-value-bind (body response) (dex:get url
                                                         :basic-auth
                                                         (cons (getjso "gh_username" *config*)
                                                               (getjso "gh_token" *config*)))
             (when (/= 200 response)
               (error "Got non-200 response ~a" response))
             (let ((json (read-json-from-string body)))
               (add-gh-cache url json)
               (values json response)))))))

(defun get-all-user-gh-repos (username)
  (loop
    :with per-page = 50
    :for page :from 1
    :for next-page = (gh-get
                      "/users/~a/repos?sort=updated&direction=DECs&per_page=~a&page=~a"
                      username per-page page)
    :for page-count = (length next-page)
    :while (> page-count 0)
    :appending next-page :into results
    :until (< page-count per-page)
    :finally (return results)))

(defun clone-or-pull-git-repo (base-directory repo)
  (let* ((repo-directory (format nil "~a~a" base-directory (getjso "name" repo)))
         (clone-cmd (format nil "git clone ~a ~a"
                            (getjso "ssh_url" repo)
                            repo-directory))
         (pull-cmd (format nil "git pull ~a"
                           (getjso "ssh_url" repo))))
    ;;(format t "Running: ~a~%" cmd)
    (cond
      ((uiop:directory-exists-p repo-directory)
       ;; Pull if directory exists
       (multiple-value-bind (output error-output result)
           (uiop:with-current-directory (repo-directory)
             (uiop:run-program pull-cmd :output nil :error-output nil))
         (declare (ignore output error-output))
         (list repo clone-cmd (= result 0))))
      (t
       ;; Clone if it doesn't
       (multiple-value-bind (output error-output result)
           (uiop:run-program clone-cmd :output nil :error-output nil)
         (declare (ignore output error-output))
         (list repo clone-cmd (= result 0)))))))

(defun clone-user-gh-repos (username base-directory &key (thread-count 8))
  (let ((repos (get-all-user-gh-repos username)))
    (ensure-directories-exist base-directory)
    (let ((clone-results (lparallel.kernel-util:with-temp-kernel (thread-count :name "git-clone")
                           (lparallel:pmapcar
                            (alexandria:curry #'clone-git-repo base-directory)
                            repos))))
      (loop
        :for result :in clone-results
        :when (null (caddr result))
          :do
             (format t
                     "~a failed to clone!:~%    ~a~%"
                     (getjso "name" (car result))
                     (cadr result))))))
