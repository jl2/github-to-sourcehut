;; common.lisp
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

(defparameter *show-git-output* nil)

(defparameter *config-path*
  (asdf:system-relative-pathname :github-to-sourcehut ".config")
  "Path to a .config JSON file with a single object with
'sh_username', 'sh_token', 'gh_token', and 'gh_username' entries.")

(defun load-config ()
  "Read a JSON config file."
  (with-input-from-file (ins *config-path*)
    (st-json:read-json ins)))

(defparameter *config* (load-config)
  "The global github-to-sourcehut configuration.")

(defun repo-directory (base-directory repo)
  "Return the name of the local directory where repo will be cloned."
  (if-let (rd (getjso "repo-directory" repo))
    rd
    (setf (getjso "repo-directory" repo)
          (format nil "~a~a/" base-directory (getjso "name" repo)))))

(defun github-repo-url (repo)
  "Return the GitHub repo URL (the ssh_url) for the repo."
  (getjso "ssh_url" repo))

(defun sourcehut-repo-url (repo)
  "Compute the name of the SourceHut repo URL and cache it in repo."
  (if-let (rd (getjso "sourcehut_url" repo))
    rd
    (setf (getjso "sourcehut_url" repo)
          (format nil "git@git.sr.ht:~~~a/~a"
                  (getjso "sh_username" *config*)
                  (getjso "name" repo)))))

;; Git, github API, and sourcehut API function calls return 3 element lists
;; of the form (command output result-code)
;; These functions return the elements of those lists by name
(deftype command-list () 'list)

(defun cmd-command (result)
  "Return the command of a cmd-list"
  (car result))

(defun cmd-output (result)
  "Return the command of a cmd-list"
  (cadr result))

(defun cmd-result (result)
  "Return the command of a cmd-list"
  (caddr result))

(defun run-git (cmd &rest args)
  "Run a Git command with arguments."
  (let ((git-cmd (format nil "git ~a ~{~a~^ ~}" cmd args)))
    (when *show-git-output*
      (format *show-git-output* "Running ~a~%" git-cmd))
    (multiple-value-bind (output error-output result)
        (uiop:run-program git-cmd :output *show-git-output*
                                  :error-output *show-git-output*
                                  :ignore-error-status t)
      (declare (ignore output error-output))
      (list git-cmd nil result))))


(defun clone-github-repo (base-directory repo)
  "Clone a GitHub repo into base-directory."
  (let ((repo-directory (repo-directory base-directory repo)))
    (when (uiop:directory-exists-p repo-directory)
      (error "Directory ~a already exists!" repo-directory))
    (run-git "clone -q" (github-repo-url repo) repo-directory)))

(defun pull-repo (base-directory repo remote)
  "Pull the specified remote of the given repo in base-directory."
  (run-git "-C" (repo-directory base-directory repo)
           "pull -q" remote))

(defun add-git-remote (base-directory repo remote-name remote-url)
  "Add a remote to the specified repo in base-directory."
  (run-git "-C" (repo-directory base-directory repo)
           "remote add" remote-name remote-url))

(defun push-repo (base-directory repo remote-name)
  "Push the repo in base-directory to the specified remote."
  (run-git "-C" (repo-directory base-directory repo) "push --all" remote-name))
