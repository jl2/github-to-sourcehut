;; github-to-sourcehut.lisp
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


(defun mirror-github-repo-to-sourcehut (base-directory repo)
  "Mirror a repo cloned from GitHub and push it to SourceHut."
  (let ((command-results nil))
    ;; Clone repo into base-directory if the repo doesn't exist there
    ;; Otherwise pull the existing clone
    (if (not (uiop:directory-exists-p (repo-directory base-directory repo)))
        (push (clone-github-repo base-directory repo)
              command-results)
        (push (pull-repo base-directory repo "origin")
              command-results))

    ;; Create the repo on SourceHut if it doesn't exist.
    (when (not (sourcehut-repo-exists-p repo))
      (push (create-sourcehut-repo repo
                                   :visibility (if (eq :true (getjso "private" repo))
                                                   "private"
                                                   "public"))
            command-results))

    ;; It seems that sometimes the create doesn't take effect immediately, so
    ;; wait up to 5 seconds
    (j-utils:timed-loop 5.0 0.1
                        (lambda () (sourcehut-repo-exists-p repo))
                        :err-on-timeout nil)

    ;; If it still doesn't exist, error out
    (when (not (sourcehut-repo-exists-p repo))
      (error "repo ~a still does not exist!" repo))

    ;; Add the srht-remote if it doesn't exist
    (push (add-git-remote base-directory repo "srht-remote" (sourcehut-repo-url repo))
          command-results)

    ;; There must be a bug somewhere, but this push fails with error 128 sometimes.
    ;; I haven't figured out where the error is yet, but trying again usually works.
    (loop
      :for tries :below 5
      :for push-result = (push-repo base-directory repo "srht-remote")
      :do
         (push push-result command-results)
         (when (/= 0 (cmd-result push-result))
           (format t "git push try ~a failed, trying again in 0.5 seconds.~%" tries)
           (sleep 0.5))
      :until (= (cmd-result push-result) 0))
    command-results))

(defun mirror-github-repos-on-sourcehut (base-directory &key (thread-count 8)
                                                                   (show-results nil)
                                                                   (show-failures nil))
  "Clone all of a user's GitHub repos into base-directory, and then push to SourceHut.
Uses JSON .config file in the package directory.
Uses the gh_username, gh_token, sh_username, and sh_token for API and Git calls."
  (let ((repos (all-user-github-repos (getjso "gh_username" *config*))))
    (ensure-directories-exist base-directory)
    (let ((mirror-results
            (lparallel.kernel-util:with-temp-kernel (thread-count :name "git-clone")
              (lparallel:pmapcar
               (alexandria:curry #'mirror-github-repo-to-sourcehut base-directory)
               repos))))

      ;; Show the output if requested
      (when (or show-results show-failures)
        (loop
          :for results :in mirror-results
          :do
             (loop
               :for (cmd output result error) :in (reverse results)
               :when (or show-results
                         (and show-failures
                              (or
                               ;; run-program errors
                               (and (/= result 0)
                                    (< result 150))
                               ;; HTTP errors
                               (>= 400 result))))
                 :do
                    (format t
                            "(~a) ~a~%"
                            result cmd)))))))
