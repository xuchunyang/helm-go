;;; helm-go.el --- Helm Go

;; Copyright (C) 2015 by Chunyang Xu <xuchunyang56@gmail.com>

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://github.com/xuchunyang/helm-go
;; Version: 0.0.1
;; Package-Requires: ((helm "1.0"))

;;; Commentary:

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Change Log:
;; 0.0.1   - 2014/04/03 - Created File.

;;; Code:

(require 'helm)

(defvar helm-go-apps-list nil)
(defvar helm-go-apps-list-refresh-flag nil)

(defun helm-go--collect-apps ()
  "Collect installed apps.

Currently, only Mac OS X is supported."
  (with-temp-buffer
    (let ((ret (call-process "mdfind" nil t nil
                             "kMDItemKind=Application")))
      (unless (zerop ret)
        (error "Failed: mdfind"))
      (setq helm-go-apps-list (split-string (buffer-string) "\n")))))

(defun helm-go--open-app (candidate)
  "Open app whose PATH is CANDIDATE.

Currently, only Mac OS X is supported."
  (call-process "open" nil 0 nil
                "-a" candidate))

(defvar helm-go--app-source
  '((name . "Open App")
    (candidates . helm-go-apps-list)
    (action . (("Open App" . helm-go--open-app)))
    (candidate-number-limit .  9999)))

(defvar helm-go--web-source
  (helm-build-dummy-source "Web Search"
    :action '(("Web Search" .
               (lambda (candidate)
                 (browse-url
                  (concat
                   "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
                   (url-hexify-string candidate))))))))

(defun helm-go--calc-persistent-action (candidate)
  (let ((buf (get-buffer-create "*helm calculate result*"))
        result)
    (setq result (calc-eval candidate))
    (with-current-buffer buf
      (erase-buffer)
      (setq cursor-type nil)
      (insert result)
      (goto-char (point-min)))
    (display-buffer buf)))

(defvar helm-go--calc-source
  `((name . "Calculator")
    (candidates . (lambda () (list helm-pattern)))
    (action . (("Calculate" . (lambda (candidate)
                                (message (calc-eval candidate))))))
    (persistent-action . helm-go--calc-persistent-action)
    (volatile)
    (follow . 1)
    (follow-delay . 1)
    (require-pattern . 3)))

;;;###autoload
(defun helm-go (arg)
  "Helm Go entry point.
With prefix argument, refresh `helm-go-apps-list'."
  (interactive "P")
  (when arg
    (setq helm-go-apps-list-refresh-flag t))
  (when (or (null helm-go-apps-list) helm-go-apps-list-refresh-flag)
    (helm-go--collect-apps))
  (helm :sources '(helm-go--app-source
                   helm-go--calc-source
                   helm-go--web-source)
        :buffer "*Helm Go*"))

(provide 'helm-go)

;;; helm-go.el ends here
