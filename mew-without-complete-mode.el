;;; mew-without-complete-mode.el --  -*- coding: utf-8-emacs; lexical-binding: t -*-
;; Copyright (C) 2021, 2022 fubuki

;; Author: fubuki@frill.org
;; Version: @(#)$Revision: 1.7 $$Name:  $
;; Keywords: Mail

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; IN ADDITION, THE ORIGINAL COPYRIGHT IS AT THE BOTTOM OF THIS FILE.

;;; Commentary:

;; Deactivates Mew's Go and Refile completions to allow external extensions.

;;; Installation:

;; Activate when require.
;; (require 'mew-without-complete-mode)
;; (mew-without-complete-mode 1)

;; To inactivate...
;; (mew-without-complete-mode -1)

;; For command execution, on / off is toggled with no arguments.

;;; Code:
(require 'mew)

(defvar mcx-mew-complete-folders-hook nil)

(defun mcx-list2alist (default lst)
  (mapcar (lambda (a) (cons (car a) (car a))) (mcx-def2top default lst)))

(defun mcx-list2alist2 (default lst)
  (mapcar (lambda (a) (cons (car a) (car a))) (mcx-def2top2 default lst)))

(defun mcx-def2top (default lst)
  "Move DEFAULT in LST first. with case."
  (let* ((case (car (mew-summary-case-proto)))
         (default (list default))
         (lst (copy-sequence lst)))
    (if (and (not (string-equal case "default"))
             (member case mew-config-cases))
        (setq lst (append (mapcar (lambda (a) (list (concat case ":" (car a)))) lst) lst))
      lst)
    (cons default (delete default lst))))

(defun mcx-def2top2 (default lst)
  "Move DEFAULT in LST first. Not case."
  (let* ((default (list default))
         (lst (copy-sequence lst)))
    (cons default (delete default lst))))

(defun mcx-mew-complete-folders (&optional add-folders)
  "For `completing-read'. Returns a collection alist of accessible Mew folders."
  (let ((mew-inherit-complete-folder t)
        result)
    (run-hooks 'mcx-mew-complete-folders-hook)
    (setq result
          (append
           (mew-local-folder-alist)
           (mew-pop-folder-alist)
           (mew-nntp-folder-alist nil)
           (mew-imap-folder-alist nil)
           (mew-buffer-list "^\\*" t 'mew-virtual-mode)
           add-folders result))))

;; THIS FUNCTION IS NO TESTED.
(defun mcx/mew-input-local-folder (folder)
  "Input a local folder from the minibuffer."
  (mew-input-clear)
  (mew-input-folder-clean-up)
  (let* ((default folder)
	 (init (mew-folder-prefix folder))
	 (mew-input-complete-function 'mew-complete-local-folder)
	 (mew-circular-complete-function 'mew-circular-complete-case:)
         (ret (completing-read (format "Folder name (%s): " default)
                               (mcx-list2alist2 default (mew-local-folder-alist)) ;*
                               nil nil nil 'mew-input-folder-hist)))
    (when (or (string= ret "") (string= ret init))
      (setq ret default))
    (car (mew-input-folder-check (list ret)))))

(defun mcx/mew-input-folder (case folder)
  "Input a folder from the minibuffer."
  (mew-input-clear)
  (mew-input-folder-clean-up)
  (let* ((default (mew-case-folder case folder))
	 (init (mew-case-folder case (mew-folder-prefix folder)))
	 (mew-input-complete-function 'mew-complete-folder)
	 (mew-circular-complete-function 'mew-circular-complete-case:)
	 (ret (completing-read (format "Folder name (%s): " default)
                               (mcx-list2alist2 default (mcx-mew-complete-folders)) ;*
                               nil nil nil 'mew-input-folder-hist))
         (new-folder))
    (when (or (string= ret "") (string= ret init))
      (setq ret default))
    ;; mew-input-folder-check requires an IMAP separator,
    ;; which requires C-uZ. C-uZ needs "g". So, chicken and egg.
    ;; Skip the check for inbox to avoid the problem.
    (setq new-folder (mew-case:folder-folder ret))
    (if (member new-folder (list mew-inbox-folder
				 mew-pop-inbox-folder
				 mew-imap-inbox-folder
				 mew-nntp-newsgroup))
	ret
      (car (mew-input-folder-check (list ret))))))

(defun mcx/mew-input-refile-folders (folder-list singlep case proto)
  "Input refile folders from the minibuffer."
  (mew-input-clear)
  (mew-input-folder-clean-up)
  (let ((mew-input-complete-function (if (mew-folder-imapp proto)
					 'mew-complete-imap-folder
				       'mew-complete-local-folder))
	(mew-inherit-case case)
	(mew-input-folder-search-multi t)
	(mew-input-folder-refile t)
	;; Emacs 21.1, 21.2 and 21.3 has a bug of inhibit-quit.
	;; Set inhibit-quit to nil so that C-g can be used
	(inhibit-quit nil)
	(default (car folder-list))
        prompt init ret)
    (cond
     (singlep
      (setq init (mew-folder-prefix default))
      (if case
	  (setq prompt (format "Folder name <%s:> (%s): " case default))
	(setq prompt (format "Folder name (%s): " default))))
     (t
      (if case
	  (setq prompt (format "Folder name <%s:>: " case))
	(setq prompt "Folder name: "))
      (setq init (mew-join "," folder-list))))
    (setq ret (completing-read prompt
                               (mcx-list2alist2 default (mew-local-folder-alist)) ;*
			       nil nil nil 'mew-input-folder-hist))
    (when (and singlep (or (string= ret "") (string= ret init)))
      (setq ret default))
    (setq ret (mapcar 'mew-chop (mew-split ret ?,)))
    (mew-input-refile-folder-check
     ret (if (mew-folder-imapp proto) 'imap 'local))))

(defun mcx-mew-config-cases ()
  (mapcar #'list mew-config-cases))

(defun mcx/mew-input-case (default &optional edit)
  (mew-input-clear)
  (unless default (setq default mew-case-default))
  (let ((mew-input-complete-function 'mew-complete-case)
	(mew-circular-complete-function 'mew-circular-complete-case)
	(mew-input-exit-minibuffer-function 'mew-input-case-check)
	(mew-input-comma-function 'mew-input-case-check)
	case ret)
    (if edit
        (setq case (completing-read
                    "Case value: "
                    (mcx-list2alist2 default (mcx-mew-config-cases))
                    nil nil nil 'mew-input-case-hist))
      (setq case (completing-read
                  (format "Case value (%s): " default)
                  (mcx-list2alist2 default (mcx-mew-config-cases))
                  nil nil nil 'mew-input-case-hist)))
    (if (string= case "")
	default
      (dolist (cs (mew-split case ?,))
	(if (member cs mew-config-cases)
	    (setq ret (cons cs ret))))
      (mapconcat 'identity (nreverse ret) ","))))

;; (define-key mew-summary-mode-map
;;   "\C-c\C-w" 'mew-without-complete-mode)

(add-hook 'mew-summary-mode-hook
          #'(lambda ()
              (define-key-after
                (or (lookup-key mew-summary-mode-map [menu-bar Mew Misc])
                    (lookup-key mew-summary-mode-map [menu-bar mew Misc]))
                [without-complete]
                '(menu-item "Without Complete" mew-without-complete-mode
                            :button (:toggle . mew-without-complete-mode)))))

(define-minor-mode mew-without-complete-mode "Mew Completing eXtention."
  :group 'mail
  :init-value nil :global t
  (if mew-without-complete-mode
      (progn
        (advice-add 'mew-input-local-folder :override #'mcx/mew-input-local-folder)
        (advice-add 'mew-input-folder :override #'mcx/mew-input-folder)
        (advice-add 'mew-input-refile-folders :override #'mcx/mew-input-refile-folders)
        (advice-add 'mew-input-case :override #'mcx/mew-input-case))
    (advice-remove 'mew-input-local-folder #'mcx/mew-input-local-folder)
    (advice-remove 'mew-input-folder #'mcx/mew-input-folder)
    (advice-remove 'mew-input-refile-folders #'mcx/mew-input-refile-folders)
    (advice-remove 'mew-input-case #'mcx/mew-input-case)))

;; この拡張を実現するにあたり、オリジナル Source のかなりの部分をそのまま使っています.
;; オリジナルのコードが持っているコピーライトや配布規定はこの次にある引用によるものとし、
;; 拡張部分の配布規定等はこのファイルヘッダにあるよう GPL3 とします.
;; 引用部出典は Mew 6.8 の配布パッケージの中の記載ファイル名のものです.

;;; Original Copyright.

;;; ━━━━━ここから━━━━━
;;; mew-minibuf.el --- Minibuffer input methods for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 23, 1997

;;; Copyright Notice:

;; Copyright (C) 1997-2015 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-minibuf.el ends here
;;; ━━━━━ここまで━━━━━

(provide 'mew-without-complete-mode)
;; fin.
