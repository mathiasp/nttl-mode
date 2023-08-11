;;; nttl-mode.el --- mode for NTurtle -*- lexical-binding: t; -*-

;;; Commentary:
;; nttl-mode.el is released under the terms of the two-clause BSD licence:
;;
;; Copyright 2003-2007, Hugo Haas <http://www.hugoh.net>
;; Copyright 2011-2012, Norman Gray <https://nxg.me.uk>
;; Copyright 2013, Daniel Gerber <https://danielgerber.net>
;; Copyright 2016, Peter Vasil <http://petervasil.net>
;; Copyright 2023, Mathias Picker <https://virtual-earth.de>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms,
;; with or without modification,
;; are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;;
;;
;;
;; See Hugo's commentary for original goals and further discussion,
;; at http://larve.net/people/hugo/2003/scratchpad/NotationThreeEmacsMode.html
;; Also draws on http://dishevelled.net/elisp/turtle-mode.el (which is for the _other_ turtle!)
;;
;; Project hosted at <https://github.com/mathiasp/nttl-mode>.  See there for updates.
;;
;; For documentation on Notation 3, see:
;; http://www.w3.org/DesignIssues/Notation3.html
;;
;; Current features:
;; - *Turtle* grammar subset
;; - approximate syntax highlighting
;; - comment/uncomment block with M-;
;; - indentation
;;
;; To use:
;;
;; (autoload 'nttl-mode "nttl-mode")
;; (add-hook 'nttl-mode-hook 'turn-on-font-lock)
;; (add-to-list 'auto-mode-alist '("\\.\\(n3\\|ttl\\|trig\\)\\'" . nttl-mode))
;;
;; Version: 0.1
;;
;; <2023-08-11 Fr.> seems I do not need to add anything for this to accept curly
;;                  braces. Lucy me :)

;;; Code:


;;;###autoload
(define-derived-mode nttl-mode prog-mode "NTurtle mode"
  "Major mode for NTurtle RDF documents. 

Nested Turtle or NTurtle is a conservative extension of turtle,
allowing to nest information about named nodes just like blank
nodes. It uses {} following the node IRI for named nodes
following the example of [] as used by blank nodes."

  ;; Comments syntax
  (set (make-local-variable 'comment-start) "# ")
  (modify-syntax-entry ?# "< b" nttl-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" nttl-mode-syntax-table)

  ;; fontification
  (setq font-lock-defaults
        `((,(regexp-opt '("@prefix" "PREFIX" "@base" "BASE" "a") 'symbols) ;keywords
           ("\\^\\^[^,;.]+" 0 font-lock-preprocessor-face t) ;literal types
           ("@[[:word:]_]+" . font-lock-preprocessor-face)   ;languages
           ("\\S-*?:" . font-lock-type-face)                 ;prefix
           (":\\([[:word:]_-]+\\)\\>" 1 font-lock-constant-face nil) ;suffix
           ("<.*?>" 0 font-lock-function-name-face t)                ;resources
           ("[,;.]" 0 font-lock-keyword-face)             ;punctuation
           ("^\\s-*\\(#.*\\)" 1 font-lock-comment-face t) ;comment
           ) nil))

  ;; indentation
  (set (make-local-variable 'indent-line-function) 'nttl-indent-line)
  (set (make-local-variable 'indent-tabs-mode) nil))

;; electric punctuation
;; (define-key nttl-mode-map (kbd "\,") 'nttl-electric-comma)
(define-key nttl-mode-map (kbd "\;") 'nttl-electric-semicolon)
(define-key nttl-mode-map (kbd "\.") 'nttl-electric-dot)
(define-key nttl-mode-map [backspace] 'nttl-hungry-delete-backwards)


(defgroup ttl nil "Customization for nttl-mode")

(defcustom nttl-indent-level 4
  "Number of spaces for each indentation step in `nttl-mode'."
  :type 'integer)

(defcustom nttl-electric-punctuation t
  "*If non-nil, `\;' or `\.' will self insert, reindent the line, and do a newline. (To insert while t, do: \\[quoted-insert] \;)."
  :type 'boolean)


(defun nttl-indent-line ()
  (interactive)
  (save-excursion
    (indent-line-to
     (or (ignore-errors (nttl-calculate-indentation)) 0)))
  (move-to-column (max (current-indentation) (current-column))))

(defun nttl-calculate-indentation ()
  (save-excursion
    (backward-to-indentation 0)
    (cond
     ;; in multiline string
     ((nth 3 (syntax-ppss)) 'noindent)	; (current-indentation)
     ;; empty line
     ((looking-at "$") (save-excursion (backward-to-indentation 1)))
     ;; beginning of stanza
     ((or (looking-at "@")         ; @prefix or @base
          (looking-at "PREFIX")    ; PREFIX
          (looking-at "BASE")      ; BASE
          (looking-at "#")         ; comment
          (save-excursion          ; a subject
            (while (forward-comment -1))
            (or (looking-back "\\.")
                (looking-back "PREFIX.*")
                (looking-back "BASE.*")
                (back-to-indentation) (looking-at "@"))) ; after prolog
          ) 0)
     ;; inside blank nodes
     (t (* nttl-indent-level
           (+ (if (save-excursion
                    (while (forward-comment -1))
                    (looking-back "\\,")) ; object list
                  2 1)
              (nth 0 (syntax-ppss))	; levels in parens
              ))))))

(defun nttl-insulate ()
  "Return true if this location should not be electrified"
  (or (not nttl-electric-punctuation)
      (let '(s (syntax-ppss))
        (or (nth 3 s)
            (nth 4 s)
            (nttl-in-resource-p)))))

(defun nttl-in-resource-p ()
  "Is point within a resource, marked by <...>?"
  (save-excursion
    (and (re-search-backward "[<> ]" nil t)
         (looking-at "<"))))

;; (defun nttl-electric-comma ()
;;   (interactive)
;;   (if (nttl-insulate) (insert ",")
;;     (if (not (looking-back " ")) (insert " "))
;;     (insert ",")
;;     (reindent-then-newline-and-indent)))

(defun nttl-electric-semicolon ()
  (interactive)
  (if (nttl-insulate) (insert ";")
    (if (not (looking-back " ")) (insert " "))
    (insert ";")
    (reindent-then-newline-and-indent)))

(defun nttl-electric-dot ()
  (interactive)
  (if (nttl-insulate) (insert ".")
    (if (not (looking-back " ")) (insert " "))
    (insert ".")
    (reindent-then-newline-and-indent)))

(defun nttl-skip-ws-backwards ()  ;adapted from cc-mode
  "Move backwards across whitespace."
  (while (progn
           (skip-chars-backward " \t\n\r\f\v")
           (and (eolp)
                (eq (char-before) ?\\)))
    (backward-char)))

(defun nttl-hungry-delete-backwards ()
  "Delete backwards, either all of the preceding whitespace,
or a single non-whitespace character if there is no whitespace before point."
  (interactive)
  (let ((here (point)))
    (nttl-skip-ws-backwards)
    (if (/= (point) here)
        (delete-region (point) here)
      (backward-delete-char-untabify 1))))

(defun nttl-mode-version ()
  "The version of nttl-mode."
  "0.1")

(provide 'nttl-mode)
;;; nttl-mode.el ends here
