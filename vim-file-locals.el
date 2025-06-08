;;; vim-file-locals.el --- Set editor options from Vim modeline

;; Copyright (C) 2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa
;; Keywords: files, text, emulations, matching
;; Version: 1.0.0
;; URL: https://github.com/abougouffa/vim-file-locals
;; Package-Requires: ((emacs "30.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Inspired by `vim-modeline' of Seong-Kook Shin

;; Vim's documentation at: https://vimhelp.org/options.txt.html

;;; Code:

(require 'editorconfig)

(defgroup vim-file-locals nil
  "Set some Emacs' editor options form Vim's modelines."
  :group 'editing)

(defcustom vim-file-locals-modelines 5
  "Number of lines to consider when looking for modelines in the beginning and the end of the buffer."
  :type 'integer
  :group 'vim-file-locals)

(defcustom vim-file-locals-verbose nil
  "Verbose mode."
  :type 'boolean
  :group 'vim-file-locals)

(defcustom vim-file-locals-before-apply-hook nil
  "Runs after setting `vim-file-locals-buffer-options' and before applying options."
  :type '(repeat function)
  :group 'vim-file-locals)

(defcustom vim-file-locals-after-apply-hook nil
  "A hook to run after applying options."
  :type '(repeat function)
  :group 'vim-file-locals)

(defcustom vim-file-locals-options-alist
  '((("filetype" "ft" "syntax" "syn") . vim-file-locals-filetype)
    (("shiftwidth" "sw") . vim-file-locals-shiftwidth)
    (("textwidth" "tw") . vim-file-locals-textwidth)
    (("tabstop" "ts" "softtabstop" "sts") . vim-file-locals-tabstop)
    (("number" "nu" "nonumber" "nonu") . vim-file-locals-number)
    (("expandtab" "et" "noexpandtab" "noet") . vim-file-locals-expandtab)
    (("readonly" "ro" "modifiable" "ma") . vim-file-locals-readonly)
    (("linebreak" "lbr" "nolinebreak" "nolbr") . vim-file-locals-linebreak)
    (("smartindent" "si" "nosmartindent" "nosi" "autoindent" "ai" "noautoindent" "noai") . vim-file-locals-smartindent)
    (("encoding" "enc") . vim-file-locals-encoding)
    (("fileencoding" "fenc" "fileformat" "ff") . vim-file-locals-fileencoding-fileformat)
    (("relativenumber" "rnu" "norelativenumber" "nornu") . vim-file-locals-relativenumber))
  "Vim modeline options and their handler functions."
  :type '(alist :key-type ((repeat symbol) :tag "Option and aliases") :value-type (function :tag "Handler function"))
  :group 'vim-file-locals)

(defun vim-file-locals--log (fmt &rest args)
  (when vim-file-locals-verbose
    (apply #'message (append (list (concat "vim-file-locals: " fmt)) args))))

(defun vim-file-locals-no-p (name)
  (and (string-prefix-p "no" name) t))

(defvar-local vim-file-locals-buffer-options nil)
(defun vim-file-locals-buffer-option (&rest names)
  (seq-some (lambda (name) (assoc name vim-file-locals-buffer-options)) names))

(defun vim-file-locals-extract-region (beg end)
  "Extract the options form region between BEG and END."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (mapcar
       (lambda (str) (let ((strs (string-split str "="))) (cons (car strs) (cadr strs))))
       (cond ((re-search-forward "[[:space:]]+\\(?:vi\\|vim\\|Vim\\|ex\\):[[:space:]]?set?[[:space:]]\\([^:]*\\):$" nil t)
              (string-split (match-string-no-properties 1) "[[:space:]]" t))
             ((re-search-forward "[[:space:]]+\\(?:vi:\\|vim:\\|ex:\\)[[:space:]]?\\(.*\\)$" nil t)
              (string-split (match-string-no-properties 1) "[[:space:]:]" t)))))))

(defun vim-file-locals-extract ()
  "Extract the options from the current buffer."
  (save-excursion
    (append
     (vim-file-locals-extract-region (goto-char (point-min)) (line-end-position (1+ vim-file-locals-modelines)))
     (let ((pos (goto-char (point-max))))
       (vim-file-locals-extract-region (line-beginning-position (- (1- vim-file-locals-modelines))) pos)))))

;;;###autoload
(defun vim-file-locals-apply ()
  "Apply the options in the current buffer."
  (when-let* ((options (vim-file-locals-extract)))
    ;; Move "filetype" to the beginning so the mode is applied before other options
    (when-let* ((ft (assoc nil options (lambda (key _k) (member key '("filetype" "ft" "syntax" "syn"))))))
      (setq options (cons ft (assoc-delete-all (car ft) options))))
    (setq vim-file-locals-buffer-options options)
    (run-hooks 'vim-file-locals-before-apply-hook)
    (dolist (opt vim-file-locals-buffer-options)
      (when-let* ((name (car opt))
                  (name (string-trim name))
                  (handler (cdr (assoc name vim-file-locals-options-alist (lambda (keys key) (member key keys))))))
        (let* ((value (cdr opt))
               (value (and value (string-trim value))))
          (vim-file-locals--log "setting %s%s" name (if value (format " to %s" value) ""))
          (funcall handler name value))))
    (run-hooks 'vim-file-locals-after-apply-hook)))

(defun vim-file-locals-tabstop (name &optional value)
  (when-let* ((offset (ignore-errors (string-to-number value))))
    (editorconfig-set-indentation nil value)))

(defun vim-file-locals-shiftwidth (name &optional value)
  (when-let* ((offset (ignore-errors (string-to-number value))))
    (editorconfig-set-indentation nil nil value)))

(defun vim-file-locals-textwidth (name &optional value)
  (when-let* ((width (ignore-errors (string-to-number value))))
    (setq fill-column width)))

(defun vim-file-locals-number (name &optional _value)
  (display-line-numbers-mode (if (vim-file-locals-no-p name) -1 1)))

(defun vim-file-locals-relativenumber (name &optional _value)
  (setq display-line-numbers (if (vim-file-locals-no-p name) 'relative t))
  (unless (vim-file-locals-buffer-option "nonumber" "nonu")
    (display-line-numbers-mode 1)))

(defun vim-file-locals-expandtab (name &optional _value)
  (setq indent-tabs-mode (vim-file-locals-no-p name)))

(defun vim-file-locals-filetype (name &optional value)
  (when-let* ((value (car (string-split value "\\."))) ; In values like ":set ft=c.doxygen", ignore the second type
              ;; Interpret the syntax value as a file extension or a mode name,
              ;; this is not the accurate way to do this, but it can work for
              ;; some syntaxes without hassle
              (mode (or (alist-get (file-name-with-extension "dummy" value) auto-mode-alist nil nil #'string-match-p)
                        (seq-find #'fboundp (mapcar #'intern `(,(format "%s-mode" value) ,(format "%s-ts-mode" value)))))))
    (vim-file-locals--log "inferred major mode for %s=%s: %S" name value mode)
    ;; Don't lose the saved options after applying the mode
    (let ((state (buffer-local-set-state vim-file-locals-buffer-options vim-file-locals-buffer-options)))
      (funcall mode)
      (buffer-local-restore-state state))))

(defun vim-file-locals-readonly (name &optional value)
  (when (or (and (member name '("readonly" "ro")) (equal value "on"))
            (and (member name '("modifiable" "ma")) (equal value "off")))
    (read-only-mode 1)))

(defun vim-file-locals-linebreak (name &optional _value)
  (cond ((member name '("linebreak" "lbr"))
         (visual-line-mode 1)
         (when (fboundp 'visual-wrap-prefix-mode) ; Emacs 30
           (cond ((vim-file-locals-buffer-option "breakindent" "bri")
                  (vim-file-locals--log "setting breakindent")
                  (visual-wrap-prefix-mode 1))
                 ((vim-file-locals-buffer-option "nobreakindent" "nobri")
                  (vim-file-locals--log "setting nobreakindent")
                  (visual-wrap-prefix-mode -1)))))
        ((member name '("nolinebreak" "nolbr"))
         (visual-line-mode -1))))

(defun vim-file-locals-smartindent (name &optional _value)
  (electric-indent-mode (if (vim-file-locals-no-p name) -1 1)))

(defun vim-file-locals-encoding (name &optional value)
  (when-let* ((enc (unless (string-empty-p value) (intern value)))
              ((memq enc (coding-system-list 'base-only))))
    (setq buffer-file-coding-system enc)))

(defun vim-file-locals-fileencoding-fileformat (_name &optional _value)
  (let ((coding
         (or (when-let* ((fenc (vim-file-locals-buffer-option "fileencoding" "fenc"))
                         (val (unless (string-empty-p (cdr fenc)) (intern (downcase (cdr fenc)))))
                         (val (ignore-errors (coding-system-aliases val)))
                         (val (car (seq-intersection (coding-system-list 'base-only) val))))
               val)
             (coding-system-type buffer-file-coding-system))) ; Get the current coding system
        (eol
         (or (when-let* ((ff (vim-file-locals-buffer-option "fileformat" "ff"))
                         ((member (downcase (cdr ff)) '("unix" "mac" "dos"))))
               (intern (concat "undecided-" ff)))
             ;; Get the default EOF type from the current coding system
             (let* ((type (coding-system-eol-type buffer-file-coding-system))
                    (type (if (vectorp type) (seq-elt type 0) type)))
               (nth type '(undecided-unix undecided-dos undecided-mac))))))
    (when-let* ((encoding (merge-coding-systems coding eol)))
      ;; We lure the `editorconfig-merge-coding-systems' function to return our
      ;; encoding, then, we call `editorconfig-set-coding-system-revert' with
      ;; garbage arguments to apply that encoding to the buffer
      (vim-file-locals--log "applying encoding %S" encoding)
      (cl-letf (((symbol-function 'editorconfig-merge-coding-systems) (lambda (&rest _) encoding)))
        (editorconfig-set-coding-system-revert nil nil)))))

(define-obsolete-function-alias 'vim-modelines-mode 'vim-file-locals-mode "v1.0.0")

;;;###autoload
(define-minor-mode vim-file-locals-mode
  "Enable support for Vim's modeline options."
  :global t
  (if vim-file-locals-mode
      (add-hook 'find-file-hook #'vim-file-locals-apply)
    (remove-hook 'find-file-hook #'vim-file-locals-apply)))


(provide 'vim-file-locals)
;;; vim-file-locals.el ends here
