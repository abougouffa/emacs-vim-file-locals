;;; vim-modelines.el --- Set editor options from Vim modeline

;; Copyright (C) 2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa
;; Keywords: files, text, emulations, matching
;; Version: 0.2.1
;; URL: https://github.com/abougouffa/vim-modelines
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

;; Inspired by `vim-modelines' of Seong-Kook Shin

;; Vim's documentation at: https://vimhelp.org/options.txt.html

;;; Code:

(require 'editorconfig)

(defgroup vim-modelines nil
  "Set some Emacs' editor options form Vim's modelines."
  :group 'editing)

(defcustom vim-modelines-modelines 5
  "Number of lines to consider when looking for modelines in the beginning and the end of the buffer."
  :type 'integer
  :group 'vim-modelines)

(defcustom vim-modelines-verbose nil
  "Verbose mode."
  :type 'boolean
  :group 'vim-modelines)

(defcustom vim-modelines-before-apply-hook nil
  "Runs after setting `vim-modelines-buffer-options' and before applying options."
  :type '(repeat function)
  :group 'vim-modelines)

(defcustom vim-modelines-after-apply-hook nil
  "A hook to run after applying options."
  :type '(repeat function)
  :group 'vim-modelines)

(defcustom vim-modelines-options-alist
  '((("filetype" "ft" "syntax" "syn") . vim-modelines-filetype)
    (("shiftwidth" "sw") . vim-modelines-shiftwidth)
    (("textwidth" "tw") . vim-modelines-textwidth)
    (("tabstop" "ts" "softtabstop" "sts") . vim-modelines-tabstop)
    (("number" "nu" "nonumber" "nonu") . vim-modelines-number)
    (("expandtab" "et" "noexpandtab" "noet") . vim-modelines-expandtab)
    (("readonly" "ro" "modifiable" "ma") . vim-modelines-readonly)
    (("linebreak" "lbr" "nolinebreak" "nolbr") . vim-modelines-linebreak)
    (("smartindent" "si" "nosmartindent" "nosi" "autoindent" "ai" "noautoindent" "noai") . vim-modelines-smartindent)
    (("encoding" "enc") . vim-modelines-encoding)
    (("fileencoding" "fenc" "fileformat" "ff") . vim-modelines-fileencoding-fileformat)
    (("relativenumber" "rnu" "norelativenumber" "nornu") . vim-modelines-relativenumber))
  "Vim modeline options and their handler functions."
  :type '(alist :key-type ((repeat symbol) :tag "Option and aliases") :value-type (function :tag "Handler function"))
  :group 'vim-modelines)

(defun vim-modelines--log (fmt &rest args)
  (when vim-modelines-verbose
    (apply #'message (append (list (concat "vim-modelines: " fmt)) args))))

(defun vim-modelines-no-p (name)
  (and (string-prefix-p "no" name) t))

(defvar-local vim-modelines-buffer-options nil)
(defun vim-modelines-buffer-option (&rest names)
  (seq-some (lambda (name) (assoc name vim-modelines-buffer-options)) names))

(defun vim-modelines-extract-region (beg end)
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

(defun vim-modelines-extract ()
  "Extract the options from the current buffer."
  (save-excursion
    (append
     (vim-modelines-extract-region (goto-char (point-min)) (line-end-position (1+ vim-modelines-modelines)))
     (let ((pos (goto-char (point-max))))
       (vim-modelines-extract-region (line-beginning-position (- (1- vim-modelines-modelines))) pos)))))

;;;###autoload
(defun vim-modelines-apply ()
  "Apply the options in the current buffer."
  (when-let* ((options (vim-modelines-extract)))
    ;; Move "filetype" to the beginning so the mode is applied before other options
    (when-let* ((ft (assoc nil options (lambda (key _k) (member key '("filetype" "ft" "syntax" "syn"))))))
      (setq options (cons ft (assoc-delete-all (car ft) options))))
    (setq vim-modelines-buffer-options options)
    (run-hooks 'vim-modelines-before-apply-hook)
    (dolist (opt vim-modelines-buffer-options)
      (when-let* ((name (car opt))
                  (name (string-trim name))
                  (handler (cdr (assoc name vim-modelines-options-alist (lambda (keys key) (member key keys))))))
        (let* ((value (cdr opt))
               (value (and value (string-trim value))))
          (vim-modelines--log "setting %s%s" name (if value (format " to %s" value) ""))
          (funcall handler name value))))
    (run-hooks 'vim-modelines-after-apply-hook)))

(defun vim-modelines-tabstop (name &optional value)
  (when-let* ((offset (ignore-errors (string-to-number value))))
    (editorconfig-set-indentation nil value)))

(defun vim-modelines-shiftwidth (name &optional value)
  (when-let* ((offset (ignore-errors (string-to-number value))))
    (editorconfig-set-indentation nil nil value)))

(defun vim-modelines-textwidth (name &optional value)
  (when-let* ((width (ignore-errors (string-to-number value))))
    (setq fill-column width)))

(defun vim-modelines-number (name &optional _value)
  (display-line-numbers-mode (if (vim-modelines-no-p name) -1 1)))

(defun vim-modelines-relativenumber (name &optional _value)
  (setq display-line-numbers (if (vim-modelines-no-p name) 'relative t))
  (unless (vim-modelines-buffer-option "nonumber" "nonu")
    (display-line-numbers-mode 1)))

(defun vim-modelines-expandtab (name &optional _value)
  (setq indent-tabs-mode (vim-modelines-no-p name)))

(defun vim-modelines-filetype (name &optional value)
  (when-let* ((value (car (string-split value "\\."))) ; In values like ":set ft=c.doxygen", ignore the second type
              (mode (alist-get (file-name-with-extension "dummy" value) auto-mode-alist nil nil #'string-match-p)))
    (vim-modelines--log "inferred major mode for %s=%s: %S" name value mode)
    ;; Don't lose the saved options after applying the mode
    (let ((state (buffer-local-set-state vim-modelines-buffer-options vim-modelines-buffer-options)))
      (funcall mode)
      (buffer-local-restore-state state))))

(defun vim-modelines-readonly (name &optional value)
  (when (or (and (member name '("readonly" "ro")) (equal value "on"))
            (and (member name '("modifiable" "ma")) (equal value "off")))
    (read-only-mode 1)))

(defun vim-modelines-linebreak (name &optional _value)
  (cond ((member name '("linebreak" "lbr"))
         (visual-line-mode 1)
         (when (fboundp 'visual-wrap-prefix-mode) ; Emacs 30
           (cond ((vim-modelines-buffer-option "breakindent" "bri")
                  (vim-modelines--log "setting breakindent")
                  (visual-wrap-prefix-mode 1))
                 ((vim-modelines-buffer-option "nobreakindent" "nobri")
                  (vim-modelines--log "setting nobreakindent")
                  (visual-wrap-prefix-mode -1)))))
        ((member name '("nolinebreak" "nolbr"))
         (visual-line-mode -1))))

(defun vim-modelines-smartindent (name &optional _value)
  (electric-indent-mode (if (vim-modelines-no-p name) -1 1)))

(defun vim-modelines-encoding (name &optional value)
  (when-let* ((enc (unless (string-empty-p value) (intern value)))
              ((memq enc (coding-system-list 'base-only))))
    (setq buffer-file-coding-system enc)))


(defun vim-modelines-fileencoding-fileformat (_name &optional _value)
  (let ((coding
         (when-let* ((fenc (vim-modelines-buffer-option "fileencoding" "fenc"))
                     (val (unless (string-empty-p (cdr fenc)) (intern (downcase (cdr fenc)))))
                     (val (ignore-errors (coding-system-aliases val)))
                     (val (car (seq-intersection (coding-system-list 'base-only) val))))
           val))
        (eol
         (when-let* ((ff (vim-modelines-buffer-option "fileformat" "ff")))
           (pcase (cdr ff)
             ("unix" 'undecided-unix)
             ("mac" 'undecided-mac)
             ("dos" 'undecided-dos))))
        (def-type (coding-system-type buffer-file-coding-system))
        (def-eol (let* ((type (coding-system-eol-type buffer-file-coding-system))
                        (type (if (vectorp type) (seq-elt type 0) type)))
                   (nth type '(undecided-unix undecided-dos undecided-mac)))))
    (when-let* ((encoding (merge-coding-systems (or coding def-type) (or eol def-eol))))
      ;; We lure the `editorconfig-merge-coding-systems' function to return
      ;; our encoding, then, we call `editorconfig-merge-coding-systems' with
      ;; garbage arguments to apply that encoding to the buffer
      (vim-modelines--log "applying encoding %S" encoding)
      (cl-letf (((symbol-function 'editorconfig-merge-coding-systems) (lambda (&rest _) encoding)))
        (editorconfig-set-coding-system-revert nil nil)))))

;;;###autoload
(define-minor-mode vim-modelines-mode
  "Enable support for Vim's modeline options."
  :global t
  (if vim-modelines-mode
      (add-hook 'find-file-hook #'vim-modelines-apply)
    (remove-hook 'find-file-hook #'vim-modelines-apply)))


(provide 'vim-modelines)
;;; vim-modelines.el ends here
