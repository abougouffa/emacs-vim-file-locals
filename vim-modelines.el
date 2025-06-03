;;; vim-modelines.el --- Set editor options from Vim modeline

;; Copyright (C) 2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa
;; Keywords: files, text, emulations, matching

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

;; Vim's documentation at: https://vimhelp.org/options.txt.html#%27breakindent%27

;;; Code:

(require 'editorconfig)

(defgroup vim-modelines nil
  "Set some Emacs' editor options form Vim's modelines.")

(defcustom vim-modelines-modelines 5
  "Number of lines to consider when looking for modelines in the beginning and the end of the buffer."
  :type 'integer
  :group 'vim-modelines)

(defcustom vim-modelines-verbose nil
  "Verbose mode."
  :type 'boolean
  :group 'vim-modelines)

(defcustom vim-modelines-options-alist
  '(((filetype ft syntax syn) . vim-modelines-filetype)
    ((shiftwidth sw) . vim-modelines-shiftwidth)
    ((textwidth tw) . vim-modelines-textwidth)
    ((tabstop ts softtabstop sts) . vim-modelines-tabstop)
    ((number nu nonumber nonu) . vim-modelines-number)
    ((expandtab et noexpandtab noet) . vim-modelines-expandtab)
    ((readonly ro modifiable ma) . vim-modelines-readonly)
    ((breakindent bri nobreakindent nobri) . vim-modelines-breakindent)
    ((linebreak lbr nolinebreak nolbr) . vim-modelines-linebreak)
    ((relativenumber rnu norelativenumber nornu) . vim-modelines-relativenumber))
  "Vim modeline options and their handler functions."
  :type '(alist :key-type ((repeat symbol) :tag "Option and aliases") :value-type (function :tag "Handler function"))
  :group 'vim-modelines)

(defun vim-modelines--log (fmt &rest args)
  (when vim-modelines-verbose
    (apply #'message (append (list (concat "vim-modelines: " fmt)) args))))

(defun vim-modelines-extract-region (beg end)
  "Extract the options form region between BEG and END."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (mapcar
       (lambda (str) (let ((strs (string-split str "="))) (cons (car strs) (cadr strs))))
       (cond ((re-search-forward "[[:space:]]+\\(?:vi\\|vim\\|Vim\\|ex\\):[[:space:]]?set?[[:space:]]\\([^:]*\\):" nil t)
              (string-split (match-string-no-properties 1) "[[:space:]]" t))
             ((re-search-forward "[[:blank:]]+\\(?:vi:\\|vim:\\|ex:\\)[[:blank:]]?\\(.*\\)" nil t)
              (string-split (match-string-no-properties 1) "[[:space:]:]" t)))))))

(defun vim-modelines-extract ()
  "Extract the options from the current buffer."
  (append
   (save-excursion
     (goto-char (point-min))
     (let ((pos (point)))
       (forward-line vim-modelines-modelines)
       (vim-modelines-extract-region pos (point))))
   (save-excursion
     (goto-char (point-max))
     (let ((pos (point)))
       (forward-line (- vim-modelines-modelines))
       (vim-modelines-extract-region (point) pos)))))

;;;###autoload
(defun vim-modelines-apply ()
  "Apply the options in the current buffer."
  (let ((options (vim-modelines-extract)))
    (dolist (opt options)
      (when-let* ((name (intern (car opt)))
                  (value (cdr opt))
                  (handler (cdr (assoc name vim-modelines-options-alist (lambda (keys key) (memq key keys))))))
        (funcall handler name value options)))))

(defun vim-modelines-tabstop (name &optional value options)
  (let ((offset (string-to-number value)))
    (when (or (> offset 0) (< offset 40))
      (vim-modelines--log "set %S to %d" name offset)
      (cond ((memq name '(tabstop ts))
             (unless (or (assoc "sts" options) (assoc "smarttab" options))
               (message "vim-modelines: the smarttab option isn't implemented")
               (editorconfig-set-indentation nil offset)))
            ((memq name '(softtabstop sts))
             (editorconfig-set-indentation nil offset))))))

(defun vim-modelines-shiftwidth (name &optional value _options)
  (when-let* ((offset (string-to-number value))
              ((or (> offset 0) (< offset 40))))
    (vim-modelines--log "set %S to %d" name offset)
    (editorconfig-set-indentation nil nil value)))

(defun vim-modelines-textwidth (name &optional value _options)
  (let ((width (string-to-number value)))
    (when (or (> width 20))
      (vim-modelines--log "set %S to %d" name width)
      (setq fill-column width))))

(defun vim-modelines-number (name &optional value _options)
  (vim-modelines--log "set %S to %s" name value)
  (cond ((memq name '(number nu))
         (display-line-numbers-mode 1))
        ((memq name '(nonumber nonu))
         (display-line-numbers-mode -1))))

(defun vim-modelines-relativenumber (name &optional value _options)
  (vim-modelines--log "set %S to %s" name value)
  (cond ((memq name '(relativenumber rnu))
         (setq display-line-numbers 'relative))
        ((memq name '(norelativenumber nornu))
         (setq display-line-numbers t)))
  (display-line-numbers-mode 1))

(defun vim-modelines-expandtab (name &optional value _options)
  (vim-modelines--log "set %S to %s" name value)
  (cond ((memq name '(expandtab et))
         (setq indent-tabs-mode nil))
        ((memq name '(noexpandtab noet))
         (setq indent-tabs-mode t))))

(defun vim-modelines-filetype (name &optional value _options)
  (vim-modelines--log "set %S to %s" name value)
  (let ((value (string-split value "\\."))) ; In values like ":set ft=c.doxygen", ignore the second type
    (when-let* ((mode (alist-get (file-name-with-extension "dummy" value) auto-mode-alist nil nil #'string-match-p)))
      (message "vim-modelines: set filetype to %S (emacs mode: %S)" value mode)
      (funcall mode))))

(defun vim-modelines-readonly (name &optional value _options)
  (vim-modelines--log "set %S to %s" name value)
  (when (or (and (memq name '(readonly ro)) (equal value "on"))
            (and (memq name '(modifiable ma)) (equal value "off")))
    (read-only-mode 1)))

(defun vim-modelines-linebreak (name &optional value _options)
  (vim-modelines--log "set %S to %s" name value)
  (cond ((memq name '(linebreak lbr))
         (visual-line-mode 1))
        ((memq name '(nolinebreak nolbr))
         (visual-line-mode -1))))

(defun vim-modelines-breakindent (name &optional value _options)
  (vim-modelines--log "set %S to %s" name value)
  (when (fboundp 'visual-wrap-prefix-mode) ; Emacs 30
    (cond ((memq name '(breakindent bri))
           (visual-wrap-prefix-mode 1))
          ((memq name '(nobreakindent nobri))
           (visual-wrap-prefix-mode -1)))))

(define-minor-mode vim-modelines-mode
  "Enable support for Vim's modeline options."
  :global t
  (if vim-modelines-mode
      (add-hook 'find-file-hook #'vim-modelines-apply)
    (remove-hook 'find-file-hook #'vim-modelines-apply)))


(provide 'vim-modelines)
;;; vim-modelines.el ends here
