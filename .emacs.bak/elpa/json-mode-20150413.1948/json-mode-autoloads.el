;;; json-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

<<<<<<< HEAD
;;;### (autoloads nil "json-mode" "json-mode.el" (21810 31668 0 0))
=======
;;;### (autoloads nil "json-mode" "json-mode.el" (21840 21280 0 0))
>>>>>>> acb741b2c1b24e016b5f05a912ebeb61c8fe42f8
;;; Generated autoloads from json-mode.el

(autoload 'json-mode "json-mode" "\
Major mode for editing JSON files

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(autoload 'json-mode-show-path "json-mode" "\


\(fn)" t nil)

(autoload 'json-mode-beautify "json-mode" "\
Beautify / pretty-print the active region (or the entire buffer if no active region).

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; json-mode-autoloads.el ends here
