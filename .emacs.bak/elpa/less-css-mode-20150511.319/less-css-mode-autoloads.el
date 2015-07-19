;;; less-css-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

<<<<<<< HEAD
;;;### (autoloads nil "less-css-mode" "less-css-mode.el" (21863 27317
=======
;;;### (autoloads nil "less-css-mode" "less-css-mode.el" (21873 17877
>>>>>>> acb741b2c1b24e016b5f05a912ebeb61c8fe42f8
;;;;;;  0 0))
;;; Generated autoloads from less-css-mode.el

(autoload 'less-css-compile "less-css-mode" "\
Compiles the current buffer to css using `less-css-lessc-command'.

\(fn)" t nil)

(autoload 'less-css-mode "less-css-mode" "\
Major mode for editing LESS files, http://lesscss.org/
Special commands:
\\{less-css-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; less-css-mode-autoloads.el ends here
