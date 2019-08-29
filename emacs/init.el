
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)



(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")t)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

(global-display-line-numbers-mode)
(setq linum-format "%d: ")

(electric-pair-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)

(overwrite-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

(keyboard-translate ?\C-h ?\C-?)

(set-frame-font "Tamsyn 20" nil t)

(pdf-tools-install)

(setq auto-save-default nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (exec-path-from-shell magit company racer rust-mode helm-cider helm-gtags helm cyberpunk-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'cyberpunk t)

(require 'helm-config)

(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x C-b") #'helm-buffers-list)
(global-set-key (kbd "C-x C-d") #'helm-browse-project)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(global-set-key (kbd "C-x g") 'magit-status)

(require 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            ;; enable telemetry
            (meghanada-telemetry-enable t)
            (flycheck-mode +1)
            (setq c-basic-offset 2)
            ;; use code format
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
(cond
   ((eq system-type 'windows-nt)
    (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
    (setq meghanada-maven-path "mvn.cmd"))
   (t
    (setq meghanada-java-path "java")

;;(global-set-key (kbd "TAB") 'do-indent)

;; (setq max-lisp-eval-depth 10000)

(defun switch-to-previous-buffer()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "M-o") #'switch-to-previous-buffer)


(delete-selection-mode t)

;;(require 'helm-gtags)
;; Enable helm-gtags-mode
;; (add-hook 'dired-mode-hook 'helm-gtags-mode)
;; (add-hook 'eshell-mode-hook 'helm-gtags-mode)
;; (add-hook 'c-mode-hook 'helm-gtags-mode)
;; (add-hook 'c++-mode-hook 'helm-gtags-mode)
;; (add-hook 'asm-mode-hook 'helm-gtags-mode)


;; (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
;; (define-key helm-gtags-mode (kbd "C-j") 'helm-gtags-select
;; (define-key helm-gtags-mode (kbd "M-.") 'helm-gtags-dwim)
;; (define-key helm-gtags-mode (kbd "M-,") 'helm-gtags-pop-stack)
;; (define-key helm-gtags-mode (kbd "C-c <") 'helm-gtags-previous-history)
;; (define-key helm-gtags-mode (kbd "C-c >") 'helm-gtags-next-history)


(add-hook 'racer-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(racer-mode 1)

;; setq tab-always-indent 'complete)

;; (defun do-indent ()
;;   "Insert 4 spaces"
;;   (interactive)
;;   (insert "    "))

