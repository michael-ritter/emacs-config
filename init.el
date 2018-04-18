;; -*- emacs-lisp -*-

;; ---------- bootstrapping --------------------------------------------------
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish)
  (package-install 'bind-key)
  (package-install 'quelpa)
  (package-install 'quelpa-use-package)
)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(require 'quelpa)
(require 'quelpa-use-package)

;;; Sane default
(setq
 use-package-verbose nil  ; use-package is not verbose
 delete-old-versions t  ; delete old versions
 version-control t      ; enable version control
 auto-save-file-name-transforms
 '((".*" "~/.emacs.d/auto-save-list/" t)); transform autosave filenames
 inhibit-startup-screen t ;
 ring-bell-function 'ignore ;
 sentence-end-double-space nil          ; sentences do not end with double space.
 default-fill-column 72
 initial-scratch-message ""
 save-interprogram-paste-before-kill t
 help-window-select t                   ; focus help window when opened
 tab-width 2                    ; tab are 2 spaces large
 )

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq-default indent-tabs-mode nil
              tab-width 2)

(prefer-coding-system 'utf-8)           ; utf-8 as system default

(defalias 'yes-or-no-p 'y-or-n-p) ; remplace yes no par y n
(show-paren-mode) ; highlight delimiters
(line-number-mode) ; display line number in mode line
(column-number-mode) ; display colum number in mode line
(save-place-mode)    ; save cursor position between sessions
(delete-selection-mode 1)               ; replace highlighted text with type
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; appearence
(when window-system
  (tooltip-mode -1)                     ; no tooltips
  (tool-bar-mode -1)                    ; no toolbar
  (menu-bar-mode -1)                    ; no menubar
  (scroll-bar-mode -1)                  ; no scrollbar
  (blink-cursor-mode -1)                 ; no blinking cursor
  (set-frame-size (selected-frame) 85 61)
  (add-to-list 'default-frame-alist '(height . 46))
  (add-to-list 'default-frame-alist '(width . 85))
  ;; change default font for current frame
(add-to-list 'default-frame-alist '(font . "Fira Code 14"))
  (set-face-attribute 'default nil :font "Fira Code 14")
  )

;;; keybindings
(when (eq system-type 'darwin)           ; mac specific bindings
  (setq mac-command-modifier 'meta    ; left command = emacs meta
        mac-option-modifier nil       ; left option = leave untouched
        mac-right-option-modifier nil ; right option = leave untouched
        mac-control-modifier 'control ; left control = emacs control
        ns-function-modifier 'hyper ; fn key = hyper
        )
  (setq mac-pass-control-to-system nil)  ; idem
  (setq locate-command "mdfind")
  (setq delete-by-moving-to-trash t)
;  (defun system-move-file-to-trash (file)
;    "Use \"trash\" to move FILE to the system trash.
;When using Homebrew, install it using \"brew install trash\"."
 ;   (call-process (executable-find "trash") nil nil nil file))
  )

;; ---------- A --------------------------------------------------

(use-package apropospriate
  :ensure apropospriate-theme
  )

;; ---------- B --------------------------------------------------
;; ---------- C --------------------------------------------------
;; ---------- D --------------------------------------------------
;; ---------- E --------------------------------------------------
;; ---------- F --------------------------------------------------
;; ---------- G --------------------------------------------------
;; ---------- H --------------------------------------------------

(use-package hl-line
  ;; souligne la ligne du curseur
  :init
  (global-hl-line-mode -1))

(use-package htmlize :ensure t
  :defer t)

(use-package hungry-delete :ensure t
  :diminish ""
  :config
  (global-hungry-delete-mode))

(use-package hydra :ensure t
  :config
  (setq hydra-is-helpful t))

;; ---------- I --------------------------------------------------

(use-package ivy
  :quelpa (ivy :fetcher github :repo "abo-abo/swiper")
  :diminish (ivy-mode . "")
  :commands (ivy-switch-buffer
             ivy-switch-buffer-other-window)
  :config
  (ivy-mode 1)

  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  ;; from https://github.com/company-mode/company-statistics
  ;; ignore buffers in the ignore buffer list.
  (setq ivy-use-ignore-default 'always)
  (setq ivy-ignore-buffers '("company-statistics-cache.el" "company-statistics-autoload.el"))
  ;; if ivy-flip is t, presents results on top of query.
  (setq ivy-flip t)
  (setq ivy-overlay-at nil)
                                        ;  (setq ivy-re-builders-alist '((t      . ivy--regex-ignore-order)))
    (setq ivy-re-builders-alist '((t      . ivy--regex-fuzzy)))

  (defun ivy--matcher-desc ()
    (if (eq ivy--regex-function
            'ivy--regex-fuzzy)
        "fuzzy"
      "ivy"))

  (defhydra hydra-ivy (:hint nil
                       :color pink)
    "
^ ^ ^ ^ ^ ^ | ^Call^      ^ ^  | ^Cancel^ | ^Options^ | Action _b_/_é_/_p_: %-14s(ivy-action-name)
^-^-^-^-^-^-+-^-^---------^-^--+-^-^------+-^-^-------+-^^^^^^^^^^^^^^^^^^^^^^^^^^^^^---------------------------
^ ^ _s_ ^ ^ | _f_ollow occ_u_r | _i_nsert | _C_: calling %-5s(if ivy-calling \"on\" \"off\") _C-c_ase-fold: %-10`ivy-case-fold-search
_c_ ^+^ _r_ | _d_one      ^ ^  | _o_ops   | _m_: matcher %-5s(ivy--matcher-desc)^^^^^^^^^^^^ _T_runcate: %-11`truncate-lines
^ ^ _t_ ^ ^ | _g_o        ^ ^  | ^ ^      | _<_/_>_: shrink/grow^^^^^^^^^^^^^^^^^^^^^^^^^^^^ _D_efinition of this menu
"
    ;; arrows
    ("c" ivy-beginning-of-buffer)
    ("t" ivy-next-line)
    ("s" ivy-previous-line)
    ("r" ivy-end-of-buffer)
    ;; actions
    ("o" keyboard-escape-quit :exit t)
    ("C-g" keyboard-escape-quit :exit t)
    ("i" nil)
    ("C-o" nil)
    ("f" ivy-alt-done :exit nil)
    ("C-j" ivy-alt-done :exit nil)
    ("d" ivy-done :exit t)
    ("g" ivy-call)
    ("C-m" ivy-done :exit t)
    ("C" ivy-toggle-calling)
    ("m" ivy-toggle-fuzzy)
    (">" ivy-minibuffer-grow)
    ("<" ivy-minibuffer-shrink)
    ("b" ivy-prev-action)
    ("é" ivy-next-action)
    ("p" ivy-read-action)
    ("T" (setq truncate-lines (not truncate-lines)))
    ("C-c" ivy-toggle-case-fold)
    ("u" ivy-occur :exit t)
    ("D" (ivy-exit-with-action
          (lambda (_) (find-function 'hydra-ivy/body)))
     :exit t))

  (defun ivy-switch-project ()
    (interactive)
    (ivy-read
     "Switch to project: "
     (if (projectile-project-p)
         (cons (abbreviate-file-name (projectile-project-root))
               (projectile-relevant-known-projects))
       projectile-known-projects)
     :action #'projectile-switch-project-by-name))

  (global-set-key (kbd "C-c m") 'ivy-switch-project)

  (ivy-set-actions
   'ivy-switch-project
   '(("d" dired "Open Dired in project's directory")
     ("v" counsel-projectile "Open project root in vc-dir or magit")
     ("c" projectile-compile-project "Compile project")
     ("r" projectile-remove-known-project "Remove project(s)"))))

;; ---------- J --------------------------------------------------
;; ---------- K --------------------------------------------------
;; ---------- L --------------------------------------------------
;; ---------- M --------------------------------------------------

(use-package markdown-mode :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("README\\'"   . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook (lambda () (auto-fill-mode 0)))
)

;; ---------- N --------------------------------------------------

;; (use-package nlinum
;;   :ensure t
;;   :commands (global-nlinum-mode
;;              nlinum-mode)
;;   :init
;;   :config
;;   (global-nlinum-mode))

;; (use-package nlinum-relative :ensure t
;;   :disabled t
;;   :commands (nlinum-relative-mode -1)
;;   :init
;;   (add-hook 'prog-mode-hook 'nlinum-relative-mode))

;; ---------- O --------------------------------------------------
;; ---------- P --------------------------------------------------
;; ---------- Q --------------------------------------------------
;; ---------- R --------------------------------------------------
;; ---------- S --------------------------------------------------

(use-package smartparens
   :ensure t
   :diminish (smartparens-mode . "")
   :commands (smartparens-global-mode)
   :init
   (add-hook 'after-init-hook (lambda () (smartparens-global-mode)))
   (add-hook 'prog-mode-hook (lambda () (smartparens-strict-mode)))

;;   :config
;;   ;; Only use smartparens in web-mode
;;   (sp-local-pair 'markdown-mode "_" "_")
;;   (sp-local-pair 'markdown-mode "**" "**")
;;   (sp-local-pair 'markdown-mode "`" "`")
;;   (sp-local-pair 'web-mode "<% " " %>")
;;   (sp-local-pair 'web-mode "{ " " }")
;;   (sp-local-pair 'web-mode "<%= "  " %>")
;;   (sp-local-pair 'web-mode "<%# "  " %>")
;;   (sp-local-pair 'web-mode "<%$ "  " %>")
;;   (sp-local-pair 'web-mode "<%@ "  " %>")
;;   (sp-local-pair 'web-mode "<%: "  " %>")
;;   (sp-local-pair 'web-mode "{{ "  " }}")
;;   (sp-local-pair 'web-mode "{% "  " %}")
;;   (sp-local-pair 'web-mode "{%- "  " %}")
;;   (sp-local-pair 'web-mode "{# "  " #}")
 )


(use-package smyx-theme
  :ensure t
  )

;; ---------- T --------------------------------------------------
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
)
;; ---------- U --------------------------------------------------
;; ---------- V --------------------------------------------------
;; ---------- W --------------------------------------------------
;; ---------- X --------------------------------------------------
;; ---------- Y --------------------------------------------------
;; ---------- Z --------------------------------------------------


;; Load a dark theme if in GUI, load a light theme if in NW
(if (display-graphic-p)
    (load-theme 'smyx t)
  (load-theme 'apropospriate-light t)
  )

;;; custom
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)
;(put 'erase-buffer 'disabled nil)
;(put 'narrow-to-region 'disabled nil)
;(put 'downcase-region 'disabled nil)
;(put 'upcase-region 'disabled nil)
