;; -*- emacs-lisp -*-
;; inspired by https://github.com/sam217pa/emacs-config

;; ---------- bootstrapping --------------------------------------------------
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
;;(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
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
;(add-to-list 'default-frame-alist '(font . "Fira Code 14"))
                                        ;  (set-face-attribute 'default nil :font "Fira Code 14")
  (add-to-list 'default-frame-alist '(font . "SF Mono 14"))
  (set-face-attribute 'default nil :font "SF Mono 14")

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
(use-package company :ensure t
  :diminish ""
  :commands global-company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  (setq
   company-idle-delay 0.2
   company-selection-wrap-around t
   company-minimum-prefix-length 2
   company-require-match nil
   company-dabbrev-ignore-case nil
   company-dabbrev-downcase nil
   company-show-numbers t)

  :config
  (global-company-mode)

  (use-package company-statistics
    :quelpa (company-statistics :fetcher github :repo "company-mode/company-statistics")
    :config
    (company-statistics-mode))

  (bind-keys :map company-active-map
    ("C-d" . company-show-doc-buffer)
    ("C-l" . company-show-location)
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("C-t" . company-select-next)
    ("C-s" . company-select-previous)
    ("TAB" . company-complete))

  (setq company-backends
        '((company-css
           company-clang
           company-capf
           company-semantic
           company-xcode
           company-cmake
           company-files
           company-gtags
           company-etags
           company-keywords)))

  ;; from https://github.com/syl20bnr/spacemacs/blob/master/layers/auto-completion/packages.el
  (setq hippie-expand-try-functions-list
        '(
          ;; Try to expand word "dynamically", searching the current buffer.
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching all other buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill ring.
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a file name, as many characters as unique.
          try-complete-file-name-partially
          ;; Try to complete text as a file name.
          try-complete-file-name
          ;; Try to expand word before point according to all abbrev tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-list
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-line
          ;; Try to complete as an Emacs Lisp symbol, as many characters as
          ;; unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol)))

  (use-package counsel :ensure t
  :bind*
  (("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c f"   . counsel-git)
   ("C-c s"   . counsel-git-grep)
   ("C-c /"   . counsel-ag)
   ("C-c o"   . counsel-find-file-extern)
   ("C-S-s"   . counsel-ag)
   ("C-c l"   . counsel-locate))
  :config
  (setq counsel-find-file-at-point t)
  (setq counsel-locate-cmd 'counsel-locate-cmd-mdfind)
  (setq counsel-find-file-ignore-regexp "\\.DS_Store\\|.git")

  ;; from http://blog.binchen.org/posts/use-ivy-mode-to-search-bash-history.html
  (defun counsel-yank-bash-history ()
    "Yank the bash history"
    (interactive)
    (let (hist-cmd collection val)
      (shell-command "history -r")      ; reload history
      (setq collection
            (nreverse
             (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.bash_history"))
                                             (buffer-string))
                           "\n"
                           t)))
      (when (and collection (> (length collection) 0)
                 (setq val (if (= 1 (length collection)) (car collection)
                             (ivy-read (format "Bash history:") collection))))
        (insert val)
        (message "%s => kill-ring" val))))

  ;; TODO make the function respects reverse order of file
  (defun counsel-yank-zsh-history ()
    "Yank the zsh history"
    (interactive)
    (let (hist-cmd collection val)
      (shell-command "history -r")      ; reload history
      (setq collection
            (nreverse
             (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.zhistory"))
                                             (buffer-string))
                           "\n"
                           t)))
      (setq collection (mapcar (lambda (it) (replace-regexp-in-string ".*;" "" it)) collection))
      (when (and collection (> (length collection) 0)
                 (setq val (if (= 1 (length collection)) (car collection)
                             (ivy-read (format "Zsh history:") collection :re-builder #'ivy--regex-ignore-order))))
        (kill-new val)
        (insert val)
        (message "%s => kill-ring" val))))

  (defun counsel-package-install ()
    (interactive)
    (ivy-read "Install package: "
              (delq nil
                    (mapcar (lambda (elt)
                              (unless (package-installed-p (car elt))
                                (symbol-name (car elt))))
                            package-archive-contents))
              :action (lambda (x)
                        (package-install (intern x)))
              :caller 'counsel-package-install))
  (ivy-set-actions
   'counsel-find-file
   '(("o" (lambda (x) (counsel-find-file-extern x)) "open extern"))))

(use-package counsel-osx-app :ensure t
  :commands counsel-osx-app
  :bind*
  ("C-c a" . counsel-osx-app)
  :config
  (setq counsel-osx-app-location
        '("/Applications/" "~/Applications/")))

(use-package counsel-projectile :ensure t
  :bind* (("H-P" . counsel-projectile-switch-to-buffer)
          ("H-p" . counsel-projectile))
  :config
  (counsel-projectile-on))

(use-package counsel-gtags :ensure t
  :defer t)

(use-package css-mode :ensure t
  :mode (("\\.css\\'" . css-mode)))

(use-package csv-mode :ensure t
  :mode (("\\.csv\\'" . csv-mode))
  :bind (:map csv-mode-map
         ("'" . hydra-csv/body))
  :defines hydra-csv/body
  :config
  (defhydra hydra-csv (:hint nil :color amaranth)
    "
^NAV^        ^TRANSFORM^      ^ALIGN^         ^TOGGLE^         ^YANK^
_f_: fwd     _s_: sort        _a_: align      _d_: desc        _k_: kill
_b_: bwd     _S_: sort num    _A_: unalign    _T_: invisible   _y_: yank
_n_: next    _t_: transpose   ^ ^             ^ ^              _Y_: yank as new table
_p_: prev    _r_: reverse
"
    ("f" csv-forward-field)
    ("b" csv-backward-field)
    ("n" next-line)
    ("p" previous-line)
    ("t" csv-transpose)
    ("s" csv-sort-fields)
    ("S" csv-sort-numeric-fields)
    ("a" csv-align-fields)
    ("A" csv-unalign-fields)
    ("r" csv-reverse-region)
    ("d" csv-toggle-descending)
    ("T" csv-toggle-invisibility)
    ("k" csv-kill-fields)
    ("y" csv-yank-fields)
    ("Y" csv-yank-as-new-table)
    ("u" undo "undo")
    ("q" nil "quit" :color blue))

  (setq csv-invisibility-default nil)

  (defun csv--align-buffer ()
    (save-excursion
      (csv-align-fields t (point-min) (point-max))))

  (defun csv--next-or-new-field ()
    (cond ((looking-at ",$")
           (forward-char 1)
           (save-excursion (insert ",")))
          ((eq (point) (point-at-eol))
           (insert ","))
          (t
           (unless (re-search-forward "," nil t)
             (end-of-line))))
    (csv--align-buffer))

  (defun csv-tab-to-next-field ()
    (interactive)
    (if (or (mapcar #'looking-at csv-separators))
        (csv--next-or-new-field)
      (yas-expand)))

  (defun csv--previous-field ()
    (re-search-backward "," nil t))

  (defun csv-backtab-to-previous-field ()
    (interactive)
    (when (or (mapcar #'looking-at csv-separators))
      (csv--previous-field)))

  (defun csv--new-line-or-next-field ()
    (cond ((and (looking-back "," (- (point) 2))
                (eq (point) (point-at-eol)))
           (delete-char -1)
           (unless (re-search-forward "," nil t)
             (newline)))
          (t
           (if (eq (point) (point-max))
               (newline)
             (next-line))))
    (csv--align-buffer))

  (defun csv-new-line-or-next-field ()
    (interactive)
    (when (mapcar #'looking-at csv-separators)
      (csv--new-line-or-next-field)))

  (general-define-key
   :keymaps 'csv-mode-map
    "<tab>" 'csv-tab-to-next-field
    "<backtab>" 'csv-backtab-to-previous-field
    "RET" 'csv-new-line-or-next-field))


;; ---------- D --------------------------------------------------
;; ---------- E --------------------------------------------------
;; ---------- F --------------------------------------------------
;; ---------- G --------------------------------------------------
;; ---------- H --------------------------------------------------

(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package hl-line
  ;; highlight cursor line
  :init
  (global-hl-line-mode t))

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
(use-package ido
  :defer t
  :config
  (general-define-key :keymaps 'ido-completion-map
    "C-n" 'ido-next-match
    "C-p" 'ido-prev-match))

(use-package ivy
  :quelpa (ivy :fetcher github :repo "abo-abo/swiper")
  :diminish (ivy-mode . "")
  :commands (ivy-switch-buffer
             ivy-switch-buffer-other-window)
  :config
  (ivy-mode 1)

  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-height-alist nil)
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

(use-package magit
  :ensure t
  :commands (magit-blame
             magit-commit
             magit-commit-popup
             magit-diff-popup
             magit-diff-unstaged
             magit-fetch-popup
             magit-init
             magit-log-popup
             magit-pull-popup
             magit-push-popup
             magit-revert
             magit-stage-file
             magit-status
             magit-unstage-file
             magit-blame-mode)
  :bind (("C-x g" . magit-status))

  :config

  (global-git-commit-mode)

  (general-define-key
   :keymaps 'magit-mode-map
   "'" #'eshell-here)

  (use-package magit-popup :ensure t)
  (use-package git-commit :ensure t :defer t)

  (setq magit-completing-read-function 'ivy-completing-read))


(use-package markdown-mode :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("README\\'"   . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook (lambda () (auto-fill-mode 0)))
  )

(use-package markdown-preview-mode :ensure t
  )

;;;; Mercurial integration - Monky
(use-package monky
  :ensure t
  :commands monky-status
  :bind ("C-x h" . monky-status)
  :init
  (progn
    (setq monky-process-type 'cmdserver))
  :config
  (use-package hgignore-mode
    :ensure t
    )
  (use-package hgrc-mode
    :ensure t
    )
  )



(use-package multiple-cursors
  :ensure t
  :bind*
  (("M-s-s" . mc/mark-previous-like-this)
   ("M-s-t" . mc/mark-next-like-this)
   ("M-s-S" . mc/unmark-next-like-this)
   ("M-s-T" . mc/unmark-previous-like-this))
  :commands
  (hydra-mc/mc/mark-previous-like-this
   hydra-mc/mc/mark-next-like-this)
  :config

  ;; from https://github.com/abo-abo/hydra/wiki/multiple-cursors
  (defhydra hydra-mc (:hint nil)
    "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-p" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-n" mc/unmark-previous-like-this)
    ("r" mc/mark-all-in-region-regexp :exit t)
    ("q" nil :color blue)
    (" " nil "quit" :color blue)))


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

;; recent files
(use-package recentf
  :init
  (recentf-mode 1)

  :config

  ;; Increase limit
  (setq recentf-max-menu-items 25)

  ;; Emacs
  (add-to-list 'recentf-exclude (format "%s/.orhc-bibtex-cache" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/configuration/emacs\\.d/\\(?!\\(main.*\\)\\)" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/.*" (getenv "HOME")))

  ;; Some caches
  (add-to-list 'recentf-exclude (format "%s/\\.ido\\.last" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/\\.recentf" (getenv "HOME")))
  )

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

(use-package smooth-scrolling :ensure t
  :config
  (smooth-scrolling-mode)
  (setq smooth-scroll-margin 5))

(use-package smyx-theme
  :ensure t
  )

(use-package swiper :ensure t
  :bind* (("M-s" . swiper)
          ("M-S" . swiper-all)
          :map swiper-map
          ("C-s" . ivy-previous-history-element)
          ("C-t" . ivy-yank-word)))

;; ---------- T --------------------------------------------------
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (add-hook 'LaTeX-mode-hook (lambda ()
              ;                 (rainbow-delimiters-mode)
                               (turn-on-reftex)
                               (turn-on-visual-line-mode)
                               (setq TeX-source-correlate-mode t)
                               (setq TeX-source-correlate-method 'synctex)
                               (setq TeX-source-correlate-start-server t)
                               (setq TeX-PDF-mode-parsed t)
                               (setq TeX-parse-self t)
                               (setq-default TeX-master nil)
                               (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
                               (setq TeX-view-program-list '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

                               )
            )
  :bind (("C-c C-ö" . next-error))
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

;;; global keys
(global-set-key (kbd "C->") 'end-of-buffer)
(global-set-key (kbd "C-<") 'beginning-of-buffer)
(global-set-key (kbd "M->") 'end-of-buffer)
(global-set-key (kbd "M-<") 'beginning-of-buffer)

;; line numbers
(global-display-line-numbers-mode t)

;;; custom
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)
;(put 'erase-buffer 'disabled nil)
;(put 'narrow-to-region 'disabled nil)
;(put 'downcase-region 'disabled nil)
;(put 'upcase-region 'disabled nil)
