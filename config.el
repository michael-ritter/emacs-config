;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Michael Ritter"
      user-mail-address "michael.ritter@tum.de")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
 (setq doom-font (font-spec :family "Fira Code" :size 14 :weight 'semi-light)
       doom-variable-pitch-font (font-spec :family "Overpass" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Emacs control is Ctrl. Emacs Meta is Command. Mac "option" key is unmodified. Function key is Emacs super.
(when (string= system-type "darwin")
  (setq mac-control-modifier 'control)
  (setq mac-option-modifier 'nil)
  (setq mac-command-modifier 'meta)
  (setq mac-right-command-modifier 'meta)
  (setq mac-right-option-modifier 'nil)
  (setq ns-function-modifier 'super)
  (mac-auto-operator-composition-mode t)
  )

;; set theme
(use-package! modus-operandi-theme
  :init
  ;; code here will run immediately
  :config
  ;; code here will run after the package is loaded
  ;(load-theme 'modus-operandi t)
  (load-theme 'modus-vivendi t)
 )

(use-package! dimmer
  :custom (dimmer-fraction 0.1)
  :config (dimmer-mode)
)

(use-package! multiple-cursors
  :init
        (setq mc/always-run-for-all t)
  :config
        (add-to-list 'mc/unsupported-minor-modes 'lispy-mode)
  :bind (("C-c m m" . mc/edit-lines)
         ("C-c m d" . mc/mark-all-like-this-dwim)
         ("C-c m >" . mc/mark-next-like-this)
         )
)

;; keybindings
;; Move backward / forward one word with C-left/right
(map! :after smartparens
      :map smartparens-mode-map
      [C-right] nil
      [C-left] nil)
(map!
 [C-right] 'forward-word
 "<C-left>" 'backward-word
 "<C-backspace>" 'backward-kill-word
 "<C-s-backspace>" 'kill-word
 )

(map! :after latex
      "C-c C-ö" 'next-error
)

(setq
 auto-save-default t
 )
(global-subword-mode 1) ; Iterate through CamelCase words
