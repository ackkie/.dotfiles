;;; キーボード関連
;; describe-bindings

;; ナローイング
;; C-x n n
;; ワイデン
;; C-x n w
;;
;;; 改行、エンコーディング変換
;; C-x RET f

;; init.el　再ロード
;; M-x eval-buffer
;; テーマ変更
;; M-x customize-themes

;; see http://ctags.sourceforge.net/ctags.html
;; ctags -Re --languages=Perl
;; M-. or C-x 4 .
;; M-* or pop-tag-mark
;; C-u M-.

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-packageをインストールする
(straight-use-package 'use-package)

;; オプションなしで自動的にuse-packageをstraight.elにフォールバックする
;; 本来は (use-package hoge :straight t) のように書く必要がある
(setq straight-use-package-by-default t)


(use-package bind-key)


(use-package git-gutter
  :init
  (when (window-system)
	(use-package git-gutter-fringe))
  (global-git-gutter-mode))


;; --- go mode

;;(use-package exec-path-from-shell)
(use-package auto-complete)
(use-package flycheck) 

(use-package go-autocomplete)

(use-package go-mode
  :config
  (bind-keys :map go-mode-map
			 ("M-." . godef-jump)
			 ("M-j" . godef-jump-other-window)
			 ("M-," . pop-tag-mark))
  (add-hook 'go-mode-hook '(lambda () (setq tab-width 2)))
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-eldoc
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-dlv)


;; godef-jump C-cC-j
;; TODO: xref-pop-marker-stack で戻れる (M-,)

(use-package php-mode)

(ac-config-default)

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-c y"   . helm-show-kill-ring)
         ("C-c m"   . helm-man-woman)
         ("C-c o"   . helm-occur)
         :map helm-map
         ("C-h" . delete-backward-char)
         :map helm-find-files-map
         ("C-h" . delete-backward-char))
  :init
  (custom-set-faces
   '(helm-header           ((t (:background "#3a3a3a" :underline nil))))
   '(helm-source-header    ((t (:background "gray16" :foreground "gray64" :slant italic))))
   '(helm-candidate-number ((t (:foreground "#00afff"))))
   '(helm-selection        ((t (:background "#005f87" :weight normal))))
   '(helm-match            ((t (:foreground "darkolivegreen3")))))
  :config
  (helm-mode 1))

;;(use-package helm-flycheck)
;; (global-flycheck-mode)


(use-package magit
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch-popup))
  )

;; バッファの同一ファイル名を区別する(デフォルトパッケージ)
(use-package uniquify
  :straight nil
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*")
  )

(bind-key "M-g" 'goto-line)
(bind-key "M-q" 'query-replace-regexp)
(bind-key "M-r" 'replace-regexp)
(bind-key "M-e" 'eval-buffer)
(bind-key "M-f" 'grep-find)

;;; 現在行を目立たせる
;; (global-hl-line-mode)

;;
;;
;;
;; タブにスペースを使用する
(setq-default tab-width 4 indent-tabs-mode nil)
;cperlmodeの変数でもよいかも

;; diffのバッファを上下ではなく左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)

;; 起動時のメッセージを非表示
(setq inhibit-startup-message t)

;; always end a file with a newline
(setq require-final-newline 'query)

;; 反対側のウィンドウにいけるように
(setq windmove-wrap-around t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ctrl-x p で逆向きへのウィンドウ移動
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-xp" (lambda () (interactive) (other-window -1)))

(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'perl-mode-hook 'flycheck-mode)

;; 行番号を表示
(require 'linum)
(global-linum-mode)
