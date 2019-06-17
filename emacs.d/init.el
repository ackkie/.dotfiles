;;; キーボード関連
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
;; 天気
;; M-x w3m-weather

;; load-file-name指定があるときにそのディレクトリ内に閉じ込める
;;emacs -q -l ~/path/to/somewhere/init.el
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;;; MELPA
(defvar my-favorite-package-list
  '(auto-complete
    bind-key
    helm
    image+
    magit
    migemo
    neotree
    use-package
    w3m
    win-switch
    php-mode
    web-mode
    yaml-mode)
  "packages to be installed")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)
(setq package-pinned-packages
      '((auto-complete . "melpa-stable")
        (bind-key . "melpa-stable")
        (helm . "melpa-stable")
        (image+ . "melpa-stable")
        (magit . "melpa-stable")
        (migemo . "melpa-stable")
        (neotree . "melpa-stable")
        (use-package . "melpa-stable")
        (w3m . "melpa-stable")
        (win-switch . "melpa-stable")
        (php-mode . "melpa-stable")
        (web-mode . "melpa-stable")
        (yaml-mode . "melpa-stable")))
(unless package-archive-contents (package-refresh-contents))
(dolist (pkg my-favorite-package-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))


(load-theme 'adwaita t)
(enable-theme 'adwaita)

(require 'use-package)

(keyboard-translate ?\C-h ?\C-?)
(bind-key "M-g" 'goto-line)
;;(bind-key "C-x g" 'magit-status)
(bind-key "M-q" 'query-replace-regexp)
(bind-key "M-r" 'replace-regexp)
(bind-key "M-e" 'eval-buffer)

(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)
(setq w3m-use-cookies t)

(setq w3m-weather-default-area "神奈川県・東部")

;; バッファの同一ファイル名を区別する(デフォルトパッケージ)
;;(require 'uniquify)
;;(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*")
  )

(use-package magit
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch-popup))
  )

;;; 現在行を目立たせる
(global-hl-line-mode)

(require 'win-switch)
;;; 0.75秒間受け付けるタイマー
(setq win-switch-idle-time 0.75)
(win-switch-setup-keys-ijkl "\C-xo")

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
;; 隠しファイルをデフォルトで表示
(setq neo-show-hidden-files t)
;; キーバインドをシンプルにする
;;(setq neo-keymap-style 'concise)
(setq neo-smart-open t)
;; neotree でファイルを新規作成した後、自動的にファイルを開く
(setq neo-create-file-auto-open t)

(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))

;; 辞書ファイルを環境に合わせて設定してください！
(setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")

(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(migemo-init)

;; 日本語
;;先にかかないとだめかも
(set-language-environment "Japanese")
;; テキストエンコーディングとしてUTF-8を優先使用
(prefer-coding-system 'utf-8)

;; mozc
;;(when (require 'mozc nil t)
;;  (setq default-input-method "japanese-mozc"))
(use-package mozc
  :bind  (("C-j" . mozc-mode)
          ("C-\\" . mozc-mode))
  :config  (setq default-input-method "japanese-mozc")
  )

(require 'helm-config)
(helm-mode 1)


;;
;; Auto Complete
;;
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'text-mode)         ;; text-modeでも自動的に有効にする
(add-to-list 'ac-modes 'fundamental-mode)  ;; fundamental-mode
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'yatex-mode)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)       ;; 補完メニュー表示時にC-n/C-pで補完候補選択
;;(setq ac-use-fuzzy t)          ;; 曖昧マッチ fuzzy.elが必要

;;
;;
;;
(setq-default tab-width 4)

;; 起動時のメッセージを非表示
(setq inhibit-startup-message t)

;; always end a file with a newline
(setq require-final-newline 'query)

;; 反対側のウィンドウにいけるように
(setq windmove-wrap-around t)

(defun my-c-c++-mode-init ()
  (setq c-basic-offset 4)
  (setq tab-width 4)
  )
(add-hook 'c-mode-hook 'my-c-c++-mode-init)
(add-hook 'c++-mode-hook 'my-c-c++-mode-init)

;;perl-mode の代わりに cperl-mode を使用
;; (defalias 'perl-mode 'cperl-mode)
(setq cperl-indent-level 4)
(setq cperl-highlight-variables-indiscriminately t)
(setq cperl-merge-trailing-else nil)


;; diffのバッファを上下ではなく左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)
