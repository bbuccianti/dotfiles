(local noscript (require :noscript))
(local modes (require :modes))
(local webview (require :webview))
(local completion (require :completion))
(local settings (require :settings))
(local downloads (require :downloads))
(local gselect (require :select))
(local follow (require :follow))

(global lume (require :lume))
(global fennel (require :fennel))
(local view (require :fennelview))

(global pp (fn [x] (print (view x)))) ; convenience helper

(set settings.window.default_search_engine :google)
(set settings.window.home_page "luakit://help/")
(set noscript.enable_scripts false)
(set downloads.default_dir (.. (os.getenv "HOME") "/downloads"))
(set follow.pattern_maker follow.pattern_styles.match_label)
(set follow.stylesheet
     (.. follow.stylesheet
         "#luakit_select_overlay .hint_label { font-size: 15px !important;}"))

(modes.add_binds :normal [["<C-A-r>" "Reinite" #($:enter_cmd ":reinit ")]
                          [",b" "Fancy tab switch" #($:enter_cmd ":switch ")]])

(modes.add_binds :command [["<C-m>" "Another enter" #($:activate)]])

(fn matching-tab [uris input n]
  (match (. uris n)
    uri (if (uri:find input)
            n
            (matching-tab uris input (+ n 1)))))

(fn switch [w opts]
  (match opts
    {: arg} (match (matching-tab (lume.map w.tabs.children :uri) arg 1)
              tabnum (w:goto_tab tabnum))))

;; I had to patch completion.lua to expose this; need to submit a PR.
(when completion.completers
  ;; TODO: actually do completion
  ;; TODO: trigger completion when in tab switcher
  (fn tab-completer [buf]
    )
  (set completion.completers.tabs {:header [:title :uri]
                                   :func tab-completer}))

(modes.add_cmds [[::reinit "Reload this file" (partial lume.hotswap :browser)]
                 [::fennel "Run Fennel code"
                  (fn [_ o]
                    (match o
                      {: arg} (let [value (fennel.eval arg)]
                                (print (view value)))))]
                 [::switch "Switch tabs" switch]])

(fn redirect [view status]
  (when (and (= status :committed) (view.uri:match "https://twitter.com/(.*)"))
    (set view.uri (view.uri:gsub "twitter.com" "nitter.net"))))

(webview.add_signal :init (fn [view] (view:add_signal :load-status redirect)))

(print :loaded-init)

;; bindings cheat sheet:

;; * Y - copy current URL
;; * H - back
;; * ;y - select target to copy URL
;; * ,ts - toggle scripts for domain
