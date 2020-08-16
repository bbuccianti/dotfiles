(local noscript (require :noscript))
(local modes (require :modes))
(local webview (require :webview))
(local completion (require :completion))
(local settings (require :settings))
(local downloads (require :downloads))
(local follow (require :follow))
(local window (require :window))
(local lousy (require :lousy))

(global lume (require :lume))
(global fennel (require :fennel))
(local view (require :fennelview))

(global pp (fn [x] (print (view x)))) ; convenience helper
(global escape lousy.util.escape)

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

(modes.add_binds :command [["<C-m>" "Emacs enter" #($:activate)]])

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
  (fn tab-completer [buf]
    (let [ret []]
      (each [_ w (pairs window.bywidget)]
        (each [_ v (ipairs w.tabs.children)]
          (table.insert ret {1 (escape v.title)
                             2 (escape v.uri)
                             :format v.uri})))
      ret))
  (set completion.completers.tabs {:header [:Title :URI]
                                   :func tab-completer}))

(modes.add_cmds [[::reinit "Reload this file" (partial lume.hotswap :browser)]
                 [::fennel "Run Fennel code"
                  (fn [_ o]
                    (match o
                      {: arg} (pp (fennel.eval arg))))]
                 [::switch "Switch tabs" {:func switch
                                          :format "{tabs}"}]])

(print :loaded-init)
