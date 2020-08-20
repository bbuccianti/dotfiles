(local noscript (require :noscript))
(local modes (require :modes))
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

(set settings.window.default_search_engine :duckduckgo)
(set settings.window.home_page "https://html.duckduckgo.com/html")
(tset settings.window.search_engines "duckduckgo" "https://html.duckduckgo.com/html?q=%s")
(set noscript.enable_scripts false)
(set downloads.default_dir (.. (os.getenv "HOME") "/downloads"))
(set follow.pattern_maker follow.pattern_styles.match_label)
(set follow.stylesheet
     (.. follow.stylesheet
         "#luakit_select_overlay .hint_label { font-size: 15px !important; }"))

(modes.add_binds :normal [["<C-A-r>" "Reinit" #($:enter_cmd ":reinit ")]
                          [",b" "Fancy tab switch" #($:enter_cmd ":switch ")]])

(modes.add_binds :all [["<C-m>" "Emacs enter" #($:activate)]])

(fn matching-tab [uris input n]
  (let [uri (. uris n)]
    (if (uri:find input)
        n
        (matching-tab uris input (+ n 1)))))

(fn switch [w opts]
  (w:goto_tab (matching-tab (lume.map w.tabs.children :uri) opts.arg 1)))

(fn tab-completer [buf]
  (let [ret {}]
    (each [_ w (pairs window.bywidget)]
      (each [_ v (ipairs w.tabs.children)]
        (table.insert ret {1 (escape v.title)
                           2 (escape v.uri)
                           :format v.uri})))
    ret))

(set completion.completers.tabs {:header [:Title :URI]
                                 :func tab-completer})

(modes.add_binds :command
                 [[::reinit "Reload this file"
                   {:func (partial lume.hotswap :browser)}]
                  [::fennel "Run Fennel code"
                   {:func (fn [_ o] (pp (fennel.eval o.arg)))}]
                  [::switch "Switch tabs"
                   {:func switch :format "{tabs}"}]])

(print :loaded-init)
