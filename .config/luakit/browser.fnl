(local noscript (require :noscript))
(local modes (require :modes))
(local completion (require :completion))
(local settings (require :settings))
(local downloads (require :downloads))
(local follow (require :follow))
(local window (require :window))
(local lousy (require :lousy))
(local escape lousy.util.escape)

(local filter (require :filter))

(global lume (require :lume))
(global fennel (require :fennel))
(local view (require :fennelview))

;; Use html version of duckduckgo by default
(set settings.window.default_search_engine :duckduckgo)
(set settings.window.home_page "https://html.duckduckgo.com/html")
(tset settings.window.search_engines
      "duckduckgo"
      "https://html.duckduckgo.com/html?q=%s")

;; Disable javascript by default
(set noscript.enable_scripts false)

;; Set downloads dir
(set downloads.default_dir (.. (os.getenv "HOME") "/downloads"))

;; Increase size of follow hints
(set follow.pattern_maker follow.pattern_styles.match_label)
(set follow.stylesheet
     (.. follow.stylesheet
         "#luakit_select_overlay .hint_label { font-size: 15px !important; }"))

;; Emacs like keybindings

(modes.add_binds :all [["<C-m>" "Emacs like enter" #($:activate)]
                       ["<C-g>" "Emacs like quit"
                        #(if (not ($:is_mode "passthrough"))
                             (do ($:set_prompt) ($:set_mode))
                             (not ($:is_mode "passthrough")))]])

;; White space in order to not fail on binds_chrome.lua

(modes.add_binds :command
                 [[::reinit "Reload this file"
                   {:func (partial lume.hotswap :browser)}]
                  [::fennel "Run Fennel code"
                   {:func (fn [w o] (w:notify (view (fennel.eval o.arg))))}]])

;; Same here!

(print :loaded-browser)
