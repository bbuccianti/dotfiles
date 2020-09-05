(local history (require :history))
(local bookmarks (require :bookmarks))
(local lousy (require :lousy))
(local modes (require :modes))

(local view (require :fennelview))

(local pp (fn [x] (print (view x)))) ; convenience helper
(local escape lousy.util.escape)

(local _M {})
(local data (setmetatable {} { :__mode :k}))

(fn get-input [input]
  (or (string.match input ":%a+ (.*)") ""))

(fn make-candidates [w buf]
  (let [cmd (string.match buf ":(%a+) ")
        input (get-input buf)
        group (match cmd
                :switch :tabs
                :gohistory :history
                :gobookmark :bookmarks)
        ret {}]
    (table.insert ret (. _M.groups group :header))
    ((. _M.groups group :func) ret w input)))

(fn _M.update-menu [w buf]
  (w.menu:build (make-candidates w buf))
  (w.menu:show)
  (w.menu:move_down))

(modes.new_mode "filter"
                {:enter (fn [w cmd]
                          (let [input (.. ":" cmd " ")]
                            (w:set_prompt)
                            (w:set_input input)
                            (w.menu:add_signal "changed" #(when $ $))
                            (_M.update-menu w input)))
                 :changed _M.update-menu
                 :leave #($.menu:hide)
                 :activate (fn [w text]
                             (w:enter_cmd text)
                             (w:activate))})

(modes.add_binds :normal [[",b" "Fancy tab switch"
                           #($:set_mode "filter" :switch)]
                          [",h" "Fancy open history on new tab"
                           #($:set_mode "filter" :gohistory)]
                          [",B" "Fancy open bookmark on new tab"
                           #($:set_mode "filter" :gobookmark)]])

(modes.add_binds :filter
                 [["<Tab>" "Move the menu row focus downwards."
                   #($.menu:move_down)]
                  ["<S-Tab>" "Move the menu row focus upwards."
                   #($.menu:move_up)]
                  ["<C-n>" "Move the menu row focus downwards."
                   #($.menu:move_down)]
                  ["<C-p>" "Move the menu row focus upwards."
                   #($.menu:move_up)]])

;;; Without this space, binds_chrome.lua can't build :binds page

(fn switch [w]
  (if (w.menu:get)
      (w:goto_tab (. (w.menu:get) :format))
      (w:new_tab (get-input w.ibar.input.text))))

(fn go-newtab [w]
  (if (w.menu:get)
      (w:new_tab (. (w.menu:get) :format))
      (w:new_tab (get-input w.ibar.input.text))))

(modes.add_binds :command
                 [[::switch "Switch tabs" switch]
                  [::gohistory "Search history" go-newtab]
                  [::gobookmark "New tab on selected bookmark" go-newtab]])

;; Groups

(set _M.groups {})

(set _M.groups.tabs
     {:header {1 :Title 2 :URI :title true}
      :func (fn [ret w input]
              (each [index v (ipairs w.tabs.children)]
                (let [uri v.uri
                      title (or v.title uri)]
                  (when (or (uri:find input) (title:find input))
                    (table.insert ret {1 title
                                       2 uri
                                       :format index}))))
              ret)})

(set _M.groups.history
     {:header {1 :History 2 :URI :title true}
      :func (fn [ret w input]
              (let [sql "SELECT uri, title, lower(uri||ifnull(title,'')) AS text FROM history WHERE text LIKE ? ESCAPE '\\' ORDER BY visits DESC LIMIT 10"
                    rows (history.db:exec sql [(.. "%" input "%")])]
                (each [_ row (ipairs rows)]
                  (let [title (escape row.title)
                        uri (escape row.uri)]
                    (table.insert ret {1 (or title uri)
                                       2 uri
                                       :format row.uri})))
                ret))})

(set _M.groups.bookmarks
     {:header {1 :Bookmarks 2 :URI :title true}
      :func (fn [ret w input]
              (let [sql "SELECT uri, title, lower(uri||ifnull(title,'')||ifnull(tags,'')) AS text FROM bookmarks WHERE text LIKE ? ESCAPE '\\' ORDER BY title DESC LIMIT 10"
                    rows (bookmarks.db:exec sql [(.. "%" input "%")])]
                (each [_ row (ipairs rows)]
                  (let [title (escape row.title)
                        uri (escape row.uri)]
                    (table.insert ret {1 (or title uri)
                                       2 uri
                                       :format row.uri})))
                ret))})

(print :loaded-filter)

_M
