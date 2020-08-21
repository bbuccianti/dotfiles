(local lousy (require :lousy))
(local modes (require :modes))

(local view (require :fennelview))

(local pp (fn [x] (print (view x)))) ; convenience helper
(local escape lousy.util.escape)

(local _M {})
(local data (setmetatable {} { :__mode :k}))

(fn make-candidates [w buf]
  (let [cmd (string.match buf ":(%a+) ")
        input (or (string.match buf ":%a+ (%a+)") "")
        group (match cmd
                :switch :tabs
                :gohistory :history)
        ret {}]
    (table.insert ret (. _M.groups group :header))
    ((. _M.groups group :func) ret w buf)))

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
                          [",gh" "Fancy history navigator"
                           #($:set_mode "filter" :gohistory)]])

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
      ;; If candidate exist, switch to tab
      (w:goto_tab (. (w.menu:get) :format))
      ;; Otherwise, navigate to input in new tab
      (w:new_tab (string.match w.ibar.input.text ":%a+ (%a*)"))))

(modes.add_binds :command
                 [[::switch "Switch tabs"
                   {:func switch :groups :tabs}]
                  [::gohistory "Search history"
                   {:func #(pp "Search history")}]])

;; Same space thing

;; Groups

(set _M.groups {})

(set _M.groups.tabs
     {:header {1 :Title 2 :URI :title true}
      :func (fn [ret w buf]
              (let [input (or (string.match buf ":%a+ (%a+)") "")]
                (each [index v (ipairs w.tabs.children)]
                  (let [uri v.uri
                        title (or v.title uri)]
                    (when (or (uri:find input) (title:find input))
                      (table.insert ret {1 title
                                         2 uri
                                         :format index}))))
                ret))})


(print :loaded-filter)

_M
