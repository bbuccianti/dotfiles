local downloads = require "downloads"
local _select = require "select"
local follow = require "follow"

-- Set downloads default dir
downloads.default_dir = os.getenv("HOME") .. "/downloads"

-- Use letters for hint labels
_select.label_maker = function ()
   return trim(sort(reverse(charset("aeouidhtns"))))
end

-- Match only hint label text
follow.pattern_maker = follow.pattern_styles.match_label

-- Bigger follow hint label font size
follow.stylesheet = follow.stylesheet .. [[
#luakit_select_overlay .hint_label {
  font-size: 15px !important;
}
]]
