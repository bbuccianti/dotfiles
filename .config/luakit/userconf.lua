local downloads = require "downloads"
local _select = require "select"
local follow = require "follow"

-- Set downloads default dir
downloads.default_dir = os.getenv("HOME") .. "/downloads"

-- Use letters for hint labels
_select.label_maker = function ()
   local chars = charset("aeouidhtns")
   return trim(sort(reverse(chars)))
end

-- Match only hint label text
follow.pattern_maker = follow.pattern_styles.match_label
