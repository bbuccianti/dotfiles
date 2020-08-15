-- just enough to bootstrap fennel!
local fennel = require("fennel")
fennel.path = luakit.config_dir .. "/?.fnl"
table.insert(package.loaders, fennel.searcher)
require("browser")

-- follow hint labels using letters
local select = require "select"

select.label_maker = function ()
   return trim(sort(reverse(charset("aoeuidhtns"))))
end
