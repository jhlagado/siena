const wordsx = ["ab", "ad", "ag", "ah", "am", "an", "as", "at", "aw", "ax", "be", "bi", "by", "cc", "cd", "co", "do", "ed", "eh", "ex", "fm", "fx", "go", "ha", "he", "hi", "hm", "ho", "id", "if", "im", "in", "iq", "is", "it", "jr", "la", "lo", "ma", "me", "my", "no", "of", "oh", "ok", "on", "op", "or", "ow", "ox", "oy", "pa", "pi", "pm", "re", "so", "sr", "ta", "to", "tv", "uh", "um", "un", "up", "us", "uv", "vi", "vp", "we", "ya", "ye", "yo", "abet", "able", "ably", "abut", "aced", "aces", "ache", "achy", "acid", "acme", "acne", "acre", "acts", "adds", "adoo", "afar", "afro", "aged", "ages", "agog", "ahem", "ahoy", "aide", "ails", "aims", "aint", "airs", "airy", "ajar", "akin", "alas", "ally", "alms", "aloe", "also", "alto", "alum", "amen", "amid", "ammo", "amok", "amps", "anal", "anew", "ante", "anti", "ants", "apes", "apex", "apps", "aqua", "arch", "area", "argh", "arid", "arms", "army", "arts", "arty", "asap", "ashy", "asks", "atom", "atop", "aunt", "aura", "auto", "avid", "avow", "away", "awol", "awry", "axel", "axes", "axis", "axle", "babe", "baby", "bach", "back", "bade", "bags", "bail", "bain", "bait", "bake", "bald", "bale", "balk", "ball", "balm", "band", "bane", "bang", "bank", "barb", "bard", "bare", "barf", "bark", "barn", "bars", "base", "bash", "bask", "bass", "bath", "bats", "bays", "bead", "beak", "beam", "bean", "bear", "beat", "beau", "beck", "beds", "beef", "beek", "beep", "beer", "bees", "beet", "begs", "bell", "belt", "bend", "bent", "berm", "best", "beta", "bets", "bevy", "bias", "bide", "bids", "bike", "bile", "bilk", "bill", "bind", "bing", "bins", "bird", "bite", "bits", "blab", "blah", "bled", "blew", "blip", "blob", "bloc", "blot", "blow", "blue", "blur", "boar", "boat", "bode", "body", "bogs", "boil", "bold", "bole", "bolo", "bolt", "bomb", "bond", "bone", "bong", "bonk", "bony", "boob", "book", "boom", "boon", "boop", "boot", "bore", "born", "boss", "both", "bots", "bout", "bowl", "bows", "boys", "bozo", "brad", "brag", "bran", "bras", "brat", "bray", "bred", "brew", "brie", "brig", "brim", "brow", "buck", "buds", "buff", "bugs", "bulb", "bulk", "bull", "bump", "bums", "bunk", "buns", "bunt", "buoy", "burg", "burl", "burn", "burp", "burr", "bury", "bush", "busk", "bust", "busy", "buts", "butt", "buys", "buzz", "byes", "byte", "cabs", "cafe", "cage", "cake", "calf", "call", "calm", "came", "camp", "cams", "cane", "cans", "cant", "cape", "caps", "carb", "card", "care", "carp", "cars", "cart", "case", "cash", "cask", "cast", "cats", "cave", "cede", "cell", "cent", "cert", "chad", "chap", "char", "chat", "chef", "chew", "chic", "chin", "chip", "chop", "chow", "chug", "chum", "cite", "city", "clad", "clam", "clan", "clap", "claw", "clay", "clef", "clip", "clod", "clog", "clop", "clot", "club", "clue", "coal", "coat", "coax", "cock", "code", "coed", "coif", "coil", "coin", "coke", "cola", "cold", "colt", "coma", "comb", "come", "comp", "coms", "cone", "conk", "cons", "cook", "cool", "coop", "coot", "cope", "cops", "copy", "cord", "core", "cork", "corn", "corp", "cost", "cots", "coup", "cove", "cowl", "cows", "cozy", "crab", "crag", "cram", "cran", "crap", "craw", "cred", "crew", "crib", "crit", "croc", "crop", "crow", "crud", "crux", "cryo", "cube", "cubs", "cued", "cues", "cuff", "cull", "cult", "cups", "curb", "curd", "cure", "curl", "curt", "cusp", "cuss", "cute", "cuts", "cyst", "czar", "dads", "daft", "dais", "dale", "dame", "damn", "damp", "dang", "dank", "dare", "dark", "darn", "dart", "dash", "data", "date", "dawn", "days", "daze", "dead", "deaf", "deal", "dean", "dear", "debt", "deck", "deed", "deem", "deep", "deer", "deft", "defy", "deja", "deli", "dell", "delt", "demo", "dens", "dent", "deny", "dept", "derp", "desk", "deux", "dial", "dibs", "dice", "died", "dies", "diet", "digs", "dike", "dill", "dime", "dine", "ding", "dink", "dino", "dint", "dips", "dire", "dirt", "disc", "dish", "disk", "diss", "ditz", "diva", "dive", "dock", "docs", "dodo", "doer", "does", "doff", "dogs", "dojo", "dole", "doll", "dolt", "dome", "done", "dong", "dons", "dont", "doof", "doom", "door", "dope", "dork", "dorm", "dose", "dote", "doth", "dots", "dour", "dove", "down", "doze", "drab", "drag", "drat", "draw", "dreg", "drew", "drip", "drop", "drug", "drum", "dual", "duck", "duct", "dude", "duds", "duel", "dues", "duet", "duff", "duke", "dull", "duly", "dumb", "dump", "dune", "dung", "dunk", "dupe", "dusk", "dust", "duty", "dyad", "dyed", "each", "earl", "earn", "ears", "ease", "east", "easy", "eats", "echo", "ecru", "eddy", "edge", "edgy", "edit", "eels", "egad", "eggs", "egos", "elks", "elms", "else", "emit", "ends", "envy", "eons", "epee", "epic", "ergo", "etch", "even", "ever", "eves", "evil", "exam", "exec", "exes", "exit", "expo", "eyed", "eyes", "face", "fact", "fade", "fads", "fail", "fair", "fake", "fall", "fame", "fang", "fans", "fare", "farm", "fart", "fast", "fate", "faux", "fave", "fawn", "faze", "fear", "feat", "feed", "feel", "fees", "feet", "fell", "felt", "fend", "fern", "fest", "fete", "feud", "fiat", "fief", "fife", "figs", "file", "fill", "film", "find", "fine", "fink", "fins", "fire", "firm", "firs", "fish", "fist", "fits", "five", "fizz", "flag", "flak", "flan", "flap", "flat", "flaw", "flax", "flay", "flea", "fled", "flee", "flew", "flex", "flip", "flit", "flog", "flop", "flow", "flue", "flux", "flys", "foal", "foam", "foes", "foil", "fold", "folk", "fond", "font", "food", "fool", "foot", "ford", "fore", "fork", "form", "fort", "foul", "four", "fowl", "foxy", "frag", "frat", "fray", "free", "fret", "froe", "frog", "from", "fuel", "full", "fume", "fund", "funk", "furl", "furs", "fury", "fuse", "fuss", "fuze", "fuzz", "gaff", "gaga", "gags", "gain", "gait", "gala", "gale", "gall", "gals", "game", "gamy", "gang", "gape", "gaps", "garb", "gash", "gasp", "gate", "gave", "gawk", "gays", "gaze", "gear", "geek", "geld", "gels", "gelt", "gems", "gene", "gent", "germ", "gets", "gibe", "gift", "gigs", "gild", "gill", "gilt", "gimp", "gins", "gird", "girl", "gist", "give", "glad", "glam", "glee", "glen", "glib", "glop", "glow", "glue", "glum", "glut", "gnat", "gnaw", "goad", "goal", "goat", "gobs", "gods", "goer", "goes", "gogo", "gold", "golf", "gone", "gong", "good", "goof", "gook", "goon", "goop", "gore", "gory", "gosh", "goth", "gout", "gown", "grab", "grad", "gram", "gray", "grew", "grey", "grid", "grim", "grin", "grip", "grit", "grog", "grow", "grub", "guff", "gulf", "gull", "gulp", "gums", "gunk", "guns", "guru", "gush", "gust", "guts", "guys", "gyms", "gyro", "hack", "hags", "haha", "hail", "hair", "half", "hall", "halo", "halt", "hams", "hand", "hang", "hard", "hare", "hark", "harm", "harp", "hart", "hash", "hate", "hath", "hats", "haul", "have", "hawk", "haze", "hazy", "head", "heal", "heap", "hear", "heat", "heck", "heed", "heel", "heft", "heir", "held", "hell", "helm", "help", "hemp", "hens", "herb", "herd", "here", "hero", "hers", "hick", "hide", "high", "hike", "hill", "hilt", "hims", "hind", "hint", "hips", "hire", "hiss", "hits", "hive", "hoax", "hobo", "hock", "hoes", "hogs", "hoho", "hold", "hole", "holy", "home", "hone", "honk", "hood", "hoof", "hook", "hoop", "hoot", "hope", "hops", "horn", "hose", "host", "hots", "hour", "hove", "howl", "hows", "huff", "huge", "hugs", "hula", "hulk", "hull", "humm", "hump", "hums", "hung", "hunk", "hunt", "hurl", "hurt", "hush", "husk", "huts", "hymn", "hype", "iced", "icky", "icon", "idea", "ides", "idle", "idly", "idol", "iffy", "ills", "inch", "info", "inns", "into", "ions", "iota", "iris", "iron", "isle", "isnt", "itch", "item", "itsy", "itty", "jabs", "jack", "jade", "jags", "jail", "jamb", "jams", "jape", "jars", "jaws", "jays", "jazz", "jedi", "jeep", "jeer", "jeez", "jerk", "jest", "jets", "jibe", "jigs", "jilt", "jinx", "jive", "jobs", "jock", "joey", "jogs", "john", "join", "joke", "jolt", "jowl", "joys", "judo", "jugs", "juju", "juke", "july", "jump", "june", "junk", "jury", "just", "juts", "kale", "keel", "keen", "keep", "kegs", "kelp", "keno", "kept", "kern", "keys", "kick", "kids", "kill", "kiln", "kilt", "kind", "king", "kink", "kiss", "kite", "kits", "kiwi", "knee", "knew", "knit", "knob", "knot", "know", "koan", "kook", "labs", "lace", "lack", "lacy", "lads", "lady", "laid", "lain", "lair", "lake", "lamb", "lame", "lamp", "land", "lane", "lank", "laps", "lard", "lark", "lash", "lass", "last", "late", "laud", "lava", "lawn", "laws", "lays", "laze", "lazy", "lead", "leaf", "leak", "lean", "leap", "lede", "leek", "leer", "left", "lego", "legs", "lend", "lens", "lent", "less", "lest", "lets", "levy", "lewd", "liar", "lice", "lick", "lids", "lied", "lien", "lier", "lies", "lieu", "life", "lift", "like", "lilt", "lily", "limb", "lime", "limo", "limp", "line", "link", "lint", "lion", "lips", "lisp", "list", "lite", "live", "load", "loaf", "loam", "loan", "lobe", "loch", "lock", "loco", "lode", "loft", "logo", "logs", "loin", "lone", "long", "look", "loom", "loon", "loop", "loot", "lope", "lord", "lore", "lose", "loss", "lost", "lots", "loud", "lout", "love", "lows", "luau", "lube", "luck", "luge", "lull", "lump", "lung", "lure", "lurk", "lush", "lust", "lute", "luxe", "lynx", "lyre", "maam", "mace", "mach", "made", "mage", "magi", "mags", "maid", "mail", "maim", "main", "make", "male", "mall", "malt", "mame", "mane", "many", "maps", "mare", "mark", "mars", "mart", "mash", "mask", "mass", "mast", "mate", "math", "mats", "maul", "mayo", "mays", "maze", "mazy", "mead", "meal", "mean", "meat", "meds", "meek", "mega", "melt", "meme", "memo", "mend", "mens", "menu", "meow", "mere", "mesa", "mesh", "mess", "meta", "meth", "mice", "mild", "mile", "milk", "mill", "mime", "mind", "mine", "mini", "mink", "mint", "mire", "miry", "miss", "mist", "mite", "mitt", "moan", "moat", "mobs", "mock", "mode", "mojo", "mold", "mole", "molt", "moms", "monk", "mono", "mood", "mook", "moon", "moor", "moot", "mope", "mops", "more", "mosh", "moss", "most", "moth", "move", "mown", "much", "muck", "muff", "mugs", "mule", "mull", "muse", "mush", "musk", "must", "mute", "mutt", "myth", "naan", "nada", "nags", "nail", "name", "nano", "nape", "naps", "narc", "nary", "navy", "near", "neat", "neck", "need", "neon", "nerd", "nest", "nets", "news", "newt", "next", "nice", "nick", "nigh", "nine", "nips", "nite", "nits", "node", "nods", "noes", "noir", "none", "nook", "noon", "nope", "norm", "nose", "nosh", "nosy", "note", "noun", "nova", "nude", "nuke", "null", "numb", "nuns", "nuts", "oaks", "oars", "oath", "oats", "obey", "obit", "oboe", "odds", "odor", "ogle", "ogre", "oils", "oily", "oink", "okay", "okie", "okra", "olde", "omen", "omit", "once", "ones", "only", "onto", "onus", "onyx", "oops", "ooze", "opal", "open", "opus", "oral", "orbs", "orca", "ores", "orgy", "ouch", "ours", "oust", "outs", "oval", "oven", "over", "owed", "owes", "owls", "owns", "oxen", "pace", "pack", "pact", "pads", "page", "paid", "pail", "pain", "pair", "pale", "pall", "palm", "pals", "pane", "pang", "pans", "pant", "papa", "pare", "park", "part", "pass", "past", "pate", "path", "pats", "pave", "pawn", "paws", "pays", "peak", "peal", "pear", "peas", "peat", "peck", "peek", "peel", "peen", "peep", "peer", "pegs", "pelt", "pend", "pens", "pent", "peon", "perk", "perm", "perp", "pert", "perv", "pest", "pets", "pfft", "phew", "pick", "pied", "pier", "pies", "pigs", "pike", "pile", "pill", "pine", "ping", "pink", "pins", "pint", "pipe", "pips", "piss", "pith", "pits", "pity", "plan", "play", "plea", "pled", "plod", "plop", "plot", "plow", "ploy", "plug", "plum", "plus", "pock", "pods", "poem", "poet", "poke", "pole", "poll", "polo", "poly", "pomp", "pond", "pong", "pony", "poof", "pool", "poop", "poor", "pope", "pops", "pore", "pork", "porn", "port", "pose", "posh", "post", "posy", "pots", "pour", "pout", "pray", "prep", "prey", "prim", "prod", "prom", "prop", "pros", "psst", "puce", "puck", "puff", "puke", "pull", "pulp", "puma", "pump", "punk", "puns", "punt", "puny", "pupa", "pure", "purl", "purr", "push", "puss", "puts", "putt", "putz", "pyre", "pyro", "quad", "quay", "quip", "quit", "quiz", "race", "rack", "racy", "raff", "raft", "rage", "rags", "raid", "rail", "rain", "rake", "ramp", "rams", "rang", "rank", "rant", "raps", "rapt", "rare", "rash", "rasp", "rate", "rats", "rave", "rays", "raze", "razz", "read", "real", "ream", "reap", "rear", "reck", "redo", "reds", "reed", "reef", "reek", "reel", "rein", "rely", "rend", "rent", "reps", "rest", "ribs", "rice", "rich", "rick", "ride", "rife", "riff", "rift", "rigs", "rile", "rims", "rind", "ring", "rink", "riot", "ripe", "rips", "rise", "risk", "rite", "ritz", "rive", "road", "roam", "roan", "roar", "robe", "robs", "rock", "rode", "rods", "roil", "role", "rolf", "roll", "romp", "roof", "rook", "room", "root", "rope", "rose", "rosy", "rots", "rout", "rove", "rows", "rube", "rubs", "ruby", "rude", "ruff", "rugs", "ruin", "rule", "rump", "rune", "rung", "runs", "runt", "ruse", "rush", "rust", "ruts", "sack", "safe", "saga", "sage", "said", "sail", "sake", "sale", "salt", "same", "sand", "sane", "sang", "sank", "sans", "saps", "sash", "sass", "sate", "save", "saws", "says", "scab", "scam", "scan", "scar", "scat", "scud", "scum", "seal", "seam", "sear", "seas", "seat", "sect", "seed", "seek", "seem", "seen", "seep", "seer", "sees", "self", "sell", "semi", "send", "sent", "serf", "sets", "sewn", "sexy", "shad", "shag", "shah", "sham", "shed", "shes", "shim", "shin", "ship", "shiv", "shoe", "shoo", "shop", "shot", "show", "shun", "shut", "sick", "side", "sift", "sigh", "sign", "silk", "sill", "silo", "silt", "sine", "sing", "sink", "sins", "sips", "sire", "sirs", "site", "sits", "size", "skew", "skid", "skim", "skin", "skip", "skis", "skit", "slab", "slam", "slap", "slat", "slay", "sled", "slew", "slid", "slim", "slip", "slit", "slob", "slog", "slop", "slot", "slow", "slue", "slug", "slum", "slur", "smog", "smug", "smut", "snag", "snap", "snip", "snit", "snob", "snot", "snow", "snub", "snug", "soak", "soap", "soar", "sobs", "sock", "soda", "sofa", "soft", "soil", "sold", "sole", "solo", "some", "song", "sons", "soon", "soot", "sore", "sort", "soso", "soul", "soup", "sour", "sown", "spam", "span", "spar", "spas", "spat", "spay", "spaz", "spec", "sped", "spew", "spin", "spit", "spot", "spry", "spud", "spun", "spur", "stab", "stag", "star", "stat", "stay", "stem", "step", "stew", "stir", "stop", "stow", "stub", "stud", "stun", "stye", "subs", "such", "suck", "suds", "sued", "sues", "suit", "sulk", "sumo", "sump", "sums", "sung", "sunk", "suns", "sure", "surf", "suss", "swab", "swag", "swam", "swan", "swap", "swat", "sway", "swig", "swim", "swum", "sync", "tabs", "tach", "tack", "taco", "tact", "tada", "tags", "tail", "take", "talc", "tale", "talk", "tall", "tame", "tang", "tank", "tans", "tape", "taps", "tare", "tarp", "tart", "task", "taut", "taxi", "teak", "teal", "team", "tear", "teas", "teat", "tech", "teed", "teem", "teen", "tees", "tell", "temp", "tend", "tens", "tent", "term", "test", "text", "than", "that", "thaw", "thee", "them", "then", "they", "thin", "this", "thou", "thru", "thud", "thug", "tick", "tide", "tidy", "tied", "tier", "ties", "tiff", "tiki", "tile", "till", "tilt", "time", "tine", "ting", "tink", "tins", "tint", "tiny", "tips", "tire", "tits", "toad", "tock", "todo", "toed", "toes", "tofu", "toga", "toil", "toke", "told", "toll", "tomb", "tome", "tone", "tong", "tons", "took", "tool", "toon", "toot", "tops", "tore", "torn", "tort", "toss", "tote", "tots", "tout", "town", "toys", "tram", "trap", "tray", "tree", "trek", "trim", "trio", "trip", "trod", "trot", "true", "tuba", "tube", "tubs", "tuck", "tuft", "tuna", "tune", "turd", "turf", "turn", "tusk", "tutu", "twas", "twee", "twig", "twin", "twit", "twos", "tyke", "type", "typo", "uber", "ufos", "ugly", "undo", "unit", "unto", "upon", "urge", "urns", "used", "user", "uses", "vail", "vain", "vale", "vamp", "vane", "vans", "vary", "vase", "vast", "vats", "veal", "veep", "veer", "veil", "vein", "vend", "vent", "verb", "vert", "very", "vest", "veto", "vets", "vial", "vibe", "vice", "view", "vile", "vine", "visa", "vise", "void", "volt", "vote", "vows", "wack", "wade", "waft", "wage", "waif", "wail", "wait", "wake", "wale", "walk", "wall", "wand", "wane", "want", "ward", "ware", "warm", "warn", "warp", "wars", "wart", "wary", "wash", "wasp", "watt", "wave", "wavy", "waxy", "ways", "weak", "wean", "wear", "webs", "weds", "weed", "week", "weep", "weld", "well", "welt", "went", "wept", "were", "west", "west", "wets", "weve", "wham", "what", "whee", "when", "whet", "whew", "whey", "whim", "whip", "whiz", "whoa", "whom", "whop", "whys", "wick", "wide", "wife", "wigs", "wild", "wile", "will", "wilt", "wily", "wimp", "wind", "wine", "wing", "wink", "wins", "wipe", "wire", "wiry", "wise", "wish", "wisp", "with", "wits", "woah", "woes", "woke", "wolf", "womb", "wont", "wood", "woof", "wool", "woot", "word", "wore", "work", "worm", "worn", "wove", "wrap", "wren", "writ", "wuss", "xray", "yack", "yada", "yaks", "yall", "yams", "yang", "yank", "yard", "yarn", "yawn", "yeah", "year", "yell", "yelp", "yeti", "yipe", "yoga", "yoke", "yolk", "yore", "your", "yowl", "yuck", "yule", "yurt", "zany", "zeal", "zero", "zest", "zinc", "zing", "zion", "zits", "zone", "zoom", "zoos", "aah", "abs", "ace", "ack", "act", "add", "ado", "ads", "aft", "age", "aha", "ahh", "aid", "ail", "aim", "air", "aka", "ale", "all", "alt", "amp", "and", "ant", "any", "ape", "app", "apt", "arc", "are", "ark", "arm", "art", "ash", "ask", "asp", "ass", "ate", "ave", "awe", "awk", "awl", "aww", "axe", "aye", "bad", "bag", "bah", "bam", "ban", "bap", "bar", "bat", "bay", "bbq", "bed", "bee", "beg", "bet", "bib", "bid", "big", "bin", "bio", "bit", "biz", "bob", "bod", "bog", "bon", "boo", "bop", "bot", "bow", "box", "boy", "bra", "bro", "bub", "bud", "bug", "bum", "bun", "bur", "bus", "but", "buy", "bye", "cab", "cad", "cam", "can", "cap", "car", "cat", "cay", "cob", "cod", "cog", "com", "con", "coo", "cop", "cot", "cow", "coy", "cry", "cub", "cud", "cue", "cul", "cup", "cut", "dab", "dad", "dam", "day", "den", "dew", "dib", "did", "die", "dig", "dim", "din", "dip", "dir", "dis", "doc", "doe", "dog", "doh", "don", "dot", "dry", "dub", "dud", "due", "dug", "duh", "duo", "dye", "ear", "eat", "ebb", "eek", "eel", "egg", "ego", "eke", "elf", "elk", "elm", "emu", "end", "eon", "era", "ere", "err", "eve", "ewe", "eww", "eye", "fab", "fad", "fan", "far", "fat", "fax", "fed", "fee", "few", "fib", "fig", "fin", "fir", "fit", "fix", "flu", "fly", "fog", "for", "fox", "fro", "fry", "fun", "fur", "gab", "gag", "gah", "gal", "gap", "gas", "gay", "gee", "gel", "gem", "get", "gig", "gin", "gnu", "gob", "god", "goo", "got", "gum", "gun", "gut", "guy", "gym", "had", "hag", "ham", "has", "hat", "haw", "hay", "hee", "heh", "hem", "hen", "her", "hes", "hew", "hex", "hey", "hid", "him", "hip", "his", "hit", "hmm", "hog", "hoo", "hop", "hot", "how", "hub", "hue", "hug", "huh", "hum", "hun", "hut", "ice", "ick", "icy", "ide", "ilk", "ill", "imp", "ink", "inn", "ins", "ion", "ire", "irk", "its", "ive", "ivy", "jab", "jag", "jam", "jar", "jaw", "jay", "jet", "jib", "jig", "job", "joe", "jog", "jot", "joy", "jug", "jut", "keg", "key", "kid", "kin", "kit", "koi", "lab", "lad", "lag", "lam", "lap", "law", "lax", "lay", "led", "leg", "let", "lid", "lie", "lil", "lip", "lit", "lob", "log", "lol", "lop", "lot", "low", "lug", "lux", "lye", "mad", "mag", "man", "map", "mar", "mat", "mav", "max", "may", "med", "meh", "met", "mic", "mid", "min", "mit", "mix", "mob", "mod", "mom", "moo", "mop", "mow", "mud", "mug", "mum", "nab", "nag", "nah", "nap", "naw", "nay", "neg", "net", "new", "nil", "nip", "nit", "nix", "nod", "nog", "nor", "not", "now", "nub", "nun", "nut", "oaf", "oak", "oar", "oat", "odd", "ode", "off", "oft", "oil", "old", "ole", "one", "oof", "ops", "opt", "orb", "orc", "ore", "org", "our", "out", "owe", "owl", "own", "pad", "pal", "pan", "par", "pat", "paw", "pay", "pea", "peg", "pen", "pep", "per", "pet", "pew", "pic", "pie", "pig", "pin", "pip", "pit", "ply", "pod", "poo", "pop", "pot", "pow", "pro", "pry", "pub", "pug", "pun", "pup", "put", "quo", "rad", "rag", "rah", "ram", "ran", "rap", "rat", "raw", "ray", "red", "ref", "rep", "rib", "rid", "rig", "rim", "rip", "rob", "rod", "rot", "row", "rub", "rue", "rug", "rum", "run", "rut", "rye", "sad", "sag", "sap", "sat", "saw", "sax", "say", "sea", "see", "set", "sew", "sex", "she", "shh", "shy", "sim", "sin", "sip", "sir", "sis", "sit", "six", "ska", "ski", "sky", "sly", "sob", "sod", "son", "sos", "sot", "sow", "soy", "spa", "spy", "sub", "sue", "sum", "sun", "sup", "tab", "tac", "tad", "tag", "tan", "tap", "tar", "tat", "tax", "tea", "tee", "ten", "the", "thy", "tic", "tie", "til", "tin", "tip", "tis", "tit", "toe", "ton", "too", "top", "tot", "tow", "toy", "try", "tub", "tug", "tux", "tvs", "two", "ufo", "ugh", "uhh", "uhm", "umm", "ump", "urn", "use", "uzi", "van", "vat", "veg", "vet", "vex", "via", "vid", "vie", "vip", "vow", "wad", "wag", "war", "was", "wax", "way", "wed", "wee", "wet", "who", "why", "wig", "win", "wit", "wiz", "woe", "won", "woo", "wow", "wry", "wtf", "yah", "yak", "yam", "yap", "yaw", "yay", "yea", "yep", "yes", "yet", "yip", "yon", "you", "yuk", "yum", "yup", "zag", "zap", "zen", "zig", "zip", "zit", "zoo"];

const words3 = ["aah", "abs", "ace", "ack", "act", "add", "ado", "ads", "aft", "age", "aha", "ahh", "aid", "ail", "aim", "air", "aka", "ale", "all", "alt", "amp", "and", "ant", "any", "ape", "app", "apt", "arc", "are", "ark", "arm", "art", "ash", "ask", "asp", "ass", "ate", "ave", "awe", "awk", "awl", "aww", "axe", "aye", "bad", "bag", "bah", "bam", "ban", "bap", "bar", "bat", "bay", "bbq", "bed", "bee", "beg", "bet", "bib", "bid", "big", "bin", "bio", "bit", "biz", "bob", "bod", "bog", "bon", "boo", "bop", "bot", "bow", "box", "boy", "bra", "bro", "bub", "bud", "bug", "bum", "bun", "bur", "bus", "but", "buy", "bye", "cab", "cad", "cam", "can", "cap", "car", "cat", "cay", "cob", "cod", "cog", "com", "con", "coo", "cop", "cot", "cow", "coy", "cry", "cub", "cud", "cue", "cul", "cup", "cut", "dab", "dad", "dam", "day", "den", "dew", "dib", "did", "die", "dig", "dim", "din", "dip", "dir", "dis", "doc", "doe", "dog", "doh", "don", "dot", "dry", "dub", "dud", "due", "dug", "duh", "duo", "dye", "ear", "eat", "ebb", "eek", "eel", "egg", "ego", "eke", "elf", "elk", "elm", "emu", "end", "eon", "era", "ere", "err", "eve", "ewe", "eww", "eye", "fab", "fad", "fan", "far", "fat", "fax", "fed", "fee", "few", "fib", "fig", "fin", "fir", "fit", "fix", "flu", "fly", "fog", "for", "fox", "fro", "fry", "fun", "fur", "gab", "gag", "gah", "gal", "gap", "gas", "gay", "gee", "gel", "gem", "get", "gig", "gin", "gnu", "gob", "god", "goo", "got", "gum", "gun", "gut", "guy", "gym", "had", "hag", "ham", "has", "hat", "haw", "hay", "hee", "heh", "hem", "hen", "her", "hes", "hew", "hex", "hey", "hid", "him", "hip", "his", "hit", "hmm", "hog", "hoo", "hop", "hot", "how", "hub", "hue", "hug", "huh", "hum", "hun", "hut", "ice", "ick", "icy", "ide", "ilk", "ill", "imp", "ink", "inn", "ins", "ion", "ire", "irk", "its", "ive", "ivy", "jab", "jag", "jam", "jar", "jaw", "jay", "jet", "jib", "jig", "job", "joe", "jog", "jot", "joy", "jug", "jut", "keg", "key", "kid", "kin", "kit", "koi", "lab", "lad", "lag", "lam", "lap", "law", "lax", "lay", "led", "leg", "let", "lid", "lie", "lil", "lip", "lit", "lob", "log", "lol", "lop", "lot", "low", "lug", "lux", "lye", "mad", "mag", "man", "map", "mar", "mat", "mav", "max", "may", "med", "meh", "met", "mic", "mid", "min", "mit", "mix", "mob", "mod", "mom", "moo", "mop", "mow", "mud", "mug", "mum", "nab", "nag", "nah", "nap", "naw", "nay", "neg", "net", "new", "nil", "nip", "nit", "nix", "nod", "nog", "nor", "not", "now", "nub", "nun", "nut", "oaf", "oak", "oar", "oat", "odd", "ode", "off", "oft", "oil", "old", "ole", "one", "oof", "ops", "opt", "orb", "orc", "ore", "org", "our", "out", "owe", "owl", "own", "pad", "pal", "pan", "par", "pat", "paw", "pay", "pea", "peg", "pen", "pep", "per", "pet", "pew", "pic", "pie", "pig", "pin", "pip", "pit", "ply", "pod", "poo", "pop", "pot", "pow", "pro", "pry", "pub", "pug", "pun", "pup", "put", "quo", "rad", "rag", "rah", "ram", "ran", "rap", "rat", "raw", "ray", "red", "ref", "rep", "rib", "rid", "rig", "rim", "rip", "rob", "rod", "rot", "row", "rub", "rue", "rug", "rum", "run", "rut", "rye", "sad", "sag", "sap", "sat", "saw", "sax", "say", "sea", "see", "set", "sew", "sex", "she", "shh", "shy", "sim", "sin", "sip", "sir", "sis", "sit", "six", "ska", "ski", "sky", "sly", "sob", "sod", "son", "sos", "sot", "sow", "soy", "spa", "spy", "sub", "sue", "sum", "sun", "sup", "tab", "tac", "tad", "tag", "tan", "tap", "tar", "tat", "tax", "tea", "tee", "ten", "the", "thy", "tic", "tie", "til", "tin", "tip", "tis", "tit", "toe", "ton", "too", "top", "tot", "tow", "toy", "try", "tub", "tug", "tux", "tvs", "two", "ufo", "ugh", "uhh", "uhm", "umm", "ump", "urn", "use", "uzi", "van", "vat", "veg", "vet", "vex", "via", "vid", "vie", "vip", "vow", "wad", "wag", "war", "was", "wax", "way", "wed", "wee", "wet", "who", "why", "wig", "win", "wit", "wiz", "woe", "won", "woo", "wow", "wry", "wtf", "yah", "yak", "yam", "yap", "yaw", "yay", "yea", "yep", "yes", "yet", "yip", "yon", "you", "yuk", "yum", "yup", "zag", "zap", "zen", "zig", "zip", "zit", "zoo"];

const words0 = [
    'add',
    'and',
    'bytes',
    'case',
    'def',
    'div',
    'exec',
    'eq',
    'filter',
    'get',
    'gt',
    'in',
    'inv',
    'ife',
    'if',
    'key',
    'let',
    'lt',
    'map',
    'neg',
    'or',
    'print',
    'scan',
    'set',
    'shift',
    'sub',
    'switch',
    'while',
    'words',
    'xor'
];

const code0 = '0'.charCodeAt(0);
const code9 = '9'.charCodeAt(0);
const codeA = 'A'.charCodeAt(0);
const codeZ = 'Z'.charCodeAt(0);
const codea = 'a'.charCodeAt(0);
const codez = 'z'.charCodeAt(0);
const code_ = '_'.charCodeAt(0);

const pearsonSize = 64

const getPearson = () => Array(pearsonSize).fill(0).map((item, index) => index).sort(() => Math.random() - 0.5);

const pearson1 = [46, 7, 26, 16, 20, 32, 38, 42, 40, 55, 56, 4, 0, 1, 5, 41, 27, 51, 2, 48, 59, 3, 9, 49, 19, 39, 29, 45, 13, 10, 33, 8, 34, 63, 12, 52, 18, 23, 24, 14, 61, 31, 50, 30, 36, 15, 44, 60, 47, 25, 28, 43, 21, 53, 6, 37, 54, 17, 62, 35, 57, 22, 11, 58] // 115

const unused = 0xff; // hi byte cannot be 0xff
const tableSize = 0x100; // must be a power of 2

type TableEntry = [string, number, number];
const table: TableEntry[] = Array(tableSize).fill(null);
const buckets: number[] = Array(tableSize).fill(unused);
let maxMissed = 0;
let wordCount = 0;
let missed = 0;
let totalMissedBy = 0;
let collisions = 0;
let rejections = 0;
let avgMissSize = 0;

const getCode = (char: string) => {
    let code = char.charCodeAt(0);
    if (code >= codea) code = code - 6;
    if (code >= codeA) code = code - 7;
    code = code - code0;
    return code;
}

const getChar = (code: number) => {
    code = code + code0;
    if (code > code9) code = code + 7;
    if (code > codeZ) code = code + 6;
    let char = String.fromCharCode(code);
    return char;
}

// for (let i=0;i<pearsonSize;i++){
//     console.log(i, getChar(i), pearson[i]);
// }

const hash = (s: string) => {
    let hash = 0;
    for (var c of s) {
        let code = c.charCodeAt(0);
        hash = (hash << 5 + hash) ^ code;
    }
    return hash;
}

const hash2 = (pearson: number[], s: string, offset: number) => {
    let hash = 0;
    for (var c of s) {
        let code = (getCode(c) + offset) % pearsonSize;
        const x = hash ^ code;
        hash = pearson[x];
        if (hash == null) console.log('hash', s, x, hash, c, code)
    }
    return hash;
}

const addEntry2 = (pearson: number[], key: string) => {
    let hi = hash2(pearson, key, 1);
    const lo = (hash2(pearson, key, 0)) * 2;
    let lo1 = lo;
    while (buckets[lo1] !== unused) {
        if (buckets[lo1] === hi) {
            collisions++
            return unused
        }
        lo1 = (lo1 + 1) % 256;
        // if (lo === lo1) {
        //     rejections++;
        //     return unused
        // };
    };
    wordCount++;
    let missedby = lo1 - lo;
    if (missedby < 0) missedby += tableSize;
    if (missedby > 0) missed++;
    totalMissedBy += missedby;
    if (missedby > maxMissed) maxMissed = missedby;
    buckets[lo1] = hi;
    table[lo1] = [key, lo, lo1];
    avgMissSize = Math.floor(totalMissedBy / wordCount)
    return lo1;
}


const lookupEntry = (s: string, prime: number) => {
    let hi = hash2(pearson, s, 1);
    const lo = (hash2(pearson, s, 0)) * 2;
    if (buckets[lo] === unused) return unused;
    let lo1 = lo;
    while (buckets[lo1] !== hi) {
        lo1 = (lo1 + 1) % 0xff;
        if (lo === lo1) return unused;
    }
    return table[lo];
}

const tableInit2 = (pearson: number[], wordList: string[]) => {
    for (const w of wordList) {
        const index = addEntry2(pearson, w);
        if (index === unused) break;
    }
}

const run2 = (pearson: number[], title: string, words: string[], mt: number, wt: number) => {
    maxMissed = 0;
    wordCount = 0;
    missed = 0;
    totalMissedBy = 0;
    collisions = 0;
    rejections = 0;
    table.fill(['', 0, 0]);
    buckets.fill(unused);
    tableInit2(pearson, words);
}

const genName = () => {
    const num = Math.floor(Math.random() * 5) + 1;
    let s = '';
    for (let i = 0; i < num; i++) {
        // const code = Math.floor(Math.random() * 26 + 36)
        const code = Math.floor(Math.random() * 62)
        s += String.fromCharCode(code + codea);
    }
    return s;
}

const genwords = () => {
    const words = new Set<string>();
    for (let i = 1; i < 0x100; i++) {
        words.add(genName());
    }
    return [...words];
}

let bestPearson = getPearson();
let bestWordCount = 0;
let bestAvgMissSize = 1000;

let pearson = getPearson();
for (let i = 0; i < 1000; i++) {
    pearson = getPearson();
    for (let i = 0; i < 10; i++) {
        const words = genwords();
        run2(pearson, 'words', words, 3, 0);
        if (wordCount > bestWordCount && avgMissSize <= bestAvgMissSize) {
            bestWordCount = wordCount;
            bestAvgMissSize = avgMissSize;
            bestPearson = pearson;
            console.log('NEW2!', bestWordCount, bestAvgMissSize, JSON.stringify(pearson), missed);
        }
    }
}
for (let i = 0; i < 10; i++) {
    run2(pearson1, 'genwords()', genwords(), 3, 0);
    console.log(wordCount, avgMissSize);
}
run2(pearson1, 'wordsx', wordsx, 3, 0);
console.log(wordCount, avgMissSize);

const h = hash2(pearson1,'Hello1',1)
console.log({h});
