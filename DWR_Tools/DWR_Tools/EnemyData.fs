module EnemyData

let ENEMY_DATA = [|
    // name                    XP  GOLD  STR  AGI   HP
    "slime",                    1,    2,   5,   3,   2   // 0
    "redslime",                 2,    4,   7,   3,   3
    "drakee",                   3,    6,   9,   6,   5
    "ghost",                    4,    8,  11,   8,   7
    "magician",                 8,   16,  11,  12,  12
    "magidrakee",              12,   20,  14,  14,  13
    "scorpion",                16,   25,  18,  16,  13
    "druin",                   14,   21,  20,  18,  22
    "poltergeist",             15,   19,  18,  20,  23
    "droll",                   18,   30,  22,  24,  20
    "drakeema",                20,   25,  24,  26,  16  // 10
    "skeleton",                25,   42,  28,  22,  24
    "warlock",                 28,   50,  28,  22,  28
    "metalscorpion",           31,   48,  36,  42,  18
    "wolf",                    40,   60,  40,  30,  33
    "wraith",                  42,   62,  44,  34,  39
    "metalslime",             255,    6,  10, 255,   3
    "specter",                 47,   75,  40,  38,  33
    "wolflord",                52,   80,  50,  36,  37
    "druinlord",               58,   95,  47,  40,  35
    // group 2
    "drollmagi",               58,  110,  52,  50,  44  // 20
    "wyvern",                  64,  105,  56,  48,  37
    "roguescorpion",           70,  110,  60,  90,  40
    "wraithknight",            72,  120,  68,  56,  40
    "golem",                  255,   10, 120,  60, 153
    "goldman",                  6,  255,  48,  40,  35
    "knight",                  78,  150,  76,  78,  47
    "magiwyvern",              83,  135,  78,  68,  48
    "demonknight",             90,  148,  79,  64,  38
    "werewolf",                95,  155,  86,  70,  70
    // group 3
    "greendragon",            135,  160,  88,  74,  72  // 30
    "starwyvern",             105,  169,  86,  80,  74
    "wizard",                 120,  185,  80,  70,  65
    "axeknight",              130,  165,  94,  82,  67
    "bluedragon",             180,  150,  98,  84,  98
    // group 4
    "stoneman",               155,  148, 100,  40, 135
    "armoredknight",          172,  152, 105,  86,  99
    "reddragon",              255,  143, 120,  90, 106
    "dragonlord1",              0,    0,  90,  75, 100
    "dragonlord2",              0,    0, 140, 200, 165  // 39
    |]

let ENEMY_NAME(i) = let name, _xp, _gold,_str,_agi,_hp = ENEMY_DATA.[i] in name
let XP   = dict [| for name, xp, _gold,_str,_agi,_hp in ENEMY_DATA do yield name,xp |]
let GOLD = dict [| for name, _xp, gold,_str,_agi,_hp in ENEMY_DATA do yield name,gold |]
let STR  = dict [| for name, _xp, _gold,str,_agi,_hp in ENEMY_DATA do yield name,str |]
let AGI  = dict [| for name, _xp, _gold,_str,agi,_hp in ENEMY_DATA do yield name,agi |]
let HP   = dict [| for name, _xp, _gold,_str,_agi,hp in ENEMY_DATA do yield name,hp |]

let crop(bmp:System.Drawing.Bitmap, w:int, h:int, ulx, uly) =
    let targetBmp = new System.Drawing.Bitmap(w, h)
    use g = System.Drawing.Graphics.FromImage(targetBmp)
    g.DrawImage(bmp, new System.Drawing.Rectangle(0, 0, w, h), 
                     new System.Drawing.Rectangle(ulx, uly, w, h),                        
                     System.Drawing.GraphicsUnit.Pixel)
    targetBmp

let ENEMY_BMP = 
    let a = ResizeArray()
    for e,_,_,_,_,_ in ENEMY_DATA do
        if e <> "demonknight" then // TODO how to match him, all black
            try
                // TODO where file resources load from
                let imageStream = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream(e+".png")
                let bmp = new System.Drawing.Bitmap(imageStream)
                //let bmp = new System.Drawing.Bitmap(System.IO.Path.Combine("""C:\Users\Admin1\Source\Repos\Misc\DragonWarriorRandomizerDisplay\DragonWarriorRandomizerDisplay\bin\Debug""",e+".png"))
                let crop = crop(bmp, 140, 150, 346, 290)
                a.Add(e, bmp, crop)
            with e ->
                ()
    a.ToArray()

let computeMatch(bmp1:System.Drawing.Bitmap, bmp2:System.Drawing.Bitmap, x1, x2, y1, y2) =
    let mutable n = 0
    for x = x1 to x2 do
        for y = y1 to y2 do
            if bmp1.GetPixel(x,y) = bmp2.GetPixel(x,y) then
                n <- n + 1
    float n / (float ((x2-x1+1)*(y2-y1+1)))

let bestMatch(screen:System.Drawing.Bitmap) =
    let matches = ResizeArray()
    for name,bmp,crop in ENEMY_BMP do
        if name.Contains("drakee") then
            let r = computeMatch(bmp, screen, 402, 426, 342, 364)
            if r > 0.85 then
                matches.Add(r,name,bmp,crop)
        elif name.Contains("skeleton") || name.Contains("wraith") || name.Contains("knight") || name.Contains("wyvern") || name.Contains("druin") || name.Contains("stoneman") || name.Contains("golem") || name.Contains("goldman") then
            let r = computeMatch(bmp, screen, 402, 426, 360, 380)
            if r > 0.85 then
                matches.Add(r,name,bmp,crop)
        else
            let r = computeMatch(bmp, screen, 402, 426, 384, 396)
            if r > 0.85 then
                matches.Add(r,name,bmp,crop)
    if matches.Count > 0 then
        for r,name,_,_ in matches do
            ()
//            printfn "%1.3f  %s" r name
//        printfn "-----"
    matches.Sort()
    matches.Reverse()
    matches

