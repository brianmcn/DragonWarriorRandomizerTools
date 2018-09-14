// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.Windows
open System.Windows.Controls 
open System.Windows.Media
open System.Windows.Interop 

// TODO now that can align tiles, can 'see' the half tiles at edge of screen, incorporate into map
// TODO need 'tp home' button, also need buttons that don't cause fceux popups

// TODO add death counter? (can i auto-recognize? HP 0 in upper left maybe)
// TODO add spell tracker? (see spell menu)
// TODO add AP/DP/STR/AGI tracker (when that screen pops up?)
// TODO exp level time splits?

let ENABLE_MAP = true

//////////////////////////////////////////////////

open System.Runtime.InteropServices 
module Winterop = 
    [<DllImport("User32.dll")>]
    extern bool RegisterHotKey(IntPtr hWnd,int id,uint32 fsModifiers,uint32 vk)

    [<DllImport("User32.dll")>]
    extern bool UnregisterHotKey(IntPtr hWnd,int id)

    let HOTKEY_ID = 9000

    [<DllImport("user32.dll")>]
    extern IntPtr GetForegroundWindow()

    [<StructLayout(LayoutKind.Sequential)>]
    [<Struct>]
    type RECT =
         val Left : int
         val Top : int
         val Right : int
         val Bottom : int

    [<DllImport("user32.dll", SetLastError = true)>]
    extern [<MarshalAs(UnmanagedType.Bool)>] bool GetWindowRect(IntPtr hWnd, RECT* lpRect)

//////////////////////////////////////////////////

type FastRGBBitmap(orig:System.Drawing.Bitmap) =
    let imageData = orig.LockBits(new System.Drawing.Rectangle(0, 0, orig.Width, orig.Height), System.Drawing.Imaging.ImageLockMode.ReadWrite, System.Drawing.Imaging.PixelFormat.Format24bppRgb)
    let bytes = Array.create (imageData.Stride * orig.Height) 0uy
    do
        System.Runtime.InteropServices.Marshal.Copy(imageData.Scan0, bytes, 0, bytes.Length)
    member private this.Bytes = bytes
    member private this.ImageData = imageData
    member this.GetR(x,y) = bytes.[imageData.Stride*y + x*3 + 2]
    member this.GetG(x,y) = bytes.[imageData.Stride*y + x*3 + 1]
    member this.GetB(x,y) = bytes.[imageData.Stride*y + x*3 + 0]
    member this.SetRGB(x,y,r,g,b) = 
        bytes.[imageData.Stride*y + x*3 + 0] <- b
        bytes.[imageData.Stride*y + x*3 + 1] <- g
        bytes.[imageData.Stride*y + x*3 + 2] <- r
    member this.Equals(x,y,argb) = 
        let r = byte((argb &&& 0x00FF0000) >>> 16)
        let g = byte((argb &&& 0x0000FF00) >>> 8)
        let b = byte((argb &&& 0x000000FF))
        let r1 = bytes.[imageData.Stride*y + x*3 + 2]
        let g1 = bytes.[imageData.Stride*y + x*3 + 1]
        let b1 = bytes.[imageData.Stride*y + x*3 + 0]
        //if r <> 0uy then
        //    ignore(r,g,b,r1,g1,b1)
        r=r1 && g=g1 && b=b1
    member this.Equals(x,y,other:FastRGBBitmap,ox,oy) = 
        let r1 = bytes.[imageData.Stride*y + x*3 + 2]
        let g1 = bytes.[imageData.Stride*y + x*3 + 1]
        let b1 = bytes.[imageData.Stride*y + x*3 + 0]
        let r = other.Bytes.[other.ImageData.Stride*oy + ox*3 + 2]
        let g = other.Bytes.[other.ImageData.Stride*oy + ox*3 + 1]
        let b = other.Bytes.[other.ImageData.Stride*oy + ox*3 + 0]
        //if r <> 0uy then
        //    ignore(r,g,b,r1,g1,b1)
        r=r1 && g=g1 && b=b1
    member this.Finish() = 
        System.Runtime.InteropServices.Marshal.Copy(bytes, 0, imageData.Scan0, bytes.Length)
        orig.UnlockBits(imageData)

//////////////////////////////////////////////////

let OVERWORLD_MAP_TILE_FILENAMES = 
    [|
    "ow_bridge.png"
    "ow_castle.png"
    "ow_cave.png"
    "ow_desert.png"
    "ow_forest.png"
    "ow_hills.png"
    "ow_mountain.png"
    "ow_plains.png"
    "ow_swamp.png"
    "ow_town.png"
    "ow_wall.png"
    "ow_water.png"
    "ow_water_e.png"
    "ow_water_es.png"
    "ow_water_n.png"
    "ow_water_ne.png"
    "ow_water_nes.png"
    "ow_water_ns.png"
    "ow_water_nw.png"
    "ow_water_nwe.png"
    "ow_water_nwes.png"
    "ow_water_nws.png"
    "ow_water_s.png"
    "ow_water_w.png"
    "ow_water_we.png"
    "ow_water_wes.png"
    "ow_water_ws.png"
    |]

module Screenshot =
    let RedToWhiteColor(c:System.Drawing.Color) =
        if c.R = byte 0xFC && c.G = byte 0x74 && c.B = byte 0x60 then // red
            System.Drawing.Color.FromArgb(0xFC, 0xFC, 0xFC)   // white
        else
            c
    let RedToWhiteBmp(bmp:System.Drawing.Bitmap) =
        // Dragon Warrior paints things white, except when your health is low, the white all turns to red.
        // Turn all those red to white to be able to do exact pixel matching regardless of current health.
        let clone = bmp.Clone() :?> System.Drawing.Bitmap
        let fast = FastRGBBitmap(clone)
        for x = 0 to clone.Width-1 do
            for y = 0 to clone.Height-1 do
                if fast.GetR(x,y) = byte 0xFC && fast.GetG(x,y) = byte 0x74 && fast.GetB(x,y) = byte 0x60 then // red
                    fast.SetRGB(x, y, 0xFCuy, 0xFCuy, 0xFCuy)   // white
        fast.Finish()
        (*
        for x = 0 to clone.Width-1 do
            for y = 0 to clone.Height-1 do
                let c = clone.GetPixel(x,y)
                if c.R = byte 0xFC && c.G = byte 0x74 && c.B = byte 0x60 then // red
                    clone.SetPixel(x, y, System.Drawing.Color.FromArgb(0xFC, 0xFC, 0xFC))   // white
        *)
        clone
    let mutable badTileNum = 1
    let UniqueOverworldTiles = ResizeArray<string*FastRGBBitmap>()
    let BMPtoImage(bmp:System.Drawing.Bitmap) =
        let ms = new System.IO.MemoryStream()
        bmp.Save(ms, System.Drawing.Imaging.ImageFormat.Bmp)
        let bmimage = new System.Windows.Media.Imaging.BitmapImage()
        bmimage.BeginInit()
        ms.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore
        bmimage.StreamSource <- ms
        bmimage.EndInit()
        bmimage
    let GetDWRBitmap() =
        let bmpScreenshot = new System.Drawing.Bitmap(System.Windows.Forms.Screen.PrimaryScreen.Bounds.Width, System.Windows.Forms.Screen.PrimaryScreen.Bounds.Height, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
        let gfxScreenshot = System.Drawing.Graphics.FromImage(bmpScreenshot)
        let hWnd_DragonWarrior = Winterop.GetForegroundWindow()
        let mutable rect = Winterop.RECT()
        let b = Winterop.GetWindowRect(hWnd_DragonWarrior, &&rect)
        //printfn "%A %d %d %d %d" b rect.Left rect.Top rect.Right rect.Bottom 
        (*
        gfxScreenshot.CopyFromScreen(System.Windows.Forms.Screen.PrimaryScreen.Bounds.X, System.Windows.Forms.Screen.PrimaryScreen.Bounds.Y,
                                    0, 0,
                                    System.Windows.Forms.Screen.PrimaryScreen.Bounds.Size,
                                    System.Drawing.CopyPixelOperation.SourceCopy)
        *)
        gfxScreenshot.CopyFromScreen(rect.Left, rect.Top,
                                    0, 0,
                                    System.Drawing.Size(rect.Right-rect.Left+1,rect.Bottom-rect.Top+1),
                                    System.Drawing.CopyPixelOperation.SourceCopy)
        bmpScreenshot
    let GetInnerDWRBitmaps() =
        let outerBmp = GetDWRBitmap()
        let innerWidth  = 744
        let innerHeight = 672
        let innerBmp = outerBmp.Clone(new System.Drawing.Rectangle(8+24, 51, innerWidth, innerHeight), System.Drawing.Imaging.PixelFormat.Format32bppArgb)
        let innerBmp = RedToWhiteBmp(innerBmp)
        let fastInnerBmp = new FastRGBBitmap(innerBmp.Clone() :?> System.Drawing.Bitmap)
        //innerBmp.Save("Crop024.png")
        let getDownscaledPixel(x,y) =
            innerBmp.GetPixel(x*3,y*3)
        let matchesKnownTile(ulx, uly) =
            let mutable result = false
            for tilename,tile in UniqueOverworldTiles do
                if not result then
                    let mutable matchesThis = true
                    for i = 0 to 15 do
                        if matchesThis then
                            for j = 0 to 15 do
                                if matchesThis then
//                                    if getDownscaledPixel(ulx+i, uly+j).ToArgb() <> tile.GetPixel(i,j).ToArgb() then
//                                    if not(tile.Equals(i,j,getDownscaledPixel(ulx+i, uly+j).ToArgb())) then
                                    if not(tile.Equals(i,j,fastInnerBmp,3*(ulx+i),3*(uly+j))) then
                                        matchesThis <- false
                    if matchesThis then
                        result <- true
            result
        //TODO perf
        let mutable leftX = 0
        let mutable topY = 0
        let mutable ok = false
        let THRESHOLD = 2  // TODO set at higher number to save new world tile, if failing to sync because legal tile not in our resource set
        let doesThisLeftTopWork(lx,ty) = 
            // see if vast majority match known tiles
            let bad = ResizeArray()  // each mismatched tile stored here
            if bad.Count < THRESHOLD then
                for by = 0 to 12 do //in [0;1;2;3;4;9;10] do      // 0-12 is all, but just get enough to feel mostly confident we've synchronized
                    if bad.Count < THRESHOLD then
                        for bx = 0 to 13 do //in [1;2;3;4;11;12] do // 0-13 is all, but just get enough to feel mostly confident we've synchronized
                            // hero can cover up these map squares, must skip
                            if  bx = 6 && by = 6 ||
                                bx = 7 && by = 5 ||
                                bx = 7 && by = 6 ||
                                bx = 7 && by = 7 ||
                                bx = 8 && by = 6 then
                                () // do nothing
                            else
                                // check for match
                                let ulx = bx*16+lx
                                let uly = by*16+ty
                                if not(matchesKnownTile(ulx, uly)) then
                                    bad.Add( (ulx,uly) )
            if bad.Count < THRESHOLD then
                printfn "pixel sync succeeded with %d outliers" bad.Count 
                // save outliers to human verify and add to unique map tiles
                for ulx,uly in bad do
                    let ot = new System.Drawing.Bitmap(16,16)
                    for i = 0 to 15 do
                        for j = 0 to 15 do
                            ot.SetPixel(i,j,getDownscaledPixel(ulx+i,uly+j))
                    ot.Save(sprintf "BadTile%06d.png" badTileNum)
                    badTileNum <- badTileNum + 1
                badTileNum <- ((badTileNum + 100) / 100) * 100
                true
            else
                false
        // for each possible pixel alignment
        for lx = 0 to 15 do
            if not ok then
                for ty = 0 to 15 do
                    if not ok then
                        if doesThisLeftTopWork(lx,ty) then
                            ok <- true
                            leftX <- lx
                            topY <- ty
        if not ok then
            printfn "pixel sync failed"
            fastInnerBmp.Finish()
            ResizeArray()
        else
            let makeTiny(leftX, topY) = 
                // turn each 16x16 block of the overworld map down to a single pixel
                let tinyBmp = new System.Drawing.Bitmap(14,13)
                for x = 0 to 13 do
                    for y = 0 to 12 do
                        let mutable r, g, b = 0, 0, 0
                        for i = 0 to 15 do
                            for j = 0 to 15 do
                                let c = getDownscaledPixel(leftX+x*16+i, topY+y*16+j)
                                r <- r + int c.R
                                g <- g + int c.G
                                b <- b + int c.B
                        tinyBmp.SetPixel(x, y, System.Drawing.Color.FromArgb(r/256, g/256, b/256))
                // tinyBmp.Save("Tiny028.png")
                tinyBmp
            let results = ResizeArray()
            results.Add(makeTiny(leftX, topY))
            // we can mistakenly sync on half tiles, as some tiles like swamp/desert/grass repeat every 8 pixels of their 16x16 grids - ensure find 'real' sync
            if leftX < 8 then
                if topY < 8 then
                    if doesThisLeftTopWork(leftX+8, topY+8) then
                        printfn "extra result +8,+8"
                        results.Add(makeTiny(leftX+8, topY+8))
                if doesThisLeftTopWork(leftX+8, topY) then
                    printfn "extra result +8,+0"
                    results.Add(makeTiny(leftX+8, topY))
            if topY < 8 then
                if doesThisLeftTopWork(leftX, topY+8) then
                    printfn "extra result +0,+8"
                    results.Add(makeTiny(leftX, topY+8))
            fastInnerBmp.Finish()
            results


type Mapper() =
    let EXPLORED_MAP_BORDER_THICKNESS = 2
    let mutable exploredMapImageWidth = 0
    let UNKNOWN = System.Drawing.Color.Magenta 
    let UARGB = UNKNOWN.ToArgb()
    let MAX = 400
    let W = 14
    let H = 13
    let wholeMap = new System.Drawing.Bitmap(MAX,MAX)
    let mutable lowULX = MAX/2
    let mutable lowULY = MAX/2
    let mutable curULX = MAX/2
    let mutable curULY = MAX/2
    let mutable hiULX = MAX/2
    let mutable hiULY = MAX/2
    let mutable hasStarted = false
    member this.HasStarted = hasStarted
    member private this.Mask(bmp:System.Drawing.Bitmap) =
        if bmp.Width <> W || bmp.Height <> H then
            failwith "bad bmp to mask"
        let bmp = bmp.Clone() :?> System.Drawing.Bitmap
        // hero can cover up these map squares
        bmp.SetPixel(6,6,UNKNOWN)
        bmp.SetPixel(7,5,UNKNOWN)
        bmp.SetPixel(7,6,UNKNOWN)
        bmp.SetPixel(7,7,UNKNOWN)
        bmp.SetPixel(8,6,UNKNOWN)
        // TODO issue with bottom row? pressing f10 to start causes fceux to pup up display on bottom row // TODO can probably avoid better moving up to start?
        for x = 0 to W-1 do
            bmp.SetPixel(x,H-1,UNKNOWN)
        bmp
    member private this.PaintHere(bmp:System.Drawing.Bitmap) =
        for x = 0 to W-1 do
            for y = 0 to H-1 do
                let c = bmp.GetPixel(x,y)
                if c.ToArgb() <> UARGB then
                    let oldc = wholeMap.GetPixel(curULX+x, curULY+y).ToArgb()
                    if (oldc <> UARGB) && (oldc <> c.ToArgb()) then
                        failwith "bad painting"
                    wholeMap.SetPixel(curULX+x, curULY+y, c)
    member this.StartFromScratch(bmp:System.Drawing.Bitmap) =
        // erase map
        for x = 0 to MAX-1 do
            for y = 0 to MAX-1 do
                wholeMap.SetPixel(x, y, UNKNOWN)
        // set position
        curULX <- MAX/2
        curULY <- MAX/2
        // paint it
        let bmp = this.Mask(bmp)
        this.PaintHere(bmp)
        hasStarted <- true
    member private this.ExactMatch(dx, dy, bmp:System.Drawing.Bitmap) =
        let mutable ok = true
        for x = 0 to W-1 do
            if ok then
                for y = 0 to H-1 do
                    if ok then
                        let c1 = bmp.GetPixel(x,y).ToArgb()
                        let x = curULX + dx + x
                        let y = curULY + dy + y
                        let c2 = wholeMap.GetPixel(x,y).ToArgb()
                        if (c1 <> c2) && (c1 <> UARGB) && (c2 <> UARGB) then
                            ok <- false
        ok
    member this.TryIncrementalPaint(bmps:ResizeArray<System.Drawing.Bitmap>) =
        let mutable found = false
        for bmp in bmps do
            if this.TryIncrementalPaint(bmp) then
                if found then
                    printfn "FOUND TWO PAINTS, this is bad and will be hard to recover from"
                found <- true
    member private this.TryIncrementalPaint(bmp:System.Drawing.Bitmap) =
        let mutable ok = false
        let bmp = this.Mask(bmp)
        for dx = -5 to 5 do
            if not ok then
                for dy = -5 to 5 do
                    if not ok && (abs dx + abs dy) < 6 then
                        if this.ExactMatch(dx, dy, bmp) then
                            curULX <- curULX + dx
                            curULY <- curULY + dy
                            this.PaintHere(bmp)
                            printfn "...curx,cury now is %d,%d..." curULX curULY
                            ok <- true
                            lowULX <- min lowULX curULX
                            lowULY <- min lowULY curULY
                            hiULX <- max hiULX curULX
                            hiULY <- max hiULY curULY
        ok
    member this.GetWholeMap() = 
        wholeMap.Clone() :?> System.Drawing.Bitmap 
    member this.GetNearbyMap(size) = 
        let ulx = curULX + W/2 - size/2
        let uly = curULY + H/2 - size/2
        let cloned = wholeMap.Clone(new System.Drawing.Rectangle(ulx, uly, size, size), System.Drawing.Imaging.PixelFormat.Format32bppArgb)
        cloned.SetPixel(size/2, size/2, System.Drawing.Color.Red)
        cloned
    member private this.ComputeExploreMapSize() =
        let N = EXPLORED_MAP_BORDER_THICKNESS
        // explored size
        let ew = hiULX-lowULX+W
        let eh = hiULY-lowULY+H
        let m = max ew eh
        let m = max m 50  // ensure don't begin ridiculously zoomed in
        m+2*N
    member this.GetExploredMap(width) = 
        exploredMapImageWidth <- width
        let N = EXPLORED_MAP_BORDER_THICKNESS
        let m = this.ComputeExploreMapSize()
        let cloned = wholeMap.Clone(new System.Drawing.Rectangle(lowULX-N, lowULY-N, m, m), System.Drawing.Imaging.PixelFormat.Format32bppArgb)
        cloned
    member this.ResetCurrentLocation(x,y) =
        // user clicked at x,y relative to upper left of the GetExploredMap image, want to update curULX/curULX appropriately
        let N = EXPLORED_MAP_BORDER_THICKNESS
        let M = this.ComputeExploreMapSize()
        let E = exploredMapImageWidth
        // we drew an image that is MxM pixels on a window panel that is ExE
        // the user clicked at x,y on the ExE map
        // where is this on MxM scale?
        let ex = x * M / E
        let ey = y * M / E
        printfn "ex, ey = %d,%d" ex ey
        printfn "cur x, y = %d,%d" curULX curULY
        curULX <- lowULX - N + ex - W/2
        curULY <- lowULY - N + ey - H/2
        printfn "new cur x, y = %d,%d" curULX curULY

// randomizer xp levels are 75% the normal NES game
let DWR_XP_LEVEL_THRESHOLDS = [|
    5
    17
    35
    82
    165
    337
    600
    975
    1500
    2175
    3000
    4125
    5625
    7500
    9750
    12000
    14250
    16500
    19500
    |]

let ENEMY_DATA = [|
    // name                    XP  GOLD  STR  AGI   HP
    "slime",                    1,    2,   5,   3,   2
    "redslime",                 2,    4,   7,   3,   3
    "drakee",                   3,    6,   9,   6,   5
    "ghost",                    4,    8,  11,   8,   7
    "magician",                 8,   16,  11,  12,  12
    "magidrakee",              12,   20,  14,  14,  13
    "scorpion",                16,   25,  18,  16,  13
    "druin",                   14,   21,  20,  18,  22
    "poltergeist",             15,   19,  18,  20,  23
    "droll",                   18,   30,  22,  24,  20
    "drakeema",                20,   25,  24,  26,  16
    "skeleton",                25,   42,  28,  22,  24
    "warlock",                 28,   50,  28,  22,  28
    "metalscorpion",           31,   48,  36,  42,  18
    "wolf",                    40,   60,  40,  30,  33
    "wraith",                  42,   62,  44,  34,  39
    "metalslime",             255,    6,  10, 255,   3
    "specter",                 47,   75,  40,  38,  33
    "wolflord",                52,   80,  50,  36,  37
    "druinlord",               58,   95,  47,  40,  35
    "drollmagi",               58,  110,  52,  50,  44
    "wyvern",                  64,  105,  56,  48,  37
    "roguescorpion",           70,  110,  60,  90,  40
    "wraithknight",            72,  120,  68,  56,  40
    "golem",                  255,   10, 120,  60, 153
    "goldman",                  6,  255,  48,  40,  35
    "knight",                  78,  150,  76,  78,  47
    "magiwyvern",              83,  135,  78,  68,  48
    "demonknight",             90,  148,  79,  64,  38
    "werewolf",                95,  155,  86,  70,  70
    "greendragon",            135,  160,  88,  74,  72
    "starwyvern",             105,  169,  86,  80,  74
    "wizard",                 120,  185,  80,  70,  65
    "axeknight",              130,  165,  94,  82,  67
    "bluedragon",             180,  150,  98,  84,  98
    "stoneman",               155,  148, 100,  40, 135
    "armoredknight",          172,  152, 105,  86,  99
    "reddragon",              255,  143, 120,  90, 106
    "dragonlord1",              0,    0,  90,  75, 100
    "dragonlord2",              0,    0, 140, 200, 165
    |]

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

let LOCATIONS = [|
    "---Tantagel (4box, cave)", ""
    "Charlock Castle", "DW_Charlock.png"
    "Brecconary (Motel 6)", ""
    "Rimuldar (keys)", ""
    "---Rimuldar (1box)", ""
    "Cantlin (coordinates)", ""
    "Kol (fountain)", ""
    "Hauksness (dead)", ""
    "Garinham (grave below)", ""
    "---Garinham (3box)", ""
    "Sun Stones Cave (v)", ""
    "Staff Rain Cave (>)", ""
    "Jerk Cave (<)", ""
    "Swamp Cave North", "DW_SwampCave.png"
    "Swamp Cave South", "DW_SwampCave.png"
    "Mountain Cave (5box)", "DW_MountainCave.png"
    "Tablet Cave (1box)", "DW_TabletCave.png"
    "Garin's Tomb (3box)", "DW_GarinTomb.png"
    "---Garin's Tomb (2box)", "DW_GarinTomb.png"
    |]

let ITEMS = [|
    "Stones of Sunlight", ""
    "Silver Harp", ""
    "Staff of Rain", ""
    "Erdrick Token (*)", ""
    "Rainbow Drop", ""
    "Erdrick Sword", ""
    "Erdrick Armor (*)", ""
    "Fairy Flute (*)", ""
    "Death Necklace", ""
    "Princess' Love", ""
    |]

let gridAdd(g:Grid, x, c, r) =
    g.Children.Add(x) |> ignore
    Grid.SetColumn(x, c)
    Grid.SetRow(x, r)

type MyWindow(ihrs,imins,isecs) as this = 
    inherit Window()
    let mapper = new Mapper()
    let mutable startTime = DateTime.Now + TimeSpan.FromSeconds(0.0)
    let image1 = new Image()
    let image2 = new Image()
    let monsterImage = new Image()
    let tab = new TabControl(Background=Brushes.Black)
    let mutable ssNum = 1
    let mutable prevMatchName = ""
    let onCheckedChanged(resource) =
        if resource <> "" then
            let imageStream = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
            image1.Source <- System.Windows.Media.Imaging.BitmapFrame.Create(imageStream)
        else
            image1.Source <- null
    let makeCheckedStuff(labelStr,strs) = 
        let sp = new StackPanel(Background=Brushes.Black)
        sp.Background<-Brushes.Black
        let label = new Label()
        label.Content <- new TextBox(Text=labelStr,FontSize=16.0,Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0))
        sp.Children.Add(label) |> ignore
        for s,res in strs do
            let cb = new CheckBox(Content=new TextBox(Text=s,FontSize=16.0,Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0)))
            cb.Checked.Add(fun _ -> onCheckedChanged(res))
            cb.Unchecked.Add(fun _ -> onCheckedChanged(""))
            sp.Children.Add(cb) |> ignore
        sp
    let content = new Grid()
    let pretty = DWR_XP_LEVEL_THRESHOLDS |> Array.map (fun x -> let s = x.ToString() in (String.replicate (5-s.Length) " ") + s)
    let xpTextBox = new TextBox(Text="XP to\nlevel\n"+String.Join("\n",pretty),FontSize=20.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0))
    let goalTextBox = new TextBox(Text="-GOAL-\nAG>75\nHP>100\nMP>80\nAP>120\nDP>86",FontSize=20.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0))
    let hmsTimeTextBox = new TextBox(Text="timer",FontSize=20.0,Background=Brushes.Black,Foreground=Brushes.LightGreen,BorderThickness=Thickness(0.0))
    //let locationTextBox = new TextBox(Text=String.Join("\n",LOCATIONS),FontSize=20.0,Background=Brushes.Black,Foreground=Brushes.Orange)
    //let itemTextBox = new TextBox(Text=String.Join("\n",ITEMS),FontSize=20.0,Background=Brushes.Black,Foreground=Brushes.Orange)
    let monsterName = new TextBox(Text="name",FontSize=20.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0))
    let monsterXP = new TextBox(Text="EXP",FontSize=20.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0))
    let monsterGold = new TextBox(Text="GOLD",FontSize=20.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0))
    let monsterSTR = new TextBox(Text="STR",FontSize=20.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0))
    let monsterAGI = new TextBox(Text="AGI",FontSize=20.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0))
    let monsterHP = new TextBox(Text="HP",FontSize=20.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0))
    let day1Start = 60*24 + 8*60 + 15
    let mutable source = null
    let stackPanel = new StackPanel(Background=Brushes.Black)
    let update() =
        let timer = System.Diagnostics.Stopwatch.StartNew()
        // update time
        let ts = DateTime.Now - startTime
        let h,m,s = ts.Hours, ts.Minutes, ts.Seconds
        hmsTimeTextBox.Text <- sprintf "%02d:%02d:%02d" h m s
        // auto screenshot
        if false then
            let bmpScreenshot = Screenshot.GetDWRBitmap()
            bmpScreenshot.Save(sprintf "Auto%03d.png" ssNum, System.Drawing.Imaging.ImageFormat.Png)
            ssNum <- ssNum + 1
        // update map
        if ENABLE_MAP && mapper.HasStarted then
            let innerBMPs = Screenshot.GetInnerDWRBitmaps()
            if innerBMPs.Count > 0 then
                mapper.TryIncrementalPaint(innerBMPs)
                //printfn "width: %f %f %f" stackPanel.ActualWidth image1.ActualWidth image2.ActualWidth 
                let width = int stackPanel.ActualWidth - 8  // image width given border/thickness
                image1.Source <- Screenshot.BMPtoImage(mapper.GetNearbyMap(width/4))  // TODO decide ratio
                image2.Source <- Screenshot.BMPtoImage(mapper.GetExploredMap(width))
        // update monster TODO
        let bmpScreenshot = Screenshot.GetDWRBitmap()
        let matches = bestMatch(bmpScreenshot)
        if matches.Count > 0 then
            // TODO erase if goes away for 2 frames, only show monster portrait area, eventually add stats
            let _,name,_bmp,crop = matches.[0]
            if prevMatchName = "1" || prevMatchName = "" then
                prevMatchName <- name
                tab.SelectedIndex <- 1
                monsterImage.Source <- Screenshot.BMPtoImage(crop)
                monsterName.Text <- name
                monsterXP.Text   <- sprintf "XP:   %d" XP.[name]
                monsterGold.Text <- sprintf "GOLD: %d" GOLD.[name]
                monsterSTR.Text  <- sprintf "STR:  %d" STR.[name]
                monsterAGI.Text  <- sprintf "AGI:  %d" AGI.[name]
                monsterHP.Text   <- sprintf "HP:   %d" HP.[name]
            elif prevMatchName <> name then
                //printfn "changed from %s to %s" prevMatchName name
                prevMatchName <- "1"   // e.g. once saw wyvern change to magician - flashing screen screwed it up? give it one tempo to fix
            // TODO eventually check for stats screen
        elif prevMatchName = "1" then
            prevMatchName <- ""
            tab.SelectedIndex <- 0
        elif prevMatchName <> "" then
            //printfn "one tick no match"
            prevMatchName <- "1"
        //printfn "update took %d ms" timer.ElapsedMilliseconds 

    //let activate() =
    //    this.Activate() |> ignore
    do
        RenderOptions.SetBitmapScalingMode(image1, BitmapScalingMode.NearestNeighbor)
        RenderOptions.SetBitmapScalingMode(image2, BitmapScalingMode.NearestNeighbor)

        content.ColumnDefinitions.Add(new ColumnDefinition(Width=GridLength(76.0)))
        content.ColumnDefinitions.Add(new ColumnDefinition(Width=GridLength(200.0)))
        content.ColumnDefinitions.Add(new ColumnDefinition())
        content.RowDefinitions.Add(new RowDefinition())

        // left grid
        let leftGrid = new Grid()
        leftGrid.ColumnDefinitions.Add(new ColumnDefinition())
        leftGrid.RowDefinitions.Add(new RowDefinition(Height=GridLength(76.0)))
        leftGrid.RowDefinitions.Add(new RowDefinition(Height=GridLength(474.0)))
        leftGrid.RowDefinitions.Add(new RowDefinition())
        let kitty = new Image()
        let imageStream = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream("CroppedBrianKitty.png")
        kitty.Source <- System.Windows.Media.Imaging.BitmapFrame.Create(imageStream)
        gridAdd(leftGrid,kitty,0,0)
        gridAdd(leftGrid,xpTextBox,0,1)
        gridAdd(leftGrid,goalTextBox,0,2)
        gridAdd(content,leftGrid,0,0)
        // right grid
        let rightGrid = new Grid()
        rightGrid.ColumnDefinitions.Add(new ColumnDefinition())
        rightGrid.RowDefinitions.Add(new RowDefinition(Height=GridLength(24.0)))
        gridAdd(rightGrid,hmsTimeTextBox,0,0)

        rightGrid.RowDefinitions.Add(new RowDefinition(Height=GridLength(416.0)))
        let locationTextBox = makeCheckedStuff("LOCATIONS",LOCATIONS)
        gridAdd(rightGrid,locationTextBox,0,1)

        rightGrid.RowDefinitions.Add(new RowDefinition())
        let itemTextBox = makeCheckedStuff("ITEMS",ITEMS)
        gridAdd(rightGrid,itemTextBox,0,2)

        // add right grid
        gridAdd(content,rightGrid,1,0)

        // picture area
        let maps = new TabItem(Background=Brushes.Black, Header="Maps")
        let monsters = new TabItem(Background=Brushes.Black, Header="Monsters")
        tab.Items.Add(maps) |> ignore
        tab.Items.Add(monsters) |> ignore
        let sp = new StackPanel(Background=Brushes.Black,Orientation=Orientation.Vertical)
        sp.Children.Add(image1) |> ignore
        sp.Children.Add(new TextBox(Text="nearby world",FontSize=16.0,Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(2.0))) |> ignore
        image1.Margin <- Thickness(1.0)
        image2.Margin <- Thickness(1.0)
        image2.MouseLeftButtonDown.Add(fun x -> 
            let point = x.GetPosition(image2) 
            mapper.ResetCurrentLocation(int point.X, int point.Y)
            ())
        sp.Children.Add(image2) |> ignore
        sp.Children.Add(new TextBox(Text="all explored",FontSize=16.0,Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(2.0))) |> ignore
        maps.Content <- sp

        let monsterGrid = new Grid()
        monsterGrid.ColumnDefinitions.Add(new ColumnDefinition())
        monsterGrid.RowDefinitions.Add(new RowDefinition())
        monsterGrid.RowDefinitions.Add(new RowDefinition(Height=GridLength(24.0)))
        monsterGrid.RowDefinitions.Add(new RowDefinition(Height=GridLength(24.0)))
        monsterGrid.RowDefinitions.Add(new RowDefinition(Height=GridLength(24.0)))
        monsterGrid.RowDefinitions.Add(new RowDefinition(Height=GridLength(24.0)))
        monsterGrid.RowDefinitions.Add(new RowDefinition(Height=GridLength(24.0)))
        monsterGrid.RowDefinitions.Add(new RowDefinition(Height=GridLength(24.0)))
        monsters.Content <- monsterGrid
        gridAdd(monsterGrid,monsterImage,0,0)
        gridAdd(monsterGrid,monsterName,0,1)
        gridAdd(monsterGrid,monsterXP,0,2)
        gridAdd(monsterGrid,monsterGold,0,3)
        gridAdd(monsterGrid,monsterSTR,0,4)
        gridAdd(monsterGrid,monsterAGI,0,5)
        gridAdd(monsterGrid,monsterHP,0,6)

        tab.SelectedIndex <- 0
        stackPanel.Children.Add(tab) |> ignore
        gridAdd(content,stackPanel,2,0)

        // full window
        this.Title <- "Dragon Warrior Randomizer"
        this.Content <- content
        this.Width <- 1280.0 - 720.0
        this.Height <- 720.0
        //this.SizeToContent <- SizeToContent.Height
        this.WindowStartupLocation <- WindowStartupLocation.Manual
        this.Left <- 1300.0
        this.Top <- 20.0

        let timer = new System.Windows.Threading.DispatcherTimer()
        timer.Interval <- TimeSpan.FromSeconds(if ENABLE_MAP then 0.5 (*0.25*) else 1.0)  // TODO decide time
        timer.Tick.Add(fun _ -> update())
        timer.Start()

        //this.Topmost <- true
        //this.Deactivated.Add(fun _ -> activate())

    //////////////////////////////////////////////////
    // global hotkey

    let VK_F4 =  0x73
    let VK_F9 =  0x78
    let VK_F10 = 0x79
    //let MOD_CTRL = uint32 0x0002
    let MOD_NONE = 0u

    override this.OnSourceInitialized(e) =
        base.OnSourceInitialized(e)
        let helper = new WindowInteropHelper(this)
        source <- HwndSource.FromHwnd(helper.Handle)
        source.AddHook(HwndSourceHook(fun a b c d e -> this.HwndHook(a,b,c,d,&e)))
        this.RegisterHotKey()

    override this.OnClosed(e) =
        source.RemoveHook(HwndSourceHook(fun a b c d e -> this.HwndHook(a,b,c,d,&e)))
        source <- null
        this.UnregisterHotKey()
        base.OnClosed(e)

    member this.RegisterHotKey() =
        let helper = new WindowInteropHelper(this);
        if(not(Winterop.RegisterHotKey(helper.Handle, Winterop.HOTKEY_ID, MOD_NONE, uint32 VK_F10))) then
            // handle error
            ()
        if(not(Winterop.RegisterHotKey(helper.Handle, Winterop.HOTKEY_ID, MOD_NONE, uint32 VK_F9))) then
            // handle error
            ()
        if(not(Winterop.RegisterHotKey(helper.Handle, Winterop.HOTKEY_ID, MOD_NONE, uint32 VK_F4))) then
            // handle error
            ()

    member this.UnregisterHotKey() =
        let helper = new WindowInteropHelper(this)
        Winterop.UnregisterHotKey(helper.Handle, Winterop.HOTKEY_ID) |> ignore

    member this.HwndHook(hwnd:IntPtr, msg:int, wParam:IntPtr, lParam:IntPtr, handled:byref<bool>) : IntPtr =
        let WM_HOTKEY = 0x0312
        if msg = WM_HOTKEY then
            if wParam.ToInt32() = Winterop.HOTKEY_ID then
                //let ctrl_bits = lParam.ToInt32() &&& 0xF  // see WM_HOTKEY docs
                let key = lParam.ToInt32() >>> 16
                if key = VK_F10 then
                    printfn "reset time"
                    startTime <- DateTime.Now - System.TimeSpan.FromHours(float ihrs) - System.TimeSpan.FromMinutes(float imins) - System.TimeSpan.FromSeconds(float isecs)
                elif key = VK_F9 then
                    printfn "reset map"
                    if ENABLE_MAP then
                        let bmps = Screenshot.GetInnerDWRBitmaps()
                        if bmps.Count = 1 then
                            mapper.StartFromScratch(bmps.[0])
                        else
                            printfn "FAILED TO START, count was %d" bmps.Count 
                elif key = VK_F4 then
                    let bmpScreenshot = Screenshot.GetDWRBitmap()
                    bmpScreenshot.Save(sprintf "Screen%03d.png" ssNum, System.Drawing.Imaging.ImageFormat.Png)
                    ssNum <- ssNum + 1
                handled <- true
        IntPtr.Zero


[<STAThread>]
[<EntryPoint>]
let main argv = 
    let app = new Application()
    (*
    printfn "Init hours?"
    let hrs  = System.Console.ReadLine() |> int
    printfn "Init mins?"
    let mins = System.Console.ReadLine() |> int
    printfn "Init secs?"
    let secs = System.Console.ReadLine() |> int
    app.Run(MyWindow(hrs,mins,secs)) |> ignore
    *)
    let myAssembly = System.Reflection.Assembly.GetExecutingAssembly()
    let names = myAssembly.GetManifestResourceNames()
    for n in names do
        printfn "%s" n

    // load up known overworld map tiles
    for ow in OVERWORLD_MAP_TILE_FILENAMES do
        let imageStream = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream(ow)
        let bmp = new System.Drawing.Bitmap(imageStream)
        Screenshot.UniqueOverworldTiles.Add(ow,new FastRGBBitmap(bmp))
    (*
    for file in System.IO.Directory.EnumerateFiles("""C:\Users\Admin1\Source\Repos\Misc\DragonWarriorRandomizerDisplay\DragonWarriorRandomizerDisplay\bin\Debug\""", "ow_*.png") do
        let bmp = System.Drawing.Bitmap.FromFile(file) :?> System.Drawing.Bitmap
        Screenshot.UniqueOverworldTiles.Add(bmp)
    *)
    printfn "loaded %d overworld tiles" Screenshot.UniqueOverworldTiles.Count 

    (*
    let tiles = ResizeArray<System.Drawing.Bitmap>()
    for file in System.IO.Directory.EnumerateFiles("""C:\Users\Admin1\Source\Repos\Misc\DragonWarriorRandomizerDisplay\DragonWarriorRandomizerDisplay\bin\Debug\""", "?Tile???.png") do
        let bmp = System.Drawing.Bitmap.FromFile(file) :?> System.Drawing.Bitmap
        let mutable unique = true
        for tile in tiles do
            if unique then
                let mutable differ = false
                for x = 0 to 15 do
                    if not differ then
                        for y = 0 to 15 do
                            if not differ then
                                if tile.GetPixel(x,y).ToArgb() <> bmp.GetPixel(x,y).ToArgb() then
                                    differ <- true
                if not differ then
                    unique <- false
        if unique then
            tiles.Add(bmp)
    printfn "# tiles = %d" tiles.Count
    let mutable i = 0
    for tile in tiles do
        tile.Save(sprintf "OWMap%03d.png" i)
        i <- i + 1
    *)
        
    (*
    let bmp = new System.Drawing.Bitmap(System.IO.Path.Combine("""C:\Users\Admin1\Source\Repos\Misc\DragonWarriorRandomizerDisplay\DragonWarriorRandomizerDisplay\bin\Debug\roguescorpion.png"""))
    let screen = new System.Drawing.Bitmap(System.IO.Path.Combine("""C:\Users\Admin1\Source\Repos\Misc\DragonWarriorRandomizerDisplay\DragonWarriorRandomizerDisplay\bin\Debug\Screen001.png"""))
    let r = computeMatch(bmp, screen, 402, 426, 380, 396)
    printfn "%f" r
    let r = computeMatch(bmp, screen, 402, 426, 384, 396)
    printfn "%f" r
    *)

    app.Run(MyWindow(0,0,0)) |> ignore
    0
