open System
open System.Windows
open System.Windows.Controls 
open System.Windows.Media
open System.Windows.Interop 

// TODO way to deal with 'reset' without crashing (e.g. xp decreasing)

// TODO maybe a checklist of 'explored branch' on each location, to help keep track of remain ways from locations? hmmm

// TODO add AP/DP/STR/AGI tracker (when that screen pops up?) also note weapon/armor/etc
//   - could capture the level when it was taken, and always know current level, so could show how 'stale' the info was that way

// TODO rainbow drop changes map, can no longer sync charlock
// TODO consider adding 15x15 enemy zone map overlay thingy, when certain found a map edge
// TODO maybe, if click town checkbox, add it to map at that location, highlight with crosshairs when name is clicked?

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

    [<DllImport("user32.dll")>]
    extern int SendMessage(IntPtr hWnd, int wMsg, IntPtr wParam, IntPtr lParam)

    [<DllImport ("User32.dll")>]
    extern int SetForegroundWindow(IntPtr hWnd)

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

let UNKNOWN = System.Drawing.Color.Magenta 

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
    let UniqueOverworldTiles = ResizeArray<string*Constants.OverworldMapTile*FastRGBBitmap>()
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
        gfxScreenshot.CopyFromScreen(rect.Left, rect.Top, 0, 0,
                                     System.Drawing.Size(rect.Right-rect.Left+1,rect.Bottom-rect.Top+1),
                                     System.Drawing.CopyPixelOperation.SourceCopy)
        bmpScreenshot
    let GetInnerDWRBitmaps() =
        let outerBmp = GetDWRBitmap()
        let innerWidth  = 744
        let innerHeight = 672
        //                                                         8 for fceux left border, 24 for black pixels, 51 for fceux top menu
        let innerBmp = outerBmp.Clone(new System.Drawing.Rectangle(8+24, 51, innerWidth, innerHeight), System.Drawing.Imaging.PixelFormat.Format32bppArgb)
        // innerBmp is now just the 'colorful' area - the (3x) NES window without the 8(x3) black pixels along the left edge
        let innerBmp = RedToWhiteBmp(innerBmp)
        //innerBmp.Save("Crop024.png")
        let fastInnerBmp = new FastRGBBitmap(innerBmp.Clone() :?> System.Drawing.Bitmap)
        let getDownscaledPixel(x,y) =
            innerBmp.GetPixel(x*3,y*3)
        let matchesExactlyOneTileTopEdge(x, y) =
            // see if 16 pixels from x,y to x+15,y match the top edge of exactly one tile
            let result = ResizeArray()
            for tilename,kind,tile in UniqueOverworldTiles do
                let mutable matchesThis = true
                for i = 0 to 15 do
                    if matchesThis then
                        if not(tile.Equals(i,0,fastInnerBmp,3*(x+i),3*y)) then
                            matchesThis <- false
                if matchesThis then
                    result.Add(kind)
            if result.Count = 1 then
                Some result.[0]
            else
                None
        let matchesExactlyOneTileBottomEdge(x, y) =
            // see if 16 pixels from x,y to x+15,y match the bottom edge of exactly one tile
            let result = ResizeArray()
            for tilename,kind,tile in UniqueOverworldTiles do
                let mutable matchesThis = true
                for i = 0 to 15 do
                    if matchesThis then
                        if not(tile.Equals(i,15,fastInnerBmp,3*(x+i),3*y)) then
                            matchesThis <- false
                if matchesThis then
                    result.Add(kind)
            if result.Count = 1 then
                Some result.[0]
            else
                None
        let matchesExactlyOneTileRightEdge(x, y) =
            // see if 16 pixels from x,y to x,y+16 match the right edge of exactly one tile
            let result = ResizeArray()
            for tilename,kind,tile in UniqueOverworldTiles do
                let mutable matchesThis = true
                for j = 0 to 15 do
                    if matchesThis then
                        if not(tile.Equals(15,j,fastInnerBmp,3*x,3*(y+j))) then
                            matchesThis <- false
                if matchesThis then
                    result.Add(kind)
            if result.Count = 1 then
                Some result.[0]
            else
                None
        let matchesExactlyOneTileLeftEdge(x, y) =
            // see if 16 pixels from x,y to x,y+16 match the left edge of exactly one tile
            let result = ResizeArray()
            for tilename,kind,tile in UniqueOverworldTiles do
                let mutable matchesThis = true
                for j = 0 to 15 do
                    if matchesThis then
                        if not(tile.Equals(0,j,fastInnerBmp,3*x,3*(y+j))) then
                            matchesThis <- false
                if matchesThis then
                    result.Add(kind)
            if result.Count = 1 then
                Some result.[0]
            else
                None
        let matchesKnownTile(ulx, uly) =
            let mutable result = None
            for tilename,kind,tile in UniqueOverworldTiles do
                if result.IsNone then
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
                        result <- Some kind
            result
        let THRESHOLD = 2  // TODO set at higher number to save new world tile, if failing to sync because legal tile not in our resource set
        let doesThisLeftTopWork(lx,ty) = 
            // see if vast majority match known tiles
            let bad = ResizeArray()  // each mismatched tile stored here
            let good = Array2D.create 14 13 None
            for by = 0 to 12 do //in [0;1;2;3;4;9;10] do      // 0-12 is all, but just get enough to feel mostly confident we've synchronized
                if bad.Count < THRESHOLD then
                    for bx = 0 to 13 do //in [1;2;3;4;11;12] do // 0-13 is all, but just get enough to feel mostly confident we've synchronized
                        if bad.Count < THRESHOLD then
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
                                match matchesKnownTile(ulx, uly) with
                                | None -> bad.Add( (ulx,uly) )
                                | Some kind -> good.[bx,by] <- Some kind
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
                // now 'good' is the 14x13 region we can totally see and identify
                // we can probably see parts of tiles around this region, try to identify those too
                let better = Array2D.init 16 15 (fun x y ->
                    if x = 0 || x = 15 then None
                    elif y = 0 || y = 14 then None
                    else good.[x-1,y-1])
                // look along top edge
                for x = 1 to 14 do
                    match matchesExactlyOneTileBottomEdge(lx+(x-1)*16,ty-1) with
                    | None -> ()
                    | Some k -> better.[x,0] <- Some k
                // look along bottom edge
                if ty <> 16 then   // TODO if ty=16, then this means we are halfway thru an up-down move, and can see exactly 14 full tiles tall, can't look down for 15th, as completely offscreen (and sadly only matching edge of top tiles, when entire top tiles on-screen)
                    for x = 1 to 14 do
                        match matchesExactlyOneTileTopEdge(lx+(x-1)*16,ty+16*13) with
                        | None -> ()
                        | Some k -> better.[x,14] <- Some k
                // look along left edge
                for y = 1 to 13 do
                    match matchesExactlyOneTileRightEdge(lx-1,ty+(y-1)*16) with
                    | None -> ()
                    | Some k -> better.[0,y] <- Some k
                // look along right edge
                for y = 1 to 13 do
                    match matchesExactlyOneTileLeftEdge(lx+16*14,ty+(y-1)*16) with
                    | None -> ()
                    | Some k -> better.[15,y] <- Some k
                (*
                for y = 0 to 14 do
                    for x = 0 to 15 do
                        printf "%s" (if better.[x,y].IsNone then "." else "X")
                    printfn ""
                *)
                better
            else
                null
        let mutable leftX = 0
        let mutable topY = 0
        let mutable goodResult = null
        // either leftX is 8 (hero is moving up-down, exactly half a tile is visible at left edge), 
        // or topY is 8 (hero is moving left-right, exactly half a tile visible at top edge)
        for ty = 1 to 16 do
            if goodResult=null then
                match doesThisLeftTopWork(8,ty) with
                | null -> ()
                | r ->
                    goodResult <- r
                    leftX <- 8
                    topY <- ty
        for lx = 1 to 16 do
            if goodResult=null then
                match doesThisLeftTopWork(lx,8) with
                | null -> ()
                | r ->
                    goodResult <- r
                    leftX <- lx
                    topY <- 8
        if goodResult=null then
            printfn "pixel sync failed"
            fastInnerBmp.Finish()
            ResizeArray()
        else
            let makeTiny(leftX, topY, goodResult:Constants.OverworldMapTile option[,]) = 
                // turn each 16x16 block of the overworld map down to a single pixel
                let tinyBmp = new System.Drawing.Bitmap(goodResult.GetLength(0),goodResult.GetLength(1))
                for x = 0 to goodResult.GetLength(0)-1 do
                    for y = 0 to goodResult.GetLength(1)-1 do
                        match goodResult.[x,y] with
                        | None -> tinyBmp.SetPixel(x, y, UNKNOWN)
                        | Some k -> tinyBmp.SetPixel(x, y, k.ProjectionColor)
                tinyBmp
            let results = ResizeArray()
            results.Add(makeTiny(leftX, topY,goodResult))
            // we can mistakenly sync on half tiles, as some tiles like swamp/desert/grass repeat every 8 pixels of their 16x16 grids - ensure find 'real' sync
            if leftX <= 8 then
                if topY <= 8 then
                    match doesThisLeftTopWork(leftX+8, topY+8) with
                    | null -> ()
                    | r ->
                        printfn "extra result +8,+8"
                        results.Add(makeTiny(leftX+8, topY+8, r))
                match doesThisLeftTopWork(leftX+8, topY) with
                | null -> ()
                | r ->
                    printfn "extra result +8,+0"
                    results.Add(makeTiny(leftX+8, topY, r))
            if topY <= 8 then
                match doesThisLeftTopWork(leftX, topY+8) with
                | null -> ()
                | r ->
                    printfn "extra result +0,+8"
                    results.Add(makeTiny(leftX, topY+8, r))
            fastInnerBmp.Finish()
            results

let NUM_ANIMATION_FRAMES = 2

let animateColors(bmp:System.Drawing.Bitmap, colors:System.Drawing.Color[]) =
    let colors = colors |> Array.map (fun c -> c, c.ToArgb())
    let F = NUM_ANIMATION_FRAMES
    let scale(i, b) = byte(int b*(F-i-1)/F)
    let frames = Array.init F (fun _ -> bmp.Clone() :?> System.Drawing.Bitmap)
    let fastFrames = Array.init F (fun i -> new FastRGBBitmap(frames.[i]) )
    for x = 0 to frames.[0].Width-1 do
        for y = 0 to frames.[0].Height-1 do
            for c, argb in colors do
                if fastFrames.[0].Equals(x,y,argb) then
                    for i = 1 to F-1 do
                        fastFrames.[i].SetRGB(x, y, scale(i,c.R), scale(i,c.G), scale(i,c.B))
    fastFrames |> Array.iter (fun ff -> ff.Finish())
    frames

type Mapper() =
    let EXPLORED_MAP_BORDER_THICKNESS = 2
    let mutable exploredMapImageWidth = 0
    let UARGB = UNKNOWN.ToArgb()
    let MAX = 400
    let W = 16
    let H = 15
    let wholeMap = new System.Drawing.Bitmap(MAX,MAX)
    let mutable lowULX = MAX/2
    let mutable lowULY = MAX/2
    let mutable curULX = MAX/2
    let mutable curULY = MAX/2
    let mutable hiULX = MAX/2
    let mutable hiULY = MAX/2
    let mutable hasStarted = false
    let recolor(r:System.Drawing.Bitmap,x,y) =  // highlights on map
        let c = r.GetPixel(x,y)
        if not(Constants.OverworldMapTile.IsAnimationColor(c)) then
            r.SetPixel(x,y,System.Drawing.Color.FromArgb(int c.R*3/4, int c.G*3/4, int c.B*3/4))
            //r.SetPixel(x,y,System.Drawing.Color.FromArgb(int c.R*7/8, int c.G*7/8, int c.B*7/8))
    member this.HasStarted = hasStarted
    member private this.Mask(bmp:System.Drawing.Bitmap) =
        if bmp.Width <> W || bmp.Height <> H then
            failwith "bad bmp to mask"
        let bmp = bmp.Clone() :?> System.Drawing.Bitmap
        // hero can cover up these map squares
        bmp.SetPixel(7,7,UNKNOWN)
        bmp.SetPixel(8,6,UNKNOWN)
        bmp.SetPixel(8,7,UNKNOWN)
        bmp.SetPixel(8,8,UNKNOWN)
        bmp.SetPixel(9,7,UNKNOWN)
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
        // TODO issue with bottom row? pressing f9 to start causes fceux to pop up display on bottom row
        for x = 0 to W-1 do
            bmp.SetPixel(x,H-1,UNKNOWN)
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
        if not found then
            // maybe just return'd or wings'd, try looking near start
            let tmpx,tmpy = curULX, curULY
            curULX <- MAX/2
            curULY <- MAX/2
            for bmp in bmps do
                if this.TryIncrementalPaint(bmp) then
                    if found then
                        printfn "FOUND TWO PAINTS, this is bad and will be hard to recover from"
                    found <- true
            // if still not found, return to last known
            if not found then
                curULX <- tmpx
                curULY <- tmpy
        found
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
    member this.GetWholeMap(nearbyMapSize) = 
        let size = nearbyMapSize
        let r = wholeMap.Clone() :?> System.Drawing.Bitmap 
        let ulx = curULX + W/2 - size/2
        let uly = curULY + H/2 - size/2
        for x = ulx to ulx+size-1 do
            recolor(r,x,uly)
            recolor(r,x,uly+size/2)
            recolor(r,x,uly+size-1)
        for y = uly+1 to uly+size-2 do
            recolor(r,ulx,y)
            recolor(r,ulx+size/2,y)
            recolor(r,ulx+size-1,y)
        r
    member this.GetNearbyMap(size) = 
        let ulx = curULX + W/2 - size/2
        let uly = curULY + H/2 - size/2
        let cloned = wholeMap.Clone(new System.Drawing.Rectangle(ulx, uly, size, size), System.Drawing.Imaging.PixelFormat.Format32bppArgb)
        cloned.SetPixel(size/2, size/2, System.Drawing.Color.Red)
        // draw crosshairs to help locate hero
        for x = 0 to size-1 do
            recolor(cloned,x,size/2)
        for y = 0 to size-1 do
            recolor(cloned,size/2,y)
        // draw bounds of currently visible tiles
        let lox, hix = (size-W)/2, (size-W)/2 + W - 1
        let loy, hiy = (size-H)/2, (size-H)/2 + H - 1
        for x = lox to hix do
            recolor(cloned, x, loy)
            recolor(cloned, x, hiy)
        for y = loy+1 to hiy-1 do
            recolor(cloned, lox, y)
            recolor(cloned, hix, y)
        cloned
    member private this.ComputeExploreMapSize() =
        let N = EXPLORED_MAP_BORDER_THICKNESS
        // explored size
        let ew = hiULX-lowULX+W
        let eh = hiULY-lowULY+H
        let m = max ew eh
        let m = max m 50  // ensure don't begin ridiculously zoomed in
        m+2*N
    member this.GetExploredMap(width, nearbyMapSize) = 
        exploredMapImageWidth <- width
        let N = EXPLORED_MAP_BORDER_THICKNESS
        let m = this.ComputeExploreMapSize()
        let cloned = this.GetWholeMap(nearbyMapSize).Clone(new System.Drawing.Rectangle(lowULX-N, lowULY-N, m, m), System.Drawing.Imaging.PixelFormat.Format32bppArgb)
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
    member this.ResetCurrentLocationToStart() =
        curULX <- MAX/2
        curULY <- MAX/2

let gridAdd(g:Grid, x, c, r) =
    g.Children.Add(x) |> ignore
    Grid.SetColumn(x, c)
    Grid.SetRow(x, r)

type MyWindow(ihrs,imins,isecs,racingMode,leagueMode,xp_thresholds) as this = 
    inherit Window()
    let nearbyCaption = new TextBox(Text="nearby world 0",FontSize=16.0,Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(2.0))
    let allCaption = new TextBox(Text="all explored 0",FontSize=16.0,Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(2.0))
    let mapper0 = new Mapper()
    let mapper1 = new Mapper()
    let mutable currentContinent = 0
    let mutable src_cb : CheckBox = null  // staff of rain checkbox
    let mutable jerk_cb : CheckBox = null  // jerk checkbox
    let CurrentMapper() = if currentContinent = 0 then mapper0 else mapper1
    let TryPaintOther(innerBMPs) =
         if currentContinent = 0 then 
            if mapper1.TryIncrementalPaint(innerBMPs) then
                currentContinent <- 1
                nearbyCaption.Text <- "nearby world 1"
                allCaption.Text <- "all explored 1"
         else 
            if mapper0.TryIncrementalPaint(innerBMPs) then
                currentContinent <- 0
                nearbyCaption.Text <- "nearby world 0"
                allCaption.Text <- "all explored 0"
    let mutable startTime = DateTime.Now + TimeSpan.FromSeconds(0.0)
    let mutable mostRecentDeathTime = DateTime.Now 
    let mutable numDeaths = 0
    let mutable changed = false
    let mutable curFrame = 0
    let mutable image1Frames = null
    let mutable image2Frames = null
    let image1 = new Image()
    let image2 = new Image()
    let mutable caveMapIsCurrentlyDisplayed = false
    let monsterImage = new Image()
    let tab = new TabControl(Background=Brushes.Black)
    let mutable ssNum = 1
    let mutable prevMatchName = ""
    let mutable heroExp = 0
    let mutable heroLevel = 1
    let heroLevelTimes = Array.create 30 null
    let heroSpells = Array.create 10 false
    let mutable heroWeaponIndex = -1
    let mutable heroArmorIndex = -1 
    let mutable heroShieldIndex = -1 
    let weaponTextBox = new RichTextBox(FontSize=16.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0),Focusable=false)
    let armorTextBox  = new RichTextBox(FontSize=16.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0),Focusable=false)
    let shieldTextBox = new RichTextBox(FontSize=16.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0),Focusable=false)
    let makeAnimationBrush() =
        let colorAnimation = new System.Windows.Media.Animation.ColorAnimation()
        colorAnimation.From <- Nullable<_>(System.Windows.Media.Colors.Black)
        colorAnimation.To <- Nullable<_>(System.Windows.Media.Color.FromRgb(0x6Fuy, 0x6Fuy, 0x00uy))
        colorAnimation.Duration <- new Duration(TimeSpan.FromSeconds(1.5))
        colorAnimation.AutoReverse <- true
        let brush = new SolidColorBrush(Colors.Black)
        brush, (fun () -> brush.BeginAnimation(SolidColorBrush.ColorProperty, colorAnimation))
    let appendRichTextWithBackground(box:RichTextBox, text, color, bgcolor) = 
        let range = new System.Windows.Documents.TextRange(box.Document.ContentEnd, box.Document.ContentEnd)
        range.Text <- text
        range.ApplyPropertyValue(System.Windows.Documents.TextElement.ForegroundProperty, color)
        range.ApplyPropertyValue(System.Windows.Documents.TextElement.FontWeightProperty, FontWeights.Bold)
        range.ApplyPropertyValue(System.Windows.Documents.TextElement.BackgroundProperty, bgcolor)
    let appendRichText(box:RichTextBox, text, color) = 
        let range = new System.Windows.Documents.TextRange(box.Document.ContentEnd, box.Document.ContentEnd)
        range.Text <- text
        range.ApplyPropertyValue(System.Windows.Documents.TextElement.ForegroundProperty, color)
        range.ApplyPropertyValue(System.Windows.Documents.TextElement.FontWeightProperty, FontWeights.Bold)
    let appendRichTextAnimate(box:RichTextBox, text, color) = 
        appendRichText(box, text, color)
        let brush, thunk = makeAnimationBrush()
        box.Background <- brush
        thunk()
    let updateWeapon() =
        heroWeaponIndex <- (heroWeaponIndex + 1) % Constants.WEAPONS.Length
        weaponTextBox.Document.Blocks.Clear()
        appendRichText(weaponTextBox, "weapon ", Brushes.Orange)
        appendRichTextAnimate(weaponTextBox, sprintf "%s" Constants.WEAPONS.[heroWeaponIndex], Brushes.White)
    let updateArmor() =
        heroArmorIndex <- (heroArmorIndex + 1) % Constants.ARMOR.Length
        armorTextBox.Document.Blocks.Clear()
        appendRichText(armorTextBox, " armor ", Brushes.Orange)
        appendRichTextAnimate(armorTextBox, sprintf "%s" Constants.ARMOR.[heroArmorIndex], Brushes.White)
    let updateShield() =
        heroShieldIndex <- (heroShieldIndex + 1) % Constants.SHIELD.Length
        shieldTextBox.Document.Blocks.Clear()
        appendRichText(shieldTextBox, "shield ", Brushes.Orange)
        appendRichTextAnimate(shieldTextBox, sprintf "%s" Constants.SHIELD.[heroShieldIndex], Brushes.White)
    let onCheckedChanged(resource) =
        if resource <> "" then
            let imageStream = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
            image1.Source <- System.Windows.Media.Imaging.BitmapFrame.Create(imageStream)
            caveMapIsCurrentlyDisplayed <- true
        else
            image1.Source <- null
            caveMapIsCurrentlyDisplayed <- false
    let makeCheckedStuff(labelStr,strResEffects,cba:CheckBox[]) = 
        let sp = new StackPanel(Background=Brushes.Black)
        sp.Background<-Brushes.Black
        let label = new Label()
        label.Content <- new TextBox(Text=labelStr,FontSize=16.0,Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0))
        sp.Children.Add(label) |> ignore
        let mutable index = 0
        for s,res,effect in strResEffects do
            let tb = new TextBox(Text=s,FontSize=16.0,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0))
            let brush, thunk = makeAnimationBrush()
            tb.Background <- brush
            let cb = new CheckBox(Content=tb)
            cb.IsChecked <- System.Nullable.op_Implicit false
            cba.[index] <- cb
            index <- index + 1
            if s = "Staff Rain Cave (>)" then 
                src_cb <- cb
            if s = "Jerk Cave (<)" then 
                jerk_cb <- cb
            cb.Checked.Add(fun _ -> 
                effect()
                if s = "Staff of Rain" then 
                    src_cb.IsChecked <- System.Nullable.op_Implicit true
                if s = "Rainbow Drop" then 
                    jerk_cb.IsChecked <- System.Nullable.op_Implicit true
                if res = "E_SWORD" then 
                    heroWeaponIndex <- Constants.WEAPONS.Length-2
                    updateWeapon()
                elif res = "E_ARMOR" then 
                    heroArmorIndex <- Constants.ARMOR.Length-2
                    updateArmor()
                else 
                    onCheckedChanged(res)
                tb.Foreground <- Brushes.DarkSlateBlue 
                thunk())
            cb.Unchecked.Add(fun _ -> 
                onCheckedChanged("")
                tb.Foreground <- Brushes.Orange
                thunk())
            sp.Children.Add(cb) |> ignore
        sp
    let content = new Grid()
    let xpTextBox = new RichTextBox(FontSize=16.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0),Focusable=false)
    let hmsTimeTextBox = new TextBox(Text="timer",FontSize=42.0,Background=Brushes.Black,Foreground=Brushes.LightGreen,BorderThickness=Thickness(0.0))
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
        curFrame <- curFrame+1
        let timer = System.Diagnostics.Stopwatch.StartNew()
        // update time
        let ts = DateTime.Now - startTime
        let h,m,s = ts.Hours, ts.Minutes, ts.Seconds
        hmsTimeTextBox.Text <- sprintf "%02d:%02d:%02d\r\ndeaths:%2d" h m s numDeaths
        // update XP & spell info
        let bmpScreenshot = Screenshot.GetDWRBitmap()
        let gp(x,y) =
            let c = bmpScreenshot.GetPixel(x*3+8,y*3+51)  // TODO factor fceux window's 3x pixels and 8,51 offset somewhere better
            if c.ToArgb() = PixelLayout.RED.ToArgb() then // if hero low health, convert red to white
                PixelLayout.WHITE 
            else
                c
        if PixelLayout.identifySpells(gp,heroSpells) then
            changed <- true
        match PixelLayout.identifyEXP(gp) with   
        | Some exp -> 
            heroExp <- exp
            let newHeroLevel = (xp_thresholds |> Array.findIndex(fun z -> z > exp)) + 1
            if newHeroLevel <> heroLevel then
                heroLevel <- newHeroLevel
                changed <- true
                if newHeroLevel = 1 then  // can be 1 on a reset
                    // zero out all the auto-tracked progress (user can uncheck equipment/locations/etc if desired)
                    (Array.create 30 null).CopyTo(heroLevelTimes, 0)
                    (Array.create 10 false).CopyTo(heroSpells, 0)
                else
                    heroLevelTimes.[heroLevel-2] <- sprintf "%03d:%02d " (60*h+m) s
        | None -> ()
        match PixelLayout.identifyHP(gp) with   
        | Some 0 -> // HP = 0 - we are dying now
            if (DateTime.Now - mostRecentDeathTime) > TimeSpan.FromSeconds(10.0) then // ensure don't run this two frames in a row
                numDeaths <- numDeaths + 1
                mostRecentDeathTime <- DateTime.Now 
                mapper0.ResetCurrentLocationToStart()
                currentContinent <- 0
                changed <- true
        | _ -> ()
        // level xp/times/spells text area
        if changed then
            xpTextBox.Document.Blocks.Clear()
            for l = 0 to 17 do
                let bg = if l=3||l=8||l=13 then new SolidColorBrush(System.Windows.Media.Color.FromRgb(0x45uy, 0x45uy, 0x55uy)) else Brushes.Black // highlight levels 5,10,15
                if heroLevelTimes.[l] <> null then
                    appendRichTextWithBackground(xpTextBox, heroLevelTimes.[l], Brushes.White, bg)
                else
                    appendRichTextWithBackground(xpTextBox, sprintf "%-6d " xp_thresholds.[l], Brushes.Orange, bg)
            for i = 0 to 9 do
                appendRichText(xpTextBox, " " + PixelLayout.SPELL_NAMES.[i].Substring(0,6), if heroSpells.[i] then Brushes.White else Brushes.DarkSlateGray)
        if racingMode then
            // TODO for reasons I don't understand, not running this causes an unmanaged memory leak that makes the app crash in less than 2 minutes
            EnemyData.bestMatch(bmpScreenshot) |> ignore
        else
            // auto screenshot
            if false then
                bmpScreenshot.Save(sprintf "Auto%03d.png" ssNum, System.Drawing.Imaging.ImageFormat.Png)
                ssNum <- ssNum + 1
            // update map
            if mapper0.HasStarted then
                let innerBMPs = Screenshot.GetInnerDWRBitmaps()
                if innerBMPs.Count > 0 then
                    if not(CurrentMapper().TryIncrementalPaint(innerBMPs)) then
                        TryPaintOther(innerBMPs)
                    //printfn "width: %f %f %f" stackPanel.ActualWidth image1.ActualWidth image2.ActualWidth 
                    let width = int stackPanel.ActualWidth - 8  // image width given border/thickness
                    image1Frames <- animateColors(CurrentMapper().GetNearbyMap(width/4), Constants.OverworldMapTile.AnimationColors)
                    image2Frames <- animateColors(CurrentMapper().GetExploredMap(width,width/4), Constants.OverworldMapTile.AnimationColors)
                    caveMapIsCurrentlyDisplayed <- false // if innerBMPs was non-null, we're on overworld map, turn off any cave map
                if image1Frames <> null && not(caveMapIsCurrentlyDisplayed) then
                    // draw/animate overworld map
                    image1.Source <- Screenshot.BMPtoImage(image1Frames.[curFrame%NUM_ANIMATION_FRAMES])
                    image2.Source <- Screenshot.BMPtoImage(image2Frames.[curFrame%NUM_ANIMATION_FRAMES])
            else
                // auto-start mapping when we first see overworld
                let bmps = Screenshot.GetInnerDWRBitmaps()
                if bmps.Count = 1 then
                    mapper0.StartFromScratch(bmps.[0])
            // update monster
            let matches = EnemyData.bestMatch(bmpScreenshot)
            if matches.Count > 0 then
                let _,name,_bmp,crop = matches.[0]
                if prevMatchName = "1" || prevMatchName = "" then
                    prevMatchName <- name
                    tab.SelectedIndex <- 1
                    monsterImage.Source <- Screenshot.BMPtoImage(crop)
                    monsterName.Text <- name
                    monsterXP.Text   <- sprintf "XP:   %d" EnemyData.XP.[name]
                    monsterGold.Text <- sprintf "GOLD: %d" EnemyData.GOLD.[name]
                    monsterSTR.Text  <- sprintf "STR:  %d" EnemyData.STR.[name]
                    monsterAGI.Text  <- sprintf "AGI:  %d" EnemyData.AGI.[name]
                    monsterHP.Text   <- sprintf "HP:   %d" EnemyData.HP.[name]
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
        Constants.voice.Volume <- 30
        let pretty = xp_thresholds |> Array.map (fun x -> let s = x.ToString() in (String.replicate (5-s.Length) " ") + s)
        appendRichText(xpTextBox, "XP to level ", Brushes.Orange)
        appendRichText(xpTextBox, String.Join(" ",pretty), Brushes.White)
        updateWeapon()
        updateArmor()
        updateShield()
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
        leftGrid.RowDefinitions.Add(new RowDefinition(Height=GridLength(506.0)))
        leftGrid.RowDefinitions.Add(new RowDefinition())
        leftGrid.RowDefinitions.Add(new RowDefinition())
        leftGrid.RowDefinitions.Add(new RowDefinition())
        let makeKitty() =
            let kitty = new Image()
            let imageStream = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream("CroppedBrianKitty.png")
            kitty.Source <- System.Windows.Media.Imaging.BitmapFrame.Create(imageStream)
            kitty
        let kitty = makeKitty()
        gridAdd(leftGrid,kitty,0,0)
        gridAdd(leftGrid,xpTextBox,0,1)
        gridAdd(leftGrid,weaponTextBox,0,2)
        gridAdd(leftGrid,armorTextBox,0,3)
        gridAdd(leftGrid,shieldTextBox,0,4)
        weaponTextBox.MouseDown.Add(fun _ -> updateWeapon())
        armorTextBox.MouseDown.Add(fun _ -> updateArmor())
        shieldTextBox.MouseDown.Add(fun _ -> updateShield())
        gridAdd(content,leftGrid,0,0)
        // right grid
        let rightGrid = new Grid()
        rightGrid.ColumnDefinitions.Add(new ColumnDefinition())
        rightGrid.RowDefinitions.Add(new RowDefinition(Height=GridLength(108.0)))
        gridAdd(rightGrid,hmsTimeTextBox,0,0)

        rightGrid.RowDefinitions.Add(new RowDefinition())
        let locationTextBox = makeCheckedStuff("LOCATIONS FOUND",Constants.LOCATIONS,Constants.LocationCheckboxes)
        gridAdd(rightGrid,locationTextBox,0,1)

        (* moved, but no longer fits in non-racing mode
        rightGrid.RowDefinitions.Add(new RowDefinition())
        let itemTextBox = makeCheckedStuff("ITEMS",Constants.ITEMS)
        gridAdd(rightGrid,itemTextBox,0,2)
        *)

        // add right grid
        gridAdd(content,rightGrid,1,0)

        // picture area
        if racingMode then
            let sp = new StackPanel(Background=Brushes.Black,Orientation=Orientation.Vertical)
            if not leagueMode then
                let kitty = makeKitty()
                sp.Children.Add(kitty) |> ignore
            else
                sp.Children.Add(new TextBox(TextWrapping=TextWrapping.Wrap,Text="League mode, flags: CDGPRVWZks\nChanges:\n- no spell learning randomization\n- no keys\n- short Charlock\n- very fast XP\n\nSpells: Heal 3 / Hurt 4 / Sleep 7\nRadiant 9 / Stopspell 10 / Outside 12\nReturn 13 / Repel 15\nHealmore 17 / Hurtmore 19",FontSize=14.0,Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(8.0))) |> ignore
            
            // no longer used to free up screen space:
            //sp.Children.Add(new TextBox(TextWrapping=TextWrapping.Wrap,Text="Monster data and maps are disabled during this stream, since this is a race!\n\nNo hints/advice/spoilers in chat during the race!",FontSize=16.0,Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(8.0))) |> ignore

            let itemTextBox = makeCheckedStuff("ITEMS",Constants.ITEMS,Array.zeroCreate Constants.ITEMS.Length)
            sp.Children.Add(itemTextBox) |> ignore
            
            let shops = new Grid()
            shops.ColumnDefinitions.Add(new ColumnDefinition(Width=GridLength(76.0)))
            shops.ColumnDefinitions.Add(new ColumnDefinition())
            shops.ColumnDefinitions.Add(new ColumnDefinition())
            shops.ColumnDefinitions.Add(new ColumnDefinition())
            shops.ColumnDefinitions.Add(new ColumnDefinition(Width=GridLength(14.0)))
            shops.ColumnDefinitions.Add(new ColumnDefinition())
            shops.ColumnDefinitions.Add(new ColumnDefinition(Width=GridLength(14.0)))
            shops.ColumnDefinitions.Add(new ColumnDefinition())
            shops.ColumnDefinitions.Add(new ColumnDefinition())
            shops.ColumnDefinitions.Add(new ColumnDefinition())
            shops.RowDefinitions.Add(new RowDefinition())
            shops.RowDefinitions.Add(new RowDefinition(Height=GridLength(2.0)))
            shops.RowDefinitions.Add(new RowDefinition())
            shops.RowDefinitions.Add(new RowDefinition(Height=GridLength(2.0)))
            shops.RowDefinitions.Add(new RowDefinition())
            shops.RowDefinitions.Add(new RowDefinition(Height=GridLength(2.0)))
            shops.RowDefinitions.Add(new RowDefinition())
            shops.RowDefinitions.Add(new RowDefinition(Height=GridLength(2.0)))
            shops.RowDefinitions.Add(new RowDefinition())
            shops.RowDefinitions.Add(new RowDefinition(Height=GridLength(2.0)))
            shops.RowDefinitions.Add(new RowDefinition())
            //shops.ShowGridLines <- true
            let makeText(txt) = new TextBox(Text=txt,FontSize=14.0,Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0))
            gridAdd(shops,makeText("HA"),1,0)
            gridAdd(shops,makeText("BS"),2,0)
            gridAdd(shops,makeText("FS"),3,0)
            gridAdd(shops,makeText("MA"),5,0)
            gridAdd(shops,makeText("LS"),7,0)
            gridAdd(shops,makeText("SS"),8,0)
            gridAdd(shops,makeText("Rimuldar"),0,2)
            gridAdd(shops,makeText("Cantlin"),0,4)
            gridAdd(shops,makeText("Kol"),0,6)
            gridAdd(shops,makeText("Garinham"),0,8)
            gridAdd(shops,makeText("Brecconary"),0,10)
            for i = 0 to 8 do
                for j = 1 to 10 do
                    if j%2=0 then
                        if i <> 0 && i <> 4 && i <> 6 then
                            gridAdd(shops,new CheckBox(),i,j)
                    else
                        gridAdd(shops,new StackPanel(Background=Brushes.Orange),i,j)

            let shopPanel = new StackPanel()
            shopPanel.Children.Add(makeText("SHOPS")) |> ignore
            shopPanel.Children.Add(shops) |> ignore
            sp.Children.Add(shopPanel) |> ignore
            gridAdd(content,sp,2,0)
        else
            let maps = new TabItem(Background=Brushes.Black, Header="Maps")
            let monsters = new TabItem(Background=Brushes.Black, Header="Monsters")
            tab.Items.Add(maps) |> ignore
            tab.Items.Add(monsters) |> ignore
            let sp = new StackPanel(Background=Brushes.Black,Orientation=Orientation.Vertical)
            sp.Children.Add(image1) |> ignore
            sp.Children.Add(nearbyCaption) |> ignore
            image1.Margin <- Thickness(1.0)
            image2.Margin <- Thickness(1.0)
            image2.MouseLeftButtonDown.Add(fun x -> 
                let point = x.GetPosition(image2) 
                CurrentMapper().ResetCurrentLocation(int point.X, int point.Y)
                ())
            sp.Children.Add(image2) |> ignore
            sp.Children.Add(allCaption) |> ignore
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
        timer.Interval <- TimeSpan.FromSeconds(0.5)  // TODO decide time, interacts with both mapping and monster portraits...
        timer.Tick.Add(fun _ -> update())
        timer.Start()

        //this.Topmost <- true
        //this.Deactivated.Add(fun _ -> activate())

    //////////////////////////////////////////////////
    // global hotkey

    let VK_F4 =  0x73
    let VK_F8 =  0x77
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
        if(not(Winterop.RegisterHotKey(helper.Handle, Winterop.HOTKEY_ID, MOD_NONE, uint32 VK_F8))) then
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
                    printfn "reset map 0 "
                    changed <- true // populate initial level stats
                    let bmps = Screenshot.GetInnerDWRBitmaps()
                    if bmps.Count = 1 then
                        mapper0.StartFromScratch(bmps.[0])
                    else
                        printfn "FAILED TO START MAPPING, count was %d" bmps.Count 
                elif key = VK_F8 then
                    printfn "reset map 1 "
                    let bmps = Screenshot.GetInnerDWRBitmaps()
                    if bmps.Count = 1 then
                        mapper1.StartFromScratch(bmps.[0])
                        TryPaintOther(bmps)
                    else
                        printfn "FAILED TO START MAPPING, count was %d" bmps.Count 
                elif key = VK_F4 then
                    let bmpScreenshot = Screenshot.GetDWRBitmap()
                    bmpScreenshot.Save(sprintf "Screen%03d.png" ssNum, System.Drawing.Imaging.ImageFormat.Png)
                    ssNum <- ssNum + 1
                handled <- true
        IntPtr.Zero

let neededAttacksTable() =
    for mp in 56..8..160 do
        let heals = mp / 8
        let mutable ap = 120
        let mutable foundLo = false
        while ap <> -1 do
            let mx = (ap-100) / 2
            let mn = mx / 2
            let av = (mx + mn) / 2
            let numAttacksLo = (150 / av) + 1
            let numAttacksHi = (165 / av) + 1
            if not foundLo && (heals >= numAttacksLo - 2) then
                printf "%3d mp - need %3d " mp ap
                foundLo <- true
            if heals >= numAttacksHi - 2 then
                printfn "to %3d ap to win" ap
                ap <- -2
            ap <- ap + 1


let testSendMessage() =
    let fceuxProcess = System.Diagnostics.Process.GetProcessesByName("fceux").[0]
    let h = fceuxProcess.MainWindowHandle 
    
    // https://github.com/wine-mirror/wine/blob/master/include/mmsystem.h
    // MM_JOY1BUTTONDOWN   0x3B5
    // MM_JOY1BUTTONUP     0x3B7
    // JOY_BUTTON1         	0x0001
    // JOY_BUTTON2         	0x0002
    // JOY_BUTTON1CHG      	0x0100
    // JOY_BUTTON2CHG      	0x0200
    // does not work
    // Winterop.SendMessage(h, 0x3B5, IntPtr(0x0100 ||| 0x0001), IntPtr(0) (* yPos/xPos are two words *) )

    // below does work if remap FCEUX to e.g. read keyboard 'b' as B joypress
    Winterop.SetForegroundWindow(h) |> ignore
    System.Windows.Forms.SendKeys.SendWait("b")


[<STAThread>]
[<EntryPoint>]
let main argv = 
    //testSendMessage()
    //neededAttacksTable()
    //ROM.test_rng()
    //ROM.test_period(0x7c65)   // period was 32768, with 2 calls per frame and 60fps, every 4.5 mins this cycles
    let DISPLAY_MAP_OF_SEED = argv.Length > 5
    if DISPLAY_MAP_OF_SEED then
        use fd = new System.Windows.Forms.OpenFileDialog()
        fd.InitialDirectory <- """C:\Users\Admin1\Desktop\dwrandomizer-2.0.6-windows\"""
        fd.ValidateNames <- true
        fd.CheckFileExists <- true
        fd.CheckPathExists <- true
        if fd.ShowDialog() = System.Windows.Forms.DialogResult.OK then
            //let bmp1,bmp2 = ROM.decode_rom("""C:\Users\Admin1\Desktop\fceux-2.2.3-win32\DWRando.3900483431572982.CDFGMPRWZ.nes""")
            //let bmp1,bmp2 = ROM.decode_rom("""C:\Users\Admin1\Desktop\dwrandomizer-2.0.6-windows\DWRando.8523561777557155.CDFGMPRWZ.nes""")
            let bmp1,bmp2 = ROM.decode_rom(fd.FileName)
            let w = new Window()

            let g = new Grid()
            g.RowDefinitions.Add(new RowDefinition())
            g.ColumnDefinitions.Add(new ColumnDefinition())
            g.ColumnDefinitions.Add(new ColumnDefinition())

            let image1 = new Image()
            image1.Source <- Screenshot.BMPtoImage(bmp1)
            RenderOptions.SetBitmapScalingMode(image1, BitmapScalingMode.NearestNeighbor)
            let image2 = new Image()
            image2.Source <- Screenshot.BMPtoImage(bmp2)
            RenderOptions.SetBitmapScalingMode(image2, BitmapScalingMode.NearestNeighbor)

            gridAdd(g, image1, 0, 0)
            gridAdd(g, image2, 1, 0)

            //w.Width <- 960.0
            //w.Height <- 960.0
            w.Title <- System.IO.Path.GetFileNameWithoutExtension(fd.FileName)
            w.Content <- g
            (new Application()).Run(w)
        else
            printfn "bad file selection"
            1
    else
    let app = new Application()
    let myAssembly = System.Reflection.Assembly.GetExecutingAssembly()
    let names = myAssembly.GetManifestResourceNames()
    //for n in names do
    //    printfn "%s" n

    // load up known overworld map tiles
    for ow,kind in Constants.OVERWORLD_MAP_TILE_FILENAMES do
        let imageStream = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream(ow)
        let bmp = new System.Drawing.Bitmap(imageStream)
        Screenshot.UniqueOverworldTiles.Add(ow,kind,new FastRGBBitmap(bmp))
    printfn "loaded %d overworld tiles" Screenshot.UniqueOverworldTiles.Count 
     
    (*
    let bmp = new System.Drawing.Bitmap(System.IO.Path.Combine("""C:\Users\Admin1\Source\Repos\Misc\DragonWarriorRandomizerDisplay\DragonWarriorRandomizerDisplay\bin\Debug\roguescorpion.png"""))
    let screen = new System.Drawing.Bitmap(System.IO.Path.Combine("""C:\Users\Admin1\Source\Repos\Misc\DragonWarriorRandomizerDisplay\DragonWarriorRandomizerDisplay\bin\Debug\Screen001.png"""))
    let r = computeMatch(bmp, screen, 402, 426, 380, 396)
    printfn "%f" r
    let r = computeMatch(bmp, screen, 402, 426, 384, 396)
    printfn "%f" r
    *)

    (*    
    let bmp = new System.Drawing.Bitmap(System.IO.Path.Combine("""C:\Users\Admin1\Documents\GitHubVisualStudio\DragonWarriorRandomizerTools\DWR_Tools\DWR_Tools\spellmenu.png"""))
    let downscaled = new System.Drawing.Bitmap(256,224)
    for x = 0 to 255 do
        for y = 0 to 223 do
            downscaled.SetPixel(x,y, bmp.GetPixel(x*3+8, y*3+51))
    let spells = PixelLayout.identifySpells(fun (x,y) -> downscaled.GetPixel(x,y))
    printfn "%A" spells
    let exp = PixelLayout.identifyEXP(fun (x,y) -> downscaled.GetPixel(x,y))
    printfn "EXP: %A" exp
    *)

    let racingMode = argv.Length > 0
    let leagueMode = argv.Length > 1
    let xp = if leagueMode then Constants.DWR_XP_LEVEL_THRESHOLDS_50_PERCENT else Constants.DWR_XP_LEVEL_THRESHOLDS 
    app.Run(MyWindow(0,0,0,racingMode,leagueMode,xp)) |> ignore
    0
