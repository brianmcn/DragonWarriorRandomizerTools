module ROM

let decode_rom(file) =
    let bytes = System.IO.File.ReadAllBytes(file)
    let content = bytes.[16..]   // first 16 bytes are a header

    let location_text_bytes = bytes.[0xA236..0xA298]
    let dw_alphabet = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ\"\"'*>_:__.,-_?!;)(``_'___________  "
    let chars = Array.init location_text_bytes.Length (fun i -> try dw_alphabet.[int location_text_bytes.[i]] with _ -> ' ')
    let location_str = (new System.String(chars))
    printfn "loc: %s" location_str 
    // From Tantegel Castle travel 49 leagues to the north and 24 to the west
    let to1 = location_str.IndexOf("to the")
    let to2 = location_str.IndexOf("to the",to1+1)
    let buried_dx, buried_dy = 
        try
            let num1 = int(location_str.Substring(to1 - 12,3))
            let num2 = int(location_str.Substring(to2 - 4,3))
            printfn "%d %d" num1 num2
            let buried_dy = if location_str.[to1+7] = 'n' then -num1 else num1
            let buried_dx = if location_str.[to2+7] = 'w' then -num2 else num2
            buried_dx, buried_dy 
        with _ -> -999, -999

    let map_pointers_0 = content.[0x2653]  // 120 16-bit pointers (subtract 0x9d5d from these pointers)
    let map_encoded_0 = content.[0x1d5d]   // RLE, top nibble tile, bottom nibble count

    let ptr_0 = System.BitConverter.ToUInt16(content, 0x2653)
//    printfn "%u %x" ptr_0 ptr_0

    let map_pointers = [|
        for i = 0 to 119 do
            yield System.BitConverter.ToUInt16(content, 0x2653 + 2*i)
        |]
    let encoded = content.[0x1d5d..]
    let tiles = Array2D.zeroCreate 120 120
    for row = 0 to 119 do
//        printfn "row: %d" row
        let mutable offset = int(map_pointers.[row] - uint16 0x9d5d)
//        printfn "offset: %d" offset
        let mutable x = 0
        while x < 120 do
            let cur_byte = encoded.[offset]
            let tile = cur_byte >>> 4
            let count = int(cur_byte &&& 0xfuy) + 1
//            printfn "x %d tile %d count %d" x tile count
            for j = 0 to count-1 do
                if x < 120 then  // TODO kludge
                    tiles.[row,x] <- tile
                    x <- x + 1
            offset <- offset + 1

(*
    printfn "map:"
    printfn ""
    for row = 0 to 119 do
        for col = 0 to 119 do
            printf "%x" tiles.[row,col]
        printfn ""
    printfn ""
*)

    let tiles = Array2D.init 120 120 (fun x y -> 
        match tiles.[x,y] with
        | 0uy -> Constants.OverworldMapTile.Plains
        | 1uy -> Constants.OverworldMapTile.Desert
        | 2uy -> Constants.OverworldMapTile.Hills
        | 3uy -> Constants.OverworldMapTile.Mountain
        | 4uy -> Constants.OverworldMapTile.Water_xxxx 
        | 5uy -> Constants.OverworldMapTile.Wall 
        | 6uy -> Constants.OverworldMapTile.Forest
        | 7uy -> Constants.OverworldMapTile.Swamp
        | 8uy -> Constants.OverworldMapTile.Town
        | 9uy -> Constants.OverworldMapTile.Cave 
        | 10uy -> Constants.OverworldMapTile.Castle 
        | 11uy -> Constants.OverworldMapTile.Bridge
        | 12uy -> Constants.OverworldMapTile.Cave // STAIRS
        | z -> Constants.OverworldMapTile.Water_NWES  // TODO failwithf "bad tiles data: %A" z
        )

    let EDGE = 6
    let bmp1 = new System.Drawing.Bitmap(120+2*EDGE,120+2*EDGE)
    let bmp2 = new System.Drawing.Bitmap(120+2*EDGE,120+2*EDGE)
    for x = 0 to bmp2.Width-1 do
        for y = 0 to bmp2.Height-1 do
            bmp1.SetPixel(y, x, Constants.OverworldMapTile.Water_xxxx.ProjectionColor)
            bmp2.SetPixel(y, x, Constants.OverworldMapTile.Mountain.AltProjectionColor)
    for x = 0 to 119 do
        for y = 0 to 119 do
            bmp1.SetPixel(y+EDGE, x+EDGE, tiles.[x,y].ProjectionColor)
            bmp2.SetPixel(y+EDGE, x+EDGE, tiles.[x,y].AltProjectionColor)


    let tryAllPlace(x,y,isCave,a:string[]) =
        let darken(x,y) =
            let c = bmp2.GetPixel(x,y)
            let K = 6
            bmp2.SetPixel(x,y, System.Drawing.Color.FromArgb(int c.R*K/8, int c.G*K/8, int c.B*K/8))
        let ok(c:System.Drawing.Color) = 
            c.ToArgb() = Constants.OverworldMapTile.Desert.AltProjectionColor.ToArgb() ||
            c.ToArgb() = Constants.OverworldMapTile.Mountain.AltProjectionColor.ToArgb()
        let tryPlace(x,y,isCave,a:string[]) =
            let mutable works = true
            for i = 0 to 4 do
                for j = 0 to 4 do
                    if not(ok(bmp2.GetPixel(x+i,y+j))) then
                        works <- false
            if works then
                for i = 0 to 4 do
                    for j = 0 to 4 do
                        if a.[j].[i] = 'X' then
                            bmp2.SetPixel(x+i,y+j, if isCave then Constants.OverworldMapTile.Cave.AltProjectionColor else Constants.OverworldMapTile.Town.AltProjectionColor)
                        else
                            darken(x+i,y+j)
            works            
        if not(tryPlace(x+EDGE-3, y, isCave, a)) then
            if not(tryPlace(x, y+EDGE-1, isCave, a)) then
                if not(tryPlace(x+EDGE-1, y+EDGE+2, isCave, a)) then
                    if not(tryPlace(x+EDGE+2, y+EDGE-3, isCave, a)) then
                        printfn "failed to place a label!"
                    else
                        darken(x+EDGE+1,y+EDGE)
                else
                    darken(x+EDGE,y+EDGE+1)
            else
                darken(x+EDGE-1,y+EDGE)
        else
            darken(x+EDGE,y+EDGE-1)

    let warps = bytes.[0xf3d8..]
    let mutable gx,gy,tx,ty = -1, -1, -1, -1  // garin and tantagel coords
    let mutable gf, tf = (fun _ -> ()), (fun _ -> ())  // thunks to backpatch basement labels when find topside coords later
    for i = 0 to 50 do
        let d = i*3
        let from_map = warps.[0+d]
        let from_x = warps.[1+d]
        let from_y = warps.[2+d]
        let to_map = warps.[153+d]
        let to_x = warps.[154+d]
        let to_y = warps.[155+d]
        let printIt = from_map = 1uy || from_map = 9uy || from_map = 4uy
        if printIt then
            printf "%3d %3d %3d %3d %3d %3d " from_map from_x from_y to_map to_x to_y
        let dest() = 
            match to_map with
            | 9uy -> "Garinham", false, [|"XXXXX";"X    ";"X XXX";"X   X";"XXXXX"|]
            | 13uy -> "Staff of Rain Shrine", true, [|" X   ";"  X  ";"   X ";"  X  ";" X   "|]
            | 7uy -> "Kol", false, [|"X  X ";"X X  ";"XX   ";"X X  ";"X  X "|]
            | 8uy -> "Brecconary", false, [|"XXXX ";"X  X ";"XXXXX";"X   X";"XXXXX"|]
            | 4uy -> "Tantagel Castle", false, [|"XXXXX";"  X  ";"  X  ";"  X  ";"  X  "|]
            | 21uy when to_y = 0uy -> "Swamp Cave North", true, [|"X   X";"XX  X"; "X X X";"X  XX";"X   X"|]
            | 21uy when to_y <> 0uy -> "Swamp Cave South", true, [|"XXXXX"; "X    ";"XXXXX";"    X";"XXXXX"|]
            | 2uy -> "Charlock Castle", false, null
            | 22uy -> "Mountain Cave", true, [|"X   X";"XX XX";"X X X";"X   X";"X   X"|]
            | 11uy -> "Rimuldar", false, [|"XXXXX";"X   X";"XXXX ";"X   X";"X   X"|]
            | 3uy -> "Hauksness", false, [|"X   X";"X   X";"XXXXX";"X   X";"X   X"|]
            | 10uy -> "Cantlin", false, [|"XXXXX";"X    ";"X    ";"X    ";"XXXXX"|]
            | 14uy -> "Jerk Cave", true, [|"   X ";"  X  ";" X   ";"  X  ";"   X "|]
            | 28uy -> "Tablet Cave", true, [|"XXXXX";"  X  ";"  X  ";"  X  ";"  X  "|]
            | 12uy -> "Sun Stones Cave", true, [|"     ";"X   X";" X X ";"  X  ";"     "|]
            | 24uy -> "Grave of Garin", true, [|"XXXXX";"X    ";"X XXX";"X   X";"XXXXX"|]
            | _ -> failwith "unexpected warp dest"
        if from_map = 1uy then
            let dest, isCave, a = dest()
            printfn "at %3d %3d : %s" from_x from_y dest
            if a <> null then
                tryAllPlace(int from_x, int from_y, isCave, a)
            if dest="Garinham" then
                gx <- int from_x
                gy <- int from_y
                gf(gx,gy)
            if dest="Tantagel Castle" then
                tx <- int from_x
                ty <- int from_y
                tf(tx,ty)
        elif from_map = 9uy then
            let dest, isCave, a = dest()
            printfn "under Garin: %s" dest
            if gx <> -1 then
                tryAllPlace(gx, gy, isCave, a)
            else
                gf <- fun (gx,gy) -> tryAllPlace(gx, gy, isCave, a)
        elif from_map = 4uy then
            let dest, isCave, a = dest()
            printfn "under Tantagel: %s" dest
            if tx <> -1 then
                tryAllPlace(tx, ty, isCave, a)
            else
                tf <- fun (tx,ty) -> tryAllPlace(tx, ty, isCave, a)
        else
            if printIt then
                printfn ""
  
    if buried_dx <> -999 then  
        bmp2.SetPixel(EDGE+tx+buried_dx, EDGE+ty+buried_dy, System.Drawing.Color.Orange )

    bmp1, bmp2

(*
*)