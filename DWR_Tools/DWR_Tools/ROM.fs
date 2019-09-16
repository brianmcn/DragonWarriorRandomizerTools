module ROM

let MAPS = [|
    "NO_MAP"
    "OVERWORLD"
    "CHARLOCK"
    "HAUKSNESS"
    "TANTEGEL_TREASURY"   // TANTEGEL
    "TANTEGEL_THRONE_ROOM" // 5 
    "CHARLOCK_THRONE_ROOM"
    "KOL"
    "BRECCONARY"
    "GARINHAM"
    "CANTLIN" // 10 
    "RIMULDAR"
    "SUN_STONES_CAVE"    // TANTEGEL_BASEMENT
    "NORTHERN_SHRINE"
    "SOUTHERN_SHRINE"
    "CHARLOCK_CAVE_1" // 15 
    "CHARLOCK_CAVE_2"
    "CHARLOCK_CAVE_3"
    "CHARLOCK_CAVE_4"
    "CHARLOCK_CAVE_5"
    "CHARLOCK_CAVE_6" // 20 
    "SWAMP_CAVE"
    "MOUNTAIN_CAVE"
    "MOUNTAIN_CAVE_2"
    "GARINS_GRAVE_1"
    "GARINS_GRAVE_2" // 25 
    "GARINS_GRAVE_3"
    "GARINS_GRAVE_4"
    "ERDRICKS_CAVE"
    "ERDRICKS_CAVE_2"
    |]

let CHESTS = [|
    "unused"
    "unused"
    "HERB"
    "KEY"
    "TORCH"
    "FAIRY_WATER"  // 5
    "WINGS"
    "DRAGON_SCALE"
    "FLUTE"
    "RING"
    "TOKEN"        // 10
    "GWAELINS_LOVE"
    "CURSED_BELT"
    "HARP"
    "NECKLACE"
    "STONES"       // 15
    "STAFF"
    "SWORD"
    "GOLD_5"
    "GOLD_6"
    "GOLD_10"      // 20
    "GOLD_500"
    "GOLD_120"
    "TABLET"
    |]

let SHOP_ITEM = [|
    "BAMBOO_POLE"
    "CLUB"
    "COPPER_SWORD"
    "HAND_AXE"
    "BROAD_SWORD"
    "FLAME_SWORD"
    "ERDRICKS_SWORD"
    "CLOTHES"
    "LEATHER_ARMOR"
    "CHAIN_MAIL"
    "HALF_PLATE"
    "FULL_PLATE"
    "MAGIC_ARMOR"
    "ERDRICKS_ARMOR"
    "SMALL_SHIELD"
    "LARGE_SHIELD"
    "SILVER_SHIELD"
    "SHOP_HERB"
    "unused"
    "SHOP_TORCH"
    "unused"
    "SHOP_WINGS"
    "SHOP_DRAGON_SCALE"
    |]

let compute_go_mode(str, hp, mp, s:string) =
    let max_dl = ((int str + 42) - 100) / 2
    let avg_dl = max_dl * 3 / 4
    let go_mode = int hp > 96 && ((int mp/8)+2)*avg_dl > 155  // quick approx
    let mini_go_mode = int hp > 96 && ((int mp/8)+2)*avg_dl > 150  // quick approx
    let go_mode = go_mode || int hp >= 129 && ((int mp/8)+2)*(avg_dl+3) > 155  // quick approx with DN
    let mini_go_mode = mini_go_mode || int hp >= 129 && ((int mp/8)+2)*(avg_dl+3) > 150  // quick approx with DN
    let go_mode = go_mode && (s.[24] = 'H') // need healmore
    let mini_go_mode = mini_go_mode && (s.[24] = 'H') // need healmore
    go_mode, mini_go_mode

let show_go_mode_stats(bytes:byte[], print, file) =
    if print then
        printfn "for build 'Z' (STR+HP)..."
    let header = "        LV    STR   AGI   HP    MP   rawAG rawMP" 
    let mutable p_str, p_ag, p_hp, p_mp, p_hu, hu = 0, 0, 0, 0, 0, 0
    let mutable go_mode_str_hp,go_mode_str_ag = 31, 31
    if print then
        printfn "%s" header
    for i = 0 to 29 do
        let fives = (if i%5=4 then "-- " else "   ")
        let b1 = bytes.[0x60DD+6*i+4]
        let b2 = bytes.[0x60DD+6*i+5]
        let s = if b2 &&&  1uy > 0uy then "HE " else fives 
              + if b2 &&&  2uy > 0uy then "HU " else fives
              + if b2 &&&  4uy > 0uy then "SL " else fives
              + if b2 &&&  8uy > 0uy then "RA " else fives
              + if b2 &&& 16uy > 0uy then "ST " else fives
              + if b2 &&& 32uy > 0uy then "OU " else fives
              + if b2 &&& 64uy > 0uy then "RT " else fives
              + if b2 &&&128uy > 0uy then "RP " else fives
              + if b1 &&&  1uy > 0uy then "HE " else fives
              + if b1 &&&  2uy > 0uy then (hu <- 1; "HU ") else fives
//        if i=0 && (b2 &&&  8uy > 0uy) then
//            failwithf "radiant at start: %s" file
        let str, ag, hp, mp = bytes.[0x60DD+6*i+0], bytes.[0x60DD+6*i+1], bytes.[0x60DD+6*i+2], bytes.[0x60DD+6*i+3] 
        let agZ = ag - ((ag+9uy)/10uy) + 3uy
        let mpZ = mp - ((mp+9uy)/10uy) + 3uy
        let hpSTRAG = hp - ((hp+9uy)/10uy) + 3uy
        let big_str = i<>0 && int str - p_str > 12
        let big_ag = i<>0 && int agZ - p_ag > 12
        let big_hp = i<>0 && int hp - p_hp > 16
        let big_mp = i<>0 && int mpZ - p_mp > 12
        let big_hu = hu = 1 && p_hu = 0
        let big_any = big_str || big_ag || big_hp || big_mp || big_hu
        p_str <- int str
        p_ag <- int agZ
        p_hp <- int hp
        p_mp <- int mpZ
        p_hu <- hu
        let go_mode,mini_go_mode = compute_go_mode(str, hp, mpZ, s)
        let strag_go_mode,strag_mini_go_mode = compute_go_mode(str, hpSTRAG, mpZ, s)
        let x(b) = if b then "+" else " "
        if go_mode && go_mode_str_hp=31 then
            go_mode_str_hp <- i+1
        if strag_go_mode && go_mode_str_ag=31 then
            go_mode_str_ag <- i+1
        if print then
            printfn "%s %s %3d %s%3d%s  %3d%s  %3d%s  %3d%s  %3d  %3d   %s" 
                (if big_any then "**" else "  ") (if go_mode then "GO " elif mini_go_mode then "go " else "   ") (i+1) fives str (x big_str) agZ (x big_ag) hp (x big_hp) mpZ (x big_mp) ag mp s 
        if i=14 && print then
            printfn "%s" header
    go_mode_str_hp, go_mode_str_ag 

let decode_rom(file) =
    let bytes = System.IO.File.ReadAllBytes(file)
    let content = bytes.[16..]   // first 16 bytes are a header

    let location_text_bytes = bytes.[0xA236..0xA298]
    let dw_alphabet = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ\"\"'*>_:__.,-_?!;)(``_'___________  "
    let chars = Array.init location_text_bytes.Length (fun i -> try dw_alphabet.[int location_text_bytes.[i]] with _ -> ' ')
    let location_str = (new System.String(chars))
    //printfn "loc: %s" location_str 
    // From Tantegel Castle travel 49 leagues to the north and 24 to the west
    let to1 = location_str.IndexOf("to the")
    let to2 = location_str.IndexOf("to the",to1+1)
    let buried_dx, buried_dy = 
        try
            let num1 = int(location_str.Substring(to1 - 12,3))
            let num2 = int(location_str.Substring(to2 - 4,3))
            //printfn "%d %d" num1 num2
            let buried_dy = if location_str.[to1+7] = 'n' then -num1 else num1
            let buried_dx = if location_str.[to2+7] = 'w' then -num2 else num2
            buried_dx, buried_dy 
        with _ -> -999, -999

    // buried items
    printfn "BURIED ITEMS"    
    for thing, address in ["armor", 0xe160; "flute", 0xe14a; "token", 0xe10b] do
        let data = content.[address..address+15]
        let thing_map = data.[3]
        let thing_x = data.[9]
        let thing_y = data.[15]
        printfn "  %s %s %d %d" thing MAPS.[int thing_map] thing_x thing_y

    // spike tile enemies
    printfn ""
    printfn "SPIKE TILES"
    let hauks_enemy    = int content.[0xcd64]
    let swamp_enemy    = int content.[0xcd81]
    let charlock_enemy = int content.[0xcd9e]
    printfn "  hauks: %2d %s" hauks_enemy (EnemyData.ENEMY_NAME hauks_enemy)
    printfn "  swamp: %2d %s" swamp_enemy (EnemyData.ENEMY_NAME swamp_enemy)
    printfn "  charl: %2d %s" charlock_enemy (EnemyData.ENEMY_NAME charlock_enemy)

    // chests
    // each chest data is 4 bytes: map, x, y, item
    printfn ""
    let chest_bytes = content.[0x5dcd..0x5dcd+4*31]
    let all = new System.Text.StringBuilder()
    let sra = ResizeArray()
    let ksb = new System.Text.StringBuilder()
    for i = 0 to 30 do
        let map, item = chest_bytes.[i*4], chest_bytes.[i*4+3] 
        all.AppendLine(sprintf "  %20s    %-20s" MAPS.[int map] CHESTS.[int item]) |> ignore
        match item with
        | 8uy | 9uy | 10uy | 13uy | 14uy | 15uy | 17uy -> sra.Add(CHESTS.[int item], MAPS.[int map])
        | 3uy -> if map <> 5uy then ksb.Append(MAPS.[int map]).Append("  ") |> ignore
        | _ -> ()
    printfn "SUMMARY"
    let sorted = sra.ToArray() |> Array.sortBy (fun (item,_loc) ->
        match item with
        | "STONES" -> 1
        | "HARP" -> 2
        | "TOKEN" -> 3
        | "SWORD" -> 4
        | "RING" -> 5
        | "FLUTE" -> 6
        | "NECKLACE" -> 7
        | _ -> failwith "bad summary item"
        )
    for item,loc in sorted do
        printfn "  %12s   %s" item loc
//        if item="HARP" && loc="TANTEGEL_THRONE_ROOM" then
//            failwithf "harp in throne room: %s" file
    printfn ""
    printfn "KEYS: %s" (ksb.ToString())
    printfn ""
    printfn "ALL CHESTS"
    printf "%s" (all.ToString())

    // shops
    printfn ""
    printfn "SHOPS"
    let mutable cur_shop = "kol"
    let shops = content.[0x1991..0x1991+80]
    let mutable shop_count = 0
    let shop_items = new System.Collections.Generic.Dictionary<_,_>()
    let cur_items = ResizeArray()
    for item in shops do
        if shop_count < 7 then
            if item = 253uy then
                shop_count <- shop_count + 1
                let desc = 
                    match shop_count with
                    | 1 -> "brecc"
                    | 2 -> "garin"
                    | 3 -> "cantlin open 1"
                    | 4 -> "cantlin open 2"
                    | 5 -> "cantlin locked"
                    | 6 -> "rimu"
                    | 7 -> ""
                    | _ -> failwith "bad shop"
                cur_items.Add("") // ensure always at least 6
                shop_items.Add(cur_shop, cur_items.ToArray())
                cur_items.Clear()
                cur_shop <- desc
            else
                cur_items.Add(SHOP_ITEM.[int item])
    let s1,s2,s3,s4 = "kol", "brecc", "garin", "rimu"
    printfn ""
    printfn "%-26s %-26s %-26s %-26s" s1 s2 s3 s4
    printfn ""
    for i = 0 to 5 do
        printfn "%-26s %-26s %-26s %-26s" shop_items.[s1].[i] shop_items.[s2].[i] shop_items.[s3].[i] shop_items.[s4].[i] 
    let s1,s2,s3 = "cantlin open 1", "cantlin open 2", "cantlin locked"
    printfn ""
    printfn "%-26s %-26s %-26s" s1 s2 s3
    printfn ""
    for i = 0 to 5 do
        printfn "%-26s %-26s %-26s" shop_items.[s1].[i] shop_items.[s2].[i] shop_items.[s3].[i] 
    
    let zone_has_charlock_enemy = Array.zeroCreate 20
    let zone_has_metal_slime = Array.zeroCreate 20
    let zone_has_x = Array.zeroCreate 20  // for ad-hoc mapping
    let zone_has_y = Array.zeroCreate 20  // for ad-hoc mapping
    for zone = 0 to 19 do
        let data = content.[0xf54f+5*zone..0xf54f+5*zone+4]
        let extra = 
            match zone with
            | 13 -> "HAUKS"
            | 14 -> "M2/G1"
            | 15 -> "GTLOW"
            | 16 | 17 | 18 -> "CHAR "
            | 19 -> "SWAMP"
            | _  -> "     "
        printf "zone %2d (%s): " zone extra
        for enemy in data do
            printf "%3d %-16s " enemy (let name,_,_,_,_,_ = EnemyData.ENEMY_DATA.[int enemy] in name)
            if enemy = 16uy then
                zone_has_metal_slime.[zone] <- zone_has_metal_slime.[zone] + 1
            // golem is 24, werewolf is 29
            if enemy = 24uy || enemy >= 29uy then
                zone_has_charlock_enemy.[zone] <- zone_has_charlock_enemy.[zone] + 1
            if enemy = 35uy then // stoneman
                zone_has_x.[zone] <- zone_has_x.[zone] + 1
            if enemy = 36uy then // armored_knight
                zone_has_y.[zone] <- zone_has_y.[zone] + 1
        printfn ""

    let overworld_zone_data = content.[0xf522..0xf522+31]
    let ow_zones = Array2D.zeroCreate 8 8
    for x = 0 to 7 do
        for y = 0 to 7 do
            let zone_index = x + 8*y
            let i = zone_index / 2
            if zone_index % 2 = 0 then
                ow_zones.[x,y] <- (overworld_zone_data.[i] &&& 0xf0uy) >>> 4
            else
                ow_zones.[x,y] <- (overworld_zone_data.[i] &&& 0x0fuy)

    printfn "OW ZONES"
    for y = 0 to 7 do
        for x = 0 to 7 do
            printf "%3d" ow_zones.[x,y]
        printfn ""
    printfn ""

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
// tantagel at 83 98
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

    let tiles = Array2D.init 120 120 (fun x y -> Constants.OverworldMapTile.FromROMByte tiles.[x,y])

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

    let pixelsWhereLabelsHaveModified = new System.Collections.Generic.HashSet<_>()
    let tryAllPlace(x,y,isCave,a:string[]) =
        let darken(x,y) =
            let c = bmp2.GetPixel(x,y)
            let K = 7
            bmp2.SetPixel(x,y, System.Drawing.Color.FromArgb(int c.R*K/8, int c.G*K/8, int c.B*K/8))
            pixelsWhereLabelsHaveModified.Add(x,y) |> ignore
        let ok(c:System.Drawing.Color) = // TODO could just use not-in-pixelsWhereLabelsHaveModified now
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
                            pixelsWhereLabelsHaveModified.Add(x+i,y+j) |> ignore
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

    let WARPS_INDEX_IN_BYTES = 0xf3d8
    let warps = bytes.[WARPS_INDEX_IN_BYTES..]
    let mapCoords = new System.Collections.Generic.Dictionary<_,_>()
    let mutable uc1i, uc2i = -1, -1           // useless charlock warp index 1 & 2
    let mutable rimi = -1                     // rimuldar index
    let mutable gf, tf = (fun _ -> ()), (fun _ -> ())  // thunks to backpatch basement labels when find topside coords later
    for i = 0 to 50 do
        let d = i*3
        let from_map = warps.[0+d]
        let from_x = warps.[1+d]
        let from_y = warps.[2+d]
        let to_map = warps.[153+d]
        let to_x = warps.[154+d]
        let to_y = warps.[155+d]
        let printIt = false //from_map = 1uy || from_map = 9uy || from_map = 4uy 
        let debugPrint = false
        if from_map = 15uy && from_x = 13uy && from_y = 7uy then
            uc1i <- i
        if from_map = 15uy && from_x = 19uy && from_y = 7uy then
            uc2i <- i
        if from_map = 1uy && from_x = 22uy && from_y = 47uy then
            rimi <- i
        if debugPrint || printIt then
            printf "%3d %3d %3d %3d %3d %3d " from_map from_x from_y to_map to_x to_y
        let dest() = 
            match to_map with
            | 9uy  -> Constants.MAP_LOCATIONS.GARINHAM, false, [|"XXXXX";"X    ";"X XXX";"X   X";"XXXXX"|]
            | 13uy -> Constants.MAP_LOCATIONS.STAFF_CAVE, true, [|" X   ";"  X  ";"   X ";"  X  ";" X   "|]
            | 7uy  -> Constants.MAP_LOCATIONS.KOL, false, [|"X  X ";"X X  ";"XX   ";"X X  ";"X  X "|]
            | 8uy  -> Constants.MAP_LOCATIONS.BRECCONARY, false, [|"XXXX ";"X  X ";"XXXXX";"X   X";"XXXXX"|]
            | 4uy  -> Constants.MAP_LOCATIONS.TANTAGEL, false, [|"XXXXX";"  X  ";"  X  ";"  X  ";"  X  "|]
            | 21uy when to_y = 0uy -> Constants.MAP_LOCATIONS.SWAMP_NORTH, true, [|"X   X";"XX  X"; "X X X";"X  XX";"X   X"|]
            | 21uy when to_y <> 0uy -> Constants.MAP_LOCATIONS.SWAMP_SOUTH, true, [|"XXXXX"; "X    ";"XXXXX";"    X";"XXXXX"|]
            | 2uy  -> Constants.MAP_LOCATIONS.CHARLOCK, false, null
            | n when n=6uy || (n >= 15uy && n <= 20uy) -> Constants.MAP_LOCATIONS.CHARLOCK, false, null  // short charlock (or anywhere in charlock)
            | 22uy -> Constants.MAP_LOCATIONS.MOUNTAIN_CAVE, true, [|"X   X";"XX XX";"X X X";"X   X";"X   X"|]
            | 11uy -> Constants.MAP_LOCATIONS.RIMULDAR, false, [|"XXXXX";"X   X";"XXXX ";"X   X";"X   X"|]
            | 3uy  -> Constants.MAP_LOCATIONS.HAUKSNESS, false, [|"X   X";"X   X";"XXXXX";"X   X";"X   X"|]
            | 10uy -> Constants.MAP_LOCATIONS.CANTLIN, false, [|"XXXXX";"X    ";"X    ";"X    ";"XXXXX"|]
            | 14uy -> Constants.MAP_LOCATIONS.JERK_CAVE, true, [|"   X ";"  X  ";" X   ";"  X  ";"   X "|]
            | 28uy -> Constants.MAP_LOCATIONS.TABLET_CAVE, true, [|"XXXXX";"  X  ";"  X  ";"  X  ";"  X  "|]
            | 12uy -> Constants.MAP_LOCATIONS.SUN_STONES_CAVE, true, [|"     ";"X   X";" X X ";"  X  ";"     "|]
            | 24uy -> Constants.MAP_LOCATIONS.GARINS_TOMB, true, [|"XXXXX";"X    ";"X XXX";"X   X";"XXXXX"|]
            | 6uy  -> "Charlock Throne", false, null  // in short-charlock
            | _ -> failwith "unexpected warp dest"
        if from_map = 1uy then
            let dest, isCave, a = dest()
            printfn "at %3d %3d : %s" from_x from_y dest
            if Constants.MAP_LOCATIONS.IsLocation(dest) then
                mapCoords.Add(dest, (int from_x,int from_y) )
            if a <> null then
                tryAllPlace(int from_x, int from_y, isCave, a)
            if dest=Constants.MAP_LOCATIONS.GARINHAM then
                gf(int from_x,int from_y)
            if dest=Constants.MAP_LOCATIONS.TANTAGEL then
                tf(int from_x,int from_y)
        elif from_map = 9uy then
            let dest, isCave, a = dest()
            printfn "under Garin: %s" dest
            match mapCoords.TryGetValue(Constants.MAP_LOCATIONS.GARINHAM) with
            | true, (gx,gy) -> tryAllPlace(gx, gy, isCave, a)
            | _ -> gf <- fun (gx,gy) -> tryAllPlace(gx, gy, isCave, a)
        elif from_map = 4uy then
            let dest, isCave, a = dest()
            printfn "under Tantagel: %s" dest
            match mapCoords.TryGetValue(Constants.MAP_LOCATIONS.TANTAGEL) with
            | true, (tx,ty) -> tryAllPlace(tx, ty, isCave, a)
            | _ -> tf <- fun (tx,ty) -> tryAllPlace(tx, ty, isCave, a)
        else
            if debugPrint || printIt then
                printfn ""

    let reachable_continents = Array2D.zeroCreate 120 120
    let walk(x, y, label) =
        let rec loop(x, y, label, acc, k) = 
            if x < 0 || y < 0 || x > 119 || y > 119 then  // outside map
                k acc
            elif reachable_continents.[x,y] <> 0 then     // already marked
                k acc
            elif not(tiles.[y,x].IsWalkable) then         // can't be walked
                k acc
            else
                reachable_continents.[x,y] <- label
                loop(x-1,y,label,acc, (fun acc -> 
                loop(x+1,y,label,acc, (fun acc -> 
                loop(x,y-1,label,acc, (fun acc -> 
                loop(x,y+1,label,acc, (fun acc -> k (acc+1)))))))))
        if reachable_continents.[x,y] <> 0 then
            failwith "bad walk() call"
        loop(x, y, label, 0, fun s -> s)
    let tx,ty = mapCoords.[Constants.MAP_LOCATIONS.TANTAGEL]
    let nx,ny = try mapCoords.[Constants.MAP_LOCATIONS.SWAMP_NORTH] with _ -> -1,-1
    let sx,sy = try mapCoords.[Constants.MAP_LOCATIONS.SWAMP_SOUTH] with _ -> -1,-1
    let gx,gy = mapCoords.[Constants.MAP_LOCATIONS.GARINHAM]
    let cont_1_size = walk(tx, ty, 1)  // label tantagel as continent 1
    let cont_2_size = 
        if nx <> -1 && reachable_continents.[nx,ny]=0 then
            walk(nx, ny, 2)  // swamp N not on tantagel continent, label 2
        elif sx <> -1 && reachable_continents.[sx,sy]=0 then
            walk(sx, sy, 2)  // swamp S not on tantagel continent, label 2
        elif reachable_continents.[gx,gy]=0 then
            walk(gx, gy, 2)  // garinham not on tantagel continent, label 2
        else
            0  // single continent
    (*
    let kx,ky = mapCoords.[Constants.MAP_LOCATIONS.KOL]
    let bx,by = mapCoords.[Constants.MAP_LOCATIONS.BRECCONARY]
    printfn "Kol   is on continent %d" reachable_continents.[kx,ky]
    printfn "Brecc is on continent %d" reachable_continents.[bx,by]
    *)
    let compute_charlock_distance_to_inn() =
        let visited = Array2D.zeroCreate 120 120
        let cx,cy = mapCoords.[Constants.MAP_LOCATIONS.CHARLOCK]
        let hx,hy = mapCoords.[Constants.MAP_LOCATIONS.HAUKSNESS]
        let cx = cx + 3 // reachable location on start map across to-be-created bridge
        let q = new System.Collections.Generic.Queue<_>()
        q.Enqueue( (cx,cy,3) )
        let mutable distance, finished = 999, false
        while not finished && not(q.Count = 0) do
            let x, y, dist = q.Dequeue()
            distance <- dist
            if not visited.[x,y] then
                visited.[x,y] <- true
                match tiles.[x,y] with
                | Constants.OverworldMapTile.Castle -> finished <- true  // must be tantagel, charlock not reachable
                | Constants.OverworldMapTile.Town when not(x=hx && y=hy) -> finished <- true // in town
                | _ -> ()
                let can_walk(x,y) = x>=0 && y>=0 && x<=119 && y<=119 && tiles.[x,y].IsWalkable 
                if not finished then
                    if can_walk(x+1,y) then q.Enqueue(x+1,y,dist+1)
                    if can_walk(x-1,y) then q.Enqueue(x-1,y,dist+1)
                    if can_walk(x,y+1) then q.Enqueue(x,y+1,dist+1)
                    if can_walk(x,y-1) then q.Enqueue(x,y-1,dist+1)
        distance
  
    let BURIED_COLOR = System.Drawing.Color.Orange
    if buried_dx <> -999 then  
        bmp2.SetPixel(EDGE+tx+buried_dx, EDGE+ty+buried_dy, BURIED_COLOR)

    // remove unreachable continents
    // TODO ideally do this before applying labels, both to avoid edge-of-unreachable-under-letter appearing, and also so could color two continents two different colors
    for x = 0 to 119 do
        for y = 0 to 119 do
            if reachable_continents.[x,y] = 0 && bmp2.GetPixel(x+EDGE,y+EDGE).ToArgb() = Constants.OverworldMapTile.Desert.AltProjectionColor.ToArgb() then
                // charlock is 'unreachable', but dont uncolor it
                let cx,cy = mapCoords.[Constants.MAP_LOCATIONS.CHARLOCK]
                if abs(x-cx) < 4 && abs(y-cy) < 4 then
                    () // do nothing, dont discolor charlock
                else
                    bmp2.SetPixel(x+EDGE,y+EDGE, Constants.OverworldMapTile.Mountain.AltProjectionColor)
    // gridlines
    let darken(c:System.Drawing.Color) = 
        let F(b:byte) = int b * 7 / 8
        System.Drawing.Color.FromArgb(F c.R, F c.G, F c.B)
    for i = 0 to 8 do
        for y = EDGE+0 to EDGE+119 do
            let x = EDGE+i*15
            bmp1.SetPixel(x, y, darken(bmp1.GetPixel(x,y)))
            if pixelsWhereLabelsHaveModified.Contains(x,y) then
                () // do nothing, dont re-color labels
            else
                bmp2.SetPixel(x, y, darken(bmp2.GetPixel(x,y)))
    for x = EDGE+0 to EDGE+119 do
        for j = 0 to 8 do
            let y = EDGE+j*15
            bmp1.SetPixel(x, y, darken(bmp1.GetPixel(x,y)))
            bmp2.SetPixel(x, y, darken(bmp2.GetPixel(x,y)))
    // redden charlock-enemy zones, bluen-grid metal-slime zones 
    let redden(c:System.Drawing.Color) = 
        let F(b:byte) = int b * 29 / 32
        System.Drawing.Color.FromArgb(int c.R, F c.G, F c.B)
    let bluen(c:System.Drawing.Color) = 
        let F(b:byte) = int b * 29 / 32
        System.Drawing.Color.FromArgb(F c.R, F c.G, int c.B)
    for j = 0 to 7 do
        for i = 0 to 7 do
            for y = EDGE+15*j to EDGE+15*j+14 do
                for x = EDGE+15*i to EDGE+15*i+14 do
                    if pixelsWhereLabelsHaveModified.Contains(x,y) then
                        () // do nothing, dont re-color labels
                    else
                        if true then // false to switch to ad-hoc
                            if x%2=0 || y%2 = 0 then
                                for r = 1 to zone_has_charlock_enemy.[ int ow_zones.[i,j] ] do
                                    bmp2.SetPixel(x, y, redden(bmp2.GetPixel(x,y)))
                            if (x+y)%2=0 then
                                for r = 1 to zone_has_metal_slime.[ int ow_zones.[i,j] ] do
                                    bmp2.SetPixel(x, y, bluen(bmp2.GetPixel(x,y)))
                        else
                            if x%2=0 || y%2 = 0 then
                                for r = 1 to zone_has_x.[ int ow_zones.[i,j] ] do
                                    bmp2.SetPixel(x, y, redden(bmp2.GetPixel(x,y)))
                            if (x+y)%2=0 then
                                for r = 1 to zone_has_y.[ int ow_zones.[i,j] ] do
                                    bmp2.SetPixel(x, y, bluen(bmp2.GetPixel(x,y)))

(*
// TODO extract enemy zones from seed DWRando.414823156739942.CDFGMPRWZ

[11:42 PM] Lorgon111: it could be just confirmation bias, but i feel like i have seen too many 'really unusual' things happen to just be explained by chance, and i like tinkering, so as i find time i'll dig deeper into rng algorithm it uses to try to find biases
[11:53 PM] Lorgon111: @up2ng you saw 25 magidrakees before earning any experience!
[11:53 PM] Lorgon111: here are the encounters:
[11:54 PM] Lorgon111: d = drakee, s = scorpion, # = magidrakee, r = red slime
1,d,2,s,3
hard save
4,5,6,7,8
reset
9,10
reset
11
reset
12,13,14,15
[11:55 PM] Lorgon111: reset
16,17,18,s,s,s
reset
19,20,21,22,23,24,25,r
[11:55 PM] Lorgon111: you were definitely in an rng-pit.
[11:56 PM] Lorgon111: when the music cycles through almost the entire overworld music before an encounter, this can signify one of the rng-pits (based on what I have read).  the algorithm always selects enemy #5 from the zone after the many-step-no-encounter thingy
[11:57 PM] Lorgon111: i think your reset saved you into that rng-pit, so your six post-resets have highly correlated outcomes
[11:58 PM] Lorgon111: I think magidrakke was most likely enemy #3 and #5 in the rom storage.  I need to write code to extract the enemy data yet to see if that is confirmed... enemy #3 is most probable (25%), and enemy #5 is always selected after rng-pit.

*)

    let monster_data = bytes.[0x5E5B..0x60DB]
    //Strength, Agility, HP, spells, resistance, dodge, xp, gold, 8 bytes of graphics
    // XX-- ----    00 = sleep, 01 = stopspell, 10 = heal, 11 = healmore
    // ---- XX--    00 = hurt, 01 = hurtmore, 10 = weak breath, 11 = dl2 breath
    // --XX ----    0, 25, 50, 75%
    // ---- --XX    0, 25, 50, 75%

    let spell(b) =
        let util_spell = (b &&& 0xc0uy) / 64uy
        let util_pct   = (b &&& 0x30uy) / 16uy
        let attack     = (b &&& 0x0cuy) / 4uy
        let attack_pct = (b &&& 0x03uy)
        let pct(x) = 
            match x with
            | 0uy -> 0
            | 1uy -> 25
            | 2uy -> 50
            | 3uy -> 75
        let util(x) = 
            match x with
            | 0uy -> "SLEEP"
            | 1uy -> "STOPSPELL"
            | 2uy -> "HEAL"
            | 3uy -> "HEALMORE"
        let atk(x) = 
            match x with
            | 0uy -> "HURT"
            | 1uy -> "HURTMORE"
            | 2uy -> "WEAK BREATH"
            | 3uy -> "DL2 BREATH"
        let sb = new System.Text.StringBuilder()
        let u = pct(util_pct)
        let a = pct(attack_pct)
        match u, a with
        | 0, 0 -> ""
        | 0, _ -> sprintf "%d%% %s" a (atk attack)
        | _, 0 -> sprintf "%d%% %s" u (util util_spell)
        | _, _ -> sprintf "%d%% %s, %d%% %s" a (atk attack) u (util util_spell)

    // sleep/stopspell resists, sleep is high nibble
    for x in [0..39] do
        let golem = monster_data.[16*x..16*x+15]
        printfn "%16s: str %3d agi %3d hp %3d spells %2x s_ss_resist %2x dodge_mag_phys %2x xp %3d gold %3d" (let n,_,_,_,_,_=EnemyData.ENEMY_DATA.[x] in n) golem.[0] golem.[1] golem.[2] golem.[3] golem.[4] golem.[5] golem.[6] golem.[7]
        match spell(golem.[3]) with
        | "" -> ()
        | s -> printfn "                                              %s" s

    show_go_mode_stats(bytes, true, file) |> ignore

// TODO how fast is start (keys, sword, zones, swamp, hurtmore ...)

(*
    // make minor change to map warps
    let new_bytes = Array.copy bytes
    printfn "tantagel at %d %d" tx ty
#if OVERWORLD_SHORTCUT
    new_bytes.[WARPS_INDEX_IN_BYTES + uc1i*3] <- 1uy
    new_bytes.[WARPS_INDEX_IN_BYTES + uc1i*3 + 1] <- byte tx + 2uy
    new_bytes.[WARPS_INDEX_IN_BYTES + uc1i*3 + 2] <- byte ty
    new_bytes.[WARPS_INDEX_IN_BYTES + uc1i*3 + 153] <- 1uy
    new_bytes.[WARPS_INDEX_IN_BYTES + uc1i*3 + 154] <- byte tx + 2uy
    new_bytes.[WARPS_INDEX_IN_BYTES + uc1i*3 + 155] <- byte ty + 2uy

    new_bytes.[WARPS_INDEX_IN_BYTES + uc2i*3] <- 1uy
    new_bytes.[WARPS_INDEX_IN_BYTES + uc2i*3 + 1] <- byte tx + 2uy
    new_bytes.[WARPS_INDEX_IN_BYTES + uc2i*3 + 2] <- byte ty + 2uy
    new_bytes.[WARPS_INDEX_IN_BYTES + uc2i*3 + 153] <- 1uy
    new_bytes.[WARPS_INDEX_IN_BYTES + uc2i*3 + 154] <- byte tx + 2uy
    new_bytes.[WARPS_INDEX_IN_BYTES + uc2i*3 + 155] <- byte ty
#else
    //  15  15   1  16   8   0
    //  16   9   1  17   2   2
    // put a warp next to start that goes to charlock mid-stairs
    new_bytes.[WARPS_INDEX_IN_BYTES + uc1i*3] <- 1uy
    new_bytes.[WARPS_INDEX_IN_BYTES + uc1i*3 + 1] <- byte tx + 2uy
    new_bytes.[WARPS_INDEX_IN_BYTES + uc1i*3 + 2] <- byte ty
    new_bytes.[WARPS_INDEX_IN_BYTES + uc1i*3 + 153] <- 16uy
    new_bytes.[WARPS_INDEX_IN_BYTES + uc1i*3 + 154] <- 8uy
    new_bytes.[WARPS_INDEX_IN_BYTES + uc1i*3 + 155] <- 0uy
    // put a warp from second charlock stairs two steps away to rimuldar
    new_bytes.[WARPS_INDEX_IN_BYTES + uc2i*3] <- 16uy
    new_bytes.[WARPS_INDEX_IN_BYTES + uc2i*3 + 1] <- 9uy
    new_bytes.[WARPS_INDEX_IN_BYTES + uc2i*3 + 2] <- 1uy
    // rimu: 11  29  14
    new_bytes.[WARPS_INDEX_IN_BYTES + uc2i*3 + 153] <- 11uy
    new_bytes.[WARPS_INDEX_IN_BYTES + uc2i*3 + 154] <- 29uy
    new_bytes.[WARPS_INDEX_IN_BYTES + uc2i*3 + 155] <- 14uy
    // observed: when you step off rimuldar map, you are warped to outside it on overworld map
    // but what if rimu not on overworld? make rimu overworld tile also go to elsewhere
    new_bytes.[WARPS_INDEX_IN_BYTES + rimi*3 + 153] <- 16uy
    new_bytes.[WARPS_INDEX_IN_BYTES + rimi*3 + 154] <- 8uy
    new_bytes.[WARPS_INDEX_IN_BYTES + rimi*3 + 155] <- 0uy
    // observed: then you step off rimuldar map, you are warped back to those charlock stairs
    // thus it seems town exits simply search all the warps to find one that goes into town, and reverses it to exit... if more than one, chooses first
    // thus town-inside-a-dungeon is possible
#endif

    // offset 1534 is one tile of grass (0,0), change to one tile of stairs (c,0)
    new_bytes.[16 + 0x1d5d + 1534] <- 0xc0uy
    new_bytes.[16 + 0x1d5d + 1568] <- 0xc0uy

    let new_file = file+".new.nes"
    System.IO.File.WriteAllBytes(new_file, new_bytes)
*)

    bmp1, bmp2, reachable_continents, mapCoords, cont_1_size, cont_2_size, compute_charlock_distance_to_inn()

(*

the prng subroutine, extracted from fceux emulator using write-breakpoint from hex editor

 03:C55B:A5 95     LDA $0095 = #$1A
 03:C55D:85 3D     STA $003D = #$1A
 03:C55F:A5 94     LDA $0094 = #$B8
 03:C561:85 3C     STA $003C = #$B8
>03:C563:06 94     ASL $0094 = #$B8
 03:C565:26 95     ROL $0095 = #$1A
 03:C567:18        CLC
 03:C568:65 94     ADC $0094 = #$B8
 03:C56A:85 94     STA $0094 = #$B8
 03:C56C:A5 95     LDA $0095 = #$1A
 03:C56E:65 3D     ADC $003D = #$1A
 03:C570:85 95     STA $0095 = #$1A
 03:C572:A5 94     LDA $0094 = #$B8
 03:C574:18        CLC
 03:C575:65 95     ADC $0095 = #$1A
 03:C577:85 95     STA $0095 = #$1A
 03:C579:A5 94     LDA $0094 = #$B8
 03:C57B:18        CLC
 03:C57C:69 81     ADC #$81
 03:C57E:85 94     STA $0094 = #$B8
 03:C580:A5 95     LDA $0095 = #$1A
 03:C582:69 00     ADC #$00
 03:C584:85 95     STA $0095 = #$1A
 03:C586:60        RTS ---------------

*)

let simulate_prng(init_a94, init_a95) =
    // the seed is two bytes located at addresses 0x94 and 0x95
    // a temporary snapshot of the prior seed lives in 0x3c and 0x3d

    // simulate bytes with ints to do my own work with carry
    let mutable a94, a95, a3c, a3d, r = init_a94, init_a95, 0, 0, 0    // r is the 'A' register, used for all computation
    let mutable carry = false

    // LDA $0095
    // STA $003D
    a3d <- a95
    // LDA $0094
    // STA $003C
    a3c <- a94
    r <- a94
    // ASL $0094
    a94 <- a94 * 2
    if a94 > 255 then 
        a94 <- a94 - 256
        carry <- true
    // ROL $0095
    let prior_carry = carry
    a95 <- a95 * 2
    if a95 > 255 then 
        a95 <- a95 - 256
        carry <- true
    if prior_carry then
        a95 <- a95 + 1
    // CLC
    carry <- false
    // ADC $0094
    r <- r + a94
    if r > 255 then
        r <- r - 256
        carry <- true
    // STA $0094
    a94 <- r
    // LDA $0095
    r <- a95
    // ADC $003D
    r <- r + a3d + (if carry then 1 else 0)
    if r > 255 then
        r <- r - 256
        carry <- true
    // STA $0095
    a95 <- r
    // LDA $0094
    r <- a94
    // CLC
    carry <- false
    // ADC $0095
    r <- r + a95
    if r > 255 then
        r <- r - 256
        carry <- true
    // STA $0095
    a95 <- r
    // LDA $0094
    r <- a94
    // CLC
    carry <- false
    // ADC #$81
    r <- r + 0x81   // add 129
    if r > 255 then
        r <- r - 256
        carry <- true
    // STA $0094
    a94 <- r
    // LDA $0095
    r <- a95
    // ADC #$00
    r <- r + 0 + (if carry then 1 else 0)
    if r > 255 then
        r <- r - 256
        carry <- true
    // STA $0095
    a95 <- r
    // RTS -----
    a94, a95

let test_rng() = 
    let nexta94, nexta95 = simulate_prng(0xa9, 0x78)
    printfn "%x %x" nexta94 nexta95   // 7c 65

    let nexta94, nexta95 = simulate_prng(0x7c, 0x65)
    printfn "%x %x" nexta94 nexta95   // f5 a4

    let nexta94, nexta95 = simulate_prng(0xf5, 0xa4)
    printfn "%x %x" nexta94 nexta95   // 60 ce

    let mutable x, y = 0x60, 0xce
    for i = 1 to 100 do
        for j = 1 to 20 do
            let nx, ny = simulate_prng(x, y)
            x <- nx
            y <- ny
            printfn "%x %x" x y
        printfn "...%d iterations..." (i*20)
        System.Console.ReadLine() |> ignore

let simulate_prng_int(seed) =
    let a94 = (seed &&& 0x0000ff00) / 0x0100
    let a95 = seed &&& 0x000000ff
    let na94, na95 = simulate_prng(a94, a95)
    let next = na94 * 0x0100 + na95
    next

let show_rng() =
    let SHOW_ONLY_EVERY_16 = false
    let mutable x = 0 
    for j = 1 to 1024 do
        for i = 1 to 32 do
            if not SHOW_ONLY_EVERY_16 || (i=16 || i=32) then
                printfn "%10d     %8x" x x
            x <- simulate_prng_int(x)
        System.Console.ReadLine() |> ignore

let test_period(init_seed) =
    let mutable count = 0
    let set = new System.Collections.Generic.HashSet<int>()
    let mutable seed = init_seed
    while set.Add(seed) do
        seed <- simulate_prng_int(seed)
        count <- count + 1
    printfn "period was %d" count
