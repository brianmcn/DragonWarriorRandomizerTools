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

    let WARPS_INDEX_IN_BYTES = 0xf3d8
    let warps = bytes.[WARPS_INDEX_IN_BYTES..]
    let mutable gx,gy,tx,ty = -1, -1, -1, -1  // garin and tantagel coords
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
        let printIt = from_map = 1uy || from_map = 9uy || from_map = 4uy 
        let debugPrint = true
(*
 15  13   7  16   4   4
 15  19   7  16   9   8
 *)
        if from_map = 15uy && from_x = 13uy && from_y = 7uy then
            uc1i <- i
        if from_map = 15uy && from_x = 19uy && from_y = 7uy then
            uc2i <- i
        if from_map = 1uy && from_x = 22uy && from_y = 47uy then // rimu is at 1  22  47  on that one seed
            rimi <- i
        if debugPrint || printIt then
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
            | 6uy -> "Charlock Throne", false, null  // in short-charlock
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
            if debugPrint || printIt then
                printfn ""
  
    if buried_dx <> -999 then  
        bmp2.SetPixel(EDGE+tx+buried_dx, EDGE+ty+buried_dy, System.Drawing.Color.Orange )

    // gridlines
    let darken(c:System.Drawing.Color) = 
        let F(b:byte) = int b * 7 / 8
        System.Drawing.Color.FromArgb(F c.R, F c.G, F c.B)
    for i = 0 to 8 do
        for y = EDGE+0 to EDGE+119 do
            let x = EDGE+i*15
            bmp1.SetPixel(x, y, darken(bmp1.GetPixel(x,y)))
            bmp2.SetPixel(x, y, darken(bmp2.GetPixel(x,y)))
    for x = EDGE+0 to EDGE+119 do
        for j = 0 to 8 do
            let y = EDGE+j*15
            bmp1.SetPixel(x, y, darken(bmp1.GetPixel(x,y)))
            bmp2.SetPixel(x, y, darken(bmp2.GetPixel(x,y)))


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
    // sleep/stopspell rsists, sleep is high nibble
    for x in [23; 24] do
        let golem = monster_data.[16*x..16*x+15]
        printfn "%s: str %d agi %d hp %d spells %d s_ss_resist %x dodge_mag_phys %x xp %d gold %d" (let n,_,_,_,_,_=EnemyData.ENEMY_DATA.[x] in n) golem.[0] golem.[1] golem.[2] golem.[3] golem.[4] golem.[5] golem.[6] golem.[7]
        // golem: 68 56 40 234 80 52 72 120
        // golem: 120 60 153 239 245 240 255 10

    let zone0 = content.[0xf54f..0xf54f+4]
    let zone1 = content.[0xf54f+5..0xf54f+9]
    let zone2 = content.[0xf54f+10..0xf54f+14]
    printfn "enemies in zone 0 1 2"
    for enemy in zone0 do
        printfn "%3d  %s" enemy (let name,_,_,_,_,_ = EnemyData.ENEMY_DATA.[int enemy] in name)
    printfn "-----"
    for enemy in zone1 do
        printfn "%3d  %s" enemy (let name,_,_,_,_,_ = EnemyData.ENEMY_DATA.[int enemy] in name)
    printfn "-----"
    for enemy in zone2 do
        printfn "%3d  %s" enemy (let name,_,_,_,_,_ = EnemyData.ENEMY_DATA.[int enemy] in name)

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


    bmp1, bmp2

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

let test_period(init_seed) =
    let mutable count = 0
    let set = new System.Collections.Generic.HashSet<int>()
    let mutable seed = init_seed
    while set.Add(seed) do
        seed <- simulate_prng_int(seed)
        count <- count + 1
    printfn "period was %d" count
