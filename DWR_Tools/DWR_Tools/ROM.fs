module ROM

let decode_rom(file) =
    let bytes = System.IO.File.ReadAllBytes(file)
    let content = bytes.[16..]   // first 16 bytes are a header
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
        printfn "row: %d" row
        let mutable offset = int(map_pointers.[row] - uint16 0x9d5d)
//        printfn "offset: %d" offset
        let mutable x = 0
        while x < 120 do
            let cur_byte = encoded.[offset]
            let tile = cur_byte >>> 4
            let count = int(cur_byte &&& 0xfuy) + 1
            printfn "x %d tile %d count %d" x tile count
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

    let bmp = new System.Drawing.Bitmap(120,120)
    for x = 0 to 119 do
        for y = 0 to 119 do
            bmp.SetPixel(y, x, tiles.[x,y].ProjectionColor)
    bmp

(*
*)