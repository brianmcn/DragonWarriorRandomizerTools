module ROM_mods

type Assembly = 
    | Label of int
    | BNE of int
    | Instr1 of byte
    | Instr2 of byte*byte
    | Instr3 of byte*byte*byte
    member this.Size =
        match this with
        | Label _ -> 0
        | BNE _ -> 2
        | Instr1 _ -> 1
        | Instr2 _ -> 2
        | Instr3 _ -> 3

let backpatch (a:Assembly[]) =
    // given an array of Assembly where Label and BNE carry labels, convert to an array of Assembly where Labels are removed and BNE carry offsets
    let numInstructions = a |> Array.sumBy (fun x -> match x with Label _ -> 0 | _ -> 1)
    let offsets = Array.zeroCreate numInstructions
    // compute label offsets (zero is first instruction byte)
    let mutable curOffset = 0
    let labelOffsets = new System.Collections.Generic.Dictionary<int,int>()
    for x in a do
        match x with
        | Label n ->
            if labelOffsets.ContainsKey(n) then failwithf "duplicate label %d" n
            labelOffsets.Add(n, curOffset)
        | _ -> 
            curOffset <- curOffset + x.Size 
    // make a new set of Assembly with Labels removed and BNEs patched
    let newInstructions = ResizeArray()
    curOffset <- 0
    for x in a do
        curOffset <- curOffset + x.Size   // do this up front, BNE offsets are from the subsequent instruction
        match x with
        | Label n -> ()
        | BNE n -> 
            match labelOffsets.TryGetValue(n) with
            | true, addr ->
                let diff = addr - curOffset
                newInstructions.Add(BNE diff)
            | false, _ -> failwithf "bad destination label %d" n
        | _ ->
            newInstructions.Add(x)
    newInstructions.ToArray()

let I1 x = Instr1(byte x)
let I2 x y = Instr2(byte x, byte y)
let I3 x y z = Instr3(byte x, byte y, byte z)

let swampToDesertAssembly = 
    // goal - keep an array of overworld tiles that have been transformed swamp to desert.
    // look them up and decode them on the fly during map decode tile lookup.
    // for proof of concept, this code could replace existing gwaelin & hidden stairs tile-replacement code (and break those features).
    [|
        I2 0xC9 0x06        // CMP #$06           ;C9 06             compare swamp  (swamp is 'border tile 6' as per https://github.com/mcgrew/dwrandomizer/blob/master/notes/maps.txt )
        BNE 9               // BNE $02D23         ;D0 TODO           if not, go on to treasure chests
        I2 0xA5 0x45        // LDA $45            ;A5 45             load map
        I2 0xC9 0x01        // CMP #$01           ;C9 01             compare overworld
        BNE 9               // BNE $02D3C         ;D0 TODO           if not, go on to treasure chests
        I2 0xA0 0x00        // LDY #$00           ;A0 00             set Y to 0
        Label 0
        I2 0xA5 0x42        // LDA $42            ;A5 42             load x coord
        I3 0xD9 0x00 0x70   // CMP $7000,Y        ;D9 00 70          compare to array of coords starting at $7000    TODO see if memory after 7000 is safe to write (unused)
        BNE 1               // BNE $TODO          ;D0 TODO           if not, jump to first INY below (increment array index to next coord set)
        I1 0xC8             // INY                ;C8                inc y
        I2 0xA5 0x43        // LDA $43            ;A5 43             load y coord
        I3 0xD9 0x00 0x70   // CMP $7000,Y        ;D9 00 70          compare to array of coords starting at $7000    TODO see if memory after 7000 is safe to write (unused)
        BNE 2               // BNE $TODO          ;D0 TODO           if not, jump to second INY below (increment array index to next coord set)
        I2 0xA9 0x01        // LDA #$01           ;A9 01             load desert
        I2 0x85 0x3C        // STA $3C            ;85 3C             store desert as tile result
        I1 0x68             // PLA                ;68                |
        I1 0xA8             // TAY                ;A8                |- subroutine exit
        I1 0x60             // RTS                ;60                |
        Label 1
        I1 0xC8             // INY                ;C8                move down coord array
        Label 2
        I1 0xC8             // INY                ;C8                move down coord array
        I2 0xC0 0x10        // CPY #$10           ;C0 10             compare Y - TODO decide how large an array of swamp->desert tiles we want, '10' means 8 tileswaps (16 coords = 8 xy pairs)
        BNE 0               // BNE $TODO          ;D0 TODO           if not at end of array, go back up to LDA $42 
        Label 9
        I1 0xEA             // NOP                ; TODO as many NOPs as needed to take up same size instruction space as what we replaced/overwrote to hack this in
    |]

let showBackpatch() =
    let patched = backpatch(swampToDesertAssembly)
    for x in patched do
        match x with
        | Label _ -> failwith "impossible"
        | BNE offset -> 
            printfn "D0 %02X" (if offset >=0 then offset else offset + 256)
        | Instr1(x) ->
            printfn "%02X" x
        | Instr2(x,y) ->
            printfn "%02X %02X" x y
        | Instr3(x,y,z) ->
            printfn "%02X %02X %02X" x y z

let makePatchedBytes(length) =
    let patched = backpatch(swampToDesertAssembly)
    let bytes = ResizeArray()
    for x in patched do
        match x with
        | Label _ -> failwith "impossible"
        | BNE offset -> 
            bytes.Add(0xD0uy)
            bytes.Add(if offset >=0 then byte(offset) else byte(offset + 256))
        | Instr1(x) ->
            bytes.Add(byte x)
        | Instr2(x,y) ->
            bytes.Add(byte x)
            bytes.Add(byte y)
        | Instr3(x,y,z) ->
            bytes.Add(byte x)
            bytes.Add(byte y)
            bytes.Add(byte z)
    if bytes.Count > length then
        failwith "tried to squeeze too much code into too small a length"
    while bytes.Count < length do
        bytes.Add(0xEAuy) // EA = NOP
    bytes.ToArray()

let patch_rom(file) =
    let bytes = System.IO.File.ReadAllBytes(file)
    // want to write [$02CF1..$02D23)
    let length = 0x02D23 - 0x02CF1
    let offset = 0x02CF1 + 16 // first 16 bytes are a header
    // do minor verification that the code we expect to be there is there
    if bytes.[offset..offset+11] = Array.map byte [| 0xC9; 0x17; 0xD0; 0x0C; 0xA5; 0xDF; 0x29; 0x03; 0xF0; 0x41; 0xA9; 0x04 |] then
        let replacementBytes = makePatchedBytes(length)
        for i = 0 to length-1 do
            bytes.[offset+i] <- replacementBytes.[i]
        // but it turns out the code I patched in only runs in the non-overworld-map branch.  so to continue the hack, change the 'return' at the end of the overworld map part, namely
        //    $02C95: PLA TAY RTS
        // to a jump to my code which will also eventually end the subroutine similarly
        //    JMP ACF1    // 4C F1 AC
        // NOTE: this means we are changing the code path a bit.  the treasure-chest and magic-door code does not inspect the map, so it will be running here, but given that
        // transition-to-overworld is the thing that resets those banks, I don't think it should interact.  but i've not looked into debugger to verify that those locations are already reset.
        let offset = 0x02C95 + 16
        if bytes.[offset..offset+2] = [| 0x68uy; 0xA8uy; 0x60uy |] then
            bytes.[offset+0] <- 0x4Cuy // JMP
            bytes.[offset+1] <- 0xF1uy // 
            bytes.[offset+2] <- 0xACuy // ACF1 is the location 2CF1 in my code to JMP to
        else
            failwith "unexpected ROM code found"
        System.IO.File.WriteAllBytes(file+".patched.nes", bytes)
    else
        failwith "unexpected ROM code found"
