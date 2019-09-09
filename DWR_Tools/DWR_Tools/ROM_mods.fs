module ROM_mods

//////////////////////////////////////////////////////////////////
// Basic assembler to make it easier to write code with branches
//////////////////////////////////////////////////////////////////

type Assembly = 
    | Label of int
    | BNE of int
    | BEQ of int
    | BPL of int
    | BMI of int
    | Instr1 of byte
    | Instr2 of byte*byte
    | Instr3 of byte*byte*byte
    member this.Size =
        match this with
        | Label _ -> 0
        | BNE _ -> 2
        | BEQ _ -> 2
        | BPL _ -> 2
        | BMI _ -> 2
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
        | BEQ n -> 
            match labelOffsets.TryGetValue(n) with
            | true, addr ->
                let diff = addr - curOffset
                newInstructions.Add(BEQ diff)
            | false, _ -> failwithf "bad destination label %d" n
        | BPL n -> 
            match labelOffsets.TryGetValue(n) with
            | true, addr ->
                let diff = addr - curOffset
                newInstructions.Add(BPL diff)
            | false, _ -> failwithf "bad destination label %d" n
        | BMI n -> 
            match labelOffsets.TryGetValue(n) with
            | true, addr ->
                let diff = addr - curOffset
                newInstructions.Add(BMI diff)
            | false, _ -> failwithf "bad destination label %d" n
        | _ ->
            newInstructions.Add(x)
    newInstructions.ToArray()

let I1 x = Instr1(byte x)
let I2 x y = Instr2(byte x, byte y)
let I3 x y z = Instr3(byte x, byte y, byte z)

let showBackpatch(assembly) =
    let patched = backpatch(assembly)
    for x in patched do
        match x with
        | Label _ -> failwith "impossible"
        | BNE offset -> 
            printfn "D0 %02X" (if offset >=0 then offset else offset + 256)
        | BEQ offset -> 
            printfn "F0 %02X" (if offset >=0 then offset else offset + 256)
        | BPL offset -> 
            printfn "10 %02X" (if offset >=0 then offset else offset + 256)
        | BMI offset -> 
            printfn "30 %02X" (if offset >=0 then offset else offset + 256)
        | Instr1(x) ->
            printfn "%02X" x
        | Instr2(x,y) ->
            printfn "%02X %02X" x y
        | Instr3(x,y,z) ->
            printfn "%02X %02X %02X" x y z

let makePatchedBytes(code,length) =
    let patched = backpatch(code)
    let bytes = ResizeArray()
    for x in patched do
        match x with
        | Label _ -> failwith "impossible"
        | BNE offset -> 
            bytes.Add(0xD0uy)
            bytes.Add(if offset >=0 then byte(offset) else byte(offset + 256))
        | BEQ offset -> 
            bytes.Add(0xF0uy)
            bytes.Add(if offset >=0 then byte(offset) else byte(offset + 256))
        | BPL offset -> 
            bytes.Add(0x10uy)
            bytes.Add(if offset >=0 then byte(offset) else byte(offset + 256))
        | BMI offset -> 
            bytes.Add(0x30uy)
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

//////////////////////////////////////////////////////////////////
//
// Feature: When a player dies on the overworld to swamp damage,
//   that swamp tile is converted to a desert tile.  Up to 15
//   tiles may be converted (after that, no more changes are made).
//
// Intent: Get rid of having to 'reseed' in bad swamp starts.
//   Also may create strategic overworld grind possibilities.
//
// Details: overworld map coordinates that have been changed are
//   stored to battery-backed RAM on a per-adventure-log-save basis.
//
//////////////////////////////////////////////////////////////////

// Implemented in 3 parts.  
// First, a routine called when the player takes swamp damage:
let swampToDesertAssemblyWrite = 
    [|
        I2 0xA5 0xC5        // LDA $C5            ;A5 C5             load player health
        I2 0xC9 0x00        // CMP #$00           ;C9 00             compare to zero
        BNE 9               // BNE $0TODO         ;D0 TODO           if not, done
        I2 0xA5 0x45        // LDA $45            ;A5 45             load map
        I2 0xC9 0x01        // CMP #$01           ;C9 01             compare overworld
        BNE 9               // BNE $0TODO         ;D0 TODO           if not, done
        // We died to swamp damage on the overworld. Load up the array of the current adventure log file, and see if there is any space left to write more coords.

        // Load up the appropriate array, based on which log file we're in
        I3 0xAD 0x39 0x60   // LDA $6039          ;AD 39 60          load adventure log file number
        I2 0xC9 0x02        // CMP #$02           ;C9 02             compare to log file 3
        BEQ 3               // BEQ $TODO          ;F0 TODO           if is, use code copy #3
        I2 0xC9 0x01        // CMP #$01           ;C9 01             compare to log file 2
        BEQ 2               // BEQ $TODO          ;F0 TODO           if is, use code copy #2
                            //                                       else fallthru to copy #1
        // The 'count' byte will be twice the number of coordinate pairs that have been written, e.g. an offset to the first 'unused' spot in the array.  So for example
        // in log file 2, if 703E contains the value 0C, it means 6 sets of coordinates have been written, and the next one is written to 7020+0C.  A value of 1E means 'full'.

        // 3 copies of same code for different array offsets
        Label 1
        I2 0xA0 0x1E        // LDY #$1E           ;A0 1E             set Y to 1E (offset into 7000 array to look up count)
        I2 0xA9 0x1E        // LDA #$1E           ;A9 1E             load MAX
        I3 0xD9 0x00 0x70   // CMP $7000,Y        ;D9 00 70          compare (have we already written 0x1E bytes of desert coords?)
        BEQ 9               // BEQ $TODO          ;F0 TODO           if so, done
        I2 0xA2 0x1E        // LDX #$1E           ;A2 1E             load MAX (to X)
        I3 0xBC 0x00 0x70   // LDY $7000,X        ;BC 00 70          read in (to Y) num-written-desert-bytes
        I2 0xA5 0x42        // LDA $42            ;A5 42             load map x coord
        I3 0x99 0x00 0x70   // STA $7000,Y        ;99 00 70          store it to my array
        I1 0xC8             // INY                ;C8                inc y
        I2 0xA5 0x43        // LDA $43            ;A5 43             load map y coord
        I3 0x99 0x00 0x70   // STA $7000,Y        ;99 00 70          store it to my array
        I1 0xC8             // INY                ;C8                inc y
        I3 0x8C 0x1E 0x70   // STY $701E          ;8C 1E 70          store new Y back into my num-written-desert-bytes cell
        // no 'uncondition branch' instruction, so do this to jump to bottom
        BEQ 9
        BNE 9

        Label 2
        I2 0xA0 0x1E        // LDY #$1E           ;A0 1E             set Y to 1E (offset into 7020 array to look up count)
        I2 0xA9 0x1E        // LDA #$1E           ;A9 1E             load MAX
        I3 0xD9 0x20 0x70   // CMP $7020,Y        ;D9 20 70          compare (have we already written 0x1E bytes of desert coords?)
        BEQ 9               // BEQ $TODO          ;F0 TODO           if so, done
        I2 0xA2 0x1E        // LDX #$1E           ;A2 1E             load MAX (to X)
        I3 0xBC 0x20 0x70   // LDY $7020,X        ;BC 20 70          read in (to Y) num-written-desert-bytes
        I2 0xA5 0x42        // LDA $42            ;A5 42             load map x coord
        I3 0x99 0x20 0x70   // STA $7020,Y        ;99 20 70          store it to my array
        I1 0xC8             // INY                ;C8                inc y
        I2 0xA5 0x43        // LDA $43            ;A5 43             load map y coord
        I3 0x99 0x20 0x70   // STA $7020,Y        ;99 20 70          store it to my array
        I1 0xC8             // INY                ;C8                inc y
        I3 0x8C 0x3E 0x70   // STY $703E          ;8C 3E 70          store new Y back into my num-written-desert-bytes cell
        // no 'uncondition branch' instruction, so do this to jump to bottom
        BEQ 9
        BNE 9

        Label 3
        I2 0xA0 0x1E        // LDY #$1E           ;A0 1E             set Y to 1E (offset into 7040 array to look up count)
        I2 0xA9 0x1E        // LDA #$1E           ;A9 1E             load MAX
        I3 0xD9 0x40 0x70   // CMP $7040,Y        ;D9 40 70          compare (have we already written 0x1E bytes of desert coords?)
        BEQ 9               // BEQ $TODO          ;F0 TODO           if so, done
        I2 0xA2 0x1E        // LDX #$1E           ;A2 1E             load MAX (to X)
        I3 0xBC 0x40 0x70   // LDY $7040,X        ;BC 40 70          read in (to Y) num-written-desert-bytes
        I2 0xA5 0x42        // LDA $42            ;A5 42             load map x coord
        I3 0x99 0x40 0x70   // STA $7040,Y        ;99 40 70          store it to my array
        I1 0xC8             // INY                ;C8                inc y
        I2 0xA5 0x43        // LDA $43            ;A5 43             load map y coord
        I3 0x99 0x40 0x70   // STA $7040,Y        ;99 40 70          store it to my array
        I1 0xC8             // INY                ;C8                inc y
        I3 0x8C 0x5E 0x70   // STY $705E          ;8C 5E 70          store new Y back into my num-written-desert-bytes cell
        // fall thru to bottom
            
        // the 'return'
        Label 9
        I3 0x20 0x74 0xFF   // JSR $FF74          ;20 74 FF          original JSR that I overwrote
        I3 0x4C 0xED 0xCD   // JMP $CDED          ;4C ED CD          jump back to instruction after one I overwrote
    |]

// Second, the routine to convert swamp-to-desert tiles on-the-fly as the overworld map is read:
let swampToDesertAssembly = 
    // This code is called at the end of the 'what overworld tile is this' lookup that checks the original map data.
    [|
        // If the tile is an overworld swamp
        I2 0xC9 0x06        // CMP #$06           ;C9 06             compare swamp  (swamp is 'border tile 6' as per https://github.com/mcgrew/dwrandomizer/blob/master/notes/maps.txt )
        BNE 9               // BNE $TODO          ;D0 TODO           if not, done
        I2 0xA5 0x45        // LDA $45            ;A5 45             load map
        I2 0xC9 0x01        // CMP #$01           ;C9 01             compare overworld
        BNE 9               // BNE $TODO          ;D0 TODO           if not, done
        // Load up the appropriate array, based on which log file we're in
        I3 0xAD 0x39 0x60   // LDA $6039          ;AD 39 60          load adventure log file number
        I2 0xC9 0x02        // CMP #$02           ;C9 02             compare to log file 3
        BNE 5               // BNE $TODO          ;D0 TODO           if not, keep going
        I2 0xA0 0x40        // LDY #$40           ;A0 40             set Y to 40
        Label 5
        I2 0xC9 0x01        // CMP #$01           ;C9 01             compare to log file 2
        BNE 6               // BNE $TODO          ;D0 TODO           if not, keep going
        I2 0xA0 0x20        // LDY #$20           ;A0 20             set Y to 20
        Label 6
        I2 0xC9 0x00        // CMP #$00           ;C9 00             compare to log file 1
        BNE 0               // BNE $TODO          ;D0 TODO           if not, keep going
        I2 0xA0 0x00        // LDY #$00           ;A0 00             set Y to 0
        // Compare to each and every saved coordinate in current array (array will start as all zeros, so the single upper left tile of overworld map will never be swamp, that's fine)
        Label 0
        I2 0xA5 0x42        // LDA $42            ;A5 42             load x coord
        I3 0xD9 0x00 0x70   // CMP $7000,Y        ;D9 00 70          compare to array of coords starting at $7000
        BNE 1               // BNE $TODO          ;D0 TODO           if not, jump to first INY below (increment array index to next coord set)
        I2 0xA5 0x43        // LDA $43            ;A5 43             load y coord
        I3 0xD9 0x01 0x70   // CMP $7001,Y        ;D9 01 70          compare to array of coords starting at $7000
        BNE 1               // BNE $TODO          ;D0 TODO           if not, jump to first INY below (increment array index to next coord set)
        I2 0xA9 0x01        // LDA #$01           ;A9 01             load desert
        I2 0x85 0x3C        // STA $3C            ;85 3C             store desert as tile result
        Label 1
        I1 0xC8             // INY                ;C8                move down coord array
        I1 0xC8             // INY                ;C8                move down coord array
        // Check for end of (any) array
        I2 0xC0 0x1E        // CPY #$1E           ;C0 1E             compare Y (see if at end of first array)
        BEQ 9               // BEQ $TODO          ;F0 TODO           if at end of array, go to return
        I2 0xC0 0x3E        // CPY #$3E           ;C0 3E             compare Y (see if at end of second array)
        BEQ 9               // BEQ $TODO          ;F0 TODO           if at end of array, go to return
        I2 0xC0 0x5E        // CPY #$5E           ;C0 5E             compare Y (see if at end of third array)
        BEQ 9               // BEQ $TODO          ;F0 TODO           if at end of array, go to return
        BNE 0               // BNE $TODO          ;D0 TODO           else, go back up to LDA $42 
        Label 9
        I1 0x68             // PLA                ;68                |
        I1 0xA8             // TAY                ;A8                |- subroutine exit
        I1 0x60             // RTS                ;60                |
    |]

// Finally, code to zero it out when an adventure log is cleared (without this code, if you start with gold, you can take some deaths to clear path out of swamp, erase save, 
//   restart, and walk the gold to a town, as the map is still changed even after the erased save; I don't want that behavior):
let clearArrayAssociatedWithALogFile = 
    [|
        I1 0x48             // PHA
        I1 0x8A             // TXA                ; same preamble as method we're hijacking (0xF80F), will jump past it in the original method at the end of this routine
        I1 0x48             // PHA

        // Load up the appropriate array, based on which log file we're in
        I3 0xAD 0x39 0x60   // LDA $6039          ;AD 39 60          load adventure log file number
        I2 0xC9 0x02        // CMP #$02           ;C9 02             compare to log file 3
        BEQ 3               // BEQ $TODO          ;F0 TODO           if is, use code copy #3
        I2 0xC9 0x01        // CMP #$01           ;C9 01             compare to log file 2
        BEQ 2               // BEQ $TODO          ;F0 TODO           if is, use code copy #2
                            //                                       else fallthru to copy #1
        // 3 copies of same code for different array offsets - zero out the 32 bytes that comprise the array
        Label 1
        I2 0xA0 0x1F        // LDY #$1F           ;A0 1F             set Y to 1F (last offset into 7000 array)
        I2 0xA9 0x00        // LDA #$00           ;A9 00             load 0
        Label 6
        I3 0x99 0x00 0x70   // STA $7000,Y        ;99 00 70          store it to my array
        I1 0x88             // DEY                ;88                decrement Y
        BPL 6               // BPL $TODO          ;10 TODO           branch on plus (while Y>=0)
        BMI 9               // BMI $TODO          ;30 TODO           branch on minus (jump to end)

        Label 2
        I2 0xA0 0x1F        // LDY #$1F           ;A0 1F             set Y to 1F (last offset into 7020 array)
        I2 0xA9 0x00        // LDA #$00           ;A9 00             load 0
        Label 7
        I3 0x99 0x20 0x70   // STA $7020,Y        ;99 20 70          store it to my array
        I1 0x88             // DEY                ;88                decrement Y
        BPL 7               // BPL $TODO          ;10 TODO           branch on plus (while Y>=0)
        BMI 9               // BMI $TODO          ;30 TODO           branch on minus (jump to end)

        Label 3
        I2 0xA0 0x1F        // LDY #$1F           ;A0 1F             set Y to 1F (last offset into 7040 array)
        I2 0xA9 0x00        // LDA #$00           ;A9 00             load 0
        Label 8
        I3 0x99 0x40 0x70   // STA $7040,Y        ;99 40 70          store it to my array
        I1 0x88             // DEY                ;88                decrement Y
        BPL 8               // BPL $TODO          ;10 TODO           branch on plus (while Y>=0)
                            // else fall thru to end
        // the 'return'
        Label 9
        I3 0x4C 0x12 0xF8   // JMP $F812          ;4C 12 F8          jump into middle of routine I hijacked, after preamble that I already copied at start of this routine
    |]

let patch_rom(file) =
    // Based on:
    //     https://github.com/mcgrew/dwrandomizer/blob/master/common/dwr.c
    //     /* Clear the unused code so we can make sure it's unused */
    //     memset(&rom.content[0xc288], 0xff, 0xc4f5 - 0xc288);
    let unused_min_offset = 0xC298      // 'content' is 0x10 after 'bytes' due to header
    let unused_end = 0xC505 

    let bytes = System.IO.File.ReadAllBytes(file)
    // when dying in swamp, write my extra desert tiles
    let length = 124
    // do minor verification that the code we expect to be there is there
    if bytes.[unused_min_offset..unused_min_offset+length-1] = Array.create length 0xFFuy then
        let replacementBytes = makePatchedBytes(swampToDesertAssemblyWrite,length)
        for i = 0 to length-1 do
            bytes.[unused_min_offset+i] <- replacementBytes.[i]
        // change the JSR after swamp damage, namely
        //    03:CDEA:20 74 FF  JSR $FF74
        // to a JMP to my code which will also eventually JMP back
        //    4C A0 C2          JMP $C2A0    // or wherever address my code finally lives
        let offset = 0x0CDEA + 16
        if bytes.[offset..offset+2] = [| 0x20uy; 0x74uy; 0xFFuy |] then
            bytes.[offset+0] <- 0x4Cuy // JMP
            let myoffset = unused_min_offset-16 // header adjust
            bytes.[offset+1] <- byte (myoffset % 256)
            bytes.[offset+2] <- byte ((myoffset / 256) + 0) // the location of my code to JMP to, converting bank 3 (0xCnnn) to bank 3 (0xCnnn)
        else
            failwith "unexpected ROM code found"
    else
        failwith "unexpected ROM code found"
    let unused_min_offset = unused_min_offset + length
    if unused_min_offset > unused_end then
        failwith "used too much space"

    // when reading overworld map, load my extra desert tiles
    let length = 68
    // do minor verification that the code we expect to be there is there
    if bytes.[unused_min_offset..unused_min_offset+length-1] = Array.create length 0xFFuy then
        let replacementBytes = makePatchedBytes(swampToDesertAssembly,length)
        for i = 0 to length-1 do
            bytes.[unused_min_offset+i] <- replacementBytes.[i]
        // change the 'return' at the end of the overworld map part, namely
        //    $02C95: PLA TAY RTS
        // to a jump to my code which will also eventually end the subroutine similarly
        //    JMP A580    // 4C 80 A5       // or wherever address my code finally lives
        let offset = 0x02C95 + 16
        if bytes.[offset..offset+2] = [| 0x68uy; 0xA8uy; 0x60uy |] then
            bytes.[offset+0] <- 0x4Cuy                        // JMP
            let myoffset = unused_min_offset-16 // header adjust
            bytes.[offset+1] <- byte (myoffset % 256)
            bytes.[offset+2] <- byte ((myoffset / 256) + 0) // the location of my code to JMP to, converting bank 3 (0xCnnn) to bank 3 (0xCnnn)
            //bytes.[offset+2] <- byte ((myoffset / 256) + 128) // the location of my code to JMP to, converting bank 0 (0x2nnn) to bank 2 (0xAnnn)
        else
            failwith "unexpected ROM code found - did not find overworld-map-tile-return code where expected"
    else
        failwith "unexpected ROM code found"
    let unused_min_offset = unused_min_offset + length
    if unused_min_offset > unused_end then
        failwith "used too much space"

    // erase array when player clears an adventure log
    // (code at 03:F80F does the 'clear a file' logic, it seems 6039 is the selected file, 6038 stores a mask of existing saves (1/2/4 is files 0/1/2), 6035/6036/6037 store some data for 0/1/2)
    // (see also https://www.nicholasmikstas.com/games/ )
    let length = 51
    // do minor verification that the code we expect to be there is there
    if bytes.[unused_min_offset..unused_min_offset+length-1] = Array.create length 0xFFuy then
        let replacementBytes = makePatchedBytes(clearArrayAssociatedWithALogFile,length)
        for i = 0 to length-1 do
            bytes.[unused_min_offset+i] <- replacementBytes.[i]
        // change the JSR at clear-log-file, namely
        //    03:F935:20 0F F8  JSR $F80F
        // to a JSR to my code which will also eventually JMP back into the method at F80F which I hijacked
        //    20 A0 C2          JMP $C2A0    // or wherever address my code finally lives
        let offset = 0x0F935 + 16
        if bytes.[offset..offset+2] = [| 0x20uy; 0x0Fuy; 0xF8uy |] then
            bytes.[offset+0] <- 0x20uy // JSR
            let myoffset = unused_min_offset-16 // header adjust
            bytes.[offset+1] <- byte (myoffset % 256)
            bytes.[offset+2] <- byte ((myoffset / 256) + 0) // the location of my code to JSR to, converting bank 3 (0xCnnn) to bank 3 (0xCnnn)
        else
            failwith "unexpected ROM code found"
    else
        failwith "unexpected ROM code found"
    let unused_min_offset = unused_min_offset + length
    if unused_min_offset > unused_end then
        failwith "used too much space"

    System.IO.File.WriteAllBytes(file+".patched.nes", bytes)
