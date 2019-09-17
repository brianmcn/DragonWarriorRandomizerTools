module ROM_mods

//////////////////////////////////////////////////////////////////
// Basic assembler to make it easier to write code with branches
//////////////////////////////////////////////////////////////////

// https://www.masswerk.at/6502/6502_instruction_set.html
// http://6502.org/tutorials/6502opcodes.html

type Assembly = 
    | Label of int
    | Comment of string
    | BNE of int * string * string * string
    | BEQ of int * string * string * string
    | BPL of int * string * string * string
    | BMI of int * string * string * string
    | Instr1 of byte * string * string * string
    | Instr2 of byte*byte * string * string * string
    | Instr3 of byte*byte*byte * string * string * string
    member this.Size =
        match this with
        | Label _ -> 0
        | Comment _ -> 0
        | BNE _ -> 2
        | BEQ _ -> 2
        | BPL _ -> 2
        | BMI _ -> 2
        | Instr1 _ -> 1
        | Instr2 _ -> 2
        | Instr3 _ -> 3

let mkBNE n a b c = BNE(n,a,b,c)
let mkBEQ n a b c = BEQ(n,a,b,c)
let mkBPL n a b c = BPL(n,a,b,c)
let mkBMI n a b c = BMI(n,a,b,c)
let I1 x a b c = Instr1(byte x, a, b, c)
let I2 x y a b c = Instr2(byte x, byte y, a, b, c)
let I3 x y z a b c = Instr3(byte x, byte y, byte z, a, b, c)

// given an array of Assembly where Label and BNE carry labels, convert to an array of Assembly where Labels are removed and BNE carry offsets
let backpatch (a:Assembly[]) =
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
        | Label n -> 
            newInstructions.Add(Comment(sprintf "Label %d" n))
        | BNE(n,a,b,c) -> 
            match labelOffsets.TryGetValue(n) with
            | true, addr ->
                let diff = addr - curOffset
                newInstructions.Add(mkBNE diff a b c)
            | false, _ -> failwithf "bad destination label %d" n
        | BEQ(n,a,b,c) -> 
            match labelOffsets.TryGetValue(n) with
            | true, addr ->
                let diff = addr - curOffset
                newInstructions.Add(mkBEQ diff a b c)
            | false, _ -> failwithf "bad destination label %d" n
        | BPL(n,a,b,c) -> 
            match labelOffsets.TryGetValue(n) with
            | true, addr ->
                let diff = addr - curOffset
                newInstructions.Add(mkBPL diff a b c)
            | false, _ -> failwithf "bad destination label %d" n
        | BMI(n,a,b,c) -> 
            match labelOffsets.TryGetValue(n) with
            | true, addr ->
                let diff = addr - curOffset
                newInstructions.Add(mkBMI diff a b c)
            | false, _ -> failwithf "bad destination label %d" n
        | _ ->
            newInstructions.Add(x)
    newInstructions.ToArray()

let showBackpatch(assembly) =
    let patched = backpatch(assembly)
    for x in patched do
        match x with
        | Label _ -> failwith "impossible"
        | Comment s -> 
            printfn "        /* %s */" s
        | BNE(offset,a,b,c) -> 
            printfn "        0xd0,  0x%02x,        /* %s %s */" (if offset >=0 then offset else offset + 256) a c
        | BEQ(offset,a,b,c) -> 
            printfn "        0xf0,  0x%02x,        /* %s %s */" (if offset >=0 then offset else offset + 256) a c
        | BPL(offset,a,b,c) -> 
            printfn "        0x10,  0x%02x,        /* %s %s */" (if offset >=0 then offset else offset + 256) a c
        | BMI(offset,a,b,c) -> 
            printfn "        0x30,  0x%02x,        /* %s %s */" (if offset >=0 then offset else offset + 256) a c
        | Instr1(x,a,b,c) ->
            printfn "        0x%02x,               /* %s %s */" x a c
        | Instr2(x,y,a,b,c) ->
            printfn "        0x%02x,  0x%02x,        /* %s %s */" x y a c
        | Instr3(x,y,z,a,b,c) ->
            printfn "        0x%02x,  0x%02x,  0x%02x, /* %s %s */" x y z a c

let makePatchedBytes(code,length) =
    let patched = backpatch(code)
    let bytes = ResizeArray()
    for x in patched do
        match x with
        | Label _ -> failwith "impossible"
        | Comment s -> ()
        | BNE(offset,_,_,_) -> 
            bytes.Add(0xD0uy)
            bytes.Add(if offset >=0 then byte(offset) else byte(offset + 256))
        | BEQ(offset,_,_,_) -> 
            bytes.Add(0xF0uy)
            bytes.Add(if offset >=0 then byte(offset) else byte(offset + 256))
        | BPL(offset,_,_,_) -> 
            bytes.Add(0x10uy)
            bytes.Add(if offset >=0 then byte(offset) else byte(offset + 256))
        | BMI(offset,_,_,_) -> 
            bytes.Add(0x30uy)
            bytes.Add(if offset >=0 then byte(offset) else byte(offset + 256))
        | Instr1(x,_,_,_) ->
            bytes.Add(byte x)
        | Instr2(x,y,_,_,_) ->
            bytes.Add(byte x)
            bytes.Add(byte y)
        | Instr3(x,y,z,_,_,_) ->
            bytes.Add(byte x)
            bytes.Add(byte y)
            bytes.Add(byte z)
    if bytes.Count > length then
        failwith "tried to squeeze too much code into too small a length"
    while bytes.Count < length do
        bytes.Add(0xEAuy) // EA = NOP
    showBackpatch(code)
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
        I2 0xA5 0xC5        "LDA $C5     "       "A5 C5        "     "load player health"
        I2 0xC9 0x00        "CMP #$00    "       "C9 00        "     "compare to zero"
        mkBNE 9             "BNE 9       "       "D0 TODO      "     "if not, done"
        I2 0xA5 0x45        "LDA $45     "       "A5 45        "     "load map"
        I2 0xC9 0x01        "CMP #$01    "       "C9 01        "     "compare overworld"
        mkBNE 9             "BNE 9       "       "D0 TODO      "     "if not, done"
        Comment "We died to swamp damage on the overworld. Load up the array of the current adventure log file, and see if there is any space left to write more coords."

        Comment "Free up some zero-page bytes for indirect addressing."
        I2 0xA5 0x20        "LDA $20     "      "A5 20         "     ""   
        I1 0x48             "PHA         "      "48            "     ""
        I2 0xA5 0x21        "LDA $21     "      "A5 21         "     ""
        I1 0x48             "PHA         "      "48            "     ""
        Comment "Load up the appropriate array, based on which log file we're in.  0x20 multiplied by adventure log number is array address offset from 7000, so..."
        I3 0xAD 0x39 0x60   "LDA $6039   "      "AD 39 60      "   "load adventure log file number (0/1/2)"
        I1 0x0A             "ASL         "      "0A            "   ""
        I1 0x0A             "ASL         "      "0A            "   ""
        I1 0x0A             "ASL         "      "0A            "   ""
        I1 0x0A             "ASL         "      "0A            "   ""
        I1 0x0A             "ASL         "      "0A            "   "five shifts is a multiply-by-0x20"
        I2 0x85 0x20        "STA $20     "      "85 20         "   ""
        I2 0xA9 0x70        "LDA #$70    "      "A9 70         "   ""
        I2 0x85 0x21        "STA $21     "      "85 21         "   "now $21$20 contains either $7000, $7020, or $7040 as appropriate"

        Comment "The 'count' byte will be twice the number of coordinate pairs that have been written, e.g. an offset to the first 'unused' spot in the array.  So for example"
        Comment "in log file 2, if 703E contains the value 0C, it means 6 sets of coordinates have been written, and the next one is written to 7020+0C.  A value of 1E means 'full'."
        I2 0xA0 0x1E        "LDY #$1E    "      "A0 1E         "   "set Y to 1E (offset into 70x0 array to look up count)"
        I2 0xB1 0x20        "LDA ($20),Y "      "B1 20         "   "load num-written-desert-bytes"
        I1 0xA8             "TAY         "      "A8            "   "copy to Y"
        I2 0xC9 0x1E        "CMP #$1E    "      "C9 1E         "   "compare to MAX (have we already written 0x1E bytes of desert coords?)"
        mkBEQ 8             "BEQ 8       "      "F0 TODO       "   "if so, done"
        I2 0xA5 0x42        "LDA $42     "      "A5 42         "   "load map x coord"
        I2 0x91 0x20        "STA ($20),Y "      "91 20         "   "store it to my array"
        I1 0xC8             "INY         "      "C8            "   "inc Y"
        I2 0xA5 0x43        "LDA $43     "      "A5 43         "   "load map y coord"
        I2 0x91 0x20        "STA ($20),Y "      "91 20         "   "store it to my array"
        I1 0xC8             "INY         "      "C8            "   "inc Y"
        I1 0x98             "TYA         "      "98            "   "copy Y to A"
        I2 0xA0 0x1E        "LDY #$1E    "      "A0 1E         "   "set Y to 1E (offset into 70x0 array to store new count)"
        I2 0x91 0x20        "STA ($20),Y "      "91 20         "   "store new count to num-written-desert-bytes cell"
            
        Label 8
        Comment "restore the zero-page bytes used for indirect addressing"
        I1 0x68             "PLA         "      "68            "   ""
        I2 0x44 0x21        "STA $21     "      "44 21         "   ""    
        I1 0x68             "PLA         "      "68            "   ""
        I2 0x44 0x20        "STA $20     "      "44 20         "   ""    
        Label 9
        Comment "return"
        I3 0x20 0x74 0xFF   "JSR $FF74   "      "20 74 FF      "   "original JSR that I overwrote"
        I3 0x4C 0xED 0xCD   "JMP $CDED   "      "4C ED CD      "   "jump back to instruction after one I overwrote"
    |]

// Second, the routine to convert swamp-to-desert tiles on-the-fly as the overworld map is read:
let swampToDesertAssembly = 
    // This code is called at the end of the 'what overworld tile is this' lookup that checks the original map data.
    [|
        Comment "If the tile is an overworld swamp"
        I2 0xC9 0x06        "CMP #$06    "      "C9 06         "   "compare swamp  (swamp is 'border tile 6' as per https://github.com/mcgrew/dwrandomizer/blob/master/notes/maps.txt )"
        mkBNE 9             "BNE 9       "      "D0 TODO       "   "if not, done"
        I2 0xA5 0x45        "LDA $45     "      "A5 45         "   "load map"
        I2 0xC9 0x01        "CMP #$01    "      "C9 01         "   "compare overworld"
        mkBNE 9             "BNE 9       "      "D0 TODO       "   "if not, done"
        Comment "Load up the appropriate array, based on which log file we're in"
        I3 0xAD 0x39 0x60   "LDA $6039   "      "AD 39 60      "   "load adventure log file number (0/1/2)"
        I1 0x0A             "ASL         "      "0A            "   ""
        I1 0x0A             "ASL         "      "0A            "   ""
        I1 0x0A             "ASL         "      "0A            "   ""
        I1 0x0A             "ASL         "      "0A            "   ""
        I1 0x0A             "ASL         "      "0A            "   "five shifts is a multiply-by-0x20"
        I1 0xA8             "TAY         "      "A8            "   "Y is now 00 or 20 or 40 as appropriate"
        Comment "Compare to each and every saved coordinate in current array (array will start as all zeros, so the single upper left tile of overworld map will never be swamp, that's fine)"
        Label 0
        I2 0xA5 0x42        "LDA $42     "      "A5 42         "   "load x coord"
        I3 0xD9 0x00 0x70   "CMP $7000,Y "      "D9 00 70      "   "compare to array of coords starting at $7000"
        mkBNE 1             "BNE 1       "      "D0 TODO       "   "if not, jump to first INY below (increment array index to next coord set)"
        I2 0xA5 0x43        "LDA $43     "      "A5 43         "   "load y coord"
        I3 0xD9 0x01 0x70   "CMP $7001,Y "      "D9 01 70      "   "compare to array of coords starting at $7000"
        mkBNE 1             "BNE 1       "      "D0 TODO       "   "if not, jump to first INY below (increment array index to next coord set)"
        I2 0xA9 0x01        "LDA #$01    "      "A9 01         "   "load desert"
        I2 0x85 0x3C        "STA $3C     "      "85 3C         "   "store desert as tile result"
        Label 1
        I1 0xC8             "INY         "      "C8            "   "move down coord array"
        I1 0xC8             "INY         "      "C8            "   "move down coord array"
        // Check for end of (any) array
        I2 0xC0 0x1E        "CPY #$1E    "      "C0 1E         "   "compare Y (see if at end of first array)"
        mkBEQ 9             "BEQ 9       "      "F0 TODO       "   "if at end of array, go to return"
        I2 0xC0 0x3E        "CPY #$3E    "      "C0 3E         "   "compare Y (see if at end of second array)"
        mkBEQ 9             "BEQ 9       "      "F0 TODO       "   "if at end of array, go to return"
        I2 0xC0 0x5E        "CPY #$5E    "      "C0 5E         "   "compare Y (see if at end of third array)"
        mkBEQ 9             "BEQ 9       "      "F0 TODO       "   "if at end of array, go to return"
        mkBNE 0             "BNE 0       "      "D0 TODO       "   "else, go back up to LDA $42"
        Label 9
        I1 0x68             "PLA         "      "68            "   "|"
        I1 0xA8             "TAY         "      "A8            "   "|- subroutine exit"
        I1 0x60             "RTS         "      "60            "   "|"
    |]

// Finally, code to zero it out when an adventure log is cleared (without this code, if you start with gold, you can take some deaths to clear path out of swamp, erase save, 
//   restart, and walk the gold to a town, as the map is still changed even after the erased save; I don't want that behavior):
let clearArrayAssociatedWithALogFile = 
    [|
        I1 0x48             "PHA         "      "              "   ""
        I1 0x8A             "TXA         "      "              "   "same preamble as method we're hijacking (0xF80F), will jump past it in the original method at the end of this routine"
        I1 0x48             "PHA         "      "              "   ""

        Comment "Load up the appropriate array, based on which log file we're in"
        I3 0xAD 0x39 0x60   "LDA $6039   "      "AD 39 60      "   "load adventure log file number (0/1/2)"
        I1 0x0A             "ASL         "      "0A            "   ""
        I1 0x0A             "ASL         "      "0A            "   ""
        I1 0x0A             "ASL         "      "0A            "   ""
        I1 0x0A             "ASL         "      "0A            "   ""
        I1 0x0A             "ASL         "      "0A            "   "five shifts is a multiply-by-0x20"
        I1 0xA8             "TAY         "      "A8            "   "Y is now 00 or 20 or 40 as appropriate"
        
        I2 0xA2 0x20        "LDX #$20    "      "A2 20         "   "loop counter"
        I2 0xA9 0x00        "LDA #$00    "      "A9 00         "   "zero for storing"
        Label 1
        I3 0x99 0x00 0x70   "STA $7000,Y "       "99 00 70     "   "store 0 to 7000+Y"
        I1 0xC8             "INY         "       "C8           "   "inc Y"
        I1 0xCA             "DEX         "       "CA           "   "dec X"
        mkBNE 1             "BNE 1       "       "D0 TODO      "   "loop until zeroed all 32 bytes"

        Comment "the 'return'"
        I3 0x4C 0x12 0xF8   "JMP $F812   "       "4C 12 F8     "   "jump into middle of routine I hijacked, after preamble that I already copied at start of this routine"
    |]

let patch_rom(file) =
    // Based on:
    //     https://github.com/mcgrew/dwrandomizer/blob/master/common/dwr.c
    //     /* Clear the unused code so we can make sure it's unused */
    //     memset(&rom.content[0xc288], 0xff, 0xc4f5 - 0xc288);
    let unused_min_offset = 0xC298      // 'content' is 0x10 after 'bytes' due to header
    let unused_end = 0xC505 

    let bytes = System.IO.File.ReadAllBytes(file)
    printfn "/* when dying in swamp, write my extra desert tiles */"
    let length = 68
    // do minor verification that the code we expect to be there is there
    if bytes.[unused_min_offset..unused_min_offset+length-1] = Array.create length 0xFFuy then
        printfn "    vpatch(rom, 0x%05x, %4d," unused_min_offset length
        let replacementBytes = makePatchedBytes(swampToDesertAssemblyWrite,length)
        printfn "    );"
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
            printfn "    vpatch(rom, 0x%05x, %4d," offset 3
            printfn "        0x4c,  0x%02x,  0x%02x /* JMP to new code */" (myoffset % 256) ((myoffset / 256) + 0)
            printfn "    );"
        else
            failwith "unexpected ROM code found"
    else
        failwith "unexpected ROM code found"
    let unused_min_offset = unused_min_offset + length
    if unused_min_offset > unused_end then
        failwith "used too much space"

    printfn "/* when reading overworld map, load my extra desert tiles */"
    let length = 56
    // do minor verification that the code we expect to be there is there
    if bytes.[unused_min_offset..unused_min_offset+length-1] = Array.create length 0xFFuy then
        printfn "    vpatch(rom, 0x%05x, %4d," unused_min_offset length
        let replacementBytes = makePatchedBytes(swampToDesertAssembly,length)
        printfn "    );"
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
            printfn "    vpatch(rom, 0x%05x, %4d," offset 3
            printfn "        0x4c,  0x%02x,  0x%02x /* JMP to new code */" (myoffset % 256) ((myoffset / 256) + 0)
            printfn "    );"
        else
            failwith "unexpected ROM code found - did not find overworld-map-tile-return code where expected"
    else
        failwith "unexpected ROM code found"
    let unused_min_offset = unused_min_offset + length
    if unused_min_offset > unused_end then
        failwith "used too much space"

    printfn "/* erase array when player clears an adventure log */"
    // (code at 03:F80F does the 'clear a file' logic, it seems 6039 is the selected file, 6038 stores a mask of existing saves (1/2/4 is files 0/1/2), 6035/6036/6037 store some data for 0/1/2)
    // (see also https://www.nicholasmikstas.com/games/ )
    let length = 26
    // do minor verification that the code we expect to be there is there
    if bytes.[unused_min_offset..unused_min_offset+length-1] = Array.create length 0xFFuy then
        printfn "    vpatch(rom, 0x%05x, %4d," unused_min_offset length
        let replacementBytes = makePatchedBytes(clearArrayAssociatedWithALogFile,length)
        printfn "    );"
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
            printfn "    vpatch(rom, 0x%05x, %4d," offset 3
            printfn "        0x20,  0x%02x,  0x%02x /* JSR to new code */" (myoffset % 256) ((myoffset / 256) + 0)
            printfn "    );"
        else
            failwith "unexpected ROM code found"
    else
        failwith "unexpected ROM code found"
    let unused_min_offset = unused_min_offset + length
    if unused_min_offset > unused_end then
        failwith "used too much space"

    System.IO.File.WriteAllBytes(file+".patched.nes", bytes)

let patch_rom_dark_overworld(file) =
    // TODO does not work for various reasons
    let bytes = System.IO.File.ReadAllBytes(file)
    // map type being CMP'd  -  #$00-over world, #$10-town/castle, #$20-cave.

    // make everything dark
    let offset = 0x2961+16   // checks to see if dungeon, for darkness
    if bytes.[offset..offset+4] = [|165uy; 22uy; 201uy; 32uy; 0xD0uy|] then
        bytes.[offset+3] <- 0x10uy   // town rather than dungeon
        bytes.[offset+4] <- 0xF0uy   // BEQ rather than BNE
    else
        failwith "unexpected bytes"
(*
code above starts to differ with code below at
  LA9F8:  LDX $22                 ;Load store offset.
  ...
  LA9FE:  STA $6436,X   
which seems to be writing to
  .alias WndLineBuf       $6436   ;Through $6471. 60 bytes. buffers 2 window rows.

Ah, A880 has a prologue that either call into A8AD or A921 (which continues to A961 above)
A880 is called when windows are removed (to repaint what's behind them)


Aside:
  LE89D:  JSR DoDialogLoBlock     ;($C7CB)
  LE8A0:  .byte $F6
The F6 means, in bank2, F is TextBlock16, and 6 is the 6th entry, labeled as TB16E6 here https://www.nicholasmikstas.com/dragon-warrior-bank-2
ModEnemyStats is effective AG calculation, followed by chance-to-run computation.
*)

    let offset = 0x2DA6+16   // checks to see if dungeon, for darkness
    if bytes.[offset..offset+4] = [|165uy; 22uy; 201uy; 32uy; 0xD0uy|] then
        bytes.[offset+3] <- 0x10uy   // town rather than dungeon
        bytes.[offset+4] <- 0xF0uy   // BEQ rather than BNE
    else
        failwith "unexpected bytes"

    // now at least two problems
    //  - casting radiant does nothing in towns/overworld
    //  - dont want towns to be dark

    // allow radiant cast in other places
    let offset = 0xDA64+16   // checks to see if dungeon, for darkness
    if bytes.[offset..offset+4] = [|165uy; 22uy; 201uy; 32uy; 0xD0uy|] then
        bytes.[offset+3] <- 0x10uy   // town rather than dungeon
        bytes.[offset+4] <- 0xF0uy   // BEQ rather than BNE
    else
        failwith "unexpected bytes"

    // allow torch in other places
    let offset = 0xDD1E+16   // checks to see if dungeon, for darkness
    if bytes.[offset..offset+4] = [|165uy; 22uy; 201uy; 32uy; 0xF0uy|] then
        bytes.[offset+3] <- 0x10uy   // town rather than dungeon
        bytes.[offset+4] <- 0xD0uy   // BNE rather than BEQ
    else
        failwith "unexpected bytes"

(* some tile-loading something
LAE4B:  LDA MapType
LAE4D:  CMP #$20
LAE4F:  BNE $AE5B
...
LAE71:  LDA MapType
LAE73:  CMP #$20
LAE75:  BNE $AE81
...
LAEA2:  LDA MapType
LAEA4:  CMP #$20
LAEA6:  BNE $AEB2
...
LAEC8:  LDA MapType
LAECA:  CMP #MAP_DUNGEON
LAECC:  BNE $AED8

movement tiles
LB265:  LDA MapType
LB267:  CMP #MAP_DUNGEON
LB269:  BNE $B28C
...
LB35F:  LDA MapType
LB361:  CMP #MAP_DUNGEON
LB363:  BNE $B386
...
LB3EB:  LDA MapType
LB3ED:  CMP #MAP_DUNGEON
LB3EF:  BNE $B412
...
LB517:  LDA MapType
LB519:  CMP #MAP_DUNGEON
LB51B:  BNE $B53E
*)

    System.IO.File.WriteAllBytes(file+".patched.nes", bytes)
