﻿module ROM_mods

type Assembly = 
    | Label of int
    | BNE of int
    | BEQ of int
    | Instr1 of byte
    | Instr2 of byte*byte
    | Instr3 of byte*byte*byte
    member this.Size =
        match this with
        | Label _ -> 0
        | BNE _ -> 2
        | BEQ _ -> 2
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
        | _ ->
            newInstructions.Add(x)
    newInstructions.ToArray()

let I1 x = Instr1(byte x)
let I2 x y = Instr2(byte x, byte y)
let I3 x y z = Instr3(byte x, byte y, byte z)

let swampToDesertAssembly = 
    // goal - keep an array of overworld tiles that have been transformed swamp to desert.
    // look them up and decode them on the fly during map decode tile lookup.
    // keep 3 different arrays, one for each adventure log file, starting at 7000, 7020, and 7040, containing 15 pairs of coords (e.g. 7000-701D) and a count (e.g. 701E), each
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
        // Compare to each and every saved coordinate in current array (array will start as all zeros, so upper left of overworld map will never be swamp, that's fine)
        Label 0
        I2 0xA5 0x42        // LDA $42            ;A5 42             load x coord
        I3 0xD9 0x00 0x70   // CMP $7000,Y        ;D9 00 70          compare to array of coords starting at $7000    TODO see if memory after 7000 is safe to write (unused)
        BNE 1               // BNE $TODO          ;D0 TODO           if not, jump to first INY below (increment array index to next coord set)
        I2 0xA5 0x43        // LDA $43            ;A5 43             load y coord
        I3 0xD9 0x01 0x70   // CMP $7001,Y        ;D9 01 70          compare to array of coords starting at $7000    TODO see if memory after 7000 is safe to write (unused)
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
// TODO cuurent stored map changes in battery-backed storage shared by all 3 log files, can see which log is in play via
//     looks like RAM byte 0x21 is temporarily 0/1/2 depending on if in file 1/2/3
// and discriminate among 3 arrays... but even then, clearing a file would not clear it... hm
(*

[7:27 PM] Lorgon111: looks like 0x21 is 0/1/2 depending on if in file 1/2/3
[7:31 PM] Lorgon111: ah, but even if i discriminate on-the-fly, the player could still: play, change the map, reset, clear the data file, and then start a new character in the same log 
                     file and see prior changes... so i would need to be able to hook 'clear' or else king save/load
[7:31 PM] Thilotilo: $F148 is the save code
[7:35 PM] Thilotilo: I don't have it documented perfectly enough to be sure of this, but it appears that $6039 is what is used by the game for save index for all save/load file logic
[7:39 PM] Lorgon111: indeed, I see code at F9E4 reading it when i talk to king to save
[7:40 PM] Thilotilo: $FB6B is the code that is run after checksum passes on file load, which does the byte-by-byte copy from save file to current status
[7:42 PM] Thilotilo: Yeah, on saving, $F148 is actually wait-for-next-frame then call $F9DF, which configures everything with $6039 for save address location (stored to $23$22), and then 
                     calls into $FA18, which is the actual byte-by-byte copy of status

code at 03:F80F does the 'clear a file' logic, it seems 6039 is the selected file, 6038 stores a mask of existing saves (1/2/4 is files 0/1/2), 6035/6036/6037 store some data for 0/1/2

see also https://www.nicholasmikstas.com/games/

*)

let showBackpatch() =
    let patched = backpatch(swampToDesertAssembly)
    for x in patched do
        match x with
        | Label _ -> failwith "impossible"
        | BNE offset -> 
            printfn "D0 %02X" (if offset >=0 then offset else offset + 256)
        | BEQ offset -> 
            printfn "F0 %02X" (if offset >=0 then offset else offset + 256)
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

(*

TODO - it appears that swamp damage is computed here

 03:CDDF:A5 C5     LDA $00C5 = #$00
 03:CDE1:38        SEC
 03:CDE2:E9 02     SBC #$02
 03:CDE4:B0 02     BCS $CDE8
 03:CDE6:A9 00     LDA #$00
 03:CDE8:85 C5     STA $00C5 = #$00
 03:CDEA:20 74 FF  JSR $FF74
 03:CDED:20 28 EE  JSR $EE28

C5 is the player health, and this subtracts 2 from it

So i could replace the first JSR with a JMP to my own code that does:
    if player health = 0 then
        if map = overworld then
            if num converted tiles < max then
                store map x,y into my array at 7000
                increment converted tiles count
    original JSR
    JMP back

which looks something like:
*)
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
        // if in log file 2, if 703E contains the value 0E, it means 7 sets of coordinates have been written, and the next one is written to 7020+0E.  A value of 1E means 'full'.

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
            
        // the 'return'
        Label 9
        I3 0x20 0x74 0xFF   // JSR $FF74          ;20 74 FF          original JSR that I overwrote
        I3 0x4C 0xED 0xCD   // JMP $CDED          ;4C ED CD          jump back to instruction after one I overwrote
    |]

(*

[9:13 AM] Lorgon111: Another architecture question, if you don't mind.  How do cross-bank JMP/JSRs work (or do they)?  I need space to author new code, and I know there is blank space after $7000, 
                     but I don't know how to call it from $2xxx ($Axxx at runtime).  (Alternatively, do you know if there is unused space in bank 0? I expect not.)
[9:16 AM] Thilotilo: That's more complicated.  I could answer it better this evening, when I have code in front of me, but it's done via the BRK command.  I explained it to mcgrew sometime last year.  
                     Let me see if I can dig that up.
[9:17 AM] Thilotilo: Here it is:
[9:17 AM] Thilotilo: Also, for the BRK instruction - The way the BRK ultimately works in the dragon warrior code is that it uses the two bytes after the BRK instruction to determine the bank and 
                     location to jump to.  You'll get something like at $F9AC ($39AC in bank 3)  00 10 17.
[9:17 AM] Thilotilo: The 10 defines a word offset (meaning 0x10 2-byte segments), and the upper nibble of the 17 defines the bank (bank 1 in this case).   This code will thus put bank 1 in $8000-$BFFF 
                     and then use the address at $8021$8020 as a vector to call with JSR, in this case, that address is $A194.
[9:19 AM] Thilotilo: Once the subroutine at $A194 reaches the RTS instruction, we will restore the original bank into $8000-$BFFF, restore all registers at the time of the BRK, and return to the 
                     instruction after the two bytes that the BRK uses.
[9:29 AM] Lorgon111: So if I can rephrase what you said, it sounds like at the start of the bank, there's an effectively an address-jump-table of routines which can be called from outside the bank.  
                     So I could find a word of unused space somewhere in e.g. $4000-$41FF of the ROM, and then use 00 nn 17 to load up bank 1 and then look up bytes (nn*2+1,nn*2) as an address in 
                     the $8000-$BFFF range where the bank is loaded to call e.g. code located at $7000 in the rom (which would be $B000 in the call).  
                     What is the '17'?  The upper nibble '1' is the bank to load ($4000-$7FFF) but what is the lower nibble?
[9:33 AM] Thilotilo: I have yet to figure that lower nibble out.  It always seems to be 7 in every call I've hit so far.  It does something, iirc, but I was never able to figure out what they were 
                     aiming for.  I'd have to have the code in front of me to give any more detail, but for your purposes, you can just leave it at 7.
[9:33 AM] Lorgon111: Ok good enough
[9:33 AM] Thilotilo: Your rephrase sounds right to me.


So, knowing that, if there is unused space in 4000-41FF (yes, tons), then I can add some code somewhere in unused 7000 block which will
 - be called via BRK interrupt in swamp-damage code (replacing code at CDDF), and
 - go something like this

do actual swamp damage (code overwritten to insert my BRK subroutine call)
if player health is 0
   if map is overworld
     if $TILE_INDEX < TILE_MAX               // tile index can be a byte which stores 'next Y' offset into my $7000 coordinate array, TILE_MAX could be 16 or something, e.g. 8 coord pairs
       load $TILE_INDEX into Y
       store $PLAYER_X into $7000,Y
       INY
       store $PLAYER_Y into $7000,Y
       INY
       store Y into $TILE_INDEX
RTS

[9:36 AM] Lorgon111: still will be just 'proof of concept' - i am surely breaking hidden-stairs and gwaelin-tile code, and dunno if I can use the 'interrupt trick' in the tile loop (because it is 
                     called SOOOOOO frequently - I expect the interrupt is expensive)... but if get concept proof code working, surely will motivate me to rewrite tile code and squeeze it in somehow
[9:43 AM] Thilotilo: I know there is some unused code in bank 3, but I also know mcgrew has made use of some of it, so I don't know how much is remaining.  That would be the first place I'd look, 
                     though.  He zeros that section out in the rando code, so you can probably figure out what is available.  I would tend to agree that doing the interrupt in the loop would seem 
                     prohibitively expensive.

bank 3

 03:C288:FF    
 ...
 03:C4F4:FF 

 is all FFs... i see mcgrew patching in new routines in latest betas towards the end of that range

also bank 0
  00:A574:FF   
  ...
  00:A652:FF      
oh, except that's not actually 'free', this is the end of the RLE-encoded map area, which happened to be free in
my big swamp example due to high compression of big swamp.
see https://github.com/mcgrew/dwrandomizer/blob/master/notes/rom.txt

*)

let patch_rom(file) =
    // Based on:
    //     https://github.com/mcgrew/dwrandomizer/blob/master/common/dwr.c
    //     /* Clear the unused code so we can make sure it's unused */
    //     memset(&rom.content[0xc288], 0xff, 0xc4f5 - 0xc288);
    let unused_min_offset = 0xC298      // content is 0x10 after bytes due to header
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

    System.IO.File.WriteAllBytes(file+".patched.nes", bytes)
