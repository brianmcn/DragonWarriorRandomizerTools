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
    [|
        I2 0xC9 0x06        // CMP #$06           ;C9 06             compare swamp  (swamp is 'border tile 6' as per https://github.com/mcgrew/dwrandomizer/blob/master/notes/maps.txt )
        BNE 9               // BNE $02D23         ;D0 TODO           if not, done
        I2 0xA5 0x45        // LDA $45            ;A5 45             load map
        I2 0xC9 0x01        // CMP #$01           ;C9 01             compare overworld
        BNE 9               // BNE $02D3C         ;D0 TODO           if not, done
        I2 0xA0 0x00        // LDY #$00           ;A0 00             set Y to 0
        Label 0
        I2 0xA5 0x42        // LDA $42            ;A5 42             load x coord
        I3 0xD9 0x00 0x70   // CMP $7000,Y        ;D9 00 70          compare to array of coords starting at $7000    TODO see if memory after 7000 is safe to write (unused)
        BNE 1               // BNE $TODO          ;D0 TODO           if not, jump to first INY below (increment array index to next coord set)
        I2 0xA5 0x43        // LDA $43            ;A5 43             load y coord
        I3 0xD9 0x01 0x70   // CMP $7001,Y        ;D9 01 70          compare to array of coords starting at $7000    TODO see if memory after 7000 is safe to write (unused)
        BNE 1               // BNE $TODO          ;D0 TODO           if not, jump to first INY below (increment array index to next coord set)
        I2 0xA9 0x01        // LDA #$01           ;A9 01             load desert
        I2 0x85 0x3C        // STA $3C            ;85 3C             store desert as tile result
        (*
        // omit next 3 lines, no harm doing entire loop rather than early exit, saves bytes so can fit this code in easier
        I1 0x68             // PLA                ;68                |
        I1 0xA8             // TAY                ;A8                |- subroutine exit
        I1 0x60             // RTS                ;60                |
        *)
        Label 1
        I1 0xC8             // INY                ;C8                move down coord array
        I1 0xC8             // INY                ;C8                move down coord array
        I2 0xC0 0x10        // CPY #$10           ;C0 10             compare Y - TODO decide how large an array of swamp->desert tiles we want, '10' means 8 tileswaps (16 coords = 8 xy pairs)
        BNE 0               // BNE $TODO          ;D0 TODO           if not at end of array, go back up to LDA $42 
        Label 9
        I1 0x68             // PLA                ;68                |
        I1 0xA8             // TAY                ;A8                |- subroutine exit
        I1 0x60             // RTS                ;60                |
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

let makePatchedBytes(code,length) =
    let patched = backpatch(code)
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
        I2 0xA0 0x10        // LDY #$10           ;A0 10             set Y to 10  // TODO first byte after array will store num-converted-tiles max currently 8 (16 coords = 0x10)
        I2 0xA9 0x10        // LDA #$10           ;A9 10             load MAX
        I3 0xD9 0x00 0x70   // CMP $7000,Y        ;D9 00 70          compare (have we already written 0x10 bytes of desert coords?)
// TODO could factor common exit if I implement BEQ
        BNE 1               // BNE $TODO          ;D0 TODO           if no, do stuff further below
        Label 9             //                                       else fallthru to common exit                                         
        I3 0x20 0x74 0xFF   // JSR $FF74          ;20 74 FF          original JSR that I overwrote
        I3 0x4C 0xED 0xCD   // JMP $CDED          ;4C ED CD          jump back to instruction after one I overwrote
        Label 1
        I2 0xA2 0x10        // LDX #$10           ;A2 10             load MAX (to X)
        I3 0xBC 0x00 0x70   // LDY $7000,X        ;BC 00 70          read in (to Y) num-written-desert-bytes
        I2 0xA5 0x42        // LDA $42            ;A5 42             load map x coord
        I3 0x99 0x00 0x70   // STA $7000,Y        ;99 00 70          store it to my array
        I1 0xC8             // INY                ;C8                inc y
        I2 0xA5 0x43        // LDA $43            ;A5 43             load map y coord
        I3 0x99 0x00 0x70   // STA $7000,Y        ;99 00 70          store it to my array
        I1 0xC8             // INY                ;C8                inc y
        I3 0x8C 0x10 0x70   // STY $7010          ;8C 10 70          store new Y back into my num-written-desert-bytes cell
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
and https://github.com/mcgrew/dwrandomizer/blob/master/common/dwr.c
    /* Clear the unused code so we can make sure it's unused */
    memset(&rom.content[0xc288], 0xff, 0xc4f5 - 0xc288);

*)

let patch_rom(file) =
    let bytes = System.IO.File.ReadAllBytes(file)
    // when reading overworld map, load my extra desert tiles
    let length = 39
    let myoffset = 0x02652-length+1  // currently squeezing into very back of unused overworld map area, when possible
    let myoffset = myoffset + 16     // header
    // do minor verification that the code we expect to be there is there
    if bytes.[myoffset..myoffset+length-1] = Array.create length 0xFFuy then
        let replacementBytes = makePatchedBytes(swampToDesertAssembly,length)
        for i = 0 to length-1 do
            bytes.[myoffset+i] <- replacementBytes.[i]
        // change the 'return' at the end of the overworld map part, namely
        //    $02C95: PLA TAY RTS
        // to a jump to my code which will also eventually end the subroutine similarly
        //    JMP A580    // 4C 80 A5       // or wherever address my code finally lives
        let offset = 0x02C95 + 16
        if bytes.[offset..offset+2] = [| 0x68uy; 0xA8uy; 0x60uy |] then
            bytes.[offset+0] <- 0x4Cuy                        // JMP
            let myoffset = myoffset-16 // header adjust
            bytes.[offset+1] <- byte (myoffset % 256)
            bytes.[offset+2] <- byte ((myoffset / 256) + 128) // the location of my code to JMP to, converting bank 0 (0x2nnn) to bank 2 (0xAnnn)
        else
            failwith "unexpected ROM code found"
    else
        failwith "unexpected ROM code found"

    // when dying in swamp, write my extra desert tiles
    // want to write [$0C2A0..$0C2F0)
    let length = 0x0C2F0 - 0x0C2A0
    let offset = 0x0C2A0 + 16 // first 16 bytes are a header
    // do minor verification that the code we expect to be there is there
    if bytes.[offset..offset+length-1] = Array.create length 0xFFuy then
        let replacementBytes = makePatchedBytes(swampToDesertAssemblyWrite,length)
        for i = 0 to length-1 do
            bytes.[offset+i] <- replacementBytes.[i]
        // change the JSR after swamp damage, namely
        //    03:CDEA:20 74 FF  JSR $FF74
        // to a JSR to my code which will also eventually JMP back
        //    20 A0 C2          JSR $C2A0
        let offset = 0x0CDEA + 16
        if bytes.[offset..offset+2] = [| 0x20uy; 0x74uy; 0xFFuy |] then
            bytes.[offset+0] <- 0x20uy // JSR
            bytes.[offset+1] <- 0xA0uy // 
            bytes.[offset+2] <- 0xC2uy // C2A0 is the location C2A0 in my code to JSR to
        else
            failwith "unexpected ROM code found"
    else
        failwith "unexpected ROM code found"

    System.IO.File.WriteAllBytes(file+".patched.nes", bytes)
