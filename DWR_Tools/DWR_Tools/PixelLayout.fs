module PixelLayout

// fceux with Dragon Warrior puts out 256x224 color pixels

// on overworld map... 15.5 x 14 tiles visible
// when walking up-down, hero left hand side is at X=128; 7 tiles fully visible to his left, 8th (leftmost) is half-drawn (left half black); 7 tiles fully visible to his right
// when walking left-right, hero top is at Y=104; 6 tiles fully visible above, 7th (topmost) is half-drawn (cut off screen top); 6 tiles fully visible below, 7th (bottom most) half drawn (cut off screen)
// can potentially see part of 17x15 tiles

////////////////////////////////////////////////////////////////

// TEXT
// Text is white on black, unless hero is < 25% health, in which case it's red on black.

let WHITE = System.Drawing.Color.FromArgb(0xFC, 0xFC, 0xFC)
let RED   = System.Drawing.Color.FromArgb(0xFC, 0x74, 0x60)
let BLACK = System.Drawing.Color.FromArgb(0x00, 0x00, 0x00)

let CAPS_ALPHABET = [|
    "  XXX                           XXXXXXX                         XXXXXXX                 XXX     XX   XX                 XXXXXX          XXXXXX   XXXXX  XXXXXXX XX   XX                                                 "
    " XX XX                          XX                                XXX                   XXX     XXX XXX                 XX   XX         XX   XX XX   XX   XXX   XX   XX                                                 "
    "XX   XX                         XX                                XXX                   XXX     XXX XXX                 XX   XX         XX   XX XX        XXX   XX   XX                                                 "
    "XX   XX                         XXXXXX                            XXX                   XXX     XXXXXXX                 XX   XX         XX   XX  XXXXX    XXX   XX   XX                                                 "
    "XXXXXXX                         XX                                XXX                   XXX     XX X XX                 XXXXXX          XXXXXX       XX   XXX   XX   XX                                                 "
    "XX   XX                         XX                                XXX                   XXX     XX   XX                 XX              XX   XX XX   XX   XXX   XX   XX                                                 "
    "XX   XX                         XXXXXXX                         XXXXXXX                 XXXXXXX XX   XX                 XX              XX   XX  XXXXX    XXX    XXXXX                                                  "
    |]

let DIGITS = [|
    "  XXXX    XXX    XXXXX   XXXXX      XX  XXXXXX   XXXXX  XXXXXXX  XXXXX   XXXXX  "
    " XX  XX  XXXX   XX   XX XX   XX    XXX  XX      XX      XX   XX XX   XX XX   XX "
    " XX  XX   XXX   XX   XX XX   XX   X XX  XX      XX           XX XX   XX XX   XX "
    " XX  XX   XXX       XX     XXX   X  XX  XXXXXX  XXXXXX     XX    XXXXX   XXXXXX "
    " XX  XX   XXX    XXX    XX   XX XX  XX       XX XX   XX   XX    XX   XX      XX "
    " XX  XX   XXX   XX      XX   XX XXXXXXX      XX XX   XX   XX    XX   XX      XX "
    "  XXXX   XXXXX  XXXXXXX  XXXXX      XX  XXXXXX   XXXXX    XX     XXXXX   XXXXX  "
    |]

let index c = 
    if c >= 'A' && c <= 'Z' then
        CAPS_ALPHABET, (int c - int 'A') * 8
    elif c = ' ' then
        CAPS_ALPHABET, 26 * 8
    elif c >= '0' && c <= '9' then
        DIGITS, (int c - int '0') * 8
    else
        failwith "bad char"

let matchLetter(getPixel,x,y,c) =
    let mutable isMatch = true
    let AA,a = index c
    for i = 0 to 6 do
        if isMatch then
            for j = 0 to 6 do
                if isMatch then
                    if AA.[j].[a+i] = 'X' then
                        if getPixel(x+i,y+j) <> WHITE then
                            isMatch <- false
                    elif AA.[j].[a+i] = ' ' then
                        if getPixel(x+i,y+j) <> BLACK then
                            isMatch <- false
                    else
                        failwith "bad CAPS_ALPHABET/DIGITS"
    isMatch

let spellCoords(n) = 
    if n<0 || n>9 then
        failwith "bad spell index"
    // pixel coords of the upper left of the first letter of the nth spell
    160, 40+n*16

let isSpellMenuActive(getPixel) =  // assume RED->WHITE already converted
    let mutable result = true
    // there's a line that appears before word 'SPELL' here 
    for x = 146 to 174 do
        if result then
            for y = 25 to 26 do
                if getPixel(x,y) <> WHITE then
                    result <- false
    result    

let SPELL_NAMES = [|
    "HEAL     "
    "HURT     "
    "SLEEP    "
    "RADIANT  "
    "STOPSPELL"
    "OUTSIDE  "
    "RETURN   "
    "REPEL    "
    "HEALMORE "
    "HURTMORE "
    |]

let identifySpells(getPixel, spells:_[]) = 
    let mutable changed = false
    if isSpellMenuActive(getPixel) then
        for n = 0 to 9 do
            let x,y = spellCoords(n)
            // only need to match letters 4 & 5 to uniquely identify each spell name
            let x = x + 24 // move to 4th letter
            for spell = 0 to 9 do
                if matchLetter(getPixel,x,y,SPELL_NAMES.[spell].[3]) && matchLetter(getPixel,x+8,y,SPELL_NAMES.[spell].[4]) then
                    if not spells.[spell] then
                        changed <- true
                    spells.[spell] <- true
    changed

let statCoords(n) = 
    // LV / HP / MP / G / E
    if n<0 || n>4 then
        failwith "bad stat index"
    // pixel coords of the upper left of the first letter of the nth stat
    24, 40+n*16

let value(getPixel,x,y) =
    if matchLetter(getPixel,x,y,'0') then 0
    elif matchLetter(getPixel,x,y,'1') then 1
    elif matchLetter(getPixel,x,y,'2') then 2
    elif matchLetter(getPixel,x,y,'3') then 3
    elif matchLetter(getPixel,x,y,'4') then 4
    elif matchLetter(getPixel,x,y,'5') then 5
    elif matchLetter(getPixel,x,y,'6') then 6
    elif matchLetter(getPixel,x,y,'7') then 7
    elif matchLetter(getPixel,x,y,'8') then 8
    elif matchLetter(getPixel,x,y,'9') then 9
    elif matchLetter(getPixel,x,y,' ') then 0
    else failwith "expected digit here"

let identifyEXP(getPixel) =
    let x,y = statCoords(4)
    if matchLetter(getPixel,x,y,'E') then
        let mutable exp = 0 
        let mutable x = x+8
        for place = 0 to 4 do
            exp <- exp + value(getPixel,x,y)            
            exp <- exp * 10
            x <- x + 8
        Some(exp/10)
    else
        None
            