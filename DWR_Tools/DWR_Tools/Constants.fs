﻿module Constants

///////////////////////////////////////////////////////

[<RequireQualifiedAccess>]
type OverworldMapTile = 
    | Bridge
    | Castle
    | Cave
    | Desert
    | Forest
    | Hills
    | Mountain
    | Plains
    | Swamp
    | Town
    | Wall
    | Water_xxxx
    | Water_Nxxx
    | Water_NWxx
    | Water_NxEx
    | Water_NxxS
    | Water_NWEx
    | Water_NWxS
    | Water_NxES
    | Water_NWES
    | Water_xWxx
    | Water_xWEx
    | Water_xWxS
    | Water_xWES
    | Water_xxEx
    | Water_xxES
    | Water_xxxS
    member this.ProjectionColor =
        match this with
        | Bridge      -> System.Drawing.Color.LightGray
        | Castle      -> System.Drawing.Color.Pink
        | Cave        -> System.Drawing.Color.DarkSalmon
        | Desert      -> System.Drawing.Color.Yellow
        | Forest      -> System.Drawing.Color.ForestGreen
        | Hills       -> System.Drawing.Color.YellowGreen 
        | Mountain    -> System.Drawing.Color.Purple
        | Plains      -> System.Drawing.Color.LawnGreen
        | Swamp       -> System.Drawing.Color.SlateGray
        | Town        -> System.Drawing.Color.Red
        | Wall        -> System.Drawing.Color.Black
        | Water_xxxx  -> System.Drawing.Color.Blue
        | Water_Nxxx  -> System.Drawing.Color.FromArgb(0,0,0xFE)
        | Water_NWxx  -> System.Drawing.Color.FromArgb(0,0,0xFD)
        | Water_NxEx  -> System.Drawing.Color.FromArgb(0,0,0xFC)
        | Water_NxxS  -> System.Drawing.Color.FromArgb(0,0,0xFB)
        | Water_NWEx  -> System.Drawing.Color.FromArgb(0,0,0xFA)
        | Water_NWxS  -> System.Drawing.Color.FromArgb(0,0,0xF9)
        | Water_NxES  -> System.Drawing.Color.FromArgb(0,0,0xF8)
        | Water_NWES  -> System.Drawing.Color.FromArgb(0,0,0xF7)
        | Water_xWxx  -> System.Drawing.Color.FromArgb(0,0,0xF6)
        | Water_xWEx  -> System.Drawing.Color.FromArgb(0,0,0xF5)
        | Water_xWxS  -> System.Drawing.Color.FromArgb(0,0,0xF4)
        | Water_xWES  -> System.Drawing.Color.FromArgb(0,0,0xF3)
        | Water_xxEx  -> System.Drawing.Color.FromArgb(0,0,0xF2)
        | Water_xxES  -> System.Drawing.Color.FromArgb(0,0,0xF1)
        | Water_xxxS  -> System.Drawing.Color.FromArgb(0,0,0xF0)
    member this.AltProjectionColor =
        let NO_WALK = System.Drawing.Color.LightGray
        let WALK = System.Drawing.Color.LightSlateGray 
        let TOWNY = System.Drawing.Color.Red
        let result = 
            match this with
            | Bridge      -> WALK
            | Castle      -> TOWNY
            | Cave        -> System.Drawing.Color.Black
            | Desert      -> WALK
            | Forest      -> WALK
            | Hills       -> WALK
            | Mountain    -> NO_WALK
            | Plains      -> WALK
            | Swamp       -> WALK
            | Town        -> TOWNY
            | Wall        -> System.Drawing.Color.DarkGray 
            | Water_xxxx  -> NO_WALK
            | Water_Nxxx  -> NO_WALK
            | Water_NWxx  -> NO_WALK
            | Water_NxEx  -> NO_WALK
            | Water_NxxS  -> NO_WALK
            | Water_NWEx  -> NO_WALK
            | Water_NWxS  -> NO_WALK
            | Water_NxES  -> NO_WALK
            | Water_NWES  -> NO_WALK
            | Water_xWxx  -> NO_WALK
            | Water_xWEx  -> NO_WALK
            | Water_xWxS  -> NO_WALK
            | Water_xWES  -> NO_WALK
            | Water_xxEx  -> NO_WALK
            | Water_xxES  -> NO_WALK
            | Water_xxxS  -> NO_WALK
        if result = WALK then
            let pc = this.ProjectionColor 
            let r = (3 * int result.R + int pc.R) / 4 
            let g = (3 * int result.G + int pc.G) / 4 
            let b = (3 * int result.B + int pc.B) / 4 
            System.Drawing.Color.FromArgb(r,g,b)
        else
            result
    member this.IsWalkable =
        match this with
        | Bridge      -> true
        | Castle      -> true
        | Cave        -> true
        | Desert      -> true
        | Forest      -> true
        | Hills       -> true
        | Mountain    -> false
        | Plains      -> true
        | Swamp       -> true
        | Town        -> true
        | Wall        -> false 
        | Water_xxxx  -> false
        | Water_Nxxx  -> false
        | Water_NWxx  -> false
        | Water_NxEx  -> false
        | Water_NxxS  -> false
        | Water_NWEx  -> false
        | Water_NWxS  -> false
        | Water_NxES  -> false
        | Water_NWES  -> false
        | Water_xWxx  -> false
        | Water_xWEx  -> false
        | Water_xWxS  -> false
        | Water_xWES  -> false
        | Water_xxEx  -> false
        | Water_xxES  -> false
        | Water_xxxS  -> false
    static member AnimationColors = [| OverworldMapTile.Cave.ProjectionColor; OverworldMapTile.Town.ProjectionColor |]
    static member IsAnimationColor(c:System.Drawing.Color) = 
        if c.ToArgb() = OverworldMapTile.Cave.ProjectionColor.ToArgb() || c.ToArgb() = OverworldMapTile.Town.ProjectionColor.ToArgb() then
            true
        else
            false
    static member FromROMByte b =
        match b with
        | 0uy -> OverworldMapTile.Plains
        | 1uy -> OverworldMapTile.Desert
        | 2uy -> OverworldMapTile.Hills
        | 3uy -> OverworldMapTile.Mountain
        | 4uy -> OverworldMapTile.Water_xxxx 
        | 5uy -> OverworldMapTile.Wall 
        | 6uy -> OverworldMapTile.Forest
        | 7uy -> OverworldMapTile.Swamp
        | 8uy -> OverworldMapTile.Town
        | 9uy -> OverworldMapTile.Cave 
        | 10uy -> OverworldMapTile.Castle 
        | 11uy -> OverworldMapTile.Bridge
        | 12uy -> OverworldMapTile.Cave // STAIRS
        | z -> OverworldMapTile.Water_NWES  // TODO failwithf "bad tiles data: %A" z

let OVERWORLD_MAP_TILE_FILENAMES = 
    [|
    "ow_bridge.png"          , OverworldMapTile.Bridge
    "ow_castle.png"          , OverworldMapTile.Castle 
    "ow_cave.png"            , OverworldMapTile.Cave
    "ow_desert.png"          , OverworldMapTile.Desert
    "ow_forest.png"          , OverworldMapTile.Forest
    "ow_hills.png"           , OverworldMapTile.Hills
    "ow_mountain.png"        , OverworldMapTile.Mountain
    "ow_plains.png"          , OverworldMapTile.Plains
    "ow_swamp.png"           , OverworldMapTile.Swamp
    "ow_town.png"            , OverworldMapTile.Town
    "ow_wall.png"            , OverworldMapTile.Wall 
    "ow_water.png"           , OverworldMapTile.Water_xxxx 
    "ow_water_e.png"         , OverworldMapTile.Water_xxEx
    "ow_water_es.png"        , OverworldMapTile.Water_xxES
    "ow_water_n.png"         , OverworldMapTile.Water_Nxxx
    "ow_water_ne.png"        , OverworldMapTile.Water_NxEx
    "ow_water_nes.png"       , OverworldMapTile.Water_NxES
    "ow_water_ns.png"        , OverworldMapTile.Water_NxxS
    "ow_water_nw.png"        , OverworldMapTile.Water_NWxx
    "ow_water_nwe.png"       , OverworldMapTile.Water_NWEx
    "ow_water_nwes.png"      , OverworldMapTile.Water_NWES
    "ow_water_nws.png"       , OverworldMapTile.Water_NWxS
    "ow_water_s.png"         , OverworldMapTile.Water_xxxS
    "ow_water_w.png"         , OverworldMapTile.Water_xWxx
    "ow_water_we.png"        , OverworldMapTile.Water_xWEx
    "ow_water_wes.png"       , OverworldMapTile.Water_xWES
    "ow_water_ws.png"        , OverworldMapTile.Water_xWxS
    |]

///////////////////////////////////////////////////////

// randomizer xp levels are 75% the normal NES game
let DWR_XP_LEVEL_THRESHOLDS = [|
    5
    17
    35
    82    // 5
    165
    337
    600
    975
    1500  // 10
    2175
    3000
    4125
    5625
    7500  // 15
    9750
    12000
    14250
    16500
    19500 // 20
    22500
    25500
    28500
    31500
    34500
    |]

// some runs use 50% xp
let DWR_XP_LEVEL_THRESHOLDS_50_PERCENT = [|
    3
    11
    23
    55     // 5
    110
    225
    400
    650
    1000   // 10
    1450
    2000
    2750
    3750
    5000   // 15
    6500
    8000
    9500
    11000
    13000  // 20
    15000
    17000
    19000
    21000
    23000
|]

///////////////////////////////////////////////////////

// locations that can appear on the overworld map
module MAP_LOCATIONS =
    // 6 towns
    let GARINHAM = "Garinham"
    let KOL = "Kol"
    let BRECCONARY = "Brecconary"
    let RIMULDAR = "Rimuldar"
    let CANTLIN = "Cantlin"
    let HAUKSNESS = "Hauksness"
    // 2 castles
    let TANTAGEL = "Tantagel Castle"
    let CHARLOCK = "Charlock Castle"
    // 3 old men
    let STAFF_CAVE = "Staff of Rain Shrine"
    let JERK_CAVE = "Jerk Cave"
    let SUN_STONES_CAVE = "Sun Stones Cave"
    // 5 caves
    let SWAMP_NORTH = "Swamp Cave North"
    let SWAMP_SOUTH = "Swamp Cave South"
    let MOUNTAIN_CAVE = "Mountain Cave"
    let TABLET_CAVE = "Tablet Cave"
    let GARINS_TOMB = "Grave of Garin"
    let ALL = 
        [|GARINHAM; KOL; BRECCONARY; RIMULDAR; CANTLIN; HAUKSNESS; 
          TANTAGEL; CHARLOCK; 
          STAFF_CAVE; JERK_CAVE; SUN_STONES_CAVE; 
          SWAMP_NORTH; SWAMP_SOUTH; MOUNTAIN_CAVE; TABLET_CAVE; GARINS_TOMB|]
    let IsLocation(s) = ALL |> Array.contains s

// maps as warps (encoded by game byte id) where chests are found
let MAPS = [|
    "NO_MAP",                                                               ""
    "OVERWORLD",                                                            ""
    "CHARLOCK",                                                             MAP_LOCATIONS.CHARLOCK
    "HAUKSNESS",                                                            MAP_LOCATIONS.HAUKSNESS 
    "TANTEGEL_TREASURY",   (* TANTEGEL *)                                   MAP_LOCATIONS.TANTAGEL 
    "TANTEGEL_THRONE_ROOM",(* 5 *)                                          MAP_LOCATIONS.TANTAGEL 
    "CHARLOCK_THRONE_ROOM",                                                 MAP_LOCATIONS.CHARLOCK 
    "KOL",                                                                  MAP_LOCATIONS.KOL 
    "BRECCONARY",                                                           MAP_LOCATIONS.BRECCONARY 
    "GARINHAM",                                                             MAP_LOCATIONS.GARINHAM 
    "CANTLIN", (* 10 *)                                                     MAP_LOCATIONS.CANTLIN 
    "RIMULDAR",                                                             MAP_LOCATIONS.RIMULDAR 
    "SUN_STONES_CAVE",   (* TANTEGEL_BASEMENT *)                            MAP_LOCATIONS.SUN_STONES_CAVE 
    "NORTHERN_SHRINE",                                                      MAP_LOCATIONS.STAFF_CAVE 
    "SOUTHERN_SHRINE",                                                      MAP_LOCATIONS.JERK_CAVE 
    "CHARLOCK_CAVE_1", (* 15 *)                                             MAP_LOCATIONS.CHARLOCK 
    "CHARLOCK_CAVE_2",                                                      MAP_LOCATIONS.CHARLOCK 
    "CHARLOCK_CAVE_3",                                                      MAP_LOCATIONS.CHARLOCK 
    "CHARLOCK_CAVE_4",                                                      MAP_LOCATIONS.CHARLOCK 
    "CHARLOCK_CAVE_5",                                                      MAP_LOCATIONS.CHARLOCK 
    "CHARLOCK_CAVE_6", (* 20 *)                                             MAP_LOCATIONS.CHARLOCK 
    "SWAMP_CAVE",                                                           ""
    "MOUNTAIN_CAVE",                                                        MAP_LOCATIONS.MOUNTAIN_CAVE 
    "MOUNTAIN_CAVE_2",                                                      MAP_LOCATIONS.MOUNTAIN_CAVE 
    "GARINS_GRAVE_1",                                                       MAP_LOCATIONS.GARINS_TOMB
    "GARINS_GRAVE_2", (* 25 *)                                              MAP_LOCATIONS.GARINS_TOMB
    "GARINS_GRAVE_3",                                                       MAP_LOCATIONS.GARINS_TOMB
    "GARINS_GRAVE_4",                                                       MAP_LOCATIONS.GARINS_TOMB
    "ERDRICKS_CAVE",                                                        MAP_LOCATIONS.TABLET_CAVE 
    "ERDRICKS_CAVE_2",                                                      MAP_LOCATIONS.TABLET_CAVE 
    |]

///////////////////////////////////////////////////////

// locations and items are just for a manual 'checklist' - second string is optional image to bring up when clicked
let voice = new System.Speech.Synthesis.SpeechSynthesizer()
type LocationIDs = 
    | TANTAGEL                = 0
    | CHARLOCK                = 1
    | BRECCONARY              = 2
    | RIMULDAR                = 3
    | RIMULDAR_BOX            = 4
    | CANTLIN                 = 5
    | CANTLIN_COORDS          = 6
    | KOL                     = 7
    | HAUKSNESS               = 8
    | HAUKSNESS_ITEM          = 9
    | GARINHAM                = 10
    | GARINHAM_BOXES          = 11
    | SUN_STONES_CAVE         = 12
    | SUN_STONES_CAVE_BOX     = 13
    | STAFF_OF_RAIN_CAVE      = 14
    | JERK_CAVE               = 15
    | SWAMP_NORTH             = 16
    | SWAMP_SOUTH             = 17
    | MOUNTAIN_CAVE           = 18
    | MOUNTAIN_CAVE_5_BOXES   = 19
    | TABLET_CAVE             = 20
    | TABLET_CAVE_BOX         = 21
    | GARINS_TOMB             = 22
    | GARINS_TOMB_3_BOXES     = 23
    | GARINS_TOMB_2_BOXES     = 24
let mutable UIThreadSynchronizationContext = null : System.Threading.SynchronizationContext
let ItemCheckboxes : System.Windows.Controls.CheckBox[] = Array.zeroCreate 10
let LocationCheckboxes : System.Windows.Controls.CheckBox[] = Array.zeroCreate 25
let mutable harpReminderIsPending = false
let rec harp() = 
    async {
        do! Async.SwitchToContext(UIThreadSynchronizationContext)
        if not ItemCheckboxes.[2].IsChecked.Value                                               // no staff yet
            && LocationCheckboxes.[int LocationIDs.STAFF_OF_RAIN_CAVE].IsChecked.Value then     // staff cave found
            harpReminderIsPending <- true
            do! Async.SwitchToThreadPool()
            voice.Speak("Turn in the silver harp")
            System.Threading.Thread.Sleep(System.TimeSpan.FromMinutes(2.0))
            harp()
        elif ItemCheckboxes.[2].IsChecked.Value then // staff
            harpReminderIsPending <- false
    } |> Async.Start
let mutable jerkReminderIsPending = false
let rec jerk() = 
    async {
        do! Async.SwitchToContext(UIThreadSynchronizationContext)
        if ItemCheckboxes.[0].IsChecked.Value && ItemCheckboxes.[2].IsChecked.Value && ItemCheckboxes.[3].IsChecked.Value && not ItemCheckboxes.[4].IsChecked.Value // stones, staff, token
            && LocationCheckboxes.[int LocationIDs.JERK_CAVE].IsChecked.Value then
            jerkReminderIsPending <- true
            do! Async.SwitchToThreadPool()
            voice.Speak("Get rainbow drop from jerk")
            System.Threading.Thread.Sleep(System.TimeSpan.FromMinutes(2.0))
            jerk()
        elif ItemCheckboxes.[4].IsChecked.Value then // drop
            jerkReminderIsPending <- false
    } |> Async.Start
let ITEMS = [|
    "Stones of Sunlight", ""         , (fun () -> if not jerkReminderIsPending then jerk())
    "Silver Harp", ""                , (fun () -> if not harpReminderIsPending then harp())
    "Staff of Rain", ""              , (fun () -> if not jerkReminderIsPending then jerk())
    "Erdrick Token", ""              , (fun () -> if not jerkReminderIsPending then jerk())
    "Rainbow Drop", ""               , (fun () -> ())
    "Erd Sword", "E_SWORD"           , (fun () -> ())
    "Erd Armor", "E_ARMOR"           , (fun () -> ())
    "Fairy Flute", ""                , (fun () -> ())
    "Death Necklace", ""             , (fun () -> ())
    "Princess' Love", ""             , (fun () -> ())
    |]
let howManyOf7SpecialItemsChecked() =
    let count(n) = if ItemCheckboxes.[n].IsChecked.HasValue && ItemCheckboxes.[n].IsChecked.Value then 1 else 0
    count(0) + count(1) + count(3) + count(5) + count(6) + count(7) + count(8)

///////////////////////////////////////////////////////

let WEAPONS = [|
    "(none)"
    "bamboo"
    "club"
    "copper"
    "handax"
    "broad"
    "flame"
    "erdrik"
    |]

let ARMOR = [|
    "(none)"
    "cloth"
    "leathr"
    "chain"
    "half"
    "full"
    "magic"
    "erdrik"
    |]

let SHIELD = [|
    "(none)"
    "small"
    "large"
    "silver"
    |]

///////////////////////////////////////////////////////
