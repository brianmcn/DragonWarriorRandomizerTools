module Constants

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
        | Cave        -> System.Drawing.Color.White // CornflowerBlue 
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
let LocationCheckboxes : System.Windows.Controls.CheckBox[] = Array.zeroCreate 25
let LOCATIONS = [|
    "---Tantagel (4box, cave)", ""                       , (fun () -> ())              
    "Charlock Castle", "DW_Charlock.png"                 , (fun () -> ())
    "Brecconary (Motel 6)", ""                           , (fun () -> ())
    "Rimuldar (keys)", ""                                , (fun () -> ())
    "---Rimuldar (1box)", ""                             , (fun () -> LocationCheckboxes.[int LocationIDs.RIMULDAR].IsChecked <- System.Nullable.op_Implicit true)
    "Cantlin", ""                                        , (fun () -> if not LocationCheckboxes.[int LocationIDs.CANTLIN_COORDS].IsChecked.Value then async { voice.Speak("If you don't have keys, write down the location") } |> Async.Start)
    "---Cantlin coordinates", ""                         , (fun () -> LocationCheckboxes.[int LocationIDs.CANTLIN].IsChecked <- System.Nullable.op_Implicit true)
    "Kol (fountain)", ""                                 , (fun () -> ())
    "Hauksness (dead)", ""                               , (fun () -> if not LocationCheckboxes.[int LocationIDs.HAUKSNESS_ITEM].IsChecked.Value then async { voice.Speak("If you aren't strong enough, write down the location") } |> Async.Start)
    "---Hauksness item", ""                              , (fun () -> LocationCheckboxes.[int LocationIDs.HAUKSNESS].IsChecked <- System.Nullable.op_Implicit true)
    "Garinham (grave below)", ""                         , (fun () -> if not LocationCheckboxes.[int LocationIDs.GARINHAM_BOXES].IsChecked.Value then async { voice.Speak("If you don't have keys, write down the location") } |> Async.Start)
    "---Garinham (3box)", ""                             , (fun () -> LocationCheckboxes.[int LocationIDs.GARINHAM].IsChecked <- System.Nullable.op_Implicit true)
    "Sun Stones Cave (v)", ""                            , (fun () -> LocationCheckboxes.[int LocationIDs.SUN_STONES_CAVE_BOX].IsChecked <- System.Nullable.op_Implicit true)
    "---1 box", ""                                       , (fun () -> LocationCheckboxes.[int LocationIDs.SUN_STONES_CAVE].IsChecked <- System.Nullable.op_Implicit true)
    "Staff Rain Cave (>)", ""                            , (fun () -> ())
    "Jerk Cave (<)", ""                                  , (fun () -> ())
    "Swamp Cave North", "DW_SwampCave.png"               , (fun () -> ())
    "Swamp Cave South", "DW_SwampCave.png"               , (fun () -> ())
    "Mountain Cave (5box)", "DW_MountainCave.png"        , (fun () -> ())
    "---all 5 box", "DW_MountainCave.png"                , (fun () -> LocationCheckboxes.[int LocationIDs.MOUNTAIN_CAVE].IsChecked <- System.Nullable.op_Implicit true)
    "Tablet Cave", "DW_TabletCave.png"                   , (fun () -> ())
    "---1 box", "DW_TabletCave.png"                      , (fun () -> LocationCheckboxes.[int LocationIDs.TABLET_CAVE].IsChecked <- System.Nullable.op_Implicit true)
    "Garin's Tomb", "DW_GarinTomb.png"                   , (fun () -> if not LocationCheckboxes.[int LocationIDs.GARINS_TOMB_2_BOXES].IsChecked.Value then async { voice.Speak("Write down the tomb location") } |> Async.Start)
    "---top 3 box", "DW_GarinTomb.png"                   , (fun () -> LocationCheckboxes.[int LocationIDs.GARINS_TOMB].IsChecked <- System.Nullable.op_Implicit true)
    "---bottom 2 box", "DW_GarinTomb.png"                , (fun () -> LocationCheckboxes.[int LocationIDs.GARINS_TOMB].IsChecked <- System.Nullable.op_Implicit true
                                                                      LocationCheckboxes.[int LocationIDs.GARINS_TOMB_3_BOXES].IsChecked <- System.Nullable.op_Implicit true)
    |]
let ITEMS = [|
    "Stones of Sunlight", ""             , (fun () -> ())
    "Silver Harp", ""                    , (fun () -> ())
    "Staff of Rain", ""                  , (fun () -> ())
    "Erdrick Token (*)", ""              , (fun () -> ())
    "Rainbow Drop", ""                   , (fun () -> ())
    "Erdrick Sword", "E_SWORD"           , (fun () -> ())
    "Erdrick Armor (*)", "E_ARMOR"       , (fun () -> ())
    "Fairy Flute (*)", ""                , (fun () -> ())
    "Death Necklace", ""                 , (fun () -> ())
    "Princess' Love", ""                 , (fun () -> ())
    |]

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
