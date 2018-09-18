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
        | Cave        -> System.Drawing.Color.White
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
    82
    165
    337
    600
    975
    1500
    2175
    3000
    4125
    5625
    7500
    9750
    12000
    14250
    16500
    19500
    22500
    25500
    28500
    31500
    34500
    |]

///////////////////////////////////////////////////////

// locations and items are just for a manual 'checklist' - second string is optional image to bring up when clicked
let LOCATIONS = [|
    "---Tantagel (4box, cave)", ""
    "Charlock Castle", "DW_Charlock.png"
    "Brecconary (Motel 6)", ""
    "Rimuldar (keys)", ""
    "---Rimuldar (1box)", ""
    "Cantlin (coordinates)", ""
    "Kol (fountain)", ""
    "Hauksness (dead)", ""
    "Garinham (grave below)", ""
    "---Garinham (3box)", ""
    "Sun Stones Cave (v)", ""
    "Staff Rain Cave (>)", ""
    "Jerk Cave (<)", ""
    "Swamp Cave North", "DW_SwampCave.png"
    "Swamp Cave South", "DW_SwampCave.png"
    "Mountain Cave (5box)", "DW_MountainCave.png"
    "Tablet Cave (1box)", "DW_TabletCave.png"
    "Garin's Tomb (3box)", "DW_GarinTomb.png"
    "---Garin's Tomb (2box)", "DW_GarinTomb.png"
    |]

let ITEMS = [|
    "Stones of Sunlight", ""
    "Silver Harp", ""
    "Staff of Rain", ""
    "Erdrick Token (*)", ""
    "Rainbow Drop", ""
    "Erdrick Sword", ""
    "Erdrick Armor (*)", ""
    "Fairy Flute (*)", ""
    "Death Necklace", ""
    "Princess' Love", ""
    |]

///////////////////////////////////////////////////////
