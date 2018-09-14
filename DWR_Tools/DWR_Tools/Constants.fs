module Constants

///////////////////////////////////////////////////////

let OVERWORLD_MAP_TILE_FILENAMES = 
    [|
    "ow_bridge.png"
    "ow_castle.png"
    "ow_cave.png"
    "ow_desert.png"
    "ow_forest.png"
    "ow_hills.png"
    "ow_mountain.png"
    "ow_plains.png"
    "ow_swamp.png"
    "ow_town.png"
    "ow_wall.png"
    "ow_water.png"
    "ow_water_e.png"
    "ow_water_es.png"
    "ow_water_n.png"
    "ow_water_ne.png"
    "ow_water_nes.png"
    "ow_water_ns.png"
    "ow_water_nw.png"
    "ow_water_nwe.png"
    "ow_water_nwes.png"
    "ow_water_nws.png"
    "ow_water_s.png"
    "ow_water_w.png"
    "ow_water_we.png"
    "ow_water_wes.png"
    "ow_water_ws.png"
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
