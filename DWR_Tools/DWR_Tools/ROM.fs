﻿module ROM

let CHESTS = [|
    "unused"
    "ARMOR"
    "HERB"
    "KEY"
    "TORCH"
    "FAIRY_WATER"  // 5
    "WINGS"
    "DRAGON_SCALE"
    "FLUTE"
    "RING"
    "TOKEN"        // 10
    "GWAELINS_LOVE"
    "CURSED_BELT"
    "HARP"
    "NECKLACE"
    "STONES"       // 15
    "STAFF"
    "SWORD"
    "GOLD_5"
    "GOLD_6"
    "GOLD_10"      // 20
    "GOLD_500"
    "GOLD_120"
    "TABLET"
    |]

let SHOP_ITEM = [|
    "BAMBOO_POLE"
    "CLUB"
    "COPPER_SWORD"
    "HAND_AXE"
    "BROAD_SWORD"
    "FLAME_SWORD"
    "ERDRICKS_SWORD"
    "CLOTHES"
    "LEATHER_ARMOR"
    "CHAIN_MAIL"
    "HALF_PLATE"
    "FULL_PLATE"
    "MAGIC_ARMOR"
    "ERDRICKS_ARMOR"
    "SMALL_SHIELD"
    "LARGE_SHIELD"
    "SILVER_SHIELD"
    "SHOP_HERB"
    "unused"
    "SHOP_TORCH"
    "unused"
    "SHOP_WINGS"
    "SHOP_DRAGON_SCALE"
    |]

let rng = new System.Random()
let attack(max2) = (rng.Next(max2+1) + max2) / 4
let player_attack(ap) = attack(max (ap - 100) 0)

let dl2_swings_chart() =
    let MIN = 124
    let NUM_AP = 30
    let wins_per_ap_per_swings = Array2D.zeroCreate NUM_AP 30
    let cumulative_wins_per_ap_per_swings = Array2D.zeroCreate NUM_AP 30
    let player(ap,fw) = 
        if ap <= 132 && fw > 0 then
            9+rng.Next(8), fw-1
        else
            player_attack(ap), fw
    for ap in [MIN..MIN+NUM_AP-1] do
        for dl_health in [150..165] do
            for i = 1 to 1000 do
                let mutable fw = 5
                let mutable h = dl_health
                let mutable swings = 0
                while h > 0 do
                    let d,f = player(ap,fw)
                    fw <- f
                    h <- h - d
                    swings <- swings + 1
                wins_per_ap_per_swings.[ap-MIN,swings] <- wins_per_ap_per_swings.[ap-MIN,swings] + 1
        cumulative_wins_per_ap_per_swings.[ap-MIN,0] <- wins_per_ap_per_swings.[ap-MIN,0]
        for i = 1 to 29 do
            cumulative_wins_per_ap_per_swings.[ap-MIN,i] <- cumulative_wins_per_ap_per_swings.[ap-MIN,i-1] + wins_per_ap_per_swings.[ap-MIN,i]
        printf "AP=%3d  " ap
        for s = 8 to 20 do
            printf "%2d:%3d%% " s (cumulative_wins_per_ap_per_swings.[ap-MIN,s]*100/16000)
        printfn ""
    printfn "        05%% 10%% 15%% 20%% 25%% 30%% 35%% 40%% 45%% 50%% 55%% 60%% 65%% 70%% 75%% 80%% 85%% 90%% 95%%100%%"
    for ap in [MIN..MIN+NUM_AP-1] do
        printf "AP=%3d  " ap
        for pct in [1..20] |> List.map (fun n -> n*5) do
            let mutable to_print = "    "
            for s = 8 to 20 do
                let x = (cumulative_wins_per_ap_per_swings.[ap-MIN,s]*100/16000)
                if x >= pct && x < pct+5 then
                    to_print <- sprintf " %2d " s
            printf "%s" to_print
        printfn ""
(*
UPDATE: with 5 fairy waters, top of chart is now

        05% 10% 15% 20% 25% 30% 35% 40% 45% 50% 55% 60% 65% 70% 75% 80% 85% 90% 95%100%
AP=124           15                          16                      17          20
AP=125   14                  15                          16                  17  19  20
AP=126       14                          15                          16          19  20
AP=127               14                              15                      16  18  20
AP=128   13                      14                              15              18  20
AP=129       13                          14                          15          17  20
AP=130               13                              14                      15  17  20
AP=131                       13                                  14              17  20
AP=132   12                          13                              14          16  20
AP=133   12                              13                              14      16  20

so 124-127 gains two fewer attacks, 128-131 has about one fewer.


                             30              50              70              90

AP=124       17                      18                          19              20
AP=125   16                  17                              18              19  20
AP=126               16                          17                      18      20
AP=127       15                          16                          17          20
AP=128   14                  15                              16              17  19  20
AP=129       14                              15                          16      18  20
AP=130                       14                              15                  18  20
AP=131       13                              14                          15      17  20
AP=132                   13                                  14                  16  20
AP=133   12                              13                              14      16  20
AP=134           12                                  13                      14  15  20
AP=135                       12                                  13              15  20
AP=136       11                              12                              13  15  20
AP=137               11                                  12                      14  20
AP=138                       11                                      12          14  20
AP=139   10                              11                              12      14  20
AP=140           10                                  11                          13  20
AP=141                   10                                      11              14  20
AP=142                           10                                      11      13  20
AP=143    9                                  10                              11  13  20
AP=144        9                                      10                          12  20
AP=145                9                                          10              12  20
AP=146                        9                                      10          11  20
AP=147                                9                                      10  11  20
AP=148    8                                       9                          10  11  20
AP=149        8                                           9                      11  20
AP=150            8                                               9              11  20
AP=151                    8                                           9          10  20
AP=152                            8                                       9      10  20
AP=153                                    8                                   9  10  20

num of swings to have: at least about 1/4, at least about 3/4

AP=124  18-19

AP=125  17-18
AP=126  17-18

AP=127  16-17

AP=128  15-16
AP=129  15-16

AP=130  14-15
AP=131  14-15

AP=132  13-14
AP=133  13-14
AP=134  13-14

AP=135  12-13
AP=136  12-13
AP=137  12-13

AP=138  11-12
AP=139  11-12
AP=140  11-12

AP=141  10-11
AP=142  10-11
AP=143  10-11
AP=144  10-11
AP=145  10

AP=146  9-10
AP=147  9-10
AP=148  9-10
AP=149  9-10
AP=150  9

AP=151  8-9

healmores(MP) to probable win AP, assuming exactly HM+2 swings (no doubles, no back attack):
 8( 64): AP >= 145  10 swings
 9( 72): AP >= 141  11 swings
10( 80): AP >= 138  12 swings
11( 88): AP >= 135  13 swings
12( 96): AP >= 132  14 swings
13(104): AP >= 130  15 swings
14(112): AP >= 128  16 swings
15(120): AP >= 127  17 swings
16(128): AP >= 125  18 swings
17(136): AP >= 124  19 swings
*)

let dl2_swings = """70% win swings
AP >= 151   9
AP >= 145  10
AP >= 141  11
AP >= 138  12
AP >= 135  13
AP >= 132  14
AP >= 130  15
AP >= 128  16
AP >= 127  17
AP >= 125  18
AP >= 124  19"""

let simulate_dl2_core(ag, start_hp, start_mp, max_hp, dp, ap) =
    let max_bite = 69 - (dp-2)/4
    let breath = [| 42; 44; 46; 48 |]
    let dl() =
        if rng.Next(2) = 0 then
            breath.[rng.Next(4)]
        else
            attack(max_bite*2)
    let player(fw) = 
        if ap <= 132 && fw > 0 then
            9+rng.Next(8), fw-1
        else
            player_attack(ap), fw
    let healmore() = 85 + rng.Next(16)
    let mutable wins = 0
    let mutable damage = 0
    let mutable sum_num_attacks = 0
    for i = 1 to 10000 do
        // simulate fight
        let mutable num_fw = 0
        let mutable player_died = false
        let mutable hp = start_hp
        let mutable mp = start_mp
        let mutable num_attacks = 0
        let dl_start_hp = rng.Next(16) + 150
        let mutable dl_hp = dl_start_hp 
        if rng.Next(256)*50 > rng.Next(256)*ag then
            hp <- hp - dl()
            if hp <= 0 then
                player_died <- true  // died to back attack
        while not player_died && dl_hp > 0 do
            // player strategy
            if (hp <= max_bite || hp <= 48) && mp >= 8 then   // heal above max - conservative
//            if (hp <= 48) && mp >= 8 then   // heal above 48 - usually increases odds on close fight though risks a max bite
                mp <- mp - 8
                hp <- hp + healmore()
                if hp > max_hp then
                    hp <- max_hp
            else
                let dmg,fw = player(num_fw)
                num_fw <- fw
                dl_hp <- dl_hp - dmg
                damage <- damage + dmg
                num_attacks <- num_attacks + 1
            if dl_hp > 0 then
                hp <- hp - dl()
                if hp <= 0 then
                    player_died <- true
        if not player_died then
            wins <- wins + 1
        elif false then //ap = 140 then
            printfn "lost, after player had %d attacks, DL had %d of %d left" num_attacks dl_hp dl_start_hp
        sum_num_attacks <- sum_num_attacks + num_attacks
    if false then //ap = 141 then
        printfn "player averaged %5.1f attacks, each averaging %5.1f damage" (float sum_num_attacks / 10000.0) (float damage / float sum_num_attacks)
    wins/10
let simulate_dl2(ag, start_hp, start_mp, max_hp, dp, ap) =
    let no_dn = simulate_dl2_core(ag, start_hp, start_mp, max_hp, dp, ap)
    let dn = simulate_dl2_core(ag, start_hp-max_hp/4, start_mp, max_hp*3/4, dp, ap+10)
    max no_dn dn

let compute_go_mode(str, ag, hp, mp, s:string) =
    let str, ag, hp, mp = int str, int ag, int hp, int mp
    let max_dl = ((int str + 42) - 100) / 2
    let avg_dl = max_dl * 3 / 4
    (simulate_dl2(ag,hp-20,mp,hp,(ag/2)+48,str+42)), // assumes survive dl1 with max-20, silver shield, sword+FR
        (simulate_dl2(ag,hp-20,mp-8,hp,(ag/2)+48,str+42)),  // assumes survive dl1 with max-20, silver shield, sword+FR, lost a healmore
        (simulate_dl2(ag,hp-20,mp,hp,(ag/2)+38,str+42)), // assumes survive dl1 with max-20, large shield, sword+FR
        (simulate_dl2(ag,hp-20,mp-8,hp,(ag/2)+38,str+42))  // assumes survive dl1 with max-20, large shield, sword+FR, lost a healmore

let mutable agg_count = 0
let agg_stats = Array2D.zeroCreate 30 4
let show_go_mode_stats(bytes:byte[], print, file) =
    if print then
        printfn "for build 'Z' (STR+HP)..."
    let header1 = "   silver shield   large shield" 
    let header2 = "    full down1heal full down1   LV    STR   AGI    HP    MP  rawAG rawMP" 
    let mutable p_str, p_ag, p_hp, p_mp, p_hu, hu = 0, 0, 0, 0, 0, 0
    let mutable go_mode,lhe,lHE,lhu,lHU = 31, 31, 31, 31, 31
    let mutable return_L1 = false
    if print then
        printfn "%s" header1
        printfn "%s" header2
    for i = 0 to 29 do
        let fives = (if i%5=4 then "-- " else "   ")
        let b1 = bytes.[0x60DD+6*i+4]
        let b2 = bytes.[0x60DD+6*i+5]
        let s = if b2 &&&  1uy > 0uy then ((if lhe=31 then lhe <- i+1); "HE ") else fives 
              + if b2 &&&  2uy > 0uy then ((if lhu=31 then lhu <- i+1); "HU ") else fives
              + if b2 &&&  4uy > 0uy then "SL " else fives
              + if b2 &&&  8uy > 0uy then "RA " else fives
              + if b2 &&& 16uy > 0uy then "ST " else fives
              + if b2 &&& 32uy > 0uy then "OU " else fives
              + if b2 &&& 64uy > 0uy then ((if i=0 then return_L1 <- true); "RT ") else fives
              + if b2 &&&128uy > 0uy then "RP " else fives
              + if b1 &&&  1uy > 0uy then ((if lHE=31 then lHE <- i+1); "HE ") else fives
              + if b1 &&&  2uy > 0uy then ((if lHU=31 then lHU <- i+1); hu <- 1; "HU ") else fives
//        if i=0 && (b2 &&&  8uy > 0uy) then
//            failwithf "radiant at start: %s" file
        let have_healmore = b1 &&&  1uy > 0uy 
        let str, ag, hp, mp = bytes.[0x60DD+6*i+0], bytes.[0x60DD+6*i+1], bytes.[0x60DD+6*i+2], bytes.[0x60DD+6*i+3] 
        let agZ = ag - ((ag+9uy)/10uy) + 3uy
        let mpZ = mp - ((mp+9uy)/10uy) + 3uy
        let hpSTRAG = hp - ((hp+9uy)/10uy) + 3uy
        let big_str = i<>0 && int str - p_str > 12
        let big_ag = i<>0 && int agZ - p_ag > 12
        let big_hp = i<>0 && int hp - p_hp > 16
        let big_mp = i<>0 && int mpZ - p_mp > 12
        let big_hu = hu = 1 && p_hu = 0
        let big_any = big_str || big_ag || big_hp || big_mp || big_hu
        p_str <- int str
        p_ag <- int agZ
        p_hp <- int hp
        p_mp <- int mpZ
        p_hu <- hu
        let wins,wins_less1_heal,wins_lg,wins_lg_less1_heal = 
            if have_healmore then
                compute_go_mode(str, agZ, hp, mpZ, s)
            else
                0, 0, 0, 0
        let x(b) = if b then "+" else " "
        if print then
            printfn "%s %5.1f%% %5.1f%% %5.1f%% %5.1f%% %3d %s%3d%s  %3d%s  %3d%s  %3d%s  %3d  %3d   %s" 
                (if big_any then "**" else "  ") (float wins / 10.0) (float wins_less1_heal / 10.0) (float wins_lg / 10.0) (float wins_lg_less1_heal / 10.0) 
                (i+1) fives str (x big_str) agZ (x big_ag) hp (x big_hp) mpZ (x big_mp) ag mp s 
        if (float wins_lg / 10.0) > 60. then  // if >60% with large, call it go-mode
            if go_mode=31 then
                go_mode <- i+1
        if i=14 && print then
            printfn "%s" header1
            printfn "%s" header2
        agg_stats.[i,0] <- int str + agg_stats.[i,0]
        agg_stats.[i,1] <- int agZ + agg_stats.[i,1]
        agg_stats.[i,2] <- int hp  + agg_stats.[i,2]
        agg_stats.[i,3] <- int mpZ + agg_stats.[i,3]
    agg_count <- agg_count+1
    go_mode, lhe, lhu, lHE, lHU, return_L1

let decode_rom(file) =
    let bytes = System.IO.File.ReadAllBytes(file)
    let content = bytes.[16..]   // first 16 bytes are a header

    let location_text_bytes = content.[0xA242..0xA242+70]
    let dw_alphabet = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ\"\"'*>_:__.,-_?!;)(``_'___________  "
    let chars = Array.init location_text_bytes.Length (fun i -> try dw_alphabet.[int location_text_bytes.[i]] with _ -> ' ')
    let location_str = (new System.String(chars))
    //printfn "loc: %s" location_str 
    // From Tantegel Castle travel 49 leagues to the north and 24 to the west
    let to1 = location_str.IndexOf("to the")
    let to2 = location_str.IndexOf("and",to1+1)
    let buried_dx, buried_dy = 
        try
            let num1 = int(location_str.Substring(to1 - 12,3))
            let num2 = int(location_str.Substring(to2 + 4,3))
            //printfn "%d %d" num1 num2
            let buried_dy = if location_str.Contains("north") then -num1 else num1
            let buried_dx = if location_str.Contains("wes") then -num2 else num2  // wes, not west, since 3-digit numbers cut off text
            buried_dx, buried_dy 
        with _ -> -999, -999

    // spike tile enemies
    let hauks_enemy    = int content.[0xcd64]
    let swamp_enemy    = int content.[0xcd81]
    let charlock_enemy = int content.[0xcd9e]
    let mutable hauks_info = sprintf "  hauks: %2d %s" hauks_enemy (EnemyData.ENEMY_NAME hauks_enemy)
    let mutable swamp_info = sprintf "  swamp: %2d %s" swamp_enemy (EnemyData.ENEMY_NAME swamp_enemy)
    let mutable charlock_info = sprintf "  charl: %2d %s" charlock_enemy (EnemyData.ENEMY_NAME charlock_enemy)

    let uniqueItems = new System.Collections.Generic.Dictionary<_,_>()
    // buried items
    printfn ""
    printfn "BURIED ITEMS"    
    // 0e11d+1, +32, +54 = kol, hauks, buried
    for address_offset, location in [1, "overworld coords"; 32, "kol"; 54, "hauks"] do
        for idx in [17; 1; 14; 13; 15; 8; 10] do // 7 special items that can be buried
            let patched_data = content.[0x0e11d..0x0e11d+109]
            if int patched_data.[address_offset] = idx then
                let prettyCoords() = sprintf "%d %s %d %s" (abs buried_dy) (if buried_dy<=0 then "N" else "S") (abs buried_dx) (if buried_dx<=0 then "W" else "E")
                printfn "  %12s is located at %s %s" CHESTS.[idx] location (if address_offset=1 then prettyCoords() else "")
                uniqueItems.Add(idx, (if address_offset=1 then (sprintf "OVERWORLD %s" (prettyCoords()),sprintf "%d,%d" buried_dx buried_dy) 
                                                          elif address_offset=32 then ("KOL",Constants.MAP_LOCATIONS.KOL) 
                                                          else ("HAUKSNESS",Constants.MAP_LOCATIONS.HAUKSNESS)))

    // chests
    // each chest data is 4 bytes: map, x, y, item
    printfn ""
    let chest_bytes = content.[0x5dcd..0x5dcd+4*31]
    let all = new System.Text.StringBuilder()
    let sra = ResizeArray()
    let ksb = new System.Text.StringBuilder()
    let mutable num_keys_throne_room = 0
    let mutable wings_in_throne_room = false
    let throne_room_items = ResizeArray()
    let tantagel_treasury_items = ResizeArray()
    for i = 0 to 30 do
        let map, item = chest_bytes.[i*4], chest_bytes.[i*4+3] 
        all.AppendLine(sprintf "  %20s    %-20s" (fst Constants.MAPS.[int map]) CHESTS.[int item]) |> ignore
        match item with
        | 1uy | 8uy | 9uy | 10uy | 13uy | 14uy | 15uy | 17uy -> 
            sra.Add(CHESTS.[int item], (fst Constants.MAPS.[int map]))  // 7 key items plus RING
            uniqueItems.Add(int item, ((fst Constants.MAPS.[int map]),(snd Constants.MAPS.[int map])))
        | 3uy -> 
            if map <> 5uy then 
                ksb.Append(Constants.MAPS.[int map]).Append("  ") |> ignore // KEY
            else
                num_keys_throne_room <- num_keys_throne_room + 1
        | 6uy -> if map = 5uy then wings_in_throne_room <- true
        | _ -> ()
        if map = 5uy then
            if item<>3uy || num_keys_throne_room > 1 then
                throne_room_items.Add(CHESTS.[int item])
        if map = 4uy then
            tantagel_treasury_items.Add(CHESTS.[int item])
    printfn "SUMMARY"
    let sorted = sra.ToArray() |> Array.sortBy (fun (item,_loc) ->
        match item with
        | "STONES" -> 1
        | "HARP" -> 2
        | "TOKEN" -> 3
        | "RING" -> 4
        | "FLUTE" -> 5
        | "NECKLACE" -> 6
        | "SWORD" -> 7
        | "ARMOR" -> 8
        | _ -> failwith "bad summary item"
        )
    for item,loc in sorted do
        printfn "  %12s   %s" item loc
//        if item="HARP" && loc="TANTEGEL_THRONE_ROOM" then
//            failwithf "harp in throne room: %s" file
    printfn ""
    printfn "KEYS: %s" (ksb.ToString())
    printfn ""
    printfn "ALL CHESTS"
    printf "%s" (all.ToString())

    // shops
    printfn ""
    printfn "SHOPS"
    let mutable cur_shop = "kol"
    let shops = content.[0x1991..0x1991+80]
    let mutable shop_count = 0
    let shop_items = new System.Collections.Generic.Dictionary<_,_>()
    let cur_items = ResizeArray()
    for item in shops do
        if shop_count < 7 then
            if item = 253uy then
                shop_count <- shop_count + 1
                let desc = 
                    match shop_count with
                    | 1 -> "brecc"
                    | 2 -> "garin"
                    | 3 -> "cantlin open 1"
                    | 4 -> "cantlin open 2"
                    | 5 -> "cantlin locked"
                    | 6 -> "rimu"
                    | 7 -> ""
                    | _ -> failwith "bad shop"
                cur_items.Add("") // ensure always at least 6
                shop_items.Add(cur_shop, cur_items.ToArray())
                cur_items.Clear()
                cur_shop <- desc
            else
                cur_items.Add(SHOP_ITEM.[int item])
    let si(x,y) = // vanilla has smaller shops
        try shop_items.[x].[y]
        with _ -> ""
    let s1,s2,s3,s4 = "kol", "brecc", "garin", "rimu"
    printfn ""
    printfn "%-26s %-26s %-26s %-26s" s1 s2 s3 s4
    printfn ""
    for i = 0 to 5 do
        printfn "%-26s %-26s %-26s %-26s" (si(s1,i)) (si(s2,i)) (si(s3,i)) (si(s4,i))
    let s1,s2,s3 = "cantlin open 1", "cantlin open 2", "cantlin locked"
    printfn ""
    printfn "%-26s %-26s %-26s" s1 s2 s3
    printfn ""
    for i = 0 to 5 do
        printfn "%-26s %-26s %-26s" (si(s1,i)) (si(s2,i)) (si(s3,i))
    
    let monster_data = bytes.[0x5E5B..0x60DB]
    //Strength, Agility, HP, spells, resistance, dodge, xp, gold, 8 bytes of graphics
    // XX-- ----    00 = sleep, 01 = stopspell, 10 = heal, 11 = healmore
    // ---- XX--    00 = hurt, 01 = hurtmore, 10 = weak breath, 11 = dl2 breath
    // --XX ----    0, 25, 50, 75%
    // ---- --XX    0, 25, 50, 75%
    let spell(b) =
        let util_spell = (b &&& 0xc0uy) / 64uy
        let util_pct   = (b &&& 0x30uy) / 16uy
        let attack     = (b &&& 0x0cuy) / 4uy
        let attack_pct = (b &&& 0x03uy)
        let pct(x) = 
            match x with
            | 0uy -> 0
            | 1uy -> 25
            | 2uy -> 50
            | 3uy -> 75
        let util(x) = 
            match x with
            | 0uy -> "SLEEP"
            | 1uy -> "STOPSPELL"
            | 2uy -> "HEAL"
            | 3uy -> "HEALMORE"
        let atk(x) = 
            match x with
            | 0uy -> "HURT"
            | 1uy -> "HURTMORE"
            | 2uy -> "WEAK BREATH"
            | 3uy -> "DL2 BREATH"
        let sb = new System.Text.StringBuilder()
        let u = pct(util_pct)
        let a = pct(attack_pct)
        match u, a with
        | 0, 0 -> ""
        | 0, _ -> sprintf "%d%% %s" a (atk attack)
        | _, 0 -> sprintf "%d%% %s" u (util util_spell)
        | _, _ -> sprintf "%d%% %s, %d%% %s" a (atk attack) u (util util_spell)
    let zone_has_charlock_enemy = Array.zeroCreate 20
    let zone_has_metal_slime = Array.zeroCreate 20
    let zone_has_x = Array.zeroCreate 20  // for ad-hoc mapping
    let zone_has_y = Array.zeroCreate 20  // for ad-hoc mapping
    let zone_enemies = Array.create 20 null
    for zone = 0 to 19 do
        let data = content.[0xf54f+5*zone..0xf54f+5*zone+4] |> Array.sort
        let extra = 
            match zone with
            | 13 -> "HAUKS"
            | 14 -> "M2/G1"
            | 15 -> "GTLOW"
            | 16 | 17 | 18 -> "CHAR "
            | 19 -> "SWAMP"
            | _  -> "     "
        printf "zone %2d (%s): " zone extra
        zone_enemies.[zone] <- Array.create 5 (0,"","",0uy)
        for i = 0 to 4 do
            let enemy=data.[i]
            let name,_,_,_,_,_ = EnemyData.ENEMY_DATA.[int enemy]
            let spells = spell(monster_data.[16*(int enemy)..16*(int enemy)+15].[3])
            let ss_resist = monster_data.[16*(int enemy)..16*(int enemy)+15].[4] &&& 0xFuy
            zone_enemies.[zone].[i] <- int enemy, name, spells, ss_resist
            printf "%3d %-16s " enemy name
            if enemy = 16uy then
                zone_has_metal_slime.[zone] <- zone_has_metal_slime.[zone] + 1
            // golem is 24, werewolf is 29
            if enemy = 24uy || enemy >= 29uy then
                zone_has_charlock_enemy.[zone] <- zone_has_charlock_enemy.[zone] + 1
            if enemy = 35uy then // stoneman
                zone_has_x.[zone] <- zone_has_x.[zone] + 1
            if enemy = 36uy then // armored_knight
                zone_has_y.[zone] <- zone_has_y.[zone] + 1
        printfn ""

    let overworld_zone_data = content.[0xf522..0xf522+31]
    let ow_zones = Array2D.zeroCreate 8 8
    for x = 0 to 7 do
        for y = 0 to 7 do
            let zone_index = x + 8*y
            let i = zone_index / 2
            if zone_index % 2 = 0 then
                ow_zones.[x,y] <- (overworld_zone_data.[i] &&& 0xf0uy) >>> 4
            else
                ow_zones.[x,y] <- (overworld_zone_data.[i] &&& 0x0fuy)

    printfn "OW ZONES"
    for y = 0 to 7 do
        for x = 0 to 7 do
            printf "%3d" ow_zones.[x,y]
        printfn ""
    printfn ""

    let map_pointers_0 = content.[0x2653]  // 120 16-bit pointers (subtract 0x9d5d from these pointers)
    let map_encoded_0 = content.[0x1d5d]   // RLE, top nibble tile, bottom nibble count

    let ptr_0 = System.BitConverter.ToUInt16(content, 0x2653)
//    printfn "%u %x" ptr_0 ptr_0

    let map_pointers = [|
        for i = 0 to 119 do
            yield System.BitConverter.ToUInt16(content, 0x2653 + 2*i)
        |]
    let encoded = content.[0x1d5d..]
    let tiles = Array2D.zeroCreate 120 120
    for row = 0 to 119 do
//        printfn "row: %d" row
        let mutable offset = int(map_pointers.[row] - uint16 0x9d5d)
//        printfn "offset: %d" offset
        let mutable x = 0
        while x < 120 do
            let cur_byte = encoded.[offset]
            let tile = cur_byte >>> 4
            let count = int(cur_byte &&& 0xfuy) + 1
//            printfn "x %d tile %d count %d" x tile count
// tantagel at 83 98
            for j = 0 to count-1 do
                if x < 120 then  // TODO kludge
                    tiles.[row,x] <- tile
                    x <- x + 1
            offset <- offset + 1

(*
    printfn "map:"
    printfn ""
    for row = 0 to 119 do
        for col = 0 to 119 do
            printf "%x" tiles.[row,col]
        printfn ""
    printfn ""
*)

    let WARPS_INDEX_IN_BYTES = 0xf3d8
    let warps = bytes.[WARPS_INDEX_IN_BYTES..]
    let mapCoords = new System.Collections.Generic.Dictionary<_,_>()
    let mutable uc1i, uc2i = -1, -1           // useless charlock warp index 1 & 2
    let mutable rimi = -1                     // rimuldar index
    let mutable gf, tf = (fun _ -> ()), (fun _ -> ())  // thunks to backpatch basement labels when find topside coords later
    let thingsToTryAllPlace = ResizeArray()
    for i = 0 to 50 do
        let d = i*3
        let from_map = warps.[0+d]
        let from_x = warps.[1+d]
        let from_y = warps.[2+d]
        let to_map = warps.[153+d]
        let to_x = warps.[154+d]
        let to_y = warps.[155+d]
        let printIt = false //from_map = 1uy || from_map = 9uy || from_map = 4uy 
        let debugPrint = false
        if from_map = 15uy && from_x = 13uy && from_y = 7uy then
            uc1i <- i
        if from_map = 15uy && from_x = 19uy && from_y = 7uy then
            uc2i <- i
        if from_map = 1uy && from_x = 22uy && from_y = 47uy then
            rimi <- i
        if debugPrint || printIt then
            printf "%3d %3d %3d %3d %3d %3d " from_map from_x from_y to_map to_x to_y
        let dest() = 
            match to_map with
            | 9uy  -> Constants.MAP_LOCATIONS.GARINHAM, false, [|"XXXXX";"X    ";"X XXX";"X   X";"XXXXX"|]
            | 13uy -> Constants.MAP_LOCATIONS.STAFF_CAVE, true, [|" X   ";"  X  ";"   X ";"  X  ";" X   "|]
            | 7uy  -> Constants.MAP_LOCATIONS.KOL, false, [|"X  X ";"X X  ";"XX   ";"X X  ";"X  X "|]
            | 8uy  -> Constants.MAP_LOCATIONS.BRECCONARY, false, [|"XXXX ";"X  X ";"XXXXX";"X   X";"XXXXX"|]
            | 4uy  -> Constants.MAP_LOCATIONS.TANTAGEL, false, [|"XXXXX";"  X  ";"  X  ";"  X  ";"  X  "|]
            | 21uy when to_y = 0uy -> Constants.MAP_LOCATIONS.SWAMP_NORTH, true, [|"X   X";"XX  X"; "X X X";"X  XX";"X   X"|]
            | 21uy when to_y <> 0uy -> Constants.MAP_LOCATIONS.SWAMP_SOUTH, true, [|"XXXXX"; "X    ";"XXXXX";"    X";"XXXXX"|]
            | 2uy  -> Constants.MAP_LOCATIONS.CHARLOCK, false, null
            | n when n=6uy || (n >= 15uy && n <= 20uy) -> Constants.MAP_LOCATIONS.CHARLOCK, false, null  // short charlock (or anywhere in charlock)
            | 22uy -> Constants.MAP_LOCATIONS.MOUNTAIN_CAVE, true, [|"X   X";"XX XX";"X X X";"X   X";"X   X"|]
            | 11uy -> Constants.MAP_LOCATIONS.RIMULDAR, false, [|"XXXXX";"X   X";"XXXX ";"X   X";"X   X"|]
            | 3uy  -> Constants.MAP_LOCATIONS.HAUKSNESS, false, [|"X   X";"X   X";"XXXXX";"X   X";"X   X"|]
            | 10uy -> Constants.MAP_LOCATIONS.CANTLIN, false, [|"XXXXX";"X    ";"X    ";"X    ";"XXXXX"|]
            | 14uy -> Constants.MAP_LOCATIONS.JERK_CAVE, true, [|"   X ";"  X  ";" X   ";"  X  ";"   X "|]
            | 28uy -> Constants.MAP_LOCATIONS.TABLET_CAVE, true, [|"XXXXX";"  X  ";"  X  ";"  X  ";"  X  "|]
            | 12uy -> Constants.MAP_LOCATIONS.SUN_STONES_CAVE, true, [|"     ";"X   X";" X X ";"  X  ";"     "|]
            | 24uy -> Constants.MAP_LOCATIONS.GARINS_TOMB, true, [|"XXXXX";"X    ";"X XXX";"X   X";"XXXXX"|]
            | 6uy  -> "Charlock Throne", false, null  // in short-charlock
            | _ -> failwith "unexpected warp dest"
        if from_map = 1uy then
            let dest, isCave, a = dest()
            printfn "at %3d %3d : %s" from_x from_y dest
            if Constants.MAP_LOCATIONS.IsLocation(dest) then
                mapCoords.Add(dest, (int from_x,int from_y) )
            if a <> null then
                thingsToTryAllPlace.Add((int from_x, int from_y, isCave, a))
            if dest=Constants.MAP_LOCATIONS.GARINHAM then
                gf(int from_x,int from_y)
            if dest=Constants.MAP_LOCATIONS.TANTAGEL then
                tf(int from_x,int from_y)
        elif from_map = 9uy then
            let dest, isCave, a = dest()
            printfn "under Garin: %s" dest
            match mapCoords.TryGetValue(Constants.MAP_LOCATIONS.GARINHAM) with
            | true, (gx,gy) -> 
                thingsToTryAllPlace.Add((gx, gy, isCave, a))
                mapCoords.Add(dest, (gx,gy))
            | _ -> gf <- fun (gx,gy) -> 
                thingsToTryAllPlace.Add((gx, gy, isCave, a))
                mapCoords.Add(dest, (gx,gy))
        elif from_map = 4uy then
            let dest, isCave, a = dest()
            printfn "under Tantagel: %s" dest
            match mapCoords.TryGetValue(Constants.MAP_LOCATIONS.TANTAGEL) with
            | true, (tx,ty) -> 
                thingsToTryAllPlace.Add((tx, ty, isCave, a))
                mapCoords.Add(dest, (tx,ty))
            | _ -> tf <- fun (tx,ty) -> 
                thingsToTryAllPlace.Add((tx, ty, isCave, a))
                mapCoords.Add(dest, (tx,ty))
        else
            if debugPrint || printIt then
                printfn ""

    // compute uniqueItem coordinates
    let uniqueItemLocations = ResizeArray()
    for KeyValue(item, (desc,location)) in uniqueItems do
        match mapCoords.TryGetValue(location) with
        | true, (x,y) ->
            uniqueItemLocations.Add(item, (desc,x,y))
        | _ ->
            let tx,ty = mapCoords.[Constants.MAP_LOCATIONS.TANTAGEL]
            let [|dxs;dys|] = location.Split(',')
            let dx = System.Int32.Parse(dxs)
            let dy = System.Int32.Parse(dys)
            uniqueItemLocations.Add(item, (desc,tx+dx,ty+dy))

    let tiles = Array2D.init 120 120 (fun x y -> Constants.OverworldMapTile.FromROMByte tiles.[x,y])

    let EDGE = 6
    let bmp1 = new System.Drawing.Bitmap(120+2*EDGE,120+2*EDGE)
    let bmp2 = new System.Drawing.Bitmap(120+2*EDGE,120+2*EDGE)
    for x = 0 to bmp2.Width-1 do
        for y = 0 to bmp2.Height-1 do
            bmp1.SetPixel(y, x, Constants.OverworldMapTile.Water_xxxx.ProjectionColor)
            bmp2.SetPixel(y, x, Constants.OverworldMapTile.Mountain.AltProjectionColor)
    for x = 0 to 119 do
        for y = 0 to 119 do
            bmp1.SetPixel(y+EDGE, x+EDGE, tiles.[x,y].ProjectionColor)
            bmp2.SetPixel(y+EDGE, x+EDGE, tiles.[x,y].AltProjectionColor)
    // remove unreachable continents
    let reachable_continents = Array2D.zeroCreate 120 120
    let walk(x, y, label) =
        let rec loop(x, y, label, acc, k) = 
            if x < 0 || y < 0 || x > 119 || y > 119 then  // outside map
                k acc
            elif reachable_continents.[x,y] <> 0 then     // already marked
                k acc
            elif not(tiles.[y,x].IsWalkable) then         // can't be walked
                k acc
            else
                reachable_continents.[x,y] <- label
                loop(x-1,y,label,acc, (fun acc -> 
                loop(x+1,y,label,acc, (fun acc -> 
                loop(x,y-1,label,acc, (fun acc -> 
                loop(x,y+1,label,acc, (fun acc -> k (acc+1)))))))))
        if reachable_continents.[x,y] <> 0 then
            failwith "bad walk() call"
        loop(x, y, label, 0, fun s -> s)
    let tx,ty = mapCoords.[Constants.MAP_LOCATIONS.TANTAGEL]
    let nx,ny = try mapCoords.[Constants.MAP_LOCATIONS.SWAMP_NORTH] with _ -> -1,-1
    let sx,sy = try mapCoords.[Constants.MAP_LOCATIONS.SWAMP_SOUTH] with _ -> -1,-1
    let gx,gy = mapCoords.[Constants.MAP_LOCATIONS.GARINHAM]
    let cont_1_size = walk(tx, ty, 1)  // label tantagel as continent 1
    let cont_2_size = 
        if nx <> -1 && reachable_continents.[nx,ny]=0 then
            walk(nx, ny, 2)  // swamp N not on tantagel continent, label 2
        elif sx <> -1 && reachable_continents.[sx,sy]=0 then
            walk(sx, sy, 2)  // swamp S not on tantagel continent, label 2
        elif reachable_continents.[gx,gy]=0 then
            walk(gx, gy, 2)  // garinham not on tantagel continent, label 2
        else
            0  // single continent
    for x = 0 to 119 do
        for y = 0 to 119 do
            if reachable_continents.[x,y] = 0 then
                // charlock is 'unreachable', but dont uncolor it
                let cx,cy = mapCoords.[Constants.MAP_LOCATIONS.CHARLOCK]
                if abs(x-cx) < 4 && abs(y-cy) < 4 then
                    () // do nothing, dont discolor charlock
                else
                    bmp2.SetPixel(x+EDGE,y+EDGE, Constants.OverworldMapTile.Mountain.AltProjectionColor)
    let pixelsWhereLabelsHaveModified = new System.Collections.Generic.HashSet<_>()
    let tryAllPlace(x,y,isCave,a:string[]) =
        let darken(x,y) =
            let c = bmp2.GetPixel(x,y)
            let K = 7
            bmp2.SetPixel(x,y, System.Drawing.Color.FromArgb(int c.R*K/8, int c.G*K/8, int c.B*K/8))
            pixelsWhereLabelsHaveModified.Add(x,y) |> ignore
        let tryPlace(x,y,isCave,a:string[]) =
            let mutable works = true
            for i = 0 to 4 do
                for j = 0 to 4 do
                    if pixelsWhereLabelsHaveModified.Contains(x+i,y+j) then
                        works <- false
                    if bmp2.GetPixel(x+i,y+j).ToArgb() = Constants.OverworldMapTile.Town.AltProjectionColor.ToArgb() then
                        works <- false
                    if bmp2.GetPixel(x+i,y+j).ToArgb() = Constants.OverworldMapTile.Cave.AltProjectionColor.ToArgb() then
                        works <- false
                    if bmp2.GetPixel(x+i,y+j).ToArgb() = Constants.OverworldMapTile.Wall.AltProjectionColor.ToArgb() then
                        works <- false
            if works then
                for i = 0 to 4 do
                    for j = 0 to 4 do
                        if a.[j].[i] = 'X' then
                            bmp2.SetPixel(x+i,y+j, if isCave then Constants.OverworldMapTile.Cave.AltProjectionColor else Constants.OverworldMapTile.Town.AltProjectionColor)
                            pixelsWhereLabelsHaveModified.Add(x+i,y+j) |> ignore
                        else
                            darken(x+i,y+j)
            works            
        if not(tryPlace(x+EDGE-3, y, isCave, a)) then
            if not(tryPlace(x, y+EDGE-1, isCave, a)) then
                if not(tryPlace(x+EDGE-1, y+EDGE+2, isCave, a)) then
                    if not(tryPlace(x+EDGE+2, y+EDGE-3, isCave, a)) then
                        printfn "failed to place a label!"
                    else
                        darken(x+EDGE+1,y+EDGE)
                else
                    darken(x+EDGE,y+EDGE+1)
            else
                darken(x+EDGE-1,y+EDGE)
        else
            darken(x+EDGE,y+EDGE-1)
    for x,y,isCave,a in thingsToTryAllPlace do
        tryAllPlace(x,y,isCave,a)
    // darken 2nd continent (brighten rest of map)
    for x = 0 to 119 do
        for y = 0 to 119 do
            let c = bmp2.GetPixel(x+EDGE,y+EDGE)
            let m(a) = 
                if reachable_continents.[x,y] = 2 then
                    max (int a - 0) 0
                else
                    min (int a + 40) 255
            let c2 = System.Drawing.Color.FromArgb(m c.R, m c.G, m c.B)
            bmp2.SetPixel(x+EDGE,y+EDGE, c2)

    (*
    let kx,ky = mapCoords.[Constants.MAP_LOCATIONS.KOL]
    let bx,by = mapCoords.[Constants.MAP_LOCATIONS.BRECCONARY]
    printfn "Kol   is on continent %d" reachable_continents.[kx,ky]
    printfn "Brecc is on continent %d" reachable_continents.[bx,by]
    *)
    let compute_charlock_distance_to_inn() =
        let visited = Array2D.zeroCreate 120 120
        let cx,cy = mapCoords.[Constants.MAP_LOCATIONS.CHARLOCK]
        let hx,hy = mapCoords.[Constants.MAP_LOCATIONS.HAUKSNESS]
        let cx = cx + 3 // reachable location on start map across to-be-created bridge
        let q = new System.Collections.Generic.Queue<_>()
        q.Enqueue( (cx,cy,3) )
        let mutable distance, finished = 999, false
        while not finished && not(q.Count = 0) do
            let x, y, dist = q.Dequeue()
            distance <- dist
            if not visited.[x,y] then
                visited.[x,y] <- true
                match tiles.[x,y] with
                | Constants.OverworldMapTile.Castle -> finished <- true  // must be tantagel, charlock not reachable
                | Constants.OverworldMapTile.Town when not(x=hx && y=hy) -> finished <- true // in town
                | _ -> ()
                let can_walk(x,y) = x>=0 && y>=0 && x<=119 && y<=119 && tiles.[x,y].IsWalkable 
                if not finished then
                    if can_walk(x+1,y) then q.Enqueue(x+1,y,dist+1)
                    if can_walk(x-1,y) then q.Enqueue(x-1,y,dist+1)
                    if can_walk(x,y+1) then q.Enqueue(x,y+1,dist+1)
                    if can_walk(x,y-1) then q.Enqueue(x,y-1,dist+1)
        distance
  
    let BURIED_COLOR = System.Drawing.Color.Orange
    if buried_dx <> -999 then  
        bmp2.SetPixel(EDGE+tx+buried_dx, EDGE+ty+buried_dy, BURIED_COLOR)

    // gridlines
    let darken(c:System.Drawing.Color) = 
        let F(b:byte) = int b * 7 / 8
        System.Drawing.Color.FromArgb(F c.R, F c.G, F c.B)
    for i = 0 to 8 do
        for y = EDGE+0 to EDGE+119 do
            let x = EDGE+i*15
            bmp1.SetPixel(x, y, darken(bmp1.GetPixel(x,y)))
            if pixelsWhereLabelsHaveModified.Contains(x,y) then
                () // do nothing, dont re-color labels
            else
                bmp2.SetPixel(x, y, darken(bmp2.GetPixel(x,y)))
    for x = EDGE+0 to EDGE+119 do
        for j = 0 to 8 do
            let y = EDGE+j*15
            bmp1.SetPixel(x, y, darken(bmp1.GetPixel(x,y)))
            bmp2.SetPixel(x, y, darken(bmp2.GetPixel(x,y)))
(*
    // redden charlock-enemy zones, bluen-grid metal-slime zones 
    let redden(c:System.Drawing.Color) = 
        let F(b:byte) = int b * 29 / 32
        System.Drawing.Color.FromArgb(int c.R, F c.G, F c.B)
    let bluen(c:System.Drawing.Color) = 
        let F(b:byte) = int b * 29 / 32
        System.Drawing.Color.FromArgb(F c.R, F c.G, int c.B)
    for j = 0 to 7 do
        for i = 0 to 7 do
            for y = EDGE+15*j to EDGE+15*j+14 do
                for x = EDGE+15*i to EDGE+15*i+14 do
                    if pixelsWhereLabelsHaveModified.Contains(x,y) then
                        () // do nothing, dont re-color labels
                    else
                        if true then // false to switch to ad-hoc
                            if x%2=0 || y%2 = 0 then
                                for r = 1 to zone_has_charlock_enemy.[ int ow_zones.[i,j] ] do
                                    bmp2.SetPixel(x, y, redden(bmp2.GetPixel(x,y)))
                            if (x+y)%2=0 then
                                for r = 1 to zone_has_metal_slime.[ int ow_zones.[i,j] ] do
                                    bmp2.SetPixel(x, y, bluen(bmp2.GetPixel(x,y)))
                        else
                            if x%2=0 || y%2 = 0 then
                                for r = 1 to zone_has_x.[ int ow_zones.[i,j] ] do
                                    bmp2.SetPixel(x, y, redden(bmp2.GetPixel(x,y)))
                            if (x+y)%2=0 then
                                for r = 1 to zone_has_y.[ int ow_zones.[i,j] ] do
                                    bmp2.SetPixel(x, y, bluen(bmp2.GetPixel(x,y)))
*)
    let zone_charlock_count = Array2D.zeroCreate 8 8
    let zone_metal_count = Array2D.zeroCreate 8 8
    for j = 0 to 7 do
        for i = 0 to 7 do
            zone_charlock_count.[i,j] <- zone_has_charlock_enemy.[ int ow_zones.[i,j] ]
            zone_metal_count.[i,j] <- zone_has_metal_slime.[ int ow_zones.[i,j] ]

    // sleep/stopspell resists, sleep is high nibble
    for x in [0..39] do
        let golem = monster_data.[16*x..16*x+15]
        printfn "%16s: str %3d agi %3d hp %3d spells %2x s_ss_resist %2x dodge_mag_phys %2x xp %3d gold %3d" (let n,_,_,_,_,_=EnemyData.ENEMY_DATA.[x] in n) golem.[0] golem.[1] golem.[2] golem.[3] golem.[4] golem.[5] golem.[6] golem.[7]
        match spell(golem.[3]) with
        | "" -> ()
        | s -> 
            printfn "                                              %s" s
            if x = hauks_enemy then
                hauks_info <- hauks_info + "          " + s
            if x = swamp_enemy then
                swamp_info <- swamp_info + "          " + s
            if x = charlock_enemy then
                charlock_info <- charlock_info + "          " + s

    let str_hp_go_level,lhe,lhu,lHE,lHU,rL1 = show_go_mode_stats(bytes, true, file)
    let first_heal_level = min lhe lHE
    let start_with_keys = rL1 || (num_keys_throne_room>1) || wings_in_throne_room
    printfn ""
    if start_with_keys then
        printfn "key start: %s%s%s" (if rL1 then "RETURN   " else "") (if num_keys_throne_room>1 then "extra key   " else "") (if wings_in_throne_room then "WINGS" else "")
    printfn "starting items available: %A %A" (throne_room_items.ToArray()) (if start_with_keys then tantagel_treasury_items.ToArray() else [||])
    printfn "healing at level  %2d" first_heal_level
    printfn "hurtmore at level %2d" lHU
    printfn "first GO at level %2d" str_hp_go_level 
    printfn "SPIKE TILES"
    printfn "%s" hauks_info
    printfn "%s" swamp_info
    printfn "%s" charlock_info
    bmp1, bmp2, reachable_continents, mapCoords, cont_1_size, cont_2_size, compute_charlock_distance_to_inn(), ow_zones, zone_enemies, uniqueItemLocations, zone_charlock_count, zone_metal_count
