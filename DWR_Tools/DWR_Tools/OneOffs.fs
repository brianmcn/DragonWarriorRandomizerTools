module OneOffs

let rng = new System.Random()

let either() = 
    let r = rng.Next(256)
    if r < 32 then 5
    elif r < 80 then 2
    elif r < 173 then 3
    else 4

let hero() =
    let attempt = 
        if rng.Next(32)=0 then
            9 + rng.Next(10) // crit
        else
            either()
    if rng.Next(64) = 0 then
        0 // it is dodging
    else
        attempt

let log(s) =
   printfn "%s" s

let up2ngScorpionFight() =
    let N = 1000
    let heroMaxHP = 14
    let scorpionMaxHP = 13
    let attacksToKill = Array.zeroCreate N
    for i = 0 to N-1 do
        let mutable attacks = 0
        let mutable heroHP = 9 // up2ng enters fight with this
        let mutable scorpionHP = scorpionMaxHP
        let mutable heroHeals = 11 // 35 mp
        while scorpionHP > 0 && heroHP > 0 do
            let mutable s = ""
            if heroHP < 6 && heroHeals > 0 then
                heroHP <- heroMaxHP // heal
                heroHeals <- heroHeals - 1
                s <- s + "hero heals,   "
            else
                scorpionHP <- scorpionHP - hero() // attack
                s <- s + "hero attacks, "
            if scorpionHP > 0 then
                if scorpionHP < 4 && rng.Next(4)<>0 then
                    scorpionHP <- scorpionMaxHP // healmore
                    s <- s + "scorpion heals   "
                else
                    heroHP <- heroHP - either()
                    s <- s + "scorpion attacks "
            attacks <- attacks + 1
            s <- s + sprintf " now hero/scorpion HP are %2d/%2d" heroHP scorpionHP 
            log(s)
        log("-----END OF FIGHT-----")
        if heroHP <= 0 then
            attacksToKill.[i] <- -999
        else
            attacksToKill.[i] <- attacks
    let a = Array.sort attacksToKill
    let wins = a |> Seq.filter (fun x -> x <> -999) |> Seq.length 
    printfn "win percentage = %f" (100.0 * float wins / float N)

