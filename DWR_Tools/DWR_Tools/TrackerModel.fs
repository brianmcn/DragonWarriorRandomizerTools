module TrackerModel

// abstraction for a set of scrollable choices
type ChoiceDomain(name:string,maxUsesArray:int[]) =
    // index keys are integers used as identifiers, values are max number of times it can appear among a set of cells
    let name = name
    let maxUsesArray = Array.copy maxUsesArray
    let uses = Array.zeroCreate maxUsesArray.Length
    let ev = new Event<_>()
    [<CLIEvent>]
    member _this.Changed = ev.Publish
    member _this.Name = name  // just useful for debugging etc
    member _this.MaxKey = uses.Length-1
    member this.RemoveUse(key) =
        if uses.[key] > 0 then
            uses.[key] <- uses.[key] - 1
            ev.Trigger(this,key)
        else
            failwith "choice domain underflow"
    member this.AddUse(key) =
        if uses.[key] >= maxUsesArray.[key] then
            failwith "choice domain overflow"
        else
            uses.[key] <- uses.[key] + 1
            ev.Trigger(this,key)
    member this.NextFreeKey(key) =
        if key = uses.Length-1 then
            -1
        elif uses.[key+1] < maxUsesArray.[key+1] then
            key+1
        else
            this.NextFreeKey(key+1)
    member this.PrevFreeKey(key) =
        if key = -1 then
            this.PrevFreeKey(uses.Length)
        elif key = 0 then
            -1
        elif uses.[key-1] < maxUsesArray.[key-1] then
            key-1
        else
            this.PrevFreeKey(key-1)
    member _this.NumUses(key) = uses.[key]
    member _this.MaxUses(key) = maxUsesArray.[key]
        
type Cell(cd:ChoiceDomain) =
    // a location that can hold one item, e.g. a chest or search spot
    // this is about player-knowing-location-contents, not about players _having_ the things there
    let mutable state = -1 // -1 means empty, 0-N are item identifiers
    member _this.Current() = state
    member this.Next() =
        if state <> -1 then
            cd.RemoveUse(state)
        state <- cd.NextFreeKey(state)
        if state <> -1 then
            cd.AddUse(state)
    member this.Prev() =
        if state <> -1 then
            cd.RemoveUse(state)
        state <- cd.PrevFreeKey(state)
        if state <> -1 then
            cd.AddUse(state)
    member this.TrySet(newState) =
        if newState < -1 || newState > cd.MaxKey then
            failwith "TrySet out of range"
        try
            cd.AddUse(newState)
            state <- newState
        with _ -> 
            printfn "TrySet failed, ignoring..."

//////////////////////////////////////////////////////////////////////////////////////////

module ITEMS =
    let STONES = 0
    let HARP = 1
    let STAFF = 2
    let TOKEN = 3
    let RAINBOW_DROP = 4
    let SWORD = 5
    let ARMOR = 6
    let NECKLACE = 7
    let FLUTE = 8
    let RING = 9
    let DRAGON_SCALE = 10
    let WINGS = 11
    let FAIRY_WATER = 12
    let CURSED_BELT = 13
    let TORCH = 14
    let KEY = 15
    let HERB = 16 
    let GOLD = 17
    // TODO princess' love icon?
    let IsBuryable(item) =
        match item with
        | 0 | 1 | 3 | 5 | 6 | 7 | 8 -> true
        | _ -> false
    let init() =
        let imageStream = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream("dwr_items.png")
        let bmp = new System.Drawing.Bitmap(imageStream)
        [|  for i = 0 to bmp.Width/7 - 1 do
                let r = new System.Drawing.Bitmap(7*3,7*3)
                for px = 0 to 7*3-1 do
                    for py = 0 to 7*3-1 do
                        r.SetPixel(px, py, bmp.GetPixel(px/3 + i*7, py/3))
                yield r
        |]
    let Bitmaps = init()

let allTrackedChestItemsChoiceDomain = ChoiceDomain("allTrackedChestItems", [|
    1
    1
    1 // staff will be used once at startup
    1
    1 // rainbow drop will be used once at startup
    1
    1
    1
    1
    1
    1
    2
    2
    3 // could be fewer than 3 belts
    3
    4 // includes key in throne room
    4
    6
    |])

type Box() as this =
    // this contains both a Cell (player-knowing-location-contents), and a bool (whether the players _has_ the thing there)
    let cell = new Cell(allTrackedChestItemsChoiceDomain)
    let mutable playerHas = true  // usually only find out what when getting, so assume have, player can mark un-had in rare circumstances they know but don't have
    static let allBoxes = ResizeArray<Box>()
    static let recomputeKeyItems() =
        let haveKeyItem = Array.zeroCreate 10
        for i = 0 to 9 do
            for b in allBoxes do
                if b.PlayerHas() && b.CellCurrent()=i then
                    haveKeyItem.[i] <- true
        haveKeyItem
    do
        allBoxes.Add(this)
    member _this.PlayerHas() = playerHas
    member _this.CellPrev() = cell.Prev()
    member _this.CellNext() = cell.Next()
    member _this.CellCurrent() = cell.Current()
    member _this.CellTrySet(i) = cell.TrySet(i)
    member _this.TogglePlayerHas() = playerHas <- not playerHas
    static member KeyItemsHeld() = recomputeKeyItems()

open System.Windows.Controls
open System.Windows.Media

let BMPtoImage(bmp:System.Drawing.Bitmap) =
    let ms = new System.IO.MemoryStream()
    bmp.Save(ms, System.Drawing.Imaging.ImageFormat.Png)  // must be png (not bmp) to save transparency info
    let bmimage = new System.Windows.Media.Imaging.BitmapImage()
    bmimage.BeginInit()
    ms.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore
    bmimage.StreamSource <- ms
    bmimage.EndInit()
    let i = new Image()
    i.Source <- bmimage
    i.Height <- float bmp.Height 
    i.Width <- float bmp.Width 
    i

let greyInvert(bmp:System.Drawing.Bitmap) =
    let r = new System.Drawing.Bitmap(7*3,7*3)
    for px = 0 to 7*3-1 do
        for py = 0 to 7*3-1 do
            let c = bmp.GetPixel(px,py)
            if c.ToArgb() <> System.Drawing.Color.Black.ToArgb() then
                r.SetPixel(px, py, System.Drawing.Color.DarkSlateBlue)
            else
                r.SetPixel(px, py, System.Drawing.Color.Black)
    r

let canvasAdd(c:Canvas, item, left, top) =
    if item <> null then
        c.Children.Add(item) |> ignore
        Canvas.SetTop(item, top)
        Canvas.SetLeft(item, left)

let no = System.Windows.Media.Brushes.DarkRed
let yes = System.Windows.Media.Brushes.Gray
let yesBuryable = System.Windows.Media.Brushes.Lime

let AnythingChanged = new Event<_>()
let boxItemImpl(box:Box,isScrollable) = 
    let c = new Canvas(Width=30., Height=30., Background=Brushes.Black)
    let rect = new System.Windows.Shapes.Rectangle(Width=30., Height=30., StrokeThickness=3.0)
    let color() =
        if box.PlayerHas() then
            if ITEMS.IsBuryable(box.CellCurrent()) then
                rect.Stroke <- yesBuryable
            else
                rect.Stroke <- yes
        else
            rect.Stroke <- no
    color()
    c.Children.Add(rect) |> ignore
    let innerc = new Canvas(Width=30., Height=30., Background=Brushes.Transparent)  // just has item drawn on it, not the box
    c.Children.Add(innerc) |> ignore
    let draw() =
        innerc.Children.Clear()
        let i = box.CellCurrent()
        if i <> -1 then 
            let bmp = ITEMS.Bitmaps.[i]
            canvasAdd(innerc, BMPtoImage bmp, 5., 5.)
    c.MouseLeftButtonDown.Add(fun _ ->
        box.TogglePlayerHas()
        color()
        AnythingChanged.Trigger()
    )
    if isScrollable then
        // item
        c.MouseWheel.Add(fun x -> 
            if x.Delta<0 then
                box.CellNext()
            else
                box.CellPrev()
            draw()
            color()
            AnythingChanged.Trigger()
        )
    draw()
    // TODO timelineItems.Add(new TimelineItem(innerc, (fun()->obj.Equals(rect.Stroke,yes))))
    c

//////////////////////////////////////////////////////////////////////////////////////////

let mkBox() = boxItemImpl(Box(),true)
let mkBoxes(n) = if n=0 then [||] else Array.init n (fun _ -> mkBox())
let mkThroneBoxes() = Array.init 3 (fun i -> if i=2 then (let b = Box() in b.CellPrev(); b.CellPrev(); b.CellPrev(); boxItemImpl(b,true)) else mkBox())
let mkSpike() =
    let cb = new ComboBox(IsEditable=false,IsReadOnly=true,Width=100.)
    cb.ItemsSource <- [|
            ""
            "Axe"
            "AK47"
            "Stoneman"
            "Golem"
            "Blue Dragon"
            "Red Dragon"
        |]
    cb.SelectedIndex <- 0
    cb
let redBox() = [|let b = Box() in b.TogglePlayerHas(); boxItemImpl(b,true)|]
let mkPL() = 
    let tb = new TextBox(Text="GPS",FontSize=16.0,BorderThickness=System.Windows.Thickness(0.0),IsReadOnly=true,Foreground=Brushes.Orange,Background=Brushes.Black)
    let cb = new CheckBox(Content=tb)
    cb.VerticalContentAlignment <- System.Windows.VerticalAlignment.Center
    cb

let LOCATIONS = [|
    "Throne room", ""                     ,true,  mkThroneBoxes(),  ([||]:System.Windows.UIElement[]),           (fun () -> ())              
    "Tantagel", ""                        ,true,  mkBoxes(4),       [||],                                        (fun () -> ())              
    "Charlock", "DW_Charlock.png"         ,false, mkBoxes(7),       [||],                                        (fun () -> ())
    "Brecconary (Motel 6)", ""            ,false, mkBoxes(0),       [||],                                        (fun () -> ())
    "Rimuldar (keys)", ""                 ,false, mkBoxes(1),       [||],                                        (fun () -> ())
    "Cantlin (coords)", ""                ,false, redBox(),         [|TextBox(Width=100.,FontSize=16.)|],        (fun () -> ())
    "Kol (fountain)", ""                  ,false, redBox(),         [||],                                        (fun () -> ())
    "Hauksness (dead)", ""                ,false, redBox(),         [|mkSpike()|],                               (fun () -> ())
    "Garinham", ""                        ,false, mkBoxes(3),       [||],                                        (fun () -> ())
    "Sun Stones Cave (v)", ""             ,false, mkBoxes(1),       [||],                                        (fun () -> ())
    "Staff Rain Cave (>)", ""             ,false, [|let b = Box() in b.CellTrySet(2); b.TogglePlayerHas(); boxItemImpl(b,false)|], [||], (fun () -> ()) // TODO logic: if ItemCheckboxes.[1].IsChecked.Value && not ItemCheckboxes.[2].IsChecked.Value then async { voice.Speak("Turn in the silver harp") } |> Async.Start)
    "Jerk Cave (<)", ""                   ,false, [|let b = Box() in b.CellTrySet(4); b.TogglePlayerHas(); boxItemImpl(b,false)|], [||], (fun () -> ()) // TODO logic:  if ItemCheckboxes.[0].IsChecked.Value && ItemCheckboxes.[2].IsChecked.Value && ItemCheckboxes.[3].IsChecked.Value && not ItemCheckboxes.[4].IsChecked.Value then async { voice.Speak("Get raindbow drop from jerk") } |> Async.Start)
    "Swamp Cave North", "DW_SwampCave.png",false, mkBoxes(0),       [|mkSpike()|],                               (fun () -> ())
    "Swamp Cave South", "DW_SwampCave.png",false, mkBoxes(0),       [|mkPL()|],                                  (fun () -> ())
    "Mountain Cave", "DW_MountainCave.png",false, mkBoxes(5),       [||],                                        (fun () -> ())
    "Tablet Cave", "DW_TabletCave.png"    ,false, mkBoxes(1),       [||],                                        (fun () -> ())
    "Garin's Tomb", "DW_GarinTomb.png"    ,false, mkBoxes(5),       [||],                                        (fun () -> ())
    |]
