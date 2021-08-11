open System
open System.Windows
open System.Windows.Controls 
open System.Windows.Media
open System.Windows.Interop 

// TODO re-add shop-tracking UI, add fullplate track
// TODO tabs or somewhere to display dungeon maps
// TODO tab for full DL2 simulator
// TODO timeline
// TODO possible xp graph over time

// TODO add AP/DP/STR/AGI tracker (when that screen pops up?) also note weapon/armor/etc
//   - could capture the level when it was taken, and always know current level, so could show how 'stale' the info was that way

//////////////////////////////////////////////////

open System.Runtime.InteropServices 
module Winterop = 
    [<DllImport("User32.dll")>]
    extern bool RegisterHotKey(IntPtr hWnd,int id,uint32 fsModifiers,uint32 vk)

    [<DllImport("User32.dll")>]
    extern bool UnregisterHotKey(IntPtr hWnd,int id)

    let HOTKEY_ID = 9000

    [<DllImport("user32.dll")>]
    extern IntPtr GetForegroundWindow()

    [<DllImport("user32.dll")>]
    extern int SendMessage(IntPtr hWnd, int wMsg, IntPtr wParam, IntPtr lParam)

    [<DllImport("user32.dll")>]
    extern [<MarshalAs(UnmanagedType.Bool)>] bool PostMessage(IntPtr hWnd, int wMsg, IntPtr wParam, IntPtr lParam)

    [<DllImport ("User32.dll")>]
    extern int SetForegroundWindow(IntPtr hWnd)

    [<StructLayout(LayoutKind.Sequential)>]
    [<Struct>]
    type RECT =
         val Left : int
         val Top : int
         val Right : int
         val Bottom : int

    [<DllImport("user32.dll", SetLastError = true)>]
    extern [<MarshalAs(UnmanagedType.Bool)>] bool GetWindowRect(IntPtr hWnd, RECT* lpRect)

//////////////////////////////////////////////////

type FastRGBBitmap(orig:System.Drawing.Bitmap) =
    let imageData = orig.LockBits(new System.Drawing.Rectangle(0, 0, orig.Width, orig.Height), System.Drawing.Imaging.ImageLockMode.ReadWrite, System.Drawing.Imaging.PixelFormat.Format24bppRgb)
    let bytes = Array.create (imageData.Stride * orig.Height) 0uy
    do
        System.Runtime.InteropServices.Marshal.Copy(imageData.Scan0, bytes, 0, bytes.Length)
    member private this.Bytes = bytes
    member private this.ImageData = imageData
    member this.GetR(x,y) = bytes.[imageData.Stride*y + x*3 + 2]
    member this.GetG(x,y) = bytes.[imageData.Stride*y + x*3 + 1]
    member this.GetB(x,y) = bytes.[imageData.Stride*y + x*3 + 0]
    member this.SetRGB(x,y,r,g,b) = 
        bytes.[imageData.Stride*y + x*3 + 0] <- b
        bytes.[imageData.Stride*y + x*3 + 1] <- g
        bytes.[imageData.Stride*y + x*3 + 2] <- r
    member this.Equals(x,y,argb) = 
        let r = byte((argb &&& 0x00FF0000) >>> 16)
        let g = byte((argb &&& 0x0000FF00) >>> 8)
        let b = byte((argb &&& 0x000000FF))
        let r1 = bytes.[imageData.Stride*y + x*3 + 2]
        let g1 = bytes.[imageData.Stride*y + x*3 + 1]
        let b1 = bytes.[imageData.Stride*y + x*3 + 0]
        //if r <> 0uy then
        //    ignore(r,g,b,r1,g1,b1)
        r=r1 && g=g1 && b=b1
    member this.Equals(x,y,other:FastRGBBitmap,ox,oy) = 
        let r1 = bytes.[imageData.Stride*y + x*3 + 2]
        let g1 = bytes.[imageData.Stride*y + x*3 + 1]
        let b1 = bytes.[imageData.Stride*y + x*3 + 0]
        let r = other.Bytes.[other.ImageData.Stride*oy + ox*3 + 2]
        let g = other.Bytes.[other.ImageData.Stride*oy + ox*3 + 1]
        let b = other.Bytes.[other.ImageData.Stride*oy + ox*3 + 0]
        //if r <> 0uy then
        //    ignore(r,g,b,r1,g1,b1)
        r=r1 && g=g1 && b=b1
    member this.Finish() = 
        System.Runtime.InteropServices.Marshal.Copy(bytes, 0, imageData.Scan0, bytes.Length)
        orig.UnlockBits(imageData)

//////////////////////////////////////////////////

let UNKNOWN = System.Drawing.Color.Magenta 

module Screenshot =
    let RedToWhiteColor(c:System.Drawing.Color) =
        if c.R = byte 0xFC && c.G = byte 0x74 && c.B = byte 0x60 then // red
            System.Drawing.Color.FromArgb(0xFC, 0xFC, 0xFC)   // white
        else
            c
    let RedToWhiteBmp(bmp:System.Drawing.Bitmap) =
        // Dragon Warrior paints things white, except when your health is low, the white all turns to red.
        // Turn all those red to white to be able to do exact pixel matching regardless of current health.
        let clone = bmp.Clone() :?> System.Drawing.Bitmap
        let fast = FastRGBBitmap(clone)
        for x = 0 to clone.Width-1 do
            for y = 0 to clone.Height-1 do
                if fast.GetR(x,y) = byte 0xFC && fast.GetG(x,y) = byte 0x74 && fast.GetB(x,y) = byte 0x60 then // red
                    fast.SetRGB(x, y, 0xFCuy, 0xFCuy, 0xFCuy)   // white
        fast.Finish()
        clone
    let mutable badTileNum = 1
    let UniqueOverworldTiles = ResizeArray<string*Constants.OverworldMapTile*FastRGBBitmap>()
    let BMPtoImage(bmp:System.Drawing.Bitmap) =
        let ms = new System.IO.MemoryStream()
        bmp.Save(ms, System.Drawing.Imaging.ImageFormat.Bmp)
        let bmimage = new System.Windows.Media.Imaging.BitmapImage()
        bmimage.BeginInit()
        ms.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore
        bmimage.StreamSource <- ms
        bmimage.EndInit()
        bmimage
    let GetDWRBitmap() =
        let bmpScreenshot = new System.Drawing.Bitmap(System.Windows.Forms.Screen.PrimaryScreen.Bounds.Width, System.Windows.Forms.Screen.PrimaryScreen.Bounds.Height, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
        let gfxScreenshot = System.Drawing.Graphics.FromImage(bmpScreenshot)
        try
            let procs = System.Diagnostics.Process.GetProcesses()
            let fceux = procs |> Seq.find (fun p -> p.MainWindowTitle.ToLower().StartsWith("fceux"))
            let fceux_hWnd = fceux.MainWindowHandle 
            let mutable rect = Winterop.RECT()
            let b = Winterop.GetWindowRect(fceux_hWnd, &&rect)
            gfxScreenshot.CopyFromScreen(rect.Left, rect.Top, 0, 0,
                                         System.Drawing.Size(rect.Right-rect.Left+1,rect.Bottom-rect.Top+1),
                                         System.Drawing.CopyPixelOperation.SourceCopy)
        with _e -> 
            ()  // if fceux process not there, fail silently
        bmpScreenshot

let NUM_ANIMATION_FRAMES = 2

let animateColors(bmp:System.Drawing.Bitmap, colors:System.Drawing.Color[]) =
    let colors = colors |> Array.map (fun c -> c, c.ToArgb())
    let F = NUM_ANIMATION_FRAMES
    let scale(i, b) = byte(int b*(F-i-1)/F)
    let frames = Array.init F (fun _ -> bmp.Clone() :?> System.Drawing.Bitmap)
    let fastFrames = Array.init F (fun i -> new FastRGBBitmap(frames.[i]) )
    for x = 0 to frames.[0].Width-1 do
        for y = 0 to frames.[0].Height-1 do
            for c, argb in colors do
                if fastFrames.[0].Equals(x,y,argb) then
                    for i = 1 to F-1 do
                        fastFrames.[i].SetRGB(x, y, scale(i,c.R), scale(i,c.G), scale(i,c.B))
    fastFrames |> Array.iter (fun ff -> ff.Finish())
    frames

let gridAdd(g:Grid, x, c, r) =
    g.Children.Add(x) |> ignore
    Grid.SetColumn(x, c)
    Grid.SetRow(x, r)

type MyWindow(ihrs,imins,isecs,racingMode,leagueMode,xp_thresholds) as this = 
    inherit Window()
    let mutable src_cb : CheckBox = null  // staff of rain checkbox
    let mutable jerk_cb : CheckBox = null  // jerk checkbox
    let mutable startTime = DateTime.Now + TimeSpan.FromSeconds(0.0)
    let mutable mostRecentDeathTime = DateTime.Now 
    let mutable mostRecentStatsTime = DateTime.Now 
    let mutable numDeaths = 0
    let mutable changed = false
    let mutable heroAG = 0
    let mutable heroMaxHP = 0
    let mutable heroMaxMP = 0
    let mutable heroAP = 0
    let mutable heroDP = 0
    let mutable caveMapIsCurrentlyDisplayed = false
    let monsterImage = new Image()
    let tab = new TabControl(Background=Brushes.Black)
    let mutable ssNum = 1
    let mutable heroExp = 0
    let mutable heroLevel = 1
    let heroLevelTimes = Array.create 30 null
    let heroSpells = Array.create 10 false
    let mutable heroWeaponIndex = -1
    let mutable heroArmorIndex = -1 
    let mutable heroShieldIndex = -1 
    let weaponTextBox = new RichTextBox(FontSize=16.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0),Focusable=false)
    let armorTextBox  = new RichTextBox(FontSize=16.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0),Focusable=false)
    let shieldTextBox = new RichTextBox(FontSize=16.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0),Focusable=false)
    let statsTextBox = new TextBox(TextWrapping=TextWrapping.Wrap,FontSize=16.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(1.0),Focusable=false)
    let makeAnimationBrush() =
        let colorAnimation = new System.Windows.Media.Animation.ColorAnimation()
        colorAnimation.From <- Nullable<_>(System.Windows.Media.Colors.Black)
        colorAnimation.To <- Nullable<_>(System.Windows.Media.Color.FromRgb(0x6Fuy, 0x6Fuy, 0x00uy))
        colorAnimation.Duration <- new Duration(TimeSpan.FromSeconds(1.5))
        colorAnimation.AutoReverse <- true
        let brush = new SolidColorBrush(Colors.Black)
        brush, (fun () -> brush.BeginAnimation(SolidColorBrush.ColorProperty, colorAnimation))
    let appendRichTextWithBackground(box:RichTextBox, text, color, bgcolor) = 
        let range = new System.Windows.Documents.TextRange(box.Document.ContentEnd, box.Document.ContentEnd)
        range.Text <- text
        range.ApplyPropertyValue(System.Windows.Documents.TextElement.ForegroundProperty, color)
        range.ApplyPropertyValue(System.Windows.Documents.TextElement.FontWeightProperty, FontWeights.Bold)
        range.ApplyPropertyValue(System.Windows.Documents.TextElement.BackgroundProperty, bgcolor)
    let appendRichText(box:RichTextBox, text, color) = 
        let range = new System.Windows.Documents.TextRange(box.Document.ContentEnd, box.Document.ContentEnd)
        range.Text <- text
        range.ApplyPropertyValue(System.Windows.Documents.TextElement.ForegroundProperty, color)
        range.ApplyPropertyValue(System.Windows.Documents.TextElement.FontWeightProperty, FontWeights.Bold)
    let appendRichTextAnimate(box:RichTextBox, text, color) = 
        appendRichText(box, text, color)
        let brush, thunk = makeAnimationBrush()
        box.Background <- brush
        thunk()
    let updateWeapon() =
        heroWeaponIndex <- (heroWeaponIndex + 1) % Constants.WEAPONS.Length
        weaponTextBox.Document.Blocks.Clear()
        appendRichText(weaponTextBox, "weapon ", Brushes.Orange)
        appendRichTextAnimate(weaponTextBox, sprintf "%s" Constants.WEAPONS.[heroWeaponIndex], Brushes.White)
    let updateArmor() =
        heroArmorIndex <- (heroArmorIndex + 1) % Constants.ARMOR.Length
        armorTextBox.Document.Blocks.Clear()
        appendRichText(armorTextBox, " armor ", Brushes.Orange)
        appendRichTextAnimate(armorTextBox, sprintf "%s" Constants.ARMOR.[heroArmorIndex], Brushes.White)
    let updateShield() =
        heroShieldIndex <- (heroShieldIndex + 1) % Constants.SHIELD.Length
        shieldTextBox.Document.Blocks.Clear()
        appendRichText(shieldTextBox, "shield ", Brushes.Orange)
        appendRichTextAnimate(shieldTextBox, sprintf "%s" Constants.SHIELD.[heroShieldIndex], Brushes.White)
    let onCheckedChanged(resource) =
        if resource <> "" then
            let imageStream = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
            //image1.Source <- System.Windows.Media.Imaging.BitmapFrame.Create(imageStream)
            caveMapIsCurrentlyDisplayed <- true
        else
            //image1.Source <- null
            caveMapIsCurrentlyDisplayed <- false
    let makeCheckedStuff(strResBoxesEffects,cba:CheckBox[]) = 
        let sp = new StackPanel(Background=Brushes.Black)
        let mutable index = 0
        for s,res,isChecked,boxes:_[],uies,effect in strResBoxesEffects do
            let tb = new TextBox(Text=s,FontSize=10.0,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0),IsReadOnly=true)
            tb.Margin <- Thickness(2.0) // space between rows
            let brush, thunk = makeAnimationBrush()
            tb.Background <- brush
            let cb = new CheckBox(Content=tb)
            cb.VerticalContentAlignment <- VerticalAlignment.Center
            cb.LayoutTransform <- new ScaleTransform(2.0, 2.0)
            cb.IsChecked <- System.Nullable.op_Implicit isChecked
            if isChecked then
                tb.Foreground <- Brushes.SlateBlue
            cba.[index] <- cb
            index <- index + 1
            if s = "Staff Rain Cave (>)" then 
                src_cb <- cb
            if s = "Jerk Cave (<)" then 
                jerk_cb <- cb
            cb.Checked.Add(fun _ -> 
                effect()
                if s = "Staff of Rain" then 
                    src_cb.IsChecked <- System.Nullable.op_Implicit true
                if s = "Rainbow Drop" then 
                    jerk_cb.IsChecked <- System.Nullable.op_Implicit true
(* TODO put this logic with changeDomain box tracking
                if res = "E_SWORD" then 
                    heroWeaponIndex <- Constants.WEAPONS.Length-2
                    updateWeapon()
                    let tb = cb.Content :?> TextBox
                    let ts = DateTime.Now - startTime
                    tb.Text <- tb.Text + sprintf " %02d:%02d:%02d" ts.Hours ts.Minutes ts.Seconds
                elif res = "E_ARMOR" then 
                    heroArmorIndex <- Constants.ARMOR.Length-2
                    updateArmor()
                    let tb = cb.Content :?> TextBox
                    let ts = DateTime.Now - startTime
                    tb.Text <- tb.Text + sprintf " %02d:%02d:%02d" ts.Hours ts.Minutes ts.Seconds
                else 
*)
                onCheckedChanged(res)
                tb.Foreground <- Brushes.SlateBlue 
                thunk()
                TrackerModel.AnythingChanged.Trigger())
            cb.Unchecked.Add(fun _ -> 
                onCheckedChanged("")
                tb.Foreground <- Brushes.Orange
                thunk()
                TrackerModel.AnythingChanged.Trigger())
            let hp = new StackPanel(Background=Brushes.Black,Orientation=Orientation.Horizontal)
            hp.Children.Add(cb) |> ignore
            let c = new Canvas(Height=30., Width=30.0*float(boxes.Length))
            for i = 0 to boxes.Length-1 do
                TrackerModel.canvasAdd(c,boxes.[i],float(30*i),0.)
            hp.Children.Add(c) |> ignore
            for uie in uies do
                hp.Children.Add(uie) |> ignore
            sp.Children.Add(hp) |> ignore
        sp
    let content = new Grid()
    let xpTextBox = new RichTextBox(FontSize=16.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0),Focusable=false)
    let hmsTimeTextBox = new TextBox(Text="timer",FontSize=36.0,Background=Brushes.Black,Foreground=Brushes.LightGreen,BorderThickness=Thickness(0.0))
    let monsterName = new TextBox(Text="name",FontSize=20.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0))
    let monsterXP = new TextBox(Text="EXP",FontSize=20.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0))
    let monsterGold = new TextBox(Text="GOLD",FontSize=20.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0))
    let monsterSTR = new TextBox(Text="STR",FontSize=20.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0))
    let monsterAGI = new TextBox(Text="AGI",FontSize=20.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0))
    let monsterHP = new TextBox(Text="HP",FontSize=20.0,FontFamily=System.Windows.Media.FontFamily("Courier New"),Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0))
    let day1Start = 60*24 + 8*60 + 15
    let mutable source = null
    let stackPanel = new StackPanel(Background=Brushes.Black)
    let update() =
        let timer = System.Diagnostics.Stopwatch.StartNew()
        // update time
        let ts = DateTime.Now - startTime
        let h,m,s = ts.Hours, ts.Minutes, ts.Seconds
        hmsTimeTextBox.Text <- sprintf "%02d:%02d:%02d    deaths:%2d" h m s numDeaths
        // update XP & spell info
        let bmpScreenshot = Screenshot.GetDWRBitmap()
        let gp(x,y) =
            let c = bmpScreenshot.GetPixel(x*3+8,y*3+51)  // TODO factor fceux window's 3x pixels and 8,51 offset somewhere better
            if c.ToArgb() = PixelLayout.RED.ToArgb() then // if hero low health, convert red to white
                PixelLayout.WHITE 
            else
                c
        if PixelLayout.identifySpells(gp,heroSpells) then
            changed <- true
        match PixelLayout.identifyEXP(gp) with   
        | Some exp -> 
            heroExp <- exp
            let newHeroLevel = (xp_thresholds |> Array.findIndex(fun z -> z > exp)) + 1
            if newHeroLevel <> heroLevel then
                heroLevel <- newHeroLevel
                changed <- true
                if newHeroLevel = 1 then  // can be 1 on a reset
                    // zero out all the auto-tracked progress (user can uncheck equipment/locations/etc if desired)
                    (Array.create 30 null).CopyTo(heroLevelTimes, 0)
                    (Array.create 10 false).CopyTo(heroSpells, 0)
                else
                    heroLevelTimes.[heroLevel-2] <- sprintf "%03d:%02d " (60*h+m) s
        | None -> ()
        match PixelLayout.identifyHP(gp) with   
        | Some 0 -> // HP = 0 - we are dying now
            if (DateTime.Now - mostRecentDeathTime) > TimeSpan.FromSeconds(10.0) then // ensure don't run this two frames in a row
                numDeaths <- numDeaths + 1
                mostRecentDeathTime <- DateTime.Now 
                changed <- true
        | _ -> ()
        match PixelLayout.getAGHPMPAPDP(gp) with   
        | Some(ag,hp,mp,ap,dp) ->
            if (DateTime.Now - mostRecentStatsTime) > TimeSpan.FromSeconds(10.0) then // ensure don't run this two frames in a row
                mostRecentStatsTime <- DateTime.Now
                heroAG <- ag
                heroMaxHP <- hp
                heroMaxMP <- mp
                heroAP <- ap
                heroDP <- dp
                let ts = DateTime.Now - startTime
                let h,m,s = ts.Hours, ts.Minutes, ts.Seconds
                let winsPerThousand = ROM.simulate_dl2(ag,hp-20,mp,hp,dp,ap)  // assumes survive dl1 with max-20
                let winsPerThousandDown1 = ROM.simulate_dl2(ag,hp-20,mp-8,hp,dp,ap)  // assumes survive dl1 with max-20
                let winsPerThousandDN = ROM.simulate_dl2(ag,hp-20,mp,hp,dp,ap+10)  // assumes survive dl1 with max-20
                let winsPerThousandDNDown1 = ROM.simulate_dl2(ag,hp-20,mp-8,hp,dp,ap+10)  // assumes survive dl1 with max-20
                let pad(n) = 
                    let s = sprintf "%3.2f%%" (float n / 10.0)
                    (String.replicate (7-s.Length) " ") + s
                statsTextBox.Text <- 
                    sprintf "Stats as of\n%02d:%02d:%02d\ninto the race\nAG:     %3d\nMax HP: %3d\nMax MP: %3d\nAP:     %3d\nDP:     %3d\n            full down1heal\nDL2 win%%: %s  %s\nplus DN%%: %s  %s" 
                        h m s ag hp mp ap dp (pad winsPerThousand) (pad winsPerThousandDown1) (pad winsPerThousandDN) (pad winsPerThousandDNDown1)
        | None -> ()
        // level xp/times/spells text area
        if changed then
            xpTextBox.Document.Blocks.Clear()
            for l = 0 to 17 do
                let bg = if l=3||l=8||l=13 then new SolidColorBrush(System.Windows.Media.Color.FromRgb(0x45uy, 0x45uy, 0x55uy)) else Brushes.Black // highlight levels 5,10,15
                if heroLevelTimes.[l] <> null then
                    appendRichTextWithBackground(xpTextBox, heroLevelTimes.[l], Brushes.White, bg)
                else
                    appendRichTextWithBackground(xpTextBox, sprintf "%-6d " xp_thresholds.[l], Brushes.Orange, bg)
            for i = 0 to 9 do
                appendRichText(xpTextBox, " " + PixelLayout.SPELL_NAMES.[i].Substring(0,6), if heroSpells.[i] then Brushes.White else Brushes.DarkSlateGray)
        if racingMode then
            // TODO for reasons I don't understand, not running this causes an unmanaged memory leak that makes the app crash in less than 2 minutes
            () //EnemyData.bestMatch(bmpScreenshot) |> ignore

    do
        Constants.voice.Volume <- 30
        let pretty = xp_thresholds |> Array.map (fun x -> let s = x.ToString() in (String.replicate (5-s.Length) " ") + s)
        appendRichText(xpTextBox, "XP to level ", Brushes.Orange)
        appendRichText(xpTextBox, String.Join(" ",pretty), Brushes.White)
        updateWeapon()
        updateArmor()
        updateShield()

        content.ColumnDefinitions.Add(new ColumnDefinition(Width=GridLength(76.0)))
        content.ColumnDefinitions.Add(new ColumnDefinition())
        content.RowDefinitions.Add(new RowDefinition())

        // left grid
        let leftGrid = new Grid()
        leftGrid.ColumnDefinitions.Add(new ColumnDefinition())
        leftGrid.RowDefinitions.Add(new RowDefinition(Height=GridLength(506.0)))
        leftGrid.RowDefinitions.Add(new RowDefinition())
        leftGrid.RowDefinitions.Add(new RowDefinition())
        leftGrid.RowDefinitions.Add(new RowDefinition())
        let makeKitty() =
            let kitty = new Image()
            let imageStream = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream("CroppedBrianKitty.png")
            kitty.Source <- System.Windows.Media.Imaging.BitmapFrame.Create(imageStream)
            kitty
        let kitty = makeKitty()// TODO add back kitty somewhere else
        gridAdd(leftGrid,xpTextBox,0,0)
        gridAdd(leftGrid,weaponTextBox,0,1)
        gridAdd(leftGrid,armorTextBox,0,2)
        gridAdd(leftGrid,shieldTextBox,0,3)
        weaponTextBox.MouseDown.Add(fun _ -> updateWeapon())
        armorTextBox.MouseDown.Add(fun _ -> updateArmor())
        shieldTextBox.MouseDown.Add(fun _ -> updateShield())
        gridAdd(content,leftGrid,0,0)
        // right panel
        let rightPanel = new StackPanel(Orientation=Orientation.Vertical, Background=Brushes.Black)
        rightPanel.Children.Add(hmsTimeTextBox) |> ignore

        let locationTextBox = makeCheckedStuff(TrackerModel.LOCATIONS,Constants.LocationCheckboxes)
        rightPanel.Children.Add(locationTextBox) |> ignore

        let unfounds = StackPanel(Orientation=Orientation.Horizontal, Background=Brushes.Black)
        unfounds.Children.Add(new TextBox(Text="Remaining:",FontSize=16.0,Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0),IsReadOnly=true)) |> ignore
        let draw(held,c:Canvas,i) =
            c.Children.Clear()
            if not(held) then
                TrackerModel.canvasAdd(c, TrackerModel.BMPtoImage TrackerModel.ITEMS.Bitmaps.[i], 5., 5.)
            else
                TrackerModel.canvasAdd(c, TrackerModel.BMPtoImage (TrackerModel.greyInvert TrackerModel.ITEMS.Bitmaps.[i]), 5., 5.)
        let kih = TrackerModel.Box.KeyItemsHeld()
        let unfoundCanvases = ResizeArray()
        for i = 0 to kih.Length-1 do
            let c = new Canvas(Width=30., Height=30.)
            unfoundCanvases.Add(c)
            draw(kih.[i],c,i)
            unfounds.Children.Add(c) |> ignore
        TrackerModel.AnythingChanged.Publish.Add(fun _ ->
            let kih = TrackerModel.Box.KeyItemsHeld()
            for i = 0 to kih.Length-1 do
                draw(kih.[i], unfoundCanvases.[i], i)
            )
        rightPanel.Children.Add(unfounds) |> ignore

        // add right panel
        gridAdd(content,rightPanel,1,0)

#if NOMORE
        // picture area
        if racingMode then
            let sp = new StackPanel(Background=Brushes.Black,Orientation=Orientation.Vertical)
            let c = Canvas()
            c.Children.Add(sp) |> ignore
            if not leagueMode then
                let kitty = makeKitty()
                sp.Children.Add(kitty) |> ignore
                TrackerModel.canvasAdd(c, TrackerModel.boxItemImpl(TrackerModel.Box()), 0., 0.)
                TrackerModel.canvasAdd(c, TrackerModel.boxItemImpl(TrackerModel.Box()), 30., 0.)
                TrackerModel.canvasAdd(c, TrackerModel.boxItemImpl(TrackerModel.Box()), 60., 0.)
                TrackerModel.canvasAdd(c, TrackerModel.boxItemImpl(TrackerModel.Box()), 90., 0.)
                //statsTextBox.Text<-"Stats not yet seen"
                //sp.Children.Add(statsTextBox) |> ignore
            else
                //sp.Children.Add(new TextBox(TextWrapping=TextWrapping.Wrap,Text="League mode, flags: CDGPRVWZks\nChanges:\n- no spell learning randomization\n- no keys\n- short Charlock\n- very fast XP\n\nSpells: Heal 3 / Hurt 4 / Sleep 7\nRadiant 9 / Stopspell 10 / Outside 12\nReturn 13 / Repel 15\nHealmore 17 / Hurtmore 19",FontSize=14.0,Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(8.0))) |> ignore
                sp.Children.Add(new TextBox(TextWrapping=TextWrapping.Wrap,Text="Super speed run\n- vanilla map\n- vanilla monsters\n- DWR monster xp & gold\n- very fast XP\n\nSpells: Heal 3 / Hurt 4 / Sleep 7\nRadiant 9 / Stopspell 10 / Outside 12\nReturn 13 / Repel 15\nHealmore 17 / Hurtmore 19",FontSize=14.0,Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(8.0))) |> ignore
            
            // no longer used to free up screen space:
            //sp.Children.Add(new TextBox(TextWrapping=TextWrapping.Wrap,Text="Monster data and maps are disabled during this stream, since this is a race!\n\nNo hints/advice/spoilers in chat during the race!",FontSize=16.0,Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(8.0))) |> ignore

            let xGrid = new Grid()
            xGrid.ColumnDefinitions.Add(new ColumnDefinition(Width=GridLength(156.0)))
            xGrid.ColumnDefinitions.Add(new ColumnDefinition())
            xGrid.RowDefinitions.Add(new RowDefinition())

            let itemTextBox = makeCheckedStuff("ITEMS",Constants.ITEMS,Constants.ItemCheckboxes)
            gridAdd(xGrid,itemTextBox,0,0)
            let dl_chart = new TextBox(Text=ROM.dl2_swings,FontSize=15.0,Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0))
            gridAdd(xGrid,dl_chart,1,0)
            
            sp.Children.Add(xGrid) |> ignore
            
            let shops = new Grid()
            shops.ColumnDefinitions.Add(new ColumnDefinition(Width=GridLength(76.0)))
            shops.ColumnDefinitions.Add(new ColumnDefinition())
            shops.ColumnDefinitions.Add(new ColumnDefinition())
            shops.ColumnDefinitions.Add(new ColumnDefinition())
            shops.ColumnDefinitions.Add(new ColumnDefinition(Width=GridLength(14.0)))
            shops.ColumnDefinitions.Add(new ColumnDefinition())
            shops.ColumnDefinitions.Add(new ColumnDefinition(Width=GridLength(14.0)))
            shops.ColumnDefinitions.Add(new ColumnDefinition())
            shops.ColumnDefinitions.Add(new ColumnDefinition())
            shops.ColumnDefinitions.Add(new ColumnDefinition())
            shops.RowDefinitions.Add(new RowDefinition())
            shops.RowDefinitions.Add(new RowDefinition(Height=GridLength(2.0)))
            shops.RowDefinitions.Add(new RowDefinition())
            shops.RowDefinitions.Add(new RowDefinition(Height=GridLength(2.0)))
            shops.RowDefinitions.Add(new RowDefinition())
            shops.RowDefinitions.Add(new RowDefinition(Height=GridLength(2.0)))
            shops.RowDefinitions.Add(new RowDefinition())
            shops.RowDefinitions.Add(new RowDefinition(Height=GridLength(2.0)))
            shops.RowDefinitions.Add(new RowDefinition())
            shops.RowDefinitions.Add(new RowDefinition(Height=GridLength(2.0)))
            shops.RowDefinitions.Add(new RowDefinition())
            //shops.ShowGridLines <- true
            let makeText(txt) = new TextBox(Text=txt,FontSize=14.0,Background=Brushes.Black,Foreground=Brushes.Orange,BorderThickness=Thickness(0.0))
            gridAdd(shops,makeText("HA"),1,0)
            gridAdd(shops,makeText("BS"),2,0)
            gridAdd(shops,makeText("FS"),3,0)
            gridAdd(shops,makeText("MA"),5,0)
            gridAdd(shops,makeText("LS"),7,0)
            gridAdd(shops,makeText("SS"),8,0)
            gridAdd(shops,makeText("Rimuldar"),0,2)
            gridAdd(shops,makeText("Cantlin"),0,4)
            gridAdd(shops,makeText("Kol"),0,6)
            gridAdd(shops,makeText("Garinham"),0,8)
            gridAdd(shops,makeText("Brecconary"),0,10)
            for i = 0 to 8 do
                for j = 1 to 10 do
                    if j%2=0 then
                        if i <> 0 && i <> 4 && i <> 6 then
                            gridAdd(shops,new CheckBox(),i,j)
                    else
                        gridAdd(shops,new StackPanel(Background=Brushes.Orange),i,j)

            let shopPanel = new StackPanel()
            shopPanel.Children.Add(makeText("SHOPS")) |> ignore
            shopPanel.Children.Add(shops) |> ignore
            sp.Children.Add(shopPanel) |> ignore
            let cb = new CheckBox(Content=makeText("Audio reminders"))
            cb.IsChecked <- System.Nullable.op_Implicit true
            Constants.voice.Volume <- 30
            cb.Checked.Add(fun _ -> Constants.voice.Volume <- 30)
            cb.Unchecked.Add(fun _ -> Constants.voice.Volume <- 0)
            sp.Children.Add(cb) |> ignore
            gridAdd(content,c,2,0)
#endif
        // full window
        this.Title <- "Dragon Warrior Randomizer"
        this.Content <- content
        //this.Width <- 1280.0 - 720.0
        //this.Height <- 720.0
        //this.SizeToContent <- SizeToContent.Height
        this.SizeToContent <- SizeToContent.WidthAndHeight
        this.WindowStartupLocation <- WindowStartupLocation.Manual
        this.Left <- 1450.0
        this.Top <- 0.0

        System.Windows.Application.Current.DispatcherUnhandledException.Add(fun e -> 
            let ex = e.Exception
            printfn "An unhandled exception from UI thread:"
            printfn "%s" (ex.ToString())
            printfn "press Enter to end"
            System.Console.ReadLine() |> ignore
            )
        System.AppDomain.CurrentDomain.UnhandledException.Add(fun e -> 
            let ex = e.ExceptionObject
            printfn "An unhandled exception from background thread:"
            printfn "%s" (ex.ToString())
            printfn "press Enter to end"
            System.Console.ReadLine() |> ignore
            )

        let timer = new System.Windows.Threading.DispatcherTimer()
        timer.Interval <- TimeSpan.FromSeconds(0.2)  // TODO decide time, interacts with both mapping and monster portraits...
        timer.Tick.Add(fun _ -> update())
        timer.Start()

    //////////////////////////////////////////////////
    // global hotkey

    let VK_F4 =  0x73
    let VK_F8 =  0x77
    let VK_F9 =  0x78
    let VK_F10 = 0x79
    //let MOD_CTRL = uint32 0x0002
    let MOD_NONE = 0u

    override this.OnSourceInitialized(e) =
        base.OnSourceInitialized(e)
        let helper = new WindowInteropHelper(this)
        source <- HwndSource.FromHwnd(helper.Handle)
        source.AddHook(HwndSourceHook(fun a b c d e -> this.HwndHook(a,b,c,d,&e)))
        this.RegisterHotKey()
        Constants.UIThreadSynchronizationContext <- System.Threading.SynchronizationContext.Current 

    override this.OnClosed(e) =
        source.RemoveHook(HwndSourceHook(fun a b c d e -> this.HwndHook(a,b,c,d,&e)))
        source <- null
        this.UnregisterHotKey()
        base.OnClosed(e)

    member this.RegisterHotKey() =
        let helper = new WindowInteropHelper(this);
        if(not(Winterop.RegisterHotKey(helper.Handle, Winterop.HOTKEY_ID, MOD_NONE, uint32 VK_F10))) then
            // handle error
            ()
        if(not(Winterop.RegisterHotKey(helper.Handle, Winterop.HOTKEY_ID, MOD_NONE, uint32 VK_F9))) then
            // handle error
            ()
        if(not(Winterop.RegisterHotKey(helper.Handle, Winterop.HOTKEY_ID, MOD_NONE, uint32 VK_F8))) then
            // handle error
            ()
        if(not(Winterop.RegisterHotKey(helper.Handle, Winterop.HOTKEY_ID, MOD_NONE, uint32 VK_F4))) then
            // handle error
            ()

    member this.UnregisterHotKey() =
        let helper = new WindowInteropHelper(this)
        Winterop.UnregisterHotKey(helper.Handle, Winterop.HOTKEY_ID) |> ignore

    member this.HwndHook(hwnd:IntPtr, msg:int, wParam:IntPtr, lParam:IntPtr, handled:byref<bool>) : IntPtr =
        let WM_HOTKEY = 0x0312
        if msg = WM_HOTKEY then
            if wParam.ToInt32() = Winterop.HOTKEY_ID then
                //let ctrl_bits = lParam.ToInt32() &&& 0xF  // see WM_HOTKEY docs
                let key = lParam.ToInt32() >>> 16
                if key = VK_F10 then
                    printfn "reset time"
                    startTime <- DateTime.Now - System.TimeSpan.FromHours(float ihrs) - System.TimeSpan.FromMinutes(float imins) - System.TimeSpan.FromSeconds(float isecs)
                elif key = VK_F4 then
                    let bmpScreenshot = Screenshot.GetDWRBitmap()
                    bmpScreenshot.Save(sprintf "Screen%03d.png" ssNum, System.Drawing.Imaging.ImageFormat.Png)
                    ssNum <- ssNum + 1
                handled <- true
        IntPtr.Zero

[<STAThread>]
[<EntryPoint>]
let xmain argv = 
    // Read all seeds in directory and summary-process
    if false then
        let mutable count = 0
        let mutable cont1, cont2 = 0, 0
        let N = Constants.MAP_LOCATIONS.ALL.Length 
        let on_cont1_count = Array.zeroCreate N
        let not_overworld_count = Array.zeroCreate N
        let num_cont2_locations = ResizeArray()
        let mutable single_continent_count = 0
        let mutable charlock_inn_dist = 0
        let debug_files = ResizeArray()
        let walkable_size = ResizeArray()
        for file in System.IO.Directory.EnumerateFiles("""C:\Users\Admin1\Desktop\fceux-2.2.3-win32\""", "DWRando*.CDFGMPRSTWZlr.nes") do
            let bytes = System.IO.File.ReadAllBytes(file)
            let strhp,_lhe,_lhu,_lHE,_lHU,_rl1 = ROM.show_go_mode_stats(bytes,false,file)
            let _bmp1,_bmp2,reachable_continents,mapCoords,cont_1_size,cont_2_size,ch_dist_inn,_owz,_ze,_uil,_zcl,_zmc = ROM.decode_rom(file)
            let mutable walkable_tiles = 0
            for x = 0 to 119 do
                for y = 0 to 119 do
                    if reachable_continents.[x,y] <> 0 then
                        walkable_tiles <- walkable_tiles + 1
            walkable_size.Add(walkable_tiles)
            count <- count + 1
            cont1 <- cont1 + cont_1_size
            charlock_inn_dist <- charlock_inn_dist + ch_dist_inn
            if cont_2_size = 0 then
                single_continent_count <- single_continent_count + 1
                debug_files.Add(file)
            else
                let mutable num_c2_loc = 0
                cont2 <- cont2 + cont_2_size
                for i = 0 to N-1 do
                    let loc = Constants.MAP_LOCATIONS.ALL.[i]
                    try
                        let x,y = if loc = Constants.MAP_LOCATIONS.CHARLOCK then let x,y = mapCoords.[loc] in x+3,y else mapCoords.[loc]
                        if reachable_continents.[x,y] = 1 then
                            on_cont1_count.[i] <- on_cont1_count.[i] + 1
                        else
                            num_c2_loc <- num_c2_loc + 1
                    with _ -> // under tantagel/garinham caves
                        not_overworld_count.[i] <- not_overworld_count.[i] + 1
                num_cont2_locations.Add(num_c2_loc)
        let multi_count = count - single_continent_count 
        printfn "Summary statistics of %d seeds" count
        printfn ""
        printfn "Tantagel Continent average size: %d" (cont1 / count)
        printfn "Other Continent average size:    %d" (cont2 / count)
        printfn "Avg dist charlock to inn:    %d" (charlock_inn_dist / count)
        printfn ""
        printfn "Single continent chance:   %f (%d of %d)" (float single_continent_count/float count) single_continent_count count
        printfn ""
        printfn "Chance of being on Tantagel Continent (when 2 continents):"
        for i = 0 to N-1 do
            let loc = Constants.MAP_LOCATIONS.ALL.[i]
            printfn " - %-25s:   %2d%%" loc (on_cont1_count.[i]*100/(multi_count - not_overworld_count.[i]))
        printfn ""
        let a = num_cont2_locations |> Seq.countBy id |> Seq.toArray 
        printfn "Histogram of number of locations on continent 2:"
        for i = 0 to N-1 do
            let n = 
                match a |> Array.tryFind (fun (l,c) -> l=i) with
                | Some (_,c) -> c
                | None -> 0
            printfn "%2d locations: %3d  %s" i n (String.replicate n "X")
        printfn ""
        for file in debug_files do
            printfn "single cont: %s" file
        printfn ""
        printfn "Histogram of walkable tile size:"
        let a = walkable_size |> Array.ofSeq |> Array.sort 
        for i = 45 to 85 do
            let n = a |> Array.filter (fun x -> x >= i*100 && x <= i*100+99) |> Array.length 
            printfn "%2d00: %s" i (String.replicate n "X")
        printfn ""
        printfn "average stats per level"
        printfn "Lev: STR agZ  HP mpZ"
        for i = 0 to 29 do
            printfn "L%2d: %3d %3d %3d %3d" (i+1) (ROM.agg_stats.[i,0]/ROM.agg_count) (ROM.agg_stats.[i,1]/ROM.agg_count) (ROM.agg_stats.[i,2]/ROM.agg_count) (ROM.agg_stats.[i,3]/ROM.agg_count)
        printfn "press enter"
        System.Console.ReadLine() |> ignore
        0
    else

    let DISPLAY_MAP_OF_SEED = argv.Length > 5
    if DISPLAY_MAP_OF_SEED then
        use fd = new System.Windows.Forms.OpenFileDialog()
        fd.InitialDirectory <- """C:\Users\Admin1\Desktop\dwrandomizer-2.0.6-windows\"""
        fd.ValidateNames <- true
        fd.CheckFileExists <- true
        fd.CheckPathExists <- true
        if fd.ShowDialog() = System.Windows.Forms.DialogResult.OK then
            let bmp1,bmp2,reachable_continents,_mapCoords,_cont_1_size,_cont_2_size,_ch_inn_dist,ow_zones,zone_enemies,uniqueItemLocations,zone_charlock_count,zone_metal_count = ROM.decode_rom(fd.FileName)

            let mutable walkable_tiles = 0
            for x = 0 to 119 do
                for y = 0 to 119 do
                    if reachable_continents.[x,y] <> 0 then
                        walkable_tiles <- walkable_tiles + 1

            let w = new Window()

            let c = new Canvas()
            let sp = new StackPanel()
            let g = new Grid()
            g.RowDefinitions.Add(new RowDefinition())
            g.ColumnDefinitions.Add(new ColumnDefinition())
            g.ColumnDefinitions.Add(new ColumnDefinition())

            let image1 = new Image()
            image1.Source <- Screenshot.BMPtoImage(bmp1)
            RenderOptions.SetBitmapScalingMode(image1, BitmapScalingMode.NearestNeighbor)
            let image2 = new Image()
            image2.Source <- Screenshot.BMPtoImage(bmp2)
            RenderOptions.SetBitmapScalingMode(image2, BitmapScalingMode.NearestNeighbor)

            gridAdd(g, image1, 0, 0)
            gridAdd(g, image2, 1, 0)

            // zone highlights
            for i = 0 to 7 do
                for j = 0 to 7 do
                    let n = zone_charlock_count.[i,j]
                    if n > 0 then
                        let g = new System.Windows.Shapes.Rectangle(Width=77.0,Height=77.0,StrokeThickness=float n)
                        g.Stroke <- Brushes.Red 
                        Canvas.SetTop(g, -712.0 + 32.0 + 81.0*float j + 2.0)
                        Canvas.SetLeft(g, 712.0 + 32.0 + 81.0*float i + 2.0)
                        c.Children.Add(g) |> ignore
                    let n = zone_metal_count.[i,j]
                    if n > 0 then
                        let g = new System.Windows.Shapes.Rectangle(Width=67.0,Height=67.0,StrokeThickness=float n)
                        g.Stroke <- Brushes.Blue
                        Canvas.SetTop(g, -712.0 + 32.0 + 81.0*float j + 7.0)
                        Canvas.SetLeft(g, 712.0 + 32.0 + 81.0*float i + 7.0)
                        c.Children.Add(g) |> ignore
            // zone key
            let tb = new TextBlock(Text=sprintf " Charlock enemy ",Foreground=Brushes.White,Background=Brushes.Black)
            c.Children.Add(tb) |> ignore
            Canvas.SetTop(tb, 16.0)
            Canvas.SetLeft(tb, 1300.0)
            let r = new System.Windows.Shapes.Rectangle(Width=100.0,Height=22.0,StrokeThickness=1.0,Stroke=Brushes.Red)
            c.Children.Add(r) |> ignore
            Canvas.SetTop(r, 13.0)
            Canvas.SetLeft(r, 1295.0)
            let tb = new TextBlock(Text=sprintf " Metal slime ",Foreground=Brushes.White,Background=Brushes.Black)
            c.Children.Add(tb) |> ignore
            Canvas.SetTop(tb, 48.0)
            Canvas.SetLeft(tb, 1300.0)
            let r = new System.Windows.Shapes.Rectangle(Width=76.0,Height=22.0,StrokeThickness=1.0,Stroke=Brushes.Blue)
            c.Children.Add(r) |> ignore
            Canvas.SetTop(r, 45.0)
            Canvas.SetLeft(r, 1295.0)

            // item text & icons
            let mutable px = 0
            let mutable py = 16
            let sorted = uniqueItemLocations.ToArray() |> Array.sortBy (fun (item,_loc) ->
                match ROM.CHESTS.[item] with
                | "STONES" -> 2
                | "HARP" -> 3
                | "TOKEN" -> 4
                | "RING" -> 6
                | "FLUTE" -> 8
                | "NECKLACE" -> 7
                | "SWORD" -> 5
                | "ARMOR" -> 1
                | _ -> failwith "bad summary item"
                )
            for item, (desc, ix, iy) in sorted do
                let tb = new TextBlock(Text=sprintf " %s:  %s " ROM.CHESTS.[item] desc,Foreground=Brushes.White,Background=Brushes.Black)
                c.Children.Add(tb) |> ignore
                Canvas.SetTop(tb, float py)
                Canvas.SetLeft(tb, float (px + 32))
                px <- px + 356
                if px >= 1424 then
                    px <- 0
                    py <- py + 32
                match ROM.CHESTS.[item] with
                | "STONES" ->
                    // key icon
                    let o = new System.Windows.Shapes.Ellipse(Width=20.0,Height=20.0,StrokeThickness=3.0)
                    o.Stroke <- Brushes.Orange 
                    Canvas.SetTop(o, 12.0)
                    Canvas.SetLeft(o, 358.0)
                    c.Children.Add(o) |> ignore
                    // map marker
                    let m = new System.Windows.Shapes.Ellipse(Width=20.0,Height=20.0,StrokeThickness=3.0)
                    m.Stroke <- Brushes.Orange
                                // right grid corner   coordinate  half circle  'pixel' width
                    Canvas.SetTop(m, -712.0 + 32.0 + 5.4*float iy   - 10.0         + 2.0)
                    Canvas.SetLeft(m, 712.0 + 32.0 + 5.4*float ix   - 10.0         + 2.0)
                    c.Children.Add(m) |> ignore
                | "HARP" ->
                    // key icon
                    let o = new System.Windows.Shapes.Line(X1=20.0,Y1=20.0,X2=0.0,Y2=0.0,StrokeThickness=3.0)
                    o.Stroke <- Brushes.SlateGray 
                    Canvas.SetTop(o, 14.0)
                    Canvas.SetLeft(o, 716.0)
                    c.Children.Add(o) |> ignore
                    // map marker
                    let m = new System.Windows.Shapes.Line(X1=20.0,Y1=20.0,X2=0.0,Y2=0.0,StrokeThickness=3.0)
                    m.Stroke <- Brushes.SlateGray 
                                // right grid corner   coordinate  half line  'pixel' width
                    Canvas.SetTop(m, -712.0 + 32.0 + 5.4*float iy   - 10.0         + 2.0)
                    Canvas.SetLeft(m, 712.0 + 32.0 + 5.4*float ix   - 10.0         + 2.0)
                    c.Children.Add(m) |> ignore
                | "TOKEN" ->
                    // key icon
                    let o = new System.Windows.Shapes.Ellipse(Width=16.0,Height=16.0,StrokeThickness=5.0)
                    o.Stroke <- Brushes.Yellow
                    Canvas.SetTop(o, 14.0)
                    Canvas.SetLeft(o, 1072.0)
                    c.Children.Add(o) |> ignore
                    // map marker
                    let m = new System.Windows.Shapes.Ellipse(Width=16.0,Height=16.0,StrokeThickness=5.0)
                    m.Stroke <- Brushes.Yellow
                                // right grid corner   coordinate  half circle  'pixel' width
                    Canvas.SetTop(m, -712.0 + 32.0 + 5.4*float iy   - 8.0         + 2.0)
                    Canvas.SetLeft(m, 712.0 + 32.0 + 5.4*float ix   - 8.0         + 2.0)
                    c.Children.Add(m) |> ignore
                | "RING" -> ()
                | "FLUTE" -> ()
                | "NECKLACE" -> ()
                | "SWORD" ->
                    // key icon
                    let o = new System.Windows.Shapes.Line(X1=0.0,Y1=20.0,X2=20.0,Y2=0.0,StrokeThickness=3.0)
                    o.Stroke <- Brushes.Blue 
                    Canvas.SetTop(o, 44.0)
                    Canvas.SetLeft(o, 2.0)
                    c.Children.Add(o) |> ignore
                    // map marker
                    let m = new System.Windows.Shapes.Line(X1=0.0,Y1=20.0,X2=20.0,Y2=0.0,StrokeThickness=3.0)
                    m.Stroke <- Brushes.Blue 
                                // right grid corner   coordinate  half line   'pixel' width
                    Canvas.SetTop(m, -712.0 + 32.0 + 5.4*float iy   - 10.0         + 2.0)
                    Canvas.SetLeft(m, 712.0 + 32.0 + 5.4*float ix   - 10.0         + 2.0)
                    c.Children.Add(m) |> ignore
                | "ARMOR" -> 
                    // key icon
                    let o = new System.Windows.Shapes.Ellipse(Width=24.0,Height=24.0,StrokeThickness=3.0)
                    o.Stroke <- Brushes.Blue 
                    Canvas.SetTop(o, 10.0)
                    Canvas.SetLeft(o, 0.0)
                    c.Children.Add(o) |> ignore
                    // map marker
                    let m = new System.Windows.Shapes.Ellipse(Width=24.0,Height=24.0,StrokeThickness=3.0)
                    m.Stroke <- Brushes.Blue 
                                // right grid corner   coordinate  half circle  'pixel' width
                    Canvas.SetTop(m, -712.0 + 32.0 + 5.4*float iy   - 12.0         + 2.0)
                    Canvas.SetLeft(m, 712.0 + 32.0 + 5.4*float ix   - 12.0         + 2.0)
                    c.Children.Add(m) |> ignore
                | _ -> failwith "bad summary item"

            // walkable tiles
            let tb = new TextBlock(Text=sprintf " %d walkable tiles " walkable_tiles,Foreground=Brushes.White,Background=Brushes.Black)
            c.Children.Add(tb) |> ignore
            Canvas.SetTop(tb, 80.0)
            Canvas.SetLeft(tb, 32.0)

            // dungeon zone names and mouse popup locations
            let dxh = 32.0+128.0
            let dtb = new TextBlock(Text="HAUKS",Foreground=Brushes.White,Background=Brushes.Black)
            c.Children.Add(dtb) |> ignore
            Canvas.SetTop(dtb, 80.0)
            Canvas.SetLeft(dtb, dxh)
            let dxs = dxh+128.0
            let dtb = new TextBlock(Text="SWAMP",Foreground=Brushes.White,Background=Brushes.Black)
            c.Children.Add(dtb) |> ignore
            Canvas.SetTop(dtb, 80.0)
            Canvas.SetLeft(dtb, dxs)
            let dxm = dxs+128.0
            let dtb = new TextBlock(Text="M2/G1",Foreground=Brushes.White,Background=Brushes.Black)
            c.Children.Add(dtb) |> ignore
            Canvas.SetTop(dtb, 80.0)
            Canvas.SetLeft(dtb, dxm)
            let dxg = dxm+128.0
            let dtb = new TextBlock(Text="GTLOW",Foreground=Brushes.White,Background=Brushes.Black)
            c.Children.Add(dtb) |> ignore
            Canvas.SetTop(dtb, 80.0)
            Canvas.SetLeft(dtb, dxg)
            let dxc1 = dxg+128.0
            let dtb = new TextBlock(Text="CHAR1",Foreground=Brushes.White,Background=Brushes.Black)
            c.Children.Add(dtb) |> ignore
            Canvas.SetTop(dtb, 80.0)
            Canvas.SetLeft(dtb, dxc1)
            let dxc2 = dxc1+128.0
            let dtb = new TextBlock(Text="CHAR2",Foreground=Brushes.White,Background=Brushes.Black)
            c.Children.Add(dtb) |> ignore
            Canvas.SetTop(dtb, 80.0)
            Canvas.SetLeft(dtb, dxc2)
            let dxc3 = dxc2+128.0
            let dtb = new TextBlock(Text="CHAR3",Foreground=Brushes.White,Background=Brushes.Black)
            c.Children.Add(dtb) |> ignore
            Canvas.SetTop(dtb, 80.0)
            Canvas.SetLeft(dtb, dxc3)
            // zone popups
            let makeEnemyString(zone) =
                let a = zone_enemies.[zone] |> Array.sort 
                let enemies = new System.Text.StringBuilder()
                for i = 0 to 4 do
                    let n,name,spells,ss_resist = a.[i]
                    let resist = if spells <> "" then sprintf " (%d/16 stop resist)" (int ss_resist) else ""
                    enemies.AppendLine(name + " " + spells + resist) |> ignore
                enemies.ToString()
            let popup = new System.Windows.Controls.Primitives.Popup()
            let tb = new TextBlock(Text="testing",Foreground=Brushes.White,Background=Brushes.Black)
            popup.Child <- tb
            popup.Placement <- System.Windows.Controls.Primitives.PlacementMode.MousePoint 
            popup.IsOpen <- true
            popup.VerticalOffset <- 16.0
            popup.HorizontalOffset <- 16.0
            w.LostFocus.Add(fun _ -> popup.IsOpen <- false)
            w.Deactivated.Add(fun _ -> popup.IsOpen <- false)
            sp.MouseMove.Add(fun e ->
                popup.IsOpen <- false
                let p = e.GetPosition(sp)
                // 32 boundary, each cell is 81x81, thus 712 is x break of two 
                let x = (if p.X > 713.0 then int p.X - 712 else int p.X ) - 32
                let y = (int p.Y) - 32
                if x >= 0 && x < 8*81 && y >= 0 && y < 8*81 then
                    let x = x/81
                    let y = y/81
                    tb.Text <- makeEnemyString(int ow_zones.[x,y])
                    popup.IsOpen <- true
                elif p.X >= dxh && p.X <= dxh+20.0 && p.Y >= 712.0+80.0 && p.Y <= 712.0+100.0 then
                    tb.Text <- makeEnemyString(13) // HAUKS
                    popup.IsOpen <- true
                elif p.X >= dxs && p.X <= dxs+20.0 && p.Y >= 712.0+80.0 && p.Y <= 712.0+100.0 then
                    tb.Text <- makeEnemyString(19) // SWAMP
                    popup.IsOpen <- true
                elif p.X >= dxm && p.X <= dxm+20.0 && p.Y >= 712.0+80.0 && p.Y <= 712.0+100.0 then
                    tb.Text <- makeEnemyString(14) // M2/G1
                    popup.IsOpen <- true
                elif p.X >= dxg && p.X <= dxg+20.0 && p.Y >= 712.0+80.0 && p.Y <= 712.0+100.0 then
                    tb.Text <- makeEnemyString(15) // GTLOW
                    popup.IsOpen <- true
                elif p.X >= dxc1 && p.X <= dxc1+20.0 && p.Y >= 712.0+80.0 && p.Y <= 712.0+100.0 then
                    tb.Text <- makeEnemyString(16) // CHAR1
                    popup.IsOpen <- true
                elif p.X >= dxc2 && p.X <= dxc2+20.0 && p.Y >= 712.0+80.0 && p.Y <= 712.0+100.0 then
                    tb.Text <- makeEnemyString(17) // CHAR2
                    popup.IsOpen <- true
                elif p.X >= dxc3 && p.X <= dxc3+20.0 && p.Y >= 712.0+80.0 && p.Y <= 712.0+100.0 then
                    tb.Text <- makeEnemyString(18) // CHAR3
                    popup.IsOpen <- true
                )

            //w.Width <- 960.0
            w.Height <- 880.0
            w.Title <- System.IO.Path.GetFileNameWithoutExtension(fd.FileName)
            w.Top <- 20.0
            sp.Children.Add(g) |> ignore
            sp.Children.Add(popup) |> ignore
            sp.Children.Add(c) |> ignore
            StackPanel.SetZIndex(c, 2)
            StackPanel.SetZIndex(g, 1)
            w.Content <- sp
            w.Background <- Brushes.LightGray  
            (new Application()).Run(w)
        else
            printfn "bad file selection"
            1
    else
    let app = new Application()
    let myAssembly = System.Reflection.Assembly.GetExecutingAssembly()
    let names = myAssembly.GetManifestResourceNames()

    // load up known overworld map tiles
    for ow,kind in Constants.OVERWORLD_MAP_TILE_FILENAMES do
        let imageStream = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream(ow)
        let bmp = new System.Drawing.Bitmap(imageStream)
        Screenshot.UniqueOverworldTiles.Add(ow,kind,new FastRGBBitmap(bmp))
    printfn "loaded %d overworld tiles" Screenshot.UniqueOverworldTiles.Count 
     
    let racingMode = argv.Length > 0
    let leagueMode = argv.Length > 1
    let xp = if leagueMode then Constants.DWR_XP_LEVEL_THRESHOLDS_50_PERCENT else Constants.DWR_XP_LEVEL_THRESHOLDS 
    try
        app.Run(MyWindow(0,0,0,racingMode,leagueMode,xp)) |> ignore
    with e ->
        printfn "crashed with exception"
        printfn "%s" (e.ToString())
        printfn "press enter to end"
        System.Console.ReadLine() |> ignore
    0



