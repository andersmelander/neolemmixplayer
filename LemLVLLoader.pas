{$include lem_directives.inc}
unit LemLVLLoader;

interface

uses
  Classes, SysUtils, StrUtils,
  //Dialogs, Controls,
  LemBcGraphicSet,
  Dialogs,
  UMisc,
  Math,
  LemStrings,
  LemNeoParser,
  LemDosMainDat,
  LemPiece,
  LemTerrain,
  LemInteractiveObject,
  LemSteel,
  LemDosStructures,
  LemLevel,
  LemLemming,
  LemNeoPieceManager,
  LemMetaObject,
  LemTypes,
  LemCore;

type
  TStyleName = class
  private
    fStyleName : ShortString;
    fID : Byte;
    fSpecial : Boolean;
  protected
  public
    constructor Create(aTag: ShortString; aID: Byte; aSpec: Boolean = false);
  published
    property StyleName : ShortString read fStyleName;
    property ID: Byte read fID;
    property Special: Boolean read fSpecial;
  end;

  TStyleFinder = class
  private
    fStyleList : TList;
    //fSysDat: TSysDatRec;
    procedure InitList;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function FindNumber(tag: ShortString; spec: Boolean): Integer;
    function FindName(id: Byte; spec: Boolean): ShortString;
  published
  end;

  TMusicName = class
  private
    fMusicName: ShortString;
    fID: Byte;
  public
    constructor Create(aTag: ShortString; aID: Byte);
  published
    property MusicName: ShortString read fMusicName;
    property ID: Byte read fID;
  end;

  TMusicFinder = class
  private
    fMusicList : TList;
    procedure InitList;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function FindNumber(tag: ShortString): Integer;
    function FindName(id: Byte): ShortString;
  published
  end;

  TLVLLoader = class
  private
    class procedure UpgradeFormat(var Buf: TNeoLVLRec);
  protected
  public
    class procedure LoadLevelFromStream(aStream: TStream; aLevel: TLevel; OddLoad: Byte = 0);
    class procedure LoadTradLevelFromStream(aStream: TStream; aLevel: TLevel; OddLoad: Byte = 0);
    class procedure LoadNeoLevelFromStream(aStream: TStream; aLevel: TLevel; OddLoad: Byte = 0);
    class procedure LoadNewNeoLevelFromStream(aStream: TStream; aLevel: TLevel; OddLoad: Byte = 0);
    class procedure StoreLevelInStream(aLevel: TLevel; aStream: TStream; aComment: String = '');
  end;

  procedure BcTranslate(aLevel: TLevel);

implementation

procedure BcTranslate(aLevel: TLevel);
var
  i: Integer;
  O: TInteractiveObject;
  MO_PM: TMetaObjectInterface;  // for objects from the piece manager
  L: TPreplacedLemming;

  procedure RemovedObjectPatch(Removed: Integer);
  var
    i: Integer;
  begin
    for i := 0 to Length(aLevel.Info.WindowOrder)-1 do
      if aLevel.Info.WindowOrder[i] >= Removed then
        aLevel.Info.WindowOrder[i] := aLevel.Info.WindowOrder[i] - 1;
  end;

  function FixStyleName(aName: String): String;
  begin
    // Fixes a few style names that have been changed
    Result := aName;
    aName := Lowercase(Trim(aName)); // just in case
    if aName = 'gigalem_l3egypt' then
      Result := 'l3_egypt';
    if aName = 'gigalem_l3shadow' then
      Result := 'l3_shadow';
    if aName = 'medival' then
      Result := 'medieval';
  end;

begin
  // The level's primary graphic set should be fixed
  aLevel.Info.GraphicSetName := FixStyleName(aLevel.Info.GraphicSetName);

  // Needs to convert preplaced lemmings appropriately and correct style names on objects
  for i := aLevel.InteractiveObjects.Count-1 downto 0 do
  begin
    O := aLevel.InteractiveObjects[i];

    O.GS := FixStyleName(O.GS);

    if PieceManager = nil then Exit;

    MO_PM := PieceManager.Objects[O.GS + ':' + O.Piece].GetInterface(false, false, false);
    if MO_PM.TriggerEffect = 13 then
    begin
      L := aLevel.PreplacedLemmings.Insert(0);
      L.X := O.Left + MO_PM.TriggerLeft;
      L.Y := O.Top + MO_PM.TriggerTop;
      if O.DrawingFlags and odf_FlipLem <> 0 then
        L.Dx := -1
      else
        L.Dx := 1;
      L.IsClimber := (O.TarLev and 1) <> 0;
      L.IsSwimmer := (O.TarLev and 2) <> 0;
      L.IsFloater := (O.TarLev and 4) <> 0;
      L.IsGlider := ((O.TarLev and 8) <> 0) and not L.IsFloater;
      L.IsDisarmer := (O.TarLev and 16) <> 0;
      L.IsBlocker := (O.TarLev and 32) <> 0;
      L.IsZombie := (O.TarLev and 64) <> 0;
      aLevel.InteractiveObjects.Delete(i);
      RemovedObjectPatch(i);
      Continue;
    end;

    if MO_PM.TriggerEffect = 14 then
    begin
      case O.Skill + (O.TarLev * 16) of
        0: O.Skill := Integer(spbClimber);
        1: O.Skill := Integer(spbFloater);
        2: O.Skill := Integer(spbBomber);
        3: O.Skill := Integer(spbBlocker);
        4: O.Skill := Integer(spbBuilder);
        5: O.Skill := Integer(spbBasher);
        6: O.Skill := Integer(spbMiner);
        7: O.Skill := Integer(spbDigger);
        8: O.Skill := Integer(spbWalker);
        9: O.Skill := Integer(spbSwimmer);
        10: O.Skill := Integer(spbGlider);
        11: O.Skill := Integer(spbDisarmer);
        12: O.Skill := Integer(spbStoner);
        13: O.Skill := Integer(spbPlatformer);
        14: O.Skill := Integer(spbStacker);
        15: O.Skill := Integer(spbCloner);
        16: O.Skill := Integer(spbFencer);
      end;
    end;

    if MO_PM.TriggerEffect = 32 then
    begin
      aLevel.InteractiveObjects.Delete(i);
      RemovedObjectPatch(i);
      Continue;
    end;
  end;

  // Terrains just need graphic set name fix
  for i := 0 to aLevel.Terrains.Count-1 do
    aLevel.Terrains[i].GS := FixStyleName(aLevel.Terrains[i].GS);

  // And backgrounds
  if aLevel.Info.Background <> '' then
    aLevel.Info.Background := aLevel.Info.GraphicSetName + ':BG' + aLevel.Info.Background; // Info.GraphicSetName is already fixed

end;

{ TStyleName }

constructor TStyleName.Create(aTag: ShortString; aID: Byte; aSpec: Boolean = false);
begin
  inherited Create;
  fStyleName := trim(lowercase(leftstr(aTag, 16)));
  fID := aID;
  fSpecial := aSpec;
end;

{ TMusicName }

constructor TMusicName.Create(aTag: ShortString; aID: Byte);
begin
  inherited Create;
  fMusicName := trim(lowercase(leftstr(aTag, 16)));
  fID := aID;
end;

{ TStyleFinder }

constructor TStyleFinder.Create;
begin
  InitList;
end;

destructor TStyleFinder.Destroy;
begin
  fStyleList.Free;
  inherited Destroy;
end;

procedure TStyleFinder.InitList;
//var
  //i: Integer;
  //fMainDatExtractor : TMainDatExtractor;

  procedure Sty(tag: ShortString; ID: byte; Spec: boolean = false);
  var
    SN : TStyleName;
  begin
    SN := TStyleName.Create(tag, ID, Spec);
    fStyleList.Add(SN);
  end;

begin
  fStyleList := TList.Create;
    Sty('dirt',         0);
    Sty('fire',         1);
    Sty('marble',       2);
    Sty('pillar',       3);
    Sty('crystal',      4);
    Sty('brick',        5);
    Sty('rock',         6);
    Sty('snow',         7);
    Sty('bubble',       8);
    Sty('xmas',         9);
    Sty('tree',        10);
    Sty('purple',      11);
    Sty('psychedelic', 12);
    Sty('metal',       13);
    Sty('desert',      14);
    Sty('sky',         15);
    Sty('circuit',     16);
    Sty('martian',     17);
    Sty('lab',         18);
    Sty('sega',        19);
    Sty('dirt_md',     20);
    Sty('fire_md',     21);
    Sty('marble_md',   22);
    Sty('pillar_md',   23);
    Sty('crystal_md',  24);
    Sty('horror',      25);
end;

function TStyleFinder.FindNumber(tag: ShortString; Spec: Boolean): Integer;
var
  i: Integer;
  SN: TStyleName;
begin
  tag := trim(lowercase(leftstr(tag, 16)));
  for i := 0 to fStyleList.Count - 1 do
  begin
    SN := TStyleName(fStyleList[i]);
    if SN.Special <> Spec then Continue;
    if SN.StyleName <> tag then Continue;
    Result := SN.ID;
    Exit;
  end;
  Result := -1;
end;

function TStyleFinder.FindName(ID: byte; Spec: Boolean): ShortString;
var
  i: Integer;
  SN: TStyleName;
begin
  for i := 0 to fStyleList.Count - 1 do
  begin
    SN := TStyleName(fStyleList[i]);
    if SN.Special <> Spec then Continue;
    if SN.ID <> ID then Continue;
    Result := SN.StyleName;
    Exit;
  end;
  Result := '';
end;

{ TMusicFinder }

constructor TMusicFinder.Create;
begin
  InitList;
end;

destructor TMusicFinder.Destroy;
begin
  fMusicList.Free;
  inherited Destroy;
end;

procedure TMusicFinder.InitList;

  procedure Mus(tag: ShortString; ID: byte);
  var
    SN : TMusicName;
  begin
    SN := TMusicName.Create(tag, ID);
    fMusicList.Add(SN);
  end;

begin
  fMusicList := TList.Create;
end;

function TMusicFinder.FindNumber(tag: ShortString): Integer;
var
  i: Integer;
  SN: TMusicName;
begin
  tag := trim(lowercase(leftstr(tag, 16)));
  for i := 0 to fMusicList.Count - 1 do
  begin
    SN := TMusicName(fMusicList[i]);
    if SN.MusicName <> tag then Continue;
    Result := SN.ID;
    Exit;
  end;
  Result := -1;
end;

function TMusicFinder.FindName(ID: byte): ShortString;
var
  i: Integer;
  SN: TMusicName;
begin
  for i := 0 to fMusicList.Count - 1 do
  begin
    SN := TMusicName(fMusicList[i]);
    if SN.ID <> ID then Continue;
    Result := SN.MusicName;
    Exit;
  end;
  Result := '';
end;

{ TLVLLoader }

class procedure TLVLLoader.LoadLevelFromStream(aStream: TStream; aLevel: TLevel; OddLoad: Byte = 0);
var
  b: byte;
  i, i2: integer;
  NewLevelID: Integer;
begin
  aStream.Seek(0, soFromBeginning);
  aStream.Read(b, 1);
  aStream.Seek(0, soFromBeginning);

  aLevel.Clear;

  case b of
    0: LoadTradLevelFromStream(aStream, aLevel);
  1..3: LoadNeoLevelFromStream(aStream, aLevel);
    4: LoadNewNeoLevelFromStream(aStream, aLevel);
    else raise Exception.Create('Invalid level or unsupported format.');
  end;

  if (b <= 4) then
    aLevel.Info.HasTimeLimit := aLevel.Info.TimeLimit < 6000;

  // if the level has no Level ID, make one.
  // must be pseudo-random to enough extent to generate a different ID for each level,
  // but the same ID for the same level if unmodified

  i2 := 0;
  while aLevel.Info.LevelID = 0 do
  begin
    NewLevelID := aLevel.Info.LevelID;
    Inc(i2);
    for i := 0 to aLevel.InteractiveObjects.Count-1 do
    begin
      NewLevelID := NewLevelID + aLevel.InteractiveObjects[i].Left * i2;
      NewLevelID := NewLevelID + aLevel.InteractiveObjects[i].Top * i2;
      NewLevelID := NewLevelID + aLevel.InteractiveObjects[i].DrawingFlags;
      NewLevelID := NewLevelID + aLevel.InteractiveObjects[i].Skill;
      NewLevelID := NewLevelID + aLevel.InteractiveObjects[i].TarLev;
      if NewLevelID = 0 then NewLevelID := aLevel.Info.RescueCount;
    end;

    for i := 0 to aLevel.Terrains.Count-1 do
    begin
      NewLevelID := NewLevelID + aLevel.Terrains[i].Left * i2;
      NewLevelID := NewLevelID + aLevel.Terrains[i].Top * i2;
      NewLevelID := NewLevelID + aLevel.Terrains[i].DrawingFlags;
      if NewLevelID = 0 then NewLevelID := aLevel.Info.LemmingsCount;
    end;

    for i := 0 to aLevel.Steels.Count-1 do
    begin
      NewLevelID := NewLevelID + aLevel.Steels[i].Left * i2;
      NewLevelID := NewLevelID + aLevel.Steels[i].Top * i2;
      NewLevelID := NewLevelID + aLevel.Steels[i].Width * i2;
      NewLevelID := NewLevelID + aLevel.Steels[i].Height * i2;
      NewLevelID := NewLevelID + aLevel.Steels[i].fType;
      if NewLevelID = 0 then NewLevelID := aLevel.Info.ReleaseRate;
    end;

    while (NewLevelID > 0) do
      NewLevelID := NewLevelID xor (NewLevelID shl 1);

    for i := 1 to Length(aLevel.Info.Title)-3 do
    begin
      NewLevelID := NewLevelID + (i2 * i2);
      NewLevelID := NewLevelID xor ((Ord(aLevel.Info.Title[i]) shl 24) +
                                    (Ord(aLevel.Info.Title[i+1]) shl 16) +
                                    (Ord(aLevel.Info.Title[i+2]) shl 8) +
                                    (Ord(aLevel.Info.Title[i+3])));
    end;

    NewLevelID := NewLevelID + aLevel.InteractiveObjects.Count + aLevel.Terrains.Count + aLevel.Steels.Count;

    aLevel.Info.LevelID := NewLevelID;
  end;

  BcTranslate(aLevel);
  aLevel.Sanitize;
end;


class procedure TLVLLoader.LoadNewNeoLevelFromStream(aStream: TStream; aLevel: TLevel; OddLoad: Byte = 0);
{-------------------------------------------------------------------------------
  Translate a LVL file and fill the collections.
  For decoding and technical details see documentation or read the code :)
-------------------------------------------------------------------------------}
var
  Buf: TNeoLVLHeader;
  Buf2: TNeoLVLSecondHeader;
  i, x, x2: Integer;
  O: TNewNeoLVLObject;
  T: TNewNeoLVLTerrain;
  S: TNewNeoLVLSteel;
  Obj: TInteractiveObject;
  Ter: TTerrain;
  Steel: TSteel;
  //SFinder: TStyleFinder;
  GSNames: array of String;
  GSName: array[0..15] of Char;

  b: Byte;
  w: Word;

  LRes: Byte;

  HasSubHeader: Boolean;

  SkipObjects: Array of Integer;

  procedure AddSkill(aSkill: TSkillPanelButton);
  begin
    if x2 < 8 then
      aLevel.Info.Skillset := aLevel.Info.Skillset + [aSkill];
    Inc(x2);
  end;

  procedure SetSkillCount(aSkill: TSkillPanelButton; aCount: Integer);
  begin
    if not(aSkill in aLevel.Info.Skillset) then Exit;
    aLevel.Info.SkillCount[aSkill] := aCount;
  end;
begin
  SetLength(SkipObjects, 0);
  HasSubHeader := false;
  with aLevel do
  begin
    aStream.ReadBuffer(Buf, SizeOf(Buf));
    //UpgradeFormat(Buf);
    {-------------------------------------------------------------------------------
      Get the statics. This is easy
    -------------------------------------------------------------------------------}
    with Info do
    begin
      if OddLoad <> 1 then
      begin
        aLevel.Clear;
        ReleaseRate      := Buf.ReleaseRate;
        ReleaseRateLocked := (Buf.LevelOptions2 and 1) <> 0;
        Background := IntToStr(Buf.BgIndex);

        LemmingsCount    := Buf.LemmingsCount;
        RescueCount      := Buf.RescueCount;
        TimeLimit        := Buf.TimeLimit; // internal structure now matches NeoLemmix file format structure (just a number of seconds)

        x2 := 0;
        if Buf.Skillset and $8000 <> 0 then AddSkill(spbWalker);
        if Buf.Skillset and $4000 <> 0 then AddSkill(spbClimber);
        if Buf.Skillset and $2000 <> 0 then AddSkill(spbSwimmer);
        if Buf.Skillset and $1000 <> 0 then AddSkill(spbFloater);
        if Buf.Skillset and $800 <> 0 then AddSkill(spbGlider);
        if Buf.Skillset and $400 <> 0 then AddSkill(spbDisarmer);
        if Buf.Skillset and $200 <> 0 then AddSkill(spbBomber);
        if Buf.Skillset and $100 <> 0 then AddSkill(spbStoner);
        if Buf.Skillset and $80 <> 0 then AddSkill(spbBlocker);
        if Buf.Skillset and $40 <> 0 then AddSkill(spbPlatformer);
        if Buf.Skillset and $20 <> 0 then AddSkill(spbBuilder);
        if Buf.Skillset and $10 <> 0 then AddSkill(spbStacker);
        if Buf.Skillset and $8 <> 0 then AddSkill(spbBasher);
        if Buf.Skillset and $4 <> 0 then AddSkill(spbMiner);
        if Buf.Skillset and $2 <> 0 then AddSkill(spbDigger);
        if Buf.Skillset and $1 <> 0 then AddSkill(spbCloner);
        if Buf.LevelOptions2 and $2 <> 0 then AddSkill(spbFencer);

        SetSkillCount(spbWalker, Buf.WalkerCount);
        SetSkillCount(spbClimber, Buf.ClimberCount);
        SetSkillCount(spbSwimmer, Buf.SwimmerCount);
        SetSkillCount(spbFloater, Buf.FloaterCount);
        SetSkillCount(spbGlider, Buf.GliderCount);
        SetSkillCount(spbDisarmer, Buf.MechanicCount);
        SetSkillCount(spbBomber, Buf.BomberCount);
        SetSkillCount(spbStoner, Buf.StonerCount);
        SetSkillCount(spbBlocker, Buf.BlockerCount);
        SetSkillCount(spbPlatformer, Buf.PlatformerCount);
        SetSkillCount(spbBuilder, Buf.BuilderCount);
        SetSkillCount(spbStacker, Buf.StackerCount);
        SetSkillCount(spbBasher, Buf.BasherCount);
        SetSkillCount(spbMiner, Buf.MinerCount);
        SetSkillCount(spbDigger, Buf.DiggerCount);
        SetSkillCount(spbCloner, Buf.ClonerCount);
        SetSkillCount(spbFencer, Buf.FencerCount);

        Title            := Buf.LevelName;
        Author           := Buf.LevelAuthor;
        LevelID := Buf.LevelID;
      end;

      if (OddLoad = 2) and (Buf.LevelOptions and 16 <> 0) then
      begin
        LevelOptions := $71;
        LRes := 8;
      end else begin

        LRes := Buf.Resolution;
        if LRes = 0 then LRes := 8;

        Width := (Buf.Width * 8) div LRes;
        Height := (Buf.Height * 8) div LRes;


        if Width < 1 then Width := 1;
        if Height < 1 then Height := 1;

        // Screen positions are saved as a Word, i.e. unsigned. So we treat anything >32768 as negative
        if Buf.ScreenPosition > 32768 then ScreenPosition := 160
        else
        begin
          ScreenPosition   := ((Buf.ScreenPosition * 8) div LRes) + 160;
          if ScreenPosition > (Width - 160) then ScreenPosition := (Width - 160);
          if ScreenPosition < 0 then ScreenPosition := 0;
        end;
        if Buf.ScreenYPosition > 32768 then ScreenYPosition := 80
        else
        begin
          ScreenYPosition := ((Buf.ScreenYPosition * 8) div LRes) + 80;
          if ScreenYPosition > (Height - 80) then ScreenYPosition := (Height - 80);
          if ScreenYPosition < 0 then ScreenYPosition := 0;
        end;

        GraphicSetName := trim(Buf.StyleName);

        SetLength(GSNames, 1);
        GSNames[0] := GraphicSetName; // fallback in case lvl file has no graphic set list, as most won't

        LevelOptions := Buf.LevelOptions;
      end;
      if LevelOptions and $2 = 0 then
        LevelOptions := LevelOptions and $F7;
    end;

    //if (OddLoad = 2) and (Info.LevelOptions and 16 <> 0) then Exit;

    //b := 0;
    //aStream.Read(b, 1);
    while (aStream.Read(b, 1) <> 0) do
    begin
      case b of
        1: begin
             aStream.Read(O, SizeOf(O));
             if (O.ObjectFlags and 128) = 0 then
             begin
               SetLength(SkipObjects, Length(SkipObjects) + 1);
               SkipObjects[Length(SkipObjects)-1] := InteractiveObjects.Count; //not a mistake; it's *intended* that it doesn't take into account previous skipped objects
             end else begin
             Obj := InteractiveObjects.Add;
             Obj.Left := (O.XPos * 8) div LRes;
             Obj.Top := (O.YPos * 8) div LRes;
             Obj.GS := IntToStr(O.GSIndex);
             Obj.Piece := IntToStr(O.ObjectID);
             Obj.TarLev := O.LValue;
             if O.ObjectFlags and $1 <> 0 then
               Obj.DrawingFlags := Obj.DrawingFlags or odf_NoOverwrite;
             if O.ObjectFlags and $2 <> 0 then
               Obj.DrawingFlags := Obj.DrawingFlags or odf_OnlyOnTerrain;
             if O.ObjectFlags and $4 <> 0 then
               Obj.DrawingFlags := Obj.DrawingFlags or odf_UpsideDown;
             if O.ObjectFlags and $8 <> 0 then
               Obj.DrawingFlags := Obj.DrawingFlags or odf_FlipLem;
             if O.ObjectFlags and $10 <> 0 then
               Obj.IsFake := true;
             if O.ObjectFlags and $20 <> 0 then
               Obj.DrawingFlags := Obj.DrawingFlags or odf_Invisible;
             if O.ObjectFlags and $40 <> 0 then
               Obj.DrawingFlags := Obj.DrawingFlags or odf_Flip;
             if O.ObjectFlags and $100 <> 0 then
               Obj.DrawingFlags := Obj.DrawingFlags or odf_Rotate;
             Obj.Skill := O.SValue mod 16;

             Obj.LastDrawX := Obj.Left;
             Obj.LastDrawY := Obj.Top;
             Obj.DrawAsZombie := false;
             end;
           end;
        2: begin
             aStream.Read(T, SizeOf(T));
             if (T.TerrainFlags and 128) <> 0 then
             begin
             Ter := Terrains.Add;
             Ter.Left := (T.XPos * 8) div LRes;
             Ter.Top := (T.YPos * 8) div LRes;
             Ter.GS := IntToStr(T.GSIndex);
             Ter.Piece := IntToStr(T.TerrainID);
             if T.TerrainFlags and $1 <> 0 then
               Ter.DrawingFlags := Ter.DrawingFlags or tdf_NoOverwrite;
             if T.TerrainFlags and $2 <> 0 then
               Ter.DrawingFlags := Ter.DrawingFlags or tdf_Erase;
             if T.TerrainFlags and $4 <> 0 then
               Ter.DrawingFlags := Ter.DrawingFlags or tdf_Invert;
             if T.TerrainFlags and $8 <> 0 then
               Ter.DrawingFlags := Ter.DrawingFlags or tdf_Flip;
             if T.TerrainFlags and $20 <> 0 then
               Ter.DrawingFlags := Ter.DrawingFlags or tdf_Rotate;
             if Info.LevelOptions and $80 = 0 then
             begin
               if T.TerrainFlags and $10 <> 0 then
                 Ter.DrawingFlags := Ter.DrawingFlags or tdf_NoOneWay;
             end else begin
               if T.TerrainFlags and $10 = 0 then
                 Ter.DrawingFlags := Ter.DrawingFlags or tdf_NoOneWay;
             end;
             end;
           end;
        3: begin
             aStream.Read(S, SizeOf(S));
             if (S.SteelFlags and 128) <> 0 then
             begin
             Steel := Steels.Add;
             Steel.Left := (S.XPos * 8) div LRes;
             Steel.Top := (S.YPos * 8) div LRes;
             Steel.Width := ((S.SteelWidth + 1) * 8) div LRes;
             Steel.Height := ((S.SteelHeight + 1) * 8) div LRes;
             Steel.fType := S.SteelFlags and not $80;
             end;
           end;
        4: begin
             SetLength(Info.WindowOrder, 0);
             w := $FFFF;
             aStream.Read(w, 2);
             while w <> $FFFF do
             begin
               SetLength(Info.WindowOrder, Length(Info.WindowOrder) + 1);
               Info.WindowOrder[Length(Info.WindowOrder) - 1] := w;
               w := $FFFF;
               aStream.Read(w, 2);
             end;
           end;
        5: begin
             aStream.Read(Buf2, SizeOf(Buf2));
             HasSubHeader := true;
             Info.ScreenPosition := ((Buf2.ScreenPosition * 8) div LRes) + 160;
             Info.ScreenYPosition := ((Buf2.ScreenYPosition * 8) div LRes) + 80;
             with Info do
             begin
               if ScreenPosition > Width-1 then ScreenPosition := Width-1;
               if ScreenYPosition > Height-1 then ScreenYPosition := Height-1;
               if ScreenPosition < 0 then ScreenPosition := 0;
               if ScreenYPosition < 0 then ScreenYPosition := 0;
             end;
             if OddLoad <> 1 then
             begin
               Info.MusicFile := Trim(Buf2.MusicName);
             end;
           end;
        6: begin
             aStream.Read(w, 2);
             SetLength(GSNames, w);
             for i := 0 to w-1 do
             begin
               aStream.Read(GSName, 16);
               GSNames[i] := Lowercase(Trim(GSName));
             end;
           end;
        else Break;
      end;

      //b := 0;
      //aStream.Read(b, 1);
    end;

    for i := 0 to Length(SkipObjects)-1 do
      for x := 0 to Length(Info.WindowOrder)-1 do
        if Info.WindowOrder[x] > SkipObjects[i] then Dec(Info.WindowOrder[x]);

    if (not HasSubHeader) and (OddLoad <> 1) then
    with Info do
      begin
        case Buf.MusicNumber of
          0: MusicFile := '';
        253: MusicFile := '*';
        254: MusicFile := 'frenzy';
        255: MusicFile := 'gimmick';
          else  MusicFile := 'track_' + LeadZeroStr(Buf.MusicNumber, 2); // best compatibility with existing packs
        end;
      end;

    (*if (OddLoad = 2) and (Info.LevelOptions and $10 <> 0) then
    begin
      InteractiveObjects.Clear;
      Terrains.Clear;
      Steels.Clear;
      SetLength(Info.WindowOrder, 0);
    end;*)

    for i := 0 to InteractiveObjects.Count-1 do
      with InteractiveObjects[i] do
        GS := GSNames[StrToInt(GS)];

    for i := 0 to Terrains.Count-1 do
      with Terrains[i] do
        GS := GSNames[StrToInt(GS)];

  end; // with aLevel
end;


class procedure TLVLLoader.LoadTradLevelFromStream(aStream: TStream; aLevel: TLevel; OddLoad: Byte = 0);
{-------------------------------------------------------------------------------
  Translate a LVL file and fill the collections.
  For decoding and technical details see documentation or read the code :)
-------------------------------------------------------------------------------}
var
  Buf: TLVLRec;
  H, i: Integer;
  O: TLVLObject;
  T: TLVLTerrain;
  S: TLVLSteel;
  Obj: TInteractiveObject;
  Ter: TTerrain;
  Steel: TSteel;
  GraphicSet: Integer;
  SFinder: TStyleFinder;
begin
  with aLevel do
  begin

    aStream.ReadBuffer(Buf, LVL_SIZE);
    {-------------------------------------------------------------------------------
      Get the statics. This is easy
    -------------------------------------------------------------------------------}
    with Info do
    begin
      aLevel.Clear;
      ReleaseRate      := System.Swap(Buf.ReleaseRate) mod 256;
      LemmingsCount    := System.Swap(Buf.LemmingsCount);
      RescueCount      := System.Swap(Buf.RescueCount);
      TimeLimit        := (Buf.TimeMinutes * 60){ + Buf.TimeSeconds};
      Skillset := [spbClimber, spbFloater, spbBomber, spbBlocker, spbBuilder, spbBasher, spbMiner, spbDigger];
      SkillCount[spbClimber]     := System.Swap(Buf.ClimberCount) mod 256;
      SkillCount[spbFloater]     := System.Swap(Buf.FloaterCount) mod 256;
      SkillCount[spbBomber]      := System.Swap(Buf.BomberCount) mod 256;
      SkillCount[spbBlocker]     := System.Swap(Buf.BlockerCount) mod 256;
      SkillCount[spbBuilder]     := System.Swap(Buf.BuilderCount) mod 256;
      SkillCount[spbBasher]      := System.Swap(Buf.BasherCount) mod 256;
      SkillCount[spbMiner]       := System.Swap(Buf.MinerCount) mod 256;
      SkillCount[spbDigger]      := System.Swap(Buf.DiggerCount) mod 256;
      LevelOptions     := 0;
      Title            := Buf.LevelName;
      Author           := '';
      GraphicSet := System.Swap(Buf.GraphicSet);
      case (GraphicSet shr 8) and $FF of
        0: MusicFile := '';
      253: MusicFile := '*';
      254: MusicFile := 'frenzy';
      255: MusicFile := 'gimmick';
        else MusicFile := '?';
      end;
      ScreenPosition   := System.Swap(Buf.ScreenPosition) + 160;
      ScreenYPosition  := 0;
      Width := 1584;
      Height := 160;
      if ScreenPosition > (Width - 160) then ScreenPosition := (Width - 160);
      if ScreenPosition < 160 then ScreenPosition := 160;

      SFinder := TStyleFinder.Create;

      if SFinder.FindName(GraphicSet mod 256, false) <> '' then
        GraphicSetName := SFinder.FindName(GraphicSet mod 256, false);

      SFinder.Free;
    end;

    {-------------------------------------------------------------------------------
      Get the objects
    -------------------------------------------------------------------------------}
    for i := 0 to LVL_MAXOBJECTCOUNT - 1 do
    begin
      O := Buf.Objects[i];
      if O.AsInt64 = 0 then
        Continue;
      Obj := InteractiveObjects.Add;
      Obj.Left := (Integer(O.B0) shl 8 + Integer(O.B1) - 16) and not 7;
      Obj.Top := Integer(O.B2) shl 8 + Integer(O.B3);
      If Obj.Top > 32767 then Obj.Top := Obj.Top - 65536;
      Obj.GS := Info.GraphicSetName;
      Obj.Piece := 'O' + IntToStr(Integer(O.B5 and 31));
      //Obj.TarLev := (O.B4);
      if O.Modifier and $80 <> 0 then
        Obj.DrawingFlags := Obj.DrawingFlags or odf_NoOverwrite;
      if O.Modifier and $40 <> 0 then
        Obj.DrawingFlags := Obj.DrawingFlags or odf_OnlyOnTerrain;
      {if O.Modifier and $20 <> 0 then
        Obj.DrawingFlags := Obj.DrawingFlags or odf_FlipLem;}
      if O.DisplayMode = $8F then
        Obj.DrawingFlags := Obj.DrawingFlags or odf_UpsideDown;
      //Obj.Skill := O.Modifier and $0F;
      {if (O.Modifier and $10) <> 0 then Obj.IsFake := true;}
      if (O.ObjectID <> 1) and (i >= 16) then Obj.IsFake := true;

      Obj.LastDrawX := Obj.Left;
      Obj.LastDrawY := Obj.Top;
      Obj.DrawAsZombie := false;
    end;

    {-------------------------------------------------------------------------------
      Get the terrain.
    -------------------------------------------------------------------------------}
    for i := 0 to LVL_MAXTERRAINCOUNT - 1 do
    begin
      T := Buf.Terrain[i];
      if T.D0 = $FFFFFFFF then
        Continue;
      Ter := Terrains.Add;
      Ter.Left := Integer(T.B0 and 15) shl 8 + Integer(T.B1) - 16; // 9 bits
      Ter.DrawingFlags := T.B0 shr 5; // the bits are compatible
      H := Integer(T.B2) shl 1 + Integer(T.B3 and $80) shr 7;
      if H >= 256 then
        Dec(H, 512);
      Dec(H, 4);
      Ter.Top := H;
      Ter.GS := Info.GraphicSetName;
      if T.B0 and 16 <> 0 then
        Ter.Piece := 'T' + IntToStr((T.B3 and 63) + 64)
      else
        Ter.Piece := 'T' + IntToStr(T.B3 and 63);
    end;

    {-------------------------------------------------------------------------------
      Get the steel.
    -------------------------------------------------------------------------------}
    for i := 0 to LVL_MAXSTEELCOUNT - 1 do
    begin
      S := Buf.Steel[i];
      //if S.D0 = 0 then
      //  Continue;
      Steel := Steels.Add;  
      Steel.Left := ((Integer(S.B0) shl 1) + (Integer(S.B1 and (1 shl 7)) shr 7)) * 4 - 16;  // 9 bits
      Steel.Top := Integer(S.B1 and not (1 shl 7)) * 4;  // bit 7 belongs to steelx
      Steel.Width := Integer(S.B2 shr 4) * 4 + 4;  // first nibble bits 4..7 is width in units of 4 pixels (and then add 4)
      Steel.Height := Integer(S.B2 and $F) * 4 + 4;  // second nibble bits 0..3 is height in units of 4 pixels (and then add 4)
      {Steel.Left := Steel.Left - (Integer(S.B3 shr 6) mod 4);
      Steel.Top := Steel.Top - (Integer(S.B3 shr 4) mod 4);
      Steel.Width := Steel.Width - (Integer(S.B3 shr 2) mod 4);
      Steel.Height := Steel.Height - (Integer(S.B3) mod 4);
      if (i >= 16) and (aLevel.Info.LevelOptions and 1 <> 0) then Steel.fType := 1 else} Steel.fType := 0;
    end;

  end; // with aLevel
end;


class procedure TLVLLoader.UpgradeFormat(var Buf: TNeoLVLRec);
var
  i: Integer;
begin
  while Buf.FormatTag < 3 do
  begin
    case Buf.FormatTag of
      1: begin
           Buf.FormatTag := 2;
           Buf.MusicNumber := Buf.ScreenPosition mod 256;
           Buf.ScreenPosition := Buf.ScreenYPosition;
           Buf.ScreenYPosition := 0;
         end;
      2: begin
           Buf.StyleName := '                ';
           Buf.VgaspecName := '                ';
           for i := 0 to 31 do
             Buf.WindowOrder[i] := 0;
           for i := 0 to 63 do
             Buf.Objects[i].AsInt64 := Buf.Objects[i * 2].AsInt64;
           for i := 64 to 127 do
             Buf.Objects[i].AsInt64 := 0;
           Buf.FormatTag := 3;
         end;
    end;
  end;
end;


class procedure TLVLLoader.LoadNeoLevelFromStream(aStream: TStream; aLevel: TLevel; OddLoad: Byte = 0);
{-------------------------------------------------------------------------------
  Translate a LVL file and fill the collections.
  For decoding and technical details see documentation or read the code :)
-------------------------------------------------------------------------------}
var
  Buf: TNeoLVLRec;
  i, x, x2: Integer;
  O: TNeoLVLObject;
  T: TNeoLVLTerrain;
  S: TNeoLVLSteel;
  Obj: TInteractiveObject;
  Ter: TTerrain;
  Steel: TSteel;
  SFinder: TStyleFinder;
  TempWindowOrder: Array[0..31] of Byte;

  procedure AddSkill(aSkill: TSkillPanelButton);
  begin
    if x2 < 8 then
      aLevel.Info.Skillset := aLevel.Info.Skillset + [aSkill];
    Inc(x2);
  end;

  procedure SetSkillCount(aSkill: TSkillPanelButton; aCount: Integer);
  begin
    if not(aSkill in aLevel.Info.Skillset) then Exit;
    aLevel.Info.SkillCount[aSkill] := aCount;
  end;
begin
  with aLevel do
  begin

    aStream.ReadBuffer(Buf, NEO_LVL_SIZE);
    UpgradeFormat(Buf);
    {-------------------------------------------------------------------------------
      Get the statics. This is easy
    -------------------------------------------------------------------------------}
    with Info do
    begin
      if OddLoad <> 1 then
      begin
      aLevel.Clear;
      ReleaseRate      := Buf.ReleaseRate;
      LemmingsCount    := Buf.LemmingsCount;
      RescueCount      := Buf.RescueCount;
      TimeLimit        := Buf.TimeLimit; // internal structure now matches NeoLemmix file format structure (just a number of seconds)

        x2 := 0;
        if Buf.Skillset and $8000 <> 0 then AddSkill(spbWalker);
        if Buf.Skillset and $4000 <> 0 then AddSkill(spbClimber);
        if Buf.Skillset and $2000 <> 0 then AddSkill(spbSwimmer);
        if Buf.Skillset and $1000 <> 0 then AddSkill(spbFloater);
        if Buf.Skillset and $800 <> 0 then AddSkill(spbGlider);
        if Buf.Skillset and $400 <> 0 then AddSkill(spbDisarmer);
        if Buf.Skillset and $200 <> 0 then AddSkill(spbBomber);
        if Buf.Skillset and $100 <> 0 then AddSkill(spbStoner);
        if Buf.Skillset and $80 <> 0 then AddSkill(spbBlocker);
        if Buf.Skillset and $40 <> 0 then AddSkill(spbPlatformer);
        if Buf.Skillset and $20 <> 0 then AddSkill(spbBuilder);
        if Buf.Skillset and $10 <> 0 then AddSkill(spbStacker);
        if Buf.Skillset and $8 <> 0 then AddSkill(spbBasher);
        if Buf.Skillset and $4 <> 0 then AddSkill(spbMiner);
        if Buf.Skillset and $2 <> 0 then AddSkill(spbDigger);
        if Buf.Skillset and $1 <> 0 then AddSkill(spbCloner);

        SetSkillCount(spbWalker, Buf.WalkerCount);
        SetSkillCount(spbClimber, Buf.ClimberCount);
        SetSkillCount(spbSwimmer, Buf.SwimmerCount);
        SetSkillCount(spbFloater, Buf.FloaterCount);
        SetSkillCount(spbGlider, Buf.GliderCount);
        SetSkillCount(spbDisarmer, Buf.MechanicCount);
        SetSkillCount(spbBomber, Buf.BomberCount);
        SetSkillCount(spbStoner, Buf.StonerCount);
        SetSkillCount(spbBlocker, Buf.BlockerCount);
        SetSkillCount(spbPlatformer, Buf.PlatformerCount);
        SetSkillCount(spbBuilder, Buf.BuilderCount);
        SetSkillCount(spbStacker, Buf.StackerCount);
        SetSkillCount(spbBasher, Buf.BasherCount);
        SetSkillCount(spbMiner, Buf.MinerCount);
        SetSkillCount(spbDigger, Buf.DiggerCount);
        SetSkillCount(spbCloner, Buf.ClonerCount);

      Title            := Buf.LevelName;
      Author           := Buf.LevelAuthor;
      case Buf.MusicNumber of
        0: MusicFile := '';
      253: MusicFile := '*';
      254: MusicFile := 'frenzy';
      255: MusicFile := 'gimmick';
        else MusicFile := '?';
      end;
      end;
      if (OddLoad = 2) and (Buf.LevelOptions and 16 <> 0) then
      begin
        LevelOptions := $71;
        Exit;
      end else begin
      Width := Buf.WidthAdjust + 1584;
      Height := Buf.HeightAdjust + 160;
      if Width < 320 then Width := 320;
      if Height < 160 then Height := 160;
      ScreenPosition   := Buf.ScreenPosition + 160;
      if ScreenPosition > (Width - 160) then ScreenPosition := (Width - 160);
      if ScreenPosition < 160 then ScreenPosition := 160;
      ScreenYPosition := Buf.ScreenYPosition + 80;
      if ScreenYPosition > (Height - 80) then ScreenYPosition := (Height - 80);
      if ScreenYPosition < 80 then ScreenYPosition := 80;

      SFinder := TStyleFinder.Create;

      if (Trim(Buf.StyleName) <> '') or (Trim(Buf.VgaspecName) <> '') then
      begin
        x := SFinder.FindNumber(Buf.StyleName, false);
        if (x <> -1) and (Buf.GraphicSet <> 255) then Buf.GraphicSet := x;
        x := SFinder.FindNumber(Buf.VgaspecName, true);
        if (x <> -1) and (Buf.GraphicSetEx <> 255) then Buf.GraphicSetEx := x;
      end;

      if trim(Buf.StyleName) <> '' then
      begin
        //GraphicSetFile := 'v_' + trim(Buf.StyleName) + '.dat';
        //GraphicMetaFile := 'g_' + trim(Buf.StyleName) + '.dat';
        if LowerCase(LeftStr(Buf.StyleName, 5)) = 'vgagr' then
          GraphicSetName := SFinder.FindName(StrToInt(Trim(MidStr(Buf.StyleName, 6, 3))), false)
        else
          GraphicSetName := trim(Buf.StyleName);
        Buf.GraphicSet := 255;
      end else begin
        GraphicSetName := SFinder.FindName(Buf.GraphicSet, false);
      end;

      SFinder.Free;

      LevelOptions := Buf.LevelOptions;

      for x := 0 to 31 do
        TempWindowOrder[x] := Buf.WindowOrder[x];
      end;
      if LevelOptions and $2 = 0 then
        LevelOptions := LevelOptions and $F7;
    end;

    if (OddLoad = 2) and (Info.LevelOptions and 16 <> 0) then Exit;

    {-------------------------------------------------------------------------------
      Get the objects
    -------------------------------------------------------------------------------}
    for i := 0 to 127 do
    begin
      O := Buf.Objects[i];
      if O.ObjectFlags and 128 = 0 then
      begin
        {Obj := InteractiveObjects.Add;
        Obj.Left := -32768;
        Obj.Top := -32768;
        Obj.Identifier := 0;}
        for x := i to 127 do
          for x2 := 0 to 31 do
            if ((Buf.WindowOrder[x2] and $80) <> 0) and ((Buf.WindowOrder[x2] and $7F) = x) then
              Buf.WindowOrder[x2] := Buf.WindowOrder[x2] - 1;
        Continue;
      end;
      Obj := InteractiveObjects.Add;
      Obj.Left := O.XPos;
      Obj.Top := O.YPos;
      Obj.GS := Info.GraphicSetName;
      Obj.Piece := 'O' + IntToStr(O.ObjectID);
      Obj.TarLev := O.LValue;
      if O.ObjectFlags and $1 <> 0 then
        Obj.DrawingFlags := Obj.DrawingFlags or odf_NoOverwrite;
      if O.ObjectFlags and $2 <> 0 then
        Obj.DrawingFlags := Obj.DrawingFlags or odf_OnlyOnTerrain;
      if O.ObjectFlags and $4 <> 0 then
        Obj.DrawingFlags := Obj.DrawingFlags or odf_UpsideDown;
      if O.ObjectFlags and $8 <> 0 then
        Obj.DrawingFlags := Obj.DrawingFlags or odf_FlipLem;
      if O.ObjectFlags and $10 <> 0 then
        Obj.IsFake := true;
      if O.ObjectFlags and $20 <> 0 then
        Obj.DrawingFlags := Obj.DrawingFlags or odf_Invisible;
      if O.ObjectFlags and $40 <> 0 then
        Obj.DrawingFlags := Obj.DrawingFlags or odf_Flip;
      Obj.Skill := O.SValue mod 16;

      Obj.LastDrawX := Obj.Left;
      Obj.LastDrawY := Obj.Top;
      Obj.DrawAsZombie := false;
    end;

    {-------------------------------------------------------------------------------
      Get the terrain.
    -------------------------------------------------------------------------------}
    for i := 0 to 999 do
    begin
      T := Buf.Terrain[i];
      if T.TerrainFlags and 128 = 0 then
        Continue;
      Ter := Terrains.Add;
      Ter.Left := T.XPos;
      Ter.Top := T.YPos;
      Ter.GS := Info.GraphicSetName;
      Ter.Piece := 'T' + IntToStr(T.TerrainID);
      if T.TerrainFlags and $1 <> 0 then
        Ter.DrawingFlags := Ter.DrawingFlags or tdf_NoOverwrite;
      if T.TerrainFlags and $2 <> 0 then
        Ter.DrawingFlags := Ter.DrawingFlags or tdf_Erase;
      if T.TerrainFlags and $4 <> 0 then
        Ter.DrawingFlags := Ter.DrawingFlags or tdf_Invert;
      if T.TerrainFlags and $8 <> 0 then
        Ter.DrawingFlags := Ter.DrawingFlags or tdf_Flip;
      if Info.LevelOptions and $80 = 0 then
      begin
        if T.TerrainFlags and $10 <> 0 then
          Ter.DrawingFlags := Ter.DrawingFlags or tdf_NoOneWay;
      end else begin
        if T.TerrainFlags and $10 = 0 then
          Ter.DrawingFlags := Ter.DrawingFlags or tdf_NoOneWay;
      end;
    end;

    {-------------------------------------------------------------------------------
      Get the steel.
    -------------------------------------------------------------------------------}
    for i := 0 to 127 do
    begin
      S := Buf.Steel[i];
      if S.SteelFlags and 128 = 0 then
        Continue;
      Steel := Steels.Add;
      Steel.Left := S.XPos;
      Steel.Top := S.YPos;
      Steel.Width := S.SteelWidth + 1;
      Steel.Height := S.SteelHeight + 1;
      Steel.fType := S.SteelFlags and not $80;
    end;

    SetLength(Info.WindowOrder, 0);
    with Info do
      for i := 0 to 31 do
        if (TempWindowOrder[i] and $80) <> 0 then
        begin
          SetLength(WindowOrder, Length(WindowOrder) + 1);
          WindowOrder[Length(WindowOrder) - 1] := TempWindowOrder[i] and $7F;
        end;

  end; // with aLevel
end;



class procedure TLVLLoader.StoreLevelInStream(aLevel: TLevel; aStream: TStream; aComment: String = '');
var
  //Int16: SmallInt; Int32: Integer;
  {H,} i: Integer;
  //M: Byte;
  W: Word;
  O: TNewNeoLVLObject;
  T: TNewNeoLVLTerrain;
  S: TNewNeoLVLSteel;
  Obj: TInteractiveObject;
  Lem: TPreplacedLemming;
  Ter: TTerrain;
  Steel: TSteel;
  Buf: TNeoLVLHeader;
  Buf2: TNeoLVLSecondHeader;
  k: ShortString;
  //SFinder: TStyleFinder;
  StyleList: array of String;

  b: Byte;
  //w: Word;

  PPStyle, PPIndex: Integer;

  function GetStyleID(aLabel: String): Integer;
  var
    i: Integer;
  begin
    for i := 0 to Length(StyleList)-1 do
      if Lowercase(aLabel) = Lowercase(StyleList[i]) then
      begin
        Result := i;
        Exit;
      end;
    SetLength(StyleList, Length(StyleList)+1);
    StyleList[Length(StyleList)-1] := aLabel;
    Result := Length(StyleList)-1;
  end;
begin


  with aLevel do
  begin
    FillChar(Buf, SizeOf(Buf), 0);
    FillChar(Buf.LevelName, 32, ' ');
    FillChar(Buf.LevelAuthor, 16, ' ');
    FillChar(Buf.StyleName, 16, ' ');
    FillChar(Buf.VgaspecName, 16, ' ');
    FillChar(Buf.ReservedStr, 32, ' ');

    {-------------------------------------------------------------------------------
      Set the statics.
    -------------------------------------------------------------------------------}
    with Info do
    begin

      Buf.FormatTag     := 4;
      Buf.ReleaseRate   := ReleaseRate;
      Buf.LemmingsCount := LemmingsCount;
      Buf.RescueCount   := RescueCount;
      Buf.TimeLimit     := TimeLimit;

      Buf.Skillset := 0;
      if spbWalker in Skillset then Buf.Skillset := Buf.Skillset or $8000;
      if spbClimber in Skillset then Buf.Skillset := Buf.Skillset or $4000;
      if spbSwimmer in Skillset then Buf.Skillset := Buf.Skillset or $2000;
      if spbFloater in Skillset then Buf.Skillset := Buf.Skillset or $1000;
      if spbGlider in Skillset then Buf.Skillset := Buf.Skillset or $800;
      if spbDisarmer in Skillset then Buf.Skillset := Buf.Skillset or $400;
      if spbBomber in Skillset then Buf.Skillset := Buf.Skillset or $200;
      if spbStoner in Skillset then Buf.Skillset := Buf.Skillset or $100;
      if spbBlocker in Skillset then Buf.Skillset := Buf.Skillset or $80;
      if spbPlatformer in Skillset then Buf.Skillset := Buf.Skillset or $40;
      if spbBuilder in Skillset then Buf.Skillset := Buf.Skillset or $20;
      if spbStacker in Skillset then Buf.Skillset := Buf.Skillset or $10;
      if spbBasher in Skillset then Buf.Skillset := Buf.Skillset or $8;
      if spbMiner in Skillset then Buf.Skillset := Buf.Skillset or $4;
      if spbDigger in Skillset then Buf.Skillset := Buf.Skillset or $2;
      if spbCloner in Skillset then Buf.Skillset := Buf.Skillset or $1;

      if (Buf.Skillset and $8000 <> 0) then Buf.WalkerCount := SkillCount[spbWalker];
      if (Buf.Skillset and $4000 <> 0) then Buf.ClimberCount := SkillCount[spbClimber];
      if (Buf.Skillset and $2000 <> 0) then Buf.SwimmerCount := SkillCount[spbSwimmer];
      if (Buf.Skillset and $1000 <> 0) then Buf.FloaterCount := SkillCount[spbFloater];
      if (Buf.Skillset and $0800 <> 0) then Buf.GliderCount := SkillCount[spbGlider];
      if (Buf.Skillset and $0400 <> 0) then Buf.MechanicCount := SkillCount[spbDisarmer];
      if (Buf.Skillset and $0200 <> 0) then Buf.BomberCount := SkillCount[spbBomber];
      if (Buf.Skillset and $0100 <> 0) then Buf.StonerCount := SkillCount[spbStoner];
      if (Buf.Skillset and $0080 <> 0) then Buf.BlockerCount := SkillCount[spbBlocker];
      if (Buf.Skillset and $0040 <> 0) then Buf.PlatformerCount := SkillCount[spbPlatformer];
      if (Buf.Skillset and $0020 <> 0) then Buf.BuilderCount := SkillCount[spbBuilder];
      if (Buf.Skillset and $0010 <> 0) then Buf.StackerCount := SkillCount[spbStacker];
      if (Buf.Skillset and $0008 <> 0) then Buf.BasherCount := SkillCount[spbBasher];
      if (Buf.Skillset and $0004 <> 0) then Buf.MinerCount := SkillCount[spbMiner];
      if (Buf.Skillset and $0002 <> 0) then Buf.DiggerCount := SkillCount[spbDigger];
      if (Buf.Skillset and $0001 <> 0) then Buf.ClonerCount := SkillCount[spbCloner];

      Buf.LevelOptions := LevelOptions;

      if ReleaseRateLocked then
        Buf.LevelOptions2 := Buf.LevelOptions2 or 1;

      if spbFencer in Skillset then
      begin
        Buf.LevelOptions2 := Buf.LevelOptions2 or 2;
        Buf.FencerCount := SkillCount[spbFencer];
      end;

      //Buf.GraphicSetEx := GraphicSetEx;
      //Buf.GraphicSet   := GraphicSet;
      Buf.MusicNumber  := 0;
      Buf.ScreenPosition := ScreenPosition;
      Buf.ScreenYPosition := ScreenYPosition;

      Buf.Resolution := 8;
      k := Background;
      k := MidStr(k, Pos('BG', k) + 2, Length(k));
      Buf.BgIndex := StrToIntDef(k, 0);

      Buf.Width := Width;
      Buf.Height := Height;

      Buf.Gimmick := 0;

      if Length(Title) > 0 then
         System.Move(Title[1], Buf.LevelName, Length(Title));

      if Length(Author) > 0 then
         System.Move(Author[1], Buf.LevelAuthor, Length(Author));

      {SFinder := TStyleFinder.Create;
      k := SFinder.FindName(Buf.GraphicSet, false);
      if k <> '' then System.Move(k[1], Buf.StyleName, Length(k));
      k := SFinder.FindName(Buf.GraphicSetEx, true);
      if k <> '' then System.Move(k[1], Buf.VgaspecName, Length(k));
      SFinder.Free;}

      k := GraphicSetName;
      System.Move(k[1], Buf.StyleName, Length(k));

      k := 'none';
      System.Move(k[1], Buf.VgaspecName, Length(k));

      k := LeftStr(aComment, 32);
      System.Move(k[1], Buf.ReservedStr, Length(k));

      Buf.RefSection := 0;
      Buf.RefLevel   := 0;

      Buf.VgaspecX := 0;
      Buf.VgaspecY := 0;

      Buf.LevelID := LevelID;
    end;

    aStream.Write(Buf, SizeOf(Buf));

    {-------------------------------------------------------------------------------
      Set the objects.
    -------------------------------------------------------------------------------}
    for i := 0 to (InteractiveObjects.Count - 1) do
    begin
      Obj := InteractiveObjects[i];

      //if (Obj.Left = -32768) and (Obj.Top = -32768) then Continue;

      FillChar(O, Sizeof(O), 0);

      O.XPos := Obj.Left;
      O.YPos := Obj.Top;
      O.ObjectID := StrToIntDef(Obj.Piece, 0);
      O.LValue := Obj.TarLev;
      O.SValue := Obj.Skill;

      O.ObjectFlags := $80;

      if Obj.DrawingFlags and odf_NoOverwrite <> 0 then
        O.ObjectFlags := O.ObjectFlags or $1;
      if Obj.DrawingFlags and odf_OnlyOnTerrain <> 0 then
        O.ObjectFlags := O.ObjectFlags or $2;
      if Obj.DrawingFlags and odf_UpsideDown <> 0 then
        O.ObjectFlags := O.ObjectFlags or $4;
      if Obj.DrawingFlags and odf_FlipLem <> 0 then
        O.ObjectFlags := O.ObjectFlags or $8;
      if Obj.IsFake then
        O.ObjectFlags := O.ObjectFlags or $10;
      if Obj.DrawingFlags and odf_Invisible <> 0 then
        O.ObjectFlags := O.ObjectFlags or $20;
      if Obj.DrawingFlags and odf_Flip <> 0 then
        O.ObjectFlags := O.ObjectFlags or $40;
      if Obj.DrawingFlags and odf_Rotate <> 0 then
        O.ObjectFlags := O.ObjectFlags or $100;

      O.GSIndex := GetStyleID(Obj.GS);

      b := 1;
      aStream.Write(b, 1);
      aStream.Write(O, SizeOf(O));
    end;

    {-------------------------------------------------------------------------------
      set the preplaced lemmings.
    -------------------------------------------------------------------------------}

    PPStyle := GetStyleID('dirt');
    PPIndex := 11;
    // offset is 2, 9
    for i := 0 to PreplacedLemmings.Count-1 do
    begin
      FillChar(O, Sizeof(O), 0);

      Lem := PreplacedLemmings[i];

      O.XPos := Lem.X - 2;
      O.YPos := Lem.Y - 9;
      O.ObjectID := PPIndex;

      if Lem.IsClimber then O.LValue := O.LValue or 1;
      if Lem.IsSwimmer then O.LValue := O.LValue or 2;
      if Lem.IsFloater then O.LValue := O.LValue or 4;
      if Lem.IsGlider then O.LValue := O.LValue or 8;
      if Lem.IsDisarmer then O.LValue := O.LValue or 16;
      if Lem.IsBlocker then O.LValue := O.LValue or 32;
      if Lem.IsZombie then O.LValue := O.LValue or 64;

      O.ObjectFlags := $80;

      if Lem.Dx < 0 then
        O.ObjectFlags := O.ObjectFlags or $8;

      O.GSIndex := PPStyle;

      b := 1;
      aStream.Write(b, 1);
      aStream.Write(O, SizeOf(O));
    end;

    {-------------------------------------------------------------------------------
      set the terrain
    -------------------------------------------------------------------------------}
    for i := 0 to Terrains.Count - 1 do
    begin
      Ter := Terrains[i];

      FillChar(T, Sizeof(T), 0);

      T.XPos := Ter.Left;
      T.YPos := Ter.Top;
      T.TerrainID := StrToIntDef(Ter.Piece, 0);

      T.TerrainFlags := $80;

      if Ter.DrawingFlags and tdf_NoOverwrite <> 0 then
        T.TerrainFlags := T.TerrainFlags or $1;
      if Ter.DrawingFlags and tdf_Erase <> 0 then
        T.TerrainFlags := T.TerrainFlags or $2;
      if Ter.DrawingFlags and tdf_Invert <> 0 then
        T.TerrainFlags := T.TerrainFlags or $4;
      if Ter.DrawingFlags and tdf_Flip <> 0 then
        T.TerrainFlags := T.TerrainFlags or $8;
      if Info.LevelOptions and $80 = 0 then
      begin
        if Ter.DrawingFlags and tdf_NoOneWay <> 0 then
          T.TerrainFlags := T.TerrainFlags or $10;
      end else begin
        if Ter.DrawingFlags and tdf_NoOneWay = 0 then
          T.TerrainFlags := T.TerrainFlags or $10;
      end;
      if Ter.DrawingFlags and tdf_Rotate <> 0 then
        T.TerrainFlags := T.TerrainFlags or $20;

      T.GSIndex := GetStyleID(Ter.GS);

      b := 2;
      aStream.Write(b, 1);
      aStream.Write(T, SizeOf(T));
    end;

    {-------------------------------------------------------------------------------
      set the steel.
    -------------------------------------------------------------------------------}
  if (Info.LevelOptions and 4) = 0 then
    for i := 0 to Steels.Count - 1 do
    begin
      Steel := Steels[i];
      FillChar(S, SizeOf(S), 0);

      S.XPos := Steel.Left;
      S.YPos := Steel.Top;
      S.SteelWidth := Steel.Width - 1;
      S.SteelHeight := Steel.Height - 1;

      S.SteelFlags := Steel.fType or $80;
      b := 3;
      aStream.Write(b, 1);
      aStream.Write(s, SizeOf(S));
    end;
  {end;}

    if Length(Info.WindowOrder) > 0 then
    begin
      b := 4;
      aStream.Write(b, 1);
      for i := 0 to Length(Info.WindowOrder)-1 do
      begin
        w := Info.WindowOrder[i];
        aStream.Write(w, 2);
      end;
      w := $FFFF;
      aStream.Write(w, 2);
    end;

    b := 5;
    aStream.Write(b, 1);
    FillChar(Buf2, SizeOf(Buf2), 0);
    FillChar(Buf2.MusicName, 16, 0);
    Buf2.ScreenPosition := Info.ScreenPosition;
    Buf2.ScreenYPosition := Info.ScreenYPosition;
    k := LowerCase(LeftStr(Info.MusicFile, 16));
    Move(k[1], Buf2.MusicName, Length(k));
    aStream.Write(Buf2, SizeOf(Buf2));

    b := 6;
    aStream.Write(b, 1);
    w := Length(StyleList);
    aStream.Write(w, 2);
    for i := 0 to Length(StyleList)-1 do
    begin
      k := LeftStr(StyleList[i], 16);
      while Length(k) < 16 do
        k := k + ' ';
      aStream.Write(k[1], 16);
    end;

    //aStream.WriteBuffer(Buf, NEO_LVL_SIZE);
    b := 0;
    aStream.Write(b, 1);

  end;
end;

end.

