unit LemBCGraphicSet;

// Backwards compatibility for graphic sets.

// TODO:
//  - make use of bgcolor etc from experimental GSTool sets
//  - support for custom sounds

interface

uses
  Dialogs, LemNeoOnline, LemDosBmp,
  LemDosCmp, GR32, LemTypes, Classes, SysUtils;

const
  dtSecMarker = $FF;

  dtEof = $00;      //not strictly required
  dtComment = $01;
  dtHeader = $02;
  dtObject = $03;
  dtTerrain = $04;
  dtSound = $05;
  dtLemming = $06;

type
  TNeoLemmixSoundData = packed record
    SoundID: Byte;
    SoundLoc: LongWord;
  end;

  TNeoLemmixColorEntry = packed record
    case Byte of
      0: (A, R, G, B: Byte); // don't think this structure is ever needed, but nice to have just in case
      1: (ARGB: TColor32);
  end;

  TNeoLemmixHeader = packed record
    VersionNumber: Byte;
    Resolution: Byte;
    IsUpdated: Byte;
    Reserved: Array[0..12] of Byte;
    KeyColors: Array[0..7] of TNeoLemmixColorEntry;
  end;

  TNeoLemmixObjectData = packed record
    ObjectFlags: Word;
    FrameCount: Word;
    PreviewFrame: Word;
    KeyFrame: Word;
    BaseLoc: LongWord;
    TriggerEff: Byte;
    TriggerSound: Byte;
    PTriggerX: SmallInt;
    PTriggerY: SmallInt;
    PTriggerW: SmallInt;
    PTriggerH: SmallInt;
    Reserved: Array[0..1] of Byte;
    STriggerX: SmallInt;
    STriggerY: SmallInt;
    STriggerW: SmallInt;
    STriggerH: SmallInt;
    Reserved2: Array[0..7] of Byte;
  end;
  TObjectDataArray = array of TNeoLemmixObjectData;

  TNeoLemmixTerrainData = packed record
    TerrainFlags: Word;
    BaseLoc: LongWord;
    Reserved: Array[0..9] of Byte;
  end;
  TTerrainDataArray = array of TNeoLemmixTerrainData;

  TBcGraphicSet = class
    private
      fSpecialBitmap: TBitmap32;
      fDataStream: TMemoryStream;
      fName: String;
      fResolution: Integer;
      fMaskColor: TColor32;
      fMinimapColor: TColor32;
      fBackgroundColor: TColor32;
      fLemmings: String;
      fObjectCount: Integer;
      fTerrainCount: Integer;
      fObjectData: TObjectDataArray;
      fTerrainData: TTerrainDataArray;
      fSoundPositions: array[0..255] of Integer;
      procedure EnsureObjectLength;
      procedure EnsureTerrainLength;
      function Acquire(aName: String): Boolean;
      function GetSoundPosition(aIndex: Integer): Integer;
      procedure LoadSpecialBitmap(aStream: TStream);
    public
      constructor Create;
      destructor Destroy; override;
      procedure LoadGraphicSet(aName: String);
      function LoadBackground(aDst: TBitmap32; aIndex: Integer): Boolean;
      property DataStream: TMemoryStream read fDataStream;
      property Name: String read fName;
      property Resolution: Integer read fResolution;
      property MaskColor: TColor32 read fMaskColor;
      property MinimapColor: TColor32 read fMinimapColor;
      property BackgroundColor: TColor32 read fBackgroundColor;
      property Lemmings: String read fLemmings;
      property ObjectCount: Integer read fObjectCount;
      property TerrainCount: Integer read fTerrainCount;
      property ObjectData: TObjectDataArray read fObjectData;
      property TerrainData: TTerrainDataArray read fTerrainData;
      property SoundPosition[aIndex: Integer]: Integer read GetSoundPosition;
      property SpecialBitmap: TBitmap32 read fSpecialBitmap;
  end;

  procedure LoadNeoLemmixImage(aStream: TStream; aBmp: TBitmap32; aResolution: Integer = 1);

var
  PathOverride: String;

implementation

procedure LoadNeoLemmixImage(aStream: TStream; aBmp: TBitmap32; aResolution: Integer = 1);
var
  x, y, w, h: LongWord;
  c: TColor32;
  a, r, g, b: Byte;
  TempBMP: TBitmap32;
begin
  TempBMP := TBitmap32.Create;
  try
    aStream.Read(w, 4);
    aStream.Read(h, 4);
    TempBmp.SetSize(w, h);
    TempBmp.Clear(0);
    for y := 0 to h-1 do
      for x := 0 to w-1 do
      begin
        aStream.Read(a, 1);
        if a <> 0 then
        begin
          aStream.Read(r, 1);
          aStream.Read(g, 1);
          aStream.Read(b, 1);
        end else begin
          r := 0;
          g := 0;
          b := 0;
        end;

        c := (a shl 24) + (r shl 16) + (g shl 8) + b;
        TempBmp.Pixel[x, y] := c;
      end;

    if aResolution = 1 then
      aBmp.Assign(TempBMP)
    else begin
      aBmp.SetSize(TempBMP.Width div aResolution, TempBMP.Height div aResolution);
      TempBMP.DrawTo(aBmp, aBmp.BoundsRect, TempBmp.BoundsRect);
    end;
  finally
    TempBMP.Free;
  end;
end;

constructor TBcGraphicSet.Create;
begin
  inherited;
  SetLength(fObjectData, 20);
  SetLength(fTerrainData, 60);
  fDataStream := TMemoryStream.Create;
  fSpecialBitmap := nil;
end;

destructor TBcGraphicSet.Destroy;
begin
  fDataStream.Free;
  fSpecialBitmap.Free;
  inherited;
end;

function TBcGraphicSet.LoadBackground(aDst: TBitmap32; aIndex: Integer): Boolean;
var
  i: Integer;
begin
  Result := false;
  aDst.Clear(fBackgroundColor or $FF000000);
  for i := 0 to fObjectCount-1 do
  begin
    if fObjectData[i].TriggerEff <> 32 then Continue;
    if aIndex = 0 then
    begin
      fDataStream.Position := fObjectData[i].BaseLoc;
      LoadNeoLemmixImage(fDataStream, aDst, fResolution);
      Result := true;
      Exit;
    end else
      Dec(aIndex);
  end;
end;

procedure TBcGraphicSet.EnsureObjectLength;
begin
  if Length(fObjectData) = fObjectCount then
    SetLength(fObjectData, fObjectCount + 20);
end;

procedure TBcGraphicSet.EnsureTerrainLength;
begin
  if Length(fTerrainData) = fTerrainCount then
    SetLength(fTerrainData, fTerrainCount + 60);
end;

procedure TBcGraphicSet.LoadSpecialBitmap(aStream: TStream);
var
  Vgaspec: TVgaspecBitmap;
begin
  Vgaspec := TVgaspecBitmap.Create;
  try
    fSpecialBitmap := TBitmap32.Create;
    aStream.Position := 0;
    Vgaspec.LoadFromStream(aStream, fSpecialBitmap);
  finally
    Vgaspec.Free;
  end;
end;

procedure TBcGraphicSet.LoadGraphicSet(aName: String);
var
  Decompressor: TDosDatDecompressor;
  CmpStream, MetaStream: TMemoryStream;

  Header: TNeoLemmixHeader;
  SoundRec: TNeoLemmixSoundData;

  w: Word;
  s: String;

  procedure ClearSoundLocations;
  var
    i: Integer;
  begin
    for i := 0 to 255 do
      fSoundPositions[i] := -1;
  end;

  function ReadString(aStream: TStream): String;
  var
    c: Char;
  begin
    Result := '';
    aStream.Read(c, 1);
    while c <> #0 do
    begin
      Result := Result + c;
      aStream.Read(c, 1);
    end;
  end;

  function ApplyResolutionPatch(aObject: TNeoLemmixObjectData): TNeoLemmixObjectData;
  begin
    Result := aObject;
    if fResolution = 1 then Exit;
    with Result do
    begin
      PTriggerX := PTriggerX div fResolution;
      PTriggerY := PTriggerY div fResolution;
      PTriggerW := PTriggerW div fResolution;
      PTriggerH := PTriggerH div fResolution;
      //STrigger is only used by no-longer-supported object types (eg. single-object teleporter)
    end;
  end;
begin
  // Note: Should add compatibility with the experimental GSTool version's DAT files!

  CmpStream := CreateDataStream(aName + '.dat', ldtStyle);
  if CmpStream = nil then
  begin
    if Acquire(aName) then
      CmpStream := CreateDataStream(aName + '.dat', ldtStyle)
    else
      raise Exception.Create('The graphic set "' + aName + '.dat" could not be found.');
  end;

  MetaStream := TMemoryStream.Create;
  Decompressor := TDosDatDecompressor.Create;

  fName := aName;

  ClearSoundLocations;

  try
    try
      CmpStream.Position := 0;
      MetaStream.Position := 0;
      fDataStream.Position := 0;
      Decompressor.DecompressSection(CmpStream, MetaStream);

      if CmpStream.Position = CmpStream.Size then
      begin
        // In this case, we're dealing with an old-format VGASPEC
        LoadSpecialBitmap(CmpStream);
        Exit;
      end;

      fLemmings := 'lemming';

      Decompressor.DecompressSection(CmpStream, fDataStream);

      MetaStream.Position := 0;

      MetaStream.Read(w, 2);
      while w <> $00FF do
      begin
        case w of
          $01FF: begin // comment
                   repeat
                     MetaStream.Read(w, 2);
                     MetaStream.Position := MetaStream.Position - 1;
                   until w = $02FF;
                   MetaStream.Position := MetaStream.Position - 1;
                 end;
          $02FF: begin // header
                   MetaStream.Read(Header, SizeOf(TNeoLemmixHeader));
                   fResolution := Header.Resolution div 8;
                   if fResolution = 0 then fResolution := 1;
                   if Header.IsUpdated = 1 then
                   begin
                     fMaskColor := Header.KeyColors[0].ARGB;
                     fMinimapColor := Header.KeyColors[1].ARGB;
                     fBackgroundColor := Header.KeyColors[2].ARGB;
                   end else begin
                     fMaskColor := Header.KeyColors[0].ARGB;
                     fMinimapColor := fMaskColor;
                     fBackgroundColor := $FF000000;
                   end;
                 end;
          $03FF: begin // object data
                   Inc(fObjectCount);
                   EnsureObjectLength;
                   MetaStream.Read(fObjectData[fObjectCount-1], SizeOf(TNeoLemmixObjectData));
                   fObjectData[fObjectCount-1] := ApplyResolutionPatch(fObjectData[fObjectCount-1]);
                 end;
          $04FF: begin // terrain data
                   Inc(fTerrainCount);
                   EnsureTerrainLength;
                   MetaStream.Read(fTerrainData[fTerrainCount-1], SizeOf(TNeoLemmixTerrainData));
                 end;
          $05FF: begin // sound data
                   MetaStream.Read(SoundRec, SizeOf(TNeoLemmixSoundData));
                   fSoundPositions[SoundRec.SoundID] := SoundRec.SoundLoc;
                 end;
          $06FF: begin
                   s := ReadString(MetaStream);
                   fLemmings := LowerCase(s);
                   if fLemmings = 'lemming' then fLemmings := 'default';
                   if fLemmings = 'xlemming' then fLemmings := 'xmas';
                 end;
          else Break;
        end;

        if MetaStream.Read(w, 2) <> 2 then
          w := $00FF; //this would signal EOF, hence treat it as if a $FF00 was found
      end;
    except
      raise Exception.Create('The graphic  set "' + aName + '.dat" failed to load.');
    end;
  finally
    Decompressor.Free;
    CmpStream.Free;
    MetaStream.Free;
  end;
end;

function TBcGraphicSet.Acquire(aName: String): Boolean;
var
  SL: TStringList;
  TempStream: TMemoryStream;
begin
  Result := false;
  if not OnlineEnabled then
    Exit;

  SL := TStringList.Create;
  TempStream := TMemoryStream.Create;
  try
    if not DownloadToStringList('http://online.neolemmix.com/styles.php', SL) then Exit;
    if SL.Values[aName] = '' then Exit;
    if not DownloadToStream('http://online.neolemmix.com/' + Lowercase(aName) + '.dat', TempStream) then Exit;
    ForceDirectories(AppPath + 'styles\');
    TempStream.SaveToFile(AppPath + 'styles\' + aName + '.dat');
  finally
    SL.Free;
    TempStream.Free;
  end;
end;

function TBcGraphicSet.GetSoundPosition(aIndex: Integer): Integer;
begin
  Result := fSoundPositions[aIndex];
end;

end.