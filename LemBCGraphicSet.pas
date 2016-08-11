unit LemBCGraphicSet;

// Backwards compatibility for graphic sets.

// TODO:
//  - make use of bgcolor etc from experimental GSTool sets
//  - support for custom sounds

interface

uses
  Dialogs,
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
  TSoundDataArray = array of TNeoLemmixSoundData;

  TNeoLemmixColorEntry = packed record
    case Byte of
      0: (A, R, G, B: Byte); // don't think this structure is ever needed, but nice to have just in case
      1: (ARGB: TColor32);
  end;

  TNeoLemmixHeader = packed record
    VersionNumber: Byte;
    Resolution: Byte;
    Reserved: Array[0..13] of Byte;
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
      fDataStream: TMemoryStream;
      fName: String;
      fResolution: Integer;
      fMaskColor: TColor32;
      fXmasLemmings: Boolean;
      fObjectCount: Integer;
      fTerrainCount: Integer;
      fObjectData: TObjectDataArray;
      fTerrainData: TTerrainDataArray;
      procedure EnsureObjectLength;
      procedure EnsureTerrainLength;
    public
      constructor Create;
      destructor Destroy; override;
      procedure LoadGraphicSet(aName: String);
      property DataStream: TMemoryStream read fDataStream;
      property Name: String read fName;
      property Resolution: Integer read fResolution;
      property MaskColor: TColor32 read fMaskColor;
      property XmasLemmings: Boolean read fXmasLemmings;
      property ObjectCount: Integer read fObjectCount;
      property TerrainCount: Integer read fTerrainCount;
      property ObjectData: TObjectDataArray read fObjectData;
      property TerrainData: TTerrainDataArray read fTerrainData;
  end;

  procedure LoadNeoLemmixImage(aStream: TStream; aBmp: TBitmap32);

implementation

procedure LoadNeoLemmixImage(aStream: TStream; aBmp: TBitmap32);
var
  x, y, w, h: LongWord;
  c: TColor32;
  a, r, g, b: Byte;
begin
  aStream.Read(w, 4);
  aStream.Read(h, 4);
  aBmp.SetSize(w, h);
  aBmp.Clear(0);
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
      aBmp.Pixel[x, y] := c;
    end;
end;

constructor TBcGraphicSet.Create;
begin
  inherited;
  SetLength(fObjectData, 20);
  SetLength(fTerrainData, 60);
  fDataStream := TMemoryStream.Create;
end;

destructor TBcGraphicSet.Destroy;
begin
  fDataStream.Free;
  inherited;
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

procedure TBcGraphicSet.LoadGraphicSet(aName: String);
var
  Decompressor: TDosDatDecompressor;
  CmpStream, MetaStream: TMemoryStream;

  Header: TNeoLemmixHeader;

  w: Word;
  b: Byte;
  s: String;

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
begin
  // Note: Should add compatibility with the experimental GSTool version's DAT files!

  CmpStream := CreateDataStream(aName + '.dat', ldtStyle);
  if CmpStream = nil then
  begin
    // Insert code for handling download from online.neolemmix.com here
  end;

  if CmpStream = nil then
  begin
    raise Exception.Create('The graphic set "' + aName + '.dat" could not be found.');
  end;

  MetaStream := TMemoryStream.Create;
  Decompressor := TDosDatDecompressor.Create;

  fName := aName;

  try
    try
      CmpStream.Position := 0;
      MetaStream.Position := 0;
      fDataStream.Position := 0;
      Decompressor.DecompressSection(CmpStream, MetaStream);
      Decompressor.DecompressSection(CmpStream, fDataStream);

      MetaStream.Position := 0;

      MetaStream.Read(w, 2);
      while w <> $00FF do
      begin
        case w of
          $01FF: begin // comment
                   repeat
                     MetaStream.Read(b, 1);
                   until b = $FF;
                   MetaStream.Position := MetaStream.Position - 1;
                 end;
          $02FF: begin // header
                   MetaStream.Read(Header, SizeOf(TNeoLemmixHeader));
                   fResolution := Header.Resolution div 8;
                   if fResolution = 0 then fResolution := 1;
                   fMaskColor := Header.KeyColors[0].ARGB;
                 end;
          $03FF: begin // object data
                   Inc(fObjectCount);
                   EnsureObjectLength;
                   MetaStream.Read(fObjectData[fObjectCount-1], SizeOf(TNeoLemmixObjectData));
                 end;
          $04FF: begin // terrain data
                   Inc(fTerrainCount);
                   EnsureTerrainLength;
                   MetaStream.Read(fTerrainData[fTerrainCount-1], SizeOf(TNeoLemmixTerrainData));
                 end;
          $05FF: begin // sound data
                 end;
          $06FF: begin
                   s := ReadString(MetaStream);
                   fXmasLemmings := (LowerCase(s) = 'xlemming');
                 end;
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

end.