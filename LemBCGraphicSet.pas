unit LemBCGraphicSet;

// Backwards compatibility for graphic sets.

// TODO:
//  - make use of bgcolor etc from experimental GSTool sets
//  - support for custom sounds

interface

uses
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
  TObjectDataArray: array of TNeoLemmixObjectData;

  TNeoLemmixTerrainData = packed record
    TerrainFlags: Word;
    BaseLoc: LongWord;
    Reserved: Array[0..9] of Byte;
  end;
  TTerrainDataArray: array of TNeoLemmixTerrainData;

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
      property TerrainData: TObjectDataArray read fTerrainData;
  end;

implementation

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
  c: Char;
  s: String;

  function ReadString(aStream: TStream): String;
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
      while w <> $FF00 do
      begin

        case w of
          $FF01: repeat // comment
                   MetaStream.Read(c, 1);
                 until c = #0;
          $FF02: begin // header
                   MetaStream.Read(Header, SizeOf(TNeoLemmixHeader));
                   fResolution := Header.Resolution div 8;
                   fMaskColor := Header.KeyColors[0].ARGB;
                 end;
          $FF03: begin // object data
                   Inc(fObjectCount);
                   EnsureObjectLength;
                   MetaStream.Read(fObjectData[fObjectCount-1], SizeOf(TNeoLemmixObjectData));
                 end;
          $FF04: begin // terrain data
                   Inc(fTerrainCount);
                   EnsureTerrainLength;
                   MetaStream.Read(fTerrainData[fTerrainCount-1], SizeOf(TNeoLemmixTerrainData));
                 end;
          $FF05: begin // sound data
                 end;
          $FF06: begin
                   s := ReadString(MetaStream);
                   fXmasLemmings := (LowerCase(s) = 'xlemming');
                 end;
        end;

        if MetaStream.Read(w, 2) <> 2 then
          w := $FF00; //this would signal EOF, hence treat it as if a $FF00 was found
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