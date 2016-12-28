unit LemNeoPieceManager;

// The TNeoPieceManager class is used in a similar manner to how
// graphic sets were in the past. It could be thought of as a huge
// dynamic graphic set.

interface

uses
  Dialogs,
  LemBCGraphicSet,
  PngInterface, LemNeoTheme,
  LemMetaTerrain, LemMetaObject, LemTypes, GR32, LemStrings,
  StrUtils, Classes, SysUtils;

const
  RETAIN_PIECE_CYCLES = 10; // how many times Tidy can be called without a piece being used before it's discarded

type

  TLabelRecord = record
    GS: String;
    Piece: String;
  end;

  TNeoPieceManager = class
    private
      fIsObtaining: Boolean;
      fTheme: TNeoTheme;
      fTerrains: TMetaTerrains;
      fObjects: TMetaObjects;

      function GetTerrainCount: Integer;
      function GetObjectCount: Integer;

      function FindTerrainIndexByIdentifier(Identifier: String): Integer;
      function FindObjectIndexByIdentifier(Identifier: String): Integer;
      function ObtainTerrain(Identifier: String): Integer;
      function ObtainObject(Identifier: String): Integer;

      procedure ObtainGraphicSet(aName: String; aAsTheme: Boolean = false; aBgIndex: Integer = 0);
      procedure ObtainVgaspec(aName: String);
      function CheckForGraphicSet(aName: String): Boolean;

      function GetMetaTerrain(Identifier: String): TMetaTerrain;
      function GetMetaObject(Identifier: String): TMetaObject;

      property TerrainCount: Integer read GetTerrainCount;
      property ObjectCount: Integer read GetObjectCount;
    public
      constructor Create;
      destructor Destroy; override;

      procedure Tidy;

      procedure SetTheme(aTheme: TNeoTheme);
      procedure ApplyTheme(aSet: String);

      function GetTerrainPieceCount(aSet: String): Integer;

      property Terrains[Identifier: String]: TMetaTerrain read GetMetaTerrain;
      property Objects[Identifier: String]: TMetaObject read GetMetaObject;
  end;

  function SplitIdentifier(Identifier: String): TLabelRecord;
  function CombineIdentifier(Identifier: TLabelRecord): String;

var
  PieceManager: TNeoPieceManager; // globalized as this does not need to have seperate instances

implementation

// These two standalone functions are just to help shifting labels around

function SplitIdentifier(Identifier: String): TLabelRecord;
var
  i: Integer;
  FoundDivider: Boolean;
begin
  Result.GS := '';
  Result.Piece := '';
  FoundDivider := false;
  for i := 1 to Length(Identifier) do
    if Identifier[i] = ':' then
      FoundDivider := true
    else if FoundDivider then
      Result.Piece := Result.Piece + Identifier[i]
    else
      Result.GS := Result.GS + Identifier[i];
end;

function CombineIdentifier(Identifier: TLabelRecord): String;
begin
  // This one is much simpler.
  Result := Identifier.GS + ':' + Identifier.Piece;
end;

// Constructor, destructor, usual boring stuff

constructor TNeoPieceManager.Create;
begin
  inherited;
  fTerrains := TMetaTerrains.Create;
  fObjects := TMetaObjects.Create;
end;

destructor TNeoPieceManager.Destroy;
begin
  fTerrains.Free;
  fObjects.Free;
  inherited;
end;

// Tidy-up function. Pretty much clears out the lists. Might add
// stuff in the future so it retains frequently-used pieces.
procedure TNeoPieceManager.Tidy;
var
  i: Integer;
begin
  for i := fTerrains.Count-1 downto 0 do
  begin
    fTerrains[i].CyclesSinceLastUse := fTerrains[i].CyclesSinceLastUse + 1;
    if fTerrains[i].CyclesSinceLastUse >= RETAIN_PIECE_CYCLES then
      fTerrains.Delete(i);
  end;
  for i := fObjects.Count-1 downto 0 do
  begin
    fObjects[i].CyclesSinceLastUse := fObjects[i].CyclesSinceLastUse + 1;
    if (fObjects[i].CyclesSinceLastUse >= RETAIN_PIECE_CYCLES) or (fObjects[i].IsMasked) then
      fObjects.Delete(i);
  end;
end;

// Quick shortcuts to get number of pieces currently present

function TNeoPieceManager.GetTerrainCount: Integer;
begin
  Result := fTerrains.Count;
end;

function TNeoPieceManager.GetObjectCount: Integer;
begin
  Result := fObjects.Count;
end;

function TNeoPieceManager.GetTerrainPieceCount(aSet: String): Integer;
var
  i: Integer;
begin
  Result := 0;
  aSet := Lowercase(aSet);
  for i := 0 to fTerrains.Count-1 do
    if Lowercase(fTerrains[i].GS) = aSet then
      Inc(Result);
  if Result = 0 then
  begin
    i := fTerrains.Count;
    ObtainGraphicSet(aSet);
    Result := fTerrains.Count - i;
  end;
end;

// Some functions to locate a piece in the internal arrays...

function TNeoPieceManager.FindTerrainIndexByIdentifier(Identifier: String): Integer;
begin
  Identifier := Lowercase(Identifier);
  for Result := 0 to TerrainCount-1 do
    if fTerrains[Result].Identifier = Identifier then Exit;

  // if it's not found
  Result := ObtainTerrain(Identifier);
end;

function TNeoPieceManager.FindObjectIndexByIdentifier(Identifier: String): Integer;
begin
  Identifier := Lowercase(Identifier);
  for Result := 0 to ObjectCount-1 do
    if fObjects[Result].Identifier = Identifier then Exit;

  // if it's not found
  Result := ObtainObject(Identifier);
end;

// ... and to load it if not found.

function TNeoPieceManager.ObtainTerrain(Identifier: String): Integer;
var
  TerrainLabel: TLabelRecord;
begin
  //raise Exception.Create('ObtainTerrain called for "' + Identifier + '". Please report this occurance.');

  if fIsObtaining then raise Exception.Create('ObtainTerrain loop on "' + Identifier + '". Please report this.');

  TerrainLabel := SplitIdentifier(Identifier);
  ObtainGraphicSet(TerrainLabel.GS);
  fIsObtaining := true;
  Result := FindTerrainIndexByIdentifier(Identifier);
  fIsObtaining := false;

  (*BasePath := AppPath + SFStyles + TerrainLabel.GS + SFPiecesTerrain + TerrainLabel.Piece;

  if FileExists(BasePath + '.png') then  // .nxtp is optional, but .png is not :)
    T := TMetaTerrain.Create
  else
    raise Exception.Create('TNeoPieceManager.ObtainTerrain: Could not find terrain piece: ' + Identifier);
  fTerrains.Add(T);
  T.Load(TerrainLabel.GS, TerrainLabel.Piece);*)
end;

function TNeoPieceManager.ObtainObject(Identifier: String): Integer;
var
  ObjectLabel: TLabelRecord;
begin
  //raise Exception.Create('ObtainObject called for "' + Identifier + '". Please report this occurance.');

  if fIsObtaining then raise Exception.Create('ObtainObject loop on "' + Identifier + '". Please report this.');

  ObjectLabel := SplitIdentifier(Identifier);
  ObtainGraphicSet(ObjectLabel.GS);
  fIsObtaining := true;
  Result := FindObjectIndexByIdentifier(Identifier);
  fIsObtaining := false;
  //MO := fObjects.Add;
  //MO.Load(ObjectLabel.GS, ObjectLabel.Piece, fTheme);
end;

// Backwards-comaptibility code; load entire graphic sets.

procedure TNeoPieceManager.ObtainVgaspec(aName: String);
var
  BcSet: TBcGraphicSet;
begin
  BcSet := TBcGraphicSet.Create;
  try
    BcSet.LoadGraphicSet('x_' + aName);
    with fTerrains.Add do
      LoadVgaspec(BcSet);
  finally
    BcSet.Free;
  end;
end;

procedure TNeoPieceManager.ObtainGraphicSet(aName: String; aAsTheme: Boolean = false; aBgIndex: Integer = 0);
var
  BcSet: TBcGraphicSet;
  AlreadyHasSet: Boolean;
  i: Integer;
begin
  AlreadyHasSet := CheckForGraphicSet(aName);

  if AlreadyHasSet then
  begin
    // Zero the last use cycle. This is done instead of the normal system so that either whole graphic
    // sets will be discarded at once, or nothing of them will.
    for i := fTerrains.Count-1 downto 0 do
      if Lowercase(aName) = Lowercase(fTerrains[i].GS) then
        fTerrains[i].CyclesSinceLastUse := 0;
    for i := fObjects.Count-1 downto 0 do
      if Lowercase(aName) = Lowercase(fObjects[i].GS) then
        fObjects[i].CyclesSinceLastUse := 0;
  end;

  if AlreadyHasSet and not aAsTheme then Exit;

  BcSet := TBcGraphicSet.Create;
  try
    BcSet.LoadGraphicSet(aName);

    if aAsTheme then
      fTheme.Load(BcSet, aBgIndex);

    if AlreadyHasSet then Exit;

    for i := 0 to BcSet.ObjectCount-1 do
      with fObjects.Add do
        Load(BcSet, i);

    for i := 0 to BcSet.TerrainCount-1 do
      with fTerrains.Add do
        Load(BcSet, i);
  finally
    BcSet.Free;
  end;
end;

function TNeoPieceManager.CheckForGraphicSet(aName: String): Boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 0 to fTerrains.Count-1 do
    if Lowercase(fTerrains[i].GS) = Lowercase(aName) then
    begin
      Result := true;
      Exit;
    end;
end;

// Functions to get the metainfo

function TNeoPieceManager.GetMetaTerrain(Identifier: String): TMetaTerrain;
var
  i: Integer;
  SetToSteel: Boolean;
begin
  if RightStr(Identifier, 2) = '*s' then
  begin
    Identifier := LeftStr(Identifier, Length(Identifier)-2);
    SetToSteel := true;
  end;
  i := FindTerrainIndexByIdentifier(Identifier);
  Result := fTerrains[i];
  if SetToSteel then
    Result.IsSteel := true;
end;

function TNeoPieceManager.GetMetaObject(Identifier: String): TMetaObject;
var
  i: Integer;
begin
  i := FindObjectIndexByIdentifier(Identifier);
  Result := fObjects[i];
end;

// And the stuff for communicating with the theme

procedure TNeoPieceManager.SetTheme(aTheme: TNeoTheme);
begin
  fTheme := aTheme;
  Tidy;
end;

procedure TNeoPieceManager.ApplyTheme(aSet: String);
begin
  ObtainGraphicSet(aSet, true);
  Tidy;
end;

end.