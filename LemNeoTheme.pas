unit LemNeoTheme;

// A simple unit for management of themes. These are basically just the remnants
// of graphic sets onces the terrain and objects are taken out. They define which
// lemming graphics to use, and the key colors.

interface

uses
  Dialogs,
  LemBCGraphicSet,
  GR32, LemTypes, LemStrings, PngInterface,
  StrUtils, Classes, SysUtils,
  LemNeoParser;

const
  MASK_COLOR = 'mask';
  MINIMAP_COLOR = 'minimap';
  BACKGROUND_COLOR = 'background';
  FALLBACK_COLOR = MASK_COLOR;

  DEFAULT_COLOR = $FF808080;

type
  TNeoThemeColor = record
    Name: String;
    Color: TColor32;
  end;

  TNeoTheme = class
    private
      fColors: array of TNeoThemeColor;
      fLemmings: String;          // Which lemming graphics to use
      function GetColor(Name: String): TColor32;
      function FindColorIndex(Name: String): Integer;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Clear;
      procedure Load(aSet: String); overload;
      procedure Load(aBc: TBcGraphicSet; aBgIndex: Integer); overload;

      property Lemmings: String read fLemmings write fLemmings;
      property Colors[Name: String]: TColor32 read GetColor;
  end;

implementation

constructor TNeoTheme.Create;
begin
  inherited;
  Clear;
end;

destructor TNeoTheme.Destroy;
begin
  inherited;
end;

procedure TNeoTheme.Clear;
begin
  fLemmings := 'default';
  SetLength(fColors, 0);
end;

procedure TNeoTheme.Load(aBc: TBcGraphicSet; aBgIndex: Integer);
var
  i: Integer;
begin
  fHasImageBackground := false;
  fBackgroundImage.Clear;

  SetLength(fColors, 3);
  fColors[0].Name := 'MASK';
  fColors[0].Color := aBc.MaskColor;
  fColors[1].Name := 'MINIMAP';
  fColors[1].Color := aBc.MinimapColor;
  fColors[2].Name := 'BACKGROUND';
  fColors[2].Color := aBc.BackgroundColor;

  if aBc.XmasLemmings then
    fLemmings := 'xmas'
  else
    fLemmings := 'default';

  if aBc.LoadBackground(fBackgroundImage, aBgIndex) then
    fHasImageBackground := true; 
end;

procedure TNeoTheme.Load(aSet: String);
var
  Parser: TParser;
  Sec: TParserSection;
  i: Integer;
begin
  Clear;
  SetCurrentDir(AppPath + SFStyles + aSet + '\');
  if not FileExists('theme.nxtm') then Exit; // should this raise an exception?

  Parser := TParser.Create;
  try
    Parser.LoadFromFile('theme.nxtm');

    fLemmings := Parser.MainSection.LineString['lemmings'];

    Sec := Parser.MainSection.Section['colors'];
    if Sec = nil then
      SetLength(fColors, 0)
    else begin
      SetLength(fColors, Sec.LineList.Count);
      for i := 0 to Sec.LineList.Count-1 do
      begin
        fColors[i].Name := Sec.LineList[i].Keyword;
        fColors[i].Color := Sec.LineList[i].ValueNumeric or $FF000000;
      end;
    end;
  finally
    Parser.Free;
  end;
end;

function TNeoTheme.GetColor(Name: String): TColor32;
var
  i: Integer;
begin
  i := FindColorIndex(Name);

  // Special exception
  if (i = -1) and (Lowercase(Name) = BACKGROUND_COLOR) then
  begin
    Result := $FF000000;
    Exit;
  end;

  if i = -1 then i := FindColorIndex(FALLBACK_COLOR);

  if i = -1 then
    Result := DEFAULT_COLOR
  else
    Result := fColors[i].Color;
end;

function TNeoTheme.FindColorIndex(Name: String): Integer;
begin
  Name := Lowercase(Name);
  for Result := 0 to Length(fColors)-1 do
    if Name = fColors[Result].Name then Exit;
  Result := -1;
end;

end.