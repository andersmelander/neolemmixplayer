unit LemRecolorSprites;

interface

uses
  Dialogs,
  Classes, SysUtils,
  LemNeoParser,
  LemDosStructures, LemLemming, LemTypes, LemStrings,
  GR32, GR32_Blend;

type
  TColorSwapType = (rcl_Selected,
                    rcl_Athlete,
                    rcl_Zombie);

  TColorSwap = record
    Condition: TColorSwapType;
    SrcColor: TColor32;
    DstColor: TColor32;
  end;

  // Remember - highlight needs to be hardcoded

  TColorSwapArray = array of TColorSwap;
  TSwapProgressArray = array[Low(TColorSwapType)..High(TColorSwapType)] of Boolean;

  TRecolorImage = class
    private
      fLemming: TLemming;
      fDrawAsSelected: Boolean;
      fSwaps: TColorSwapArray;

      procedure SwapColors(F: TColor32; var B: TColor32);

    public
      constructor Create;

      procedure LoadSwaps(aName: String);
      procedure CombineLemmingPixels(F: TColor32; var B: TColor32; M: TColor32);
      procedure CombineLemmingHighlight(F: TColor32; var B: TColor32; M: TColor32);

      property Lemming: TLemming write fLemming;
      property DrawAsSelected: Boolean write fDrawAsSelected;

      class procedure CombineDefaultPixels(F: TColor32; var B: TColor32; M: TColor32);
  end;

implementation

constructor TRecolorImage.Create;
begin
  inherited;

  // Until proper loading exists
  LoadSwaps('default');
end;

procedure TRecolorImage.SwapColors(F: TColor32; var B: TColor32);
var
  i: Integer;
  Progress: TSwapProgressArray;
begin
  B := F;

  if fLemming = nil then Exit;
  if (F and $FF000000) = 0 then Exit;

  FillChar(Progress, SizeOf(TSwapProgressArray), 0);

  for i := 0 to Length(fSwaps)-1 do
  begin
    if Progress[fSwaps[i].Condition] then Continue;
    case fSwaps[i].Condition of
      rcl_Selected: if not fDrawAsSelected then Continue;
      rcl_Zombie: if not fLemming.LemIsZombie then Continue;
      rcl_Athlete: if not fLemming.HasPermanentSkills then Continue;
      else raise Exception.Create('TRecolorImage.SwapColors encountered an unknown condition' + #13 + IntToStr(Integer(fSwaps[i].Condition)));
    end;
    if (F and $FFFFFF) = fSwaps[i].SrcColor then B := fSwaps[i].DstColor;
  end;
end;

procedure TRecolorImage.CombineLemmingPixels(F: TColor32; var B: TColor32; M: TColor32);
var
  A: TColor32;
  TempColor: TColor32;
begin
  A := (F and $FF000000);
  if A = 0 then Exit;
  SwapColors(F, TempColor);
  TempColor := (TempColor and $FFFFFF) or A;
  BlendMem(TempColor, B);
end;

procedure TRecolorImage.CombineLemmingHighlight(F: TColor32; var B: TColor32; M: TColor32);
begin
  // photoflash
  if F <> 0 then B := clBlack32 else B := clWhite32;
end;

procedure RegisterSwap(Sender: TObject; aSec: TParserSection; const aIteration: Integer; aData: Pointer);
var
  RecImg: TRecolorImage absolute Sender;
  Mode: ^TColorSwapType absolute aData;
  i: Integer;
begin
  if not (Sender is TRecolorImage) then Exit;

  with RecImg do
  begin
    i := Length(fSwaps);
    SetLength(fSwaps, i+1);
    fSwaps[i].Condition := Mode^;
    fSwaps[i].SrcColor := aSec.LineNumeric['from'];
    fSwaps[i].DstColor := aSec.LineNumeric['to'];
  end;
end;

procedure TRecolorImage.LoadSwaps(aName: String);
var
  Parser: TParser;
  Mode: TColorSwapType;
begin
  SetLength(fSwaps, 0);
  Parser := TParser.Create;
  try
    if not FileExists(AppPath + SFStyles + aName + SFPiecesLemmings + 'scheme.nxmi') then
      aName := 'default';

    if FileExists(AppPath + SFStyles + aName + SFPiecesLemmings + 'scheme.nxmi') then
    begin
      Parser.LoadFromFile(AppPath + SFStyles + aName + SFPiecesLemmings + 'scheme.nxmi');

      Mode := rcl_Athlete;
      Parser.MainSection.Section['recoloring'].DoForEachSection(self, 'athlete', @RegisterSwap, @Mode);

      Mode := rcl_Zombie;
      Parser.MainSection.Section['recoloring'].DoForEachSection(self, 'zombie', @RegisterSwap, @Mode);

      Mode := rcl_Selected;
      Parser.MainSection.Section['recoloring'].DoForEachSection(self, 'selected', @RegisterSwap, @Mode);
    end;
  finally
    Parser.Free;
  end;
end;

class procedure TRecolorImage.CombineDefaultPixels(F: TColor32; var B: TColor32; M: TColor32);
begin
  if F <> 0 then B := F;
end;

end.
 
