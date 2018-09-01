{$include lem_directives.inc}
unit LemMenuFont;

interface

uses
  {System.Types,}
  LCLIntf, LCLType, LMessages, Messages, Classes, Controls, Graphics, MMSystem, Forms, Dialogs, Math,
  GR32, GR32_Image, GR32_Layers, GR32_Resamplers,
  FBaseDosForm,
  GameControl,
  LemDosStructures,
  LemSystemMessages,
  LemStrings, PngInterface, LemTypes,
  SysUtils;

const
  PURPLEFONTCOUNT = ord(#132) - ord('!') + 1;
  PurpleFontCharSet = [#26..#126] - [#32];

type
  TPurpleFont = class
    private
      fBitmaps: array[0..PURPLEFONTCOUNT - 1] of TBitmap32;
      function GetBitmapOfChar(Ch: Char): TBitmap32;
    public
      constructor Create;
      destructor Destroy; override;
      procedure LoadFontGraphics;

      function CalcTextSize(const S: string): TRect;
      procedure DrawText(Dst: TBitmap32; const S: string; X, Y: Integer; aRestoreBuffer: TBitmap32 = nil);
      procedure DrawTextCentered(Dst: TBitmap32; const S: string; Y: Integer; aRestoreBuffer: TBitmap32 = nil; EraseOnly: Boolean = False);

      property BitmapOfChar[Ch: Char]: TBitmap32 read GetBitmapOfChar;
  end;

implementation

constructor TPurpleFont.Create;
var
  i: Integer;
begin
  inherited;
  for i := 0 to PURPLEFONTCOUNT - 1 do
  begin
    fBitmaps[i] := TBitmap32.Create;
    fBitmaps[i].DrawMode := dmBlend;
    fBitmaps[i].CombineMode := cmMerge;
  end;
end;

destructor TPurpleFont.Destroy;
var
  i: Integer;
begin
  for i := 0 to PURPLEFONTCOUNT - 1 do
    fBitmaps[i].Free;
  inherited;
end;

function TPurpleFont.GetBitmapOfChar(Ch: Char): TBitmap32;
var
  Idx: Integer;
  ACh: AnsiChar;
begin
  ACh := AnsiChar(Ch);
  // Ignore any character not supported by the purple font
  if (not (ACh in [#26..#126])) and (ACh <> ' ') then
    Idx := 0
  else if Ord(ACh) > 32 then
    Idx := Ord(ACh) - 33
  else
    Idx := 94 + Ord(ACh) - 26;
  Result := fBitmaps[Idx];
end;

procedure TPurpleFont.LoadFontGraphics;
var
  TempBMP: TBitmap32;
  i: Integer;
begin
  TempBMP := TBitmap32.Create;
  try
    if (not (GameParams.CurrentLevel = nil))
       and FileExists(GameParams.CurrentLevel.Group.FindFile('menu_font.png')) then
      TPngInterface.LoadPngFile(GameParams.CurrentLevel.Group.FindFile('menu_font.png'), TempBMP)
    else if FileExists(AppPath + SFGraphicsMenu + 'menu_font.png') then
      TPngInterface.LoadPngFile(AppPath + SFGraphicsMenu + 'menu_font.png', TempBMP)
    else
      if MessageDlg('Could not find the menu font gfx\menu\menu_font.png. Try to continue?',
                    mtWarning, [mbYes, mbNo], 0) = mrNo then
        Application.Terminate();

    for i := 0 to PURPLEFONTCOUNT-7 do
    begin
      fBitmaps[i].SetSize(16, 16);
      fBitmaps[i].Clear(0);
      TempBMP.DrawTo(fBitmaps[i], 0, 0, Rect(i*16, 0, (i+1)*16, 16));
    end;

    if (not (GameParams.CurrentLevel = nil))
       and FileExists(GameParams.CurrentLevel.Group.FindFile('talismans.png')) then
      TPngInterface.LoadPngFile(GameParams.CurrentLevel.Group.FindFile('talismans.png'), TempBMP)
    else if FileExists(AppPath + SFGraphicsMenu + 'talismans.png') then
      TPngInterface.LoadPngFile(AppPath + SFGraphicsMenu + 'talismans.png', TempBMP)
    else
    begin
      if MessageDlg('Could not find the talisman graphics gfx\menu\talismans.png. Try to continue?',
                    mtWarning, [mbYes, mbNo], 0) = mrNo then Application.Terminate();
    end;

    for i := 0 to 5 do
    begin
      fBitmaps[PURPLEFONTCOUNT-6+i].SetSize(48, 48);
      fBitmaps[PURPLEFONTCOUNT-6+i].Clear(0);
      TempBMP.DrawTo(fBitmaps[PURPLEFONTCOUNT-6+i], 0, 0, Rect(48 * (i mod 2), 48 * (i div 2), 48 * ((i mod 2) + 1), 48 * ((i div 2) + 1)));
    end;
  finally
    TempBMP.Free;
  end;
end;

function TPurpleFont.CalcTextSize(const S: string): TRect;
{-------------------------------------------------------------------------------
  Linefeeds increment 16 pixels
  Spaces increment 16 pixels
-------------------------------------------------------------------------------}
var
  C: Char;
  CX, i: Integer;
begin
  CX := 0;
  FillChar(Result, SizeOf(Result), 0);
  if S <> '' then
    Result.Bottom := 16;
  for i := 1 to Length(S) do
  begin
    C := S[i];
    case C of
      #12:
        begin
          Inc(Result.Bottom, 8);
          CX := 0;
        end;
      #13:
        begin
          Inc(Result.Bottom, 16);
          CX := 0;
        end;
      #26..#126:
        begin
          Inc(CX, 16);
          if CX > Result.Right then
            Result.Right := CX;
        end;
    end;
  end;
end;

procedure TPurpleFont.DrawText(Dst: TBitmap32; const S: string; X, Y: Integer; aRestoreBuffer: TBitmap32 = nil);
{-------------------------------------------------------------------------------
  Linefeeds increment 16 pixels
  Spaces increment 16 pixels
-------------------------------------------------------------------------------}
var
  C: Char;
  CX, CY, i: Integer;
  R: TRect;
begin
  Y := Y + 1; // accounts for moving graphic up by 1 pixel

  if aRestoreBuffer <> nil then
  begin
    R := CalcTextSize(S);
    OffsetRect(R, X, Y);
    IntersectRect(R, R, aRestoreBuffer.BoundsRect); // oops, again watch out for sourceretangle!
    aRestoreBuffer.DrawTo(Dst, R, R);
  end;

  CX := X;
  CY := Y;
  for i := 1 to Length(S) do
  begin
    C := S[i];
    case C of
      #12:
        begin
          Inc(CY, 8);
          CX := X;
        end;
      #13:
        begin
          Inc(CY, 16);
          CX := X;
        end;
      ' ':
        begin
          Inc(CX, 16);
        end;
      #26..#31, #33..#132:
        begin
          BitmapOfChar[C].DrawTo(Dst, CX, CY);
          Inc(CX, 16);
        end;
    end;
  end;

end;

procedure TPurpleFont.DrawTextCentered(Dst: TBitmap32; const S: string; Y: Integer; aRestoreBuffer: TBitmap32 = nil;
  EraseOnly: Boolean = False);
{-------------------------------------------------------------------------------
  Linefeeds increment 16 pixels
  Spaces increment 16 pixels
-------------------------------------------------------------------------------}
var
  X, i: Integer;
  R: TRect;
  List: TStringList;
  H: string;

  procedure MakeList;
  var
    TempStr: String;
  begin
    TempStr := S;
    TempStr := StringReplace(TempStr, #13#10, #10, [rfReplaceAll]);
    TempStr := StringReplace(TempStr, #13, #10, [rfReplaceAll]);
    List.Delimiter := #10;
    List.StrictDelimiter := true;
    List.DelimitedText := TempStr;
  end;

begin
  List := TStringList.Create;
  MakeList;

  if aRestoreBuffer <> nil then
  begin
    R := CalcTextSize(S);
    OffsetRect(R, (Dst.Width - (R.Right - R.Left)) div 2, Y);
    IntersectRect(R, R, aRestoreBuffer.BoundsRect); // oops, again watch out for sourceretangle!
    aRestoreBuffer.DrawTo(Dst, R, R);
  end;

  if not EraseOnly then
    for i := 0 to List.Count - 1 do
    begin
      H := List[i]; // <= 40 characters!!!
      X := (Dst.Width - 16 * Length(H)) div 2;
      if (H <> #13) and (H <> #12) then
        DrawText(Dst, H, X, Y)
      else if H = #13 then
        Inc(Y, 16)
      else
        Inc(Y, 8);
    end;

  List.Free;
end;


end.

