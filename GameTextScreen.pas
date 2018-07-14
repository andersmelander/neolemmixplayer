{$include lem_directives.inc}

unit GameTextScreen;

interface

uses
  LemmixHotkeys,
  Windows, Classes, SysUtils, Controls,
  UMisc,
  Gr32, Gr32_Layers,
  LemTypes, LemStrings, LemGame,
  GameControl, GameBaseScreen;

{-------------------------------------------------------------------------------
   The dos postview screen, which shows you how you've done it.
-------------------------------------------------------------------------------}
type
  TGameTextScreen = class(TGameBaseScreen)
  private
    fPreviewText: Boolean;
    function GetScreenText: string;
    procedure Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Form_KeyPress(Sender: TObject; var Key: Char);
    procedure Form_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Img_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure HandleMouseClick(Button: TMouseButton);
  protected
    procedure BuildScreen; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property PreviewText: Boolean read fPreviewText write fPreviewText;
  published
  end;

implementation

uses Forms;

{ TDosGamePreview }

procedure TGameTextScreen.BuildScreen;
var
  Temp: TBitmap32;
begin
  ScreenImg.BeginUpdate;
  Temp := TBitmap32.Create;
  try
    InitializeImageSizeAndPosition(640, 400);
    ExtractBackGround;
    ExtractPurpleFont;

    Temp.SetSize(640, 400);
    Temp.Clear(0);
    TileBackgroundBitmap(0, 0, Temp);
    DrawPurpleTextCentered(Temp, GetScreenText, 16);
    ScreenImg.Bitmap.Assign(Temp);

    if PreviewText then
      GameParams.ShownText := true;
  finally
    ScreenImg.EndUpdate;
    Temp.Free;
  end;
end;

constructor TGameTextScreen.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  OnKeyDown := @Form_KeyDown;
  OnKeyPress := @Form_KeyPress;
  OnMouseDown := @Form_MouseDown;
  ScreenImg.OnMouseDown := @Img_MouseDown;
end;

destructor TGameTextScreen.Destroy;
begin
  inherited Destroy;
end;

function TGameTextScreen.GetScreenText: string;
var
  i: Integer;
  lfc: Integer;
  SL: TStringList;

  procedure Add(const S: string);
  begin
    Result := Result + S + #13;
    Inc(lfc);
  end;

  procedure LF(aCount: Integer);
  begin
    Result := Result + StringOfChar(#13, aCount);
    Inc(lfc, aCount);
  end;

  procedure PreLF(aCount: Integer);
  begin
    Result := StringOfChar(#13, aCount) +  Result;
    Inc(lfc, aCount);
  end;

begin
  Result := '';

  if fPreviewText then
    SL := GameParams.Level.PreText
  else
    SL := GameParams.Level.PostText;

  for i := 0 to SL.Count-1 do
    if i > 20 then
      Break
    else
      Add(SL[i]);

  while lfc < 21 do
    if lfc mod 2 = 1 then
      PreLF(1)
    else
      LF(1);

  LF(1);
  Add(SPressMouseToContinue);
end;

procedure TGameTextScreen.Form_KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  S: String;
begin
  if (GameParams.Hotkeys.CheckKeyEffect(Key).Action = lka_SaveReplay) and (GameParams.NextScreen = gstPostview) then
  begin
    S := GlobalGame.ReplayManager.GetSaveFileName(self, GlobalGame.Level);
    if S = '' then Exit;
    GlobalGame.EnsureCorrectReplayDetails;
    GlobalGame.ReplayManager.SaveToFile(S);
    Exit;
  end;

  case Key of
    VK_RETURN: CloseScreen(GameParams.NextScreen);
    VK_ESCAPE: CloseScreen(gstMenu);
  end;
end;

procedure TGameTextScreen.Form_MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  HandleMouseClick(Button);
end;

procedure TGameTextScreen.Img_MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  HandleMouseClick(Button);
end;

procedure TGameTextScreen.HandleMouseClick(Button: TMouseButton);
begin
  if Button = mbLeft then
    CloseScreen(GameParams.NextScreen);
end;

procedure TGameTextScreen.Form_KeyPress(Sender: TObject; var Key: Char);
begin

end;

end.

