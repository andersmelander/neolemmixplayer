unit FNeoBaseMenuScreen;

interface

uses
  GR32, GR32_Image, GR32_Resamplers,
  LemMenuFont,
  FBaseScreen,
  Classes, SysUtils;

type
  TBaseMenuScreen = class(TBaseScreen)
    private type
      TFadeMode = (fmIn, fmNone, fmOut);
    private
      fFadeMode: TFadeMode;
      fFadeStage: Double;
      fLastFadeTicks: Int64;
      fFadeToNewScreen: TBaseScreen;

      fImg32: TImage32;
      fBmp32: TBitmap32;
      fChanged: Boolean;

      procedure HandleFade;
    protected
      procedure Initialize; override;
      procedure Finalize; override;

      procedure InternalUpdateGame; virtual;

      property Bmp: TBitmap32 read fBmp32;
      property Changed: Boolean read fChanged write fChanged;

      procedure BeginFadeout(aNewScreen: TBaseScreen);
    public
      procedure UpdateGame; override;
  end;

implementation

uses
  GameControl;

const
  MENU_INTERNAL_WIDTH = 640;
  MENU_INTERNAL_HEIGHT = 480;
  FADE_DURATION = 1.5;

procedure TBaseMenuScreen.Initialize;
begin
  inherited;
  fImg32 := PrepareImg32(0, 0, ClientWidth, ClientHeight);
  if GameParams.LinearResampleMenu then
    fImg32.Bitmap.Resampler := TLinearResampler.Create;

  fBmp32 := TBitmap32.Create(MENU_INTERNAL_WIDTH, MENU_INTERNAL_HEIGHT);
  fBmp32.Clear($00000000);

  fFadeMode := fmIn;
  fFadeStage := FADE_DURATION;
  fLastFadeTicks := GetTickCount64;

  BlockAllInput := true;
end;

procedure TBaseMenuScreen.Finalize;
begin
  fBmp32.Free;
  fImg32.Free;
  inherited;
end;

procedure TBaseMenuScreen.BeginFadeout(aNewScreen: TBaseScreen);
begin
  fFadeMode := fmOut;
  fFadeStage := FADE_DURATION;
  fLastFadeTicks := GetTickCount64;
  fFadeToNewScreen := aNewScreen;
  BlockAllInput := true;
end;

procedure TBaseMenuScreen.HandleFade;
var
  Intensity: Double;
  NewTC: Int64;
  P: PColor32;
  x, y: Integer;
begin
  NewTC := GetTickCount64;
  fFadeStage := fFadeStage - ((NewTC - fLastFadeTicks) / 1000);
  fLastFadeTicks := NewTC;

  if fFadeStage <= 0 then
  begin
    if fFadeMode = fmIn then
    begin
      BlockAllInput := false;
      fFadeMode := fmNone;
      fFadeStage := 0;
    end else
      NewScreen := fFadeToNewScreen;

    Exit;
  end;

  Intensity := fFadeStage / FADE_DURATION;
  if fFadeMode = fmIn then Intensity := 1 - Intensity;

  for y := 0 to fImg32.Bitmap.Height-1 do
  begin
    P := fImg32.Bitmap.PixelPtr[0, y];
    for x := 0 to fImg32.Bitmap.Width-1 do
    begin
      P^ := (P^ and $FFFFFF) or (Trunc(P^ * Intensity) and $FF000000);
      Inc(P);
    end;
  end;

  fChanged := true;
end;

procedure TBaseMenuScreen.UpdateGame;
begin
  inherited;

  InternalUpdateGame;

  if fChanged then
  begin
    fImg32.Bitmap.Assign(fBmp32);
    if fFadeMode <> fmNone then HandleFade;
    fImg32.Invalidate;
    fChanged := false;
  end;
end;

procedure TBaseMenuScreen.InternalUpdateGame;
begin
  // Intentionally blank
end;

end.
