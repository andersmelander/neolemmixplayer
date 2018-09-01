unit FNeoMenuScreen;

interface

uses
  GR32,
  FNeoBaseMenuScreen,
  LemTypes,
  Classes, SysUtils;

type
  TMainMenuScreen = class(TBaseMenuScreen)
    private
      fBaseImage: TBitmap32;

      procedure MakeBaseImage;
    protected
      procedure Initialize; override;
      procedure Finalize; override;
      procedure InternalUpdateGame; override;
  end;

implementation

type
  TMenuPanel = (mpPlay, mpLevelSelect, mpRank, mpSettings, mpTalismans, mpExit);

const
  LOGO_POSITION: TPoint = (X: 8; Y: 20);

  PANEL_POSITIONS: array[TMenuPanel] of TPoint = (
    (X: 136; Y: 140), // Play
    (X: 264; Y: 140), // Select Level
    (X: 392; Y: 140), // Rank
    (X: 136; Y: 236), // Settings
    (X: 264; Y: 236), // Talismans
    (X: 392; Y: 236)  // Exit
  );

  PANEL_FILENAMES

  RANK_GRAPHIC_POSITION = TPoint( PANEL_POSITIONS[mpRank].X + 32;
                                  PANEL_POSITIONS[mpRank].Y + 24 );

  NO_TALISMANS_OFFSET = 64; // Settings offset to the right by this, and Exit to the left, if the Talismans panel is hidden

  MENU_STATIC_TEXTS_Y = 322;

  CREDITS_REEL_Y = 376;

  REEL_CHARACTERS = 36;

  REEL_WIDTH = REEL_CHARACTERS * MENU_FONT_CHAR_SIZE;

procedure TMainMenuScreen.Initialize;
begin
  inherited;
  fBaseImage := TBitmap32.Create(Bmp.Width, Bmp.Height);
  MakeBaseImage;
end;

procedure TMainMenuScreen.Finalize;
begin
  fBaseImage.Free;
  inherited;
end;

procedure TMainMenuScreen.MakeBaseImage;
var
  TempBmp: TBitmap32;
begin
  DrawBackground(fBaseImage, fBaseImage.BoundsRect);

  TempBmp := TBitmap32.Create;
  try

  finally
    TempBmp.Free;
  end;
end;

end.
