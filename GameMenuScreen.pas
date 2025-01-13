unit GameMenuScreen;

interface

uses
  Generics.Collections,
  StrUtils, Classes, SysUtils, Dialogs, Controls, ExtCtrls, Forms, Windows, ShellApi,
  Types, UMisc, Math,
  GameBaseMenuScreen,
  GameControl,
  LemNeoLevelPack,
  LemNeoOnline,
  LemNeoParser,
  LemStrings,
  LemTypes,
  GR32,
  GR32_Resamplers,
  GR32_Layers;

type
  TGameMenuPositionData = record
    LogoY: Integer;

    CardSpacingHorz: Integer;
    CardSpacingVert: Integer;
    CardsCenterY: Integer;

    GroupArrowsOffsetX: Integer;
    GroupArrowUpOffsetY: Integer;
    GroupArrowDownOffsetY: Integer;

    GroupGraphicOffsetX: Integer;
    GroupGraphicOffsetY: Integer;
    GroupGraphicAutoMaxWidth: Integer;
    GroupGraphicAutoMaxHeight: Integer;

    FooterTextY: Integer;

    ScrollerY: Integer;
    ScrollerWidth: Integer;
    ScrollerLemmingFrames: Integer;
  end;

  TGameMenuScreen = class(TGameBaseMenuScreen)
    private
      LayoutInfo: TGameMenuPositionData;

      fDisableScroller: Boolean;
      fLastReelUpdateTickCount: UInt64;
      fReelFrame: Integer;
      fReelTextPos: Integer;
      fReelTextIndex: Integer;
      fReelFreezeIterations: Integer;

      fSwitchedTextSinceForce: Boolean;
      fReelForceDirection: Integer;

      fCleanInstallFail: Boolean;
      fUpdateCheckThread: TDownloadThread;
      fVersionInfo: TStringList;

      fScrollerTextList: TStringList;

      FLayerGroup: TBitmapLayer;
      FLayerWorkerLemminLeft: TAnimatedFrameLayer;
      FLayerWorkerLemminRight: TAnimatedFrameLayer;
      FLayerScroller: TBannerLayer;
      FLayerBannerText: TBannerTextLayer;


      procedure MakeAutoSectionGraphic(Dst: TBitmap32);

      procedure CleanupIngameStuff;

      procedure DrawLogo;
      procedure MakePanels;
      procedure MakeFooterText;

      procedure LoadScrollerGraphics;
      procedure PrepareScrollerTextList;
      procedure DrawScroller;
      procedure DrawReel;
      procedure DrawReelText;
      procedure DrawWorkerLemmings;

      procedure UpdateReel;
      procedure UpdateReelIteration;
      procedure PrepareNextReelText;

      procedure BeginGame;
      procedure ExitGame;

      procedure PrevGroup;
      procedure NextGroup;
      procedure UpdateGroupSign(aRedraw: Boolean = True);

      procedure ShowSetupMenu;
      procedure DoCleanInstallCheck;
      procedure InitiateUpdateCheck;
      procedure HandleUpdateCheckResult;

      procedure ForceReelBackward;
      procedure ForceReelForward;

      procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
      procedure DisableIdle;
      procedure EnableIdle;

      procedure LoadLayoutData;
    protected
      procedure BuildScreen; override;
      procedure CloseScreen(aNextScreen: TGameScreenType); override;

      procedure DoAfterConfig; override;

      function GetBackgroundSuffix: String; override;
    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;
  end;

implementation

uses
  LemMenuFont, // for size const
  CustomPopup,
  FStyleManager,
  FNeoLemmixSetup,
  LemGame, // to clear replay
  LemVersion,
  PngInterface;

{ TGameMenuScreen }

constructor TGameMenuScreen.Create(aOwner: TComponent);
begin
  inherited;

  fVersionInfo := TStringList.Create;

  fScrollerTextList := TStringList.Create;
  GameParams.MainForm.Caption := SProgramNameFull;
end;

destructor TGameMenuScreen.Destroy;
begin
  fVersionInfo.Free;

  fScrollerTextList.Free;

  inherited;
end;

procedure TGameMenuScreen.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  if fCleanInstallFail then
  begin
    DisableIdle;

    fCleanInstallFail := False;
    ShowMessage('It appears you have installed this version of NeoLemmix over ' +
                'an older major version. It is recommended that you perform a ' +
                'clean install of NeoLemmix whenever updating to a new major ' +
                'version. If you encounter any bugs, especially relating to ' +
                'styles, please test with a fresh install before reporting them.');
    fLastReelUpdateTickCount := GetTickCount64;

    EnableIdle;
  end else if not GameParams.LoadedConfig then
  begin
    DisableIdle;

    GameParams.LoadedConfig := True;
    ShowSetupMenu;

    EnableIdle;
  end else if (fUpdateCheckThread <> nil) and (fUpdateCheckThread.Complete) then
  begin
    DisableIdle;

    if fUpdateCheckThread.Success then
      HandleUpdateCheckResult;

    fUpdateCheckThread.Free;
    fUpdateCheckThread := nil;

    EnableIdle;
  end else if not fDisableScroller then
    UpdateReel;

  Done := False;
  Sleep(1);
end;

procedure TGameMenuScreen.DisableIdle;
begin
  Application.OnIdle := nil;
end;

procedure TGameMenuScreen.EnableIdle;
begin
  Application.OnIdle := ApplicationIdle;
  fLastReelUpdateTickCount := GetTickCount64;
end;

procedure TGameMenuScreen.CloseScreen(aNextScreen: TGameScreenType);
begin
  if fUpdateCheckThread <> nil then
  begin
    fUpdateCheckThread.Kill;
    FreeAndNil(fUpdateCheckThread);
  end;

  inherited;
end;

procedure TGameMenuScreen.BuildScreen;
begin
  inherited;

  // If user has chosen to always load next unsolved level, load it whenever the menu screen is active
  if GameParams.LoadNextUnsolvedLevel then
    GameParams.CurrentLevel := GameParams.CurrentLevel.Group.ParentBasePack.FirstUnbeatenLevelRecursive;

  CleanUpIngameStuff;

  LoadLayoutData;

  DrawLogo;
  MakePanels;
  MakeFooterText;

  LoadScrollerGraphics;
  DrawScroller;
  PrepareScrollerTextList;

  DoCleanInstallCheck;

  if not GameParams.DoneUpdateCheck then
    InitiateUpdateCheck;

  if (GameParams.CurrentLevel <> nil) and
     (fScrollerTextList.Count > 0) then
  begin
    fReelTextIndex := -1;
    PrepareNextReelText;
  end else
    fDisableScroller := True;

  EnableIdle;
end;

procedure TGameMenuScreen.CleanupIngameStuff;
begin
  if Assigned(GlobalGame) then
    GlobalGame.ReplayManager.Clear(True);

  GameParams.ShownText := False;
end;

procedure TGameMenuScreen.DrawLogo;
var
  Layer: TBitmapLayer;
  r: TFloatRect;
begin
  Layer := ScreenImg.Layers.Add<TBitmapLayer>;
  Layer.Scaled := True;
  TCustomResamplerClass(ScreenImg.Bitmap.Resampler.ClassType).Create(Layer.Bitmap);

  GetGraphic('logo.png', Layer.Bitmap);

  r := FloatRect(0, 0, Layer.Bitmap.Width, Layer.Bitmap.Height);
  r.Offset((GameRect.Width - r.Width) / 2, LayoutInfo.LogoY - r.Height / 2);
  Layer.Location := r;
end;

procedure TGameMenuScreen.MakePanels;

  function MakePosition(aHorzOffset: Single; aVertOffset: Single): TPoint;
  begin
    Result.X := (GameRect.Width div 2) + Round(aHorzOffset * LayoutInfo.CardSpacingHorz);
    Result.Y := LayoutInfo.CardsCenterY + Round(aVertOffset * LayoutInfo.CardSpacingVert);
  end;

  function AddButton(const AFilenames: TArray<string>; AAction: TRegionAction; const AShortCuts: TArray<TShortCut> = [];
    AGlowSize: integer = 5; AParent: TClickableLayer = nil): TClickableLayer;
var
    i: integer;
    ShortCut: TShortCut;
  begin
    Result := ScreenImg.Layers.Add<TClickableLayer>;

    for i := 0 to High(AFilenames) do
      if (GetGraphic(AFilenames[i], Result.Bitmap, (i < High(AFilenames)))) then
        break;

    Result.Action := AAction;

    for ShortCut in AShortCuts do
      Result.ShortcutKeys.Add(ShortCut);

    Result.GlowSize := AGlowSize;
    Result.Scaled := True;
    Result.Parent := AParent;
  end;

var
  Layer, ParentLayer: TClickableLayer;
  p: TPoint;
  r: TFloatRect;
begin
    // Play
  Layer := AddButton(['sign_play.png'], BeginGame, [VK_RETURN, VK_F1]);
    p := MakePosition(-1, -0.5);
    r := FloatRect(SizedRect(p.X - Layer.Bitmap.Width div 2, p.Y - Layer.Bitmap.Height div 2, Layer.Bitmap.Width, Layer.Bitmap.Height));
  r.Inflate(Layer.GlowSize, Layer.GlowSize);
    Layer.Location := r;

    // Level select
  Layer := AddButton(['sign_code.png', 'sign_level_select.png'], DoLevelSelect, [VK_F2]);
  p := MakePosition(0, -0.5);
  r := FloatRect(SizedRect(p.X - Layer.Bitmap.Width div 2, p.Y - Layer.Bitmap.Height div 2, Layer.Bitmap.Width, Layer.Bitmap.Height));
  r.Inflate(Layer.GlowSize, Layer.GlowSize);
  Layer.Location := r;

  p := MakePosition(1.0, -0.5);
  begin

    // Group sign
    ParentLayer := AddButton(['sign_group.png'], NextGroup);
    r := FloatRect(SizedRect(p.X - ParentLayer.Bitmap.Width div 2, p.Y - ParentLayer.Bitmap.Height div 2, ParentLayer.Bitmap.Width, ParentLayer.Bitmap.Height));
    r.Inflate(ParentLayer.GlowSize, ParentLayer.GlowSize);
    ParentLayer.Location := r;

    FLayerGroup := ScreenImg.Layers.Add<TBitmapLayer>;
    UpdateGroupSign; // Load bitmap so we get its size
    FLayerGroup.Location := FloatRect(SizedRect(
      p.X + LayoutInfo.GroupGraphicOffsetX - FLayerGroup.Bitmap.Width div 2,
      p.Y + LayoutInfo.GroupGraphicOffsetY - FLayerGroup.Bitmap.Height div 2,
      FLayerGroup.Bitmap.Width, FLayerGroup.Bitmap.Height));
    FLayerGroup.Scaled := True;
    FLayerGroup.LayerOptions := Layer.LayerOptions and (not LOB_MOUSE_EVENTS);

    // Group sign buttons
    Layer := AddButton(['sign_group_up.png'], NextGroup, [VK_UP], 3, ParentLayer);
    Layer.ColorClick := $FF404040;
    r := FloatRect(SizedRect(
      p.X + LayoutInfo.GroupArrowsOffsetX - Layer.Bitmap.Width div 2,
      p.Y + LayoutInfo.GroupArrowUpOffsetY - Layer.Bitmap.Height div 2,
      Layer.Bitmap.Width, Layer.Bitmap.Height));
    r.Inflate(Layer.GlowSize, Layer.GlowSize);
    Layer.Location := r;

    Layer := AddButton(['sign_group_down.png'], PrevGroup, [VK_DOWN], 3, ParentLayer);
    Layer.ColorClick := $FF404040;
    r := FloatRect(SizedRect(
      p.X + LayoutInfo.GroupArrowsOffsetX - Layer.Bitmap.Width div 2,
      p.Y + LayoutInfo.GroupArrowDownOffsetY - Layer.Bitmap.Height div 2,
      Layer.Bitmap.Width, Layer.Bitmap.Height));
    r.Inflate(Layer.GlowSize, Layer.GlowSize);
    Layer.Location := r;

  end;

    // Config
  Layer := AddButton(['sign_config.png'], ShowConfigMenu, [VK_F3]);
  p := MakePosition(-0.5, 0.5);
  r := FloatRect(SizedRect(p.X - Layer.Bitmap.Width div 2, p.Y - Layer.Bitmap.Height div 2, Layer.Bitmap.Width, Layer.Bitmap.Height));
  r.Inflate(Layer.GlowSize, Layer.GlowSize);
  Layer.Location := r;

    // Exit
  Layer := AddButton(['sign_quit.png'], ExitGame, [VK_ESCAPE]);
  p := MakePosition(0.5, 0.5);
  r := FloatRect(SizedRect(p.X - Layer.Bitmap.Width div 2, p.Y - Layer.Bitmap.Height div 2, Layer.Bitmap.Width, Layer.Bitmap.Height));
  r.Inflate(Layer.GlowSize, Layer.GlowSize);
  Layer.Location := r;
  end;

procedure TGameMenuScreen.MakeFooterText;
var
  PackInfoText: String;
  NLInfoText: String;

  HasAuthor: Boolean;
begin
  if GameParams.CurrentLevel <> nil then
  begin
    PackInfoText := GameParams.CurrentLevel.Group.PackTitle + #13;

    HasAuthor := GameParams.CurrentLevel.Group.Author <> '';
    if HasAuthor then
      PackInfoText := PackInfoText + GameParams.CurrentLevel.Group.PackAuthor;

    if GameParams.CurrentLevel.Group.PackVersion <> '' then
    begin
      if HasAuthor then
        PackInfoText := PackInfoText + ' | ';

      PackInfoText := PackInfoText + 'Version ' + GameParams.CurrentLevel.Group.PackVersion;
    end;
  end else if GameParams.BaseLevelPack <> nil then
    PackInfoText := #13 + 'No Levels Found'
  else
    PackInfoText := #13 + 'No Pack';

  NLInfoText := SProgramName + ' V' + CurrentVersionString;
  {$ifdef exp}if COMMIT_ID <> '' then NLInfoText := NLInfoText + ':' + Uppercase(COMMIT_ID);{$endif}

  MenuFont.DrawTextCentered(ScreenImg.Bitmap, PackInfoText, LayoutInfo.FooterTextY);
  MenuFont.DrawTextCentered(ScreenImg.Bitmap, NLInfoText, LayoutInfo.FooterTextY + (3 * CHARACTER_HEIGHT));
end;

procedure TGameMenuScreen.LoadLayoutData;
var
  Parser: TParser;

  procedure ReadPositionData;
  var
    Sec: TParserSection;
  begin
    // May be called twice - first to load defaults from data/title.nxmi, second to load pack settings.
    Sec := Parser.MainSection;

    LayoutInfo.LogoY := Sec.LineNumericDefault['LOGO_CENTER_Y', LayoutInfo.LogoY];

    LayoutInfo.CardSpacingHorz := Sec.LineNumericDefault['CARDS_SPACING_X', LayoutInfo.CardSpacingHorz];
    LayoutInfo.CardSpacingVert := Sec.LineNumericDefault['CARDS_SPACING_Y', LayoutInfo.CardSpacingVert];
    LayoutInfo.CardsCenterY := Sec.LineNumericDefault['CARDS_CENTER_Y', LayoutInfo.CardsCenterY];

    LayoutInfo.GroupArrowsOffsetX := Sec.LineNumericDefault['GROUP_ARROWS_OFFSET_X', LayoutInfo.GroupArrowsOffsetX];
    LayoutInfo.GroupArrowUpOffsetY := Sec.LineNumericDefault['GROUP_ARROWS_UP_OFFSET_Y', LayoutInfo.GroupArrowUpOffsetY];
    LayoutInfo.GroupArrowDownOffsetY := Sec.LineNumericDefault['GROUP_ARROWS_DOWN_OFFSET_Y', LayoutInfo.GroupArrowDownOffsetY];

    LayoutInfo.GroupGraphicOffsetX := Sec.LineNumericDefault['GROUP_GRAPHIC_OFFSET_X', LayoutInfo.GroupGraphicOffsetX];
    LayoutInfo.GroupGraphicOffsetY := Sec.LineNumericDefault['GROUP_GRAPHIC_OFFSET_Y', LayoutInfo.GroupGraphicOffsetY];
    LayoutInfo.GroupGraphicAutoMaxWidth := Sec.LineNumericDefault['GROUP_GRAPHIC_AUTO_WIDTH_LIMIT', LayoutInfo.GroupGraphicAutoMaxWidth];
    LayoutInfo.GroupGraphicAutoMaxHeight := Sec.LineNumericDefault['GROUP_GRAPHIC_AUTO_HEIGHT_LIMIT', LayoutInfo.GroupGraphicAutoMaxHeight];

    LayoutInfo.FooterTextY := Sec.LineNumericDefault['FOOTER_TEXT_TOP_Y', LayoutInfo.FooterTextY];

    LayoutInfo.ScrollerY := Sec.LineNumericDefault['SCROLLER_TOP_Y', LayoutInfo.ScrollerY];
    LayoutInfo.ScrollerWidth := Sec.LineNumericDefault['SCROLLER_LENGTH', LayoutInfo.ScrollerWidth div CHARACTER_WIDTH] * CHARACTER_WIDTH;
    LayoutInfo.ScrollerLemmingFrames := Sec.LineNumericDefault['SCROLLER_LEMMING_FRAMES', LayoutInfo.ScrollerLemmingFrames];
  end;
begin
  Parser := TParser.Create;
  try
    FillChar(LayoutInfo, SizeOf(TGameMenuPositionData), 0);

    Parser.LoadFromFile(AppPath + SFData + 'title.nxmi');
    ReadPositionData;

    if GameParams.CurrentLevel.Group.FindFile('title.nxmi') <> '' then
    begin
      Parser.LoadFromFile(GameParams.CurrentLevel.Group.FindFile('title.nxmi'));
      ReadPositionData;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TGameMenuScreen.ForceReelForward;
begin
  if (fReelForceDirection = 1) then
    exit;

  fReelForceDirection := 1;
  fReelFreezeIterations := 0;
  fSwitchedTextSinceForce := false;
end;

procedure TGameMenuScreen.ForceReelBackward;
begin
  if (fReelForceDirection = -1) then
    exit;

  fReelForceDirection := -1;
  fReelFreezeIterations := 0;
  fSwitchedTextSinceForce := false;
end;

procedure TGameMenuScreen.LoadScrollerGraphics;
  var
  ScrollerLemmings: TBitmap32;
  r: TRect;
begin
  FLayerScroller := ScreenImg.Layers.Add<TBannerLayer>;
  FLayerBannerText := ScreenImg.Layers.Add<TBannerTextLayer>;
  FLayerWorkerLemminLeft := ScreenImg.Layers.Add<TAnimatedFrameLayer>;
  FLayerWorkerLemminRight := ScreenImg.Layers.Add<TAnimatedFrameLayer>;

  ScrollerLemmings := TBitmap32.Create;
  try
    GetGraphic('scroller_lemmings.png', ScrollerLemmings);

    // Split the bitmap into a left and a right strip

    FLayerWorkerLemminLeft.Bitmap.SetSize(ScrollerLemmings.Width div 2, ScrollerLemmings.Height);
    r := FLayerWorkerLemminLeft.Bitmap.BoundsRect;
    BlockTransfer(FLayerWorkerLemminLeft.Bitmap, 0, 0, FLayerWorkerLemminLeft.Bitmap.BoundsRect, ScrollerLemmings, r, dmOpaque);

    FLayerWorkerLemminRight.Bitmap.SetSizeFrom(FLayerWorkerLemminLeft.Bitmap);
    r := FLayerWorkerLemminRight.Bitmap.BoundsRect;
    GR32.OffsetRect(r, FLayerWorkerLemminRight.Bitmap.Width, 0);
    BlockTransfer(FLayerWorkerLemminRight.Bitmap, 0, 0, FLayerWorkerLemminRight.Bitmap.BoundsRect, ScrollerLemmings, r, dmOpaque);
  finally
    ScrollerLemmings.Free;
    end;

  FLayerWorkerLemminLeft.Scaled := True;
  // Ensure we have a whole number of frames
  FLayerWorkerLemminLeft.FrameCount := FLayerWorkerLemminLeft.Bitmap.Height div (FLayerWorkerLemminLeft.Bitmap.Height div LayoutInfo.ScrollerLemmingFrames);
  FLayerWorkerLemminLeft.Bitmap.DrawMode := dmBlend;
  FLayerWorkerLemminLeft.Action := ForceReelForward;

  FLayerWorkerLemminRight.Scaled := True;
  FLayerWorkerLemminRight.FrameCount := FLayerWorkerLemminLeft.FrameCount;
  FLayerWorkerLemminRight.Bitmap.DrawMode := dmBlend;
  FLayerWorkerLemminRight.Action := ForceReelBackward;

  FLayerWorkerLemminLeft.Location := SizedRect((GameRect.Width - LayoutInfo.ScrollerWidth) div 2 - FLayerWorkerLemminLeft.FrameWidth, LayoutInfo.ScrollerY,
    FLayerWorkerLemminLeft.FrameWidth, FLayerWorkerLemminLeft.FrameHeight);
  FLayerWorkerLemminLeft.Visible := True;

  FLayerWorkerLemminRight.Location := SizedRect((GameRect.Width + LayoutInfo.ScrollerWidth) div 2, LayoutInfo.ScrollerY,
    FLayerWorkerLemminRight.FrameWidth, FLayerWorkerLemminRight.FrameHeight);
  FLayerWorkerLemminRight.Visible := True;

  GetGraphic('scroller_segment.png', FLayerScroller.Bitmap);
  FLayerScroller.Location := FloatRect(
    FLayerWorkerLemminLeft.Location.Right, FLayerWorkerLemminLeft.Location.Top,
    FLayerWorkerLemminRight.Location.Left, FLayerWorkerLemminRight.Location.Bottom);
  FLayerScroller.Scaled := True;
  FLayerScroller.Visible := True;

  FLayerBannerText.Location := FLayerScroller.Location;
  FLayerBannerText.Scaled := True;
  FLayerBannerText.Visible := True;

end;

procedure TGameMenuScreen.PrepareScrollerTextList;
var
  i: Integer;
  Parser: TParser;
begin
  fScrollerTextList.Clear;

  Parser := TParser.Create;
  try
    Parser.LoadFromFile(AppPath + SFData + 'scroller.nxmi');
    Parser.MainSection.DoForEachLine('LINE', procedure(aLine: TParserLine; const aIteration: Integer)
    begin
      fScrollerTextList.Add(aLine.ValueTrimmed);
    end);
  finally
    Parser.Free;
  end;

  if (GameParams.CurrentLevel <> nil) and (GameParams.CurrentLevel.Group <> nil) then
    for i := 0 to GameParams.CurrentLevel.Group.ScrollerList.Count-1 do
      fScrollerTextList.Insert(i, GameParams.CurrentLevel.Group.ScrollerList[i]);
end;

procedure TGameMenuScreen.UpdateReel;
const
  MS_PER_UPDATE = 6;
var
  Updates: Integer;
  n: Integer;
begin
  Updates := (GetTickCount64 - fLastReelUpdateTickCount) div MS_PER_UPDATE;

  if Updates > 0 then
  begin
    for n := 0 to Updates-1 do
    begin
      Inc(fLastReelUpdateTickCount, MS_PER_UPDATE);
      UpdateReelIteration;
      if (fReelForceDirection <> 0) and (fReelFreezeIterations = 0) then
        UpdateReelIteration;
    end;

    DrawScroller;
  end;
end;

procedure TGameMenuScreen.UpdateReelIteration;
const
  TEXT_FREEZE_BASE_ITERATIONS = 333;
  TEXT_FREEZE_WIDTH_DIV = 3;
  TEXT_FREEZE_END_FORCE_EXTRA = 222;
begin
  if fReelFreezeIterations > 0 then
    Dec(fReelFreezeIterations)
  else begin
    if fReelForceDirection < 0 then
    begin
      Dec(fReelFrame);
      Inc(fReelTextPos);

      if fReelFrame < 0 then
        fReelFrame := fReelFrame + (LayoutInfo.ScrollerLemmingFrames * 2);
    end else begin
      Inc(fReelFrame);
      Dec(fReelTextPos);
    end;

    if (FLayerBannerText.Bitmap.Width <= LayoutInfo.ScrollerWidth) and (fReelTextPos = (LayoutInfo.ScrollerWidth - FLayerBannerText.Bitmap.Width) div 2) then
      if fReelForceDirection = 0 then
        fReelFreezeIterations := TEXT_FREEZE_BASE_ITERATIONS + (FLayerBannerText.Bitmap.Width div TEXT_FREEZE_WIDTH_DIV)
      else
      if fSwitchedTextSinceForce then
      begin
        fReelFreezeIterations := TEXT_FREEZE_BASE_ITERATIONS + TEXT_FREEZE_END_FORCE_EXTRA + (FLayerBannerText.Bitmap.Width div TEXT_FREEZE_WIDTH_DIV);
        fReelForceDirection := 0;
      end;


    if (fReelTextPos <= -FLayerBannerText.Bitmap.Width) or (fReelTextPos >= LayoutInfo.ScrollerWidth) then
      PrepareNextReelText;
  end;
end;

procedure TGameMenuScreen.PrepareNextReelText;
var
  i, realI: Integer;
  S: String;

  SizeRect: TRect;
begin
  for i := 1 to fScrollerTextList.Count do
  begin
    if i = fScrollerTextList.Count then
    begin
      fDisableScroller := True;
      Exit;
    end;

    if fReelForceDirection < 0 then
    begin
      realI := (fReelTextIndex - i);
      if realI < 0 then
        realI := realI + fScrollerTextList.Count;
    end else
      realI := (fReelTextIndex + i) mod fScrollerTextList.Count;

    if Trim(fScrollerTextList[realI]) <> '' then
    begin
      S := Trim(fScrollerTextList[realI]);
      fReelTextIndex := realI;
      Break;
    end;
  end;

  SizeRect := MenuFont.GetTextSize(S);

  FLayerBannerText.BeginUpdate;
  try
    FLayerBannerText.Bitmap.SetSize(SizeRect.Width, SizeRect.Height);
    FLayerBannerText.Bitmap.Clear(0);
    FLayerBannerText.Bitmap.DrawMode := dmBlend;

    MenuFont.DrawText(FLayerBannerText.Bitmap, S, 0, 0);

    if (fReelForceDirection < 0) then
        fReelTextPos := -FLayerBannerText.Bitmap.Width
    else
      fReelTextPos := LayoutInfo.ScrollerWidth;

    FLayerBannerText.Offset := fReelTextPos;
  finally
    FLayerBannerText.EndUpdate;
  end;

  fLastReelUpdateTickCount := GetTickCount64;
  fSwitchedTextSinceForce := True;
end;

procedure TGameMenuScreen.DrawScroller;
begin
  DrawReel;
  DrawReelText;
  DrawWorkerLemmings;
end;

procedure TGameMenuScreen.DrawReel;
begin
  FLayerScroller.OffsetX := fReelFrame;
end;

procedure TGameMenuScreen.DrawReelText;
begin
  FLayerBannerText.Offset := fReelTextPos;
end;

procedure TGameMenuScreen.DrawWorkerLemmings;
var
  Frame: Integer;
begin
  Frame := (fReelFrame div 4) mod LayoutInfo.ScrollerLemmingFrames;

  FLayerWorkerLemminLeft.Frame := Frame;
  FLayerWorkerLemminRight.Frame := Frame;
end;

procedure TGameMenuScreen.BeginGame;
begin
  if GameParams.CurrentLevel <> nil then
    CloseScreen(gstPreview);
end;

procedure TGameMenuScreen.ExitGame;
begin
  CloseScreen(gstExit);
end;

procedure TGameMenuScreen.PrevGroup;
begin
  // Repaint and wait a bit so user has time to see our UI eye-candy
  Update;
  Sleep(100);

  GameParams.PrevGroup;
  UpdateGroupSign;
end;

procedure TGameMenuScreen.NextGroup;
begin
  // Repaint and wait a bit so user has time to see our UI eye-candy
  Update;
  Sleep(100);

  GameParams.NextGroup;
  UpdateGroupSign;
end;

procedure TGameMenuScreen.UpdateGroupSign(aRedraw: Boolean);
var
  TempBmp: TBitmap32;
  Sca: Double;
begin
  if not GetGraphic('group_graphic.png', FLayerGroup.Bitmap, True, True) then
    if not GetGraphic('rank_graphic.png', FLayerGroup.Bitmap, True, True) then
    begin
      TempBmp := TBitmap32.Create;
      try
        MakeAutoSectionGraphic(TempBmp);

        if (TempBmp.Width <= LayoutInfo.GroupGraphicAutoMaxWidth) and (TempBmp.Height < LayoutInfo.GroupGraphicAutoMaxHeight) then
          Sca := 1
        else
          Sca := Min(LayoutInfo.GroupGraphicAutoMaxWidth / TempBmp.Width, LayoutInfo.GroupGraphicAutoMaxHeight / TempBmp.Height);

        FLayerGroup.Bitmap.SetSize(Round(TempBmp.Width * Sca), Round(TempBmp.Height * Sca));
        FLayerGroup.Bitmap.Clear(0);

        if Sca <> 1 then
          TLinearResampler.Create(TempBmp);

        TempBmp.DrawTo(FLayerGroup.Bitmap, FLayerGroup.Bitmap.BoundsRect);
      finally
        TempBmp.Free;
      end;
    end;

  if aRedraw then
    DrawAllClickables;
end;

function TGameMenuScreen.GetBackgroundSuffix: String;
begin
  Result := 'menu';
end;

procedure TGameMenuScreen.MakeAutoSectionGraphic(Dst: TBitmap32);
var
  S: String;
  n: Integer;
  BestMatch: Integer;
  SizeRect: TRect;
begin
  S := GameParams.CurrentLevel.Group.Name;
  if S = '' then
    S := 'N/A';

  if (Length(S) > 5) and (Pos(' ', S) > 0) then
  begin
    BestMatch := -1;
    for n := 1 to Length(S) do
      if S[n] = ' ' then
        if Abs((Length(S) / 2) - n) < Abs((Length(S) / 2) - BestMatch) then
          BestMatch := n
        else
          Break;

    if BestMatch > 0 then
      S[BestMatch] := #13;
  end;

  SizeRect := MenuFont.GetTextSize(S);
  Dst.SetSize(SizeRect.Width, SizeRect.Height);
  MenuFont.DrawTextCentered(Dst, S, 0);
end;

procedure TGameMenuScreen.ShowSetupMenu;
var
  F: TFNLSetup;
  OldFullScreen: Boolean;
  OldHighRes: Boolean;
begin
  F := TFNLSetup.Create(Self);
  try
    OldFullScreen := GameParams.FullScreen;
    OldHighRes := GameParams.HighResolution;

    F.ShowModal;

    // And apply the settings chosen
    ApplyConfigChanges(OldFullScreen, OldHighRes, False, False);
  finally
    F.Free;
  end;
end;

procedure TGameMenuScreen.DoAfterConfig;
begin
  inherited;
  ReloadCursor;
end;

procedure TGameMenuScreen.DoCleanInstallCheck;
var
  SL: TStringList;
  FMVer, CVer: Integer;
begin
  SL := TStringList.Create;
  try
    if FileExists(AppPath + 'styles\version.ini') then
    begin
      SL.LoadFromFile(AppPath + 'styles\version.ini');
      if SL.Count >= 4 then
      begin
        FMVer := StrToIntDef(SL[0], -1);
        CVer := StrToIntDef(SL[1], -1);

        if (FMVer < FORMAT_VERSION) or
           ((FMVer = FORMAT_VERSION) and (CVer < CORE_VERSION)) then
          fCleanInstallFail := True;
      end;
    end;

    SL.Clear;
    SL.Add(IntToStr(FORMAT_VERSION));
    SL.Add(IntToStr(CORE_VERSION));
    SL.Add(IntToStr(FEATURES_VERSION));
    SL.Add(IntToStr(HOTFIX_VERSION));
    {$ifdef rc}
      SL.Add('RC');
    {$else}
      {$ifdef exp}
        SL.Add('EXP');
      {$else}
        SL.Add('STABLE');
      {$endif}
    {$endif}

    ForceDirectories(AppPath + 'styles\');
    SL.SaveToFile(AppPath + 'styles\version.ini');
  finally
    SL.Free;
  end;
end;

procedure TGameMenuScreen.InitiateUpdateCheck;
begin
  GameParams.DoneUpdateCheck := True;
  if not GameParams.CheckUpdates then Exit;

  fUpdateCheckThread := DownloadInThread(VERSION_FILE, fVersionInfo);
end;

procedure TGameMenuScreen.HandleUpdateCheckResult;
var
  NewVersionStr, OrigVersionStr: String;
  SL: TStringList;
  n: Integer;
  NewestID: Int64;
  URL: String;
  F: TFManageStyles;
begin
  NewVersionStr := fVersionInfo.Values['game'];
  if LeftStr(NewVersionStr, 1) = 'V' then
    NewVersionStr := RightStr(NewVersionStr, Length(NewVersionStr)-1);

  OrigVersionStr := NewVersionStr;
  NewVersionStr := StringReplace(NewVersionStr, '-', '.', [rfReplaceAll]);

  SL := TStringList.Create;
  try
    try
      SL.Delimiter := '.';
      SL.StrictDelimiter := True;
      SL.DelimitedText := NewVersionStr;

      if SL.Count < 4 then
        SL.Add('A');

      SL[3] := Char(Ord(SL[3][1]) - 65);

      NewestID := 0;
      for n := 0 to 3 do
        NewestID := (NewestID * 1000) + StrToIntDef(SL[n], 0);

      if (NewestID > CurrentVersionID){$ifdef exp} or (NewestID = CurrentVersionID){$endif} then
      begin
        case RunCustomPopup(Self, 'Update', 'A NeoLemmix update, V' + OrigVersionStr + ', is available. Do you want to download it?',
          'Go to NeoLemmix website|Remind me later') of
          1: begin
               URL := 'https://www.neolemmix.com/?page=neolemmix';
               ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
               CloseScreen(gstExit);
             end;
           // 2: do nothing;
        end;
      end else if CheckStyleUpdates then
      begin
        // Add cursor stuff here

        case RunCustomPopup(Self, 'Styles Update', 'Styles updates are available. Do you want to download them?',
          'Open Style Manager|Remind me later') of
          1: begin
               F := TFManageStyles.Create(Self);
               try
                 F.ShowModal;
               finally
                 F.Free;
               end;
             end;
          // 2: do nothing;
        end;
      end;

    except
      // Fail silently.
    end;
  finally
    SL.Free;
  end;
end;

end.
