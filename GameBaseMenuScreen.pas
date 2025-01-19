unit GameBaseMenuScreen;

{-$define TILE_BACKGROUND}

interface

uses
  Types, UMisc,
  LemCursor,
  LemMenuFont,
  LemNeoLevelPack,
  LemNeoPieceManager,
  LemStrings,
  LemTypes,
  LemmixHotkeys,
  GameBaseScreenCommon,
  GameControl,
  GR32, GR32_Image, GR32_Layers, GR32_Resamplers,
  Generics.Collections,
  Math, Forms, Controls, ExtCtrls, Dialogs, Classes, SysUtils, Windows;

const
  INTERNAL_SCREEN_WIDTH = 864;
  INTERNAL_SCREEN_HEIGHT = 500;

  FOOTER_OPTIONS_ONE_ROW_Y = 462;

  FOOTER_OPTIONS_TWO_ROWS_HIGH_Y = 443;
  FOOTER_OPTIONS_TWO_ROWS_LOW_Y = 468;

  FOOTER_ONE_OPTION_X = INTERNAL_SCREEN_WIDTH div 2;

  FOOTER_TWO_OPTIONS_X_LEFT = INTERNAL_SCREEN_WIDTH * 5 div 16;
  FOOTER_TWO_OPTIONS_X_RIGHT = INTERNAL_SCREEN_WIDTH * 11 div 16;

  FOOTER_THREE_OPTIONS_X_LEFT = INTERNAL_SCREEN_WIDTH * 3 div 16;
  FOOTER_THREE_OPTIONS_X_MID = INTERNAL_SCREEN_WIDTH div 2;
  FOOTER_THREE_OPTIONS_X_RIGHT = INTERNAL_SCREEN_WIDTH * 13 div 16;

type
  TRegionState = (rsNormal, rsHover, rsClick);
  TRegionAction = procedure of object;

type
  TClickableLayer = class(TBitmapLayer)
  private type
    TShortcuts = TList<TShortCut>;
  private const
    HOVER_COLOR: TColor32 = $00A0A0A0; // White-ish
    CLICK_COLOR: TColor32 = $00A0A040; // Yellow-ish
  private
    FAction: TRegionAction;
    FShortCuts: TShortcuts;
    FCurrentState: TRegionState;
    FSavedState: TRegionState;
    FGlowSize: integer;
    FColorClick: TColor32;
    FColorHover: TColor32;
    FHotCount: integer;
    FParent: TClickableLayer;
    procedure SetCurrentState(const Value: TRegionState);
    function GetShortCuts: TShortcuts;
    function GetHasShortCuts: boolean;
  protected
    procedure Click; override;
    function DoHitTest(X, Y: Integer): Boolean; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Paint(Buffer: TBitmap32); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;

    function CanHandleShortCut(ShortCut: TShortcut): boolean;
    function HandleShortCut(ShortCut: TShortcut): boolean;

    property Parent: TClickableLayer read FParent write FParent;
    property Action: TRegionAction read FAction write FAction;
    property ShortcutKeys: TShortcuts read GetShortCuts;
    property HasShortcutKeys: boolean read GetHasShortCuts;
    property CurrentState: TRegionState read FCurrentState write SetCurrentState;
    property GlowSize: integer read FGlowSize write FGlowSize;
    property ColorHover: TColor32 read FColorHover write FColorHover;
    property ColorClick: TColor32 read FColorClick write FColorClick;
  end;

type
  TAnimatedFrameLayer = class(TBitmapLayer)
  private
    FFrame: integer;
    FFrameCount: integer;
    FAction: TRegionAction;
    procedure SetFrame(Value: integer);
    procedure SetFrameCount(const Value: integer);
    function GetFrameHeight: integer;
    function GetFrameWidth: integer;
  protected
    procedure Click; override;
    procedure Paint(Buffer: TBitmap32); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;

    property Action: TRegionAction read FAction write FAction;

    property FrameCount: integer read FFrameCount write SetFrameCount;
    property Frame: integer read FFrame write SetFrame;

    property FrameWidth: integer read GetFrameWidth;
    property FrameHeight: integer read GetFrameHeight;
  end;

type
  TBannerLayer = class(TBitmapLayer)
  private
    FOffsetX: integer;
    FOffsetY: integer;
    procedure SetOffsetX(Value: integer);
    procedure SetOffsetY(Value: integer);
  protected
    procedure Paint(Buffer: TBitmap32); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;

    property OffsetX: integer read FOffsetX write SetOffsetX;
    property OffsetY: integer read FOffsetY write SetOffsetY;
  end;

type
  TBannerTextLayer = class(TBitmapLayer)
  private
    FOffset: integer;
    procedure SetOffset(Value: integer);
  protected
    procedure Paint(Buffer: TBitmap32); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;

    property Offset: integer read FOffset write SetOffset;
  end;

type
  TClickableRegion = class
    private
      fBitmaps: TBitmap32;
      fBounds: TRect;
      fClickArea: TRect;
      fShortcutKeys: TList<Word>;

      fAction: TRegionAction;

      fCurrentState: TRegionState;
      fResetTimer: TTimer;

      fDrawInFrontWhenHighlit: Boolean;

      function GetSrcRect(aState: TRegionState): TRect;
    public
      constructor Create(aAction: TRegionAction; aFunc: TLemmixHotkeyAction); overload;
      constructor Create(aAction: TRegionAction; aKey: Word); overload;
      constructor Create(aCenter: TPoint; aClickRect: TRect; aAction: TRegionAction; aNormal: TBitmap32; aHover: TBitmap32 = nil; aClick: TBitmap32 = nil); overload;
      destructor Destroy; override;

      procedure AddKeysFromFunction(aFunc: TLemmixHotkeyAction);

      property Bounds: TRect read fBounds;
      property ClickArea: TRect read fClickArea;
      property Bitmaps: TBitmap32 read fBitmaps;
      property SrcRect[State: TRegionState]: TRect read GetSrcRect;
      property ShortcutKeys: TList<Word> read fShortcutKeys;

      property Action: TRegionAction read fAction;
      property CurrentState: TRegionState read fCurrentState write fCurrentState;
      property ResetTimer: TTimer read fResetTimer write fResetTimer;

      property DrawInFrontWhenHighlit: Boolean read fDrawInFrontWhenHighlit write fDrawInFrontWhenHighlit;
  end;

  TGameBaseMenuScreen = class(TGameBaseScreen)
    private
      fMenuFont          : TMenuFont;
      fKeyStates: TDictionary<Word, UInt64>;

      fBasicCursor: TNLCursor;

      fClickableRegions: TObjectList<TClickableRegion>;

      procedure LoadBasicCursor;
      procedure SetBasicCursor;

      procedure InitializeImage;

      procedure Form_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure Form_KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure Form_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure Form_MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      procedure Img_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
      procedure Img_MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

      procedure HandleKeyboardInput(Key: Word);
      procedure HandleMouseMove;
      procedure HandleMouseClick(Button: TMouseButton);

      procedure OnClickTimer(Sender: TObject);

      {$ifdef exp}{$ifndef rc}
      procedure SaveScreenImage;
      {$endif}{$endif}
    protected
      procedure CloseScreen(aNextScreen: TGameScreenType); override;

      procedure DoLevelSelect;
      procedure SaveReplay;

      procedure ShowConfigMenu;
      procedure ApplyConfigChanges(OldFullScreen, OldHighResolution, ResetWindowSize, ResetWindowPos: Boolean);
      procedure DoAfterConfig; virtual;

      function GetGraphic(aName: String; aDst: TBitmap32; aAcceptFailure: Boolean = False; aFromPackOnly: Boolean = False): Boolean;

      procedure DrawBackground; overload;
      procedure DrawBackground(aRegion: TRect); overload;

      function MakeClickableImage(aImageCenter: TPoint; aImageClickRect: TRect; aAction: TRegionAction;
                                   aNormal: TBitmap32; aHover: TBitmap32 = nil; aClick: TBitmap32 = nil): TClickableRegion;
      function MakeClickableImageAuto(aImageCenter: TPoint; aImageClickRect: TRect; aAction: TRegionAction;
                                   aNormal: TBitmap32; aMargin: Integer = -1): TClickableRegion;
      function MakeClickableText(aTextCenter: TPoint; aText: String; aAction: TRegionAction): TClickableRegion;

      function MakeHiddenOption(aKey: Word; aAction: TRegionAction): TClickableRegion; overload;
      function MakeHiddenOption(aFunc: TLemmixHotkeyAction; aAction: TRegionAction): TClickableRegion; overload;
      procedure DrawAllClickables(aForceNormalState: Boolean = False);

      function GetInternalMouseCoordinates: TPoint;

      procedure OnMouseClick(aPoint: TPoint; aButton: TMouseButton); virtual;
      procedure OnMouseMoved(aPoint: TPoint); virtual;
      procedure OnKeyPress(var aKey: Word); virtual;

      procedure AfterRedrawClickables; virtual;

      function GetBackgroundSuffix: String; virtual; abstract;

      procedure ReloadCursor;

      procedure AfterCancelLevelSelect; virtual;

      property MenuFont: TMenuFont read fMenuFont;
    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;

      procedure MainFormResized; override;
  end;

implementation

uses
  Vcl.Menus,
  LemGame, LemReplay,
  FMain, FNeoLemmixLevelSelect, FNeoLemmixConfig,
  PngInterface,
  GR32_OrdinalMaps,
  GR32.Blur;

const
  ACCEPT_KEY_DELAY = 200;

{ TGameBaseMenuScreen }

procedure TGameBaseMenuScreen.CloseScreen(aNextScreen: TGameScreenType);
begin
  OnKeyDown := nil;
  OnKeyUp := nil;
  OnMouseDown := nil;
  OnMouseMove := nil;
  ScreenImg.OnMouseDown := nil;
  ScreenImg.OnMouseMove := nil;
  inherited;
end;

constructor TGameBaseMenuScreen.Create(aOwner: TComponent);
begin
  inherited;

  fKeyStates := TDictionary<Word, UInt64>.Create;
  fClickableRegions := TObjectList<TClickableRegion>.Create;

  fMenuFont := TMenuFont.Create;
  fMenuFont.Load;

  fBasicCursor := TNLCursor.Create(Min(Screen.Width div 320, Screen.Height div 200) + EXTRA_ZOOM_LEVELS);
  LoadBasicCursor;
  SetBasicCursor;

  InitializeImage;

  OnKeyDown := Form_KeyDown;
  OnKeyUp := Form_KeyUp;
  OnMouseDown := Form_MouseDown;
  OnMouseMove := Form_MouseMove;
  ScreenImg.OnMouseUp := Img_MouseUp;
  ScreenImg.OnMouseMove := Img_MouseMove;

  {$ifdef exp}{$ifndef rc}
  MakeHiddenOption(lka_SaveImage, SaveScreenImage);
  {$endif}{$endif}
end;

{$ifdef exp}{$ifndef rc}
procedure TGameBaseMenuScreen.SaveScreenImage;
var
  i: Integer;
begin
  i := 1;
  while FileExists(AppPath + 'Screenshot_' + LeadZeroStr(i, 3) + '.png') do
    Inc(i);
  TPngInterface.SavePngFile(AppPath + 'Screenshot_' + LeadZeroStr(i, 3) + '.png', ScreenImg.Bitmap);
end;
{$endif}{$endif}

destructor TGameBaseMenuScreen.Destroy;
begin
  fMenuFont.Free;
  fBasicCursor.Free;
  fClickableRegions.Free;
  fKeyStates.Free;

  inherited;
end;

procedure TGameBaseMenuScreen.LoadBasicCursor;
var
  BMP: TBitmap32;
  i: Integer;
begin
  BMP := TBitmap32.Create;
  try
    if GameParams.HighResolution then
      TPngInterface.LoadPngFile(AppPath + 'gfx/cursor-hr/standard.png', BMP)
    else
      TPngInterface.LoadPngFile(AppPath + 'gfx/cursor/standard.png', BMP);

    fBasicCursor.LoadFromBitmap(BMP);

    for i := 1 to fBasicCursor.MaxZoom do
      Screen.Cursors[i] := fBasicCursor.GetCursor(i);
  finally
    BMP.Free;
  end;
end;

procedure TGameBaseMenuScreen.InitializeImage;
{$ifdef TILE_BACKGROUND}
var
  r: TRect;
{$endif}
begin
{$ifdef TILE_BACKGROUND}
{$else}
{$endif}

  ScreenImg.Align := alClient;

{$ifdef TILE_BACKGROUND}

  if not GetGraphic('background_' + GetBackgroundSuffix + '.png', ScreenImg.Bitmap, true) then
    GetGraphic('background.png', ScreenImg.Bitmap, true);

  ScreenImg.ScaleMode := smScale;
  ScreenImg.BitmapAlign := baTile;

  r := Rect(0, 0, INTERNAL_SCREEN_WIDTH-1, INTERNAL_SCREEN_HEIGHT-1);
  OffsetRect(r, (ScreenImg.Width - INTERNAL_SCREEN_WIDTH) div 2, (ScreenImg.Height - INTERNAL_SCREEN_HEIGHT) div 2);
  GameRect := r;

{$else}

  ScreenImg.Bitmap.SetSize(INTERNAL_SCREEN_WIDTH, INTERNAL_SCREEN_HEIGHT);
  DrawBackground;

  ScreenImg.ScaleMode := smResize;
  ScreenImg.BitmapAlign := baCenter;

  GameRect := ScreenImg.Bitmap.BoundsRect;
{$endif}

  if GameParams.LinearResampleMenu then
    TLinearResampler.Create(ScreenImg.Bitmap);
end;

procedure TGameBaseMenuScreen.MainFormResized;
begin
  ScreenImg.Width := GameParams.MainForm.ClientWidth;
  ScreenImg.Height := GameParams.MainForm.ClientHeight;
  ClientWidth := GameParams.MainForm.ClientWidth;
  ClientHeight := GameParams.MainForm.ClientHeight;

  SetBasicCursor;
end;

function TGameBaseMenuScreen.MakeClickableImage(aImageCenter: TPoint;
  aImageClickRect: TRect; aAction: TRegionAction; aNormal, aHover,
  aClick: TBitmap32): TClickableRegion;
var
  tmpNormal, tmpHover, tmpClick: TBitmap32;
  ScreenRect: TRect;
begin
  if aHover = nil then aHover := aNormal;
  if aClick = nil then aClick := aHover;

  tmpNormal := TBitmap32.Create;
  tmpHover := TBitmap32.Create;
  tmpClick := TBitmap32.Create;
  try
    tmpNormal.SetSize(aNormal.Width, aNormal.Height);
    tmpNormal.Clear(0);

    ScreenRect := SizedRect(aImageCenter.X - aNormal.Width div 2, aImageCenter.Y - aNormal.Height div 2, aNormal.Width, aNormal.Height);
    ScreenImg.Bitmap.DrawTo(tmpNormal, 0, 0, ScreenRect);

    tmpHover.Assign(tmpNormal);
    tmpClick.Assign(tmpNormal);

    aNormal.DrawTo(tmpNormal, 0, 0);
    aHover.DrawTo(tmpHover, 0, 0);
    aClick.DrawTo(tmpClick, 0, 0);

    Result := TClickableRegion.Create(aImageCenter, aImageClickRect, aAction, tmpNormal, tmpHover, tmpClick);
    fClickableRegions.Add(Result);
  finally
    tmpNormal.Free;
    tmpHover.Free;
    tmpClick.Free;
  end;
end;

function TGameBaseMenuScreen.MakeClickableImageAuto(aImageCenter: TPoint;
  aImageClickRect: TRect; aAction: TRegionAction;
  aNormal: TBitmap32; aMargin: Integer): TClickableRegion;
const
  DEFAULT_MARGIN = 5;

  HOVER_COLOR = $FFA0A0A0;
  CLICK_COLOR = $FF404040;
var
  tmpNormal, tmpHover, tmpClick: TBitmap32;
  Temp: TBitmap32;
  x, y, n: Integer;
  Intensity: Cardinal;
begin
  if aMargin < 0 then
    aMargin := DEFAULT_MARGIN;

  Temp := TBitmap32.Create;
  tmpNormal := TBitmap32.Create;
  tmpHover := TBitmap32.Create;
  tmpClick := TBitmap32.Create;
  try
    Temp.SetSize(aNormal.Width + aMargin * 2, aNormal.Height + aMargin * 2);
    Temp.Clear(0);
    Temp.DrawMode := dmBlend;

    tmpNormal.Assign(Temp);
    tmpHover.Assign(Temp);
    tmpClick.Assign(Temp);

    aNormal.DrawTo(Temp, aMargin, aMargin);

    // tmpNormal is used as a second temporary image within this loop
    for n := 1 to aMargin do
    begin
      for y := 0 to Temp.Height-1 do
        for x := 0 to Temp.Width-1 do
        begin
          Intensity := 0;

          // Diagonals
          Intensity := Intensity + ((Temp.PixelS[x - 1, y - 1] and $FF000000) shr 24);
          Intensity := Intensity + ((Temp.PixelS[x + 1, y - 1] and $FF000000) shr 24);
          Intensity := Intensity + ((Temp.PixelS[x - 1, y + 1] and $FF000000) shr 24);
          Intensity := Intensity + ((Temp.PixelS[x + 1, y + 1] and $FF000000) shr 24);

          // Straights
          Intensity := Intensity + ((Temp.PixelS[x - 1, y] and $FF000000) shr 24) * 2;
          Intensity := Intensity + ((Temp.PixelS[x + 1, y] and $FF000000) shr 24) * 2;
          Intensity := Intensity + ((Temp.PixelS[x, y - 1] and $FF000000) shr 24) * 2;
          Intensity := Intensity + ((Temp.PixelS[x, y + 1] and $FF000000) shr 24) * 2;

          Intensity := Min(Round(Intensity / 12 * 2), 255);
          tmpNormal[x, y] := Intensity shl 24;
        end;

      Temp.Assign(tmpNormal);
      tmpNormal.Clear(0);
    end;
    // end of usage of tmpNormal as a temporary image

    for y := 0 to Temp.Height-1 do
      for x := 0 to Temp.Width-1 do
      begin
        tmpHover[x, y] := (Temp[x, y] and $FF000000) or (HOVER_COLOR and $00FFFFFF);
        tmpClick[x, y] := (Temp[x, y] and $FF000000) or (CLICK_COLOR and $00FFFFFF);
      end;

    Temp.Assign(aNormal);
    Temp.DrawMode := dmBlend;

    Temp.DrawTo(tmpNormal, aMargin, aMargin);
    Temp.DrawTo(tmpHover, aMargin, aMargin);
    Temp.DrawTo(tmpClick, aMargin, aMargin);

    Types.OffsetRect(aImageClickRect, aMargin, aMargin);

    Result := MakeClickableImage(aImageCenter, aImageClickRect, aAction,
                                 tmpNormal, tmpHover, tmpClick);
  finally
    tmpNormal.Free;
    tmpHover.Free;
    tmpClick.Free;
    Temp.Free;
  end;

end;

function TGameBaseMenuScreen.MakeClickableText(aTextCenter: TPoint;
  aText: String; aAction: TRegionAction): TClickableRegion;
const
  HUE_SHIFT_NORMAL = -0.250;
  HUE_SHIFT_HOVER = -0.125;
  VALUE_SHIFT_CLICK = -0.250;
var
  tmpNormal, tmpHover, tmpClick: TBitmap32;
  ScreenRect: TRect;
  x, y: Integer;

  NormalShift, HoverShift, ClickShift: TColorDiff;
begin
  FillChar(NormalShift, SizeOf(TColorDiff), 0);
  FillChar(HoverShift, SizeOf(TColorDiff), 0);
  FillChar(ClickShift, SizeOf(TColorDiff), 0);

  NormalShift.HShift := HUE_SHIFT_NORMAL;
  HoverShift.HShift := HUE_SHIFT_HOVER;

  ClickShift.HShift := HUE_SHIFT_HOVER;
  ClickShift.VShift := VALUE_SHIFT_CLICK;

  tmpNormal := TBitmap32.Create;
  tmpHover := TBitmap32.Create;
  tmpClick := TBitmap32.Create;
  try
    ScreenRect := MenuFont.GetTextSize(aText);
    Types.OffsetRect(ScreenRect, aTextCenter.X - ScreenRect.Width div 2, aTextCenter.Y - ScreenRect.Height div 2);

    tmpNormal.SetSize(ScreenRect.Width, ScreenRect.Height);
    MenuFont.DrawText(tmpNormal, aText, 0, 0);

    tmpHover.Assign(tmpNormal);
    tmpClick.Assign(tmpNormal);

    for y := 0 to tmpNormal.Height-1 do
      for x := 0 to tmpNormal.Width-1 do
      begin
        tmpNormal[x, y] := ApplyColorShift(tmpNormal[x, y], NormalShift);
        tmpHover[x, y] := ApplyColorShift(tmpHover[x, y], HoverShift);
        tmpClick[x, y] := ApplyColorShift(tmpClick[x, y], ClickShift);
      end;

    tmpNormal.DrawMode := dmBlend;
    tmpHover.DrawMode := dmBlend;
    tmpClick.DrawMode := dmBlend;

    Result := MakeClickableImage(aTextCenter, tmpNormal.BoundsRect, aAction, tmpNormal, tmpHover, tmpClick);
  finally
    tmpNormal.Free;
    tmpHover.Free;
    tmpClick.Free;
  end;
end;

function TGameBaseMenuScreen.MakeHiddenOption(aFunc: TLemmixHotkeyAction;
  aAction: TRegionAction): TClickableRegion;
begin
  Result := TClickableRegion.Create(aAction, aFunc);
  fClickableRegions.Add(Result);
end;

function TGameBaseMenuScreen.MakeHiddenOption(aKey: Word;
  aAction: TRegionAction): TClickableRegion;
begin
  Result := TClickableRegion.Create(aAction, aKey);
  fClickableRegions.Add(Result);
end;

procedure TGameBaseMenuScreen.OnClickTimer(Sender: TObject);
var
  Region: TClickableRegion;
  P: TPoint;
begin
  if ScreenIsClosing then
    Exit;

  Region := fClickableRegions[TComponent(Sender).Tag];
  P := GetInternalMouseCoordinates;

  if Types.PtInRect(Region.ClickArea, P) then
    Region.CurrentState := rsHover
  else
    Region.CurrentState := rsNormal;

  DrawAllClickables;

  Region.ResetTimer := nil;
  Sender.Free;
end;

procedure TGameBaseMenuScreen.HandleKeyboardInput(Key: Word);
var
  i, n: Integer;
  NewTimer: TTimer;
begin
  if ScreenIsClosing then
    Exit;

  for i := 0 to fClickableRegions.Count-1 do
    for n := 0 to fClickableRegions[i].ShortcutKeys.Count-1 do
      if Key = fClickableRegions[i].ShortcutKeys[n] then
      begin
        if fClickableRegions[i].Bitmaps <> nil then
        begin
          if fClickableRegions[i].ResetTimer = nil then
          begin
            NewTimer := TTimer.Create(Self);
            NewTimer.Interval := 100;
            NewTimer.Tag := i;
            NewTimer.OnTimer := OnClickTimer;
            fClickableRegions[i].ResetTimer := NewTimer;
          end else begin
            fClickableRegions[i].ResetTimer.Enabled := False;
            fClickableRegions[i].ResetTimer.Enabled := True;
          end;

          fClickableRegions[i].CurrentState := rsClick;
          DrawAllClickables;
        end;

        fClickableRegions[i].Action;

        Key := 0;
        Exit;
      end;

  for i := 0 to ScreenImg.Layers.Count-1 do
    if (ScreenImg.Layers[i] is TClickableLayer) then
      if (TClickableLayer(ScreenImg.Layers[i]).HandleShortCut(Key)) then
      begin
        Key := 0; // TODO : This is pointless; Key isn't var parameter
        Exit;
      end;

  OnKeyPress(Key);
end;

procedure TGameBaseMenuScreen.HandleMouseMove;
var
  i: Integer;
  FoundActive: Boolean;
  StatusChanged: Boolean;

  P: TPoint;
begin
  if ScreenIsClosing then
    Exit;

  P := GetInternalMouseCoordinates;
  FoundActive := False;
  StatusChanged := False;

  for i := fClickableRegions.Count-1 downto 0 do
    if fClickableRegions[i].Bitmaps <> nil then
      if Types.PtInRect(fClickableRegions[i].ClickArea, P) and not FoundActive then
      begin
        if (fClickableRegions[i].CurrentState = rsNormal) then
        begin
          fClickableRegions[i].CurrentState := rsHover;
          StatusChanged := True;
        end;

        FoundActive := True;
      end else if (FoundActive or not Types.PtInRect(fClickableRegions[i].ClickArea, P)) and (fClickableRegions[i].CurrentState = rsHover) then
      begin
        fClickableRegions[i].CurrentState := rsNormal;
        StatusChanged := True;
      end;

  if StatusChanged then
    DrawAllClickables;

  OnMouseMoved(P); // This one we want to always call.
end;

procedure TGameBaseMenuScreen.HandleMouseClick(Button: TMouseButton);
var
  i: Integer;
  NewTimer: TTimer;

  P: TPoint;
  ExpRegion: TRect;

  InvokeCustomHandler: Boolean;
const
  DEAD_ZONE_SIZE = 6;
begin
  if ScreenIsClosing then
    Exit;

  P := GetInternalMouseCoordinates;
  InvokeCustomHandler := True;

  for i := fClickableRegions.Count-1 downto 0 do
    if fClickableRegions[i].Bitmaps <> nil then
      if Types.PtInRect(fClickableRegions[i].ClickArea, P) then
      begin
        if fClickableRegions[i].ResetTimer = nil then
        begin
          NewTimer := TTimer.Create(Self);
          NewTimer.Interval := 150;
          NewTimer.Tag := i;
          NewTimer.OnTimer := OnClickTimer;
          fClickableRegions[i].ResetTimer := NewTimer;
        end else begin
          fClickableRegions[i].ResetTimer.Enabled := False;
          fClickableRegions[i].ResetTimer.Enabled := True;
        end;

        fClickableRegions[i].CurrentState := rsClick;
        fClickableRegions[i].Bitmaps.DrawTo(ScreenImg.Bitmap, fClickableRegions[i].Bounds, fClickableRegions[i].SrcRect[rsClick]);

        fClickableRegions[i].Action;
        InvokeCustomHandler := False;
        Break;
      end else begin
        ExpRegion := fClickableRegions[i].ClickArea;
        ExpRegion.Left := ExpRegion.Left - DEAD_ZONE_SIZE;
        ExpRegion.Top := ExpRegion.Top - DEAD_ZONE_SIZE;
        ExpRegion.Right := ExpRegion.Right + DEAD_ZONE_SIZE;
        ExpRegion.Bottom := ExpRegion.Bottom + DEAD_ZONE_SIZE;

        if Types.PtInRect(ExpRegion, P) then
          InvokeCustomHandler := False;
      end;

  if InvokeCustomHandler then
    OnMouseClick(P, Button); // Only occurs if the above code didn't catch the click.
end;

procedure TGameBaseMenuScreen.OnKeyPress(var aKey: Word);
begin
  // Intentionally blank.
end;

procedure TGameBaseMenuScreen.OnMouseClick(aPoint: TPoint; aButton: TMouseButton);
begin
  // Intentionally blank.
end;

procedure TGameBaseMenuScreen.OnMouseMoved(aPoint: TPoint);
begin
  // Intentionally blank.
end;

procedure TGameBaseMenuScreen.ReloadCursor;
begin
  LoadBasicCursor;
  SetBasicCursor;
end;

procedure TGameBaseMenuScreen.AfterCancelLevelSelect;
begin
  // Intentionally blank.
end;

procedure TGameBaseMenuScreen.AfterRedrawClickables;
begin
  // Intentionally blank.
end;

procedure TGameBaseMenuScreen.SaveReplay;
var
  S: String;
begin
  GlobalGame.EnsureCorrectReplayDetails;
  S := GlobalGame.ReplayManager.GetSaveFileName(Self, rsoPostview, GlobalGame.ReplayManager);
  if S = '' then Exit;
  GlobalGame.ReplayManager.SaveToFile(S);
end;

procedure TGameBaseMenuScreen.SetBasicCursor;
var
  CursorIndex: Integer;
begin
  CursorIndex := Max(1, Min(MainForm.Width div 320, MainForm.Height div 180) div ResMod);

  Cursor := CursorIndex;
  MainForm.Cursor := CursorIndex;
  Screen.Cursor := CursorIndex;
  ScreenImg.Cursor := CursorIndex;
end;

procedure TGameBaseMenuScreen.DrawAllClickables(aForceNormalState: Boolean = False);
var
  i: Integer;
  Region: TClickableRegion;

  function CheckDrawCurrentRegion(aDrawingFront: Boolean): Boolean;
  begin
    Result := False;

    if Region.Bitmaps = nil then Exit;
    if (Region.DrawInFrontWhenHighlit and (Region.CurrentState <> rsNormal)) xor aDrawingFront then Exit;

    Result := True;
  end;
begin
  if ScreenIsClosing then
    Exit;

  if aForceNormalState then
  begin
    for i := 0 to fClickableRegions.Count-1 do
      fClickableRegions[i].CurrentState := rsNormal;
  end else
    HandleMouseMove; // To set statuses

  for i := 0 to fClickableRegions.Count-1 do
  begin
    Region := fClickableRegions[i];
    if CheckDrawCurrentRegion(False) then
      Region.Bitmaps.DrawTo(ScreenImg.Bitmap, Region.Bounds, Region.GetSrcRect(Region.CurrentState));
  end;

  for i := 0 to fClickableRegions.Count-1 do
  begin
    Region := fClickableRegions[i];
    if CheckDrawCurrentRegion(True) then
      Region.Bitmaps.DrawTo(ScreenImg.Bitmap, Region.Bounds, Region.GetSrcRect(Region.CurrentState));
  end;

  AfterRedrawClickables;
end;

function TGameBaseMenuScreen.GetGraphic(aName: String; aDst: TBitmap32; aAcceptFailure: Boolean = False; aFromPackOnly: Boolean = False): Boolean;
begin
  Result := True;

  if (not (GameParams.CurrentLevel = nil))
     and FileExists(GameParams.CurrentLevel.Group.FindFile(aName)) then
    TPngInterface.LoadPngFile(GameParams.CurrentLevel.Group.FindFile(aName), aDst)
  else if FileExists(AppPath + SFGraphicsMenu + aName) and ((not aFromPackOnly) or (not aAcceptFailure)) then // aFromPackOnly + aAcceptFailure is an invalid combination
    TPngInterface.LoadPngFile(AppPath + SFGraphicsMenu + aName, aDst)
  else begin
    if not aAcceptFailure then
      raise Exception.Create('Could not find gfx\menu\' + aName + '.');

    Result := False;
  end;

  // Work around for alpha premultiply bug in GR32 linear sampler
//(*
  var Pixel := PColor32Entry(aDst.Bits);
  for var i := 0 to aDst.Width*aDst.Height-1 do
  begin
    if (Pixel.A = 0) then
      Pixel.ARGB := 0;
    Inc(Pixel);
  end;
//*)
  aDst.DrawMode := dmBlend;
end;

procedure TGameBaseMenuScreen.DrawBackground;
begin
  DrawBackground(ScreenImg.Bitmap.BoundsRect);
end;

procedure TGameBaseMenuScreen.DrawBackground(aRegion: TRect);
var
  aX, aY: Integer;
  BgImage, Dst: TBitmap32;
  SrcRect: TRect;
begin
  Dst := ScreenImg.Bitmap;
  BgImage := TBitmap32.Create;

  try
    if not GetGraphic('background_' + GetBackgroundSuffix + '.png', BgImage, True) then
      GetGraphic('background.png', BgImage, True);

    if (BgImage.Width = 0) or (BgImage.Height = 0) then
    begin
      Dst.FillRect(aRegion.Left, aRegion.Top, aRegion.Right, aRegion.Bottom, $FF000000);
      Exit;
    end;

    aY := aRegion.Top;
    aX := aRegion.Left;
    while aY < aRegion.Bottom do
    begin
      SrcRect.Left := 0;
      SrcRect.Top := 0;
      SrcRect.Bottom := Min(BgImage.Height, aRegion.Bottom - aY);

      while aX < aRegion.Right do
      begin
        SrcRect.Right := Min(BgImage.Width, aRegion.Right - aX);

        BgImage.DrawTo(Dst, aX, aY, SrcRect);
        Inc(aX, BgImage.Width);
      end;
      Inc(aY, BgImage.Height);
      aX := aRegion.Left;
    end;
  finally
    BgImage.Free;
  end;

end;

procedure TGameBaseMenuScreen.Form_KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not fKeyStates.ContainsKey(Key) then
    fKeyStates.Add(Key, GetTickCount64);
end;

procedure TGameBaseMenuScreen.Form_KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  TimeDiff: UInt64;
begin
  if fKeyStates.ContainsKey(Key) then
  begin
    TimeDiff := GetTickCount64 - fKeyStates[Key];
    fKeyStates.Remove(Key);
    if TimeDiff <= ACCEPT_KEY_DELAY then
      HandleKeyboardInput(Key);
  end;
end;

procedure TGameBaseMenuScreen.Form_MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  HandleMouseClick(Button);
end;

procedure TGameBaseMenuScreen.Form_MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  HandleMouseMove;
end;

procedure TGameBaseMenuScreen.Img_MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  HandleMouseClick(Button);
end;

procedure TGameBaseMenuScreen.Img_MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer; Layer: TCustomLayer);
begin
  HandleMouseMove;
end;

function TGameBaseMenuScreen.GetInternalMouseCoordinates: TPoint;
begin
  Result := ScreenImg.ControlToBitmap(ScreenImg.ScreenToClient(Mouse.CursorPos));
end;

procedure TGameBaseMenuScreen.DoAfterConfig;
begin
  // Intentionally blank.
end;

procedure TGameBaseMenuScreen.DoLevelSelect;
var
  F: TFLevelSelect;
  OldLevel: TNeoLevelEntry;
  Success: Boolean;
  LoadAsPack: Boolean;

  PopupResult: Integer;
begin
  Update;

  if GameParams.TestModeLevel <> nil then Exit;

  OldLevel := GameParams.CurrentLevel;
  F := TFLevelSelect.Create(Self);
  try
    PopupResult := F.ShowModal;
    Success := PopupResult = mrOk;
    LoadAsPack := F.LoadAsPack;
  finally
    F.Free;
  end;

  if PopupResult = mrRetry then
  begin
    CloseScreen(gstReplayTest)
  end else if not Success then
  begin
    GameParams.SetLevel(OldLevel);
    AfterCancelLevelSelect;
  end else begin
    GameParams.ShownText := False;

    if LoadAsPack then
      CloseScreen(gstMenu)
    else
      CloseScreen(gstPreview);
  end;
end;

procedure TGameBaseMenuScreen.ShowConfigMenu;
var
  ConfigDlg: TFormNXConfig;
  OldFullScreen, OldHighResolution, ResetWindowSize, ResetWindowPos: Boolean;
begin
  Update;

  OldFullScreen := GameParams.FullScreen;
  OldHighResolution := GameParams.HighResolution;

  ConfigDlg := TFormNXConfig.Create(Self);
  try
    ConfigDlg.SetGameParams;
    ConfigDlg.NXConfigPages.TabIndex := 0;
    ConfigDlg.ShowModal;
    ResetWindowSize := ConfigDlg.ResetWindowSize;
    ResetWindowPos := ConfigDlg.ResetWindowPosition;
  finally
    ConfigDlg.Free;
  end;

  // Wise advice from Simon - save these things on exiting the
  // config dialog, rather than waiting for a quit or a screen
  // transition to save them.
  GameParams.Save(scImportant);

  ApplyConfigChanges(OldFullScreen, OldHighResolution, ResetWindowSize, ResetWindowPos);

  DoAfterConfig;
end;

procedure TGameBaseMenuScreen.ApplyConfigChanges(OldFullScreen, OldHighResolution, ResetWindowSize, ResetWindowPos: Boolean);
begin
  if GameParams.FullScreen and not OldFullScreen then
  begin
    GameParams.MainForm.BorderStyle := bsNone;
    GameParams.MainForm.WindowState := wsMaximized;
    GameParams.MainForm.Left := 0;
    GameParams.MainForm.Top := 0;
    GameParams.MainForm.Width := Screen.Width;
    GameParams.MainForm.Height := Screen.Height;
  end else if not GameParams.FullScreen then
  begin
    GameParams.MainForm.BorderStyle := bsSizeable;
    GameParams.MainForm.WindowState := wsNormal;

    if ResetWindowSize then TMainForm(GameParams.MainForm).RestoreDefaultSize;
    if ResetWindowPos then TMainForm(GameParams.MainForm).RestoreDefaultPosition;
  end;


  if GameParams.HighResolution <> OldHighResolution then
    PieceManager.Clear;

  if GameParams.LinearResampleMenu then
  begin
    if ScreenImg.Bitmap.Resampler is TNearestResampler then
      TLinearResampler.Create(ScreenImg.Bitmap);
  end else begin
    if ScreenImg.Bitmap.Resampler is TLinearResampler then
      TNearestResampler.Create(ScreenImg.Bitmap);
  end;

end;

{ TClickableLayer }

constructor TClickableLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  MouseEvents := True;
  AlphaHit := True;
  FColorHover := HOVER_COLOR;
  FColorClick := CLICK_COLOR;

  TCustomResamplerClass(TCustomImage32(LayerCollection.Owner).Bitmap.Resampler.ClassType).Create(Bitmap);
end;

destructor TClickableLayer.Destroy;
begin
  FShortCuts.Free;
  inherited;
end;

function TClickableLayer.DoHitTest(X, Y: Integer): Boolean;
begin
  Result := inherited;
end;

function TClickableLayer.GetHasShortCuts: boolean;
begin
  Result := (FShortCuts <> nil) and (FShortCuts.Count > 0);
end;

function TClickableLayer.GetShortCuts: TShortcuts;
begin
  if (FShortCuts = nil) then
    FShortCuts := TShortcuts.Create;

  Result := FShortCuts;
end;

function TClickableLayer.HandleShortCut(ShortCut: TShortcut): boolean;
begin
  Result := CanHandleShortCut(ShortCut);
  if (Result) then
    Click;
end;

function TClickableLayer.CanHandleShortCut(ShortCut: TShortcut): boolean;
var
  i: integer;
begin
  Result := False;

  if (HasShortcutKeys) then
  begin
    for i := 0 to FShortCuts.Count-1 do
      if (ShortCut = FShortCuts[i]) then
      begin
        Result := True;
        break;
      end;
  end;
end;

procedure TClickableLayer.KeyDown(var Key: Word; Shift: TShiftState);
var
  ShortCut: TShortCut;
begin
  inherited;

  ShortCut := Vcl.Menus.ShortCut(Key, Shift);

  if (HandleShortCut(ShortCut)) then
    Key := 0; // Handled
end;

procedure TClickableLayer.MouseEnter;
begin
  inherited;

  Inc(FHotCount);

  if (FHotCount = 1) then
  begin
    CurrentState := rsHover;
    FSavedState := rsHover;

    if (FParent <> nil) then
      FParent.MouseEnter;
  end;
end;

procedure TClickableLayer.MouseLeave;
begin
  inherited;

  if (FHotCount = 1) then
  begin
    CurrentState := rsNormal;
    FSavedState := rsNormal;

    if (FParent <> nil) then
      FParent.MouseLeave;
  end;

  Dec(FHotCount);
end;

procedure TClickableLayer.Paint(Buffer: TBitmap32);
var
  SrcRect, DstRect, ClipRect, TempRect: TRect;
  ImageRect: TRect;
  HoverBitmap: TBitmap32;
  GlowColor: TColor32;
begin
  if (Bitmap = nil) or (Bitmap.Empty) then
    Exit;

  var HoverRect := MakeRect(GetAdjustedLocation);

  var BitmapRect := Location;
  BitmapRect.Inflate(-GlowSize, -GlowSize);

  DstRect := MakeRect(GetAdjustedRect(BitmapRect));

  ClipRect := Buffer.ClipRect;
  GR32.IntersectRect(TempRect, ClipRect, HoverRect);
  if GR32.IsRectEmpty(TempRect) then
    Exit;

  SrcRect := MakeRect(0, 0, Bitmap.Width, Bitmap.Height);
  if Cropped then
  begin
    if (HoverRect.Width < 0.5) or (HoverRect.Height < 0.5) then
      Exit;
    ImageRect := TCustomImage32(LayerCollection.Owner).GetBitmapRect;
    GR32.IntersectRect(ClipRect, ClipRect, ImageRect);
  end;

  if (FCurrentState in [rsHover, rsClick]) then
  begin
    (*
    ** Draw a "hover glow" below the button bitmap
    *)
    HoverBitmap := TBitmap32.Create;
    try
      // Draw the button at the center of the hover bitmap...
      HoverBitmap.SetSize(Bitmap.Width+2*GlowSize, Bitmap.Height+2*GlowSize);
      HoverBitmap.Clear(clNone32);
      BlockTransfer(HoverBitmap, GlowSize, GlowSize, HoverBitmap.BoundsRect, Bitmap, Bitmap.BoundsRect, dmOpaque);

      // ...and blur it to produce a glow mask
      Blur32(HoverBitmap, GlowSize);

      // Replace the glow mask with a single glow color
      if (FCurrentState = rsHover) then
        GlowColor := FColorHover and $00FFFFFF
      else
        GlowColor := FColorClick and $00FFFFFF;
      var Pixel := PColor32Entry(HoverBitmap.Bits);
      for var i := 0 to HoverBitmap.Width*HoverBitmap.Height-1 do
      begin
        // Tweak the alpha to make the glow more substantial
        if (Pixel.A <> 0) then
          Pixel.ARGB := TColor32(Min(255, Pixel.A * Pixel.A * 2) shl 24) or GlowColor;
        Inc(Pixel);
      end;

      // Blur again, just a tiny bit, to soften the result
      Blur32(HoverBitmap, 2);

      // Draw the glow
      StretchTransfer(Buffer, HoverRect, ClipRect, HoverBitmap, HoverBitmap.BoundsRect, Bitmap.Resampler, Bitmap.DrawMode, Bitmap.OnPixelCombine);
    finally
      HoverBitmap.Free;
    end;
  end;

  // Draw the button
  StretchTransfer(Buffer, DstRect, ClipRect, Bitmap, SrcRect, Bitmap.Resampler, Bitmap.DrawMode, Bitmap.OnPixelCombine);
end;

procedure TClickableLayer.SetCurrentState(const Value: TRegionState);
begin
  if (FCurrentState = Value) then
    exit;
  FCurrentState := Value;
  Changed;
end;

procedure TClickableLayer.Click;
begin
  FSavedState := CurrentState;
  CurrentState := rsClick;
  try

    inherited;

    if Assigned(FAction) then
      FAction;

  finally
    CurrentState := FSavedState;
  end;
end;

{ TClickableRegion }


procedure TClickableRegion.AddKeysFromFunction(aFunc: TLemmixHotkeyAction);
var
  n: Word;
begin
  for n := 0 to MAX_KEY do
    if GameParams.Hotkeys.CheckKeyEffect(n).Action = aFunc then
      fShortcutKeys.Add(n);
end;

constructor TClickableRegion.Create(aCenter: TPoint; aClickRect: TRect; aAction: TRegionAction; aNormal, aHover, aClick: TBitmap32);
begin
  inherited Create;

  fShortcutKeys := TList<Word>.Create;

  fBitmaps := TBitmap32.Create(aNormal.Width * 3, aNormal.Height);

  fBounds := SizedRect(aCenter.X - aNormal.Width div 2, aCenter.Y - aNormal.Height div 2, aNormal.Width, aNormal.Height);
  fClickArea := SizedRect(fBounds.Left + aClickRect.Left, fBounds.Top + aClickRect.Top, aClickRect.Width, aClickRect.Height);

  if aHover = nil then aHover := aNormal;
  if aClick = nil then aClick := aHover;

  aNormal.DrawTo(fBitmaps, 0, 0);
  aHover.DrawTo(fBitmaps, aNormal.Width, 0);
  aClick.DrawTo(fBitmaps, aNormal.Width * 2, 0);

  fDrawInFrontWhenHighlit := True;

  fAction := aAction;
end;

constructor TClickableRegion.Create(aAction: TRegionAction;
  aFunc: TLemmixHotkeyAction);
begin
  inherited Create;

  fShortcutKeys := TList<Word>.Create;
  fAction := aAction;

  AddKeysFromFunction(aFunc);
end;

constructor TClickableRegion.Create(aAction: TRegionAction; aKey: Word);
begin
  inherited Create;

  fShortcutKeys := TList<Word>.Create;
  fAction := aAction;

  fShortcutKeys.Add(aKey);
end;

destructor TClickableRegion.Destroy;
begin
  fBitmaps.Free;
  fShortcutKeys.Free;
  inherited;
end;

function TClickableRegion.GetSrcRect(aState: TRegionState): TRect;
begin
  Result := Rect(0, 0, fBitmaps.Width div 3, fBitmaps.Height);

  case aState of
    rsHover: Types.OffsetRect(Result, fBitmaps.Width div 3, 0);
    rsClick: Types.OffsetRect(Result, fBitmaps.Width div 3 * 2, 0);
  end;
end;

{ TAnimatedFrameLayer }

constructor TAnimatedFrameLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  MouseEvents := True;
  AlphaHit := True;

  TCustomResamplerClass(TCustomImage32(LayerCollection.Owner).Bitmap.Resampler.ClassType).Create(Bitmap);
end;

procedure TAnimatedFrameLayer.Click;
begin
  inherited;

  if Assigned(FAction) then
    FAction;
end;

function TAnimatedFrameLayer.GetFrameHeight: integer;
begin
  Result := Bitmap.Height div FrameCount;
end;

function TAnimatedFrameLayer.GetFrameWidth: integer;
begin
  Result := Bitmap.Width;
end;

procedure TAnimatedFrameLayer.Paint(Buffer: TBitmap32);
var
  FrameHeight: integer;
  SrcRect, DstRect, ClipRect, TempRect: TRect;
  ImageRect: TRect;
begin
  if (Bitmap = nil) or (Bitmap.Empty) then
    Exit;

  if (Frame = -1) then
    Exit;

  DstRect := MakeRect(GetAdjustedLocation);
  ClipRect := Buffer.ClipRect;
  GR32.IntersectRect(TempRect, ClipRect, DstRect);
  if GR32.IsRectEmpty(TempRect) then
    Exit;

  FrameHeight := Bitmap.Height div FrameCount;
  SrcRect := MakeRect(0, 0, Bitmap.Width, FrameHeight);
  OffsetRect(SrcRect, 0, Frame * FrameHeight);

  if Cropped and (LayerCollection.Owner is TCustomImage32) then
  begin
    if (DstRect.Width < 0.5) or (DstRect.Height < 0.5) then
      Exit;
    ImageRect := TCustomImage32(LayerCollection.Owner).GetBitmapRect;
    GR32.IntersectRect(ClipRect, ClipRect, ImageRect);
  end;

  StretchTransfer(Buffer, DstRect, ClipRect, Bitmap, SrcRect, Bitmap.Resampler, Bitmap.DrawMode, Bitmap.OnPixelCombine);
end;

procedure TAnimatedFrameLayer.SetFrame(Value: integer);
begin
  Value := EnsureRange(Value, -1, FrameCount-1);

  if (Value = FFrame) then
    exit;

  FFrame := Value;
  Changed;
end;

procedure TAnimatedFrameLayer.SetFrameCount(const Value: integer);
begin
  FFrameCount := Max(0, Value);
  Frame := -1;
end;

{ TBannerLayer }

constructor TBannerLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  TCustomResamplerClass(TCustomImage32(LayerCollection.Owner).Bitmap.Resampler.ClassType).Create(Bitmap);
end;

procedure TBannerLayer.Paint(Buffer: TBitmap32);
var
  r: TFloatRect;
  UpdateRect: TRect;
  BitmapRect: TRect;
  ClipRect: TRect;
  ScaleX, ScaleY: TFloat;
  TileX, TileY: Integer;
  TileCountX, TileCountY: Integer;
  Tile: TRect;
begin
  if (Bitmap.Empty) then
    exit;

  // Area in buffer covered by layer
  UpdateRect := MakeRect(GetAdjustedLocation);
  IntersectRect(ClipRect, UpdateRect, Buffer.ClipRect);

  // Area in buffer covered by bitmap (i.e. a single tile)
  r := Location;
  r.Width := Bitmap.Width;
  r.Height := Bitmap.Height;
  BitmapRect := MakeRect(GetAdjustedRect(r));


  if (OffsetX > 0) or (OffsetY > 0) then
  begin
    LayerCollection.GetViewportScale(ScaleX, ScaleY);
    OffsetRect(BitmapRect, Round(-OffsetX * ScaleX), Round(-OffsetY * ScaleY));
  end;


  TileCountX := Ceil(UpdateRect.Width / BitmapRect.Width);
  TileCountY := Ceil(UpdateRect.Height / BitmapRect.Height);

  Tile := BitmapRect;
  for TileY := 0 to TileCountY do
  begin
    for TileX := 0 to TileCountX do
    begin
      StretchTransfer(Buffer, Tile, ClipRect, Bitmap, Bitmap.BoundsRect, Bitmap.Resampler, Bitmap.DrawMode, Bitmap.OnPixelCombine);
      GR32.OffsetRect(Tile, BitmapRect.Width, 0);
    end;

    Tile.Left := BitmapRect.Left;
    Tile.Right := BitmapRect.Right;

    GR32.OffsetRect(Tile, 0, BitmapRect.Height);
  end;
end;

procedure TBannerLayer.SetOffsetX(Value: integer);
begin
  if (Bitmap.Width <> 0) then
    Value := Value mod Bitmap.Width
  else
    Value := 0;

  if (Value = FOffsetX) then
    exit;

  FOffsetX := Value;
  Changed;
end;

procedure TBannerLayer.SetOffsetY(Value: integer);
begin
  if (Bitmap.Height <> 0) then
    Value := Value mod Bitmap.Height
  else
    Value := 0;

  if (Value = FOffsetY) then
    exit;

  FOffsetY := Value;
  Changed;
end;

{ TBannerTextLayer }

constructor TBannerTextLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  TCustomResamplerClass(TCustomImage32(LayerCollection.Owner).Bitmap.Resampler.ClassType).Create(Bitmap);
end;

procedure TBannerTextLayer.Paint(Buffer: TBitmap32);
var
  UpdateRect: TRect;
  BitmapRect: TRect;
begin
  if (Bitmap.Empty) then
    exit;

  UpdateRect := MakeRect(GetAdjustedLocation);
  BitmapRect := MakeRect(0, 0, Round(Location.Width), Round(Location.Height));
  OffsetRect(BitmapRect, -Offset, 0);

  StretchTransfer(Buffer, UpdateRect, Buffer.ClipRect, Bitmap, BitmapRect, Bitmap.Resampler, Bitmap.DrawMode, Bitmap.OnPixelCombine);
end;

procedure TBannerTextLayer.SetOffset(Value: integer);
begin
  Value := EnsureRange(Value, -Bitmap.Width, Trunc(Location.Width));

  if (Value = FOffset) then
    exit;

  FOffset := Value;
  Changed;
end;

end.
