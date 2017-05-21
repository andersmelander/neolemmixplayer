{$include lem_directives.inc}
unit GameSkillPanel;

interface

uses
  Classes, Controls, SysUtils, Math,
  GR32, GR32_Image, GR32_Layers,
  UMisc,
  Windows,
  LemmixHotkeys, LemStrings, LemTypes,
  LemLemming,
  LemDosStructures, LemDosStyle,
  LemCore, LemLevel, LemNeoTheme,
  GameControl,
  LemGame, LemRenderHelpers, //for PARTICLE_COLORS consts, not that i'm sure if it acutally needs them anymore
  GameSound,
  PngInterface,
  GameWindowInterface,
  GameBaseSkillPanel;

  {-------------------------------------------------------------------------------
    maybe this must be handled by lemgame (just bitmap writing)

  // info positions types:
  // 1. BUILDER(23)             1/14
  // 2. OUT 28                  15/23
  // 3. IN 99%                  24/31
  // 4. TIME 2-31               32/40

  -------------------------------------------------------------------------------}

type
  TSkillPanelToolbar = class(TBaseSkillPanel)
  private

  protected
    function GetButtonList: TPanelButtonArray; override;

    function PanelWidth: Integer; override;
    function PanelHeight: Integer; override;

    procedure ResizeMinimapRegion(MinimapRegion: TBitmap32); override;
    function MinimapRect: TRect; override;

    procedure CreateNewInfoString; override;
    function DrawStringLength: Integer; override;
    function DrawStringTemplate: string; override;

    // The following stuff still needs to be updated

    //procedure SetReplayMark(Status: Integer);
    //procedure SetTimeLimit(Status: Boolean); override;

  public
    constructor Create(aOwner: TComponent; aGameWindow: IGameWindow); override;
    destructor Destroy; override;

    procedure RefreshInfo; override;
  end;

const
  PANEL_WIDTH = 416;
  PANEL_HEIGHT = 40;
  COMPACT_PANEL_WIDTH = 320;
  COMPACT_PANEL_HEIGHT = 40;

  MINIMAP_X = 308;
  MINIMAP_Y = 3;
  MINIMAP_WIDTH  = 104;
  MINIMAP_HEIGHT = 34;

  COMPACT_MINIMAP_X = 212;
  COMPACT_MINIMAP_Y = 18;
  COMPACT_MINIMAP_WIDTH = 104;
  COMPACT_MINIMAP_HEIGHT = 20;

const
  MiniMapCorners: TRect = (
    Left: MINIMAP_X + 2;
    Top: MINIMAP_Y + 2;
    Right: MINIMAP_X + MINIMAP_WIDTH;
    Bottom: MINIMAP_Y + MINIMAP_HEIGHT;
  );

  CompactMinimapCorners: TRect = (
    Left: COMPACT_MINIMAP_X + 2;
    Top: COMPACT_MINIMAP_Y + 2;
    Right: COMPACT_MINIMAP_X + COMPACT_MINIMAP_WIDTH;
    Bottom: COMPACT_MINIMAP_Y + COMPACT_MINIMAP_HEIGHT;
  );


implementation

uses
  LemReplay;

function PtInRectEx(const Rect: TRect; const P: TPoint): Boolean;
begin
  Result := (P.X >= Rect.Left) and (P.X < Rect.Right) and (P.Y >= Rect.Top)
    and (P.Y < Rect.Bottom);
end;

{ TSkillPanelToolbar }

constructor TSkillPanelToolbar.Create(aOwner: TComponent; aGameWindow: IGameWindow);
begin
  inherited Create(aOwner, aGameWindow);
end;

destructor TSkillPanelToolbar.Destroy;
begin
  inherited;
end;

function TSkillPanelToolbar.PanelWidth: Integer;
begin
  if GameParams.CompactSkillPanel then
    Result := COMPACT_PANEL_WIDTH
  else
    Result := PANEL_WIDTH;
end;

function TSkillPanelToolbar.PanelHeight: Integer;
begin
  if GameParams.CompactSkillPanel then
    Result := COMPACT_PANEL_HEIGHT
  else
    Result := PANEL_HEIGHT;
end;


function TSkillPanelToolbar.DrawStringLength: Integer;
begin
  Result := 38;
end;

function TSkillPanelToolbar.DrawStringTemplate: string;
begin
  Result := '............' + '.' + ' ' + #92 + '_...' + ' ' + #93 + '_...' + ' '
                           + #94 + '_...' + ' ' + #95 +  '_.-..';
end;

procedure TSkillPanelToolbar.CreateNewInfoString;
begin
  SetInfoCursorLemming(1);
  SetReplayMark(13);
  SetInfoLemHatch(16);
  SetInfoLemAlive(22);
  SetInfoLemIn(28);
  SetTimeLimit(33);
  SetInfoTime(34, 37);
end;




procedure TSkillPanelToolbar.RefreshInfo;
var
  i: TSkillPanelButton;
  TimeRemaining: Integer;
  DoTimerBlink: Boolean;

begin
  fIsBlinkFrame := (GetTickCount mod 1000) > 499;

  CreateNewInfoString;
  DrawNewStr;
  fLastDrawnStr := fNewDrawStr;

  for i := Low(TSkillPanelButton) to LAST_SKILL_BUTTON do
    DrawSkillCount(i, Game.SkillCount[i]);

  DrawSkillCount(spbSlower, Level.Info.ReleaseRate);
  DrawSkillCount(spbFaster, Game.CurrentReleaseRate);

  if fHighlitSkill <> Game.RenderInterface.SelectedSkill then
  begin
    DrawButtonSelector(fHighlitSkill, false);
    DrawButtonSelector(Game.RenderInterface.SelectedSkill, true);
  end; // ugly code, but it's temporary

  DrawButtonSelector(spbNuke, (Game.UserSetNuking or (Game.ReplayManager.Assignment[Game.CurrentIteration, 0] is TReplayNuke)));
end;


function TSkillPanelToolbar.GetButtonList: TPanelButtonArray;
var
  ButtonList: TPanelButtonArray;
  i : Integer;
begin
  if GameParams.CompactSkillPanel then
  begin
    SetLength(ButtonList, 13);
    ButtonList[0] := spbSlower;
    ButtonList[1] := spbFaster;
    for i := 2 to 9 do
      ButtonList[i] := spbWalker; // placeholder for any skill
    ButtonList[10] := spbPause;
    ButtonList[11] := spbNuke;
    ButtonList[12] := spbFastForward;
  end
  else
  begin
    SetLength(ButtonList, 19);
    ButtonList[0] := spbSlower;
    ButtonList[1] := spbFaster;
    for i := 2 to 9 do
      ButtonList[i] := spbWalker; // placeholder for any skill
    ButtonList[10] := spbPause;
    ButtonList[11] := spbNuke;
    ButtonList[12] := spbFastForward;
    ButtonList[13] := spbRestart;
    ButtonList[14] := spbBackOneFrame;
    ButtonList[15] := spbForwardOneFrame;
    ButtonList[16] := spbClearPhysics;
    ButtonList[17] := spbDirLeft; // includes spbDirRight
    ButtonList[18] := spbLoadReplay;
  end;

  Result := ButtonList;
end;

procedure TSkillPanelToolbar.ResizeMinimapRegion(MinimapRegion: TBitmap32);
var
  TempBmp: TBitmap32;
begin
  TempBmp := TBitmap32.Create;
  TempBmp.Assign(MinimapRegion);

  if (MinimapRegion.Height <> 38) and not GameParams.CompactSkillPanel then
  begin
    MinimapRegion.SetSize(111, 38);
    MinimapRegion.Clear($FF000000);
    TempBmp.DrawTo(MinimapRegion, 0, 14);
    TempBmp.DrawTo(MinimapRegion, 0, 0, Rect(0, 0, 112, 16));
  end;

  if (MinimapRegion.Height <> 24) and GameParams.CompactSkillPanel then
  begin
    MinimapRegion.SetSize(111, 24);
    MinimapRegion.Clear($FF000000);
    TempBmp.DrawTo(MinimapRegion, 0, 0, Rect(0, 0, 112, 12));
    TempBmp.DrawTo(MinimapRegion, 0, 12, Rect(0, 26, 112, 38));
  end;

  TempBmp.Free;
end;

function TSkillPanelToolbar.MinimapRect: TRect;
begin
  if GameParams.CompactSkillPanel then
    Result := Rect(212, 18, 316, 38)
  else
    Result := Rect(308, 3, 412, 37);
end;

end.

