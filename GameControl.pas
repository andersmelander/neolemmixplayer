{$include lem_directives.inc}

unit GameControl;

{-------------------------------------------------------------------------------
  The gamecontrol class is in fact just a global var which is passed through as
  a parameter to all screens.
-------------------------------------------------------------------------------}

interface

uses
  LemNeoPieceManager,
  LemNeoLevelPack,
  LemmixHotkeys,
  Math,
  Dialogs, SysUtils, StrUtils, IOUtils, Classes, Forms, GR32,
  LemVersion,
  LemTypes, LemLevel,
  LemStrings,
  LemRendering;

var
  IsHalting: Boolean; // ONLY used during AppController's init routines. Don't use this anywhere else.
                      // Shouldn't even be used there really, but a kludgy fix is okay since that's gonna
                      // be replaced once proper level select menus are introduced. 

type
  TGameResultsRec = record
    gSuccess            : Boolean; // level played successfully?
    gCheated            : Boolean; // level cheated?
    gCount              : Integer; // number
    gToRescue           : Integer;
    gRescued            : Integer;
    gTimeIsUp           : Boolean;
    gLastRescueIteration: Integer;
    gGotTalisman        : Boolean;
    gGotNewTalisman     : Boolean;
  end;

type
  TGameScreenType = (
    gstUnknown,
    gstMenu,
    gstPreview,
    gstPlay,
    gstPostview,
    gstSounds,
    gstExit,
    gstText,
    gstReplayTest
  );

type
  TGameSoundOption = (
    gsoSound,
    gsoMusic
  );
  TGameSoundOptions = set of TGameSoundOption;

type
  TExitToPostview = (
    etpAlways,
    etpIfPassed,
    etpNever
  );

type
  TMiscOption = (
    moAutoReplaySave,
    moEnableOnline,
    moCheckUpdates,
    moLoadNextUnsolvedLevel,
    moReplayAfterBackskip,
    moReplayAfterRestart,
    moPauseAfterBackwards,
    moNoBackgrounds,
    moHideShadows,
    moHideHelpers,
    moDisableWineWarnings,
    moHighResolution,
    moLinearResampleMenu,
    moLinearResampleGame,
    moFullScreen,
    moMinimapHighQuality,
    moIncreaseZoom,
    moLoadedConfig,
    moMatchBlankReplayUsername,
    moCompactSkillPanel,
    moEdgeScroll,
    moSpawnInterval,
    moShowLevelSelectOptions,
    moFileCaching,
    moPostviewJingles,
    moForceDefaultLemmings,
    moDisableMusicInTestplay
  );

  TMiscOptions = set of TMiscOption;

const
  DEF_MISCOPTIONS = [
    moAutoReplaySave,
    moReplayAfterBackskip,
    moReplayAfterRestart,
    moPauseAfterBackwards,
    moLinearResampleMenu,
    moFullScreen,
    moMinimapHighQuality,
    moIncreaseZoom,
    moEdgeScroll,
    moShowLevelSelectOptions
  ];

type
  TGameParamsSaveCriticality = ( scNone, scImportant, scCritical );

  TDosGameParams = class(TPersistent)
  private
    fDisableSaveOptions: Boolean;
    fSaveCriticality: TGameParamsSaveCriticality;

    fHotkeys: TLemmixHotkeyManager;
    fTalismanPage: Integer;
    fDirectory    : string;
    fDumpMode : Boolean;
    fShownText: Boolean;
    fOneLevelMode: Boolean;
    fDoneUpdateCheck: Boolean;
    fCurrentLevel: TNeoLevelEntry;

    fCursorResize: Double;
    fZoomLevel: Integer;
    fPanelZoomLevel: Integer;
    fSkillQFrames: Integer;
    fWindowLeft: Integer;
    fWindowTop: Integer;
    fWindowWidth: Integer;
    fWindowHeight: Integer;
    fLoadedWindowLeft: Integer;
    fLoadedWindowTop: Integer;
    fLoadedWindowWidth: Integer;
    fLoadedWindowHeight: Integer;

    fExitToPostview: TExitToPostview;
    fUserName: String;

    fAutoSaveReplayPattern: String;
    fIngameSaveReplayPattern: String;
    fPostviewSaveReplayPattern: String;

    fMainForm: TForm; // link to the FMain form

    MiscOptions           : TMiscOptions;

    function GetOptionFlag(aFlag: TMiscOption): Boolean;
    procedure SetOptionFlag(aFlag: TMiscOption; aValue: Boolean);

    procedure LoadFromIniFile;
    procedure SaveToIniFile;

    function GetCurrentGroupName: String;

    procedure SetUserName(aValue: String);
  public
    SoundOptions   : TGameSoundOptions;

    Level        : TLevel;
    Renderer     : TRenderer;

    LevelString: String;
    BaseLevelPack: TNeoLevelGroup;


    // this is initialized by the window in which the game will be played
    TargetBitmap : TBitmap32;

    // this is initialized by the game
    GameResult: TGameResultsRec;

    // this is set by the individual screens when closing (if they know)
    NextScreen: TGameScreenType;
    NextScreen2: TGameScreenType;

    // resource vars
    LemDataInResource   : Boolean;
    LemDataOnDisk       : Boolean;
    LemSoundsInResource : Boolean;
    LemSoundsOnDisk     : Boolean;
    LemMusicInResource  : Boolean;
    LemMusicOnDisk      : Boolean;

    fZoomFactor          : Integer;
    fLevelOverride       : Integer;

    //SysDat               : TSysDatRec;
    ReplayCheckPath: String;

    TestModeLevel: TNeoLevelEntry;

    constructor Create;
    destructor Destroy; override;

    procedure Save(aCriticality: TGameParamsSaveCriticality);
    procedure Load;

    procedure SetCurrentLevelToBestMatch(aPattern: String);

    procedure CreateBasePack;

    procedure SetLevel(aLevel: TNeoLevelEntry);
    procedure NextLevel(aCanCrossRank: Boolean = False);
    procedure PrevLevel(aCanCrossRank: Boolean = False);
    procedure SetGroup(aGroup: TNeoLevelGroup);
    procedure NextGroup;
    procedure PrevGroup;
    procedure LoadCurrentLevel(NoOutput: Boolean = False); // loads level specified by CurrentLevel into Level, and prepares renderer
    procedure ReloadCurrentLevel(NoOutput: Boolean = False); // re-prepares using the existing TLevel in memory

    procedure ElevateSaveCriticality(aCriticality: TGameParamsSaveCriticality);

    property CurrentLevel: TNeoLevelEntry read fCurrentLevel write fCurrentLevel;

    property AutoSaveReplay: Boolean Index moAutoReplaySave read GetOptionFlag write SetOptionFlag;
    property EnableOnline: Boolean Index moEnableOnline read GetOptionFlag write SetOptionFlag;
    property CheckUpdates: Boolean Index moCheckUpdates read GetOptionFlag write SetOptionFlag;
    property LoadNextUnsolvedLevel: Boolean Index moLoadNextUnsolvedLevel read GetOptionFlag write SetOptionFlag;
    property ReplayAfterBackskip: Boolean Index moReplayAfterBackskip read GetOptionFlag write SetOptionFlag;
    property ReplayAfterRestart: Boolean Index moReplayAfterRestart read GetOptionFlag write SetOptionFlag;
    property PauseAfterBackwardsSkip: Boolean Index moPauseAfterBackwards read GetOptionFlag write SetOptionFlag;
    property NoBackgrounds: Boolean Index moNoBackgrounds read GetOptionFlag write SetOptionFlag;
    property HideShadows: Boolean Index moHideShadows read GetOptionFlag write SetOptionFlag;
    property HideHelpers: Boolean Index moHideHelpers read GetOptionFlag write SetOptionFlag;
    property DisableWineWarnings: Boolean Index moDisableWineWarnings read GetOptionFlag write SetOptionFlag;
    property HighResolution: Boolean Index moHighResolution read GetOptionFlag write SetOptionFlag;
    property LinearResampleMenu: Boolean Index moLinearResampleMenu read GetOptionFlag write SetOptionFlag;
    property LinearResampleGame: Boolean Index moLinearResampleGame read GetOptionFlag write SetOptionFlag;
    property FullScreen: Boolean Index moFullScreen read GetOptionFlag write SetOptionFlag;
    property MinimapHighQuality: Boolean Index moMinimapHighQuality read GetOptionFlag write SetOptionFlag;
    property IncreaseZoom: Boolean Index moIncreaseZoom read GetOptionFlag write SetOptionFlag;
    property LoadedConfig: Boolean Index moLoadedConfig read GetOptionFlag write SetOptionFlag;
    property CompactSkillPanel: Boolean Index moCompactSkillPanel read GetOptionFlag write SetOptionFlag;
    property EdgeScroll: Boolean Index moEdgeScroll read GetOptionFlag write SetOptionFlag;
    property SpawnInterval: Boolean Index moSpawnInterval read GetOptionFlag write SetOptionFlag;
    property ForceDefaultLemmings: Boolean Index moForceDefaultLemmings read GetOptionFlag write SetOptionFlag;
    property DisableMusicInTestplay: Boolean Index moDisableMusicInTestplay read GetOptionFlag write SetOptionFlag;

    property ShowLevelSelectOptions: Boolean Index moShowLevelSelectOptions read GetOptionFlag write SetOptionFlag;

    property FileCaching: Boolean Index moFileCaching read GetOptionFlag write SetOptionFlag;
    property MatchBlankReplayUsername: Boolean Index moMatchBlankReplayUsername read GetOptionFlag write SetOptionFlag;
    property PostviewJingles: Boolean Index moPostviewJingles read GetOptionFlag write SetOptionFlag;

    property DumpMode: Boolean read fDumpMode write fDumpMode;
    property OneLevelMode: Boolean read fOneLevelMode write fOneLevelMode;
    property ShownText: Boolean read fShownText write fShownText;
    property DoneUpdateCheck: Boolean read fDoneUpdateCheck write fDoneUpdateCheck;

    property Directory: string read fDirectory write fDirectory;

    property CursorResize: Double read fCursorResize write fCursorResize;
    property ZoomLevel: Integer read fZoomLevel write fZoomLevel;
    property PanelZoomLevel: Integer read fPanelZoomLevel write fPanelZoomLevel;

    property SkillQFrames: Integer read fSkillQFrames write fSkillQFrames;

    property WindowLeft: Integer read fWindowLeft write fWindowLeft;
    property WindowTop: Integer read fWindowTop write fWindowTop;
    property WindowWidth: Integer read fWindowWidth write fWindowWidth;
    property WindowHeight: Integer read fWindowHeight write fWindowHeight;

    property LoadedWindowLeft: Integer read fLoadedWindowLeft;
    property LoadedWindowTop: Integer read fLoadedWindowTop;
    property LoadedWindowWidth: Integer read fLoadedWindowWidth;
    property LoadedWindowHeight: Integer read fLoadedWindowHeight;

    property MainForm: TForm read fMainForm write fMainForm;

    property TalismanPage: Integer read fTalismanPage write fTalismanPage;

    property Hotkeys: TLemmixHotkeyManager read fHotkeys;

    property CurrentGroupName: String read GetCurrentGroupName;

    property ExitToPostview: TExitToPostview read fExitToPostview write fExitToPostview;

    property Username: String read fUsername write SetUsername;
    property AutoSaveReplayPattern: String read fAutoSaveReplayPattern write fAutoSaveReplayPattern;
    property IngameSaveReplayPattern: String read fIngameSaveReplayPattern write fIngameSaveReplayPattern;
    property PostviewSaveReplayPattern: String read fPostviewSaveReplayPattern write fPostviewSaveReplayPattern;

    property DisableSaveOptions: Boolean read fDisableSaveOptions write fDisableSaveOptions;
  published
  end;

var
  GameParams: TDosGameParams; // Easier to just globalize this than constantly pass it around everywhere


implementation

uses
  FMain,
  SharedGlobals, Controls, UITypes,
  GameBaseScreenCommon, //for EXTRA_ZOOM_LEVELS const
  GameSound;

const
  DEFAULT_REPLAY_PATTERN_INGAME = '{TITLE}__{TIMESTAMP}';
  DEFAULT_REPLAY_PATTERN_AUTO = '{TITLE}__{TIMESTAMP}';
  DEFAULT_REPLAY_PATTERN_POSTVIEW = '*{TITLE}__{TIMESTAMP}';

{ TDosGameParams }

procedure TDosGameParams.Save(aCriticality: TGameParamsSaveCriticality);
var
  i: Integer;
  Attempts: Integer;
  Success: Boolean;
begin
  ElevateSaveCriticality(aCriticality);

  if TestModeLevel <> nil then Exit;
  if fDisableSaveOptions then Exit;
  if not LoadedConfig then Exit;
  if IsHalting then Exit;

  Success := False;
  Attempts := 2;
  case fSaveCriticality of
    scImportant: Attempts := 5;
    scCritical: Attempts := 10;
  end;

  for i := 1 to Attempts do
  begin
    try
      SaveToIniFile;
      BaseLevelPack.SaveUserData;
      Hotkeys.SaveFile;
      Success := True;
    except
      Sleep(50);
    end;

    if Success then Break;
  end;

  if Success then
    fSaveCriticality := scNone
  else begin
    if fSaveCriticality = scCritical then
      ShowMessage('An error occured while trying to save data.')
    else
      Inc(fSaveCriticality);
  end;
end;

procedure TDosGameParams.Load;
begin
  if IsHalting then Exit;
  LoadFromIniFile;
  // Hotkeys automatically load when the hotkey manager is created
end;

procedure TDosGameParams.SaveToIniFile;
var
  SL, SL2: TStringList;
  LevelSavePath: String;

  procedure SaveBoolean(aLabel: String; aValue: Boolean; aValue2: Boolean = False);
  var
    NumValue: Integer;
  begin
    if aValue then
      NumValue := 1
    else
      NumValue := 0;
    if aValue2 then NumValue := NumValue + 2;
    SL.Add(aLabel + '=' + IntToStr(NumValue));
  end;

  procedure AddUnknowns;
  var
    i: Integer;
    RemoveLine: Boolean;
  begin
    for i := SL2.Count-1 downto 0 do
    begin
      RemoveLine := False;
      if SL2[i] = '' then RemoveLine := True;
      if LeftStr(SL2[i], 1) = '#' then RemoveLine := True;
      if SL.IndexOfName(SL2.Names[i]) > -1 then RemoveLine := True;

      if RemoveLine then SL2.Delete(i);
    end;

    if SL2.Count = 0 then Exit;

    SL.Add('');
    SL.Add('# Preserved unknown options');
    for i := 0 to SL2.Count-1 do
      SL.Add(SL2[i]);
  end;

  procedure SaveString(aLabel, aValue: String);
  begin
    SL.Add(aLabel + '=' + aValue);
  end;
begin
  SL := TStringList.Create;
  SL2 := TStringList.Create;

  ForceDirectories(AppPath + SFSaveData);
  if FileExists(AppPath + SFSaveData + 'settings.ini') then
    SL2.LoadFromFile(AppPath + SFSaveData + 'settings.ini')
  else if FileExists(AppPath + 'NeoLemmix147Settings.ini') then
    SL2.LoadFromFile(AppPath + 'NeoLemmix147Settings.ini');

  SL.Add('LastVersion=' + IntToStr(CurrentVersionID));
  SL.Add('UserName=' + UserName);

  SL.Add('');
  SL.Add('# Interface Options');
  SaveBoolean('AutoSaveReplay', AutoSaveReplay);
  SL.Add('AutoSaveReplayPattern=' + AutoSaveReplayPattern);
  SL.Add('IngameSaveReplayPattern=' + IngameSaveReplayPattern);
  SL.Add('PostviewSaveReplayPattern=' + PostviewSaveReplayPattern);
  SaveBoolean('ShowLevelSelectOptions', ShowLevelSelectOptions);

  if (ExitToPostview = etpAlways) then
    SaveString('ExitToPostview', 'Always')
  else if (ExitToPostview = etpIfPassed) then
    SaveString('ExitToPostview', 'IfLevelPassed')
  else if (ExitToPostview = etpNever) then
    SaveString('ExitToPostview', 'Never');

  SaveBoolean('ReplayAfterBackskip', ReplayAfterBackskip);
  SaveBoolean('ReplayAfterRestart', ReplayAfterRestart);
  SaveBoolean('PauseAfterBackwardsSkip', PauseAfterBackwardsSkip);
  SaveBoolean('NoBackgrounds', NoBackgrounds);
  SaveBoolean('ForceDefaultLemmings', ForceDefaultLemmings);
  SaveBoolean('HideShadows', HideShadows);
  SaveBoolean('HideHelpers', HideHelpers);
  SaveBoolean('CompactSkillPanel', CompactSkillPanel);
  SaveBoolean('HighQualityMinimap', MinimapHighQuality);
  SaveBoolean('EdgeScrolling', EdgeScroll);
  SaveBoolean('UseSpawnInterval', SpawnInterval);

  SL.Add('SkillQFrames=' + IntToStr(SkillQFrames));
  SL.Add('ZoomLevel=' + IntToStr(ZoomLevel));
  SL.Add('PanelZoomLevel=' + IntToStr(PanelZoomLevel));
  SL.Add('CursorResize=' + FloatToStr(CursorResize));
  SaveBoolean('IncreaseZoom', IncreaseZoom);
  SaveBoolean('FullScreen', FullScreen);

  if not FullScreen then
  begin
    SL.Add('WindowLeft=' + IntToStr(WindowLeft));
    SL.Add('WindowTop=' + IntToStr(WindowTop));
    SL.Add('WindowWidth=' + IntToStr(WindowWidth));
    SL.Add('WindowHeight=' + IntToStr(WindowHeight));
  end;

  SaveBoolean('HighResolution', HighResolution);
  SaveBoolean('LinearResampleMenu', LinearResampleMenu);
  SaveBoolean('LinearResampleGame', LinearResampleGame);

  LevelSavePath := CurrentLevel.Path;
  if Pos(AppPath + SFLevels, LevelSavePath) = 1 then
    LevelSavePath := RightStr(LevelSavePath, Length(LevelSavePath) - Length(AppPath + SFLevels));
  SL.Add('LastActiveLevel=' + LevelSavePath);

  SL.Add('');
  SL.Add('# Sound Options');
  SaveBoolean('MusicEnabled', not SoundManager.MuteMusic);
  SaveBoolean('SoundEnabled', not SoundManager.MuteSound);
  SL.Add('MusicVolume=' + IntToStr(SoundManager.MusicVolume));
  SL.Add('SoundVolume=' + IntToStr(SoundManager.SoundVolume));
  SaveBoolean('DisableTestplayMusic', DisableMusicInTestplay);
  SaveBoolean('PostviewJingles', PostviewJingles);

  SL.Add('');
  SL.Add('# Online Options');
  SaveBoolean('EnableOnline', EnableOnline);
  SaveBoolean('UpdateCheck', CheckUpdates);
  SaveBoolean('LoadNextUnsolvedLevel', LoadNextUnsolvedLevel);

  SL.Add('');
  SL.Add('# Technical Options');
  SaveBoolean('FileCaching', FileCaching);

  if UnderWine then
  begin
    SaveBoolean('DisableWineWarnings', DisableWineWarnings);
  end;

  AddUnknowns;

  SL.SaveToFile(AppPath + SFSaveData + 'settings.ini');

  SL.Free;
end;

procedure TDosGameParams.LoadFromIniFile;
var
  SL: TStringList;

  function LoadBoolean(aLabel: String; aDefault: Boolean): Boolean;
  begin
    // CANNOT load multi-saved in one for obvious reasons, those must be handled manually
    if (SL.Values[aLabel] = '0') then
      Result := False
    else if (SL.Values[aLabel] = '') then
      Result := aDefault
    else
      Result := True;
  end;

  procedure EnsureValidWindowSize;
  begin
    // Older config files might specify a zoom level of zero, to represent fullscreen.
    if ZoomLevel < 1 then
    begin
      FullScreen := True;
      ZoomLevel := Min(Screen.Width div 320 div ResMod, Screen.Height div 200 div ResMod);
    end;

    if ZoomLevel < 1 then
      ZoomLevel := 1;

    // Set window size to screen size if fullscreen. This doesn't get used directly,
    // and will be overwritten when the user changes zoom settings (unless done by
    // editing INI manually), but it keeps this function tidier.
    if FullScreen then
    begin
      WindowLeft := 0;
      WindowTop := 0;
      WindowWidth := Screen.Width;
      WindowHeight := Screen.Height;
    end;

    // If no WindowWidth or WindowHeight is specified, we want to set them so that they
    // match 416x200 x ZoomLevel exactly.
    if (WindowWidth = -1) or (WindowHeight = -1) then
    begin
      TMainForm(MainForm).RestoreDefaultSize;
      TMainForm(MainForm).RestoreDefaultPosition;
    end else begin
      if (WindowLeft = -9999) and (WindowTop = -9999) then
        TMainForm(MainForm).RestoreDefaultPosition;

      // Once we've got our window size, ensure it can fit on the screen
      if fWindowWidth > Screen.Width then
        fWindowWidth := Screen.Width;
      if fWindowHeight > Screen.Height then
        fWindowHeight := Screen.Height;
    end;

    // Disallow zoom levels that are too high
    if fZoomLevel > Min(Screen.Width div 320 div ResMod, Screen.Height div 200 div ResMod) + EXTRA_ZOOM_LEVELS then
      fZoomLevel := Min(Screen.Width div 320 div ResMod, Screen.Height div 200 div ResMod);

    // Now validate the panel zoom
    if fPanelZoomLevel < 0 then
      fPanelZoomLevel := fZoomLevel;

    if CompactSkillPanel then
      fPanelZoomLevel := Min(Screen.Width div 320 div ResMod, fPanelZoomLevel)
    else
      fPanelZoomLevel := Min(Screen.Width div 416 div ResMod, fPanelZoomLevel);

    if fPanelZoomLevel < 1 then
      fPanelZoomLevel := 1;
  end;

  procedure LoadExitToPostviewOptions;
  var
    sOption: String;
  begin
    sOption := SL.Values['ExitToPostview'];

    if (sOption = 'Always') then
      ExitToPostview := etpAlways
    else if (sOption = 'Never') then
      ExitToPostview := etpNever
    else // Set default if the string is anything else
      ExitToPostview := etpIfPassed;
  end;

  procedure ValidateSkillQFrames;
  begin
    if (SkillQFrames < 0) or (SkillQFrames > 20) then
      SkillQFrames := 15;
  end;
begin
  SL := TStringList.Create;
  try
    if FileExists(AppPath + SFSaveData + 'settings.ini') then
    begin
      SL.LoadFromFile(AppPath + SFSaveData + 'settings.ini');
      LoadedConfig := True;
    end else if UnderWine then
    begin
      // When running under WINE without an existing config, let's default to windowed.
      FullScreen := False;
      ZoomLevel := Max(Max((Screen.Width - 100) div 416 div ResMod, (Screen.Height - 100) div 200 div ResMod), 1);
      TMainForm(GameParams.MainForm).RestoreDefaultSize;
      TMainForm(GameParams.MainForm).RestoreDefaultPosition;
    end;

    UserName := SL.Values['UserName'];

    ShowLevelSelectOptions := LoadBoolean('ShowLevelSelectOptions', ShowLevelSelectOptions);
    // Bookmark - Preserve deprecated option (this can be removed later)
    if SL.Values['ShowLevelSelectOptions'] = '' then
      ShowLevelSelectOptions := not LoadBoolean('HideAdvancedOptions', ShowLevelSelectOptions);

    LoadExitToPostviewOptions;

    AutoSaveReplay := LoadBoolean('AutoSaveReplay', AutoSaveReplay);
    AutoSaveReplayPattern := SL.Values['AutoSaveReplayPattern'];
    IngameSaveReplayPattern := SL.Values['IngameSaveReplayPattern'];
    PostviewSaveReplayPattern := SL.Values['PostviewSaveReplayPattern'];

    if AutoSaveReplayPattern = '' then AutoSaveReplayPattern := DEFAULT_REPLAY_PATTERN_AUTO;
    if IngameSaveReplayPattern = '' then IngameSaveReplayPattern := DEFAULT_REPLAY_PATTERN_INGAME;
    if PostviewSaveReplayPattern = '' then PostviewSaveReplayPattern := DEFAULT_REPLAY_PATTERN_POSTVIEW;

    ReplayAfterBackskip := LoadBoolean('ReplayAfterBackskip', ReplayAfterBackskip);
    // Bookmark - Preserve deprecated option (this can be removed later)
    if SL.Values['ReplayAfterBackskip'] = '' then
      ReplayAfterBackskip := not LoadBoolean('NoAutoReplay', ReplayAfterBackskip);

    ReplayAfterRestart := LoadBoolean('ReplayAfterRestart', ReplayAfterRestart);
    PauseAfterBackwardsSkip := LoadBoolean('PauseAfterBackwardsSkip', PauseAfterBackwardsSkip);
    NoBackgrounds := LoadBoolean('NoBackgrounds', NoBackgrounds);
    ForceDefaultLemmings := LoadBoolean('ForceDefaultLemmings', ForceDefaultLemmings);
    HideShadows := LoadBoolean('HideShadows', HideShadows);
    HideHelpers := LoadBoolean('HideHelpers', HideHelpers);
    CompactSkillPanel := LoadBoolean('CompactSkillPanel', CompactSkillPanel);
    MinimapHighQuality := LoadBoolean('HighQualityMinimap', MinimapHighQuality);
    EdgeScroll := LoadBoolean('EdgeScrolling', EdgeScroll);
    IncreaseZoom := LoadBoolean('IncreaseZoom', IncreaseZoom);
    SpawnInterval := LoadBoolean('UseSpawnInterval', SpawnInterval);

    SetCurrentLevelToBestMatch(SL.Values['LastActiveLevel']);

    EnableOnline := LoadBoolean('EnableOnline', EnableOnline);
    CheckUpdates := LoadBoolean('UpdateCheck', CheckUpdates);
    LoadNextUnsolvedLevel := LoadBoolean('LoadNextUnsolvedLevel', LoadNextUnsolvedLevel);

    DisableWineWarnings := LoadBoolean('DisableWineWarnings', DisableWineWarnings);
    FileCaching := LoadBoolean('FileCaching', FileCaching);

    ZoomLevel := StrToIntDef(SL.Values['ZoomLevel'], -1);
    PanelZoomLevel := StrToIntDef(SL.Values['PanelZoomLevel'], -1);

    SkillQFrames := StrToIntDef(SL.Values['SkillQFrames'], 15);
    ValidateSkillQFrames;

    CursorResize := StrToFloatDef(SL.Values['CursorResize'], CursorResize);

    if (StrToIntDef(SL.Values['LastVersion'], 0) div 1000) mod 100 < 16 then
      FullScreen := ZoomLevel < 1;
    FullScreen := LoadBoolean('FullScreen', FullScreen);

    WindowLeft := StrToIntDef(SL.Values['WindowLeft'], -9999);
    WindowTop := StrToIntDef(SL.Values['WindowTop'], -9999);
    WindowWidth := StrToIntDef(SL.Values['WindowWidth'], -1);
    WindowHeight := StrToIntDef(SL.Values['WindowHeight'], -1);

    HighResolution := LoadBoolean('HighResolution', HighResolution);

    EnsureValidWindowSize;

    fLoadedWindowLeft := WindowLeft;
    fLoadedWindowTop := WindowTop;
    fLoadedWindowWidth := WindowWidth;
    fLoadedWindowHeight := WindowHeight;

    LinearResampleMenu := LoadBoolean('LinearResampleMenu', LinearResampleMenu);
    LinearResampleGame := LoadBoolean('LinearResampleGame', LinearResampleGame);

    if LoadBoolean('VictoryJingle', False) or LoadBoolean('FailureJingle', False) then
      PostviewJingles := True
    else
      PostviewJingles := LoadBoolean('PostviewJingles', PostviewJingles);

    DisableMusicInTestplay := LoadBoolean('DisableTestplayMusic', DisableMusicInTestplay);

    SoundManager.MuteSound := not LoadBoolean('SoundEnabled', not SoundManager.MuteSound);
    SoundManager.SoundVolume := StrToIntDef(SL.Values['SoundVolume'], 50);
    SoundManager.MuteMusic := not LoadBoolean('MusicEnabled', not SoundManager.MuteMusic);
    SoundManager.MusicVolume := StrToIntDef(SL.Values['MusicVolume'], 50);
  except
    on E: Exception do
    begin
      fDisableSaveOptions := True;
      ShowMessage('Error during settings loading:' + #10 +
                   E.ClassName + ': ' + E.Message + #10 +
                   'Default settings have been loaded. Customizations to settings during this session will not be saved.');
    end;
  end;

  SL.Free;
end;

procedure TDosGameParams.LoadCurrentLevel(NoOutput: Boolean = False);
begin
  if CurrentLevel = nil then Exit;
  if not FileExists(CurrentLevel.Path) then
  begin
    MessageDlg('Loading failed: No file at location: ' + CurrentLevel.Path, mtWarning, [mbOK], 0);
    Exit;
  end;
  Level.LoadFromFile(CurrentLevel.Path);
  PieceManager.Tidy;
  Renderer.PrepareGameRendering(Level, NoOutput);
end;

procedure TDosGameParams.ReloadCurrentLevel(NoOutput: Boolean = False);
begin
  PieceManager.Tidy;
  Renderer.PrepareGameRendering(Level, NoOutput);
end;

procedure TDosGameParams.SetLevel(aLevel: TNeoLevelEntry);
begin
  fCurrentLevel := aLevel;
end;

procedure TDosGameParams.NextLevel(aCanCrossRank: Boolean);
var
  CurLevel: TNeoLevelEntry;
  CurLevelGroup: TNeoLevelGroup;
  CurLevelIndex: Integer;
begin
  CurLevel := fCurrentLevel;
  CurLevelGroup := CurLevel.Group;
  CurLevelIndex := CurLevelGroup.LevelIndex[CurLevel];
  if CurLevelIndex = CurLevelGroup.Levels.Count-1 then
  begin
    if aCanCrossRank then
    begin
      NextGroup;
      CurLevelGroup := fCurrentLevel.Group;
    end;
    fCurrentLevel := CurLevelGroup.Levels[0];
  end else
    fCurrentLevel := CurLevelGroup.Levels[CurLevelIndex + 1];

  ShownText := False;
end;

procedure TDosGameParams.PrevLevel(aCanCrossRank: Boolean);
var
  CurLevel: TNeoLevelEntry;
  CurLevelGroup: TNeoLevelGroup;
  CurLevelIndex: Integer;
begin
  CurLevel := fCurrentLevel;
  CurLevelGroup := CurLevel.Group;
  CurLevelIndex := CurLevelGroup.LevelIndex[CurLevel];
  if CurLevelIndex = 0 then
  begin
    if aCanCrossRank then
    begin
      PrevGroup;
      CurLevelGroup := fCurrentLevel.Group;
    end;
    fCurrentLevel := CurLevelGroup.Levels[CurLevelGroup.Levels.Count-1];
  end else begin
    fCurrentLevel := CurLevelGroup.Levels[CurLevelIndex - 1];
  end;

  ShownText := False;
end;

procedure TDosGameParams.SetCurrentLevelToBestMatch(aPattern: String);
type
  TMatchType = (mtNone, mtPartial, mtFull);
var
  DeepestMatchGroup: TNeoLevelGroup;
  MatchGroup: TNeoLevelGroup;
  MatchLevel: TNeoLevelEntry;

  function GetLongestMatchIn(aPack: TNeoLevelGroup): TMatchType;
  var
    i: Integer;
  begin
    Result := mtNone;
    MatchGroup := nil;
    MatchLevel := nil;

    for i := 0 to aPack.Children.Count-1 do
      if ((MatchGroup = nil) or (Length(aPack.Children[i].Path) > Length(MatchGroup.Path))) and
         (LeftStr(aPattern, Length(aPack.Children[i].Path)) = aPack.Children[i].Path) then
      begin
        Result := mtPartial;
        MatchGroup := aPack.Children[i];
      end;

    for i := 0 to aPack.Levels.Count-1 do
      if aPack.Levels[i].Path = aPattern then
      begin
        Result := mtFull;
        MatchLevel := aPack.Levels[i];
        Exit;
      end;
  end;

  function RecursiveSearch(aPack: TNeoLevelGroup): TNeoLevelEntry;
  begin
    Result := nil;
    DeepestMatchGroup := aPack;

    case GetLongestMatchIn(aPack) of
      //mtNone: Result of "nil" sticks
      mtPartial: Result := RecursiveSearch(MatchGroup);
      mtFull: Result := MatchLevel;
    end;
  end;
begin
  // Tries to set the exact level. If the level is missing, try to set to
  // the rank it's supposedly in; or if that fails, the pack the rank is in,
  // etc. If there's no sane result whatsoever, do nothing.
  // This is used for the LastActiveLevel setting in settings.ini, and the
  // -shortcut command line parameter.

  if not TPath.IsPathRooted(aPattern) then
    aPattern := AppPath + SFLevels + aPattern;

  MatchLevel := RecursiveSearch(BaseLevelPack);

  if (MatchLevel <> nil) then
    SetLevel(MatchLevel)
  else if (DeepestMatchGroup <> nil) then
    SetLevel(DeepestMatchGroup.FirstUnbeatenLevelRecursive);
end;

procedure TDosGameParams.SetGroup(aGroup: TNeoLevelGroup);
begin
  try
    if aGroup.Levels.Count = 0 then
      SetLevel(aGroup.FirstUnbeatenLevelRecursive)
    else
      SetLevel(aGroup.FirstUnbeatenLevel);
  except
    // We don't have levels in this group
    On E : EAccessViolation do
      SetLevel(nil);
  end;
end;

procedure TDosGameParams.NextGroup;
begin
  SetLevel(CurrentLevel.Group.NextGroup.FirstUnbeatenLevel);
end;

procedure TDosGameParams.PrevGroup;
begin
  SetLevel(CurrentLevel.Group.PrevGroup.FirstUnbeatenLevel);
end;


constructor TDosGameParams.Create;
begin
  inherited Create;

  MiscOptions := DEF_MISCOPTIONS;

  UserName := 'Anonymous';

  SoundManager.MusicVolume := 50;
  SoundManager.SoundVolume := 50;
  ExitToPostview := etpIfPassed;

  fDumpMode := False;
  fShownText := False;
  fOneLevelMode := False;
  fTalismanPage := 0;
  fZoomLevel := Min(Screen.Width div 320, Screen.Height div 200);
  fPanelZoomLevel := Min(fZoomLevel, Screen.Width div 416);
  fCursorResize := 1;
  fSkillQFrames := 15;

  LemDataInResource := True;
  LemSoundsInResource := True;
  LemMusicInResource := True;

  try
    fHotkeys := TLemmixHotkeyManager.Create;
  except
    on E: Exception do
      ShowMessage('Error during hotkey loading:' + #10 +
                   E.ClassName + ': ' + E.Message + #10 +
                   'Default hotkeys have been loaded. Customizations to hotkeys during this session will not be saved.');
  end;
end;

procedure TDosGameParams.CreateBasePack;
var
  buttonSelected: Integer;
begin
  if not DirectoryExists(AppPath + SFLevels) then
  begin
    buttonSelected := MessageDlg('Could not find any levels in the folder levels\. Try to continue?',
                                 mtWarning, mbOKCancel, 0);
    if buttonSelected = mrCancel then Application.Terminate();
  end;

  try
    BaseLevelPack := TNeoLevelGroup.Create(nil, AppPath + SFLevels);
  except
    on E: Exception do
      ShowMessage('Error loading level packs and/or progression. Progress will not be saved during this session.');
  end;

  try
    SetLevel(BaseLevelPack.FirstUnbeatenLevelRecursive);
  except
    on E : EAccessViolation do
      SetLevel(nil);
  end;
end;

destructor TDosGameParams.Destroy;
begin
  fHotkeys.Free;
  BaseLevelPack.Free;
  inherited Destroy;
end;

procedure TDosGameParams.ElevateSaveCriticality(aCriticality: TGameParamsSaveCriticality);
begin
  if fSaveCriticality < aCriticality then
    fSaveCriticality := aCriticality;
end;

function TDosGameParams.GetOptionFlag(aFlag: TMiscOption): Boolean;
begin
  Result := aFlag in MiscOptions;
end;

procedure TDosGameParams.SetOptionFlag(aFlag: TMiscOption; aValue: Boolean);
begin
  if aValue then
    Include(MiscOptions, aFlag)
  else
    Exclude(MiscOptions, aFlag);
end;

procedure TDosGameParams.SetUserName(aValue: String);
begin
  if aValue = '' then
    fUsername := 'Anonymous'
  else
    fUsername := aValue;
end;

function TDosGameParams.GetCurrentGroupName: String;
begin
  Result := CurrentLevel.Group.Name;
end;

end.

