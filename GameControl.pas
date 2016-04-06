{$include lem_directives.inc}

unit GameControl;

{-------------------------------------------------------------------------------
  The gamecontrol class is in fact just a global var which is passed through as
  a parameter to all screens.
-------------------------------------------------------------------------------}

interface

uses
  LemmixHotkeys,
  Dialogs, SysUtils,
  Classes, Forms,
  GR32, GR32_Image,
  UTools,
  LemStrings,
  LemCore, LemTypes, LemLevel, LemDosStyle, LemGraphicSet, LemDosGraphicSet, LemNeoGraphicSet,
  LemDosStructures,
  LemNeoEncryption, LemNeoSave, TalisData,
  LemLevelSystem, LemRendering;

type
  TGameResultsRec = record
    gSuccess            : Boolean; // level played successfully?
    gCheated            : Boolean; // level cheated?
    gCount              : Integer; // number
    gToRescue           : Integer;
    gRescued            : Integer;
    gTarget             : Integer;
    gDone               : Integer;
    gTimeIsUp           : Boolean;
    gSecretGoto         : Integer;
    gLastRescueIteration: Integer;
    gGotTalisman        : Boolean;
  end;

type
  TGameScreenType = (
    gstUnknown,
    gstMenu,
    gstPreview,
    gstPlay,
    gstPostview,
    gstLevelCode,
    gstSounds,
    gstExit,
    gstText,
    gstTalisman
  );

type
  // how do we load the level
  TWhichLevel = (
    wlFirst,
    wlFirstSection,
    wlLevelCode,
    wlNext,
    wlSame,
    wlCongratulations,
    wlNextUnlocked,
    wlPreviousUnlocked,
    wlLastUnlocked
  );

type
  TGameSoundOption = (
    gsoSound,
    gsoMusic
  );
  TGameSoundOptions = set of TGameSoundOption;

type
  TMiscOption = (
    moGradientBridges,  // 0
    moFastForward,      // 1
    moSaveState,        // 2
    moHyperJumps,       // 3
    moCheatCodes,       // 4
    moShowParticles,    // 5
    moLookForLVLFiles,  // 6
    moDebugSteel,
    moChallengeMode,
    moTimerMode,
    moGimmickMusic, //10
    moRickrolled,
    moNoAdjustBomberMask,
    moAutoReplayNames,
    moLemmingBlink,
    moTimerBlink,
    moAutoReplaySave,
    moRescueBlink,
    moClickHighlight,
    moAltRescueBlink,
    moFixedKeys, //20
    moShowNeeded,
    moDisableFade,
    moAlwaysTimestamp,
    moConfirmOverwrite,
    moExplicitCancel,
    moWhiteOutZero,
    moIgnoreReplaySelection,
    moEnableOnline,
    moCheckUpdates,
    moNoAutoReplayMode,
    mo31
  );

  TMiscOptions = set of TMiscOption;

const
  DEF_MISCOPTIONS = [
    moGradientBridges,
    moFastForward,
    moSaveState,
    moHyperJumps,
//    moCheatCodes,
    moShowParticles,
    moGimmickMusic,
    moAutoReplayNames,
    moTimerBlink,
    moFixedKeys,
    moDisableFade,
    moAlwaysTimestamp,
    moClickHighlight,
    moEnableOnline
//    moLookForLVLFiles
//    moUseSystemCursor
  ];

type             (*
  TOptionsRec = packed record
    oSignature   : array[0..2] of Char; // LOF LemmingsOptions File
    oVersion     : Byte; // version of this record
    oRecordSize  : Integer;
    oOptions     : TMiscOptions; // 32 bits
    oSoundOptions: TGameSoundOptions; // 8 bits
  end;         *)

  TDosGameParams = class(TPersistent)
  private
    fHotkeys: TLemmixHotkeyManager;
    fTalismans : TTalismans;
    fTalismanPage: Integer;
    fDirectory    : string;
    fSteelOverride: Integer;
    fDumpMode : Boolean;
    fShownText: Boolean;
    fSaveSystem : TNeoSave;
    fOneLevelMode: Boolean;
    fDoneUpdateCheck: Boolean;
    fZoomLevel: Integer;
    {fWindowX: Integer;
    fWindowY: Integer;}

    function GetOptionFlag(aFlag: TMiscOption): Boolean;
    procedure SetOptionFlag(aFlag: TMiscOption; aValue: Boolean);

    function GetSoundFlag(aFlag: TGameSoundOption): Boolean;
    procedure SetSoundFlag(aFlag: TGameSoundOption; aValue: Boolean);

    {function GetCheatCodesEnabled: Boolean;
    procedure SetCheatCodesEnabled(Value: Boolean);
    function GetMusicEnabled: Boolean;
    function GetSoundEnabled: Boolean;
    procedure SetMusicEnabled(Value: Boolean);
    procedure SetSoundEnabled(Value: Boolean);
    function GetShowParticles: Boolean;
    procedure SetShowParticles(const Value: Boolean);
    function GetLookForLVLFiles: Boolean;
    procedure SetLookForLVLFiles(Value: Boolean);
    function GetDebugSteel: Boolean;
    procedure SetDebugSteel(Value: Boolean);
    function GetGimmickMusic: Boolean;
    procedure SetGimmickMusic(Value: Boolean);
    function GetChallengeMode: Boolean;
    procedure SetChallengeMode(Value: Boolean);
    function GetTimerMode: Boolean;
    procedure SetTimerMode(Value: Boolean);
//    function GetUsePercentages: Boolean;
//    procedure SetUsePercentages(Value: Boolean);
    function GetSteelOverride: Integer;
    procedure SetSteelOverride(Value: Integer);
    //function GetUseSystemCursor: Boolean;
    //procedure SetUseSystemCursor(const Value: Boolean);
    function GetRickrolled: Boolean;
    procedure SetRickrolled(Value: Boolean);
    function GetNoAdjustBomberMask: Boolean;
    procedure SetNoAdjustBomberMask(Value: Boolean);
    function GetAutoReplayNames: Boolean;
    procedure SetAutoReplayNames(Value: Boolean);
    function GetLemmingBlink: Boolean;
    procedure SetLemmingBlink(Value: Boolean);
    function GetTimerBlink: Boolean;
    procedure SetTimerBlink(Value: Boolean);
    function GetAutoReplaySave: Boolean;
    procedure SetAutoReplaySave(Value: Boolean);
    function GetRescueBlink: Boolean;
    procedure SetRescueBlink(Value: Boolean);
    function GetAltRescueBlink: Boolean;
    procedure SetAltRescueBlink(Value: Boolean);
    function GetClickHighlight: Boolean;
    procedure SetClickHighlight(Value: Boolean);
    function GetFixedKeys: Boolean;
    procedure SetFixedKeys(Value: Boolean);
    function GetShowNeeded: Boolean;
    procedure SetShowNeeded(Value: Boolean);
    function GetAlwaysTimestamp: Boolean;
    procedure SetAlwaysTimestamp(Value: Boolean);
    function GetConfirmOverwrite: Boolean;
    procedure SetConfirmOverwrite(Value: Boolean);
    function GetExplicitCancel: Boolean;
    procedure SetExplicitCancel(Value: Boolean);
    function GetWhiteOutZero: Boolean;
    procedure SetWhiteOutZero(Value: Boolean);
    function GetIgnoreReplaySelection: Boolean;
    procedure SetIgnoreReplaySelection(Value: Boolean);
    function GetEnableOnline: Boolean;
    procedure SetEnableOnline(Value: Boolean);
    function GetCheckUpdates: Boolean;
    procedure SetCheckUpdates(Value: Boolean);}
  public
    // this is initialized by appcontroller
    MainDatFile  : string;

    Info         : TDosGamePlayInfoRec;
    WhichLevel   : TWhichLevel;

    SoundOptions : TGameSoundOptions;

    Level        : TLevel;
    Style        : TBaseDosLemmingStyle;
    GraphicSet   : TBaseNeoGraphicSet;
    Renderer     : TRenderer;

    LevelString : String;

    // Merging from a newer, gimmick-free version deleted some needed
    // variables. Let's put them back in a kludgey form.
    ForceGimmick: LongWord;
    ForceGimmick2: LongWord;
    ForceGimmick3: LongWord;
    Rickrolled: Boolean;
    UsePercentages: Integer;
    

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

    // cheat
//    Cheatmode: Boolean; // levelcode screen
    MiscOptions         : TMiscOptions;

    fZoomFactor          : Integer;
    fForceSkillset       : Word;
    fLevelOverride       : Integer;
    fPercentOption       : Integer;

    fTestMode : Boolean;
    fTestGroundFile : String;
    fTestVgagrFile : String;
    fTestVgaspecFile : String;
    fTestLevelFile : String;

    fTestScreens: Integer;

    SysDat               : TSysDatRec;

    // mass replay check stuff
    ReplayResultList: TStringList;
    ReplayCheckIndex: Integer;

    constructor Create;
    destructor Destroy; override;
    procedure LoadFromIniFile;
    procedure SaveToIniFile;
    property SaveSystem: TNeoSave read fSaveSystem;

    property MusicEnabled: Boolean Index gsoMusic read GetSoundFlag write SetSoundFlag;
    property SoundEnabled: Boolean Index gsoSound read GetSoundFlag write SetSoundFlag;

    property CheatCodesEnabled: Boolean Index moCheatCodes read GetOptionFlag write SetOptionFlag;
    property LookForLVLFiles: Boolean Index moLookForLVLFiles read GetOptionFlag write SetOptionFlag;
    property DebugSteel: Boolean Index moDebugSteel read GetOptionFlag write SetOptionFlag;
    property ChallengeMode: Boolean Index moChallengeMode read GetOptionFlag write SetOptionFlag;
    property TimerMode: Boolean Index moTimerMode read GetOptionFlag write SetOptionFlag;
    property ClickHighlight: Boolean Index moClickHighlight read GetOptionFlag write SetOptionFlag;
    property AutoReplayNames: Boolean Index moAutoReplayNames read GetOptionFlag write SetOptionFlag;
    property AutoSaveReplay: Boolean Index moAutoReplaySave read GetOptionFlag write SetOptionFlag;
    property LemmingBlink: Boolean Index moLemmingBlink read GetOptionFlag write SetOptionFlag;
    property TimerBlink: Boolean Index moTimerBlink read GetOptionFlag write SetOptionFlag;
    property AlwaysTimestamp: boolean Index moAlwaysTimestamp read GetOptionFlag write SetOptionFlag;
    property ConfirmOverwrite: boolean Index moConfirmOverwrite read GetOptionFlag write SetOptionFlag;
    property ExplicitCancel: boolean Index moExplicitCancel read GetOptionFlag write SetOptionFlag;
    property WhiteOutZero: boolean Index moWhiteOutZero read GetOptionFlag write SetOptionFlag;
    property IgnoreReplaySelection: boolean Index moIgnoreReplaySelection read GetOptionFlag write SetOptionFlag;
    property EnableOnline: boolean Index moEnableOnline read GetOptionFlag write SetOptionFlag;
    property CheckUpdates: boolean Index moCheckUpdates read GetOptionFlag write SetOptionFlag;
    property NoAutoReplayMode: boolean Index moNoAutoReplayMode read GetOptionFlag write SetOptionFlag;

    property DumpMode: boolean read fDumpMode write fDumpMode;
    property OneLevelMode: boolean read fOneLevelMode write fOneLevelMode;
    property ShownText: boolean read fShownText write fShownText;

    property DoneUpdateCheck: Boolean read fDoneUpdateCheck write fDoneUpdateCheck;

    property Directory: string read fDirectory write fDirectory;
    property ForceSkillset: Word read fForceSkillset write fForceSkillset;
    property QuickTestMode: Integer read fTestScreens write fTestScreens;
    property ZoomLevel: Integer read fZoomLevel write fZoomLevel;
    {property WindowX: Integer read fWindowX write fWindowX;
    property WindowY: Integer read fWindowY write fWindowY;}

    property Talismans: TTalismans read fTalismans;
    property TalismanPage: Integer read fTalismanPage write fTalismanPage;

    property Hotkeys: TLemmixHotkeyManager read fHotkeys;
  published
  end;


implementation

{ TDosGameParams }

procedure TDosGameParams.SaveToIniFile;
var
  SL: TStringList;

  procedure SaveBoolean(aLabel: String; aValue: Boolean; aValue2: Boolean = false);
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
begin
  //if fTestMode then Exit;
  SL := TStringList.Create;

  SL.Add('LastVersion=' + IntToStr(Cur_MainVer) + IntToStr(Cur_SubVer) + IntToStr(Cur_MinorVer));

  SaveBoolean('MusicEnabled', MusicEnabled);
  SaveBoolean('SoundEnabled', SoundEnabled);
  SaveBoolean('ClickHighlight', ClickHighlight);
  SaveBoolean('IgnoreReplaySelection', IgnoreReplaySelection);
  SaveBoolean('AutoReplayNames', AutoReplayNames);
  SaveBoolean('AutoSaveReplay', AutoSaveReplay);
  SaveBoolean('AlwaysTimestampReplays', AlwaysTimestamp);
  SaveBoolean('ConfirmReplayOverwrite', ConfirmOverwrite);
  SaveBoolean('ExplicitReplayCancel', ExplicitCancel);
  SaveBoolean('NoAutoReplay', NoAutoReplayMode);
  SaveBoolean('LemmingCountBlink', LemmingBlink);
  SaveBoolean('TimerBlink', TimerBlink);
  SaveBoolean('WhiteOutZero', WhiteOutZero);
  SaveBoolean('EnableOnline', EnableOnline);
  SaveBoolean('UpdateCheck', CheckUpdates);

  SL.Add('ZoomLevel=' + IntToStr(ZoomLevel));


  SL.SaveToFile(ExtractFilePath(ParamStr(0)) + 'NeoLemmixSettings.ini');

  SL.Free;
end;

procedure TDosGameParams.LoadFromIniFile;
var
  SL: TStringList;

  function LoadBoolean(aLabel: String): Boolean;
  begin
    // CANNOT load multi-saved in one for obvious reasons, those must be handled manually
    if (SL.Values[aLabel] = '0') or (SL.Values[aLabel] = '') then
      Result := false
    else
      Result := true;
  end;

begin
  if not FileExists(ExtractFilePath(ParamStr(0)) + 'NeoLemmixSettings.ini') then Exit;
  SL := TStringList.Create;
  SL.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'NeoLemmixSettings.ini');

  MusicEnabled := LoadBoolean('MusicEnabled');
  SoundEnabled := LoadBoolean('SoundEnabled');
  ClickHighlight := LoadBoolean('ClickHighlight');
  AutoReplayNames := LoadBoolean('AutoReplayNames');
  AutoSaveReplay := LoadBoolean('AutoSaveReplay');
  LemmingBlink := LoadBoolean('LemmingCountBlink');
  TimerBlink := LoadBoolean('TimerBlink');
  AlwaysTimestamp := LoadBoolean('AlwaysTimestampReplays');
  ConfirmOverwrite := LoadBoolean('ConfirmReplayOverwrite');
  ExplicitCancel := LoadBoolean('ExplicitReplayCancel');
  NoAutoReplayMode := LoadBoolean('NoAutoReplay');
  WhiteOutZero := LoadBoolean('WhiteOutZero');
  IgnoreReplaySelection := LoadBoolean('IgnoreReplaySelection');
  EnableOnline := LoadBoolean('EnableOnline');
  CheckUpdates := LoadBoolean('UpdateCheck');

  ZoomLevel := 0; // always fullscreen in this version

  if StrToIntDef(SL.Values['LastVersion'], 0) < 1421 then
    EnableOnline := true;

  SL.Free;
end;


constructor TDosGameParams.Create;
var
  TempStream: TMemoryStream; //for loading talisman data
begin
  inherited Create;

  MiscOptions := DEF_MISCOPTIONS;
  fForceSkillset := 0;
  fSteelOverride := 0;
  fPercentOption := -1;
  fDumpMode := false;
  fTestScreens := 0;
  fShownText := false;
  fOneLevelMode := false;
  fTalismanPage := 0;
  fZoomLevel := 0;

  LemDataInResource := True;
  LemSoundsInResource := True;
  LemMusicInResource := True;

  fSaveSystem := TNeoSave.Create;
  fTalismans := TTalismans.Create;


  try
    TempStream := CreateDataStream('talisman.dat', ldtLemmings);
    fTalismans.LoadFromStream(TempStream);
    TempStream.Free;
  except
    // Silent fail. It's okay - and in fact common - for this file to be missing.
  end;

  fTalismans.SortTalismans;

  fSaveSystem.SetTalismans(fTalismans);

  fHotkeys := TLemmixHotkeyManager.Create;

  ReplayCheckIndex := -2;
  ReplayResultList := TStringList.Create;

end;

destructor TDosGameParams.Destroy;
begin
  fSaveSystem.Free;
  fTalismans.Free;
  fHotkeys.Free;
  ReplayResultList.Free;
  inherited Destroy;
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

function TDosGameParams.GetSoundFlag(aFlag: TGameSoundOption): Boolean;
begin
  Result := aFlag in SoundOptions;
end;

procedure TDosGameParams.SetSoundFlag(aFlag: TGameSoundOption; aValue: Boolean);
begin
  if aValue then
    Include(SoundOptions, aFlag)
  else
    Exclude(SoundOptions, aFlag);
end;

end.

