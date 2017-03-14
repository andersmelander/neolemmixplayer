{$include lem_directives.inc}

{-------------------------------------------------------------------------------
  Some source code notes:

  � Transition() method: It has a default parameter. So if you see a
    call to Transition() with three parameters and the last one is TRUE, it means
    that the lemming has to turn around as well. I commented this too at all
    places where this happens.
-------------------------------------------------------------------------------}

unit LemGame;

interface

uses
  SharedGlobals, PngInterface,
  Windows, Classes, Contnrs, SysUtils, Math, Forms, Dialogs,
  TalisData,
  Controls, StrUtils, UMisc,
  GR32, GR32_OrdinalMaps,
  LemCore, LemTypes, LemDosStructures, LemStrings,
  LemMetaObject, LemLevel, LemStyle,
  LemRenderHelpers, LemRendering,
  LemNeoTheme,
  LemObjects, LemLemming, LemRecolorSprites,
  LemReplay,
  LemGameMessageQueue,
  GameControl;

const
  ParticleColorIndices: array[0..15] of Byte =
    (4, 15, 14, 13, 12, 11, 10, 9, 8, 11, 10, 9, 8, 7, 6, 2);

  AlwaysAnimateObjects = [DOM_NONE, DOM_EXIT, DOM_FORCELEFT, DOM_FORCERIGHT,
        DOM_WATER, DOM_FIRE, DOM_ONEWAYLEFT, DOM_ONEWAYRIGHT, DOM_RADIATION,
        DOM_ONEWAYDOWN, DOM_UPDRAFT, DOM_SLOWFREEZE, DOM_HINT, DOM_NOSPLAT,
        DOM_SPLAT, DOM_BACKGROUND];

const
  // never change, do NOT trust the bits are the same as the enumerated type.

  //Recorded Action Flags
	//raf_StartPause        = Bit0;
	//raf_EndPause          = Bit1;
	//raf_Pausing           = Bit2;
	//raf_StartIncreaseRR   = Bit3;  // only allowed when not pausing
	//raf_StartDecreaseRR   = Bit4;  // only allowed when not pausing
	//raf_StopChangingRR    = Bit5;  // only allowed when not pausing
	//raf_SkillSelection    = Bit6;
	raf_SkillAssignment   = 1 shl 7; // Bit7;
	raf_Nuke              = 1 shl 8; // Bit8;  // only allowed when not pausing, as in the game
  //raf_NewNPLemming      = Bit9;  // related to emulation of right-click bug
  //raf_RR99              = Bit10;
  //raf_RRmin             = Bit11;

type
  TLemmingGame = class;

  TLemmingGameSavedState = class
    public
      LemmingList: TLemmingList;
      SelectedSkill: TSkillPanelButton;
      TerrainLayer: TBitmap32;  // the visual terrain image
      PhysicsMap: TBitmap32;    // the actual physics
      ZombieMap: TByteMap; // Still needed for now, because there is no proper method to set the ZombieMap
      CurrentIteration: Integer;
      ClockFrame: Integer;
      ButtonsRemain: Integer;
      LemmingsToRelease: Integer;
      LemmingsCloned: Integer;
      LemmingsOut: Integer;
      fSpawnedDead: Integer;
      LemmingsIn: Integer;
      LemmingsRemoved: Integer;
      NextLemmingCountdown: Integer;
      DelayEndFrames: Integer;
      TimePlay: Integer;
      EntriesOpened: Boolean;
      ObjectInfos: TInteractiveObjectInfoList;
      LowestReleaseRate: Integer;
      HighestReleaseRate: Integer;
      CurrReleaseRate: Integer;

      CurrSkillCount: array[TBasicLemmingAction] of Integer;  // should only be called with arguments in AssignableSkills
      UsedSkillCount: array[TBasicLemmingAction] of Integer;  // should only be called with arguments in AssignableSkills

      UserSetNuking: Boolean;
      ExploderAssignInProgress: Boolean;
      Index_LemmingToBeNuked: Integer;
      constructor Create;
      destructor Destroy; override;
  end;

  TLemmingGameSavedStateList = class(TObjectList)
    private
      function GetItem(Index: Integer): TLemmingGameSavedState;
    public
      procedure TidyList(aCurrentIteration: Integer);
      function FindNearestState(aTargetIteration: Integer): Integer;
      procedure ClearAfterIteration(aTargetIteration: Integer);
      function Add: TLemmingGameSavedState;
      property Items[Index: Integer]: TLemmingGameSavedState read GetItem; default;
      property List;
  end;


  TLemmingMethod = function (L: TLemming): Boolean of object;
  TLemmingMethodArray = array[TBasicLemmingAction] of TLemmingMethod;

  TNewSkillMethod = function (L: TLemming): Boolean of object;
  TNewSkillMethodArray = array[TBasicLemmingAction] of TNewSkillMethod;

  TLemmingEvent = procedure (L: TLemming) of object;

  TLemmingGame = class(TComponent)
  private
    fRenderInterface           : TRenderInterface;
    fMessageQueue              : TGameMessageQueue;

    fTalismans                 : TTalismans;
    fTalismanReceived          : Boolean;

    fLastReplayDir             : String;

    fSelectedSkill             : TSkillPanelButton; // TUserSelectedSkill; // currently selected skill restricted by F3-F9

  { internal objects }
    LemmingList                : TLemmingList; // the list of lemmings
    PhysicsMap                 : TBitmap32;
    BlockerMap                 : TByteMap; // for blockers (and force fields)
    ZombieMap                  : TByteMap;
    ExitMap                    : TArrayArrayBoolean;
    LockedExitMap              : TArrayArrayBoolean;
    WaterMap                   : TArrayArrayBoolean;
    FireMap                    : TArrayArrayBoolean;
    TrapMap                    : TArrayArrayBoolean;
    TeleporterMap              : TArrayArrayBoolean;
    UpdraftMap                 : TArrayArrayBoolean;
    PickupMap                  : TArrayArrayBoolean;
    ButtonMap                  : TArrayArrayBoolean;
    FlipperMap                 : TArrayArrayBoolean;
    RadiationMap               : TArrayArrayBoolean;
    SlowfreezeMap              : TArrayArrayBoolean;
    SplatMap                   : TArrayArrayBoolean;
    AntiSplatMap               : TArrayArrayBoolean;
    AnimationMap               : TArrayArrayBoolean;


    fReplayManager             : TReplay;

  { reference objects, mostly for easy access in the mechanics-code }
    fRenderer                  : TRenderer; // ref to gameparams.renderer
    fLevel                     : TLevel; // ref to gameparams.level
    Style                      : TBaseLemmingStyle; // ref to gameparams.style

  { masks }
    BomberMask                 : TBitmap32;
    StonerMask                 : TBitmap32;
    BasherMasks                : TBitmap32;
    FencerMasks                : TBitmap32;
    MinerMasks                  : TBitmap32;
    fMasksLoaded               : Boolean;

  { vars }
    fCurrentIteration          : Integer;
    fClockFrame                : Integer; // 17 frames is one game-second
    ButtonsRemain              : Byte;
    LemmingsToRelease           : Integer; // number of lemmings that were created
    LemmingsCloned             : Integer; // number of cloned lemmings
    LemmingsOut                : Integer; // number of lemmings currently walking around
    fSpawnedDead                : Integer; // number of zombies that were created
    LemmingsIn                 : integer; // number of lemmings that made it to heaven
    LemmingsRemoved            : Integer; // number of lemmings removed
    DelayEndFrames             : Integer;
    fCursorPoint               : TPoint;
    fIsSelectWalkerHotkey      : Boolean;
    fIsSelectUnassignedHotkey  : Boolean;
    fIsShowAthleteInfo         : Boolean;
    fIsHighlightHotkey         : Boolean;
    TimePlay                   : Integer; // positive when time limit
                                          // negative when just counting time used
    fPlaying                   : Boolean; // game in active playing mode?
    EntriesOpened              : Boolean;
    LemmingMethods             : TLemmingMethodArray; // a method for each basic lemming state
    NewSkillMethods            : TNewSkillMethodArray; // The replacement of SkillMethods
    fLemSelected               : TLemming; // lem under cursor, who would receive the skill
    fLemWithShadow             : TLemming; // needed for CheckForNewShadow to erase previous shadow
    fLemWithShadowButton       : TSkillPanelButton; // correct skill to be erased
    fExistShadow               : Boolean;  // Whether a shadow is currently drawn somewhere
    fLemNextAction             : TBasicLemmingAction; // action to transition to at the end of lemming movement
    ObjectInfos                : TInteractiveObjectInfoList; // list of objects excluding entrances
    WindowOrderList            : array of Integer; // table for entrance release order
    //fPaused                    : Boolean;
    LowestReleaseRate          : Integer;   // only used for talismans
    HighestReleaseRate         : Integer;   // only used for talismans
    CurrReleaseRate            : Integer;

    CurrSkillCount             : array[TBasicLemmingAction] of Integer;  // should only be called with arguments in AssignableSkills
    UsedSkillCount             : array[TBasicLemmingAction] of Integer;  // should only be called with arguments in AssignableSkills

    UserSetNuking              : Boolean;
    ExploderAssignInProgress   : Boolean;
    Index_LemmingToBeNuked     : Integer;
    BrickPixelColors           : array[0..11] of TColor32; // gradient steps
    fGameFinished              : Boolean;
    fGameCheated               : Boolean;
    NextLemmingCountDown       : Integer;
    fFastForward               : Boolean;
    fTargetIteration           : Integer; // this is used in hyperspeed
    fHyperSpeedCounter         : Integer; // no screenoutput
    fHyperSpeed                : Boolean; // we are at hyperspeed no targetbitmap output
    fLeavingHyperSpeed         : Boolean; // in between state (see UpdateLemmings)
    fPauseOnHyperSpeedExit     : Boolean; // to maintain pause state before invoking a savestate
    fHitTestAutoFail           : Boolean;
    fHighlightLemmingID        : Integer;
    fCancelReplayAfterSkip     : Boolean;
  { events }
    //fOnFinish                  : TNotifyEvent;
    fParticleFinishTimer       : Integer; // extra frames to enable viewing of explosions
    fSimulationDepth           : Integer; // whether we are in simulation mode for drawing shadows
  { pixel combine eventhandlers }
    procedure DoTalismanCheck;

    // CombineMaskPixels has variants based on the direction of destruction
    procedure CombineMaskPixels(F: TColor32; var B: TColor32; M: TColor32; E: TColor32); // general-purpose
    procedure CombineMaskPixelsLeft(F: TColor32; var B: TColor32; M: TColor32);       //left-facing basher
    procedure CombineMaskPixelsRight(F: TColor32; var B: TColor32; M: TColor32);      //right-facing basher
    procedure CombineMaskPixelsUpLeft(F: TColor32; var B: TColor32; M: TColor32);     //left-facing fencer
    procedure CombineMaskPixelsUpRight(F: TColor32; var B: TColor32; M: TColor32);    //right-facing fencer
    procedure CombineMaskPixelsDownLeft(F: TColor32; var B: TColor32; M: TColor32);   //left-facing miner
    procedure CombineMaskPixelsDownRight(F: TColor32; var B: TColor32; M: TColor32);  //right-facing miner
    procedure CombineMaskPixelsNeutral(F: TColor32; var B: TColor32; M: TColor32);    //bomber

    procedure CombineNoOverwriteStoner(F: TColor32; var B: TColor32; M: TColor32);
  { internal methods }
    function GetIsReplaying: Boolean;
    procedure ApplyBashingMask(L: TLemming; MaskFrame: Integer);
    procedure ApplyFencerMask(L: TLemming; MaskFrame: Integer);
    procedure ApplyExplosionMask(L: TLemming);
    procedure ApplyStoneLemming(L: TLemming);
    procedure ApplyMinerMask(L: TLemming; MaskFrame, AdjustX, AdjustY: Integer);
    procedure AddConstructivePixel(X, Y: Integer; Color: TColor32);
    function CalculateNextLemmingCountdown: Integer;
    procedure CheckForGameFinished;
    // The next few procedures are for checking the behavior of lems in trigger areas!
    procedure CheckTriggerArea(L: TLemming);
      function GetObjectCheckPositions(L: TLemming): TArrayArrayInt;
      function HasTriggerAt(X, Y: Integer; TriggerType: TTriggerTypes): Boolean;
      function FindObjectID(X, Y: Integer; TriggerType: TTriggerTypes): Word;

      function HandleTrap(L: TLemming; PosX, PosY: Integer): Boolean;
      function HandleObjAnimation(L: TLemming; PosX, PosY: Integer): Boolean;
      function HandleTeleport(L: TLemming; PosX, PosY: Integer): Boolean;
      function HandlePickup(L: TLemming; PosX, PosY: Integer): Boolean;
      function HandleButton(L: TLemming; PosX, PosY: Integer): Boolean;
      function HandleExit(L: TLemming): Boolean;
      function HandleRadiation(L: TLemming; Stoning: Boolean): Boolean;
      function HandleForceField(L: TLemming; Direction: Integer): Boolean;
      function HandleFire(L: TLemming): Boolean;
      function HandleFlipper(L: TLemming; PosX, PosY: Integer): Boolean;
      function HandleWaterDrown(L: TLemming): Boolean;
      function HandleWaterSwim(L: TLemming): Boolean;

    function CheckForOverlappingField(L: TLemming): Boolean;
    procedure CheckForQueuedAction;
    procedure CheckForReplayAction(PausedRRCheck: Boolean = false);
    procedure CheckLemmings;
    function CheckLemTeleporting(L: TLemming): Boolean;
    procedure CheckReleaseLemming;
    procedure CheckUpdateNuking;
    procedure CueSoundEffect(aSound: String); overload;
    procedure CueSoundEffect(aSound: String; aOrigin: TPoint); overload;
    function DigOneRow(PosX, PosY: Integer): Boolean;
    procedure DrawAnimatedObjects;
    procedure CheckForNewShadow;
    function HasPixelAt(X, Y: Integer): Boolean;
    procedure IncrementIteration;
    procedure InitializeBrickColors(aBrickPixelColor: TColor32);
    procedure InitializeAllObjectMaps;

    function GetIsSimulating: Boolean;

    procedure LayBrick(L: TLemming);
    function LayStackBrick(L: TLemming): Boolean;
    procedure MoveLemToReceivePoint(L: TLemming; oid: Byte);


    procedure RecordNuke;
    procedure RecordReleaseRate(aRR: Integer);
    procedure RecordSkillAssignment(L: TLemming; aSkill: TBasicLemmingAction);
    procedure RemoveLemming(L: TLemming; RemMode: Integer = 0; Silent: Boolean = false);
    procedure RemovePixelAt(X, Y: Integer);
    procedure ReplaySkillAssignment(aReplayItem: TReplaySkillAssignment);

    procedure SetObjectMap;
      procedure WriteTriggerMap(Map: TArrayArrayBoolean; Rect: TRect);
      function ReadTriggerMap(X, Y: Integer; Map: TArrayArrayBoolean): Boolean;

    procedure SetBlockerMap;
      procedure WriteBlockerMap(X, Y: Integer; aValue: Byte);
      function ReadBlockerMap(X, Y: Integer): Byte;

    procedure SetZombieField(L: TLemming);
      procedure WriteZombieMap(X, Y: Integer; aValue: Byte);
      function ReadZombieMap(X, Y: Integer): Byte;

    procedure SimulateTransition(L: TLemming; NewAction: TBasicLemmingAction);
    function SimulateLem(L: TLemming; DoCheckObjects: Boolean = True): TArrayArrayInt;
    procedure AddPreplacedLemming;
    procedure Transition(L: TLemming; NewAction: TBasicLemmingAction; DoTurn: Boolean = False);
    procedure TurnAround(L: TLemming);
    function UpdateExplosionTimer(L: TLemming): Boolean;
    procedure UpdateInteractiveObjects;

    function CheckSkillAvailable(aAction: TBasicLemmingAction): Boolean;
    procedure UpdateSkillCount(aAction: TBasicLemmingAction; Rev: Boolean = false);

  { lemming actions }
    function FindGroundPixel(x, y: Integer): Integer;
    function HasSteelAt(x, y: Integer): Boolean;
    function HasIndestructibleAt(x, y, Direction: Integer;
                                     Skill: TBasicLemmingAction): Boolean;


    function HandleLemming(L: TLemming): Boolean;
      function CheckLevelBoundaries(L: TLemming) : Boolean;
    function HandleWalking(L: TLemming): Boolean;
    function HandleJumping(L: TLemming): Boolean;
    function HandleDigging(L: TLemming): Boolean;
    function HandleClimbing(L: TLemming): Boolean;
    function HandleDrowning(L: TLemming): Boolean;
    function HandleHoisting(L: TLemming): Boolean;
    function HandleBuilding(L: TLemming): Boolean;
    function HandleBashing(L: TLemming): Boolean;
    function HandleMining(L: TLemming): Boolean;
    function HandleFalling(L: TLemming): Boolean;
    function HandleFloating(L: TLemming): Boolean;
    function HandleSplatting(L: TLemming): Boolean;
    function HandleExiting(L: TLemming): Boolean;
    function HandleVaporizing(L: TLemming): Boolean;
    function HandleBlocking(L: TLemming): Boolean;
    function HandleShrugging(L: TLemming): Boolean;
    function HandleOhNoing(L: TLemming): Boolean;
    function HandleExploding(L: TLemming): Boolean;
    function HandlePlatforming(L: TLemming): Boolean;
      function LemCanPlatform(L: TLemming): Boolean;
    function HandleStacking(L: TLemming): Boolean;
    function HandleSwimming(L: TLemming): Boolean;
    function HandleGliding(L: TLemming): Boolean;
    function HandleFixing(L: TLemming): Boolean;
    function HandleFencing(L: TLemming): Boolean;

  { interaction }
    function AssignNewSkill(Skill: TBasicLemmingAction; IsHighlight: Boolean = False; IsReplayAssignment: Boolean = false): Boolean;
    procedure GenerateClonedLem(L: TLemming);
    function GetPriorityLemming(out PriorityLem: TLemming;
                                  NewSkillOrig: TBasicLemmingAction;
                                  MousePos: TPoint;
                                  IsHighlight: Boolean = False): Integer;
    function DoSkillAssignment(L: TLemming; NewSkill: TBasicLemmingAction): Boolean;

    function MayAssignWalker(L: TLemming): Boolean;
    function MayAssignClimber(L: TLemming): Boolean;
    function MayAssignFloaterGlider(L: TLemming): Boolean;
    function MayAssignSwimmer(L: TLemming): Boolean;
    function MayAssignMechanic(L: TLemming): Boolean;
    function MayAssignBlocker(L: TLemming): Boolean;
    function MayAssignExploderStoner(L: TLemming): Boolean;
    function MayAssignBuilder(L: TLemming): Boolean;
    function MayAssignPlatformer(L: TLemming): Boolean;
    function MayAssignStacker(L: TLemming): Boolean;
    function MayAssignBasher(L: TLemming): Boolean;
    function MayAssignFencer(L: TLemming): Boolean;
    function MayAssignMiner(L: TLemming): Boolean;
    function MayAssignDigger(L: TLemming): Boolean;
    function MayAssignCloner(L: TLemming): Boolean;

    // for properties
    function GetSkillCount(aSkill: TSkillPanelButton): Integer;
  public
    GameResult                 : Boolean;
    GameResultRec              : TGameResultsRec;
    fSelectDx                  : Integer;
    fXmasPal                   : Boolean;
    fActiveSkills              : array[0..7] of TSkillPanelButton;
    LastHitCount               : Integer;
    ReleaseRateModifier        : Integer; //negative = decrease each update, positive = increase each update, 0 = no change
    ReplayInsert               : Boolean;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  { iteration }
    procedure PrepareParams;
    procedure PlayMusic;
    procedure Start(aReplay: Boolean = False);
    procedure UpdateLemmings;

  { callable }
    procedure CheckAdjustReleaseRate;
    procedure AdjustReleaseRate(aRR: Integer);
    function CheckIfLegalRR(aRR: Integer): Boolean;
    procedure CreateLemmingAtCursorPoint;
    procedure Finish(aReason: Integer);
    procedure Cheat;
    procedure HitTest(Autofail: Boolean = false);
    //procedure HyperSpeedBegin(PauseWhenDone: Boolean = False);
    //procedure HyperSpeedEnd;
    function ProcessSkillAssignment(IsHighlight: Boolean = false): Boolean;
    function ProcessHighlightAssignment: Boolean;
    procedure RegainControl(Force: Boolean = false);
    procedure EnsureCorrectReplayDetails;
    procedure SetGameResult;
    procedure SetSelectedSkill(Value: TSkillPanelButton; MakeActive: Boolean = True; RightClick: Boolean = False);
    procedure SaveGameplayImage(Filename: String);
    function GetSelectedSkill: Integer;
    function Checkpass: Boolean;
    function CheckFinishedTest: Boolean;
    function GetHighlitLemming: TLemming;

  { properties }
    property CurrentIteration: Integer read fCurrentIteration;
    property LemmingsToSpawn: Integer read LemmingsToRelease;
    property SpawnedDead: Integer read fSpawnedDead;
    property LemmingsActive: Integer read LemmingsOut;
    property LemmingsSaved: Integer read LemmingsIn;
    property CurrentReleaseRate: Integer read CurrReleaseRate; // for skill panel's usage
    property SkillCount[Index: TSkillPanelButton]: Integer read GetSkillCount;
    property ClockFrame: Integer read fClockFrame;
    property CursorPoint: TPoint read fCursorPoint write fCursorPoint;
    //property FastForward: Boolean read fFastForward write fFastForward;
    property GameFinished: Boolean read fGameFinished;
    //property HyperSpeed: Boolean read fHyperSpeed;
    //property LeavingHyperSpeed: Boolean read fLeavingHyperSpeed;
    property Level: TLevel read fLevel write fLevel;
    property MessageQueue: TGameMessageQueue read fMessageQueue;
    //property Paused: Boolean read fPaused write fPaused;
    property Playing: Boolean read fPlaying write fPlaying;
    property Renderer: TRenderer read fRenderer;
    property Replaying: Boolean read GetIsReplaying;
    property ReplayManager: TReplay read fReplayManager;
    property IsSelectWalkerHotkey: Boolean read fIsSelectWalkerHotkey write fIsSelectWalkerHotkey;
    property IsSelectUnassignedHotkey: Boolean read fIsSelectUnassignedHotkey write fIsSelectUnassignedHotkey;
    property IsShowAthleteInfo: Boolean read fIsShowAthleteInfo write fIsShowAthleteInfo;
    property IsHighlightHotkey: Boolean read fIsHighlightHotkey write fIsHighlightHotkey;
    property TargetIteration: Integer read fTargetIteration write fTargetIteration;
    property CancelReplayAfterSkip: Boolean read fCancelReplayAfterSkip write fCancelReplayAfterSkip;
    property HitTestAutoFail: Boolean read fHitTestAutoFail write fHitTestAutoFail;
    property LastReplayDir: String read fLastReplayDir write fLastReplayDir;

    property RenderInterface: TRenderInterface read fRenderInterface;
    property IsSimulating: Boolean read GetIsSimulating;

    function GetLevelWidth: Integer;
    function GetLevelHeight: Integer;

  { save / load state }
    procedure CreateSavedState(aState: TLemmingGameSavedState);
    function LoadSavedState(aState: TLemmingGameSavedState; SkipTargetBitmap: Boolean = false): Boolean;

  { events }
    //property OnFinish: TNotifyEvent read fOnFinish write fOnFinish;
  end;

{-------------------------------------------------------------------------------
  The global game mechanics instance, which has to be global  (for now) because
  we have to be able to start/save/load replay from the postview screen.
  GlobalGame is initialized by the mainform.
-------------------------------------------------------------------------------}
var
  GlobalGame: TLemmingGame;

implementation

const
  LEMMIX_REPLAY_VERSION    = 105;
  MAX_FALLDISTANCE         = 62;

const
  // Values for DOM_TRIGGERTYPE are defined in LemObjects.pas!
  // Here only for refence.
(*DOM_NOOBJECT         = 65535;
  DOM_NONE             = 0;
  DOM_EXIT             = 1;
  DOM_FORCELEFT        = 2; // left arm of blocker
  DOM_FORCERIGHT       = 3; // right arm of blocker
  DOM_TRAP             = 4; // triggered trap
  DOM_WATER            = 5; // causes drowning
  DOM_FIRE             = 6; // causes vaporizing
  DOM_ONEWAYLEFT       = 7;
  DOM_ONEWAYRIGHT      = 8;
  DOM_STEEL            = 9;
  DOM_BLOCKER          = 10; // the middle part of blocker
  DOM_TELEPORT         = 11;
  DOM_RECEIVER         = 12;
  DOM_LEMMING          = 13;
  DOM_PICKUP           = 14;
  DOM_LOCKEXIT         = 15;
  DOM_SECRET           = 16; // no longer used!!
  DOM_BUTTON           = 17;
  DOM_RADIATION        = 18;
  DOM_ONEWAYDOWN       = 19;
  DOM_UPDRAFT          = 20;
  DOM_FLIPPER          = 21;
  DOM_SLOWFREEZE       = 22;
  DOM_WINDOW           = 23;
  DOM_ANIMATION        = 24;
  DOM_HINT             = 25;
  DOM_NOSPLAT          = 26;
  DOM_SPLAT            = 27;
  DOM_TWOWAYTELE       = 28; // no longer used!!
  DOM_SINGLETELE       = 29; // no longer used!!
  DOM_BACKGROUND       = 30;
  DOM_TRAPONCE         = 31;
  DOM_BGIMAGE          = 32;
  DOM_ONEWAYUP         = 33; *)

  // removal modes
  RM_NEUTRAL           = 0;
  RM_SAVE              = 1;
  RM_KILL              = 2;
  RM_ZOMBIE            = 3;

  HEAD_MIN_Y = -7;
  LEMMING_MAX_Y = 9;

  PARTICLE_FRAMECOUNT = 52;
  PARTICLE_FINISH_FRAMECOUNT = 52;

const
  // Order is important, because fTalismans[i].SkillLimit uses the corresponding integers!!!
  // THIS IS NOT THE ORDER THE PICKUP-SKILLS ARE NUMBERED!!!
  ActionListArray: array[0..16] of TBasicLemmingAction =
            (baToWalking, baClimbing, baSwimming, baFloating, baGliding, baFixing,
             baExploding, baStoning, baBlocking, baPlatforming, baBuilding,
             baStacking, baBashing, baMining, baDigging, baCloning, baFencing);



function CheckRectCopy(const A, B: TRect): Boolean;
begin
  Result := (RectWidth(A) = RectWidth(B))
            and (Rectheight(A) = Rectheight(B));
end;



{ TLemmingGameSavedState }

constructor TLemmingGameSavedState.Create;
begin
  inherited;
  LemmingList := TLemmingList.Create(true);
  ObjectInfos := TInteractiveObjectInfoList.Create(true);
  TerrainLayer := TBitmap32.Create;
  PhysicsMap := TBitmap32.Create;
  ZombieMap := TByteMap.Create;
end;

destructor TLemmingGameSavedState.Destroy;
begin
  LemmingList.Free;
  ObjectInfos.Free;
  TerrainLayer.Free;
  PhysicsMap.Free;
  ZombieMap.Free;
  inherited;
end;

{ TLemmingGameSavedStateList }

procedure TLemmingGameSavedStateList.TidyList(aCurrentIteration: Integer);
var
  i: Integer;

  function CheckKeepSaveState(aStateIndex: Integer): Boolean;
  const
    MINUTE_IN_FRAMES = 17 * 60;
    HALF_MINUTE_IN_FRAMES = 17 * 30;
    TEN_SECONDS_IN_FRAMES = 17 * 10;
  begin
    Result := false;
    if Items[aStateIndex].CurrentIteration = 0 then
      Result := true;
    if Items[aStateIndex].CurrentIteration mod MINUTE_IN_FRAMES = 0 then
      Result := true;
    if (Items[aStateIndex].CurrentIteration mod HALF_MINUTE_IN_FRAMES = 0)
    and (aCurrentIteration - Items[aStateIndex].CurrentIteration <= MINUTE_IN_FRAMES * 3) then
      Result := true;
    if (Items[aStateIndex].CurrentIteration mod TEN_SECONDS_IN_FRAMES = 0)
    and (aCurrentIteration - Items[aStateIndex].CurrentIteration <= MINUTE_IN_FRAMES) then
      Result := true;
  end;
begin
  // What we want to save:
  // -- Last minute: One save every 10 seconds
  // -- Last 3 minutes: One save every 30 seconds
  // -- Beyond that: One save every minute
  // -- Additionally, a save immediately after the level is initially rendered
  // This will result in 11 saved states, plus one more for every minute taken
  // that isn't in the most recent 3 minutes. This should be manageable.
  for i := Count-1 downto 0 do
    if not CheckKeepSaveState(i) then Delete(i);
end;

function TLemmingGameSavedStateList.Add: TLemmingGameSavedState;
begin
  // Creates a new TLemmingGameSavedState, adds it, and returns it.
  Result := TLemmingGameSavedState.Create;
  inherited Add(Result);
end;

function TLemmingGameSavedStateList.GetItem(Index: Integer): TLemmingGameSavedState;
begin
  // Gets a TLemmingGameSavedState from the list.
  Result := inherited Get(Index);
end;

function TLemmingGameSavedStateList.FindNearestState(aTargetIteration: Integer): Integer;
var
  i: Integer;
  ClosestFrame: Integer;
begin
  Result := -1;
  ClosestFrame := -1;
  for i := 0 to Count-1 do
    if (Items[i].CurrentIteration < aTargetIteration) and (Items[i].CurrentIteration > ClosestFrame) then
    begin
      Result := i;
      ClosestFrame := Items[i].CurrentIteration;
    end;
end;

procedure TLemmingGameSavedStateList.ClearAfterIteration(aTargetIteration: Integer);
var
  i: Integer;
begin
  for i := Count-1 downto 0 do
    if Items[i].CurrentIteration > aTargetIteration then Delete(i);
end;

{ TLemmingGame }

procedure TLemmingGame.CreateSavedState(aState: TLemmingGameSavedState);
var
  i: Integer;
begin
  // Simple stuff
  aState.SelectedSkill := fSelectedSkill;
  aState.TerrainLayer.Assign(fRenderer.TerrainLayer);
  aState.PhysicsMap.Assign(PhysicsMap);
  aState.ZombieMap.Assign(ZombieMap);
  aState.CurrentIteration := fCurrentIteration;
  aState.ClockFrame := fClockFrame;
  aState.ButtonsRemain := ButtonsRemain;
  aState.LemmingsToRelease := LemmingsToRelease;
  aState.LemmingsCloned := LemmingsCloned;
  aState.LemmingsOut := LemmingsOut;
  aState.fSpawnedDead := fSpawnedDead;
  aState.LemmingsIn := LemmingsIn;
  aState.LemmingsRemoved := LemmingsRemoved;
  aState.NextLemmingCountdown := NextLemmingCountdown;
  aState.DelayEndFrames := DelayEndFrames;
  aState.TimePlay := TimePlay;
  aState.EntriesOpened := EntriesOpened;
  aState.LowestReleaseRate := LowestReleaseRate;
  aState.HighestReleaseRate := HighestReleaseRate;
  aState.CurrReleaseRate := CurrReleaseRate;

  for i := 0 to 16 do
  begin
    aState.CurrSkillCount[ActionListArray[i]] := CurrSkillCount[ActionListArray[i]];
    aState.UsedSkillCount[ActionListArray[i]] := UsedSkillCount[ActionListArray[i]];
  end;

  aState.UserSetNuking := UserSetNuking;
  aState.ExploderAssignInProgress := ExploderAssignInProgress;
  aState.Index_LemmingToBeNuked := Index_LemmingToBeNuked;

  // Lemmings.
  aState.LemmingList.Clear;
  for i := 0 to LemmingList.Count-1 do
  begin
    aState.LemmingList.Add(TLemming.Create);
    aState.LemmingList[i].Assign(LemmingList[i]);
  end;

  // Objects.
  aState.ObjectInfos.Clear;
  for i := 0 to ObjectInfos.Count-1 do
  begin
    aState.ObjectInfos.Add(TInteractiveObjectInfo.Create);
    ObjectInfos[i].AssignTo(aState.ObjectInfos[i]);
  end;
end;

function TLemmingGame.LoadSavedState(aState: TLemmingGameSavedState; SkipTargetBitmap: Boolean = false): Boolean;
var
  i: Integer;
begin
  // Let's check if we can use this state. If a skill was assigned on the frame, we should mark this one
  // unusable. Code in TGameWindow will then delete it. (This is kludgy, but easier to implement over the
  // current setup than not creating the state in the first place would be.)
  Result := true;
  //if fReplayManager.HasAnyActionAt(aState.CurrentIteration) and (aState.CurrentIteration > 1) then Result := false;
  if not Result then Exit;

  // Simple stuff
  fRenderer.TerrainLayer.Assign(aState.TerrainLayer);
  PhysicsMap.Assign(aState.PhysicsMap);
  ZombieMap.Assign(aState.ZombieMap);
  fCurrentIteration := aState.CurrentIteration;
  fClockFrame := aState.ClockFrame;
  ButtonsRemain := aState.ButtonsRemain;
  LemmingsToRelease := aState.LemmingsToRelease;
  LemmingsCloned := aState.LemmingsCloned;
  LemmingsOut := aState.LemmingsOut;
  fSpawnedDead := aState.fSpawnedDead;
  LemmingsIn := aState.LemmingsIn;
  LemmingsRemoved := aState.LemmingsRemoved;
  NextLemmingCountdown := aState.NextLemmingCountdown;
  DelayEndFrames := aState.DelayEndFrames;
  TimePlay := aState.TimePlay;
  EntriesOpened := aState.EntriesOpened;
  LowestReleaseRate := aState.LowestReleaseRate;
  HighestReleaseRate := aState.HighestReleaseRate;
  CurrReleaseRate := aState.CurrReleaseRate;

  for i := 0 to 16 do
  begin
    CurrSkillCount[ActionListArray[i]] := aState.CurrSkillCount[ActionListArray[i]];
    UsedSkillCount[ActionListArray[i]] := aState.UsedSkillCount[ActionListArray[i]];
  end;

  UserSetNuking := aState.UserSetNuking;
  ExploderAssignInProgress := aState.ExploderAssignInProgress;
  Index_LemmingToBeNuked := aState.Index_LemmingToBeNuked;

  // Lemmings.
  LemmingList.Clear;
  for i := 0 to aState.LemmingList.Count-1 do
  begin
    LemmingList.Add(TLemming.Create);
    LemmingList[i].Assign(aState.LemmingList[i]);
    LemmingList[i].LemIndex := i;
  end;

  // Objects
  for i := 0 to ObjectInfos.Count-1 do
  begin
    aState.ObjectInfos[i].AssignTo(ObjectInfos[i]);
  end;

  // Recreate Blocker map
  SetBlockerMap;

  ReleaseRateModifier := 0; // we don't want to continue changing it if it's currently changing
end;

procedure TLemmingGame.DoTalismanCheck;
var
  i, i2, j: Integer;
  TotalSkillUsed: Integer;
  FoundIssue: Boolean;
  UsedSkillLems: Integer;
  GetTalisman: Boolean;
begin
  for i := 0 to fTalismans.Count-1 do
  begin
    if GameParams.SaveSystem.CheckTalisman(fTalismans[i].Signature) then Continue;
    with fTalismans[i] do
    begin

      if    (LemmingsIn < SaveRequirement)
         or ((SaveRequirement = 0) and (LemmingsIn < GameParams.Level.Info.RescueCount)) then Continue;

      if    ((TimeLimit <> 0) and (CurrentIteration > TimeLimit))
         or ((TimeLimit = 0) and Level.Info.HasTimeLimit and (CurrentIteration > Level.Info.TimeLimit * 17)) then Continue;
      if LowestReleaseRate < RRMin then Continue;
      if HighestReleaseRate > RRMax then Continue;

      TotalSkillUsed := 0;
      GetTalisman := True;
      for j := 0 to 16 do
      begin
        if (UsedSkillCount[ActionListArray[j]] > SkillLimit[j]) and (SkillLimit[j] <> -1) then
          GetTalisman := False;
        TotalSkillUsed := TotalSkillUsed + UsedSkillCount[ActionListArray[j]];
      end;
      if not GetTalisman then Continue;

      if (TotalSkillUsed > TotalSkillLimit) and (TotalSkillLimit <> -1) then Continue;

      FoundIssue := false;
      if tmOneSkill in MiscOptions then
        for i2 := 0 to LemmingList.Count-1 do
          with LemmingList[i2] do
           if (LemUsedSkillCount > 1) then FoundIssue := true;
      if FoundIssue then Continue;

      UsedSkillLems := 0;
      if tmOneLemming in MiscOptions then
        for i2 := 0 to LemmingList.Count-1 do
          with LemmingList[i2] do
            if (LemUsedSkillCount > 0) then Inc(UsedSkillLems);
      if UsedSkillLems > 1 then Continue;

      // Award Talisman
      GameParams.SaveSystem.GetTalisman(Signature);
      if TalismanType <> 0 then fTalismanReceived := True;

    end;
  end;
end;

function TLemmingGame.Checkpass: Boolean;
begin
  Result := fGameCheated or (LemmingsIn >= Level.Info.RescueCount);
end;

function TLemmingGame.CheckFinishedTest;
var
  i: Integer;
begin
  Result := false;
  if not Checkpass then Exit;

  for i := 0 to fTalismans.Count-1 do
    if not GameParams.SaveSystem.CheckTalisman(fTalismans[i].Signature) then Exit;

  Result := true;
end;

function TLemmingGame.GetLevelWidth: Integer;
begin
  Result := GameParams.Level.Info.Width;
end;

function TLemmingGame.GetLevelHeight: Integer;
begin
  Result := GameParams.Level.Info.Height;
end;

constructor TLemmingGame.Create(aOwner: TComponent);
var
  P: string;
begin
  inherited Create(aOwner);

  fRenderInterface := TRenderInterface.Create;
  fMessageQueue := TGameMessageQueue.Create;

  LemmingList    := TLemmingList.Create;

  BomberMask     := TBitmap32.Create;
  StonerMask     := TBitmap32.Create;
  BasherMasks    := TBitmap32.Create;
  FencerMasks    := TBitmap32.Create;
  MinerMasks     := TBitmap32.Create;

  ObjectInfos    := TInteractiveObjectInfoList.Create;
  BlockerMap     := TByteMap.Create;
  ZombieMap      := TByteMap.Create;
  fReplayManager := TReplay.Create;
  fTalismans     := TTalismans.Create;

  fRenderInterface.LemmingList := LemmingList;
  fRenderInterface.ObjectList := ObjectInfos;
  fRenderInterface.SetSelectedSkillPointer(fSelectedSkill);
  fRenderInterface.SelectedLemming := nil;
  fRenderInterface.ReplayLemming := nil;
  fRenderInterface.SetSimulateLemRoutine(SimulateLem, SimulateTransition);
  fRenderInterface.SetGetHighlitRoutine(GetHighlitLemming);

  LemmingMethods[baNone]       := nil;
  LemmingMethods[baWalking]    := HandleWalking;
  LemmingMethods[baJumping]    := HandleJumping;
  LemmingMethods[baDigging]    := HandleDigging;
  LemmingMethods[baClimbing]   := HandleClimbing;
  LemmingMethods[baDrowning]   := HandleDrowning;
  LemmingMethods[baHoisting]   := HandleHoisting;
  LemmingMethods[baBuilding]   := HandleBuilding;
  LemmingMethods[baBashing]    := HandleBashing;
  LemmingMethods[baMining]     := HandleMining;
  LemmingMethods[baFalling]    := HandleFalling;
  LemmingMethods[baFloating]   := HandleFloating;
  LemmingMethods[baSplatting]  := HandleSplatting;
  LemmingMethods[baExiting]    := HandleExiting;
  LemmingMethods[baVaporizing] := HandleVaporizing;
  LemmingMethods[baBlocking]   := HandleBlocking;
  LemmingMethods[baShrugging]  := HandleShrugging;
  LemmingMethods[baOhnoing]    := HandleOhNoing;
  LemmingMethods[baExploding]  := HandleExploding;
  LemmingMethods[baToWalking]  := HandleWalking; //should never happen anyway
  LemmingMethods[baPlatforming] := HandlePlatforming;
  LemmingMethods[baStacking]   := HandleStacking;
  LemmingMethods[baStoning]    := HandleOhNoing; // same behavior!
  LemmingMethods[baStoneFinish] := HandleExploding; // same behavior, except applied mask!
  LemmingMethods[baSwimming]   := HandleSwimming;
  LemmingMethods[baGliding]    := HandleGliding;
  LemmingMethods[baFixing]     := HandleFixing;
  LemmingMethods[baFencing]    := HandleFencing;

  NewSkillMethods[baNone]         := nil;
  NewSkillMethods[baWalking]      := nil;
  NewSkillMethods[baJumping]      := nil;
  NewSkillMethods[baDigging]      := MayAssignDigger;
  NewSkillMethods[baClimbing]     := MayAssignClimber;
  NewSkillMethods[baDrowning]     := nil;
  NewSkillMethods[baHoisting]     := nil;
  NewSkillMethods[baBuilding]     := MayAssignBuilder;
  NewSkillMethods[baBashing]      := MayAssignBasher;
  NewSkillMethods[baMining]       := MayAssignMiner;
  NewSkillMethods[baFalling]      := nil;
  NewSkillMethods[baFloating]     := MayAssignFloaterGlider;
  NewSkillMethods[baSplatting]    := nil;
  NewSkillMethods[baExiting]      := nil;
  NewSkillMethods[baVaporizing]   := nil;
  NewSkillMethods[baBlocking]     := MayAssignBlocker;
  NewSkillMethods[baShrugging]    := nil;
  NewSkillMethods[baOhnoing]      := nil;
  NewSkillMethods[baExploding]    := MayAssignExploderStoner;
  NewSkillMethods[baToWalking]    := MayAssignWalker;
  NewSkillMethods[baPlatforming]  := MayAssignPlatformer;
  NewSkillMethods[baStacking]     := MayAssignStacker;
  NewSkillMethods[baStoning]      := MayAssignExploderStoner;
  NewSkillMethods[baSwimming]     := MayAssignSwimmer;
  NewSkillMethods[baGliding]      := MayAssignFloaterGlider;
  NewSkillMethods[baFixing]       := MayAssignMechanic;
  NewSkillMethods[baCloning]      := MayAssignCloner;
  NewSkillMethods[baFencing]      := MayAssignFencer;

  P := AppPath;
  fLastReplayDir := '';

  ButtonsRemain := 0;
  fHitTestAutoFail := false;

  fSimulationDepth := 0;

end;

destructor TLemmingGame.Destroy;
begin
  // Free memory of trigger area maps
  SetLength(WaterMap, 0, 0);
  SetLength(FireMap, 0, 0);
  SetLength(TeleporterMap, 0, 0);
  SetLength(UpdraftMap, 0, 0);
  SetLength(ButtonMap, 0, 0);
  SetLength(PickupMap, 0, 0);
  SetLength(FlipperMap, 0, 0);
  SetLength(RadiationMap, 0, 0);
  SetLength(SlowfreezeMap, 0, 0);
  SetLength(SplatMap, 0, 0);
  SetLength(AntiSplatMap, 0, 0);
  SetLength(AnimationMap, 0, 0);
  SetLength(ExitMap, 0, 0);
  SetLength(LockedExitMap, 0, 0);
  SetLength(TrapMap, 0, 0);

  BomberMask.Free;
  StonerMask.Free;
  BasherMasks.Free;
  FencerMasks.Free;
  MinerMasks.Free;

  LemmingList.Free;
  ObjectInfos.Free;
  BlockerMap.Free;
  ZombieMap.Free;
  fReplayManager.Free;
  fTalismans.Free;
  fRenderInterface.Free;
  inherited Destroy;
end;

procedure TLemmingGame.PlayMusic;
begin
  MessageQueue.Add(GAMEMSG_MUSIC);
end;

procedure TLemmingGame.PrepareParams;
var
  i: Integer;

  procedure LoadMask(aDst: TBitmap32; aFilename: String; aCombine: TPixelCombineEvent);
  begin
    TPngInterface.LoadPngFile(AppPath + SFGraphicsMasks + aFilename, aDst);
    aDst.DrawMode := dmCustom;
    aDst.OnPixelCombine := aCombine;
  end;

begin
  fXmasPal := GameParams.SysDat.Options2 and 2 <> 0;

  fRenderer := GameParams.Renderer; // set ref
  Level := GameParams.Level;
  Style := GameParams.Style;

  fHighlightLemmingID := -1;

  if not fMasksLoaded then
  begin
    LoadMask(BomberMask, 'bomber.png', CombineMaskPixelsNeutral);
    LoadMask(StonerMask, 'stoner.png', CombineNoOverwriteStoner);
    LoadMask(BasherMasks, 'basher.png', CombineMaskPixelsNeutral);  // combine routines for Basher, Fencer and Miner are set when used
    LoadMask(FencerMasks, 'fencer.png', CombineMaskPixelsNeutral);
    LoadMask(MinerMasks, 'miner.png', CombineMaskPixelsNeutral);
    fMasksLoaded := true;
  end;

  PhysicsMap := Renderer.PhysicsMap;
  RenderInterface.PhysicsMap := PhysicsMap;

  fTalismans.Clear;

  for i := 0 to GameParams.Talismans.Count-1 do
  begin
    if (GameParams.Talismans[i].RankNumber = GameParams.Info.dSection)
    and (GameParams.Talismans[i].LevelNumber = GameParams.Info.dLevel) then
      fTalismans.Add.Assign(GameParams.Talismans[i]);
  end;

end;

procedure TLemmingGame.Start(aReplay: Boolean = False);
{-------------------------------------------------------------------------------
  part of the initialization is still in FGame. bad programming
  (i.e: renderer.levelbitmap, level)
-------------------------------------------------------------------------------}
var
  i: Integer;
  Inf: TInteractiveObjectInfo;
  numEntries:integer;

  Skill: TSkillPanelButton;
  InitialSkill: TSkillPanelButton;
begin
  Playing := False;

  if GameParams.ChallengeMode then
  begin
    for Skill := Low(TSkillPanelButton) to High(TSkillPanelButton) do
      Level.Info.SkillCount[Skill] := 0;
  end;

  // hyperspeed things
  fTargetIteration := 0;
  fHyperSpeedCounter := 0;
  fHyperSpeed := False;
  fLeavingHyperSpeed := False;
  fPauseOnHyperSpeedExit := False;

  fFastForward := False;

  fGameFinished := False;
  fGameCheated := False;
  LemmingsToRelease := Level.Info.LemmingsCount;
  LemmingsCloned := 0;
  TimePlay := Level.Info.TimeLimit;
  if (not Level.Info.HasTimeLimit) or (GameParams.TimerMode) then
    TimePlay := 0; // infinite time

  FillChar(GameResultRec, SizeOf(GameResultRec), 0);
  GameResultRec.gCount  := Level.Info.LemmingsCount;
  GameResultRec.gToRescue := Level.Info.RescueCount;

  LemmingsOut := 0;
  fSpawnedDead := Level.Info.ZombieCount;
  LemmingsIn := 0;
  LemmingsRemoved := 0;
  DelayEndFrames := 0;
  IsSelectWalkerHotkey := False;
  IsSelectUnassignedHotkey := False;
  IsShowAthleteInfo := False;
  IsHighlightHotkey := False;
  fCurrentIteration := 0;
  fClockFrame := 0;
  EntriesOpened := False;


  SetLength(WindowOrderList, 0);

  ReleaseRateModifier := 0;
  UserSetNuking := False;
  ExploderAssignInProgress := False;
  Index_LemmingToBeNuked := 0;
  fParticleFinishTimer := 0;
  LemmingList.Clear;
  if Level.Info.LevelID <> fReplayManager.LevelID then //not aReplay then
  begin
    fReplayManager.Clear(true);
    fReplayManager.LevelName := Level.Info.Title;
    fReplayManager.LevelAuthor := Level.Info.Author;
    fReplayManager.LevelGame := Trim(GameParams.SysDat.PackName);
    fReplayManager.LevelRank := Trim(GameParams.Info.dSectionName);
    fReplayManager.LevelPosition := GameParams.Info.dLevel+1;
    fReplayManager.LevelID := Level.Info.LevelID;
  end;

  with Level.Info do
  begin
    CurrReleaseRate := ReleaseRate;

    // Set available skills
    for Skill := Low(TSkillPanelButton) to High(TSkillPanelButton) do
      if SkillPanelButtonToAction[Skill] <> baNone then
        CurrSkillCount[SkillPanelButtonToAction[Skill]] := SkillCount[Skill];
    // Initialize used skills
    for i := 0 to 16 do
      UsedSkillCount[ActionListArray[i]] := 0;
  end;

  LowestReleaseRate := CurrReleaseRate;
  HighestReleaseRate := CurrReleaseRate;

  NextLemmingCountDown := 20;

  numEntries := 0;
  ButtonsRemain := 0;

  // Create the list of interactive objects
  ObjectInfos.Clear;
  fRenderer.CreateInteractiveObjectList(ObjectInfos);

  with Level do
  for i := 0 to ObjectInfos.Count - 1 do
  begin
    Inf := ObjectInfos[i];

    // Update number of hatches
    if Inf.TriggerEffect = DOM_WINDOW then
    begin
      SetLength(WindowOrderList, numEntries + 1);
      WindowOrderList[numEntries] := i;
      Inc(numEntries);
    end;

    // Update number of buttons
    if Inf.TriggerEffect = DOM_BUTTON then
      Inc(ButtonsRemain);
  end;

  InitializeBrickColors(Renderer.Theme.Colors[MASK_COLOR]);

  InitializeAllObjectMaps;
  SetObjectMap;

  AddPreplacedLemming;

  SetBlockerMap;

  DrawAnimatedObjects; // first draw needed

  // force update
  fSelectedSkill := spbNone;
  InitialSkill := spbNone;

  for i := 0 to 7 do
    fActiveSkills[i] := spbNone;
  i := 0;
  for Skill := Low(TSkillPanelButton) to High(TSkillPanelButton) do
  begin
    if Skill in Level.Info.Skillset then
    begin
      if InitialSkill = spbNone then InitialSkill := Skill;
      fActiveSkills[i] := Skill;
      Inc(i);

      if i = 8 then Break; // remove this if we ever allow more than 8 skill types per level
    end;
  end;
  if InitialSkill <> spbNone then
    SetSelectedSkill(InitialSkill, True); // default

  fTalismanReceived := false;

  MessageQueue.Clear;

  Playing := True;
end;


procedure TLemmingGame.AddPreplacedLemming;
var
  L: TLemming;
  Lem: TPreplacedLemming;
  i: Integer;
begin
  for i := 0 to GameParams.Level.PreplacedLemmings.Count-1 do
  begin
    Lem := GameParams.Level.PreplacedLemmings[i];
    L := TLemming.Create;
    with L do
    begin
      LemIndex := LemmingList.Add(L);
      SetFromPreplaced(Lem);

      if not HasPixelAt(L.LemX, L.LemY) then
        Transition(L, baFalling)
      else if Lem.IsBlocker then
        Transition(L, baBlocking)
      else
        Transition(L, baWalking);

      if Lem.IsZombie then
      begin
        RemoveLemming(L, RM_ZOMBIE, true);
        Dec(fSpawnedDead);
      end;

    end;
    Dec(LemmingsToRelease);
    Inc(LemmingsOut);
  end;
end;

procedure TLemmingGame.CombineMaskPixels(F: TColor32; var B: TColor32; M: TColor32; E: TColor32);
// copy masks to world
begin
  if (AlphaComponent(F) <> 0) and (B and E = 0) then B := B and not PM_TERRAIN;
end;

// Not sure who wrote this (probably me), but upon seeing this I forgot what the hell they were
// for. The pixel in "E" is excluded, IE: anything that matches even one bit of E, will not be
// removed when applying the mask.
procedure TLemmingGame.CombineMaskPixelsUpLeft(F: TColor32; var B: TColor32; M: TColor32);
var
  E: TColor32;
begin
  E := PM_STEEL or PM_ONEWAYRIGHT or PM_ONEWAYDOWN;
  CombineMaskPixels(F, B, M, E);
end;

procedure TLemmingGame.CombineMaskPixelsUpRight(F: TColor32; var B: TColor32; M: TColor32);
var
  E: TColor32;
begin
  E := PM_STEEL or PM_ONEWAYLEFT or PM_ONEWAYDOWN;
  CombineMaskPixels(F, B, M, E);
end;

procedure TLemmingGame.CombineMaskPixelsLeft(F: TColor32; var B: TColor32; M: TColor32);
var
  E: TColor32;
begin
  E := PM_STEEL or PM_ONEWAYRIGHT or PM_ONEWAYDOWN or PM_ONEWAYUP;
  CombineMaskPixels(F, B, M, E);
end;

procedure TLemmingGame.CombineMaskPixelsRight(F: TColor32; var B: TColor32; M: TColor32);
var
  E: TColor32;
begin
  E := PM_STEEL or PM_ONEWAYLEFT or PM_ONEWAYDOWN or PM_ONEWAYUP;
  CombineMaskPixels(F, B, M, E);
end;

procedure TLemmingGame.CombineMaskPixelsDownLeft(F: TColor32; var B: TColor32; M: TColor32);
var
  E: TColor32;
begin
  E := PM_STEEL or PM_ONEWAYRIGHT;
  CombineMaskPixels(F, B, M, E);
end;

procedure TLemmingGame.CombineMaskPixelsDownRight(F: TColor32; var B: TColor32; M: TColor32);
var
  E: TColor32;
begin
  E := PM_STEEL or PM_ONEWAYLEFT;
  CombineMaskPixels(F, B, M, E);
end;

procedure TLemmingGame.CombineMaskPixelsNeutral(F: TColor32; var B: TColor32; M: TColor32);
var
  E: TColor32;
begin
  E := PM_STEEL;
  CombineMaskPixels(F, B, M, E);
end;



procedure TLemmingGame.CombineNoOverwriteStoner(F: TColor32; var B: TColor32; M: TColor32);
// copy Stoner to world
begin
  if (B and PM_SOLID = 0) and (AlphaComponent(F) <> 0) then B := (B or PM_SOLID);
end;



function TLemmingGame.HasPixelAt(X, Y: Integer): Boolean;
{-------------------------------------------------------------------------------
  Read value from world.
  The function returns True when the value at (x, y) is terrain
-------------------------------------------------------------------------------}
begin
  Result := (Y < 0);
  If Result = False then
    Result := (Y < PhysicsMap.Height) and (X >= 0) and (X < PhysicsMap.Width)
                   and (PhysicsMap.Pixel[X, Y] and PM_SOLID <> 0);

  (*// Code for solid sides!
  with World do
  begin
    Result := not ((X >= 0) and (Y >= 0) and (X < Width));
    if Result = False then Result := (Y < Height) and (Pixel[X, Y] and ALPHA_TERRAIN <> 0);
  end;  *)
end;


procedure TLemmingGame.RemovePixelAt(X, Y: Integer);
begin
  PhysicsMap.PixelS[X, Y] := PhysicsMap.PixelS[X, Y] and not PM_TERRAIN;
end;

procedure TLemmingGame.MoveLemToReceivePoint(L: TLemming; oid: Byte);
var
  Inf, Inf2: TInteractiveObjectInfo;
begin
  Inf := ObjectInfos[oid];
  Assert(Inf.ReceiverId <> 65535, 'Telerporter used without receiver');
  Inf2 := ObjectInfos[Inf.ReceiverId];

  if Inf.IsFlipPhysics then TurnAround(L);

  // Mirror trigger area, if Upside-Down Flag is valid for exactly one object
  if Inf.IsUpsideDown xor Inf2.IsUpsideDown then
    L.LemY := (Inf2.TriggerRect.Bottom - 1) - (L.LemY - Inf.TriggerRect.Top)
  else
    L.LemY := Inf2.TriggerRect.Top + (L.LemY - Inf.TriggerRect.Top);

  // Mirror trigger area, if FlipLem Flag is valid for exactly one object
  // The FlipImage Flag is already taken care of when computing trigger areas.
  if Inf.IsFlipPhysics xor Inf2.IsFlipPhysics then
    L.LemX := (Inf2.TriggerRect.Right - 1) - (L.LemX - Inf.TriggerRect.Left)
  else
    L.LemX := Inf2.TriggerRect.Left + (L.LemX - Inf.TriggerRect.Left);
end;


function TLemmingGame.ReadZombieMap(X, Y: Integer): Byte;
begin
  if (X >= 0) and (X < PhysicsMap.Width) and (Y >= 0) and (Y < PhysicsMap.Height) then
    Result := ZombieMap.Value[X, Y]
  else
    Result := DOM_NONE; // whoops, important
end;

procedure TLemmingGame.WriteZombieMap(X, Y: Integer; aValue: Byte);
begin
  if (X >= 0) and (X < PhysicsMap.Width) and (Y >= 0) and (Y < PhysicsMap.Height) then
    ZombieMap.Value[X, Y] := ZombieMap.Value[X, Y] or aValue
end;

procedure TLemmingGame.SetZombieField(L: TLemming);
var
  X, Y: Integer;
begin
  with L do
  begin
    for X := LemX - 5 to LemX + 5 do
    for Y := LemY - 6 to LemY + 4 do
      WriteZombieMap(X, Y, 1);
    for Y := LemY - 6 to LemY + 4 do
      WriteZombieMap(LemX + (LemDX * 6), Y, 1);
  end;
end;

function TLemmingGame.CheckForOverlappingField(L: TLemming): Boolean;
begin
  // Check only the vertices of the new blocker field
  Result :=    HasTriggerAt(L.LemX - 5, L.LemY - 6, trBlocker)
            or HasTriggerAt(L.LemX + 5, L.LemY - 6, trBlocker)
            or HasTriggerAt(L.LemX - 5, L.LemY + 4, trBlocker)
            or HasTriggerAt(L.LemX + 5, L.LemY + 4, trBlocker);
end;


procedure TLemmingGame.Transition(L: TLemming; NewAction: TBasicLemmingAction; DoTurn: Boolean = False);
{-------------------------------------------------------------------------------
  Handling of a transition and/or turnaround
-------------------------------------------------------------------------------}
var
  i: Integer;
  OldIsStartingAction: Boolean;
const
  // Number of physics frames for the various lemming actions.
  ANIM_FRAMECOUNT: array[TBasicLemmingAction] of Integer =
    (
     0, //baNone,
     4, //baWalking,
     1, //baJumping,
    16, //baDigging,
     8, //baClimbing,
    16, //baDrowning,
     8, //baHoisting,
    16, //baBuilding,
    16, //baBashing,
    24, //baMining,
     4, //baFalling,
    17, //baFloating,
    16, //baSplatting,
     8, //baExiting,
    14, //baVaporizing,
    16, //baBlocking,
     8, //baShrugging,
    16, //baOhnoing,
     1, //baExploding,
     0, //baToWalking,
    16, //baPlatforming,
     8, //baStacking,
    16, //baStoning,
     1, //baStoneFinish,
     8, //baSwimming,
    17, //baGliding,
    16, //baFixing,
     0, //baCloning,
    16  //baFencing
    );
begin
  if DoTurn then TurnAround(L);

  //Swith from baToWalking to baWalking
  if NewAction = baToWalking then NewAction := baWalking;

  if L.LemHasBlockerField and not (NewAction in [baOhNoing, baStoning]) then
  begin
    L.LemHasBlockerField := False;
    SetBlockerMap;
  end;

  // Transition to faller instead walker, if no pixel below lemming
  if (not HasPixelAt(L.LemX, L.LemY)) and (NewAction = baWalking) then
    NewAction := baFalling;

  // Should not happen, except for assigning walkers to walkers
  if L.LemAction = NewAction then Exit;

  // Set initial fall heights according to previous skill
  if (NewAction = baFalling) then
  begin
    L.LemFallen := 1;
    if L.LemAction in [baWalking, baBashing] then L.LemFallen := 3;
    if L.LemAction in [baMining, baDigging] then L.LemFallen := 0;
    L.LemTrueFallen := L.LemFallen;
  end;

  // Change Action
  L.LemAction := NewAction;
  L.LemFrame := 0;
  L.LemPhysicsFrame := 0;
  L.LemEndOfAnimation := False;
  L.LemNumberOfBricksLeft := 0;
  OldIsStartingAction := L.LemIsStartingAction; // because for some actions (eg baHoisting) we need to restore previous value
  L.LemIsStartingAction := True;

  L.LemMaxFrame := -1;
  L.LemMaxPhysicsFrame := ANIM_FRAMECOUNT[NewAction] - 1;

  // some things to do when entering state
  case L.LemAction of
    baJumping    : L.LemJumped := 0;
    baHoisting   : L.LemIsStartingAction := OldIsStartingAction; // it needs to know what the Climber's value was
    baSplatting  : begin
                     L.LemExplosionTimer := 0;
                     CueSoundEffect(SFX_SPLAT, L.Position)
                   end;
    baBlocking   : begin
                     L.LemHasBlockerField := True;
                     SetBlockerMap;
                   end;
    baExiting    : begin
                     L.LemExplosionTimer := 0;
                     CueSoundEffect(SFX_YIPPEE, L.Position);
                   end;
    baBuilding   : L.LemNumberOfBricksLeft := 12;
    baPlatforming: L.LemNumberOfBricksLeft := 12;
    baStacking   : L.LemNumberOfBricksLeft := 8;
    baOhnoing    : CueSoundEffect(SFX_OHNO, L.Position);
    baStoning    : CueSoundEffect(SFX_OHNO, L.Position);
    baExploding  : CueSoundEffect(SFX_EXPLOSION, L.Position);
    baStoneFinish: CueSoundEffect(SFX_EXPLOSION, L.Position);
    baSwimming   : begin // If possible, float up 4 pixels when starting
                     i := 0;
                     while (i < 4) and HasTriggerAt(L.LemX, L.LemY - i - 1, trWater)
                                   and not HasPixelAt(L.LemX, L.LemY - i - 1) do
                       Inc(i);
                     Dec(L.LemY, i);
                   end;
    baFixing     : L.LemMechanicFrames := 42;

  end;
end;

procedure TLemmingGame.TurnAround(L: TLemming);
// we assume that the mirrored animations have the same framecount, key frames and physics frames
// this is safe because current code elsewhere enforces this anyway
begin
  with L do
    LemDX := -LemDX;
end;


function TLemmingGame.UpdateExplosionTimer(L: TLemming): Boolean;
begin
  Result := False;

  Dec(L.LemExplosionTimer);
  if L.LemExplosionTimer = 0 then
  begin
    if L.LemAction in [baVaporizing, baDrowning, baFloating, baGliding, baFalling, baSwimming] then
    begin
      if L.LemTimerToStone then
        Transition(L, baStoneFinish)
      else
        Transition(L, baExploding);
      end
    else begin
      if L.LemTimerToStone then
        Transition(L, baStoning)
      else
        Transition(L, baOhnoing);
    end;
    Result := True;
  end;
end;


procedure TLemmingGame.CheckForGameFinished;
begin
  if fGameFinished then
    Exit;

  if (TimePlay <= 0) and not ((GameParams.TimerMode) or (not GameParams.Level.Info.HasTimeLimit)) then
  begin
    GameResultRec.gTimeIsUp := True;
    Finish(GM_FIN_TIME);
    Exit;
  end;

  if fParticleFinishTimer > 0 then
    Exit;

  if (LemmingsIn >= Level.Info.LemmingsCount + LemmingsCloned) and (DelayEndFrames = 0) then
  begin
    Finish(GM_FIN_LEMMINGS);
    Exit;
  end;

  if ((Level.Info.LemmingsCount + LemmingsCloned - fSpawnedDead) - (LemmingsRemoved) = 0) and (DelayEndFrames = 0) then
  begin
    Finish(GM_FIN_LEMMINGS);
    Exit;
  end;

  if UserSetNuking and (LemmingsOut = 0) and (DelayEndFrames = 0) then
  begin
    Finish(GM_FIN_LEMMINGS);
    Exit;
  end;

end;

//  SETTING SIZE OF OBJECT MAPS

procedure TLemmingGame.InitializeAllObjectMaps;
begin
  SetLength(WaterMap, 0, 0); // lines like these are required to clear the arrays
  SetLength(WaterMap, Level.Info.Width, Level.Info.Height);
  SetLength(FireMap, 0, 0);
  SetLength(FireMap, Level.Info.Width, Level.Info.Height);
  SetLength(TeleporterMap, 0, 0);
  SetLength(TeleporterMap, Level.Info.Width, Level.Info.Height);
  SetLength(UpdraftMap, 0, 0);
  SetLength(UpdraftMap, Level.Info.Width, Level.Info.Height);
  SetLength(ButtonMap, 0, 0);
  SetLength(ButtonMap, Level.Info.Width, Level.Info.Height);
  SetLength(PickupMap, 0, 0);
  SetLength(PickupMap, Level.Info.Width, Level.Info.Height);
  SetLength(FlipperMap, 0, 0);
  SetLength(FlipperMap, Level.Info.Width, Level.Info.Height);
  SetLength(RadiationMap, 0, 0);
  SetLength(RadiationMap, Level.Info.Width, Level.Info.Height);
  SetLength(SlowfreezeMap, 0, 0);
  SetLength(SlowfreezeMap, Level.Info.Width, Level.Info.Height);
  SetLength(SplatMap, 0, 0);
  SetLength(SplatMap, Level.Info.Width, Level.Info.Height);
  SetLength(AntiSplatMap, 0, 0);
  SetLength(AntiSplatMap, Level.Info.Width, Level.Info.Height);
  SetLength(AnimationMap, 0, 0);
  SetLength(AnimationMap, Level.Info.Width, Level.Info.Height);
  SetLength(ExitMap, 0, 0);
  SetLength(ExitMap, Level.Info.Width, Level.Info.Height);
  SetLength(LockedExitMap, 0, 0);
  SetLength(LockedExitMap, Level.Info.Width, Level.Info.Height);
  SetLength(TrapMap, 0, 0);
  SetLength(TrapMap, Level.Info.Width, Level.Info.Height);

  BlockerMap.SetSize(Level.Info.Width, Level.Info.Height);
  BlockerMap.Clear(DOM_NONE);

  ZombieMap.SetSize(Level.Info.Width, Level.Info.Height);
  ZombieMap.Clear(0);
end;


//  BLOCKER MAP TREATMENT

procedure TLemmingGame.WriteBlockerMap(X, Y: Integer; aValue: Byte);
begin
  if (X >= 0) and (X < PhysicsMap.Width) and (Y >= 0) and (Y < PhysicsMap.Height) then
    BlockerMap.Value[X, Y] := aValue;
end;

function TLemmingGame.ReadBlockerMap(X, Y: Integer): Byte;
var
  LemPosRect: TRect;
  i: Integer;
begin
  if (X >= 0) and (X < Level.Info.Width) and (Y >= 0) and (Y < Level.Info.Height) then
  begin
    Result := BlockerMap.Value[X, Y];

    // For simulations check in addition if the trigger area does not come from a blocker with removed terrain under his feet
    if IsSimulating and (Result in [DOM_FORCERIGHT, DOM_FORCELEFT]) then
    begin
      if Result = DOM_FORCERIGHT then
        LemPosRect := Rect(X - 6, Y - 5, X - 1, Y + 6)
      else
        LemPosRect := Rect(X + 2, Y - 5, X + 7, Y + 6);

      for i := 0 to LemmingList.Count - 1 do
      begin
        if     LemmingList[i].LemHasBlockerField
           and PtInRect(LemPosRect, Point(LemmingList[i].LemX, LemmingList[i].LemY))
           and not HasPixelAt(LemmingList[i].LemX, LemmingList[i].LemY) then
        begin
          Result := DOM_NONE;
          Exit;
        end;
      end;
    end;
  end
  else
    Result := DOM_NONE; // whoops, important
end;

procedure TLemmingGame.SetBlockerMap();
var
  i: Integer;

  procedure SetBlockerField(L: TLemming);
  var
    X, Y, Step: Integer;
  begin
    X := L.LemX - 6;
    if L.LemDx = 1 then Inc(X);

    for Step := 0 to 11 do
      for Y := L.LemY - 6 to L.LemY + 4 do
        case Step of
          0..3: WriteBlockerMap(X + Step, Y, DOM_FORCELEFT);
          4..7: WriteBlockerMap(X + Step, Y, DOM_BLOCKER);
          8..11: WriteBlockerMap(X + Step, Y, DOM_FORCERIGHT);
        end;
  end;

  procedure SetForceField(Rect: TRect; Direction: Integer);
  var
    X, Y: Integer;
  begin
    for X := Rect.Left to Rect.Right - 1 do
    for Y := Rect.Top to Rect.Bottom - 1 do
      WriteBlockerMap(X, Y, Direction);
  end;

begin
  BlockerMap.Clear(DOM_NONE);

  // First add all force fields
  for i := 0 to ObjectInfos.Count - 1 do
    if ObjectInfos[i].TriggerEffect in [DOM_FORCELEFT, DOM_FORCERIGHT] then
      SetForceField(ObjectInfos[i].TriggerRect, ObjectInfos[i].TriggerEffect);

  // Then add all blocker fields
  for i := 0 to LemmingList.Count-1 do
    if LemmingList[i].LemHasBlockerField and not LemmingList[i].LemRemoved then
      SetBlockerField(LemmingList[i]);
end;


//  OBJECT MAP TREATMENT

procedure TLemmingGame.WriteTriggerMap(Map: TArrayArrayBoolean; Rect: TRect);
var
  X, Y: Integer;
begin
  for X := Rect.Left to Rect.Right - 1 do
  for Y := Rect.Top to Rect.Bottom - 1 do
    if (X >= 0) and (X < Level.Info.Width) and (Y >= 0) and (Y < Level.Info.Height) then
      Map[X, Y] := True;
end;

function TLemmingGame.ReadTriggerMap(X, Y: Integer; Map: TArrayArrayBoolean): Boolean;
begin
  if (X >= 0) and (X < Level.Info.Width) and (Y >= 0) and (Y < Level.Info.Height) then
    Result := Map[X, Y]
  else
    Result := False;
end;

procedure TLemmingGame.SetObjectMap;
// WARNING: Only call this after InitializeAllObjectMaps
// Otherwise the maps might already contain trigger areas
var
  i: Integer;
begin
  for i := 0 to ObjectInfos.Count - 1 do
  begin
    case ObjectInfos[i].TriggerEffect of
      DOM_EXIT:       WriteTriggerMap(ExitMap, ObjectInfos[i].TriggerRect);
      DOM_LOCKEXIT:
          begin
            WriteTriggerMap(LockedExitMap, ObjectInfos[i].TriggerRect);
            if ButtonsRemain = 0 then ObjectInfos[i].CurrentFrame := 0;
          end;
      DOM_WATER:      WriteTriggerMap(WaterMap, ObjectInfos[i].TriggerRect);
      DOM_FIRE:       WriteTriggerMap(FireMap, ObjectInfos[i].TriggerRect);
      DOM_TRAP:       WriteTriggerMap(TrapMap, ObjectInfos[i].TriggerRect);
      DOM_TRAPONCE:   WriteTriggerMap(TrapMap, ObjectInfos[i].TriggerRect);
      DOM_TELEPORT:   WriteTriggerMap(TeleporterMap, ObjectInfos[i].TriggerRect);
      DOM_UPDRAFT:    WriteTriggerMap(UpdraftMap, ObjectInfos[i].TriggerRect);
      DOM_PICKUP:     WriteTriggerMap(PickupMap, ObjectInfos[i].TriggerRect);
      DOM_BUTTON:     WriteTriggerMap(ButtonMap, ObjectInfos[i].TriggerRect);
      DOM_FLIPPER:    WriteTriggerMap(FlipperMap, ObjectInfos[i].TriggerRect);
      DOM_RADIATION:  WriteTriggerMap(RadiationMap, ObjectInfos[i].TriggerRect);
      DOM_SLOWFREEZE: WriteTriggerMap(SlowfreezeMap, ObjectInfos[i].TriggerRect);
      DOM_SPLAT:      WriteTriggerMap(SplatMap, ObjectInfos[i].TriggerRect);
      DOM_NOSPLAT:    WriteTriggerMap(AntiSplatMap, ObjectInfos[i].TriggerRect);
      DOM_ANIMATION:  WriteTriggerMap(AnimationMap, ObjectInfos[i].TriggerRect);
    end;
  end;
end;

function TLemmingGame.AssignNewSkill(Skill: TBasicLemmingAction; IsHighlight: Boolean = False; IsReplayAssignment: Boolean = false): Boolean;
const
  PermSkillSet = [baClimbing, baFloating, baGliding, baFixing, baSwimming];
var
  L, LQueue: TLemming;
begin
  Result := False;

  // Just to be safe, though this should always return in fLemSelected
  GetPriorityLemming(L, Skill, CursorPoint, IsHighlight);
  // Get lemming to queue the skill assignment
  GetPriorityLemming(LQueue, baNone, CursorPoint);

  // Queue skill assignment if current assignment is impossible
  if not Assigned(L) or not CheckSkillAvailable(Skill) then
  begin
    if Assigned(LQueue) and not (Skill in PermSkillSet) then
    begin
      LQueue.LemQueueAction := Skill;
      LQueue.LemQueueFrame := 0;
    end;
  end

  // If the assignment is written in the replay, change lemming state
  else if IsReplayAssignment then
  begin
    Result := DoSkillAssignment(L, Skill);
    if Result then
    begin
      CueSoundEffect(SFX_ASSIGN_SKILL, L.Position);
      Inc(L.LemUsedSkillCount);
    end;
  end

  // record new skill assignment to be assigned once we call again UpdateLemmings
  else
  begin
    Result := CheckSkillAvailable(Skill);
    if Result then
    begin
      RegainControl;
      RecordSkillAssignment(L, Skill);
    end;
  end;
end;


function TLemmingGame.DoSkillAssignment(L: TLemming; NewSkill: TBasicLemmingAction): Boolean;
begin
  Result := False;

  // We check first, whether the skill is available at all
  if not CheckSkillAvailable(NewSkill) then Exit;

  UpdateSkillCount(NewSkill);

  // Remove queued skill assignment
  L.LemQueueAction := baNone;
  L.LemQueueFrame := 0;

  // Get starting position for stacker
  if (Newskill = baStacking) then L.LemStackLow := not HasPixelAt(L.LemX + L.LemDx, L.LemY);

  // Important! If a builder just placed a brick and part of the previous brick
  // got removed, he should not fall if turned into a walker!
  if     (NewSkill = baToWalking) and (L.LemAction = baBuilding)
     and HasPixelAt(L.LemX, L.LemY - 1) and not HasPixelAt(L.LemX + L.LemDx, L.LemY) then
    L.LemY := L.LemY - 1;

  // Turn around walking lem, if assigned a walker
  if (NewSkill = baToWalking) and (L.LemAction = baWalking) then
  begin
    TurnAround(L);

    // Special treatment if in one-way-field facing the wrong direction
    // see http://www.lemmingsforums.net/index.php?topic=2640.0
    if    (HasTriggerAt(L.LemX, L.LemY, trForceRight) and (L.LemDx = -1))
       or (HasTriggerAt(L.LemX, L.LemY, trForceLeft) and (L.LemDx = 1)) then
    begin
      // Go one back to cancel the Inc(L.LemX, L.LemDx) in HandleWalking
      // unless the Lem will fall down (which is handles already in Transition)
      if HasPixelAt(L.LemX, L.LemY) then Dec(L.LemX, L.LemDx);
    end;
  end;

  // Special behavior of permament skills.
  if (NewSkill = baClimbing) then L.LemIsClimber := True
  else if (NewSkill = baFloating) then L.LemIsFloater := True
  else if (NewSkill = baGliding) then L.LemIsGlider := True
  else if (NewSkill = baFixing) then L.LemIsMechanic := True
  else if (NewSkill = baSwimming) then
  begin
    L.LemIsSwimmer := True;
    if L.LemAction = baDrowning then Transition(L, baSwimming);
  end
  else if (NewSkill = baExploding) then
  begin
    L.LemExplosionTimer := 1;
    L.LemTimerToStone := False;
    L.LemHideCountdown := True;
  end
  else if (NewSkill = baStoning) then
  begin
    L.LemExplosionTimer := 1;
    L.LemTimerToStone := True;
    L.LemHideCountdown := True;
  end
  else if (NewSkill = baCloning) then
  begin
    Inc(LemmingsCloned);
    GenerateClonedLem(L);
  end
  else Transition(L, NewSkill);

  Result := True;
end;


procedure TLemmingGame.GenerateClonedLem(L: TLemming);
var
  NewL: TLemming;
begin
  Assert(not L.LemIsZombie, 'cloner assigned to zombie');

  NewL := TLemming.Create;
  NewL.Assign(L);
  NewL.LemIndex := LemmingList.Count;
  LemmingList.Add(NewL);
  TurnAround(NewL);
  NewL.LemUsedSkillCount := 0;
  Inc(LemmingsOut);

  // Avoid moving into terrain, see http://www.lemmingsforums.net/index.php?topic=2575.0
  if NewL.LemAction = baMining then
  begin
    if NewL.LemPhysicsFrame = 2 then
      ApplyMinerMask(NewL, 1, 0, 0)
    else if (NewL.LemPhysicsFrame >= 3) and (NewL.LemPhysicsFrame < 15) then
      ApplyMinerMask(NewL, 1, -2*NewL.LemDx, -1);
  end
  // Required for turned builders not to walk into air
  // For platformers, see http://www.lemmingsforums.net/index.php?topic=2530.0
  else if (NewL.LemAction in [baBuilding, baPlatforming]) and (NewL.LemPhysicsFrame >= 9) then
    LayBrick(NewL);
end;


function TLemmingGame.GetPriorityLemming(out PriorityLem: TLemming;
                                          NewSkillOrig: TBasicLemmingAction;
                                          MousePos: TPoint;
                                          IsHighlight: Boolean = False): Integer;
const
  NonPerm = 0;
  Perm = 1;
  NonWalk = 2;
  Walk = 3;
var
  i, CurPriorityBox: Integer;
  // CurValue = 1, 2, 3, 4: Lem is assignable and in one PriorityBox
  // CurValue = 8: Lem is unassignable, but no zombie
  // CurValue = 9: Lem is zombie
  CurValue: Integer;
  L: TLemming;
  LemIsInBox: Boolean;
  NumLemInCursor: Integer;
  NewSkill: TBasicLemmingAction;

  function LemIsInCursor(L: TLemming; MousePos: TPoint): Boolean;
  var
    X, Y: Integer;
  begin
    X := L.LemX - 8;
    Y := L.LemY - 10;
    Result := PtInRect(Rect(X, Y, X + 13, Y + 13), MousePos);
  end;


  function GetLemDistance(L: TLemming; MousePos: TPoint): Integer;
  begin
    // We compute the distance to the center of the cursor after 2x-zooming, to have integer values
    Result :=   Sqr(2 * (L.LemX - 8) - 2 * MousePos.X + 13)
              + Sqr(2 * (L.LemY - 10) - 2 * MousePos.Y + 13)
  end;

  function IsCloserToCursorCenter(LOld, LNew: TLemming; MousePos: TPoint): Boolean;
  begin
    Result := (GetLemDistance(LNew, MousePos) < GetLemDistance(LOld, MousePos));
  end;

  function IsLemInPriorityBox(L: TLemming; PriorityBox: Integer): Boolean;
  begin
    Result := True;
    case PriorityBox of
      Perm    : Result :=     (L.LemIsClimber or L.LemIsSwimmer or L.LemIsFloater
                                    or L.LemIsGlider or L.LemIsMechanic);
      NonPerm : Result :=     (L.LemAction in [baBashing, baFencing, baMining, baDigging, baBuilding,
                                               baPlatforming, baStacking, baBlocking, baShrugging]);
      Walk    : Result :=     (L.LemAction in [baWalking, baJumping]);
      NonWalk : Result := not (L.LemAction in [baWalking, baJumping]);
    end;
  end;

begin
  PriorityLem := nil;

  if fHitTestAutoFail then
  begin
    Result := 0;
    Exit;
  end;

  NumLemInCursor := 0;
  CurValue := 10;
  if NewSkillOrig = baNone then
  begin
    NewSkill := SkillPanelButtonToAction[fSelectedSkill];
    // Set NewSkill if level has no skill at all in the skillbar
    if NewSkill = baNone then NewSkill := baExploding;
  end
  else
    NewSkill := NewSkillOrig;

  for i := (LemmingList.Count - 1) downto 0 do
  begin
    L := LemmingList.List^[i];

    // Check if we only look for highlighted Lems
    if IsHighlight and not (L = GetHighlitLemming) then Continue;
    // Does Lemming exist
    if L.LemRemoved or L.LemTeleporting then Continue;
    // Is the Lemming a Zombie (remove unless we haven't yet had any lem under the cursor)
    if L.LemIsZombie and Assigned(PriorityLem) then Continue;
    // Is Lemming inside cursor (only check if we are not using Hightlightning!)
    if (not LemIsInCursor(L, MousePos)) and (not IsHighlight) then Continue;
    // Directional select
    if (fSelectDx <> 0) and (fSelectDx <> L.LemDx) then Continue;
    // Select unassigned lemming
    if IsSelectUnassignedHotkey and (L.LemUsedSkillCount > 0) then Continue;
    // Select only walkers
    if IsSelectWalkerHotkey and (L.LemAction <> baWalking) then Continue;

    // Increase number of lemmings in cursor (if not a zombie)
    if not L.LemIsZombie then Inc(NumLemInCursor);

    // Determine priority class of current lemming
    if IsSelectUnassignedHotkey or IsSelectWalkerHotkey then
      CurPriorityBox := 1
    else
    begin
      CurPriorityBox := 0;
      repeat
        LemIsInBox := IsLemInPriorityBox(L, CurPriorityBox {PriorityBoxOrder[CurPriorityBox]});
        Inc(CurPriorityBox);
      until (CurPriorityBox > MinIntValue([CurValue, 4])) or LemIsInBox;
    end;

    // Can this lemmings actually receive the skill?
    if not NewSkillMethods[NewSkill](L) then CurPriorityBox := 8;

    // Deprioritize zombie even when just counting lemmings
    if L.LemIsZombie then CurPriorityBox := 9;

    if     (CurPriorityBox < CurValue)
       or ((CurPriorityBox = CurValue) and IsCloserToCursorCenter(PriorityLem, L, MousePos)) then
    begin
      // New top priority lemming found
      PriorityLem := L;
      CurValue := CurPriorityBox;
    end;
  end;

  //  Delete PriorityLem if too low-priority and we wish to assign a skill
  if (CurValue > 6) and not (NewSkillOrig = baNone) then PriorityLem := nil;

  Result := NumLemInCursor;
end;

function TLemmingGame.MayAssignWalker(L: TLemming): Boolean;
const
  ActionSet = [baWalking, baShrugging, baBlocking, baPlatforming, baBuilding,
               baStacking, baBashing, baFencing, baMining, baDigging];
begin
  Result := (L.LemAction in ActionSet);
end;

function TLemmingGame.MayAssignClimber(L: TLemming): Boolean;
const
  ActionSet = [baOhnoing, baStoning, baExploding, baStoneFinish, baDrowning,
               baVaporizing, baSplatting, baExiting];
begin
  Result := (not (L.LemAction in ActionSet)) and not L.LemIsClimber;
end;

function TLemmingGame.MayAssignFloaterGlider(L: TLemming): Boolean;
const
  ActionSet = [baOhnoing, baStoning, baExploding, baStoneFinish, baDrowning,
               baVaporizing, baSplatting, baExiting];
begin
  Result := (not (L.LemAction in ActionSet)) and not (L.LemIsFloater or L.LemIsGlider);
end;

function TLemmingGame.MayAssignSwimmer(L: TLemming): Boolean;
const
  ActionSet = [baOhnoing, baStoning, baExploding, baStoneFinish, baVaporizing,
               baSplatting, baExiting];   // Does NOT contain baDrowning!
begin
  Result := (not (L.LemAction in ActionSet)) and not L.LemIsSwimmer;
end;

function TLemmingGame.MayAssignMechanic(L: TLemming): Boolean;
const
  ActionSet = [baOhnoing, baStoning, baExploding, baStoneFinish, baDrowning,
               baVaporizing, baSplatting, baExiting];
begin
  Result := (not (L.LemAction in ActionSet)) and not L.LemIsMechanic;
end;

function TLemmingGame.MayAssignBlocker(L: TLemming): Boolean;
const
  ActionSet = [baWalking, baShrugging, baPlatforming, baBuilding, baStacking,
               baBashing, baFencing, baMining, baDigging];
begin
  Result := (L.LemAction in ActionSet) and not CheckForOverlappingField(L);
end;

function TLemmingGame.MayAssignExploderStoner(L: TLemming): Boolean;
const
  ActionSet = [baOhnoing, baStoning, baDrowning, baExploding, baStoneFinish,
               baVaporizing, baSplatting, baExiting];
begin
  Result := not (L.LemAction in ActionSet);
end;


function TLemmingGame.MayAssignBuilder(L: TLemming): Boolean;
const
  ActionSet = [baWalking, baShrugging, baPlatforming, baStacking, baBashing,
               baFencing, baMining, baDigging];
begin
  Result := (L.LemAction in ActionSet) and not (L.LemY <= 1);
end;

function TLemmingGame.MayAssignPlatformer(L: TLemming): Boolean;
const
  ActionSet = [baWalking, baShrugging, baBuilding, baStacking, baBashing,
               baFencing, baMining, baDigging];
var
  n: Integer;
begin
  // Next brick must add at least one pixel, but contrary to LemCanPlatform
  // we ignore pixels above the platform
  Result := False;
  for n := 0 to 5 do
    Result := Result or not HasPixelAt(L.LemX + n*L.LemDx, L.LemY);

  // Test current action
  Result := Result and (L.LemAction in ActionSet) and LemCanPlatform(L);
end;

function TLemmingGame.MayAssignStacker(L: TLemming): Boolean;
const
  ActionSet = [baWalking, baShrugging, baPlatforming, baBuilding, baBashing,
               baFencing, baMining, baDigging];
begin
  Result := (L.LemAction in ActionSet);
end;

function TLemmingGame.MayAssignBasher(L: TLemming): Boolean;
const
  ActionSet = [baWalking, baShrugging, baPlatforming, baBuilding, baStacking,
               baFencing, baMining, baDigging];
begin
  Result := (L.LemAction in ActionSet);
end;

function TLemmingGame.MayAssignFencer(L: TLemming): Boolean;
const
  ActionSet = [baWalking, baShrugging, baPlatforming, baBuilding, baStacking,
               baBashing, baMining, baDigging];
begin
  Result := (L.LemAction in ActionSet);
end;

function TLemmingGame.MayAssignMiner(L: TLemming): Boolean;
const
  ActionSet = [baWalking, baShrugging, baPlatforming, baBuilding, baStacking,
               baBashing, baFencing, baDigging];
begin
  Result := (L.LemAction in ActionSet)
            and not HasIndestructibleAt(L.LemX, L.LemY, L.LemDx, baMining)
end;

function TLemmingGame.MayAssignDigger(L: TLemming): Boolean;
const
  ActionSet = [baWalking, baShrugging, baPlatforming, baBuilding, baStacking,
               baBashing, baFencing, baMining];
begin
  Result := (L.LemAction in ActionSet) and not HasIndestructibleAt(L.LemX, L.LemY, L.LemDx, baDigging);
end;

function TLemmingGame.MayAssignCloner(L: TLemming): Boolean;
const
  ActionSet = [baWalking, baShrugging, baPlatforming, baBuilding, baStacking,
               baBashing, baFencing, baMining, baDigging, baJumping, baFalling,
               baFloating, baSwimming, baGliding, baFixing];
begin
  Result := (L.LemAction in ActionSet);
end;

function TLemmingGame.GetObjectCheckPositions(L: TLemming): TArrayArrayInt;
// The intermediate checks are made according to:
// http://www.lemmingsforums.net/index.php?topic=2604.7
var
  CurrPosX, CurrPosY: Integer;
  n: Integer;

  procedure SaveCheckPos;
  begin
    Result[0, n] := CurrPosX;
    Result[1, n] := CurrPosY;
    Inc(n);
  end;

  procedure MoveHoizontal;
  begin
    while CurrPosX <> L.LemX do
    begin
      Inc(CurrPosX, sign(L.LemX - L.LemXOld));
      SaveCheckPos;
    end;
  end;

  procedure MoveVertical;
  begin
    while CurrPosY <> L.LemY do
    begin
      Inc(CurrPosY, sign(L.LemY - L.LemYOld));
      SaveCheckPos;
    end;
  end;

begin
  SetLength(Result, 2, 11);

  n := 0;
  CurrPosX := L.LemXOld;
  CurrPosY := L.LemYOld;
  // no movement
  if (L.LemX = L.LemXOld) and (L.LemY = L.LemYOld) then
    SaveCheckPos

  // special treatment of miners!
  else if L.LemActionOld = baMining then
  begin
    // First move one pixel down, if Y-coordinate changed
    if L.LemYOld < L.LemY then
    begin
      Inc(CurrPosY);
      SaveCheckPos;
    end;
    MoveHoizontal;
    MoveVertical;
  end

  // lem moves up or is faller; exception is made for builders!
  else if ((L.LemY < L.LemYOld) or (L.LemAction = baFalling)) and not (L.LemActionOld = baBuilding) then
  begin
    MoveHoizontal;
    MoveVertical;
  end

  // lem moves down (or straight) and is no faller; alternatively lem is a builder!
  else
  begin
    MoveVertical;
    MoveHoizontal;
  end;
end;


procedure TLemmingGame.CheckTriggerArea(L: TLemming);
// For intermediate pixels, we call the trigger function according to trigger area
var
  CheckPos: TArrayArrayInt; // Combined list for both X- and Y-coordinates
  i: Integer;
  AbortChecks: Boolean;
begin
  // Get positions to check for trigger areas
  CheckPos := GetObjectCheckPositions(L);

  // Now move through the values in CheckPosX/Y and check for trigger areas
  i := -1;
  AbortChecks := False;
  repeat
    Inc(i);

    // Make sure, that we do not move outside the range of CheckPos.
    Assert(i <= Length(CheckPos[0]), 'CheckTriggerArea: CheckPos has not enough entries');
    Assert(i <= Length(CheckPos[1]), 'CheckTriggerArea: CheckPos has not enough entries');

    // Transition if we are at the end position and need to do one
    if (fLemNextAction <> baNone) and ([CheckPos[0, i], CheckPos[1, i]] = [L.LemX, L.LemY]) then
    begin
      Transition(L, fLemNextAction);
      fLemNextAction := baNone;
    end;

    // Animations - the most useless of objects...
    if HasTriggerAt(CheckPos[0, i], CheckPos[1, i], trAnimation) then
      HandleObjAnimation(L, CheckPos[0, i], CheckPos[1, i]);

    // Pickup Skills
    if HasTriggerAt(CheckPos[0, i], CheckPos[1, i], trPickup) then
      HandlePickup(L, CheckPos[0, i], CheckPos[1, i]);

    // Buttons
    if HasTriggerAt(CheckPos[0, i], CheckPos[1, i], trButton) then
      HandleButton(L, CheckPos[0, i], CheckPos[1, i]);

    // Fire
    if HasTriggerAt(CheckPos[0, i], CheckPos[1, i], trFire) then
      AbortChecks := HandleFire(L);

    // Water - Check only for drowning here!
    if (not AbortChecks) and HasTriggerAt(CheckPos[0, i], CheckPos[1, i], trWater) then
      AbortChecks := HandleWaterDrown(L);

    // Triggered traps and one-shot traps
    if (not AbortChecks) and HasTriggerAt(CheckPos[0, i], CheckPos[1, i], trTrap) then
    begin
      AbortChecks := HandleTrap(L, CheckPos[0, i], CheckPos[1, i]);
      // Disarmers move always to final X-position, see http://www.lemmingsforums.net/index.php?topic=3004.0
      if (L.LemAction = baFixing) then CheckPos[0, i] := L.LemX;
    end;

    // Radiation
    if (not AbortChecks) and HasTriggerAt(CheckPos[0, i], CheckPos[1, i], trRadiation) then
      HandleRadiation(L, False);

    // Slowfreeze
    if (not AbortChecks) and HasTriggerAt(CheckPos[0, i], CheckPos[1, i], trSlowfreeze) then
      HandleRadiation(L, True);

    // Teleporter
    if (not AbortChecks) and HasTriggerAt(CheckPos[0, i], CheckPos[1, i], trTeleport) then
      AbortChecks := HandleTeleport(L, CheckPos[0, i], CheckPos[1, i]);

    // Exits
    if (not AbortChecks) and HasTriggerAt(CheckPos[0, i], CheckPos[1, i], trExit) then
      AbortChecks := HandleExit(L);

    // Flipper (except for blockers)
    if (not AbortChecks) and HasTriggerAt(CheckPos[0, i], CheckPos[1, i], trFlipper)
                         and not (L.LemAction = baBlocking) then
      AbortChecks := HandleFlipper(L, CheckPos[0, i], CheckPos[1, i]);

    // If the lem was required stop, move him there!
    if AbortChecks then
    begin
      L.LemX := CheckPos[0, i];
      L.LemY := CheckPos[1, i];
    end;

    // Set L.LemInFlipper correctly
    if not HasTriggerAt(CheckPos[0, i], CheckPos[1, i], trFlipper) then
      L.LemInFlipper := DOM_NOOBJECT;
  until [CheckPos[0, i], CheckPos[1, i]] = [L.LemX, L.LemY] (*or AbortChecks*);

  // Check for water to transition to swimmer only at final position
  if HasTriggerAt(L.LemX, L.LemY, trWater) then
    HandleWaterSwim(L);

  // Check for blocker fields and force-fields
  // but not for miners removing terrain, see http://www.lemmingsforums.net/index.php?topic=2710.0
  if (L.LemAction <> baMining) or not (L.LemPhysicsFrame in [1, 2]) then
  begin
    if HasTriggerAt(L.LemX, L.LemY, trForceLeft) then
      HandleForceField(L, -1)
    else if HasTriggerAt(L.LemX, L.LemY, trForceRight) then
      HandleForceField(L, 1);
  end;
end;

function TLemmingGame.HasTriggerAt(X, Y: Integer; TriggerType: TTriggerTypes): Boolean;
// Checks whether the trigger area TriggerType occurs at position (X, Y)
begin
  Result := False;

  case TriggerType of
    trExit:       Result :=     ReadTriggerMap(X, Y, ExitMap)
                             or ((ButtonsRemain = 0) and ReadTriggerMap(X, Y, LockedExitMap));
    trForceLeft:  Result :=     (ReadBlockerMap(X, Y) = DOM_FORCELEFT);
    trForceRight: Result :=     (ReadBlockerMap(X, Y) = DOM_FORCERIGHT);
    trTrap:       Result :=     ReadTriggerMap(X, Y, TrapMap);
    trWater:      Result :=     ReadTriggerMap(X, Y, WaterMap);
    trFire:       Result :=     ReadTriggerMap(X, Y, FireMap);
    trOWLeft:     Result :=     (PhysicsMap.Pixel[X, Y] and PM_ONEWAYLEFT <> 0);
    trOWRight:    Result :=     (PhysicsMap.Pixel[X, Y] and PM_ONEWAYRIGHT <> 0);    // are these ever used? (the one-way wall ones)
    trOWDown:     Result :=     (PhysicsMap.Pixel[X, Y] and PM_ONEWAYDOWN <> 0);
    trSteel:      Result :=     (PhysicsMap.Pixel[X, Y] and PM_STEEL <> 0);
    trBlocker:    Result :=     (ReadBlockerMap(X, Y) = DOM_BLOCKER)
                            or  (ReadBlockerMap(X, Y) = DOM_FORCERIGHT)
                            or  (ReadBlockerMap(X, Y) = DOM_FORCELEFT);
    trTeleport:   Result :=     ReadTriggerMap(X, Y, TeleporterMap);
    trPickup:     Result :=     ReadTriggerMap(X, Y, PickupMap);
    trButton:     Result :=     ReadTriggerMap(X, Y, ButtonMap);
    trRadiation:  Result :=     ReadTriggerMap(X, Y, RadiationMap);
    trSlowfreeze: Result :=     ReadTriggerMap(X, Y, SlowfreezeMap);
    trUpdraft:    Result :=     ReadTriggerMap(X, Y, UpdraftMap);
    trFlipper:    Result :=     ReadTriggerMap(X, Y, FlipperMap);
    trSplat:      Result :=     ReadTriggerMap(X, Y, SplatMap);
    trNoSplat:    Result :=     ReadTriggerMap(X, Y, AntiSplatMap);
    trZombie:     Result :=     (ReadZombieMap(X, Y) and 1 <> 0);
    trAnimation:  Result :=     ReadTriggerMap(X, Y, AnimationMap);
  end;
end;

function TLemmingGame.FindObjectID(X, Y: Integer; TriggerType: TTriggerTypes): Word;
// finds a suitable object that has the correct trigger type and is not currently active.
var
  ObjectID: Word;
  ObjectFound: Boolean;
  Inf: TInteractiveObjectInfo;
begin
  // Because ObjectTypeToTrigger defaults to trZombie, looking for this trigger type is nonsense!
  Assert(TriggerType <> trZombie, 'FindObjectId called for trZombie');

  ObjectID := ObjectInfos.Count;
  ObjectFound := False;
  repeat
    Dec(ObjectID);
    Inf := ObjectInfos[ObjectID];
    // Check correct TriggerType
    if (ObjectTypeToTrigger[Inf.TriggerEffect] = TriggerType)
       and not Inf.IsDisabled then   // shouldn't be necessary, but to be sure, it stays here
    begin
      // Check trigger areas for this object
      if PtInRect(Inf.TriggerRect, Point(X, Y)) then
        ObjectFound := True;
    end;

    // Additional checks for locked exit
    if (Inf.TriggerEffect = DOM_LOCKEXIT) and not (ButtonsRemain = 0) then
      ObjectFound := False;
    // Additional checks for triggered traps, triggered animations, teleporters
    if Inf.Triggered then
      ObjectFound := False;
    // ignore already used buttons, one-shot traps and pick-up skills
    if     (Inf.TriggerEffect in [DOM_BUTTON, DOM_TRAPONCE, DOM_PICKUP])
       and (Inf.CurrentFrame = 0) then  // other objects have always CurrentFrame = 0, so the first check is needed!
      ObjectFound := False;
    // Additional check, that the corresponding receiver is inactive
    if     (Inf.TriggerEffect = DOM_TELEPORT)
       and (ObjectInfos[Inf.ReceiverId].Triggered or ObjectInfos[Inf.ReceiverId].HoldActive) then
      ObjectFound := False;

  until ObjectFound or (ObjectID = 0);

  if ObjectFound then
    Result := ObjectID
  else
    Result := 65535;
end;


function TLemmingGame.HandleTrap(L: TLemming; PosX, PosY: Integer): Boolean;
var
  Inf: TInteractiveObjectInfo;
  ObjectID: Word;
begin
  Result := True;

  ObjectID := FindObjectID(PosX, PosY, trTrap);
  // Exit if there is no Object
  if ObjectID = 65535 then
  begin
    Result := False;
    Exit;
  end;

  // Set ObjectInfos
  Inf := ObjectInfos[ObjectID];

  if     L.LemIsMechanic and HasPixelAt(PosX, PosY) // (PosX, PosY) is the correct current lemming position, due to intermediate checks!
     and not (L.LemAction in [baClimbing, baHoisting, baSwimming, baOhNoing]) then
  begin
    // Set action after fixing, if we are moving upwards and haven't reached the top yet
    if (L.LemYOld > L.LemY) and HasPixelAt(PosX, PosY + 1) then L.LemActionNew := baJumping
    else L.LemActionNew := baWalking;

    Inf.IsDisabled := True;
    Transition(L, baFixing);
  end
  else
  begin
    // trigger trap
    Inf.Triggered := True;
    Inf.ZombieMode := L.LemIsZombie;
    // Make sure to remove the blocker field!
    L.LemHasBlockerField := False;
    SetBlockerMap;
    RemoveLemming(L, RM_KILL);
    CueSoundEffect(Inf.SoundEffect, L.Position);
    DelayEndFrames := MaxIntValue([DelayEndFrames, Inf.AnimationFrameCount]);
    // Check for one-shot trap and possibly disable it
    if Inf.TriggerEffect = DOM_TRAPONCE then Inf.IsDisabled := True;
  end;
end;


function TLemmingGame.HandleObjAnimation(L: TLemming; PosX, PosY: Integer): Boolean;
var
  Inf: TInteractiveObjectInfo;
  ObjectID: Word;
begin
  Result := False;
  ObjectID := FindObjectID(PosX, PosY, trAnimation);
  // Exit if there is no Object
  if ObjectID = 65535 then Exit;

  Inf := ObjectInfos[ObjectID];
  Inf.Triggered := True;
  CueSoundEffect(Inf.SoundEffect, L.Position);
end;


function TLemmingGame.HandleTeleport(L: TLemming; PosX, PosY: Integer): Boolean;
var
  Inf: TInteractiveObjectInfo;
  ObjectID: Word;
begin
  Result := False;

  // Exit if lemming is splatting
  if L.LemAction = baSplatting then Exit;
  // Exit if lemming is falling, has ground under his feet and will splat
  if (L.LemAction = baFalling) and HasPixelAt(PosX, PosY) and (L.LemFallen > MAX_FALLDISTANCE) then Exit;

  ObjectID := FindObjectID(PosX, PosY, trTeleport);

  // Exit if there is no Object
  if ObjectID = 65535 then Exit;

  Result := True;

  Inf := ObjectInfos[ObjectID];

  Assert((Inf.ReceiverID >= 0) and (Inf.ReceiverID < ObjectInfos.Count), 'ReceiverID for teleporter out of bounds.');
  Assert(ObjectInfos[Inf.ReceiverID].TriggerEffect = DOM_RECEIVER, 'Receiving object for teleporter has wrong trigger effect.');

  Inf.Triggered := True;
  Inf.ZombieMode := L.LemIsZombie;
  CueSoundEffect(Inf.SoundEffect, L.Position);
  L.LemTeleporting := True;
  Inf.TeleLem := L.LemIndex;
  // Make sure to remove the blocker field!
  L.LemHasBlockerField := False;
  SetBlockerMap;

  ObjectInfos[Inf.ReceiverID].HoldActive := True;
end;

function TLemmingGame.HandlePickup(L: TLemming; PosX, PosY: Integer): Boolean;
var
  Inf: TInteractiveObjectInfo;
  ObjectID: Word;
begin
  Result := False;

  ObjectID := FindObjectID(PosX, PosY, trPickup);
  // Exit if there is no Object
  if ObjectID = 65535 then Exit;

  if not L.LemIsZombie then
  begin
    Inf := ObjectInfos[ObjectID];
    Inf.CurrentFrame := 0;
    CueSoundEffect(SFX_PICKUP, L.Position);
    UpdateSkillCount(SkillPanelButtonToAction[Inf.SkillType], true);
  end;
end;


function TLemmingGame.HandleButton(L: TLemming; PosX, PosY: Integer): Boolean;
var
  Inf: TInteractiveObjectInfo;
  n: Integer;
  ObjectID: Word;
begin
  Result := False;

  ObjectID := FindObjectID(PosX, PosY, trButton);
  // Exit if there is no Object
  if ObjectID = 65535 then Exit;

  if not L.LemIsZombie then
  begin
    Inf := ObjectInfos[ObjectID];
    CueSoundEffect(Inf.SoundEffect, L.Position);
    Inf.Triggered := True;
    Dec(ButtonsRemain);

    if ButtonsRemain = 0 then
    begin
      for n := 0 to (ObjectInfos.Count - 1) do
        if ObjectInfos[n].TriggerEffect = DOM_LOCKEXIT then
        begin
          Inf := ObjectInfos[n];
          Inf.Triggered := True;
          if Inf.SoundEffect = '' then
            CueSoundEffect(SFX_ENTRANCE, Inf.Center)
          else
            CueSoundEffect(Inf.SoundEffect, Inf.Center);
        end;
    end;
  end;
end;

function TLemmingGame.HandleExit(L: TLemming): Boolean;
begin
  Result := False; // only see exit trigger area, if it actually used

  if     (not L.LemIsZombie)
     and (not (L.LemAction in [baFalling, baSplatting]))
     and (HasPixelAt(L.LemX, L.LemY) or not (L.LemAction = baOhNoing)) then
  begin
    Result := True;
    Transition(L, baExiting);
    CueSoundEffect(SFX_YIPPEE, L.Position);
  end;
end;

function TLemmingGame.HandleRadiation(L: TLemming; Stoning: Boolean): Boolean;
begin
  Result := False;

  if (L.LemExplosionTimer = 0) and not (L.LemAction in [baOhnoing, baStoning]) then
  begin
    L.LemExplosionTimer := 152;
    L.LemTimerToStone := Stoning;
  end;
end;

function TLemmingGame.HandleForceField(L: TLemming; Direction: Integer): Boolean;
begin
  Result := False;
  if (L.LemDx = -Direction) and not (L.LemAction = baHoisting) then
  begin
    Result := True;

    TurnAround(L);

    // Avoid moving into terrain, see http://www.lemmingsforums.net/index.php?topic=2575.0
    if L.LemAction = baMining then
    begin
      if L.LemPhysicsFrame = 2 then
        ApplyMinerMask(L, 1, 0, 0)
      else if (L.LemPhysicsFrame >= 3) and (L.LemPhysicsFrame < 15) then
        ApplyMinerMask(L, 1, -2*L.LemDx, -1);
    end
    // Required for turned builders not to walk into air
    // For platformers, see http://www.lemmingsforums.net/index.php?topic=2530.0
    else if (L.LemAction in [baBuilding, baPlatforming]) and (L.LemPhysicsFrame >= 9) then
      LayBrick(L)
    else if L.LemAction = baClimbing then
    begin
      Inc(L.LemX, L.LemDx); // Move out of the wall
      if not L.LemIsStartingAction then Inc(L.LemY); // Don't move below original position
      Transition(L, baWalking);
    end;
  end;
end;

function TLemmingGame.HandleFire(L: TLemming): Boolean;
begin
  Result := True;

  Transition(L, baVaporizing);
  CueSoundEffect(SFX_VAPORIZING, L.Position);
end;

function TLemmingGame.HandleFlipper(L: TLemming; PosX, PosY: Integer): Boolean;
var
  Inf: TInteractiveObjectInfo;
  ObjectID: Word;
begin
  Result := False;

  ObjectID := FindObjectID(PosX, PosY, trFlipper);
  // Exit if there is no Object
  if ObjectID = 65535 then Exit;

  Inf := ObjectInfos[ObjectID];
  if not (L.LemInFlipper = ObjectID) then
  begin
    L.LemInFlipper := ObjectID;
    if (Inf.CurrentFrame = 1) xor (L.LemDX < 0) then
    begin
      TurnAround(L);
      Result := True;
    end;

    Inf.CurrentFrame := 1 - Inf.CurrentFrame // swap the possible values 0 and 1
  end;
end;

function TLemmingGame.HandleWaterDrown(L: TLemming): Boolean;
const
  ActionSet = [baSwimming, baExploding, baStoneFinish, baVaporizing, baExiting, baSplatting];
begin
  Result := False;
  if not L.LemIsSwimmer then
  begin
    Result := True;

    if not (L.LemAction in ActionSet) then
    begin
      Transition(L, baDrowning);
      CueSoundEffect(SFX_DROWNING, L.Position);
    end;
  end;
end;

function TLemmingGame.HandleWaterSwim(L: TLemming): Boolean;
const
  ActionSet = [baSwimming, baClimbing, baHoisting, baOhnoing, baExploding,
                baStoning, baStoneFinish, baVaporizing, baExiting, baSplatting];
begin
  Result := True;
  if L.LemIsSwimmer and not (L.LemAction in ActionSet) then
  begin
    Transition(L, baSwimming);
    CueSoundEffect(SFX_SWIMMING, L.Position);
  end;
end;



procedure TLemmingGame.ApplyStoneLemming(L: TLemming);
var
  X: Integer;
begin
  X := L.LemX;
  if L.LemDx = 1 then Inc(X);

  StonerMask.DrawTo(PhysicsMap, X - 8, L.LemY -10);

  if not IsSimulating then // could happen as a result of slowfreeze objects!
    fRenderInterface.AddTerrainStoner(X - 8, L.LemY -10);
end;


procedure TLemmingGame.ApplyExplosionMask(L: TLemming);
var
  PosX, PosY: Integer;
begin
  PosX := L.LemX;
  if L.LemDx = 1 then Inc(PosX);
  PosY := L.LemY;

  BomberMask.DrawTo(PhysicsMap, PosX - 8, PosY - 14);

  if not IsSimulating then // could happen as a result of radiation or nuke
    fRenderInterface.RemoveTerrain(PosX - 8, PosY - 14, BomberMask.Width, BomberMask.Height);
end;

procedure TLemmingGame.ApplyBashingMask(L: TLemming; MaskFrame: Integer);
var
  S, D: TRect;
begin
  // basher mask = 16 x 10

  S := Rect(0, 0, 16, 10);

  if L.LemDx = 1 then
  begin
    BasherMasks.OnPixelCombine := CombineMaskPixelsRight;
    MoveRect(S, 16, MaskFrame * 10);
  end else begin
    BasherMasks.OnPixelCombine := CombineMaskPixelsLeft;
    MoveRect(S, 0, MaskFrame * 10);
  end;

  D.Left := L.LemX - 8;
  D.Top := L.LemY - 10;
  D.Right := D.Left + 16;
  D.Bottom := D.Top + 10;

  Assert(CheckRectCopy(D, S), 'bash rect err');

  BasherMasks.DrawTo(PhysicsMap, D, S);

  // Only change the PhysicsMap if simulating stuff
  if not IsSimulating then
    fRenderInterface.RemoveTerrain(D.Left, D.Top, D.Right - D.Left, D.Bottom - D.Top);
end;

procedure TLemmingGame.ApplyFencerMask(L: TLemming; MaskFrame: Integer);
var
  S, D: TRect;
begin
  // fencer mask = 16 x 10

  S := Rect(0, 0, 16, 10);

  if L.LemDx = 1 then
  begin
    FencerMasks.OnPixelCombine := CombineMaskPixelsUpRight;
    MoveRect(S, 16, MaskFrame * 10);
  end else begin
    FencerMasks.OnPixelCombine := CombineMaskPixelsUpLeft;
    MoveRect(S, 0, MaskFrame * 10);
  end;

  D.Left := L.LemX - 8;
  D.Top := L.LemY - 10;
  D.Right := D.Left + 16;
  D.Bottom := D.Top + 10;

  Assert(CheckRectCopy(D, S), 'fence rect err');

  FencerMasks.DrawTo(PhysicsMap, D, S);

  // Only change the PhysicsMap if simulating stuff
  if not IsSimulating then
    // Delete these pixels from the terrain layer
    fRenderInterface.RemoveTerrain(D.Left, D.Top, D.Right - D.Left, D.Bottom - D.Top);
end;

procedure TLemmingGame.ApplyMinerMask(L: TLemming; MaskFrame, AdjustX, AdjustY: Integer);
// The miner mask is usually centered at the feet of L
// AdjustX, AdjustY lets one adjust the position of the miner mask relative to this
var
  MaskX, MaskY: Integer;
  S, D: TRect;
begin
  Assert((MaskFrame >=0) and (MaskFrame <= 1), 'miner mask error');

  MaskX := L.LemX + L.LemDx - 8 + AdjustX;
  MaskY := L.LemY + MaskFrame - 12 + AdjustY;

  S := Rect(0, 0, 16, 13);

  if L.LemDx = 1 then
  begin
    MinerMasks.OnPixelCombine := CombineMaskPixelsDownRight;
    MoveRect(S, 16, MaskFrame * 13);
  end else begin
    MinerMasks.OnPixelCombine := CombineMaskPixelsDownLeft;
    MoveRect(S, 0, MaskFrame * 13);
  end;



  D.Left := MaskX;
  D.Top := MaskY;
  D.Right := MaskX + RectWidth(S);
  D.Bottom := MaskY + RectHeight(S);

  Assert(CheckRectCopy(D, S), 'miner rect error');

  MinerMasks.DrawTo(PhysicsMap, D, S);

  // Delete these pixels from the terrain layer
  if not IsSimulating then
    fRenderInterface.RemoveTerrain(D.Left, D.Top, D.Right - D.Left, D.Bottom - D.Top);
end;


procedure TLemmingGame.DrawAnimatedObjects;
var
  i, f: Integer;
  Inf : TInteractiveObjectInfo;
begin

  for i := 0 to ObjectInfos.Count-1 do
  begin
    Inf := ObjectInfos[i];
    if Inf.TriggerEffect = DOM_BACKGROUND then
    begin
      Inf.Left := Inf.Left + Inf.Movement(True, CurrentIteration); // x-movement
      Inf.Top := Inf.Top + Inf.Movement(False, CurrentIteration); // y-movement

      // Check level borders:
      // The additional "+f" are necessary! Delphi's definition of mod returns negative numbers when passing negative numbers.
      // The following code works only if the coordinates are not too negative, so Asserts are added
      f := Level.Info.Width + Inf.Width;
      Assert(Inf.Left + Inf.Width + f >= 0, 'Animation Object too far left');
      Inf.Left := ((Inf.Left + Inf.Width + f) mod f) - Inf.Width;

      f := Level.Info.Height + Inf.Height;
      Assert(Inf.Top + Inf.Height + f >= 0, 'Animation Object too far above');
      Inf.Top := ((Inf.Top + Inf.Height + f) mod f) - Inf.Height;
    end;
  end;
end;


procedure TLemmingGame.CheckForNewShadow;
var
  ShadowSkillButton: TSkillPanelButton;
  ShadowLem: TLemming;
const
  ShadowSkillSet = [spbPlatformer, spbBuilder, spbStacker, spbDigger, spbMiner,
                    spbBasher, spbFencer, spbBomber, spbGlider, spbCloner];
begin
  if fHyperSpeed then Exit;
  if GameParams.NoShadows then Exit;

  // Get correct skill to draw the shadow
  if Assigned(fLemSelected) and (fSelectedSkill in ShadowSkillSet) then
  begin
    ShadowSkillButton := fSelectedSkill;
    ShadowLem := fLemSelected;
  end
  else
  begin
    // Get next highest lemming under the cursor, even if he cannot receive the skill
    GetPriorityLemming(ShadowLem, baNone, CursorPoint);

    // Glider happens if the lemming is a glider, even when other skills are
    if Assigned(ShadowLem) and ShadowLem.LemIsGlider and (ShadowLem.LemAction in [baFalling, baGliding]) then
    begin
      ShadowSkillButton := spbGlider;
    end
    else
    begin
      ShadowSkillButton := spbNone;
      ShadowLem := nil;
    end
  end;


  // Check whether we have to redraw the Shadow (if lem or skill changed)
  if (not fExistShadow) or (not (fLemWithShadow = ShadowLem))
                        or (not (fLemWithShadowButton = ShadowSkillButton)) then
  begin
    if fExistShadow then // false if coming from UpdateLemming
    begin
      // erase existing ShadowBridge
      fRenderer.ClearShadows;
      fExistShadow := false;
    end;

    // Draw the new ShadowBridge
    try
      if not Assigned(ShadowLem) then Exit;
      if not (ShadowSkillButton in ShadowSkillSet) then Exit; // Should always be the case, but to be sure...

      // Draw the shadows
      fRenderer.DrawShadows(ShadowLem, ShadowSkillButton);

      // remember stats for lemming with shadow
      fLemWithShadow := ShadowLem;
      fLemWithShadowButton := ShadowSkillButton;
      fExistShadow := True;

    except
      // Reset existing shadows
      fLemWithShadow := nil;
      fLemWithShadowButton := fSelectedSkill;
      fExistShadow := False;
    end;
  end;
end;


procedure TLemmingGame.LayBrick(L: TLemming);
{-------------------------------------------------------------------------------
  bricks are in the lemming area so will automatically be copied to the screen
  during drawlemmings
-------------------------------------------------------------------------------}
var
  BrickPosY, n: Integer;
begin
  // Do not change the fPhysicsMap when simulating building (but do so for platformers!)
  if IsSimulating and (L.LemAction = baBuilding) then Exit;

  Assert((L.LemNumberOfBricksLeft > 0) and (L.LemNumberOfBricksLeft < 13),
            'Number bricks out of bounds');

  If L.LemAction = baBuilding then BrickPosY := L.LemY - 1
  else BrickPosY := L.LemY; // for platformers

  for n := 0 to 5 do
    AddConstructivePixel(L.LemX + n*L.LemDx, BrickPosY, BrickPixelColors[12 - L.LemNumberOfBricksLeft]);
end;

function TLemmingGame.LayStackBrick(L: TLemming): Boolean;
{-------------------------------------------------------------------------------
  bricks are in the lemming area so will automatically be copied to the screen
  during drawlemmings
-------------------------------------------------------------------------------}
var
  BrickPosY, n: Integer;
  PixPosX: Integer;
begin
  Assert((L.LemNumberOfBricksLeft > 0) and (L.LemNumberOfBricksLeft < 13),
            'Number stacker bricks out of bounds');

  BrickPosY := L.LemY - 9 + L.LemNumberOfBricksLeft;
  if L.LemStackLow then Inc(BrickPosY);

  Result := False;

  for n := 1 to 3 do
  begin
    PixPosX := L.LemX + n*L.LemDx;
    if not HasPixelAt(PixPosX, BrickPosY) then
    begin
      // Do not change the fPhysicsMap when simulating stacking
      if not IsSimulating then AddConstructivePixel(PixPosX, BrickPosY, BrickPixelColors[12 - L.LemNumberOfBricksLeft]);
      Result := true;
    end;
  end;
end;

procedure TLemmingGame.AddConstructivePixel(X, Y: Integer; Color: TColor32);
begin
  PhysicsMap.PixelS[X, Y] := PhysicsMap.PixelS[X, Y] or PM_SOLID;
  if not IsSimulating then fRenderInterface.AddTerrainBrick(X, Y, Color);
end;


function TLemmingGame.DigOneRow(PosX, PosY: Integer): Boolean;
// The central pixel of the removed row lies at (PosX, PosY)
var
  n: Integer;
begin
  Result := False;

  Assert(PosY >=0, 'Digging at negative Y-coordinate');

  For n := -4 to 4 do
  begin
    if HasPixelAt(PosX + n, PosY) and not HasIndestructibleAt(PosX + n, PosY, 0, baDigging) then // we can live with not passing a proper LemDx here
    begin
      RemovePixelAt(PosX + n, PosY);
      if (n > -4) and (n < 4) then Result := True;
    end;

    // Delete these pixels from the terrain layer
    if not IsSimulating then fRenderInterface.RemoveTerrain(PosX - 4, PosY, 9, 1);
  end;
end;

function TLemmingGame.HandleLemming(L: TLemming): Boolean;
{-------------------------------------------------------------------------------
  This is the main lemming method, called by CheckLemmings().
  The return value should return true if the lemming has to be checked by
  interactive objects.
  o Increment lemming animationframe
  o Call specialized action-method
  o Do *not* call this method for a removed lemming
-------------------------------------------------------------------------------}
const
  OneTimeActionSet = [baDrowning, baHoisting, baSplatting, baExiting,
                      baVaporizing, baShrugging, baOhnoing, baExploding,
                      baStoning];
begin
  // Remember old position and action for CheckTriggerArea
  L.LemXOld := L.LemX;
  L.LemYOld := L.LemY;
  L.LemActionOld := L.LemAction;
  // No transition to do at the end of lemming movement
  fLemNextAction := baNone;

  Inc(L.LemFrame);
  Inc(L.LemPhysicsFrame);

  if L.LemPhysicsFrame > L.LemMaxPhysicsFrame then
  begin
    L.LemPhysicsFrame := 0;
    // Floater and Glider start cycle at frame 9!
    if L.LemAction in [baFloating, baGliding] then L.LemPhysicsFrame := 9;
    if L.LemAction in OneTimeActionSet then L.LemEndOfAnimation := True;
  end;

  // Do Lem action
  Result := LemmingMethods[L.LemAction](L);

  if L.LemIsZombie and not IsSimulating then SetZombieField(L);
end;

function TLemmingGame.CheckLevelBoundaries(L: TLemming) : Boolean;
// Check for both sides and the bottom
begin
  Result := True;
  // Bottom
  if L.LemY > LEMMING_MAX_Y + PhysicsMap.Height then
  begin
    RemoveLemming(L, RM_NEUTRAL);
    Result := False;
  end;
  // Sides
  if (L.LemX < 0) or (L.LemX >= PhysicsMap.Width) then
  begin
    RemoveLemming(L, RM_NEUTRAL);
    Result := False;
  end;
end;


function TLemmingGame.HandleWalking(L: TLemming): Boolean;
var
  LemDy: Integer;
begin
  Result := True;

  Inc(L.LemX, L.LemDx);
  LemDy := FindGroundPixel(L.LemX, L.LemY);

  if (LemDy < -6) then
  begin
    if L.LemIsClimber then
      Transition(L, baClimbing)
    else
    begin
      TurnAround(L);
      Inc(L.LemX, L.LemDx);
    end;
  end
  else if (LemDy < -2) then
  begin
    Transition(L, baJumping);
    Inc(L.LemY, -2);
  end
  else if (LemDy < 1) then
    Inc(L.LemY, LemDy);

  // Get new ground pixel again in case the Lem has turned
  LemDy := FindGroundPixel(L.LemX, L.LemY);

  if (LemDy > 3) then
  begin
    Inc(L.LemY, 4);
    Transition(L, baFalling);
  end
  else if (LemDy > 0) then
    Inc(L.LemY, LemDy);
end;



function TLemmingGame.HandleSwimming(L: TLemming): Boolean;
var
  LemDy: Integer;

  function LemDive(L: TLemming): Integer;
    // Returns 0 if the lem may not dive down
    // Otherwise return the amount of pixels the lem dives
  var
    DiveDepth: Integer;
  begin
    if L.LemIsClimber then DiveDepth := 3
    else DiveDepth := 4;

    Result := 1;
    while HasPixelAt(L.LemX, L.LemY + Result) and (Result <= DiveDepth) do
    begin
      Inc(Result);
      if L.LemY + Result >= PhysicsMap.Height then Result := DiveDepth + 1; // End while loop!
    end;

    // do not dive, when there is no more water
    if not HasTriggerAt(L.LemX, L.LemY + Result, trWater) then Result := 0;

    if Result > DiveDepth then Result := 0; // too much terrain to dive
  end;

begin
  Result := True;

  Inc(L.LemX, L.LemDx);

  if HasTriggerAt(L.LemX, L.LemY, trWater) or HasPixelAt(L.LemX, L.LemY) then
  begin
    LemDy := FindGroundPixel(L.LemX, L.LemY);

    // Rise if there is water above the lemming
    if (LemDy >= -1) and HasTriggerAt(L.LemX, L.LemY -1, trWater)
                     and not HasPixelAt(L.LemX, L.LemY - 1) then
      Dec(L.LemY)

    else if LemDy < -6 then
    begin
      if LemDive(L) > 0 then
        Inc(L.LemY, LemDive(L)) // Dive below the terrain
      // Only transition to climber, if the lemming is not under water
      else if L.LemIsClimber and not HasTriggerAt(L.LemX, L.LemY - 1, trWater) then
        Transition(L, baClimbing)
      else
      begin
        TurnAround(L);
        Inc(L.LemX, L.LemDx); // Move lemming back
      end
    end

    else if LemDy <= -3 then
    begin
      Transition(L, baJumping);
      Dec(L.LemY, 2);
    end

    else if LemDy <= -1 then
    begin
      Transition(L, baWalking);
      Inc(L.LemY, LemDy);
    end;
  end

  else // if no water or terrain on current position
  begin
    LemDy := FindGroundPixel(L.LemX, L.LemY);
    If LemDy > 1 then
    begin
      Inc(L.LemY);
      Transition(L, baFalling);
    end
    else // if LemDy = 0 or 1
    begin
      Inc(L.LemY, LemDy);
      Transition(L, baWalking);
    end;
  end;

end;



function TLemmingGame.HandleJumping(L: TLemming): Boolean;
var
  dy: Integer;
begin
  Result := True;
  with L do
  begin
    dy := 0;
    while (dy < 2) and (LemJumped < 5) and (HasPixelAt(LemX, LemY-1)) do
    begin
      Inc(dy);
      Dec(LemY);
      Inc(LemJumped);
    end;

    if (dy < 2) and not HasPixelAt(LemX, LemY-1) then
    begin
      fLemNextAction := baWalking;
    end else if ((LemJumped = 4) and HasPixelAt(LemX, LemY-1) and HasPixelAt(LemX, LemY-2)) or ((LemJumped >= 5) and HasPixelAt(LemX, LemY-1)) then
    begin
      Dec(LemX, LemDx);
      Transition(L, baFalling, true); // turn around as well
    end;
  end;
end;

function TLemmingGame.HandleDigging(L: TLemming): Boolean;
var
  ContinueWork: Boolean;
begin
  Result := True;

  if L.LemIsStartingAction then
  begin
    L.LemIsStartingAction := False;
    DigOneRow(L.LemX, L.LemY - 1);
    // The first digger cycle is one frame longer!
    // So we need to artificially cancel the very first frame advancement.
    Dec(L.LemPhysicsFrame);
  end;

  if L.LemPhysicsFrame in [0, 8] then
  begin
    Inc(L.LemY);

    ContinueWork := DigOneRow(L.LemX, L.LemY - 1);

    if HasIndestructibleAt(L.LemX, L.LemY, L.LemDX, baDigging) then
    begin
      if HasSteelAt(L.LemX, L.LemY) then
        CueSoundEffect(SFX_HITS_STEEL, L.Position);
      Transition(L, baWalking);
    end

    else if not ContinueWork then
      Transition(L, baFalling);
  end;
end;


function TLemmingGame.HandleClimbing(L: TLemming): Boolean;
// Be very careful when changing the terrain/hoister checks for climbers!
// See http://www.lemmingsforums.net/index.php?topic=2506.0 first!
var
  FoundClip: Boolean;
begin
  Result := True;

  if L.LemPhysicsFrame <= 3 then
  begin
    FoundClip := (HasPixelAt(L.LemX - L.LemDx, L.LemY - 6 - L.LemPhysicsFrame))
              or (HasPixelAt(L.LemX - L.LemDx, L.LemY - 5 - L.LemPhysicsFrame) and (not L.LemIsStartingAction));

    if L.LemPhysicsFrame = 0 then // first triggered after 8 frames!
      FoundClip := FoundClip and HasPixelAt(L.LemX - L.LemDx, L.LemY - 7);

    if FoundClip then
    begin
      // Don't fall below original position on hitting terrain in first cycle
      if not L.LemIsStartingAction then L.LemY := L.LemY - L.LemPhysicsFrame + 3;
      Dec(L.LemX, L.LemDx);
      Transition(L, baFalling, True); // turn around as well
    end
    else if not HasPixelAt(L.LemX, L.LemY - 7 - L.LemPhysicsFrame) then
    begin
      // if-case prevents too deep bombing, see http://www.lemmingsforums.net/index.php?topic=2620.0
      if not (L.LemIsStartingAction and (L.LemPhysicsFrame = 1)) then
      begin
        L.LemY := L.LemY - L.LemPhysicsFrame + 2;
        L.LemIsStartingAction := False;
      end;
      Transition(L, baHoisting);
    end;
  end

  else
  begin
    Dec(L.LemY);
    L.LemIsStartingAction := False;

    FoundClip := HasPixelAt(L.LemX - L.LemDx, L.LemY - 7);

    if L.LemPhysicsFrame = 7 then
      FoundClip := FoundClip and HasPixelAt(L.LemX, L.LemY - 7);

    if FoundClip then
    begin
      Inc(L.LemY);
      Dec(L.LemX, L.LemDx);
      Transition(L, baFalling, True); // turn around as well
    end;
  end;
end;


function TLemmingGame.HandleDrowning(L: TLemming): Boolean;
begin
  Result := False;
  if L.LemEndOfAnimation then RemoveLemming(L, RM_KILL);
end;


function TLemmingGame.HandleFixing(L: TLemming): Boolean;
begin
  Result := False;
  Dec(L.LemMechanicFrames);
  if L.LemMechanicFrames <= 0 then
  begin
    if L.LemActionNew <> baNone then Transition(L, L.LemActionNew)
    else Transition(L, baWalking);
    L.LemActionNew := baNone;
  end
  else if L.LemPhysicsFrame mod 8 = 0 then
    CueSoundEffect(SFX_FIXING, L.Position);
end;


function TLemmingGame.HandleHoisting(L: TLemming): Boolean;
begin
  Result := True;
  if L.LemEndOfAnimation then
    Transition(L, baWalking)
  // special case due to http://www.lemmingsforums.net/index.php?topic=2620.0
  else if (L.LemPhysicsFrame = 1) and L.LemIsStartingAction then
    Dec(L.LemY, 1)
  else if L.LemPhysicsFrame <= 4 then
    Dec(L.LemY, 2);
end;


function TLemmingGame.LemCanPlatform(L: TLemming): Boolean;
var
  n: Integer;
begin
  Result := False;
  // Next brick must add at least one pixel
  for n := 0 to 5 do
    Result := Result or not HasPixelAt(L.LemX + n*L.LemDx, L.LemY);

  // Lem may not hit terrain where the previous brick was already placed.
  Result := Result and not HasPixelAt(L.LemX + L.LemDx, L.LemY - 1);
  Result := Result and not HasPixelAt(L.LemX + 2*L.LemDx, L.LemY - 1);
end;


function TLemmingGame.HandlePlatforming(L: TLemming): Boolean;
  function PlatformerTerrainCheck(X, Y: Integer): Boolean;
  begin
    Result := HasPixelAt(X, Y - 1) or HasPixelAt(X, Y - 2);
  end;

begin
  Result := True;

  if L.LemPhysicsFrame = 9 then
  begin
    L.LemPlacedBrick := LemCanPlatform(L);
    LayBrick(L);
  end

  else if (L.LemPhysicsFrame = 10) and (L.LemNumberOfBricksLeft <= 3) then
    CueSoundEffect(SFX_BUILDER_WARNING, L.Position)

  else if L.LemPhysicsFrame = 15 then
  begin
    if not L.LemPlacedBrick then
      Transition(L, baWalking, True) // turn around as well

    else if PlatformerTerrainCheck(L.LemX + 2*L.LemDx, L.LemY) then
    begin
      Inc(L.LemX, L.LemDx);
      Transition(L, baWalking, True);  // turn around as well
    end

    else
      Inc(L.LemX, L.LemDx);
  end

  else if L.LemPhysicsFrame = 0 then
  begin
    if PlatformerTerrainCheck(L.LemX + 2*L.LemDx, L.LemY) and (L.LemNumberOfBricksLeft > 1) then
    begin
      Inc(L.LemX, L.LemDx);
      Transition(L, baWalking, True);  // turn around as well
    end

    else if PlatformerTerrainCheck(L.LemX + 3*L.LemDx, L.LemY) and (L.LemNumberOfBricksLeft > 1) then
    begin
      Inc(L.LemX, 2*L.LemDx);
      Transition(L, baWalking, True);  // turn around as well
    end

    else
    begin
      Inc(L.LemX, 2*L.LemDx);
      Dec(L.LemNumberOfBricksLeft); // Why are we doing this here, instead at the beginning of frame 15??
      if L.LemNumberOfBricksLeft = 0 then
      begin
        // stalling if there are pixels in the way:
        if HasPixelAt(L.LemX, L.LemY - 1) then Dec(L.LemX, L.LemDx);
        Transition(L, baShrugging);
      end;
    end;
  end
end;



function TLemmingGame.HandleBuilding(L: TLemming): Boolean;
begin
  Result := True;

  if L.LemPhysicsFrame = 9 then
    LayBrick(L)

  else if (L.LemPhysicsFrame = 10) and (L.LemNumberOfBricksLeft <= 3) then
    CueSoundEffect(SFX_BUILDER_WARNING, L.Position)

  else if L.LemPhysicsFrame = 0 then
  begin
    Dec(L.LemNumberOfBricksLeft);

    if HasPixelAt(L.LemX + L.LemDx, L.LemY - 2) then
      Transition(L, baWalking, True)  // turn around as well

    else if (     HasPixelAt(L.LemX + L.LemDx, L.LemY - 3)
              or  HasPixelAt(L.LemX + 2*L.LemDx, L.LemY - 2)
              or (HasPixelAt(L.LemX + 2*L.LemDx, L.LemY - 10) and (L.LemNumberOfBricksLeft > 0))
            ) then
    begin
      Dec(L.LemY);
      Inc(L.LemX, L.LemDx);
      Transition(L, baWalking, True)  // turn around as well
    end

    else
    begin
      Dec(L.LemY);
      Inc(L.LemX, 2*L.LemDx);

      if (     HasPixelAt(L.LemX, L.LemY - 2)
           or  HasPixelAt(L.LemX, L.LemY - 3)
           or  HasPixelAt(L.LemX + L.LemDx, L.LemY - 3)
           or (HasPixelAt(L.LemX + L.LemDx, L.LemY - 9) and (L.LemNumberOfBricksLeft > 0))
         ) then
         Transition(L, baWalking, True)  // turn around as well

      else if L.LemNumberOfBricksLeft = 0 then
         Transition(L, baShrugging);
    end;
  end;
end;


function TLemmingGame.HandleStacking(L: TLemming): Boolean;
  function MayPlaceNextBrick(L: TLemming): Boolean;
  var
    BrickPosY: Integer;
  begin
    BrickPosY := L.LemY - 9 + L.LemNumberOfBricksLeft;
    if L.LemStackLow then Inc(BrickPosY);
    Result := not (     HasPixelAt(L.LemX + L.LemDX, BrickPosY)
                    and HasPixelAt(L.LemX + 2 * L.LemDX, BrickPosY)
                    and HasPixelAt(L.LemX + 3 * L.LemDX, BrickPosY))
  end;

begin
  Result := True;

  if L.LemPhysicsFrame = 7 then
    L.LemPlacedBrick := LayStackBrick(L)

  else if L.LemPhysicsFrame = 0 then
  begin
    Dec(L.LemNumberOfBricksLeft);

    // sound on last three bricks
    if L.LemNumberOfBricksLeft < 3 then CueSoundEffect(SFX_BUILDER_WARNING, L.Position);

    if not L.LemPlacedBrick then
    begin
      // Relax the check on the first brick
      // for details see http://www.lemmingsforums.net/index.php?topic=2862.0
      if (L.LemNumberOfBricksLeft < 7) or not MayPlaceNextBrick(L) then
        Transition(L, baWalking, True) // turn around as well
    end
    else if L.LemNumberOfBricksLeft = 0 then
      Transition(L, baShrugging);
  end;
end;



function TLemmingGame.HandleBashing(L: TLemming): Boolean;
var
  LemDy, n: Integer;
  ContinueWork: Boolean;

  function BasherIndestructibleCheck(x, y, Direction: Integer): Boolean;
  begin
    // check for indestructible terrain 3, 4 and 5 pixels above (x, y)
    Result := (    (HasIndestructibleAt(x, y - 3, Direction, baBashing))
                or (HasIndestructibleAt(x, y - 4, Direction, baBashing))
                or (HasIndestructibleAt(x, y - 5, Direction, baBashing))
              );
  end;

  procedure BasherTurn(L: TLemming; SteelSound: Boolean);
  begin
    // Turns basher around an transitions to walker
    Dec(L.LemX, L.LemDx);
    Transition(L, baWalking, True); // turn around as well
    if SteelSound then CueSoundEffect(SFX_HITS_STEEL, L.Position);
  end;

  function BasherStepUpCheck(x, y, Direction, Step: Integer): Boolean;
  begin
    Result := True;

    if Step = -1 then
    begin
      if (     (not HasPixelAt(x + Direction, y + Step - 1))
           and HasPixelAt(x + Direction, y + Step)
           and HasPixelAt(x + 2*Direction, y + Step)
           and HasPixelAt(x + 2*Direction, y + Step - 1)
           and HasPixelAt(x + 2*Direction, y + Step - 2)
         ) then Result := False;
      if (     (not HasPixelAt(x + Direction, y + Step - 2))
           and HasPixelAt(x + Direction, y + Step)
           and HasPixelAt(x + Direction, y + Step - 1)
           and HasPixelAt(x + 2*Direction, y + Step - 1)
           and HasPixelAt(x + 2*Direction, y + Step - 2)
         ) then Result := False;
      if (     HasPixelAt(x + Direction, y + Step - 2)
           and HasPixelAt(x + Direction, y + Step - 1)
           and HasPixelAt(x + Direction, y + Step)
         ) then Result := False;
    end
    else if Step = -2 then
    begin
      if (     (not HasPixelAt(x + Direction, y + Step))
           and HasPixelAt(x + Direction, y + Step + 1)
           and HasPixelAt(x + 2*Direction, y + Step + 1)
           and HasPixelAt(x + 2*Direction, y + Step)
           and HasPixelAt(x + 2*Direction, y + Step - 1)
         ) then Result := False;
      if (     (not HasPixelAt(x + Direction, y + Step - 1))
           and HasPixelAt(x + Direction, y + Step)
           and HasPixelAt(x + 2*Direction, y + Step)
           and HasPixelAt(x + 2*Direction, y + Step - 1)
         ) then Result := False;
      if (     HasPixelAt(x + Direction, y + Step - 1)
           and HasPixelAt(x + Direction, y + Step)
         ) then Result := False;
    end;
  end;

  // Simulate the behavior of the basher in the next two frames
  function DoTurnAtSteel(L: TLemming): Boolean;
  var
    CopyL: TLemming;
    SavePhysicsMap: TBitmap32;
    i: Integer;
  begin
    // Make deep copy of the lemming
    CopyL := TLemming.Create;
    CopyL.Assign(L);
    CopyL.LemIsPhysicsSimulation := true;

    // Make a deep copy of the PhysicsMap
    SavePhysicsMap := TBitmap32.Create;
    SavePhysicsMap.Assign(PhysicsMap);

    Result := False;

    // Simulate two basher cycles
    // 11 iterations is hopefully correct: CopyL.LemPhysicsFrame changes as follows:
    // 10 -> 11 -> 12 -> 13 -> 14 -> 15 -> 16 -> 11 -> 12 -> 13 -> 14 -> 15
    CopyL.LemPhysicsFrame := 10;
    for i := 0 to 10 do
    begin
      // On CopyL.LemPhysicsFrame = 0 or 16, apply all basher masks and jump to frame 10 again
      if (CopyL.LemPhysicsFrame in [0, 16]) then
      begin
        Inc(fSimulationDepth);
        ApplyBashingMask(CopyL, 0);
        ApplyBashingMask(CopyL, 1);
        ApplyBashingMask(CopyL, 2);
        ApplyBashingMask(CopyL, 3);
        Dec(fSimulationDepth); // should not matter, because we do this in SimulateLem anyway, but to be safe...
        // Do *not* check whether continue bashing, but move directly ahead to frame 10
        CopyL.LemPhysicsFrame := 10;
      end;

      // Move one frame forward
      SimulateLem(CopyL, False);

      // Check if we have turned around at steel
      if (CopyL.LemDX = -L.LemDX) then
      begin
        Result := True;
        Break;
      end
      // Check if we are still a basher
      else if CopyL.LemRemoved or not (CopyL.LemAction = baBashing) then
        Break; // and return false
    end;

    // Copy PhysicsMap back
    PhysicsMap.Assign(SavePhysicsMap);
    SavePhysicsMap.Free;

    // Free CopyL
    CopyL.Free;
  end;


begin
  Result := True;

  // Remove terrain
  if L.LemPhysicsFrame in [2, 3, 4, 5] then
    ApplyBashingMask(L, L.LemPhysicsFrame - 2);

  // Check for enough terrain to continue working
  if L.LemPhysicsFrame = 5 then
  begin
    ContinueWork := False;

    // check for destructible terrain at height 5 and 6
    for n := 1 to 14 do
    begin
      if (     HasPixelAt(L.LemX + n*L.LemDx, L.LemY - 6)
           and not HasIndestructibleAt(L.LemX + n*L.LemDx, L.LemY - 6, L.LemDx, baBashing)
         ) then ContinueWork := True;
      if (     HasPixelAt(L.LemX + n*L.LemDx, L.LemY - 5)
           and not HasIndestructibleAt(L.LemX + n*L.LemDx, L.LemY - 5, L.LemDx, baBashing)
         ) then ContinueWork := True;
    end;

    // check whether we turn around within the next two basher strokes (only if we don't simulate)
    if (not ContinueWork) and (not L.LemIsPhysicsSimulation) then
      ContinueWork := DoTurnAtSteel(L);

    if not ContinueWork then
    begin
      if HasPixelAt(L.LemX, L.LemY) then
        Transition(L, baWalking)
      else
        Transition(L, baFalling);
    end;
  end;

  // Basher movement
  if L.LemPhysicsFrame in [11, 12, 13, 14, 15] then
  begin
    Inc(L.LemX, L.LemDx);

    LemDy := FindGroundPixel(L.LemX, L.LemY);

    If LemDy = 4 then
    begin
      Inc(L.LemY, LemDy);
      Transition(L, baFalling);
    end

    else if LemDy = 3 then
    begin
      Inc(L.LemY, LemDy);
      Transition(L, baWalking);
    end

    else if LemDy in [0, 1, 2] then
    begin
      // Move no, one or two pixels down, if there no steel
      if BasherIndestructibleCheck(L.LemX, L.LemY + LemDy, L.LemDx) then
        BasherTurn(L, HasSteelAt(L.LemX, L.LemY + LemDy - 4))
      else
        Inc(L.LemY, LemDy)
    end

    else if (LemDy = -1) or (LemDy = -2) then
    begin
      // Move one or two pixels up, if there is no steel and not too much terrain
      if BasherIndestructibleCheck(L.LemX, L.LemY + LemDy, L.LemDx) then
        BasherTurn(L, HasSteelAt(L.LemX, L.LemY + LemDy - 4))
      else if BasherStepUpCheck(L.LemX, L.LemY, L.LemDx, LemDy) = False then
      begin
        if BasherIndestructibleCheck(L.LemX + L.LemDx, L.LemY + 2, L.LemDx) then
          BasherTurn(L,    HasSteelAt(L.LemX + L.LemDx, L.LemY + LemDy)
                        or HasSteelAt(L.LemX + L.LemDx, L.LemY + LemDy + 1))
        else
          //stall basher
          Dec(L.LemX, L.LemDx);
      end
      else
        Inc(L.LemY, LemDy); // Lem may move up
    end

    else if LemDy < -2 then
    begin
      // Either stall or turn if there is steel
      if BasherIndestructibleCheck(L.LemX, L.LemY, L.LemDx) then
        BasherTurn(L,(    HasSteelAt(L.LemX, L.LemY - 3)
                       or HasSteelAt(L.LemX, L.LemY - 4)
                       or HasSteelAt(L.LemX, L.LemY - 5)
                     ))
      else
        Dec(L.LemX, L.LemDx);
    end;
  end;
end;

function TLemmingGame.HandleFencing(L: TLemming): Boolean;
// This is based off HandleBashing but has some changes.
var
  LemDy, n: Integer;
  ContinueWork: Boolean;
  SteelContinue, MoveUpContinue: Boolean;
  NeedUndoMoveUp: Boolean;

  function FencerIndestructibleCheck(x, y, Direction: Integer): Boolean;
  begin
    // check for indestructible terrain 3, 4 and 5 pixels above (x, y)
    Result := (    (HasIndestructibleAt(x, y - 3, Direction, baFencing))
//                or (HasIndestructibleAt(x, y - 4, Direction, baFencing))
//                or (HasIndestructibleAt(x, y - 5, Direction, baFencing))
              );
  end;

  procedure FencerTurn(L: TLemming; SteelSound: Boolean);
  begin
    // Turns basher around an transitions to walker
    Dec(L.LemX, L.LemDx);
    if NeedUndoMoveUp then
      Inc(L.LemY);
    Transition(L, baWalking, True); // turn around as well
    if SteelSound then CueSoundEffect(SFX_HITS_STEEL, L.Position);
  end;

  function FencerStepUpCheck(x, y, Direction, Step: Integer): Boolean;
  begin
    Result := True;

    if Step = -1 then
    begin
      if (     (not HasPixelAt(x + Direction, y + Step - 1))
           and HasPixelAt(x + Direction, y + Step)
           and HasPixelAt(x + 2*Direction, y + Step)
           and HasPixelAt(x + 2*Direction, y + Step - 1)
           and HasPixelAt(x + 2*Direction, y + Step - 2)
         ) then Result := False;
      if (     (not HasPixelAt(x + Direction, y + Step - 2))
           and HasPixelAt(x + Direction, y + Step)
           and HasPixelAt(x + Direction, y + Step - 1)
           and HasPixelAt(x + 2*Direction, y + Step - 1)
           and HasPixelAt(x + 2*Direction, y + Step - 2)
         ) then Result := False;
      if (     HasPixelAt(x + Direction, y + Step - 2)
           and HasPixelAt(x + Direction, y + Step - 1)
           and HasPixelAt(x + Direction, y + Step)
         ) then Result := False;
    end
    else if Step = -2 then
    begin
      if (     (not HasPixelAt(x + Direction, y + Step))
           and HasPixelAt(x + Direction, y + Step + 1)
           and HasPixelAt(x + 2*Direction, y + Step + 1)
           and HasPixelAt(x + 2*Direction, y + Step)
           and HasPixelAt(x + 2*Direction, y + Step - 1)
         ) then Result := False;
      if (     (not HasPixelAt(x + Direction, y + Step - 1))
           and HasPixelAt(x + Direction, y + Step)
           and HasPixelAt(x + 2*Direction, y + Step)
           and HasPixelAt(x + 2*Direction, y + Step - 1)
         ) then Result := False;
      if (     HasPixelAt(x + Direction, y + Step - 1)
           and HasPixelAt(x + Direction, y + Step)
         ) then Result := False;
    end;
  end;

  // Simulate the behavior of the fencer in the next two frames
  procedure DoFencerContinueTests(L: TLemming; var SteelContinue: Boolean; var MoveUpContinue: Boolean);
  var
    CopyL: TLemming;
    SavePhysicsMap: TBitmap32;
    i: Integer;
  begin
    // Make deep copy of the lemming
    CopyL := TLemming.Create;
    CopyL.Assign(L);
    CopyL.LemIsPhysicsSimulation := true;

    // Make a deep copy of the PhysicsMap
    SavePhysicsMap := TBitmap32.Create;
    SavePhysicsMap.Assign(PhysicsMap);

    SteelContinue := False;
    MoveUpContinue := False;

    // Simulate two fencer cycles
    // 11 iterations is hopefully correct: CopyL.LemPhysicsFrame changes as follows:
    // 10 -> 11 -> 12 -> 13 -> 14 -> 15 -> 16 -> 11 -> 12 -> 13 -> 14 -> 15
    CopyL.LemPhysicsFrame := 10;

    for i := 0 to 10 do
    begin
      // On CopyL.LemPhysicsFrame = 0, apply all fencer masks and jump to frame 10 again
      if (CopyL.LemPhysicsFrame = 0) then
      begin
        Inc(fSimulationDepth); // do not apply the changes to the TerrainLayer
        ApplyFencerMask(CopyL, 0);
        ApplyFencerMask(CopyL, 1);
        ApplyFencerMask(CopyL, 2);
        ApplyFencerMask(CopyL, 3);
        Dec(fSimulationDepth); // should not matter, because we do this in SimulateLem anyway, but to be safe...
        // Do *not* check whether continue fencing, but move directly ahead to frame 10
        CopyL.LemPhysicsFrame := 10;
      end;

      // Move one frame forward
      SimulateLem(CopyL, False);

      // Check if we've moved upwards
      if (CopyL.LemY < L.LemY) then
        MoveUpContinue := true;

      // Check if we have turned around at steel
      if (CopyL.LemDX = -L.LemDX) then
      begin
        SteelContinue := True;
        Break;
      end

      // Check if we are still a fencer
      else if CopyL.LemRemoved or not (CopyL.LemAction = baFencing) then
        Break; // and return false
    end;

    // Copy PhysicsMap back
    PhysicsMap.Assign(SavePhysicsMap);
    SavePhysicsMap.Free;

    // Free the copy lemming! This was missing in Nepster's code.
    CopyL.Free;
  end;

begin
  Result := True;

  // Remove terrain
  if L.LemPhysicsFrame in [2, 3, 4, 5] then
    ApplyFencerMask(L, L.LemPhysicsFrame - 2);

  if L.LemPhysicsFrame = 15 then
    L.LemIsStartingAction := false;

  // Check for enough terrain to continue working
  if L.LemPhysicsFrame = 5 then
  begin
    ContinueWork := False;

    // check for destructible terrain at height 5 and 6
    for n := 1 to 14 do
    begin
      if (     HasPixelAt(L.LemX + n*L.LemDx, L.LemY - 6)
           and not HasIndestructibleAt(L.LemX + n*L.LemDx, L.LemY - 6, L.LemDx, baFencing)
         ) then ContinueWork := True;
      if (     HasPixelAt(L.LemX + n*L.LemDx, L.LemY - 5)
           and not HasIndestructibleAt(L.LemX + n*L.LemDx, L.LemY - 5, L.LemDx, baFencing)
         ) then ContinueWork := True;
    end;

    // check whether we turn around within the next two fencer strokes (only if we don't simulate)
    if not L.LemIsPhysicsSimulation then
      if not (ContinueWork and L.LemIsStartingAction) then // if BOTH of these are true, then both things being tested for are irrelevant
      begin
        DoFencerContinueTests(L, SteelContinue, MoveUpContinue);

        if not ContinueWork then
          ContinueWork := SteelContinue;

        if ContinueWork and not L.LemIsStartingAction then
          ContinueWork := MoveUpContinue;
      end;

    if not ContinueWork then
    begin
      if HasPixelAt(L.LemX, L.LemY) then
        Transition(L, baWalking)
      else
        Transition(L, baFalling);
    end;
  end;

  // Fencer movement
  if L.LemPhysicsFrame in [11, 12, 13, 14] then
  begin
    Inc(L.LemX, L.LemDx);

    LemDy := FindGroundPixel(L.LemX, L.LemY);

    if (LemDy = -1) and (L.LemPhysicsFrame in [11, 13]) then
    begin
      Dec(L.LemY, 1);
      LemDy := 0;
      NeedUndoMoveUp := true;
      // This is to ignore the effect of the fencer's own slope on determining how far it can step up or down.
      // I'm starting to think I should've based the Fencer code off Miner rather than Basher...
    end else
      NeedUndoMoveUp := false;

    If LemDy = 4 then
    begin
      Inc(L.LemY, LemDy);
      Transition(L, baFalling);
    end

    else if LemDy > 0 then
    begin
      Inc(L.LemY, LemDy);
      Transition(L, baWalking);
    end

    else if LemDy = 0 then
    begin
      // Move no, one or two pixels down, if there no steel
      if FencerIndestructibleCheck(L.LemX, L.LemY + LemDy, L.LemDx) then
        FencerTurn(L, HasSteelAt(L.LemX, L.LemY + LemDy - 4))
      else
        Inc(L.LemY, LemDy);
    end

    else if (LemDy = -1) or (LemDy = -2) then
    begin
      // Move one or two pixels up, if there is no steel and not too much terrain
      if FencerIndestructibleCheck(L.LemX, L.LemY + LemDy, L.LemDx) then
        FencerTurn(L, HasSteelAt(L.LemX, L.LemY + LemDy - 4))
      else if FencerStepUpCheck(L.LemX, L.LemY, L.LemDx, LemDy) = False then
      begin
        if FencerIndestructibleCheck(L.LemX + L.LemDx, L.LemY + 2, L.LemDx) then
          FencerTurn(L,    HasSteelAt(L.LemX + L.LemDx, L.LemY + LemDy)
                        or HasSteelAt(L.LemX + L.LemDx, L.LemY + LemDy + 1))
        else begin
          //stall fencer
          Dec(L.LemX, L.LemDx);
          if NeedUndoMoveUp then Inc(L.LemY);
        end;
      end
      else
        Inc(L.LemY, LemDy); // Lem may move up
    end

    else if LemDy < -2 then
    begin
      // Either stall or turn if there is steel
      if FencerIndestructibleCheck(L.LemX, L.LemY, L.LemDx) then
        FencerTurn(L,(    HasSteelAt(L.LemX, L.LemY - 3)
                       or HasSteelAt(L.LemX, L.LemY - 4)
                       or HasSteelAt(L.LemX, L.LemY - 5)
                     ))
      else
        Dec(L.LemX, L.LemDx);
    end;
  end;
end;

function TLemmingGame.FindGroundPixel(x, y: Integer): Integer;
begin
  // Find the new ground pixel
  // If Result = 4, then at least 4 pixels are air below (X, Y)
  // If Result = -7, then at least 7 pixels are terrain above (X, Y)
  Result := 0;
  if HasPixelAt(x, y) then
  begin
    while HasPixelAt(x, y + Result - 1) and (Result > -7) do
      Dec(Result);
  end
  else
  begin
    Inc(Result);
    while (not HasPixelAt(x, y + Result)) and (Result < 4) do
      Inc(Result);
  end;
end;


function TLemmingGame.HasIndestructibleAt(x, y, Direction: Integer;
                                          Skill: TBasicLemmingAction): Boolean;
begin
  // check for indestructible terrain at position (x, y), depending on skill.
  Result := (    ( PhysicsMap.PixelS[X, Y] and PM_STEEL <> 0)
              or ((PhysicsMap.PixelS[X, Y] and PM_ONEWAYUP <> 0) and (Skill in [baBashing, baMining, baDigging]))
              or ((PhysicsMap.PixelS[X, Y] and PM_ONEWAYDOWN <> 0) and (Skill in [baBashing, baFencing]))
              or ((PhysicsMap.PixelS[X, Y] and PM_ONEWAYLEFT <> 0) and (Direction = 1) and (Skill in [baBashing, baFencing, baMining]))
              or ((PhysicsMap.PixelS[X, Y] and PM_ONEWAYRIGHT <> 0) and (Direction = -1) and (Skill in [baBashing, baFencing, baMining]))
              or ((Y < -1) and (Skill = baFencing))
            );
end;

function TLemmingGame.HasSteelAt(X, Y: Integer): Boolean;
begin
  Result := (PhysicsMap.PixelS[X, Y] and PM_STEEL <> 0);
end;



function TLemmingGame.HandleMining(L: TLemming): Boolean;
  procedure MinerTurn(L: TLemming; X, Y: Integer);
  begin
    if HasSteelAt(X, Y) then CueSoundEffect(SFX_HITS_STEEL, L.Position);
    // Independently of (X, Y) this check is always made at Lem position
    // No longer check at Lem position, due to http://www.lemmingsforums.net/index.php?topic=2547.0
    if HasPixelAt(L.LemX, L.LemY-1) then Dec(L.LemY);
    Transition(L, baWalking, True);  // turn around as well
  end;

begin
  Result := True;

  if L.LemPhysicsFrame in [1, 2] then
    ApplyMinerMask(L, L.LemPhysicsFrame - 1, 0, 0)

  else if L.LemPhysicsFrame in [3, 15] then
  begin
    Inc(L.LemX, 2*L.LemDx);
    Inc(L.LemY);

    // Note that all if-checks are relative to the end position!

    // Lem cannot go down, so turn; see http://www.lemmingsforums.net/index.php?topic=2547.0
    if     HasIndestructibleAt(L.LemX - L.LemDx, L.LemY - 1, L.LemDx, baMining)
       and HasIndestructibleAt(L.LemX, L.LemY - 1, L.LemDx, baMining) then
    begin
      Dec(L.LemX, 2*L.LemDx);
      MinerTurn(L, L.LemX + 2*L.LemDx, L.LemY - 1);
    end

    // This first check is only relevant during the very first cycle.
    // Otherwise the pixel was already checked in frame 15 of the previous cycle
    else if (L.LemPhysicsFrame = 3) and HasIndestructibleAt(L.LemX - L.LemDx, L.LemY - 2, L.LemDx, baMining) then
    begin
      Dec(L.LemX, 2*L.LemDx);
      MinerTurn(L, L.LemX + L.LemDx, L.LemY - 2);
    end

    // Do we really want the to check the second HasPixel during frame 3 ????
    else if     not HasPixelAt(L.LemX - L.LemDx, L.LemY - 1) // Lem can walk over it
            and not HasPixelAt(L.LemX - L.LemDx, L.LemY)
            and not HasPixelAt(L.LemX - L.LemDx, L.LemY + 1) then
    begin
      Dec(L.LemX, L.LemDx);
      Inc(L.LemY);
      Transition(L, baFalling);
      L.LemFallen := L.LemFallen + 1;
    end

    else if HasIndestructibleAt(L.LemX, L.LemY - 2, L.LemDx, baMining) then
    begin
      Dec(L.LemX, L.LemDx);
      MinerTurn(L, L.LemX + L.LemDx, L.LemY - 2);
    end

    else if not HasPixelAt(L.LemX, L.LemY) then
    begin
      Inc(L.LemY);
      Transition(L, baFalling);
    end

    else if HasIndestructibleAt(L.LemX + L.LemDx, L.LemY - 2, L.LemDx, baMining) then
      MinerTurn(L, L.LemX + L.LemDx, L.LemY - 2)

    else if HasIndestructibleAt(L.LemX, L.LemY, L.LemDx, baMining) then
      MinerTurn(L, L.LemX, L.LemY);
  end;
end;


function TLemmingGame.HandleFalling(L: TLemming): Boolean;
var
  CurrFallDist: Integer;
  MaxFallDist: Integer;

  function IsFallFatal: Boolean;
  begin
    Result := (not (L.LemIsFloater or L.LemIsGlider))
          and (((L.LemFallen > MAX_FALLDISTANCE) and not HasTriggerAt(L.LemX, L.LemY, trNoSplat))
            or HasTriggerAt(L.LemX, L.LemY, trSplat));
  end;
begin
  Result := True;

  if L.LemIsFloater and (L.LemTrueFallen > 16) then
    // Depending on updrafts, this happens on the 6th-8th frame
    Transition(L, baFloating)

  else if L.LemIsGlider and (L.LemTrueFallen > 6) then
    // This always happens on the 4th frame
    Transition(L, baGliding)

  else
  begin
    CurrFallDist := 0;
    MaxFallDist := 3;

    if HasTriggerAt(L.LemX, L.LemY, trUpdraft) then MaxFallDist := 2;

    // Move lem until hitting ground
    while (CurrFallDist < MaxFallDist) and not HasPixelAt(L.LemX, L.LemY) do
    begin
      Inc(L.LemY);
      Inc(CurrFallDist);
      Inc(L.LemFallen);
      Inc(L.LemTrueFallen);
      if HasTriggerAt(L.LemX, L.LemY, trUpdraft) then L.LemFallen := 0;
    end;

    if CurrFallDist < MaxFallDist then
    begin
      // Object checks at hitting ground
      if IsFallFatal then
        fLemNextAction := baSplatting
      else
        fLemNextAction := baWalking;
    end;
  end;
end;


function TLemmingGame.HandleFloating(L: TLemming): Boolean;
var
  MaxFallDist: Integer;
const
  FloaterFallTable: array[1..17] of Integer =
    (3, 3, 3, 3, -1, 0, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2);
begin
  Result := True;

  MaxFallDist := FloaterFallTable[L.LemPhysicsFrame];
  if HasTriggerAt(L.LemX, L.LemY, trUpdraft) then Dec(MaxFallDist);

  if MaxFallDist > MaxIntValue([FindGroundPixel(L.LemX, L.LemY), 0]) then
  begin
    // Lem has found solid terrain
    Inc(L.LemY, MaxIntValue([FindGroundPixel(L.LemX, L.LemY), 0]));
    fLemNextAction := baWalking;
  end
  else
    Inc(L.LemY, MaxFallDist);
end;


function TLemmingGame.HandleGliding(L: TLemming): Boolean;
var
  MaxFallDist, GroundDist: Integer;
  LemDy: Integer;

  // Check for turning around
  function DoTurnAround(L: TLemming; MoveForwardFirst: Boolean): Boolean;
  var
    Dy: Integer;
    CurLemX: Integer; // Adapted X-coordinate of Lemming
  begin
    CurLemX := L.LemX;
    if MoveForwardFirst then Inc(CurLemX, L.LemDx);

    // Search for free pixels below
    Dy := 0;
    repeat
      // bug-fix for http://www.lemmingsforums.net/index.php?topic=2693
      if HasPixelAt(CurLemX, L.LemY + Dy) and HasPixelAt(CurLemX - L.LemDx, L.LemY + Dy) then
      begin
        // Abort computation and let lemming turn around
        Result := True;
        Exit;
      end;
      Inc(Dy);
    until (Dy > 3) or (not HasPixelAt(CurLemX, L.LemY + Dy));

    if Dy > 3 then Result := True
    else Result := False;
  end;

  // Special behavior in 1-pxiel wide shafts: Move one pixel down even when turning
  procedure CheckOnePixelShaft(L: TLemming);
  var
    LemYDir: Integer;
  begin
    // Move upwards if in updraft
    LemYDir := 1;
    if HasTriggerAt(L.LemX, L.LemY, trUpdraft) then LemYDir := -1;

    if    ((FindGroundPixel(L.LemX + L.LemDx, L.LemY) < -4) and DoTurnAround(L, True))
       or (     HasPixelAt(L.LemX + L.LemDx, L.LemY + 1)
            and HasPixelAt(L.LemX + L.LemDx, L.LemY + 2)
            and HasPixelAt(L.LemX + L.LemDx, L.LemY + 3)) then
    begin
      if HasPixelAt(L.LemX, L.LemY) and (LemYDir = 1) then
        fLemNextAction := baWalking
      else if HasPixelAt(L.LemX, L.LemY - 2) and (LemYDir = -1) then
        // Do nothing
      else
        Inc(L.LemY, LemYDir);
    end;
  end;

  function HeadCheck(LemX, Lemy: Integer): Boolean; // returns False if lemming hits his head
  begin
    Result := not (     HasPixelAt(LemX - 1, LemY - 12)
                    and HasPixelAt(LemX, LemY - 12)
                    and HasPixelAt(LemX + 1, LemY - 12));
  end;


const
  GliderFallTable: array[1..17] of Integer =
    (3, 3, 3, 3, -1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
begin
  Result := True;
  MaxFallDist := GliderFallTable[L.LemPhysicsFrame];

  if HasTriggerAt(L.LemX, L.LemY, trUpdraft) then
  begin
    Dec(MaxFallDist);
    // Rise a pixel every second frame
    if (L.LemPhysicsFrame >= 9) and (L.LemPhysicsFrame mod 2 = 1)
       and (not HasPixelAt(L.LemX + L.LemDx, L.LemY + MaxFallDist - 1))
       and HeadCheck(L.LemX, L.LemY - 1) then
      Dec(MaxFallDist);
  end;

  Inc(L.LemX, L.LemDx);

  // Do upwards movement right away
  if MaxFallDist < 0 then Inc(L.LemY, MaxFallDist);

  GroundDist := FindGroundPixel(L.LemX, L.LemY);

  if GroundDist < -4 then // pushed down or turn around
  begin
    if DoTurnAround(L, false) then
    begin
      // move back and turn around
      Dec(L.LemX, L.LemDx);
      TurnAround(L);
      CheckOnePixelShaft(L);
    end
    else
    begin
      // move down
      LemDy := 0;
      repeat
        Inc(LemDy);
      until not HasPixelAt(L.LemX, L.LemY + LemDy);
      Inc(L.LemY, LemDy);
    end
  end

  else if GroundDist < 0 then // Move 1 to 4 pixels up
  begin
    Inc(L.LemY, GroundDist);
    fLemNextAction := baWalking;
  end

  else if MaxFallDist > 0 then // no pixel above current location; not checked if one has moved upwards
  begin // same algorithm as for faller!
    if MaxFallDist > GroundDist then
    begin
      // Lem has found solid terrain
      Assert(GroundDist >= 0, 'glider GroundDist negative');
      Inc(L.LemY, GroundDist);
      fLemNextAction := baWalking;
    end
    else
      Inc(L.LemY, MaxFallDist);
  end

  else if HasTriggerAt(L.LemX, L.LemY, trUpdraft) then // head check for pushing down in updraft
  begin
    // move down at most 2 pixels until the HeadCheck passes
    LemDy := -1;
    while (not HeadCheck(L.LemX, L.LemY)) and (LemDy < 2) do
    begin
      Inc(L.LemY);
      Inc(LemDy);
      // Check whether the glider has reached the ground
      if HasPixelAt(L.LemX, L.LemY) then
      begin
        fLemNextAction := baWalking;
        LemDy := 4;
      end;
    end;
  end;
end;


function TLemmingGame.HandleSplatting(L: TLemming): Boolean;
begin
  Result := False;
  if L.LemEndOfAnimation then RemoveLemming(L, RM_KILL);
end;

function TLemmingGame.HandleExiting(L: TLemming): Boolean;
begin
  Result := False;
  if L.LemEndOfAnimation then RemoveLemming(L, RM_SAVE);
end;

function TLemmingGame.HandleVaporizing(L: TLemming): Boolean;
begin
  Result := False;
  if L.LemEndOfAnimation then RemoveLemming(L, RM_KILL);
end;

function TLemmingGame.HandleBlocking(L: TLemming): Boolean;
begin
  Result := True;
  if not HasPixelAt(L.LemX, L.LemY) then Transition(L, baFalling);
end;

function TLemmingGame.HandleShrugging(L: TLemming): Boolean;
begin
  Result := True;
  if L.LemEndOfAnimation then Transition(L, baWalking);
end;

function TLemmingGame.HandleOhNoing(L: TLemming): Boolean;
begin
  Result := True;
  if L.LemEndOfAnimation then
  begin
    if L.LemAction = baOhNoing then
      Transition(L, baExploding)
    else // if L.LemAction = baStoning then
      Transition(L, baStoneFinish);
    L.LemHasBlockerField := False; // remove blocker field
    SetBlockerMap;
    Result := False;
  end
  else if not HasPixelAt(L.LemX, L.LemY) then
  begin
    L.LemHasBlockerField := False; // remove blocker field
    SetBlockerMap;
    // let lemming fall
    if HasTriggerAt(L.LemX, L.LemY, trUpdraft) then
      Inc(L.LemY, MinIntValue([FindGroundPixel(L.LemX, L.LemY), 2]))
    else
      Inc(L.LemY, MinIntValue([FindGroundPixel(L.LemX, L.LemY), 3]));
  end;
end;


function TLemmingGame.HandleExploding(L: TLemming): Boolean;
begin
  Result := False;

  if L.LemAction = baExploding then
    ApplyExplosionMask(L)
  else // if L.LemAction = baStoneFinish
    ApplyStoneLemming(L);

  RemoveLemming(L, RM_KILL);
  L.LemExploded := True;
  L.LemParticleTimer := PARTICLE_FRAMECOUNT;
  fParticleFinishTimer := PARTICLE_FINISH_FRAMECOUNT;
end;


procedure TLemmingGame.RemoveLemming(L: TLemming; RemMode: Integer = 0; Silent: Boolean = false);
begin
  if IsSimulating then Exit;

  if L.LemIsZombie then
  begin
    Assert(RemMode <> RM_SAVE, 'Zombie removed with RM_SAVE removal type!');
    Assert(RemMode <> RM_ZOMBIE, 'Zombie removed with RM_ZOMBIE removal type!');
    L.LemRemoved := True;
    if (RemMode = RM_NEUTRAL) and not Silent then
      CueSoundEffect(SFX_FALLOUT, L.Position);
  end

  else if not L.LemRemoved then // usual and living lemming
  begin
    Inc(LemmingsRemoved);
    Dec(LemmingsOut);
    L.LemRemoved := True;

    case RemMode of
    RM_SAVE : begin
                Inc(LemmingsIn);
                if LemmingsIn = Level.Info.RescueCount then
                  GameResultRec.gLastRescueIteration := fCurrentIteration;
              end;
    RM_NEUTRAL: if not Silent then
                  CueSoundEffect(SFX_FALLOUT, L.Position);
    RM_ZOMBIE: begin
                 if not Silent then
                   CueSoundEffect(SFX_ZOMBIE);
                 L.LemIsZombie := True;
                 L.LemRemoved := False;
               end;
    end;           
  end;

  DoTalismanCheck;
end;


procedure TLemmingGame.UpdateLemmings;
{-------------------------------------------------------------------------------
  The main method: handling a single frame of the game.
-------------------------------------------------------------------------------}
begin
  if fGameFinished then
    Exit;
  CheckForGameFinished;

  CheckAdjustReleaseRate;

  if LemmingsToRelease > 0 then
  begin
    if (CurrReleaseRate < LowestReleaseRate) or (CurrentIteration < 20) then
      LowestReleaseRate := CurrReleaseRate;
    if (CurrReleaseRate > HighestReleaseRate) or (CurrentIteration < 20) then
      HighestReleaseRate := CurrReleaseRate;
  end;

  CheckForQueuedAction; // needs to be done before CheckForReplayAction, because it writes an assignment in the replay
  CheckForReplayAction;

  // erase existing ShadowBridge
  if fExistShadow then
  begin
    fRenderer.ClearShadows;
    fExistShadow := false;
  end;

  // just as a warning: do *not* mess around with the order here
  IncrementIteration;
  CheckReleaseLemming;
  CheckLemmings;
  CheckUpdateNuking;
  UpdateInteractiveObjects;

  // Get highest priority lemming under cursor
  GetPriorityLemming(fLemSelected, SkillPanelButtonToAction[fSelectedSkill], CursorPoint);

  DrawAnimatedObjects;

  fRenderInterface.SelectedLemming := fLemSelected;

  // Check lemmings under cursor
  HitTest;
end;


procedure TLemmingGame.IncrementIteration;
var
  i: Integer;
  AX, AY: Integer; // average position of entrances
  EntryOpenCount: Integer;

  function UseZombieSound: Boolean;
  var
    i: Integer;
  begin
    Result := false;
    for i := 0 to LemmingList.Count-1 do
      if LemmingList[i].LemIsZombie then
      begin
        Result := true;
        Exit;
      end;
    for i := 0 to ObjectInfos.Count-1 do
      if (ObjectInfos[i].TriggerEffect = 23)
      and not (ObjectInfos[i].IsDisabled)
      and (ObjectInfos[i].PreassignedSkills and 64 <> 0) then
      begin
        Result := true;
        Exit;
      end;
  end;
const
  OID_ENTRY = 1;
begin
  Inc(fCurrentIteration);
  Inc(fClockFrame);
  if DelayEndFrames > 0 then Dec(DelayEndFrames);

  if fParticleFinishTimer > 0 then
    Dec(fParticleFinishTimer);

  if fClockFrame = 17 then
  begin
    fClockFrame := 0;
    if TimePlay > -5999 then Dec(TimePlay);
  end;

  // hard coded dos frame numbers
  case CurrentIteration of
    15:
      if UseZombieSound then
        CueSoundEffect(SFX_ZOMBIE)
      else
        CueSoundEffect(SFX_LETSGO);
    35:
      begin
        EntriesOpened := False;
        EntryOpenCount := 0;
        AX := 0;
        AY := 0;
        for i := 0 to ObjectInfos.Count - 1 do
          if ObjectInfos[i].TriggerEffect = DOM_WINDOW then
          begin
            ObjectInfos[i].Triggered := True;
            ObjectInfos[i].CurrentFrame := 1;
            EntriesOpened := true;
            Inc(EntryOpenCount);
            AX := AX + ObjectInfos[i].Center.X;
            AY := AY + ObjectInfos[i].Center.Y;
          end;
        if EntriesOpened then
        begin
          AX := AX div EntryOpenCount;
          AY := AY div EntryOpenCount;
          CueSoundEffect(SFX_ENTRANCE, Point(AX, AY));
        end;
        if not EntriesOpened then
          PlayMusic;
        EntriesOpened := True;
      end;
    55:
      PlayMusic;
  end;

end;


procedure TLemmingGame.HitTest(Autofail: Boolean = false);
var
  HitCount: Integer;
  L, OldLemSelected: TLemming;
  S: string;
  i: integer;
  ForceShowAthleteInfo: Boolean;
begin
  if Autofail then fHitTestAutoFail := true;

  OldLemSelected := fLemSelected;
  // Shadow stuff for updated selected lemming
  GetPriorityLemming(fLemSelected, SkillPanelButtonToAction[fSelectedSkill], CursorPoint);
  CheckForNewShadow;

  // Get new priority lemming including lems that cannot receive the skill
  HitCount := GetPriorityLemming(L, baNone, CursorPoint);

  if L <> OldLemSelected then
  begin
    fLemSelected := L;
    fRenderInterface.SelectedLemming := L;
  end;

  LastHitCount := HitCount;

  if Assigned(L) and not fHitTestAutofail then
  begin
    // get highlight text
    ForceShowAthleteInfo := L.LemIsZombie
           and (L.LemIsClimber or L.LemIsFloater or L.LemIsGlider or L.LemIsSwimmer or L.LemIsMechanic);

    if IsShowAthleteInfo or ForceShowAthleteInfo then
    begin
      S := '-----';
      if L.LemIsClimber then S[1] := 'C';
      if L.LemIsSwimmer then S[2] := 'S';
      if L.LemIsFloater then S[3] := 'F';
      if L.LemIsGlider then S[3] := 'G';
      if L.LemIsMechanic then S[4] := 'D';
      if L.LemIsZombie then S[5] := 'Z';
    end else begin
      S := LemmingActionStrings[L.LemAction];

      if not (L.LemAction in [baBuilding, baPlatforming, baStacking, baBashing, baMining, baDigging, baBlocking]) then
      begin
        i := 0;
        if L.LemIsClimber then inc(i);
        if L.LemIsSwimmer then inc(i);
        if L.LemIsFloater then inc(i);
        if L.LemIsGlider then inc(i);
        if L.LemIsMechanic then inc(i);

        case i of
          5: S := SQuadathlete;
          4: S := SQuadathlete;
          3: S := STriathlete;
          2: S := SAthlete;
          1: begin
               if L.LemIsClimber then S := SClimber;
               if L.LemIsSwimmer then S := SSwimmer;
               if L.LemIsFloater then S := SFloater;
               if L.LemIsGlider  then S := SGlider;
               if L.LemIsMechanic then S := SMechanic;
             end;
        end;
      end;

      if L.LemIsZombie then S := SZombie;
    end;

  end;
end;

function TLemmingGame.ProcessSkillAssignment(IsHighlight: Boolean = false): Boolean;
var
  Sel: TBasicLemmingAction;
begin
  Result := False;

  // convert buttontype to skilltype
  Sel := SkillPanelButtonToAction[fSelectedSkill];
  if Sel = baNone then Exit;

  Result := AssignNewSkill(Sel, IsHighlight);

  if Result then
    CheckForNewShadow;        // probably unneeded now?
end;

function TLemmingGame.ProcessHighlightAssignment: Boolean;
var
  L: TLemming;
begin
  Result := False;
  if GetPriorityLemming(L, baNone, CursorPoint) > 0 then
    fHighlightLemmingID := L.LemIndex
  else
    fHighlightLemmingID := -1;
end;

procedure TLemmingGame.ReplaySkillAssignment(aReplayItem: TReplaySkillAssignment);
var
  L: TLemming;
  OldHighlightLemID: Integer;
begin
  with aReplayItem do
  begin
    if (LemmingIndex < 0) or (LemmingIndex >= LemmingList.Count) then
    begin
      RegainControl;
      Exit;
    end;

    L := LemmingList.List^[LemmingIndex];

    if Skill in AssignableSkills then
    begin
      // In order to preserve old replays, we have to check if the skill assignments are still possible
      // As the priority of lemmings has changed, we have to hightlight this lemming
      // After having done the assignment, revert the hightlightning.
      OldHighlightLemID := fHighlightLemmingID;
      fHighlightLemmingID := L.LemIndex;
      AssignNewSkill(Skill, true, true);
      fHighlightLemmingID := OldHighlightLemID;
    end
    else
    begin
      Application.Messagebox('Replay action skipped: It contains an invalid skill.', 'Replay warning')
    end;

  end;
end;


function TLemmingGame.GetSelectedSkill: Integer;
var
  i: Integer;
begin
  Result := 8;
  for i := 0 to 7 do
    if fSelectedSkill = fActiveSkills[i] then
    begin
      Result := i;
      Exit;
    end;
end;


procedure TLemmingGame.SetSelectedSkill(Value: TSkillPanelButton; MakeActive: Boolean = True; RightClick: Boolean = False);
  function CheckSkillInSet(Value: TSkillPanelButton): Boolean;
  var
    i: Integer;
  begin
    Result := false;
    for i := 0 to 7 do
      if fActiveSkills[i] = Value then Result := true;
  end;
begin

  case Value of
    spbFaster:
      begin
        if not MakeActive then
        begin
          ReleaseRateModifier := 0;
          Exit;
        end;

        if Level.Info.ReleaseRateLocked or (CurrReleaseRate = 99) then Exit;

        if RightClick then
          RecordReleaseRate(99)
        else
          ReleaseRateModifier := 1;
      end;
    spbSlower:
      begin
        if not MakeActive then
        begin
          ReleaseRateModifier := 0;
          Exit;
        end;
        
        if Level.Info.ReleaseRateLocked or (CurrReleaseRate = Level.Info.ReleaseRate) then Exit;

        if RightClick then
          RecordReleaseRate(Level.Info.ReleaseRate)
        else
          ReleaseRateModifier := -1;
      end;
    spbPause:
      begin
        // this now needs to be handled elsewhere
        //Paused := not Paused;
        //FastForward := False;
      end;
    spbNuke:
      begin
        RecordNuke;
      end;
    spbNone: ; // Do Nothing
    else // all skill buttons
      begin
        if (not CheckSkillInSet(Value)) then
          Exit;

        if fSelectedSkill <> Value then
        begin
          fSelectedSkill := Value;
          CheckForNewShadow;
        end;

        if RightClick and (GetHighlitLemming <> nil) and (SkillPanelButtonToAction[Value] <> baNone) then
        begin
          if ProcessSkillAssignment(true) then
            fRenderInterface.ForceUpdate := true;
        end;
      end;
  end;
end;

procedure TLemmingGame.CheckReleaseLemming;
var
  NewLemming: TLemming;
  ix: Integer;
begin

  if not EntriesOpened then
    Exit;
  if UserSetNuking then
    Exit;

  // NextLemmingCountdown is initialized to 20 before start of a level
  if NextLemmingCountdown > 0 then Dec(NextLemmingCountdown);

  if NextLemmingCountdown = 0 then
  begin
    NextLemmingCountdown := CalculateNextLemmingCountdown;
    if (LemmingsToRelease > 0) then
    begin
      ix := Level.Info.SpawnOrder[Level.Info.LemmingsCount - Level.PreplacedLemmings.Count - LemmingsToRelease];
      if ix >= 0 then
      begin
        NewLemming := TLemming.Create;
        with NewLemming do
        begin
          LemIndex := LemmingList.Add(NewLemming);
          Transition(NewLemming, baFalling);

          LemX := ObjectInfos[ix].TriggerRect.Left;
          LemY := ObjectInfos[ix].TriggerRect.Top;
          LemDX := 1;
          if ObjectInfos[ix].IsFlipPhysics then TurnAround(NewLemming);

          LemUsedSkillCount := 0;

          if (ObjectInfos[ix].PreAssignedSkills and 1) <> 0 then LemIsClimber := true;
          if (ObjectInfos[ix].PreAssignedSkills and 2) <> 0 then LemIsSwimmer := true;
          if (ObjectInfos[ix].PreAssignedSkills and 4) <> 0 then LemIsFloater := true
          else if (ObjectInfos[ix].PreAssignedSkills and 8) <> 0 then LemIsGlider := true;
          if (ObjectInfos[ix].PreAssignedSkills and 16) <> 0 then LemIsMechanic := true;
          if (ObjectInfos[ix].PreAssignedSkills and 64) <> 0 then
          begin
            Dec(fSpawnedDead);
            RemoveLemming(NewLemming, RM_ZOMBIE, true);
          end;
        end;
        Dec(LemmingsToRelease);
        Inc(LemmingsOut);
      end;
    end;
  end;
end;

procedure TLemmingGame.CheckUpdateNuking;
var
  CurrentLemming: TLemming;
begin

  if UserSetNuking and ExploderAssignInProgress then
  begin

    // find first following non removed lemming
    while     (Index_LemmingToBeNuked < LemmingList.Count-1)
          and (LemmingList[Index_LemmingToBeNuked].LemRemoved) do
      Inc(Index_LemmingToBeNuked);

    if (Index_LemmingToBeNuked > LemmingList.Count-1) then
      ExploderAssignInProgress := FALSE
    else
    begin
      CurrentLemming := LemmingList[Index_LemmingToBeNuked];
      with CurrentLemming do
      begin
        if     (LemExplosionTimer = 0)
           and not (LemAction in [baSplatting, baExploding])
           and not LemIsZombie then
          LemExplosionTimer := 84;
      end;
      Inc(Index_LemmingToBeNuked);

    end;
  end;
end;

procedure TLemmingGame.CreateLemmingAtCursorPoint;
{-------------------------------------------------------------------------------
  debugging procedure: click and create lemming
-------------------------------------------------------------------------------}
var
  NewLemming: TLemming;
begin
  if not EntriesOpened then
    Exit;
  if UserSetNuking then
    Exit;

  NewLemming := TLemming.Create;
  with NewLemming do
  begin
    LemIndex := LemmingList.Add(NewLemming);
    Transition(NewLemming, baFalling);
    LemX := CursorPoint.X;
    LemY := CursorPoint.Y;
    LemDX := 1;
  end;
  Inc(LemmingsOut);

end;


function TLemmingGame.CalculateNextLemmingCountdown: Integer;
(* ccexplore:
  All I know is that in the DOS version, the formula is that for a given RR,
  the number of frames from one release to the next is:
  (99 - RR) / 2 + 4
  Where the division is the standard truncating integer division
  (so for example RR 99 and RR 98 acts identically).

  This means for example, at RR 99,
  it'd be release, wait, wait, wait, release, wait, wait, wait, release,

  I don't know what the frame rate is though on the DOS version,
  although to a large extent this mostly does not matter, since most aspects
  of the game mechanics is based off of number of frames rather than absolute time.
*)
begin
  Result := 99 - currReleaseRate;
  if (Result < 0) then
    Inc(Result, 256);
  Result := Result div 2 + 4
end;

procedure TLemmingGame.CueSoundEffect(aSound: String);
begin
  if IsSimulating then Exit; // Not play sound in simulation mode

  MessageQueue.Add(GAMEMSG_SOUND, aSound);
end;

procedure TLemmingGame.CueSoundEffect(aSound: String; aOrigin: TPoint);
begin
  if IsSimulating then Exit; // Not play sound in simulation mode

  MessageQueue.Add(GAMEMSG_SOUND_BAL, aSound, aOrigin.X);
end;


function TLemmingGame.GetHighlitLemming: TLemming;
begin
  Result := nil;
  if fHighlightLemmingID < 0 then Exit;
  if fHighlightLemmingID >= LemmingList.Count then Exit;
  if LemmingList[fHighlightLemmingID].LemRemoved then Exit;
  if LemmingList[fHighlightLemmingID].LemTeleporting then Exit;
  Result := LemmingList[fHighlightLemmingID];
end;


function TLemmingGame.CheckIfLegalRR(aRR: Integer): Boolean;
begin
  if Level.Info.ReleaseRateLocked
  or (aRR > 99)
  or (aRR < Level.Info.ReleaseRate) then
    Result := false
  else
    Result := true;
end;

procedure TLemmingGame.AdjustReleaseRate(aRR: Integer);
begin
  if (aRR <> currReleaseRate) and CheckIfLegalRR(aRR) then
    currReleaseRate := aRR;
end;


procedure TLemmingGame.RecordNuke;
{-------------------------------------------------------------------------------
  Easy one: Record nuking. Always add new record.
-------------------------------------------------------------------------------}
var
  E: TReplayNuke;
begin
  if not fPlaying then
    Exit;
  E := TReplayNuke.Create;
  E.Frame := fCurrentIteration;
  fReplayManager.Add(E);
end;

procedure TLemmingGame.RecordReleaseRate(aRR: Integer);
var
  E: TReplayChangeReleaseRate;
begin
  if not fPlaying then
    Exit;

  E := TReplayChangeReleaseRate.Create;
  E.Frame := fCurrentIteration;
  E.NewReleaseRate := aRR;
  E.SpawnedLemmingCount := LemmingList.Count;

  fReplayManager.Add(E);
  CheckForReplayAction(true);
end;

procedure TLemmingGame.RecordSkillAssignment(L: TLemming; aSkill: TBasicLemmingAction);
{-------------------------------------------------------------------------------
  Always add new record.
-------------------------------------------------------------------------------}
var
  E: TReplaySkillAssignment;
begin
  if not fPlaying then Exit;

  E := TReplaySkillAssignment.Create;
  E.Skill := aSkill;
  E.SetInfoFromLemming(L, (L.LemIndex = fHighlightLemmingID));
  E.Frame := fCurrentIteration;

  fReplayManager.Add(E);
end;

procedure TLemmingGame.CheckForReplayAction(PausedRRCheck: Boolean = false);
var
  R: TBaseReplayItem;
  i: Integer;

  procedure ApplySkillAssign;
  var
    E: TReplaySkillAssignment absolute R;
  begin
    ReplaySkillAssignment(E);
  end;

  procedure ApplyReleaseRate;
  var
    E: TReplayChangeReleaseRate absolute R;
  begin
    AdjustReleaseRate(E.NewReleaseRate);
  end;

  procedure ApplyNuke;
  var
    E: TReplayNuke absolute R;
  begin
    UserSetNuking := True;
    ExploderAssignInProgress := True;
  end;

  function Handle: Boolean;
  begin
    Result := false;
    if R = nil then Exit;

    Result := true;

    if R is TReplaySkillAssignment then
      ApplySkillAssign;

    if R is TReplayChangeReleaseRate then
      ApplyReleaseRate;

    if R is TReplayNuke then
      ApplyNuke;
  end;
begin
  try
    // Note - the fReplayManager getters can return nil, and often will!
    // The "Handle" procedure ensures this does not lead to errors.
    i := 0;
    repeat
      R := fReplayManager.ReleaseRateChange[fCurrentIteration, i];
      Inc(i);
    until not Handle;
    if PausedRRCheck then Exit;

    i := 0;
    repeat
      R := fReplayManager.Assignment[fCurrentIteration, i];
      Inc(i);
    until not Handle;

  finally
    // do nothing
  end;
end;

procedure TLemmingGame.CheckForQueuedAction;
var
  i: Integer;
  L: TLemming;
  NewSkill: TBasicLemmingAction;
begin
  // First check whether there was already a skill assignment this frame
  if Assigned(fReplayManager.Assignment[fCurrentIteration, 0]) then Exit;

  for i := 0 to LemmingList.Count - 1 do
  begin
    L := LemmingList.List^[i];

    if L.LemQueueAction = baNone then Continue;

    if L.LemRemoved or L.LemIsZombie or L.LemTeleporting then
    begin
      // delete queued action first
      L.LemQueueAction := baNone;
      L.LemQueueFrame := 0;
      Continue;
    end;

    NewSkill := L.LemQueueAction;

    // Try assigning the skill
    if NewSkillMethods[NewSkill](L) and CheckSkillAvailable(NewSkill) then
      // Record skill assignment, so that we apply it in CheckForReplayAction
      RecordSkillAssignment(L, NewSkill)
    else
    begin;
      Inc(L.LemQueueFrame);
      // Delete queued action after 8 frames
      if L.LemQueueFrame > 7 then
      begin
        L.LemQueueAction := baNone;
        L.LemQueueFrame := 0;
      end;
    end;
  end;

end;


procedure TLemmingGame.CheckLemmings;
var
  i: Integer;
  CurrentLemming: TLemming;
  HandleInteractiveObjects: Boolean; // wrong name: This just remembers whether we should stop checking more of the lemming
begin

  ZombieMap.Clear(0);

  for i := 0 to LemmingList.Count - 1 do
  begin
    CurrentLemming := LemmingList.List^[i];

    with CurrentLemming do
    begin
      HandleInteractiveObjects := True;
      // @particles
      if LemParticleTimer >= 0 then
        Dec(LemParticleTimer);

      if LemRemoved then
        Continue;

      // Put lemming out of receiver if teleporting is finished.
      if LemTeleporting then
        HandleInteractiveObjects := CheckLemTeleporting(CurrentLemming);

      // Explosion-Countdown
      if HandleInteractiveObjects and (LemExplosionTimer <> 0) then
        HandleInteractiveObjects := not UpdateExplosionTimer(CurrentLemming);

      // HERE COME THE MAIN THREE LINES !!!
      // Let lemmings move
      if HandleInteractiveObjects then
        HandleInteractiveObjects := HandleLemming(CurrentLemming);
      // Check whether the lem is still on screen
      if HandleInteractiveObjects then
        HandleInteractiveObjects := CheckLevelBoundaries(CurrentLemming);
      // Check whether the lem has moved over trigger areas
      if HandleInteractiveObjects then
        CheckTriggerArea(CurrentLemming);
    end;

  end;

  // Check for lemmings meeting zombies
  // Need to do this in separate loop, because the ZombieMap gets only set during HandleLemming!
  for i := 0 to LemmingList.Count - 1 do
  begin
    CurrentLemming := LemmingList.List^[i];
    with CurrentLemming do
    begin
      // Zombies //
      if     (ReadZombieMap(LemX, LemY) and 1 <> 0)
         and (LemAction <> baExiting)
         and not CurrentLemming.LemIsZombie then
        RemoveLemming(CurrentLemming, RM_ZOMBIE);
    end;
  end;

end;

procedure TLemmingGame.SimulateTransition(L: TLemming; NewAction: TBasicLemmingAction);
begin
  Inc(fSimulationDepth);

  if (NewAction = baStacking) then L.LemStackLow := not HasPixelAt(L.LemX + L.LemDx, L.LemY);
  Transition(L, NewAction);

  Dec(fSimulationDepth);
end;

function TLemmingGame.SimulateLem(L: TLemming; DoCheckObjects: Boolean = True): TArrayArrayInt; // Simulates advancing one frame for the lemming L
var
  HandleInteractiveObjects: Boolean;
  LemPosArray: TArrayArrayInt;
  i: Integer;
begin
  // Start Simulation Mode
  Inc(fSimulationDepth);

  // Advance lemming one frame
  HandleInteractiveObjects := HandleLemming(L);
  // Check whether the lem is still on screen
  if HandleInteractiveObjects then
    HandleInteractiveObjects := CheckLevelBoundaries(L);
  // Check whether the lem has moved over trigger areas
  if HandleInteractiveObjects and DoCheckObjects then
  begin
    // Get positions to check
    LemPosArray := GetObjectCheckPositions(L);

    // Check for exit, traps and teleporters (but stop at teleporters!)
    for i := 0 to Length(LemPosArray[0]) do
    begin
      // Transition if we are at the end position and need to do one
      if (fLemNextAction <> baNone) and ([LemPosArray[0, i], LemPosArray[1, i]] = [L.LemX, L.LemY]) then
      begin
        Transition(L, fLemNextAction);
        fLemNextAction := baNone;
      end;

      if    (    HasTriggerAt(LemPosArray[0, i], LemPosArray[1, i], trTrap)
             and (FindObjectID(LemPosArray[0, i], LemPosArray[1, i], trTrap) <> 65535))
         or HasTriggerAt(LemPosArray[0, i], LemPosArray[1, i], trExit)
         or HasTriggerAt(LemPosArray[0, i], LemPosArray[1, i], trWater)
         or HasTriggerAt(LemPosArray[0, i], LemPosArray[1, i], trFire)
         or (    HasTriggerAt(LemPosArray[0, i], LemPosArray[1, i], trTeleport)
             and (FindObjectID(LemPosArray[0, i], LemPosArray[1, i], trTeleport) <> 65535))
         then
      begin
        L.LemAction := baExploding; // This always stops the simulation!
        L := nil;
        Break;
      end;

      // End this loop when we have reached the lemming position
      if (L.LemX = LemPosArray[0, i]) and (L.LemY = LemPosArray[1, i]) then Break;
    end;

    // Check for blocker fields and force-fields at the end of movement
    if Assigned(L) then
    begin
      if HasTriggerAt(L.LemX, L.LemY, trForceLeft) then
        HandleForceField(L, -1)
      else if HasTriggerAt(L.LemX, L.LemY, trForceRight) then
        HandleForceField(L, 1);
    end;
  end else
    SetLength(LemPosArray, 0);

  // End Simulation Mode
  Dec(fSimulationDepth);

  Result := LemPosArray;
end;



function TLemmingGame.CheckLemTeleporting(L: TLemming): Boolean;
// This function checks, whether a lemming appears out of a receiver
var
  ObjInfo: TInteractiveObjectInfo;
  ObjID: Integer;
begin
  Result := False;

  Assert(L.LemTeleporting = True, 'CheckLemTeleporting called for non-teleporting lemming');

  // Search for Teleporter, the lemming is in
  ObjID := -1;
  repeat
    Inc(ObjID);
  until (ObjID > ObjectInfos.Count - 1) or (L.LemIndex = ObjectInfos[ObjID].TeleLem);

  Assert(ObjID < ObjectInfos.Count, 'Teleporter associated to teleporting lemming not found');

  ObjInfo := ObjectInfos[ObjID];
  if ObjInfo.TriggerEffect = DOM_RECEIVER then
  begin
    if    (ObjInfo.CurrentFrame + 1 >= ObjInfo.AnimationFrameCount)
       or ((ObjInfo.KeyFrame <> 0) and (ObjInfo.CurrentFrame + 1 >= ObjInfo.KeyFrame)) then
    begin
      L.LemTeleporting := False; // Let lemming reappear
      ObjInfo.TeleLem := -1;
      Result := True;
      // Reset blocker map, if lemming is a blocker
      if L.LemAction = baBlocking then
      begin
        L.LemHasBlockerField := True;
        SetBlockerMap;
      end;
    end;
  end;
end;


procedure TLemmingGame.SetGameResult;
{-------------------------------------------------------------------------------
  We will not, I repeat *NOT* simulate the original Nuke-error.

  (ccexplore: sorry, code added to implement the nuke error by popular demand)
  (Nepster: sorry, namida removed the code long ago again by popular demand)
-------------------------------------------------------------------------------}
var
  gLemCap : Integer;
begin
  with GameResultRec do
  begin
    gCount              := Level.Info.LemmingsCount;
    gToRescue           := Level.Info.RescueCount;
    gRescued            := LemmingsIn;
    gLemCap             := Level.Info.LemmingsCount;

    gGotTalisman        := fTalismanReceived;

    if gLemCap = 0 then
      gTarget := 0
    else
      gTarget := (gToRescue * 100) div gLemCap;

    if not fGameCheated then
    begin
      if gCount = 0 then
        gDone := 0
      else
        gDone := (gRescued * 100) div gLemCap;   //gCount
    end else begin
      gRescued := gCount;
      if spbCloner in Level.Info.Skillset then gRescued := gRescued + Level.Info.SkillCount[spbCloner];
      gDone := (gRescued * 100) div gLemCap;
    end;

    GameResult          := gRescued >= gToRescue;
    gSuccess            := GameResult;
    gCheated            := fGameCheated;

  end;
end;

procedure TLemmingGame.RegainControl(Force: Boolean = false);
{-------------------------------------------------------------------------------
  This is a very important routine. It jumps from replay into usercontrol.
-------------------------------------------------------------------------------}
begin
  if ReplayInsert and not Force then Exit;

  fReplayManager.Cut(fCurrentIteration);
end;


(*procedure TLemmingGame.HyperSpeedBegin(PauseWhenDone: Boolean = False);
begin
  Inc(fHyperSpeedCounter);
  fHyperSpeed := True;
  FastForward := False;
  if PauseWhenDone then
    fPauseOnHyperSpeedExit := True
  else
    fPauseOnHyperSpeedExit := False;
end;

procedure TLemmingGame.HyperSpeedEnd;
begin
  if fHyperSpeedCounter > 0 then
  begin
    Dec(fHyperSpeedCounter);
    if fHyperSpeedCounter = 0 then
    begin
      fLeavingHyperSpeed := True;
      if CancelReplayAfterSkip then
      begin
        RegainControl;
        CancelReplayAfterSkip := false;
      end;
    end;
  end;
end;*)


procedure TLemmingGame.UpdateInteractiveObjects;
{-------------------------------------------------------------------------------
  This method handles the updating of the moving interactive objects:
  o Entrances moving
  o Continuously moving objects like water
  o Triggered objects (traps)
  NB: It does not handle the drawing
-------------------------------------------------------------------------------}
var
  Inf, Inf2: TInteractiveObjectInfo;
  i: Integer;
const
  OID_ENTRY = 1;
begin
  for i := ObjectInfos.Count - 1 downto 0 do
  begin
    Inf := ObjectInfos[i];

    if     (Inf.Triggered or (Inf.MetaObj.TriggerEffect in AlwaysAnimateObjects))
       and (Inf.TriggerEffect <> DOM_PICKUP) then
      Inc(Inf.CurrentFrame);

    if (Inf.TriggerEffect = DOM_TELEPORT)
      and (((Inf.CurrentFrame >= Inf.AnimationFrameCount) and (Inf.MetaObj.KeyFrame = 0))
        or ((Inf.CurrentFrame = Inf.MetaObj.KeyFrame) and (Inf.MetaObj.KeyFrame <> 0))   ) then
    begin
      MoveLemToReceivePoint(LemmingList.List^[Inf.TeleLem], i);
      Inf2 := ObjectInfos[Inf.ReceiverId];
      Assert(Inf2.TriggerEffect = DOM_RECEIVER, 'Lemming teleported to non-receiver object.');
      Inf2.TeleLem := Inf.TeleLem;
      Inf2.Triggered := True;
      Inf2.ZombieMode := Inf.ZombieMode;
      // Reset TeleLem for Teleporter
      Inf.TeleLem := -1;
    end;

    if Inf.CurrentFrame >= Inf.AnimationFrameCount then
    begin
      Inf.CurrentFrame := 0;
      Inf.Triggered := False;
      Inf.HoldActive := False;
      Inf.ZombieMode := False;
    end;

  end;

end;

procedure TLemmingGame.CheckAdjustReleaseRate;
var
  NewRR: Integer;
begin
  if ReleaseRateModifier = 0 then Exit;

  NewRR := CurrReleaseRate + ReleaseRateModifier;
  if CheckIfLegalRR(NewRR) then
    RecordReleaseRate(NewRR);
end;

procedure TLemmingGame.Finish(aReason: Integer);
begin
  SetGameResult;
  fGameFinished := True;
  MessageQueue.Add(GAMEMSG_FINISH, aReason);
end;

procedure TLemmingGame.Cheat;
begin
  fGameCheated := True;
  Finish(GM_FIN_TERMINATE);
end;

procedure TLemmingGame.EnsureCorrectReplayDetails;
begin
  with fReplayManager do
  begin
    LevelName := Trim(fLevel.Info.Title);
    LevelAuthor := Trim(fLevel.Info.Author);
    LevelGame := Trim(GameParams.SysDat.PackName);
    LevelRank := Trim(GameParams.Info.dSectionName);
    LevelPosition := GameParams.Info.dLevel + 1;
    LevelID := fLevel.Info.LevelID;
  end;
end;

function TLemmingGame.CheckSkillAvailable(aAction: TBasicLemmingAction): Boolean;
var
  HasSkillButton: Boolean;
  i: Integer;
begin
  Assert(aAction in AssignableSkills, 'CheckSkillAvailable for not assignable skill');

  HasSkillButton := false;
  for i := 0 to Length(fActiveSkills) - 1 do
    HasSkillButton := HasSkillButton or (fActiveSkills[i] = ActionToSkillPanelButton[aAction]);

  Result := HasSkillButton and ((CurrSkillCount[aAction] > 0) or GameParams.ChallengeMode);
end;


procedure TLemmingGame.UpdateSkillCount(aAction: TBasicLemmingAction; Rev : Boolean = false);
begin
  if GameParams.ChallengeMode xor Rev then
  begin
    CurrSkillCount[aAction] := Min(CurrSkillCount[aAction] + 1, 99);
  end
  else if not Rev then // and neither GameParams.ChallengeMode
  begin
    if CurrSkillCount[aAction] < 100 then // need to implement a seperate "infinite" setting
      CurrSkillCount[aAction] := Max(CurrSkillCount[aAction] - 1, 0);
    Inc(UsedSkillCount[aAction])
  end;
end;

procedure TLemmingGame.SaveGameplayImage(Filename: String);
var
  BMP: TBitmap32;
begin
  BMP := TBitmap32.Create;
  try
    fRenderer.DrawLevel(BMP);
    TPngInterface.SavePngFile(Filename, BMP, true);
  finally
    BMP.Free;
  end;
end;

procedure TLemmingGame.InitializeBrickColors(aBrickPixelColor: TColor32);
var
  i: Integer;
begin
  with TColor32Entry(aBrickPixelColor) do
  for i := 0 to Length(BrickPixelColors) - 1 do
  begin
    TColor32Entry(BrickPixelColors[i]).A := A;
    TColor32Entry(BrickPixelColors[i]).R := Min(Max(R + (i - 6) * 4, 0), 255);
    TColor32Entry(BrickPixelColors[i]).B := Min(Max(B + (i - 6) * 4, 0), 255);
    TColor32Entry(BrickPixelColors[i]).G := Min(Max(G + (i - 6) * 4, 0), 255);
  end;
end;

function TLemmingGame.GetIsReplaying: Boolean;
begin
  Result := fCurrentIteration <= fReplayManager.LastActionFrame;
end;

function TLemmingGame.GetIsSimulating: Boolean;
begin
  Result := fSimulationDepth > 0;
end;

function TLemmingGame.GetSkillCount(aSkill: TSkillPanelButton): Integer;
begin
  if (aSkill < Low(TSkillPanelButton)) or (aSkill > LAST_SKILL_BUTTON) then
    Result := 0
  else
    Result := CurrSkillCount[SkillPanelButtonToAction[aSkill]];
end;

end.
