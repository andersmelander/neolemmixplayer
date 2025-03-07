unit LemmixHotkeys;

// A quick and shitty unit to allow for customizable hotkeys.

interface

uses
  Dialogs,
  LemTypes,
  LemStrings,
  LemCore,
  Windows, Classes, SysUtils;

const
  MAX_KEY = 255;
  MAX_KEY_LEN = 4;
  KEYSET_VERSION = 10;

type
  TLemmixHotkeyAction = (lka_Null,
                         lka_Skill,
                         lka_ShowAthleteInfo,
                         lka_Exit,
                         lka_ReleaseRateMax,
                         lka_ReleaseRateUp,
                         lka_ReleaseRateDown,
                         lka_ReleaseRateMin,
                         lka_Pause,
                         lka_Nuke,
                         lka_BypassNuke,
                         lka_SaveState,
                         lka_LoadState,
                         lka_Highlight,
                         lka_DirLeft,
                         lka_DirRight,
                         lka_ForceWalker,
                         lka_Cheat,
                         lka_Skip,
                         lka_SpecialSkip,
                         lka_FastForward,
                         lka_SlowMotion,
                         lka_SaveImage,
                         lka_LoadReplay,
                         lka_SaveReplay,
                         lka_CancelReplay,
                         lka_EditReplay,
                         lka_ReplayInsert,
                         lka_Music,
                         lka_Sound,
                         lka_Restart,
                         lka_SkillLeft,
                         lka_SkillRight,
                         lka_ReleaseMouse,
                         lka_ClearPhysics,
                         lka_ToggleShadows,
                         lka_Projection,
                         lka_SkillProjection,
                         lka_ShowUsedSkills,
                         lka_FallDistance,
                         lka_ZoomIn,
                         lka_ZoomOut,
                         lka_Scroll);
  PLemmixHotkeyAction = ^TLemmixHotkeyAction;

  TSpecialSkipCondition = (ssc_LastAction,
                           ssc_NextShrugger,
                           ssc_HighlitStateChange);

  TKeyNameArray = Array [0..MAX_KEY] of String;

  TLemmixHotkey = record
    Action: TLemmixHotkeyAction;
    Modifier: Integer;
  end;

  TLemmixHotkeyManager = class
    private
      fKeyFunctions: Array[0..MAX_KEY] of TLemmixHotkey;
      fDisableSaving: Boolean;

      procedure LoadFile;
      function DoCheckForKey(aFunc: TLemmixHotkeyAction; aMod: Integer; CheckMod: Boolean): Boolean;
    public
      constructor Create;
      destructor Destroy; override;
      procedure ClearAllKeys;
      procedure SaveFile;

      procedure SetDefaultsTraditional;
      procedure SetDefaultsFunctional;
      procedure SetDefaultsMinimal;

      procedure SetKeyFunction(aKey: Word; aFunc: TLemmixHotkeyAction; aMod: Integer = 0);
      function CheckKeyEffect(aKey: Word): TLemmixHotkey;
      function CheckForKey(aFunc: TLemmixHotkeyAction): Boolean; overload;
      function CheckForKey(aFunc: TLemmixHotkeyAction; aMod: Integer): Boolean; overload;

      class function InterpretMain(s: String): TLemmixHotkeyAction;
      class function InterpretSecondary(s: String): Integer;
      class function GetKeyNames(aUseHardcoded: Boolean): TKeyNameArray;
  end;

implementation

constructor TLemmixHotkeyManager.Create;
begin
  inherited;
  LoadFile;
end;

destructor TLemmixHotkeyManager.Destroy;
begin
  inherited;
end;

procedure TLemmixHotkeyManager.ClearAllKeys;
var
  i: Integer;
begin
  for i := 0 to MAX_KEY-1 do
    fKeyFunctions[i].Action := lka_Null;
end;

procedure TLemmixHotkeyManager.SetDefaultsFunctional;
begin
  ClearAllKeys;

  // Here's the simple ones that don't need further settings.
  SetKeyFunction($08, lka_ToggleShadows);
  SetKeyFunction($53, lka_DirLeft);
  SetKeyFunction($46, lka_DirRight);
  SetKeyFunction($25, lka_DirLeft);
  SetKeyFunction($27, lka_DirRight);
  SetKeyFunction($20, lka_Pause);
  SetKeyFunction($70, lka_Restart);
  SetKeyFunction($71, lka_LoadState);
  SetKeyFunction($72, lka_SaveState);
  SetKeyFunction($34, lka_FastForward);
  SetKeyFunction($35, lka_FastForward);
  SetKeyFunction($04, lka_Pause);
  SetKeyFunction($05, lka_ZoomIn);
  SetKeyFunction($06, lka_ZoomOut);
  SetKeyFunction($1B, lka_Exit);
  SetKeyFunction($02, lka_Scroll);
  SetKeyFunction($75, lka_SaveReplay);
  SetKeyFunction($76, lka_LoadReplay);
  SetKeyFunction($11, lka_Highlight);
  SetKeyFunction($19, lka_Highlight);
  SetKeyFunction($4D, lka_Music);
  SetKeyFunction($4E, lka_Sound);
  SetKeyFunction($73, lka_ReleaseRateDown);
  SetKeyFunction($74, lka_ReleaseRateUp);
  SetKeyFunction($49, lka_FallDistance);
  SetKeyFunction($50, lka_EditReplay);
  SetKeyFunction($4F, lka_ReplayInsert);
  SetKeyFunction($0D, lka_SaveImage);
  SetKeyFunction($4A, lka_Scroll);

  // Misc ones that need other details set
  SetKeyFunction($BF, lka_ClearPhysics, 1);
  SetKeyFunction($DB, lka_SkillProjection, 1);
  SetKeyFunction($DD, lka_Projection, 1);

  // Skips
  SetKeyFunction($31, lka_Skip, -17);
  SetKeyFunction($32, lka_Skip, -1);
  SetKeyFunction($33, lka_Skip, 1);
  SetKeyFunction($36, lka_Skip, 170);
  SetKeyFunction($37, lka_SpecialSkip, 0);
  SetKeyFunction($38, lka_SpecialSkip, 1);
  SetKeyFunction($39, lka_SpecialSkip, 2);

  // Skills
  SetKeyFunction($10, lka_SkillLeft);                      // <previous skill>, shift
  SetKeyFunction($42, lka_SkillRight);                     // <next skill>, B
  SetKeyFunction($44, lka_Skill, Integer(spbWalker));      // walker, D
  SetKeyFunction($52, lka_Skill, Integer(spbJumper));      // jumper, R
  SetKeyFunction($12, lka_Skill, Integer(spbShimmier));    // shimmier, alt
  SetKeyFunction($48, lka_Skill, Integer(spbSlider));      // slider, H
  SetKeyFunction($5A, lka_Skill, Integer(spbClimber));     // climber, Z
                                                           // swimmer, <none>
  SetKeyFunction($51, lka_Skill, Integer(spbFloater));     // floater, Q
  SetKeyFunction($09, lka_Skill, Integer(spbGlider));      // glider, tab
                                                           // disarmer, <none>
  SetKeyFunction($56, lka_Skill, Integer(spbBomber));      // bomber, V
                                                           // stoner, <none>
  SetKeyFunction($58, lka_Skill, Integer(spbBlocker));     // blocker, X
  SetKeyFunction($54, lka_Skill, Integer(spbPlatformer));  // platformer, T
  SetKeyFunction($41, lka_Skill, Integer(spbBuilder));     // builder, A
                                                           // stacker, <none>
  SetKeyFunction($59, lka_Skill, Integer(spbLaserer));     // laserer, Y
  SetKeyFunction($45, lka_Skill, Integer(spbBasher));      // basher, E
  SetKeyFunction($43, lka_Skill, Integer(spbFencer));      // fencer, C
  SetKeyFunction($47, lka_Skill, Integer(spbMiner));       // miner, G
  SetKeyFunction($57, lka_Skill, Integer(spbDigger));      // digger, W
                                                           // cloner, <none>
end;

procedure TLemmixHotkeyManager.SetDefaultsMinimal;
begin
  ClearAllKeys;

  SetKeyFunction($04, lka_Pause);
  SetKeyFunction($05, lka_ZoomIn);
  SetKeyFunction($06, lka_ZoomOut);
  SetKeyFunction($02, lka_Scroll);
  SetKeyFunction($1B, lka_Exit);
end;

procedure TLemmixHotkeyManager.SetDefaultsTraditional;
begin
  ClearAllKeys;

  // Here's the simple ones that don't need further settings.
  SetKeyFunction($02, lka_Highlight);
  SetKeyFunction($04, lka_Pause);
  SetKeyFunction($05, lka_ZoomIn);
  SetKeyFunction($06, lka_ZoomOut);
  SetKeyFunction($08, lka_LoadState);
  SetKeyFunction($0D, lka_SaveState);
  SetKeyFunction($11, lka_ForceWalker);
  SetKeyFunction($19, lka_ForceWalker);
  SetKeyFunction($1B, lka_Exit);
  SetKeyFunction($25, lka_DirLeft);
  SetKeyFunction($27, lka_DirRight);
  SetKeyFunction($41, lka_Scroll);
  SetKeyFunction($43, lka_CancelReplay);
  SetKeyFunction($44, lka_FallDistance);
  SetKeyFunction($45, lka_EditReplay);
  SetKeyFunction($46, lka_FastForward);
  SetKeyFunction($48, lka_ToggleShadows);
  SetKeyFunction($49, lka_SaveImage);
  SetKeyFunction($4C, lka_LoadReplay);
  SetKeyFunction($4D, lka_Music);
  SetKeyFunction($50, lka_Pause);
  SetKeyFunction($52, lka_Restart);
  SetKeyFunction($53, lka_Sound);
  SetKeyFunction($55, lka_SaveReplay);
  SetKeyFunction($57, lka_ReplayInsert);
  SetKeyFunction($58, lka_SkillRight);
  SetKeyFunction($5A, lka_SkillLeft);
  SetKeyFunction($70, lka_ReleaseRateDown);
  SetKeyFunction($71, lka_ReleaseRateUp);
  SetKeyFunction($7A, lka_Pause);
  SetKeyFunction($7B, lka_BypassNuke);
  SetKeyFunction($C0, lka_ReleaseMouse);

  // Misc ones that need other details set
  SetKeyFunction($54, lka_ClearPhysics, 1);
  SetKeyFunction($10, lka_SkillProjection, 1);
  SetKeyFunction($12, lka_Projection, 1);

  // Here's the frameskip ones; these need a number of *frames* to skip (forwards or backwards).
  SetKeyFunction($20, lka_Skip, 17 * 10);
  SetKeyFunction($42, lka_Skip, -1);
  SetKeyFunction($4E, lka_Skip, 1);
  SetKeyFunction($6D, lka_Skip, -17);
  SetKeyFunction($BC, lka_Skip, -17 * 5);
  SetKeyFunction($BD, lka_Skip, -17);
  SetKeyFunction($BE, lka_Skip, 17 * 5);
  SetKeyFunction($DB, lka_SpecialSkip, 0);
  SetKeyFunction($DD, lka_SpecialSkip, 1);
  SetKeyFunction($DC, lka_SpecialSkip, 2);

  // And here's the skill ones; these ones need the skill specified seperately
  SetKeyFunction($09, lka_Skill, Integer(spbSlider));
  SetKeyFunction($31, lka_Skill, Integer(spbWalker));
  SetKeyFunction($32, lka_Skill, Integer(spbShimmier));
  SetKeyFunction($33, lka_Skill, Integer(spbSwimmer));
  SetKeyFunction($34, lka_Skill, Integer(spbGlider));
  SetKeyFunction($35, lka_Skill, Integer(spbDisarmer));
  SetKeyFunction($36, lka_Skill, Integer(spbStoner));
  SetKeyFunction($37, lka_Skill, Integer(spbPlatformer));
  SetKeyFunction($38, lka_Skill, Integer(spbStacker));
  SetKeyFunction($39, lka_Skill, Integer(spbFencer));
  SetKeyFunction($30, lka_Skill, Integer(spbCloner));
  SetKeyFunction($51, lka_Skill, Integer(spbLaserer));
  SetKeyFunction($72, lka_Skill, Integer(spbClimber));
  SetKeyFunction($73, lka_Skill, Integer(spbFloater));
  SetKeyFunction($74, lka_Skill, Integer(spbBomber));
  SetKeyFunction($75, lka_Skill, Integer(spbBlocker));
  SetKeyFunction($76, lka_Skill, Integer(spbBuilder));
  SetKeyFunction($77, lka_Skill, Integer(spbBasher));
  SetKeyFunction($78, lka_Skill, Integer(spbMiner));
  SetKeyFunction($79, lka_Skill, Integer(spbDigger));
  SetKeyFunction($BB, lka_Skill, Integer(spbJumper));
end;

class function TLemmixHotkeyManager.InterpretMain(s: String): TLemmixHotkeyAction;
begin
  s := LowerCase(s);
  Result := lka_Null;
  if s = 'skill' then Result := lka_Skill;
  if s = 'athlete_info' then Result := lka_ShowAthleteInfo;
  if s = 'quit' then Result := lka_Exit;
  if s = 'rr_max' then Result := lka_ReleaseRateMax;
  if s = 'rr_up' then Result := lka_ReleaseRateUp;
  if s = 'rr_down' then Result := lka_ReleaseRateDown;
  if s = 'rr_min' then Result := lka_ReleaseRateMin;
  if s = 'pause' then Result := lka_Pause;
  if s = 'nuke' then Result := lka_Nuke;
  if s = 'bypass_nuke' then Result := lka_BypassNuke;
  if s = 'save_state' then Result := lka_SaveState;
  if s = 'load_state' then Result := lka_LoadState;
  if s = 'dir_select_left' then Result := lka_DirLeft;
  if s = 'dir_select_right' then Result := lka_DirRight;
  if s = 'force_walker' then Result := lka_ForceWalker;
  if s = 'cheat' then Result := lka_Cheat;
  if s = 'skip' then Result := lka_Skip;
  if s = 'special_skip' then Result := lka_SpecialSkip;
  if s = 'fastforward' then Result := lka_FastForward;
  if s = 'slow_motion' then Result := lka_SlowMotion;
  if s = 'save_image' then Result := lka_SaveImage;
  if s = 'load_replay' then Result := lka_LoadReplay;
  if s = 'save_replay' then Result := lka_SaveReplay;
  if s = 'cancel_replay' then Result := lka_CancelReplay;
  if s = 'toggle_music' then Result := lka_Music;
  if s = 'toggle_sound' then Result := lka_Sound;
  if s = 'restart' then Result := lka_Restart;
  if s = 'previous_skill' then Result := lka_SkillLeft;
  if s = 'next_skill' then Result := lka_SkillRight;
  if s = 'release_mouse' then Result := lka_ReleaseMouse;
  if s = 'highlight' then Result := lka_Highlight;
  if s = 'clear_physics' then Result := lka_ClearPhysics;
  if s = 'toggle_shadows' then Result := lka_ToggleShadows;  
  if s = 'projection' then Result := lka_Projection;
  if s = 'skill_projection' then Result := lka_SkillProjection;
  if s = 'show_used_skills' then Result := lka_ShowUsedSkills;
  if s = 'fall_distance' then Result := lka_FallDistance;
  if s = 'edit_replay' then Result := lka_EditReplay;
  if s = 'replay_insert' then Result := lka_ReplayInsert;
  if s = 'zoom_in' then Result := lka_ZoomIn;
  if s = 'zoom_out' then Result := lka_ZoomOut;
  if s = 'scroll' then Result := lka_Scroll;
end;

class function TLemmixHotkeyManager.InterpretSecondary(s: String): Integer;
  begin
    s := LowerCase(s);

    if s = 'walker' then Result := Integer(spbWalker)
    else if s = 'jumper' then Result := Integer(spbJumper)
    else if s = 'shimmier' then Result := Integer(spbShimmier)
    else if s = 'slider' then Result := Integer(spbSlider)
    else if s = 'climber' then Result := Integer(spbClimber)
    else if s = 'swimmer' then Result := Integer(spbSwimmer)
    else if s = 'floater' then Result := Integer(spbFloater)
    else if s = 'glider' then Result := Integer(spbGlider)
    else if s = 'disarmer' then Result := Integer(spbDisarmer)
    else if s = 'bomber' then Result := Integer(spbBomber)
    else if s = 'stoner' then Result := Integer(spbStoner)
    else if s = 'blocker' then Result := Integer(spbBlocker)
    else if s = 'platformer' then Result := Integer(spbPlatformer)
    else if s = 'builder' then Result := Integer(spbBuilder)
    else if s = 'stacker' then Result := Integer(spbStacker)
    else if s = 'laserer' then Result := Integer(spbLaserer)
    else if s = 'basher' then Result := Integer(spbBasher)
    else if s = 'fencer' then Result := Integer(spbFencer)
    else if s = 'miner' then Result := Integer(spbMiner)
    else if s = 'digger' then Result := Integer(spbDigger)
    else if s = 'cloner' then Result := Integer(spbCloner)
    else if s = 'lastskill' then Result := 0
    else if s = 'nextshrug' then Result := 1
    else if s = 'highlitstate' then Result := 2
    else if s = '' then Result := 0
    else
    begin
      try
        // a lot of secondaries will be actually numeric
        Result := StrToInt(s);
      except
        Result := 0;
      end;
    end;
  end;

procedure TLemmixHotkeyManager.LoadFile;
var
  StringList: TStringList;
  i, i2: Integer;
  istr: String;
  s0, s1: String;
  FoundSplit: Boolean;
begin
  StringList := TStringList.Create;
  try
    if FileExists(AppPath + SFSaveData + 'hotkeys.ini') then
      StringList.LoadFromFile(AppPath + SFSaveData + 'hotkeys.ini')
    else if FileExists(AppPath + 'NeoLemmixHotkeys.ini') then
      StringList.LoadFromFile(AppPath + 'NeoLemmixHotkeys.ini')
    else begin
      SetDefaultsFunctional;
      Exit;
    end;
    for i := 0 to MAX_KEY do
    begin
      istr := StringList.Values[IntToHex(i, MAX_KEY_LEN)];
      if istr = '' then
      begin
        fKeyFunctions[i].Action := lka_Null;
        fKeyFunctions[i].Modifier := 0;
      end else begin
        s0 := '';
        s1 := '';
        FoundSplit := False;
        for i2 := 1 to Length(istr) do
        begin
          if istr[i2] = ':' then
          begin
            FoundSplit := True;
            Continue;
          end;
          if FoundSplit then
            s1 := s1 + istr[i2]
          else
            s0 := s0 + istr[i2];
        end;
        fKeyFunctions[i].Action := InterpretMain(s0);
        fKeyFunctions[i].Modifier := InterpretSecondary(s1);
      end;
    end;
  except
    on E: Exception do
    begin
      fDisableSaving := True;
      SetDefaultsFunctional;
      raise E;
    end;
  end;
  StringList.Free;
end;

procedure TLemmixHotkeyManager.SaveFile;
var
  StringList: TStringList;
  i: Integer;
  s: String;

  function InterpretMain(aValue: TLemmixHotkeyAction): String;
  begin
    case aValue of
      lka_Skill:            Result := 'Skill';
      lka_ShowAthleteInfo:  Result := 'Athlete_Info';
      lka_Exit:             Result := 'Quit';
      lka_ReleaseRateMax:   Result := 'RR_Max';
      lka_ReleaseRateUp:    Result := 'RR_Up';
      lka_ReleaseRateDown:  Result := 'RR_Down';
      lka_ReleaseRateMin:   Result := 'RR_Min';
      lka_Pause:            Result := 'Pause';
      lka_Nuke:             Result := 'Nuke';
      lka_BypassNuke:       Result := 'Bypass_Nuke';
      lka_SaveState:        Result := 'Save_State';
      lka_LoadState:        Result := 'Load_State';
      lka_DirLeft:          Result := 'Dir_Select_Left';
      lka_DirRight:         Result := 'Dir_Select_Right';
      lka_ForceWalker:      Result := 'Force_Walker';
      lka_Cheat:            Result := 'Cheat';
      lka_Skip:             Result := 'Skip';
      lka_SpecialSkip:      Result := 'Special_Skip';
      lka_FastForward:      Result := 'FastForward';
      lka_SlowMotion:       Result := 'Slow_Motion';
      lka_SaveImage:        Result := 'Save_Image';
      lka_LoadReplay:       Result := 'Load_Replay';
      lka_SaveReplay:       Result := 'Save_Replay';
      lka_CancelReplay:     Result := 'Cancel_Replay';
      lka_Music:            Result := 'Toggle_Music';
      lka_Sound:            Result := 'Toggle_Sound';
      lka_Restart:          Result := 'Restart';
      lka_SkillLeft:        Result := 'Previous_Skill';
      lka_SkillRight:       Result := 'Next_Skill';
      lka_ReleaseMouse:     Result := 'Release_Mouse';
      lka_Highlight:        Result := 'Highlight';
      lka_ClearPhysics:     Result := 'Clear_Physics';
      lka_ToggleShadows:    Result := 'Toggle_Shadows';
      lka_Projection:       Result := 'Projection';
      lka_SkillProjection:  Result := 'Skill_Projection';
      lka_ShowUsedSkills:   Result := 'Show_Used_Skills';
      lka_FallDistance:     Result := 'Fall_Distance';
      lka_EditReplay:       Result := 'Edit_Replay';
      lka_ReplayInsert:     Result := 'Replay_Insert';
      lka_ZoomIn:           Result := 'Zoom_In';
      lka_ZoomOut:          Result := 'Zoom_Out';
      lka_Scroll:           Result := 'Scroll';
      else Result := 'Null';
    end;
  end;

  function InterpretSecondary(aValue: Integer; aMain: TLemmixHotkeyAction): String;
  begin
    case aMain of
      lka_Skill:  case aValue of
                    Integer(spbWalker):     Result := 'Walker';
                    Integer(spbJumper):     Result := 'Jumper';
                    Integer(spbShimmier):   Result := 'Shimmier';
                    Integer(spbSlider):     Result := 'Slider';
                    Integer(spbClimber):    Result := 'Climber';
                    Integer(spbSwimmer):    Result := 'Swimmer';
                    Integer(spbFloater):    Result := 'Floater';
                    Integer(spbGlider):     Result := 'Glider';
                    Integer(spbDisarmer):   Result := 'Disarmer';
                    Integer(spbBomber):     Result := 'Bomber';
                    Integer(spbStoner):     Result := 'Stoner';
                    Integer(spbBlocker):    Result := 'Blocker';
                    Integer(spbPlatformer): Result := 'Platformer';
                    Integer(spbBuilder):    Result := 'Builder';
                    Integer(spbStacker):    Result := 'Stacker';
                    Integer(spbLaserer):    Result := 'Laserer';
                    Integer(spbBasher):     Result := 'Basher';
                    Integer(spbFencer):     Result := 'Fencer';
                    Integer(spbMiner):      Result := 'Miner';
                    Integer(spbDigger):     Result := 'Digger';
                    Integer(spbCloner):     Result := 'Cloner';
                  end;
      lka_SpecialSkip:  case aValue of
                          0: Result := 'LastSkill';
                          1: Result := 'NextShrug';
                          2: Result := 'HighlitState';
                        end;
      else Result := IntToStr(aValue);
    end;
  end;
begin
  if fDisableSaving then Exit;
  
  StringList := TStringList.Create;
  StringList.Add('Version=' + IntToStr(KEYSET_VERSION));
  for i := 0 to MAX_KEY do
  begin
    s := InterpretMain(fKeyFunctions[i].Action);
    if s = 'Null' then Continue;
    if fKeyFunctions[i].Action in [lka_Skill, lka_Skip, lka_SpecialSkip, lka_ClearPhysics, lka_Projection, lka_SkillProjection, lka_ShowUsedSkills] then
      s := s + ':' + InterpretSecondary(fKeyFunctions[i].Modifier, fKeyFunctions[i].Action);
    StringList.Add(IntToHex(i, MAX_KEY_LEN) + '=' + s);
  end;
  try
    ForceDirectories(AppPath + SFSaveData);
    StringList.SaveToFile(AppPath + SFSaveData + 'hotkeys.ini')
  finally
    StringList.Free;
  end;
end;

function TLemmixHotkeyManager.CheckKeyEffect(aKey: Word): TLemmixHotkey;
begin
  if aKey > MAX_KEY then
  begin
    Result.Action := lka_Null;
    Result.Modifier := 0;
  end else
    Result := fKeyFunctions[aKey];
end;

function TLemmixHotkeyManager.CheckForKey(aFunc: TLemmixHotkeyAction): Boolean;
begin
  Result := DoCheckForKey(aFunc, 0, False);
end;

function TLemmixHotkeyManager.CheckForKey(aFunc: TLemmixHotkeyAction; aMod: Integer): Boolean;
begin
  Result := DoCheckForKey(aFunc, aMod, True);
end;

function TLemmixHotkeyManager.DoCheckForKey(aFunc: TLemmixHotkeyAction; aMod: Integer; CheckMod: Boolean): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to MAX_KEY do
  begin
    if fKeyFunctions[i].Action <> aFunc then Continue;
    if CheckMod and (aMod <> fKeyFunctions[i].Modifier) then Continue;
    if (GetKeyState(i) < 0) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

class function TLemmixHotkeyManager.GetKeyNames(aUseHardcoded: Boolean): TKeyNameArray;
var
  i: Integer;
  P: PChar;
  ScanCode: UInt;
begin
  for i := 0 to MAX_KEY do
    Result[i] := '';

  // Too lazy to include them in an interally-included file. So I just
  // coded them in here. xD
  if aUseHardcoded then
  begin
    Result[$02] := 'Right-Click';
    Result[$04] := 'Middle-Click';
    Result[$05] := 'Wheel Up';
    Result[$06] := 'Wheel Down';
    Result[$08] := 'Backspace';
    Result[$09] := 'Tab';
    Result[$0D] := 'Enter';
    Result[$10] := 'Shift';
    Result[$11] := 'Ctrl (Left)';
    Result[$12] := 'Alt';
    Result[$13] := 'Pause';
    Result[$14] := 'Caps Lock';
    Result[$19] := 'Ctrl (Right)';
    Result[$1B] := 'Esc';
    Result[$20] := 'Space';
    Result[$21] := 'Page Up';
    Result[$22] := 'Page Down';
    Result[$23] := 'End';
    Result[$24] := 'Home';
    Result[$25] := 'Left Arrow';
    Result[$26] := 'Up Arrow';
    Result[$27] := 'Right Arrow';
    Result[$28] := 'Down Arrow';
    Result[$2D] := 'Insert';
    Result[$2E] := 'Delete';
    // Shortcut time!
    for i := 0 to 9 do
      Result[$30 + i] := IntToStr(i);
    for i := 0 to 25 do
      Result[$41 + i] := Char(i + 65);
    Result[$5B] := 'Windows';
    for i := 0 to 9 do
      Result[$60 + i] := 'NumPad ' + IntToStr(i);
    Result[$6A] := 'NumPad *';
    Result[$6B] := 'NumPad +';
    Result[$6D] := 'NumPad -';
    Result[$6E] := 'NumPad .';
    Result[$6F] := 'NumPad /';
    for i := 0 to 11 do
      Result[$70 + i] := 'F' + IntToStr(i+1);
    Result[$90] := 'NumLock';
    Result[$91] := 'Scroll Lock';
    Result[$BA] := ';';
    Result[$BB] := '+';
    Result[$BC] := ',';
    Result[$BD] := '-';
    Result[$BE] := '.';
    Result[$BF] := '/';
    Result[$C0] := '~';
    Result[$DB] := '[';
    Result[$DC] := '\';
    Result[$DD] := ']';
    Result[$DE] := '''';
  end;

  P := StrAlloc(20);
  for i := 0 to MAX_KEY do
  begin
    ScanCode := MapVirtualKeyEx(i, 0, GetKeyboardLayout(0)) shl 16;
    if (GetKeyNameText(ScanCode, P, 20) > 0) and (not aUseHardcoded) then
      Result[i] := StrPas(P)
    else if Result[i] = '' then
      Result[i] := IntToHex(i, 4);
  end;
  StrDispose(P);
end;

procedure TLemmixHotkeyManager.SetKeyFunction(aKey: Word; aFunc: TLemmixHotkeyAction; aMod: Integer = 0);
begin
  fKeyFunctions[aKey].Action := aFunc;
  fKeyFunctions[aKey].Modifier := aMod;end;

end.