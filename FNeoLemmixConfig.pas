unit FNeoLemmixConfig;

interface

uses
  GameControl, GameSound, FEditHotkeys, LemDosStyle, LemNeoOnline, LemTypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

type
  TFormNXConfig = class(TForm)
    NXConfigPages: TPageControl;
    TabSheet1: TTabSheet;
    btnOK: TButton;
    btnCancel: TButton;
    btnApply: TButton;
    GroupBox4: TGroupBox;
    cbAutoSaveReplay: TCheckBox;
    Label2: TLabel;
    cbReplayNaming: TComboBox;
    cbExplicitCancel: TCheckBox;
    cbNoAutoReplay: TCheckBox;
    TabSheet4: TTabSheet;
    tbSoundVol: TTrackBar;
    Label3: TLabel;
    Label5: TLabel;
    tbMusicVol: TTrackBar;
    Label6: TLabel;
    Label7: TLabel;
    cbSuccessJingle: TCheckBox;
    cbFailureJingle: TCheckBox;
    cbAutoUpdateStyles: TCheckBox;
    btnUpdateStyles: TButton;
    btnForceRedownload: TButton;
    TabSheet5: TTabSheet;
    GroupBox2: TGroupBox;
    btnHotkeys: TButton;
    cbPauseAfterBackwards: TCheckBox;
    GroupBox3: TGroupBox;
    cbLemmingBlink: TCheckBox;
    cbTimerBlink: TCheckBox;
    cbBlackOut: TCheckBox;
    cbNoBackgrounds: TCheckBox;
    GroupBox1: TGroupBox;
    cbEnableOnline: TCheckBox;
    cbUpdateCheck: TCheckBox;
    cbDisableShadows: TCheckBox;
    GroupBox6: TGroupBox;
    cbZoom: TComboBox;
    Label1: TLabel;
    cbLinearResampleMenu: TCheckBox;
    cbLinearResampleGame: TCheckBox;
    cbFullScreen: TCheckBox;
    cbMinimapHighQuality: TCheckBox;
    cbIncreaseZoom: TCheckBox;
    btnDownloadAll: TButton;
    procedure btnApplyClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnHotkeysClick(Sender: TObject);
    procedure OptionChanged(Sender: TObject);
    procedure cbEnableOnlineClick(Sender: TObject);
    procedure SliderChange(Sender: TObject);
    procedure btnUpdateStylesClick(Sender: TObject);
    procedure btnForceRedownloadClick(Sender: TObject);
    procedure btnDownloadAllClick(Sender: TObject);
  private
    procedure SetFromParams;
    procedure SaveToParams;
  public
    procedure SetGameParams;
  end;

var
  FormNXConfig: TFormNXConfig;

implementation

uses
  GameWindow; // for EXTRA_ZOOM_LEVELS constant

{$R *.dfm}

procedure TFormNXConfig.SetGameParams;
begin
  SetFromParams;
end;

procedure TFormNXConfig.btnApplyClick(Sender: TObject);
begin
  SaveToParams;
end;

procedure TFormNXConfig.btnOKClick(Sender: TObject);
begin
  SaveToParams;
  ModalResult := mrOK;
end;

procedure TFormNXConfig.SetFromParams;
var
  i: Integer;
begin

  //// Page 1 (Global Options) ////
  // Checkboxes
  cbAutoSaveReplay.Checked := GameParams.AutoSaveReplay;
  cbExplicitCancel.Checked := GameParams.ExplicitCancel;
  cbNoAutoReplay.Checked := GameParams.NoAutoReplayMode;

  cbUpdateCheck.Checked := GameParams.CheckUpdates; // in reverse order as the next one may override this
  cbAutoUpdateStyles.Checked := GameParams.UpdateStyles;
  cbEnableOnline.Checked := OnlineEnabled;

  // Replay Naming Dropdown
  if GameParams.AutoReplayNames = false then
    i := 3 // Manual naming
  else if GameParams.AlwaysTimestamp then
    i := 2
  else if GameParams.ConfirmOverwrite then
    i := 1
  else
    i := 0;
  cbReplayNaming.ItemIndex := i;

  //// Page 2 (Interface Options) ////
  // Checkboxes
  cbPauseAfterBackwards.Checked := GameParams.PauseAfterBackwardsSkip;

  cbLemmingBlink.Checked := GameParams.LemmingBlink;
  cbTimerBlink.Checked := GameParams.TimerBlink;
  cbBlackOut.Checked := GameParams.BlackOutZero;
  cbNoBackgrounds.Checked := GameParams.NoBackgrounds;
  cbDisableShadows.Checked := GameParams.NoShadows;

  cbFullScreen.Checked := GameParams.FullScreen;
  cbIncreaseZoom.Checked := GameParams.IncreaseZoom;
  cbLinearResampleMenu.Checked := GameParams.LinearResampleMenu;
  cbLinearResampleGame.Checked := GameParams.LinearResampleGame;
  cbMinimapHighQuality.Checked := GameParams.MinimapHighQuality;

  // Zoom Dropdown
  cbZoom.Items.Clear;
  i := 1;
  while ((i - EXTRA_ZOOM_LEVELS) * 320 <= Screen.Width) and ((i - EXTRA_ZOOM_LEVELS) * 200 < Screen.Height) do
  begin
    cbZoom.Items.Add(IntToStr(i) + 'x Zoom');
    Inc(i);
  end;
  cbZoom.ItemIndex := GameParams.ZoomLevel-1;

  //// Page 3 (Audio Options) ////
  if SoundManager.MuteSound then
    tbSoundVol.Position := 0
  else
    tbSoundVol.Position := SoundManager.SoundVolume;
  if SoundManager.MuteMusic then
    tbMusicVol.Position := 0
  else
    tbMusicVol.Position := SoundManager.MusicVolume;

  cbSuccessJingle.Checked := GameParams.PostLevelVictorySound;
  cbFailureJingle.Checked := GameParams.PostLevelFailureSound;

  btnApply.Enabled := false;
end;

procedure TFormNXConfig.SaveToParams;
begin

  //// Page 1 (Global Options) ////
  // Checkboxes
  OnlineEnabled := cbEnableOnline.Checked;
  GameParams.CheckUpdates := cbUpdateCheck.Checked;
  GameParams.UpdateStyles := cbAutoUpdateStyles.Checked;

  GameParams.AutoSaveReplay := cbAutoSaveReplay.Checked;
  GameParams.ExplicitCancel := cbExplicitCancel.Checked;
  GameParams.NoAutoReplayMode := cbNoAutoReplay.Checked;

  // Replay Naming Dropdown
  case cbReplayNaming.ItemIndex of
    0: begin
         GameParams.AutoReplayNames := true;
         GameParams.AlwaysTimestamp := false;
         GameParams.ConfirmOverwrite := false;
       end;
    1: begin
         GameParams.AutoReplayNames := true;
         GameParams.AlwaysTimestamp := false;
         GameParams.ConfirmOverwrite := true;
       end;
    2: begin
         GameParams.AutoReplayNames := true;
         GameParams.AlwaysTimestamp := true;
         GameParams.ConfirmOverwrite := false;
       end;
    3: begin
         GameParams.AutoReplayNames := false;
         GameParams.AlwaysTimestamp := false;
         GameParams.ConfirmOverwrite := false;
       end;
  end;

  //// Page 2 (Interface Options) ////
  // Checkboxes
  GameParams.PauseAfterBackwardsSkip := cbPauseAfterBackwards.Checked;

  GameParams.LemmingBlink := cbLemmingBlink.Checked;
  GameParams.TimerBlink := cbTimerBlink.Checked;
  GameParams.BlackOutZero := cbBlackOut.Checked;
  GameParams.NoBackgrounds := cbNoBackgrounds.Checked;
  GameParams.NoShadows := cbDisableShadows.Checked;

  GameParams.FullScreen := cbFullScreen.Checked;
  GameParams.IncreaseZoom := cbIncreaseZoom.Checked;
  GameParams.LinearResampleMenu := cbLinearResampleMenu.Checked;
  GameParams.LinearResampleGame := cbLinearResampleGame.Checked;
  GameParams.MinimapHighQuality := cbMinimapHighQuality.Checked;

  // Zoom Dropdown
  GameParams.ZoomLevel := cbZoom.ItemIndex + 1;

  //// Page 3 (Audio Options) ////
  SoundManager.MuteSound := tbSoundVol.Position = 0;
  if tbSoundVol.Position <> 0 then
    SoundManager.SoundVolume := tbSoundVol.Position;
  SoundManager.MuteMusic := tbMusicVol.Position = 0;
  if tbMusicVol.Position <> 0 then
    SoundManager.MusicVolume := tbMusicVol.Position;
    
  GameParams.PostLevelVictorySound := cbSuccessJingle.Checked;
  GameParams.PostLevelFailureSound := cbFailureJingle.Checked;

  btnApply.Enabled := false;
end;

procedure TFormNXConfig.btnHotkeysClick(Sender: TObject);
var
  HotkeyForm: TFLemmixHotkeys;
begin
  HotkeyForm := TFLemmixHotkeys.Create(self);
  HotkeyForm.HotkeyManager := GameParams.Hotkeys;
  HotkeyForm.ShowModal;
  HotkeyForm.Free;
end;

procedure TFormNXConfig.OptionChanged(Sender: TObject);
begin
  btnApply.Enabled := true;
end;

procedure TFormNXConfig.cbEnableOnlineClick(Sender: TObject);
begin
  cbUpdateCheck.Enabled := cbEnableOnline.Checked;
  cbAutoUpdateStyles.Enabled := cbEnableOnline.Checked;
  if not cbEnableOnline.Checked then
  begin
    cbUpdateCheck.Checked := false;
    cbAutoUpdateStyles.Checked := false;
  end;
  btnApply.Enabled := true;
end;

procedure TFormNXConfig.SliderChange(Sender: TObject);
begin
  btnApply.Enabled := true;
end;

procedure TFormNXConfig.btnUpdateStylesClick(Sender: TObject);
var
  OldEnableOnline: Boolean;
begin
  OldEnableOnline := OnlineEnabled;
  try
    if not cbEnableOnline.Checked then
      if MessageDlg('You currently have online functionality disabled. Do you want to' + #13 + 'temporarily enable it to check for style updates?', mtCustom, [mbYes, mbNo], 0) = mrNo then
        Exit;
    OnlineEnabled := true;
    CheckForStyleUpdates(true);
  finally
    OnlineEnabled := OldEnableOnline;
  end;
end;

procedure TFormNXConfig.btnForceRedownloadClick(Sender: TObject);
var
  OldEnableOnline: Boolean;
  
  procedure GenerateJunkVersionINI;
  var
    SearchRec: TSearchRec;
    SL: TStringList;
  begin
    // Replaces version.ini with one that marks all styles as out of date
    SL := TStringList.Create;
    try
      ForceDirectories(AppPath + 'styles\');
      if FindFirst(AppPath + 'styles\*.dat', faAnyFile, SearchRec) = 0 then
      begin
        repeat
          SL.Add(Lowercase(ExtractFileName(ChangeFileExt(SearchRec.Name, ''))) + '=0');
        until FindNext(SearchRec) <> 0;
        FindClose(SearchRec);
      end;
      SL.SaveToFile(AppPath + 'styles\versions.ini');
    finally
      SL.Free;
    end;
  end;
begin
  OldEnableOnline := OnlineEnabled;
  try
    if not cbEnableOnline.Checked then
      if MessageDlg('You currently have online functionality disabled. Do you want to' + #13 + 'temporarily enable it to check for style updates?', mtCustom, [mbYes, mbNo], 0) = mrNo then
        Exit;
    OnlineEnabled := true;
    GenerateJunkVersionINI;
    CheckForStyleUpdates(true);
  finally
    OnlineEnabled := OldEnableOnline;
  end;
end;

procedure TFormNXConfig.btnDownloadAllClick(Sender: TObject);
var
  SL, VerSL: TStringList;
  i: Integer;
  OldEnableOnline: Boolean;

  GSName: String;
begin
  SL := TStringList.Create;
  VerSL := TStringList.Create;
  OldEnableOnline := OnlineEnabled;
  try
    if not cbEnableOnline.Checked then
      if MessageDlg('You currently have online functionality disabled. Do you want to' + #13 + 'temporarily enable it to download styles?', mtCustom, [mbYes, mbNo], 0) = mrNo then
        Exit;
    OnlineEnabled := true;

    ForceDirectories(AppPath + 'styles\');
    if FileExists(AppPath + 'styles\versions.ini') then
      VerSL.LoadFromFile(AppPath + 'styles\versions.ini');

    DownloadToStringList(NX_STYLES_URL, SL);

    for i := 0 to SL.Count-1 do
      if Pos('=', SL[i]) > 0 then
      begin
        GSName := SL.Names[i];

        if FileExists(AppPath + 'styles\' + GSName + '.dat') and (VerSL.Values[GSName] <= SL.Values[GSName]) then
          Continue;

        DownloadToFile(NX_STYLES_FOLDER + GSName + '.dat', AppPath + 'styles\' + GSName + '.dat');
        VerSL.Values[GSName] := SL.Values[GSName];
      end;

    VerSL.SaveToFile(AppPath + 'styles\versions.ini');
  finally
    SL.Free;
    VerSL.Free;
    OnlineEnabled := OldEnableOnline;
  end;
end;

end.
