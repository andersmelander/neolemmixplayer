unit FEditHotkeys;

interface

uses
  LemmixHotkeys, LemCore,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

type
  TFLemmixHotkeys = class(TForm)
    lvHotkeys: TListView;
    cbFunctions: TComboBox;
    btnClose: TButton;
    cbSkill: TComboBox;
    lblSkill: TLabel;
    cbShowUnassigned: TCheckBox;
    lblDuration: TLabel;
    ebSkipDuration: TEdit;
    btnFindKey: TButton;
    Label3: TLabel;
    cbHardcodedNames: TCheckBox;
    cbHoldKey: TCheckBox;
    cbSpecialSkip: TComboBox;
    lblSkip: TLabel;
    btnFunctionalLayout: TButton;
    btnTraditionalLayout: TButton;
    btnMinimalLayout: TButton;
    procedure FormCreate(Sender: TObject);
    procedure cbShowUnassignedClick(Sender: TObject);
    procedure lvHotkeysClick(Sender: TObject);
    procedure cbFunctionsChange(Sender: TObject);
    procedure cbSkillChange(Sender: TObject);
    procedure ebSkipDurationChange(Sender: TObject);
    procedure lvHotkeysSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnFindKeyKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnFindKeyClick(Sender: TObject);
    procedure cbHardcodedNamesClick(Sender: TObject);
    procedure cbHoldKeyClick(Sender: TObject);
    procedure SetVisibleModifier(aKeyType: TLemmixHotkeyAction);
    procedure cbSpecialSkipChange(Sender: TObject);
    procedure btnFunctionalLayoutClick(Sender: TObject);
    procedure btnTraditionalLayoutClick(Sender: TObject);
    procedure btnMinimalLayoutClick(Sender: TObject);
  private
    fShownFindInfo: Boolean;
    fKeyNames: TKeyNameArray;
    fHotkeys: TLemmixHotkeyManager;
    procedure SetWindowPosition;
    procedure RefreshList;
    procedure SetHotkeys(aValue: TLemmixHotkeyManager);
    function FindKeyFromList(aValue: Integer): Integer;
  public
    property HotkeyManager: TLemmixHotkeyManager write SetHotkeys;
  end;

var
  FLemmixHotkeys: TFLemmixHotkeys;

implementation

{$R *.dfm}

procedure TFLemmixHotkeys.FormCreate(Sender: TObject);
begin
  fShownFindInfo := False;
  SetWindowPosition;
  fKeyNames := TLemmixHotkeyManager.GetKeyNames(True);
end;

procedure TFLemmixHotkeys.SetWindowPosition;
begin
  Left := (Screen.Width div 2) - (Width div 2);
  Top := (Screen.Height div 2) - (Height div 2);
end;

procedure TFLemmixHotkeys.RefreshList;
var
  i: Integer;
  e: Integer;
  Hotkey: TLemmixHotkey;
  s: String;
begin
  e := 0;
  for i := 0 to MAX_KEY do
  begin
    Hotkey := fHotkeys.CheckKeyEffect(i);
    if (Hotkey.Action = lka_Null) and not cbShowUnassigned.Checked then Continue;
    case Hotkey.Action of
      lka_Skill: begin
                   s := 'Select Skill: ';
                   case Hotkey.Modifier of
                     Integer(spbWalker):     s := s + 'Walker';
                     Integer(spbJumper):     s := s + 'Jumper';
                     Integer(spbShimmier):   s := s + 'Shimmier';
                     Integer(spbSlider):     s := s + 'Slider';
                     Integer(spbClimber):    s := s + 'Climber';
                     Integer(spbSwimmer):    s := s + 'Swimmer';
                     Integer(spbFloater):    s := s + 'Floater';
                     Integer(spbGlider):     s := s + 'Glider';
                     Integer(spbDisarmer):   s := s + 'Disarmer';
                     Integer(spbBomber):     s := s + 'Bomber';
                     Integer(spbStoner):     s := s + 'Stoner';
                     Integer(spbBlocker):    s := s + 'Blocker';
                     Integer(spbPlatformer): s := s + 'Platformer';
                     Integer(spbBuilder):    s := s + 'Builder';
                     Integer(spbStacker):    s := s + 'Stacker';
                     Integer(spbLaserer):    s := s + 'Laserer';
                     Integer(spbBasher):     s := s + 'Basher';
                     Integer(spbFencer):     s := s + 'Fencer';
                     Integer(spbMiner):      s := s + 'Miner';
                     Integer(spbDigger):     s := s + 'Digger';
                     Integer(spbCloner):     s := s + 'Cloner';
                     else s := s + '???';
                   end;
                 end;
      lka_Skip: begin
                  if Hotkey.Modifier < -1 then
                    s := 'Time Skip: Back ' + IntToStr(Hotkey.Modifier * -1) + ' Frames'
                  else if Hotkey.Modifier = -1 then
                    s := 'Time Skip: Back 1 Frame'
                  else if Hotkey.Modifier > 1 then
                    s := 'Time Skip: Forward ' + IntToStr(Hotkey.Modifier) + ' Frames'
                  else
                    s := 'Time Skip: Forward 1 Frame';
                end;
      lka_ClearPhysics: if Hotkey.Modifier = 0 then
                          s := 'Clear Physics Mode (toggle)'
                        else
                          s := 'Clear Physics Mode (hold)';
      lka_Projection: if Hotkey.Modifier = 0 then
                        s := 'Projection (toggle)'
                      else
                        s := 'Projection (hold)';
      lka_SkillProjection: if Hotkey.Modifier = 0 then
                             s := 'Skill Projection (toggle)'
                           else
                             s := 'Skill Projection (hold)';
      lka_ShowUsedSkills: if Hotkey.Modifier = 0 then
                            s := 'Show Used Skills (toggle)'
                          else
                            s := 'Show Used Skills (hold)';
      lka_SpecialSkip: begin
                         s := 'Skip to ';
                         case TSpecialSkipCondition(Hotkey.Modifier) of
                           ssc_LastAction: s := s + 'Previous Assignment';
                           ssc_NextShrugger: s := s + 'Next Shrugger';
                           ssc_HighlitStateChange: s := s + 'Highlit Lemming State Change';
                         end;
                       end;
      else s := cbFunctions.Items[Integer(fHotkeys.CheckKeyEffect(i).Action)];
    end;
    if e < lvHotkeys.Items.Count then
      with lvHotkeys.Items[e] do
      begin
        Caption := fKeyNames[i];
        SubItems[0] := s;
      end
    else
      with lvHotkeys.Items.Add do
      begin
        Caption := fKeyNames[i];
        SubItems.Add(s);
      end;
    Inc(e);
  end;
  while lvHotkeys.Items.Count > e do
    lvHotkeys.Items.Delete(e);
end;

procedure TFLemmixHotkeys.SetHotkeys(aValue: TLemmixHotkeyManager);
begin
  fHotkeys := aValue;
  RefreshList;
end;

procedure TFLemmixHotkeys.cbShowUnassignedClick(Sender: TObject);
begin
  RefreshList;
end;

procedure TFLemmixHotkeys.SetVisibleModifier(aKeyType: TLemmixHotkeyAction);
begin
  lblSkill.Visible := False;
  cbSkill.Visible := False;
  cbSkill.Enabled := False;
  lblDuration.Visible := False;
  ebSkipDuration.Visible := False;
  ebSkipDuration.Enabled := False;
  lblSkip.Visible := False;
  cbSpecialSkip.Visible := False;
  cbSpecialSkip.Enabled := False;
  cbHoldKey.Visible := False;
  cbHoldKey.Enabled := False;

  case aKeyType of
    lka_Skill: begin
                 lblSkill.Visible := True;
                 cbSkill.Visible := True;
                 cbSkill.Enabled := True;
               end;
    lka_Skip: begin
                lblDuration.Visible := True;
                ebSkipDuration.Visible := True;
                ebSkipDuration.Enabled := True;
              end;
    lka_ClearPhysics,
    lka_Projection,
    lka_SkillProjection,
    lka_ShowUsedSkills: begin
                          cbHoldKey.Visible := True;
                          cbHoldKey.Enabled := True;
                        end;
    lka_SpecialSkip: begin
                       lblSkip.Visible := True;
                       cbSpecialSkip.Visible := True;
                       cbSpecialSkip.Enabled := True;
                     end;
  end;
end;

procedure TFLemmixHotkeys.lvHotkeysClick(Sender: TObject);
var
  i: Integer;
begin
  i := FindKeyFromList(lvHotkeys.ItemIndex);
  if i = -1 then
  begin
    cbFunctions.ItemIndex := -1;
    cbSkill.ItemIndex := -1;
    ebSkipDuration.Text := '';
    cbFunctions.Enabled := False;
    cbSkill.Enabled := False;
    ebSkipDuration.Enabled := False;
    Label3.Caption := '';
    Exit;
  end;
  cbFunctions.Enabled := True;
  cbFunctions.ItemIndex := Integer(fHotkeys.CheckKeyEffect(i).Action);
  case fHotkeys.CheckKeyEffect(i).Action of
    lka_Skill: cbSkill.ItemIndex := fHotkeys.CheckKeyEffect(i).Modifier;
    lka_Skip: ebSkipDuration.Text := IntToStr(fHotkeys.CheckKeyEffect(i).Modifier);
    lka_ClearPhysics,
    lka_Projection,
    lka_SkillProjection,
    lka_ShowUsedSkills: cbHoldKey.Checked := fHotkeys.CheckKeyEffect(i).Modifier = 1;
  end;
  Label3.Caption := 'Editing key: ' + fKeyNames[i];
  cbFunctionsChange(Self);
end;

function TFLemmixHotkeys.FindKeyFromList(aValue: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  if aValue = -1 then Exit;
  for i := 0 to MAX_KEY do
    if fKeyNames[i] = lvHotkeys.Items[aValue].Caption then
    begin
      Result := i;
      Exit;
    end;
end;

procedure TFLemmixHotkeys.cbFunctionsChange(Sender: TObject);
var
  i: Integer;
begin
  i := FindKeyFromList(lvHotkeys.ItemIndex);
  if i = -1 then Exit; //safety; should never happen
  case TLemmixHotkeyAction(cbFunctions.ItemIndex) of
    lka_Skill: begin
                 if cbSkill.ItemIndex = -1 then cbSkill.ItemIndex := 0;
                 fHotkeys.SetKeyFunction(i, lka_Skill, cbSkill.ItemIndex);
               end;
    lka_Skip: begin
                ebSkipDuration.Text := IntToStr(StrToIntDef(ebSkipDuration.Text, 0)); // not redundant; destroys non-numeric values
                fHotkeys.SetKeyFunction(i, lka_Skip, StrToInt(ebSkipDuration.Text));
              end;
    lka_SpecialSkip: begin
                       if cbSpecialSkip.ItemIndex = -1 then cbSpecialSkip.ItemIndex := 0;
                       fHotkeys.SetKeyFunction(i, lka_SpecialSkip, cbSpecialSkip.ItemIndex);
                     end;
    lka_ClearPhysics,
    lka_Projection,
    lka_SkillProjection,
    lka_ShowUsedSkills: if cbHoldKey.Checked then
                          fHotkeys.SetKeyFunction(i, TLemmixHotkeyAction(cbFunctions.ItemIndex), 1)
                        else
                          fHotkeys.SetKeyFunction(i, TLemmixHotkeyAction(cbFunctions.ItemIndex), 0);
    else fHotkeys.SetKeyFunction(i, TLemmixHotkeyAction(cbFunctions.ItemIndex));
  end;
  SetVisibleModifier(TLemmixHotkeyAction(cbFunctions.ItemIndex));
  RefreshList;
end;

procedure TFLemmixHotkeys.cbSkillChange(Sender: TObject);
var
  i: Integer;
begin
  i := FindKeyFromList(lvHotkeys.ItemIndex);
  if i = -1 then Exit; //safety; should never happen
  if fHotkeys.CheckKeyEffect(i).Action <> lka_Skill then Exit;
  fHotkeys.SetKeyFunction(i, lka_Skill, cbSkill.ItemIndex);
  RefreshList;
end;

procedure TFLemmixHotkeys.ebSkipDurationChange(Sender: TObject);
var
  i: Integer;
  TextValue: Integer;
begin
  i := FindKeyFromList(lvHotkeys.ItemIndex);
  if i = -1 then Exit; //safety; should never happen
  if fHotkeys.CheckKeyEffect(i).Action <> lka_Skip then Exit;

  if not TryStrToInt(ebSkipDuration.Text, TextValue) then
  begin
    TextValue := 1;
    if ebSkipDuration.Text <> '' then ebSkipDuration.Text := '1';
  end;

  fHotkeys.SetKeyFunction(i, lka_Skip, TextValue);
  RefreshList;
end;


procedure TFLemmixHotkeys.lvHotkeysSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  // Just reuse the OnClick code.
  lvHotkeysClick(Sender);
end;

procedure TFLemmixHotkeys.btnFindKeyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
  KeyName: String;
begin
  if Key > MAX_KEY then
  begin
    ShowMessage('This key is not supported.');
    Exit;
  end;
  KeyName := fKeyNames[Key];
  for i := 0 to lvHotkeys.Items.Count-1 do
    if KeyName = lvHotkeys.Items[i].Caption then
    begin
      lvHotkeys.SetFocus;
      lvHotkeys.ItemIndex := i;
      Exit;
    end;
  if cbShowUnassigned.Checked = False then
  begin
    cbShowUnassigned.Checked := True;
    btnFindKeyKeyDown(Sender, Key, Shift);
  end else
    ShowMessage('Could not find the key.');
end;

procedure TFLemmixHotkeys.btnFunctionalLayoutClick(Sender: TObject);
begin
  fHotkeys.ClearAllKeys;
  fHotkeys.SetDefaultsFunctional;
  RefreshList;
end;

procedure TFLemmixHotkeys.btnMinimalLayoutClick(Sender: TObject);
begin
  fHotkeys.ClearAllKeys;
  fHotkeys.SetDefaultsMinimal;
  RefreshList;
end;

procedure TFLemmixHotkeys.btnTraditionalLayoutClick(Sender: TObject);
begin
  fHotkeys.ClearAllKeys;
  fHotkeys.SetDefaultsTraditional;
  RefreshList;
end;

procedure TFLemmixHotkeys.btnFindKeyClick(Sender: TObject);
begin
  if not fShownFindInfo then
  begin
    fShownFindInfo := True;
    ShowMessage('After clicking Find Key, press any key to jump to that key in the list.');
  end;
end;

procedure TFLemmixHotkeys.cbHardcodedNamesClick(Sender: TObject);
begin
  fKeyNames := TLemmixHotkeyManager.GetKeyNames(cbHardcodedNames.Checked);
  RefreshList;
end;

procedure TFLemmixHotkeys.cbHoldKeyClick(Sender: TObject);
var
  i: Integer;
begin
  i := FindKeyFromList(lvHotkeys.ItemIndex);
  if i = -1 then Exit; //safety; should never happen

  if not (fHotkeys.CheckKeyEffect(i).Action in [lka_ClearPhysics, lka_Projection, lka_SkillProjection, lka_ShowUsedSkills]) then Exit;

  if cbHoldKey.Checked then
    fHotkeys.SetKeyFunction(i, fHotkeys.CheckKeyEffect(i).Action, 1)
  else
    fHotkeys.SetKeyFunction(i, fHotkeys.CheckKeyEffect(i).Action, 0);
  RefreshList;
end;

procedure TFLemmixHotkeys.cbSpecialSkipChange(Sender: TObject);
var
  i: Integer;
begin
  i := FindKeyFromList(lvHotkeys.ItemIndex);
  if i = -1 then Exit; //safety; should never happen
  if fHotkeys.CheckKeyEffect(i).Action <> lka_SpecialSkip then Exit;
  fHotkeys.SetKeyFunction(i, lka_SpecialSkip, cbSpecialSkip.ItemIndex);
  RefreshList;
end;

end.
