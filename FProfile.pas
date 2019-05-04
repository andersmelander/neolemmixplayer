{$include lem_directives.inc}

unit FProfile;

{-------------------------------------------------------------------------------
  This is the main form which does almost nothing.
-------------------------------------------------------------------------------}

interface

uses
  LemTypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,  StdCtrls;

type
  TProfileForm = class(TForm)
    lblName: TLabel;
    cbName: TComboBox;
    cbAutoStart: TCheckBox;
    btnStart: TButton;
    btnQuit: TButton;
    cbTestStart: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure cbNameChange(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnQuitClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbAutoStartClick(Sender: TObject);
  private
    fStartName: String;
    fNoSave: Boolean;
  public
    property StartName: String read fStartName;
    property NoSave: Boolean read fNoSave;
  end;

implementation

{$R *.dfm}

procedure TProfileForm.btnQuitClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TProfileForm.btnStartClick(Sender: TObject);
var
  SL: TStringList;
begin
  fStartName := MakeSafeForFilename(cbName.Text, false);
  fNoSave := (cbName.ItemIndex >= 0) and (cbName.Items.Objects[cbName.ItemIndex] <> nil);


  if fNoSave then
  begin
    // If in guest mode, use a safe name.
    fStartName := 'Anonymous';
  end else begin
    // If not in guest mode, ensure folder, import pre-profile data and set up autostart if applicable.
    ForceDirectories(AppPath + 'settings\' + fStartName);

    if FileExists(AppPath + 'settings\settings.ini') then
      MoveFile(PChar(AppPath + 'settings\settings.ini'), PChar(AppPath + 'settings\' + fStartName + '\settings.ini'));
    if FileExists(AppPath + 'settings\hotkeys.ini') then
      MoveFile(PChar(AppPath + 'settings\hotkeys.ini'), PChar(AppPath + 'settings\' + fStartName + '\hotkeys.ini'));
    if FileExists(AppPath + 'settings\userdata.nxsv') then
      MoveFile(PChar(AppPath + 'settings\userdata.nxsv'), PChar(AppPath + 'settings\' + fStartName + '\userdata.nxsv'));

    if cbAutoStart.Checked then
    begin
      ShowMessage('To disable auto-starting as this user, delete the "autostart.ini" file in the Settings folder.');
      SL := TStringList.Create;
      try
        SL.Add(fStartName);
        SL.SaveToFile(AppPath + 'settings\autostart.ini');
      finally
        SL.Free;
      end;
    end else if cbTestStart.Checked then
    begin
      SL := TStringList.Create;
      try
        SL.Add(fStartName);
        SL.SaveToFile(AppPath + 'settings\teststart.ini');
      finally
        SL.Free;
      end;
    end;
  end;

  ModalResult := mrOK;
end;

procedure TProfileForm.cbAutoStartClick(Sender: TObject);
begin
  if cbAutoStart.Checked then
  begin
    cbTestStart.Checked := true;
    cbTestStart.Enabled := false;
  end else
    cbTestStart.Enabled := true;
end;

procedure TProfileForm.cbNameChange(Sender: TObject);
begin
  if (cbName.ItemIndex >= 0) and (cbName.Items.Objects[cbName.ItemIndex] <> nil) then
  begin
    cbAutoStart.Enabled := false;
    cbAutoStart.Checked := false;

    cbTestStart.Enabled := false;
    cbTestStart.Checked := false;
  end else begin
    cbAutoStart.Enabled := true;
    cbTestStart.Enabled := not cbAutoStart.Checked;
  end;
end;

procedure TProfileForm.FormCreate(Sender: TObject);
var
  SearchRec: TSearchRec;
  BestTimeStampName: String;
  BestTimeStamp: TDateTime;
begin
  BestTimeStampName := '';
  BestTimeStamp := 0;
  if FindFirst(AppPath + 'settings\*', faDirectory, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name = '..') or (SearchRec.Name = '.') or ((SearchRec.Attr and faDirectory) = 0) then
        Continue;

      cbName.AddItem(SearchRec.Name, nil);

      if SearchRec.TimeStamp > BestTimeStamp then
      begin
        BestTimeStampName := SearchRec.Name;
        BestTimeStamp := SearchRec.TimeStamp;
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;

  cbName.Sorted := false;

  cbName.AddItem('(Guest)', self);

  if BestTimeStampName = '' then
  begin
    cbName.ItemIndex := 0;
    cbNameChange(self);
  end else
    cbName.Text := BestTimeStampName;
end;

procedure TProfileForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    btnQuitClick(Sender);
  end;
end;

end.
