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
    procedure FormCreate(Sender: TObject);
    procedure cbNameChange(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnQuitClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

  // If not in guest mode, ensure folder, import pre-profile data and set up autostart if applicable.
  if not fNoSave then
  begin
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
    end;
  end;

  ModalResult := mrOK;
end;

procedure TProfileForm.cbNameChange(Sender: TObject);
begin
  if (cbName.ItemIndex >= 0) and (cbName.Items.Objects[cbName.ItemIndex] <> nil) then
  begin
    cbAutoStart.Enabled := false;
    cbAutoStart.Checked := false;
  end else
    cbAutoStart.Enabled := true;
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
    cbAutoStart.Enabled := cbName.Items.Count > 1;
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
