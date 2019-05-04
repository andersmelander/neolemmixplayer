{$include lem_directives.inc}

program NeoLemmix;





uses
  GameControl,
  Classes,
  LemTypes,
  SysUtils,

  Windows,
  Controls,
  LemRes,
  Forms,
  FMain in 'FMain.pas', {MainForm}
  FProfile;

{$R *.res}

var
  ProfileForm: TProfileForm;
  Cancel: Boolean;

  function GetAutoStartName: String;
  var
    SL: TStringList;
  begin
    if FileExists(AppPath + 'settings\autostart.ini') then
    begin
      SL := TStringList.Create;
      try
        SL.LoadFromFile(AppPath + 'settings\autostart.ini');
        if SL.Count > 0 then
          Result := SL[0]
        else
          Result := '';
      finally
        SL.Free;
      end;
    end else
      Result := '';
  end;

begin
  Application.Initialize;
  Application.Title := 'NeoLemmix';

  Cancel := true;

  UserName := GetAutoStartName;

  if UserName = '' then
  begin
    ProfileForm := TProfileForm.Create(Application);
    try
      case ProfileForm.ShowModal of
        mrOk: begin
                UserName := ProfileForm.StartName;
                NoSaveSettings := ProfileForm.NoSave;
                Cancel := false;
              end;
      end;
    finally
      ProfileForm.Free;
    end;
  end else
    NoSaveSettings := false;

  if not Cancel then
  begin
    Application.CreateForm(TMainForm, MainForm);
    Application.Run;
  end
end.
