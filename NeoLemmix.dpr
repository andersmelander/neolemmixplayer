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

  function GetTestModeStartName: String;
  var
    SL: TStringList;
  begin
    if FileExists(AppPath + 'settings\teststart.ini') then
    begin
      SL := TStringList.Create;
      try
        SL.LoadFromFile(AppPath + 'settings\teststart.ini');
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

  Cancel := false;
  NoSaveSettings := false; // Specifically override with true where applicable.

  if (Lowercase(ParamStr(1)) = 'convert') or
     (Lowercase(ParamStr(1)) = 'version') then
  begin
    // In these special modes, we don't need a user.
    UserName := 'Anonymous';
    NoSaveSettings := true;
  end else begin
    UserName := GetAutoStartName;

    if LowerCase(ParamStr(1)) = 'test' then
    begin
      if UserName = '' then UserName := GetTestModeStartName; // In test mode, load the Auto Test Mode user if there is one.
      if UserName = '' then UserName := 'Anonymous'; // If not, don't interrupt - set the username to Anonymous.
      NoSaveSettings := true; // Either way, disable saving data.
    end;
  end;

  if UserName = '' then
  begin
    // If we don't have a user name by this point, we ask for one.
    ProfileForm := TProfileForm.Create(Application);
    try
      case ProfileForm.ShowModal of
        mrOk: begin
                UserName := ProfileForm.StartName;
                NoSaveSettings := ProfileForm.NoSave;
              end;
        mrCancel: Cancel := true;
      end;
    finally
      ProfileForm.Free;
    end;
  end;

  if not Cancel then
  begin
    Application.CreateForm(TMainForm, MainForm);
    Application.Run;
  end
end.
