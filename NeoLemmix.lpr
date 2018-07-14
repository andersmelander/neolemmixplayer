{$include lem_directives.inc}

program NeoLemmix;

{$MODE Delphi}





uses
  Forms, Interfaces, FMain;

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'NeoLemmix';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
