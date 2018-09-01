{$include lem_directives.inc}

unit FMain;

{$MODE Delphi}

{-------------------------------------------------------------------------------
  This is the main form which does almost nothing.
-------------------------------------------------------------------------------}

interface

uses
  FBaseScreen,

  LemSystemMessages,
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,  StdCtrls,
  FBaseDosForm,
  LemNeoLevelPack, // compile test
  LemGame,
  AppController;

type
  TMainForm = class(TBaseDosForm)
      procedure FormActivate(Sender: TObject);
      procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
    private
      fPreviousActiveScreen: TBaseScreen;
      fCurrentActiveScreen: TBaseScreen;
      Started: Boolean;
      AppController: TAppController;
    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

uses
  SharedGlobals, // debug
  Math,
  GameControl, GameBaseScreen;

{$R *.lfm}

constructor TMainForm.Create(aOwner: TComponent);
begin
  inherited;
  GlobalGame := TLemmingGame.Create(nil);
  AppController := TAppController.Create(self);
end;

destructor TMainForm.Destroy;
begin
  GlobalGame.Free;
  AppController.Free;
  inherited;
end;

(*
procedure TMainForm.PlayGame;
begin
  try
    if not AppController.Execute then
    begin
      Close;
    end else if Assigned(ChildForm.OnActivate) then
      ChildForm.OnActivate(ChildForm); 
  except
    on E: Exception do
    begin
      Application.ShowException(E);
      Close;
    end;
  end;
end;
*)

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if Started then
    Exit;
  if not GameParams.FullScreen then
  begin
    GameParams.MainForm.Left := (Screen.Width - GameParams.WindowWidth) div 2;
    GameParams.MainForm.Top := (Screen.Height - GameParams.WindowHeight) div 2;
    GameParams.MainForm.ClientWidth := GameParams.WindowWidth;
    GameParams.MainForm.ClientHeight := GameParams.WindowHeight;
  end;
  Started := True;
  MainFormHandle := Handle;
  PostMessage(Handle, LM_START, 0, 0);
end;

procedure TMainForm.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
var
  CWDiff, CHDiff: Integer;
  NewCW, NewCH: Integer;
begin
  if GameParams.FullScreen then
  begin
    NewWidth := Screen.Width;
    NewHeight := Screen.Height;
    Exit;
  end;

  CWDiff := Width - ClientWidth;
  CHDiff := Height - ClientHeight;

  NewCW := NewWidth - CWDiff;
  NewCH := NewHeight - CHDiff;

  if GameParams.CompactSkillPanel then
    NewCW := Max(320, NewCW)
  else
    NewCW := Max(416, NewCW);
  NewCH := Max(200, NewCH);

  NewCW := Min(NewCW, Screen.Width - CWDiff);
  NewCH := Min(NewCH, Screen.Height - CHDiff);

  NewWidth := NewCW + CWDiff;
  NewHeight := NewCH + CHDiff;
end;

end.
//system
