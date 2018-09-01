{$include lem_directives.inc}

unit FMain;


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
      procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var DoResize: Boolean);
    private
      fActiveScreen: TBaseScreen;

      fLastUpdate: Int64;

      // old stuff
      Started: Boolean;
      AppController: TAppController;

      procedure UpdateGame(Sender: TObject; var Done: Boolean);
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

procedure TMainForm.UpdateGame(Sender: TObject; var Done: Boolean);
var
  NewTC: Int64;
  NewScreen: TBaseScreen;
begin
  if fActiveScreen = nil then
    raise Exception.Create('No active screen!');

  Done := false;

  NewTC := GetTickCount64;

  if NewTC - fLastUpdate < fActiveScreen.FrameDelay then
  begin
    Sleep(1);
    Exit;
  end;

  fActiveScreen.UpdateGame;

  if fActiveScreen.NewScreen <> fActiveScreen then
  begin
    NewScreen := fActiveScreen.NewScreen;
    fActiveScreen.Free;
    fActiveScreen := NewScreen;
  end;

  fLastUpdate := NewTC;
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
    Left := (Screen.Width - GameParams.WindowWidth) div 2;
    Top := (Screen.Height - GameParams.WindowHeight) div 2;
    ClientWidth := GameParams.WindowWidth;
    ClientHeight := GameParams.WindowHeight;
  end;
  Started := True;

  fLastUpdate := GetTickCount64;
  Application.OnIdle := @UpdateGame;

  // create main menu subform here
end;

procedure TMainForm.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var DoResize: Boolean);
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

