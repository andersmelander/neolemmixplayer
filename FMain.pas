{$include lem_directives.inc}

unit FMain;


{-------------------------------------------------------------------------------
  This is the main form which does almost nothing.
-------------------------------------------------------------------------------}

interface

uses
  FBaseScreen,
  GameControl,
  GameSound,
  LemNeoPieceManager,

  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,  StdCtrls,
  FBaseDosForm,
  LemGame,
  Types;

type

  { TMainForm }

  TMainForm = class(TBaseDosForm)
      procedure FormActivate(Sender: TObject);
      procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var DoResize: Boolean);
      procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);
      procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
        );
      procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);
      procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
        MousePos: TPoint; var Handled: Boolean);
      procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
        MousePos: TPoint; var Handled: Boolean);
    private
      fActiveScreen: TBaseScreen;

      fLastUpdate: Int64;

      // old stuff
      Started: Boolean;

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
  GameBaseScreen;

{$R *.lfm}

constructor TMainForm.Create(aOwner: TComponent);
begin
  inherited;
  GlobalGame := TLemmingGame.Create(nil);

  SoundManager := TSoundManager.Create;
  SoundManager.LoadDefaultSounds;

  GameParams := TDosGameParams.Create;
  PieceManager := TNeoPieceManager.Create;

  GameParams.MainForm := self;
end;

destructor TMainForm.Destroy;
begin
  GlobalGame.Free;
  GameParams.Free;
  SoundManager.Free;
  PieceManager.Free;
  inherited;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if Started then
    Exit;

  GameParams.Load;

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

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  fActiveScreen.OnKeyChange(Key, true);
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  fActiveScreen.OnKeyChange(Key, false);
end;

procedure TMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fActiveScreen.OnMouseButtonChange(Button, true, Point(X, Y));
end;

procedure TMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  fActiveScreen.OnMouseMove(Point(X, Y));
end;

procedure TMainForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fActiveScreen.OnMouseButtonChange(Button, false, Point(X, Y));
end;

procedure TMainForm.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  fActiveScreen.OnMouseWheel(MOUSE_WHEEL_DIR_DOWN);
end;

procedure TMainForm.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  fActiveScreen.OnMouseWheel(MOUSE_WHEEL_DIR_UP);
end;

end.

