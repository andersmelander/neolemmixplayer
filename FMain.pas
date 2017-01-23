{$include lem_directives.inc}

unit FMain;

{-------------------------------------------------------------------------------
  This is the main form which does almost nothing. It's black and fullscreen to
  prevent seeing the desktop when changing forms.
-------------------------------------------------------------------------------}

{ DONE : better animated objects drawing }
{ DONE : perfect level logic GUI }

{ TODO: make use of tbitmap32.drawto(dst, x, y, srcrect) }
{ TODO: make sure sounds en music can be set off before the bassmod is loaded }
{ TODO: safe load bassmod?}
{ TODO : maybe create palette class? }
{ TODO : Remove refs to kernel, when making opensource }

interface

uses
  LemSystemMessages,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,  StdCtrls,
  FBaseDosForm,
  LemGame,
  AppController;

type
  TMainForm = class(TBaseDosForm)
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    Started: Boolean;
    AppController: TAppController;
    fChildForm: TForm;
    procedure LMStart(var Msg: TMessage); message LM_START;
    procedure LMNext(var Msg: TMessage); message LM_NEXT;
    procedure LMExit(var Msg: TMessage); message LM_EXIT;
    procedure PlayGame;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property ChildForm: TForm read fChildForm write fChildForm;
  end;

var
  MainForm: TMainForm;

implementation

uses
  GameControl;

{$R *.dfm}

procedure TMainForm.LMStart(var Msg: TMessage);
begin
  //Hide;
  PlayGame;
end;

procedure TMainForm.LMNext(var Msg: TMessage);
begin
  AppController.FreeScreen;
  PlayGame;
end;

procedure TMainForm.LMExit(var Msg: TMessage);
begin
  AppController.FreeScreen;
  Close;
end;

constructor TMainForm.Create(aOwner: TComponent);
begin
  inherited;
  //ProgramSettings := TProgramSettings.Create;
  GlobalGame := TLemmingGame.Create(nil);
  AppController := TAppController.Create(self);
end;

destructor TMainForm.Destroy;
begin
  GlobalGame.Free;
  AppController.Free;
//  ProgramSettings.Free;
  inherited;
end;

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

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if Started then
    Exit;
  Started := True;
  MainFormHandle := Handle;
  PostMessage(Handle, LM_START, 0, 0);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if fChildForm = nil then Exit;
  if not Assigned(fChildForm.OnKeyDown) then Exit;
  fChildForm.OnKeyDown(Sender, Key, Shift);
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if fChildForm = nil then Exit;
  if not Assigned(fChildForm.OnKeyUp) then Exit;
  fChildForm.OnKeyUp(Sender, Key, Shift);
end;

end.
//system
