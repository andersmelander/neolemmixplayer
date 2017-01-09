unit FBaseDosForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  UMisc, Gr32,
  GameControl;

type
  {-------------------------------------------------------------------------------
    abstract black, fullscreen, ancestor form
  -------------------------------------------------------------------------------}
  TBaseDosForm = class(TForm)
  private
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure BuildScreen; virtual;
    procedure PrepareGameParams; virtual; // always call inherited
  public
    constructor Create(aOwner: TComponent); override;
    function ShowScreen: Integer; virtual;

  end;

implementation

{$R *.dfm}

{ TBaseDosForm }

procedure TBaseDosForm.BuildScreen;
begin
  //
end;



constructor TBaseDosForm.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Caption := 'NeoLemmix';
  Color := clBlack;
  BorderStyle := {bsSizeable} bsNone;
  BorderIcons := [{biSystemMenu, biMinimize, biMaximize}];
  WindowState := {wsNormal} wsMaximized;
  Cursor := crNone;
end;

procedure TBaseDosForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
  begin
    Style := Style or CS_OWNDC; // maybe faster screen output
  end;
end;

procedure TBaseDosForm.PrepareGameParams;
begin
  Caption := Trim(GameParams.SysDat.PackName);
end;

function TBaseDosForm.ShowScreen: Integer;
begin
  PrepareGameParams;
  BuildScreen;
  Result := ShowModal;
end;

end.

