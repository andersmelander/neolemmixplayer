unit FNeoLemmixSetup;

interface

uses
  Math,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Vcl.Imaging.pngimage, Vcl.ExtCtrls;

type
  TFNLSetup = class(TForm)
    SetupPages: TPageControl;
    TabSheet1: TTabSheet;
    lblWelcome: TLabel;
    lblOptionsText1: TLabel;
    lblOptionsText2: TLabel;
    btnNext: TButton;
    btnExit: TButton;
    lblHotkeys: TLabel;
    cbHotkey: TComboBox;
    lblGraphics: TLabel;
    cbGraphics: TComboBox;
    lblUsername: TLabel;
    ebUserName: TEdit;
    lblOnline: TLabel;
    cbOnline: TComboBox;
    imgFloater: TImage;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
  private
  end;

implementation

uses
  GameControl, LemmixHotkeys, LemCore;

{$R *.dfm}

{ Misc Functions }

procedure TFNLSetup.FormCreate(Sender: TObject);
begin
  SetupPages.TabIndex := 0;
end;

{ Page Control }

procedure TFNLSetup.btnExitClick(Sender: TObject);
begin
  GameParams.DisableSaveOptions := True;
  Application.Terminate;
end;

procedure TFNLSetup.btnOKClick(Sender: TObject);
begin
  // Set desired default settings
  GameParams.UserName := ebUserName.Text;

  case cbHotkey.ItemIndex of
    0: GameParams.Hotkeys.SetDefaultsFunctional;
    1: GameParams.Hotkeys.SetDefaultsTraditional;
    2: GameParams.Hotkeys.SetDefaultsMinimal;
  end;

  case cbGraphics.ItemIndex of
    1, 3: begin
         GameParams.MinimapHighQuality := True;
         GameParams.LinearResampleMenu := True;
         GameParams.LinearResampleGame := False;
       end;
    0, 2: begin
         GameParams.MinimapHighQuality := False;
         GameParams.LinearResampleMenu := False;
         GameParams.LinearResampleGame := False;
       end;
  end;

  GameParams.HighResolution := cbGraphics.ItemIndex >= 2;
  if GameParams.HighResolution then
    GameParams.ZoomLevel := Max(GameParams.ZoomLevel div 2, 1);

  GameParams.EnableOnline := cbOnline.ItemIndex >= 1;
  GameParams.CheckUpdates := cbOnline.ItemIndex >= 2;

  Close;
end;

end.
