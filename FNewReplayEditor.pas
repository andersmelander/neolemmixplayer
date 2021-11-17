unit FNewReplayEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids;

type
  TNewReplayEditorForm = class(TForm)
    btnButton1: TButton;
    btnButton2: TButton;
    btnButton3: TButton;
    gbMetadata: TGroupBox;
    ebMetadataPlaceholder: TEdit;
    lblMetadataPlaceholder: TLabel;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    Label4: TLabel;
    Edit4: TEdit;
    Label5: TLabel;
    Edit5: TEdit;
    Edit6: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    Edit7: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnButton1Click(Sender: TObject);
    procedure btnButton2Click(Sender: TObject);
    procedure btnButton3Click(Sender: TObject);
  private
    fButtonSpacing: Integer;
    fIngameMode: Boolean;

    procedure SetComponentPositionsAndSizings;
    procedure SetIngameMode(aValue: Boolean);
  public
    property IngameMode: Boolean read fIngameMode write SetIngameMode;
  end;

implementation

{$R *.dfm}

procedure TNewReplayEditorForm.btnButton1Click(Sender: TObject);
begin
  // Standalone mode: "Save"
  // Ingame mode: "Save As"
end;

procedure TNewReplayEditorForm.btnButton2Click(Sender: TObject);
begin
  // Standalone mode: "Save As"
  // Ingame mode: "OK"
end;

procedure TNewReplayEditorForm.btnButton3Click(Sender: TObject);
begin
  // Standalone mode: "Exit"
  // Ingame mode: "Cancel"
  ModalResult := mrCancel;
end;

procedure TNewReplayEditorForm.FormCreate(Sender: TObject);
begin
  fButtonSpacing := btnButton2.Left - btnButton1.Left - btnButton1.Width;
end;

procedure TNewReplayEditorForm.FormShow(Sender: TObject);
begin
  SetComponentPositionsAndSizings;
end;

procedure TNewReplayEditorForm.SetComponentPositionsAndSizings;
begin
  btnButton1.Left := (ClientWidth - (btnButton1.Width * 3) - (fButtonSpacing * 2)) div 2;
  btnButton2.Left := btnButton1.Left + btnButton1.Width + fButtonSpacing;
  btnButton3.Left := btnButton2.Left + btnButton2.Width + fButtonSpacing;
end;

procedure TNewReplayEditorForm.SetIngameMode(aValue: Boolean);
begin
  fIngameMode := aValue;

  if fIngameMode then
  begin
    btnButton1.Caption := 'Save As';
    btnButton2.Caption := 'OK';
    btnButton3.Caption := 'Cancel';
  end else begin
    btnButton1.Caption := 'Save';
    btnButton2.Caption := 'Save As';
    btnButton3.Caption := 'Exit';
  end;
end;

end.
