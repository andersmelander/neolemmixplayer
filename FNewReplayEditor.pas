unit FNewReplayEditor;

interface

uses
  LemReplay,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids;

type
  TNewReplayEditorForm = class(TForm)
    btnButton1: TButton;
    btnButton2: TButton;
    btnButton3: TButton;
    gbMetadata: TGroupBox;
    ebMetadataPlayerName: TEdit;
    lblMetadataPlayerName: TLabel;
    lblMetadataLevelID: TLabel;
    ebMetadataLevelID: TEdit;
    lblMetadataLevelTitle: TLabel;
    ebMetadataLevelTitle: TEdit;
    lblMetadataLevelAuthor: TLabel;
    ebMetadataLevelAuthor: TEdit;
    lblMetadataLevelPack: TLabel;
    ebMetadataLevelPack: TEdit;
    lblMetadataLevelGroup: TLabel;
    ebMetadataLevelGroup: TEdit;
    ebMetadataLevelNumber: TEdit;
    lblMetadataLevelNumber: TLabel;
    lblMetadataLevelVersion: TLabel;
    ebMetadataLevelVersion: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnButton1Click(Sender: TObject);
    procedure btnButton2Click(Sender: TObject);
    procedure btnButton3Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fButtonSpacing: Integer;
    fIngameMode: Boolean;
    fReplay: TReplay;

    procedure SetComponentPositionsAndSizings;
    procedure SetIngameMode(aValue: Boolean);
  public
    procedure ImportReplay(aSrc: TReplay);
    procedure ExportReplay(aDest: TReplay);

    property IngameMode: Boolean read fIngameMode write SetIngameMode;
  end;

implementation

{$R *.dfm}

const
  TAG_METADATA_PLAYER_NAME = 1;
  TAG_METADATA_LEVEL_ID = 2;
  TAG_METADATA_LEVEL_TITLE = 3;
  TAG_METADATA_LEVEL_AUTHOR = 4;
  TAG_METADATA_LEVEL_PACK = 5;
  TAG_METADATA_LEVEL_GROUP = 6;
  TAG_METADATA_LEVEL_NUMBER = 7;
  TAG_METADATA_LEVEL_VERSION = 8;

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
  fReplay := TReplay.Create;
  fButtonSpacing := btnButton2.Left - btnButton1.Left - btnButton1.Width;
end;

procedure TNewReplayEditorForm.FormDestroy(Sender: TObject);
begin
  fReplay.Free;
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

procedure TNewReplayEditorForm.ImportReplay(aSrc: TReplay);
var
  TempStream: TMemoryStream;
begin
  TempStream := TMemoryStream.Create;
  try
    aSrc.SaveToStream(TempStream);
    fReplay.Clear(true);
    fReplay.LoadFromStream(TempStream);
  finally
    TempStream.Free;
  end;
end;

procedure TNewReplayEditorForm.ExportReplay(aDest: TReplay);
var
  TempStream: TMemoryStream;
begin
  TempStream := TMemoryStream.Create;
  try
    fReplay.SaveToStream(TempStream);
    aDest.Clear(true);
    aDest.LoadFromStream(TempStream);
  finally
    TempStream.Free;
  end;
end;

end.
