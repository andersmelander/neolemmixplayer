unit FStyleManager;

interface

uses
  LemNeoOnline,
  Zip, IOUtils,
  LemTypes, DateUtils,
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ValEdit, ComCtrls, Vcl.ExtCtrls;

type
  TFManageStyles = class(TForm)
    btnExit: TButton;
    lvStyles: TListView;
    btnGetSelected: TButton;
    btnUpdateAll: TButton;
    pbDownload: TProgressBar;
    tmContinueDownload: TTimer;
    procedure btnExitClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnGetSelectedClick(Sender: TObject);
    procedure tmContinueDownloadTimer(Sender: TObject);
    procedure lvStylesCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
  private
    fClearedPieceManager: Boolean;
    fLocalList: TStringList;
    fWebList: TStringList;

    fDownloadThread: TDownloadThread;
    fDownloadStream: TMemoryStream;
    fDownloadList: TStringList;
    fDownloadIndex: Integer;

    fTimeForNextDownload: Boolean;

    procedure ClearPieceManager;
    procedure ResizeListColumns;
    procedure MakeStyleList;
    procedure SaveLocalList;

    procedure BeginDownloads;
    procedure BeginNextDownload;
    procedure EndDownloads;
    procedure ConcludeDownload;
  public
    { Public declarations }
  end;

implementation

uses
  GameControl,
  LemStrings,
  LemVersion,
  LemNeoPieceManager;

{$R *.dfm}

procedure TFManageStyles.BeginDownloads;
begin
  fDownloadIndex := 0;
  btnGetSelected.Enabled := false;
  btnUpdateAll.Enabled := false;
  btnExit.Caption := 'Cancel';
  pbDownload.Visible := true;
  tmContinueDownload.Enabled := true;
  fLocalList.Sorted := false;
  BeginNextDownload;
end;

procedure TFManageStyles.EndDownloads;
begin
  fDownloadIndex := -1;
  SaveLocalList;
  MakeStyleList;
  btnGetSelected.Enabled := true;
  btnUpdateAll.Enabled := true;
  btnExit.Caption := 'Exit';
  pbDownload.Visible := false;
  tmContinueDownload.Enabled := false;
  fLocalList.Sorted := true;
end;

procedure TFManageStyles.ConcludeDownload;
var
  Zip: TZipFile;
begin
  Zip := TZipFile.Create;
  try
    try
      fDownloadStream.Position := 0;
      Zip.Open(fDownloadStream, zmRead);
      if DirectoryExists(AppPath + SFStyles + fDownloadList[fDownloadIndex]) then
        TDirectory.Delete(AppPath + SFStyles + fDownloadList[fDownloadIndex], true);
      Zip.ExtractAll(AppPath);
      Zip.Close;
    except
      on E: Exception do
        ShowMessage(E.ClassName + ': ' + E.Message);
    end;
    try
      if PieceManager.NeedCheckStyles.IndexOf(fDownloadList[fDownloadIndex]) >= 0 then
        PieceManager.NeedCheckStyles.Delete(PieceManager.NeedCheckStyles.IndexOf(fDownloadList[fDownloadIndex]));

      fLocalList.Values[fDownloadList[fDownloadIndex]] := fWebList.Values[fDownloadList[fDownloadIndex]];
    except
      // Fail silently here.
    end;
  finally
    Zip.Free;
    Inc(fDownloadIndex);
  end;
end;

procedure TFManageStyles.BeginNextDownload;
begin
  pbDownload.Position := fDownloadIndex * pbDownload.Max div fDownloadList.Count;

  if fDownloadIndex >= fDownloadList.Count then
  begin
    EndDownloads;
    Exit;
  end;

  fDownloadStream.Clear;
  fDownloadThread := DownloadInThread(STYLES_BASE_DIRECTORY + STYLE_VERSION + fDownloadList[fDownloadIndex] + '.zip',
    fDownloadStream,
    procedure
    begin
      fDownloadThread := nil;
      fTimeForNextDownload := true;
    end
  );
end;

procedure TFManageStyles.btnExitClick(Sender: TObject);
begin
  if fDownloadIndex < 0 then
    Close
  else begin
    if fDownloadThread <> nil then
      fDownloadThread.Terminate;
    EndDownloads;
  end;
end;

procedure TFManageStyles.btnGetSelectedClick(Sender: TObject);
var
  i: Integer;
begin
  fDownloadList.Clear;
  for i := 0 to lvStyles.Items.Count-1 do
    if lvStyles.Items[i].Selected then
      fDownloadList.Add(lvStyles.Items[i].Caption);

  BeginDownloads;
end;

procedure TFManageStyles.ClearPieceManager;
begin
  if not fClearedPieceManager then
  begin
    PieceManager.Clear;
    fClearedPieceManager := true;
  end;
end;

procedure TFManageStyles.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ClearPieceManager;
  SaveLocalList;
end;

procedure TFManageStyles.FormCreate(Sender: TObject);
begin
  fLocalList := TStringList.Create;
  fWebList := TStringList.Create;
  fDownloadList := TStringList.Create;

  fLocalList.Sorted := true;
  fWebList.Sorted := true;

  fDownloadList.Sorted := true;
  fDownloadList.Duplicates := dupIgnore;

  fDownloadIndex := -1;

  fDownloadStream := TMemoryStream.Create;

  if not GameParams.EnableOnline then
  begin
    btnGetSelected.Enabled := false;
    btnUpdateAll.Enabled := false;
  end;
end;

procedure TFManageStyles.FormDestroy(Sender: TObject);
begin
  fLocalList.Free;
  fWebList.Free;
  fDownloadList.Free;
  fDownloadStream.Free;
end;

procedure TFManageStyles.FormResize(Sender: TObject);
begin
  ResizeListColumns;
end;

procedure TFManageStyles.FormShow(Sender: TObject);
begin
  ResizeListColumns;
  MakeStyleList;
end;

procedure TFManageStyles.lvStylesCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  // If it's on the "check styles list", display in red
  // If newer version is available online, display in yellow
  // If up to date, display in green
  // If non-online copy, display in purple
  // If not installed, but not on check list, display normally

  if SubItem > 1 then
    Sender.Canvas.Brush.Color := $FFFFFF
  else if PieceManager.NeedCheckStyles.IndexOf(Item.Caption) >= 0 then
    Sender.Canvas.Brush.Color := $0000C0
  else if fLocalList.Values[Item.Caption] = '-1' then
    Sender.Canvas.Brush.Color := $C000C0
  else if fLocalList.IndexOfName(Item.Caption) >= 0 then
  begin
    if StrToInt64Def(fWebList.Values[Item.Caption], 0) > StrToInt64Def(fLocalList.Values[Item.Caption], 0) then
      Sender.Canvas.Brush.Color := $00C0C0
    else
      Sender.Canvas.Brush.Color := $00C000;
  end;
end;

procedure TFManageStyles.MakeStyleList;
var
  SearchRec: TSearchRec;
  ThisStyle: String;

  StyleList: TStringList;
  n: Integer;

  NewItem: TListItem;
  NewString: String;
begin
  if FileExists(AppPath + SFSaveData + 'styletimes.ini') then
    fLocalList.LoadFromFile(AppPath + SFSaveData + 'styletimes.ini')
  else
    fLocalList.Clear;

  if FindFirst(AppPath + SFStyles + '*', faDirectory, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') or ((SearchRec.Attr and faDirectory) = 0) then Continue;

      if fLocalList.IndexOfName(SearchRec.Name) < 0 then
        fLocalList.Add(SearchRec.Name + '=-1');
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;

  fWebList.Clear;
  if GameParams.EnableOnline then
    TInternet.DownloadToStringList(STYLES_BASE_DIRECTORY + STYLE_VERSION + STYLES_PHP_FILE, fWebList);

  StyleList := TStringList.Create;
  try
    for n := 0 to fLocalList.Count-1 do
      if Trim(fLocalList.Names[n]) <> '' then
        StyleList.Add(fLocalList.Names[n]);

    for n := 0 to fWebList.Count-1 do
      if Trim(fWebList.Names[n]) <> '' then
        if StyleList.IndexOf(fWebList.Names[n]) < 0 then
          StyleList.Add(fWebList.Names[n]);

    StyleList.Sort;

    for n := 0 to PieceManager.NeedCheckStyles.Count-1 do
    begin
      ThisStyle := PieceManager.NeedCheckStyles[n];

      if StyleList.IndexOf(ThisStyle) < 0 then
        StyleList.Insert(n, ThisStyle)
      else
        StyleList.Move(StyleList.IndexOf(ThisStyle), n);
    end;

    lvStyles.Clear;

    for ThisStyle in StyleList do
    begin
      lvStyles.AddItem(ThisStyle, nil);
      NewItem := lvStyles.Items[lvStyles.Items.Count-1];

      NewString := '';
      if fLocalList.IndexOfName(ThisStyle) >= 0 then
      begin
        if fLocalList.Values[ThisStyle] = '-1' then
          NewString := 'Manual'
        else
          DateTimeToString(NewString, 'yyyy-mm-dd hh:nn', UnixToDateTime(StrToInt64Def(fLocalList.Values[ThisStyle], 0)));
      end else if PieceManager.NeedCheckStyles.IndexOf(ThisStyle) >= 0 then
        NewString := 'Missing';
      NewItem.SubItems.Add(NewString);

      NewString := '';
      if fWebList.IndexOfName(ThisStyle) >= 0 then
        DateTimeToString(NewString, 'yyyy-mm-dd hh:nn', UnixToDateTime(StrToInt64Def(fWebList.Values[ThisStyle], 0)));
      NewItem.SubItems.Add(NewString);
    end;
  finally
    StyleList.Free;
  end;
end;

procedure TFManageStyles.ResizeListColumns;
var
  BaseWidth: Integer;
begin
  BaseWidth := (lvStyles.Width - 22) * 2 div 7;

  lvStyles.Columns[0].MaxWidth := lvStyles.Width - 22 - (BaseWidth * 2);
  lvStyles.Columns[0].MinWidth := lvStyles.Columns[0].MaxWidth;
  lvStyles.Columns[0].Width := lvStyles.Columns[0].MaxWidth;

  lvStyles.Columns[1].MaxWidth := BaseWidth;
  lvStyles.Columns[1].MinWidth := lvStyles.Columns[1].MaxWidth;
  lvStyles.Columns[1].Width := lvStyles.Columns[1].MaxWidth;

  lvStyles.Columns[2].MaxWidth := BaseWidth;
  lvStyles.Columns[2].MinWidth := lvStyles.Columns[2].MaxWidth;
  lvStyles.Columns[2].Width := lvStyles.Columns[2].MaxWidth;
end;

procedure TFManageStyles.SaveLocalList;
begin
  if (fLocalList.Count > 1) or FileExists(AppPath + SFSaveData + 'styletimes.ini') then
    fLocalList.SaveToFile(AppPath + SFSaveData + 'styletimes.ini');
end;

procedure TFManageStyles.tmContinueDownloadTimer(Sender: TObject);
begin
  if fTimeForNextDownload then
  begin
    fTimeForNextDownload := false;
    ConcludeDownload;
    BeginNextDownload;
  end;
end;

end.
