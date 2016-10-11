unit LemNeoOnline;

{
Contains online functions. No - no multiplayer, no plans for that.
But it can download files from a server. Currently, this is only used
to obtain missing graphic sets; but hopefully in the future it'll be
expanded to directly access some kind of content database. :)
}

interface

uses
  Dialogs, LemVersion,
  URLMon, Wininet, Classes, ActiveX, Axctrls, StrUtils, SysUtils; // I can only guess why IStream and others are in the ActiveX units...

const
  NX_BASE_URL = 'http://online.neolemmix.com/';
  NX_VERSIONS_URL = NX_BASE_URL + 'version.php';
  NX_STYLES_URL   = NX_BASE_URL + 'styles.php';

type
  TNxAppType = (NxaPlayer, NxaEditor, NxaFlexi, NxaGS, NxaTalisman);

  // Core functions
  function DownloadToFile(aURL: String; aFilename: String): Boolean;
  function DownloadToStream(aURL: String; aStream: TStream): Boolean;
  function DownloadToStringList(aURL: String; aStringList: TStringList): Boolean;
  procedure CheckForStyleUpdates(Notify: Boolean = false);

  // Specialty functions
  function GetLatestNeoLemmixVersion(const aApp: TNxAppType; var aFormat, aCore, aFeature, aHotfix: Integer): Boolean;

var
  OnlineEnabled: Boolean;

implementation

uses
  LemTypes;

function GetLatestNeoLemmixVersion(const aApp: TNxAppType; var aFormat, aCore, aFeature, aHotfix: Integer): Boolean;
var
  SL: TStringList;
  TempString: String;
begin
  if not OnlineEnabled then
  begin
    Result := false;
    Exit;
  end;

  SL := TStringList.Create;
  try
    Result := DownloadToStringList(NX_VERSIONS_URL, SL);
  except
    Result := false;
    SL.Free;
    Exit;
  end;

  case aApp of
    NxaPlayer: TempString := 'game';
    NxaEditor: TempString := 'editor';
    NxaFlexi: TempString := 'flexi';
    NxaGS: TempString := 'gstool';
    NxaTalisman: TempString := 'talisman';
  end;

  TempString := SL.Values[TempString];
  if TempString = '' then
  begin
    Result := false;
    SL.Free;
    Exit;
  end;

  SL.Delimiter := '.';
  SL.DelimitedText := TempString;

  aFormat := StrToIntDef(SL[0], 0);
  aCore := StrToIntDef(SL[1], 0);
  aFeature := StrToIntDef(SL[2], 0);
  aHotfix := StrToIntDef(SL[3], 0);

  SL.Free;
end;

function DownloadToFile(aURL: String; aFilename: String): Boolean;
begin
  if not OnlineEnabled then
  begin
    Result := false;
    Exit;
  end;

  // Simple enough.
  try
    ForceDirectories(ExtractFilePath(aFilename));
    DeleteUrlCacheEntry(PChar(aURL));
    Result := UrlDownloadToFile(nil, PChar(aURL), PChar(aFilename), 0, nil) = 0;
  except
    Result := False;
  end;
end;

function DownloadToStream(aURL: String; aStream: TStream): Boolean;
var  hrResult:   HRESULT;
     ppStream:   IStream;
     statstg:    TStatStg;
     lpBuffer:   Pointer;
     dwRead:     Integer;
begin
  if not OnlineEnabled then
  begin
    Result := false;
    Exit;
  end;

  // Very complicated. I found this code (or very similar) in several places,
  // so I doubt the true original author can be found. So, thanks whoever you are.

  // Set default result
  result:=False;

  // Make sure stream is assigned
  if not(Assigned(aStream)) then exit;

  DeleteUrlCacheEntry(PChar(aURL));

  // Open blocking stream
  hrResult:=URLOpenBlockingStream(nil, PChar(aURL), ppStream, 0, nil);
  if (hrResult = S_OK) then
  begin
     // Get the stat from the IStream interface
     if (ppStream.Stat(statstg, STATFLAG_NONAME) = S_OK) then
     begin
        // Make sure size is greater than zero
        if (statstg.cbSize > 0) then
        begin
           // Allocate buffer for the read
           lpBuffer:=AllocMem(statstg.cbSize);
           // Read from the stream
           if (ppStream.Read(lpBuffer, statstg.cbSize, @dwRead) = S_OK) then
           begin
              // Write to delphi stream
              aStream.Write(lpBuffer^, dwRead);
              // Success
              result:=True;
           end;
           // Free the buffer
           FreeMem(lpBuffer);
        end;
     end;
     // Release the IStream interface
     ppStream:=nil;
  end;

end;

function DownloadToStringList(aURL: String; aStringList: TStringList): Boolean;
var
  TempStream: TMemoryStream;
begin
  if not OnlineEnabled then
  begin
    Result := false;
    Exit;
  end;
  
  // We just go via DownloadToStream for this one. Easier that way.
  TempStream := TMemoryStream.Create;
  try
    Result := DownloadToStream(aURL, TempStream);
    TempStream.Position := 0;
    aStringList.LoadFromStream(TempStream);
  finally
    TempStream.Free;
  end;
end;

procedure CheckForStyleUpdates(Notify: Boolean = false);
var
  SL: TStringList;
  UpdHist: TStringList;
  SearchRec: TSearchRec;
  StyName: String;
  ModifiedDate: Int64;
  LatestDate: Int64;
  TempStream: TMemoryStream;

  UpdateCount: Integer;
  UpdateList: String;
begin
  SL := TStringList.Create;
  UpdHist := TStringList.Create;
  TempStream := TMemoryStream.Create;
  UpdateCount := 0;
  UpdateList := '';
  try
    if FileExists(AppPath + 'styles\versions.ini') then
      UpdHist.LoadFromFile(AppPath + 'styles\versions.ini');
    if FindFirst(AppPath + 'styles\*.dat', faAnyFile, SearchRec) = 0 then
    begin
      DownloadToStringList('http://online.neolemmix.com/styles.php', SL);
      repeat
        StyName := ChangeFileExt(Lowercase(SearchRec.Name), '');
        if SL.Values[StyName] = '' then Continue;

        TempStream.Clear;

        LatestDate := StrToInt64Def(SL.Values[StyName], 0);
        if UpdHist.Values[StyName] = '' then
          ModifiedDate := 0
        else
          ModifiedDate := StrToInt64Def(UpdHist.Values[StyName], 0);

        if ModifiedDate >= LatestDate then Continue;

        DownloadToStream('http://online.neolemmix.com/' + StyName + '.dat', TempStream);
        TempStream.SaveToFile(AppPath + 'styles\' + StyName + '.dat');

        Inc(UpdateCount);
        UpdateList := UpdateList + StyName + ', ';
        if UpdateCount mod 4 = 0 then
          UpdateList := UpdateList + #13;

        UpdHist.Values[StyName] := SL.Values[StyName];
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);

      UpdHist.SaveToFile(AppPath + 'styles\versions.ini');

      if Notify then
        if UpdateCount = 0 then
          ShowMessage('No updates to styles were found.')
        else begin
          UpdateList := LeftStr(UpdateList, Length(UpdateList)-2);
          ShowMessage('Downloaded updates to ' + IntToStr(UpdateCount) + ' styles:' + #13 + UpdateList);
        end;

    end;
  finally
    SL.Free;
    UpdHist.Free;
    TempStream.Free;
  end;
end;

end.