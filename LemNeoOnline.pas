unit LemNeoOnline;

interface

// If GameParams.EnableOnline is false, TDownloadThread.DownloadToFile refuses to
// run. All other internet-based functionality eventually runs through that
// function, so this is a total killswitch.

uses
  URLMon, Windows, Wininet, ActiveX, Axctrls, // Don't ask. It's just where these are.
  Classes, SysUtils;

const
  BASE_URL = 'https://www.neolemmix.com/';
  VERSION_FILE = BASE_URL + 'installer/version.php';
  STYLES_BASE_DIRECTORY = BASE_URL + 'styles/';
  STYLES_PHP_FILE = 'styles.php';

type
  TDownloadThread = class(TThread)
    private
      fSourceURL: String;
      fStream: TMemoryStream;
      fTargetStream: TStream;
      fStringList: TStringList;

      fComplete: Boolean;
      fSuccess: Boolean;
      fTerminateRequested: Boolean;

      fOnComplete: TProc;

      function DownloadToFile(aURL: String; aPath: String): Boolean;
      function DownloadToStream(aURL: String; aStream: TStream): Boolean;
    protected
      procedure Execute; override;
    public
      constructor Create(aSourceURL: String; aTargetStream: TStream); overload;
      constructor Create(aSourceURL: String; aTargetStringList: TStringList); overload;
      destructor Destroy; override;

      procedure Kill;

      property Complete: Boolean read fComplete;
      property Success: Boolean read fSuccess;
      property OnComplete: TProc read fOnComplete write fOnComplete;
  end;

  function DownloadInThread(aURL: String; aStream: TStream; aOnComplete: TProc = nil): TDownloadThread; overload;
  function DownloadInThread(aURL: String; aStringList: TStringList; aOnComplete: TProc = nil): TDownloadThread; overload;

implementation

uses
  GameControl, LemTypes;

procedure SetupDownloadThread(aThread: TDownloadThread; aOnComplete: TProc);
begin
  aThread.OnComplete := aOnComplete;
  aThread.FreeOnTerminate := Assigned(aOnComplete);
  aThread.Start;
end;

function DownloadInThread(aURL: String; aStream: TStream; aOnComplete: TProc = nil): TDownloadThread;
begin
  Result := TDownloadThread.Create(aURL, aStream);
  SetupDownloadThread(Result, aOnComplete);
end;

function DownloadInThread(aURL: String; aStringList: TStringList; aOnComplete: TProc = nil): TDownloadThread;
begin
  Result := TDownloadThread.Create(aURL, aStringList);
  SetupDownloadThread(Result, aOnComplete);
end;

{ TDownloadThread }

constructor TDownloadThread.Create(aSourceURL: String; aTargetStream: TStream);
begin
  inherited Create(true);
  FreeOnTerminate := false;
  fStream := TMemoryStream.Create;
  fSourceURL := aSourceURL;
  fTargetStream := aTargetStream;
end;

constructor TDownloadThread.Create(aSourceURL: String;
  aTargetStringList: TStringList);
begin
  inherited Create(true);
  FreeOnTerminate := false;
  fStream := TMemoryStream.Create;
  fSourceURL := aSourceURL;
  fStringList := aTargetStringList;
end;

destructor TDownloadThread.Destroy;
begin
  fStream.Free;
  inherited;
end;

procedure TDownloadThread.Execute;
var
  LoadToStringList: Boolean;
begin
  inherited;
  try
    if fTargetStream = nil then
      LoadToStringList := (fStringList <> nil)
    else
      LoadToStringList := false;

    if not DownloadToStream(fSourceURL, fStream) then
    begin
      fSuccess := false;
      fComplete := true;
      Exit;
    end;

    fStream.Position := 0;

    if LoadToStringList then
    begin
      fStringList.Clear;
      fStringList.LoadFromStream(fStream);
    end else begin
      fTargetStream.CopyFrom(fStream, fStream.Size);
    end;

    fSuccess := true;
  except
    fSuccess := false;
  end;

  fComplete := true;

  if Assigned(fOnComplete) then
    fOnComplete();
end;

procedure TDownloadThread.Kill;
begin
  fTerminateRequested := true;
  fOnComplete := nil;
end;

function TDownloadThread.DownloadToFile(aURL: string; aPath: string): Boolean;
const
  BLOCK_SIZE = 8192;
var
  InetHandle: Pointer;
  URLHandle: Pointer;
  FileHandle: Cardinal;
  BytesRead: Cardinal;
  DownloadBuffer: Pointer;
  Buffer: array [1 .. BLOCK_SIZE] of byte;
  BytesWritten: Cardinal;
begin
  if not GameParams.EnableOnline then
  begin
    Result := false;
    Exit;
  end;

  try
    InetHandle := InternetOpen(PWideChar(aURL), 0, nil, nil, 0);
    if not Assigned(InetHandle) then RaiseLastOSError;
    try
      URLHandle := InternetOpenUrl(InetHandle, PWideChar(aURL), nil, 0, 0, 0);
      if not Assigned(URLHandle) then RaiseLastOSError;
      try
        ForceDirectories(ExtractFilePath(aPath));
        FileHandle := CreateFile(PWideChar(aPath), GENERIC_WRITE, FILE_SHARE_WRITE, nil,
          CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
        if FileHandle = INVALID_HANDLE_VALUE then RaiseLastOSError;
        try
          DownloadBuffer := @Buffer;
          repeat
            if (not InternetReadFile(URLHandle, DownloadBuffer, BLOCK_SIZE, BytesRead))
               or (not WriteFile(FileHandle, DownloadBuffer^, BytesRead, BytesWritten, nil)) then
              RaiseLastOSError;
          until (BytesRead = 0) or fTerminateRequested;
        finally
          CloseHandle(FileHandle);
        end;
      finally
        InternetCloseHandle(URLHandle);
      end;
    finally
      InternetCloseHandle(InetHandle);
    end;

    Result := not fTerminateRequested;
  except
    Result := false;
  end;
end;

function TDownloadThread.DownloadToStream(aURL: String; aStream: TStream): Boolean;
var
  Filename: String;
  MS: TMemoryStream;
  OldPos: Integer;
begin
  Filename := GetTemporaryFilename;
  if DownloadToFile(aURL, Filename) then
  begin
    if aStream is TMemoryStream then
    begin
      MS := TMemoryStream(aStream);
      MS.LoadFromFile(Filename);
    end else begin
      MS := TMemoryStream.Create;
      try
        MS.LoadFromFile(Filename);
        OldPos := aStream.Position;
        aStream.CopyFrom(MS, MS.Size);
        aStream.Position := OldPos;
      finally
        MS.Free;
      end;
    end;

    Result := true;
  end else
    Result := false;
end;

end.
