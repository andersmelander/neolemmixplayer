program LevelImgGen;

uses
  Interfaces,
  LemLevel,
  LemRendering,
  LemNeoPieceManager,
  GR32_Png,
  GR32,
  Classes,
  SysUtils;

{$R data.res}

var
  Sources: TStringList;
  Level: TLevel;
  OutBmp: TBitmap32;
  Renderer: TRenderer;

  procedure LoadParamStr;
  var
    i: Integer;
  begin
    WriteLn('Loading ParamStr');
    i := 1;
    while ParamStr(i) <> '' do
    begin
      Sources.Add(ExpandFileName(ParamStr(i)));
      Inc(i);
    end;
    WriteLn('... Done');
    WriteLn('');
  end;

  procedure SetupPieceManager;
  begin
    WriteLn('Preparing piece manager');
    PieceManager := TNeoPieceManager.Create;
    WriteLn('... Done');
    WriteLn('');
  end;

  procedure ProcessLevels;
  var
    i: Integer;
  begin
    for i := 0 to Sources.Count-1 do
    begin
      WriteLn('Generating image for ' + Sources[i]);
      Level.LoadFromFile(Sources[i]);
      WriteLn('... Preparing renderer');
      Renderer.PrepareGameRendering(Level);
      WriteLn('... Preparing output image');
      OutBmp.SetSize(Level.Info.Width, Level.Info.Height);
      OutBmp.Clear($00000000);
      WriteLn('... Rendering level');
      Renderer.RenderWorld(OutBmp, true);
      WriteLn('... Saving PNG');
      SaveBitmap32ToPng(OutBmp, ChangeFileExt(Sources[i], '.png'));
      WriteLn('... Done');
      WriteLn('');
    end;
  end;

begin
  Sources := TStringList.Create;
  OutBmp := TBitmap32.Create;
  Level := TLevel.Create;
  Renderer := TRenderer.Create;
  try
    LoadParamStr;
    SetupPieceManager;
    ProcessLevels;
  finally
    Sources.Free;
    OutBmp.Free;
    Level.Free;
    Renderer.Free;
  end;
end.

