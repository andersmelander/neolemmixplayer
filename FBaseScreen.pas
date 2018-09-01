unit FBaseScreen;

// Lazarus-friendly ancestor form for all subscreens.

interface

uses
  GR32, GR32_Image,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

type

  { TBaseScreen }

  TBaseScreen = class(TForm)
    private
      fNewScreen: TBaseScreen;
      fDelay: Integer;
      fBlockAllInput: Boolean;
    protected
      function PrepareImg32(const aLeft, aTop, aWidth, aHeight: Integer): TImage32;

      procedure Initialize; virtual;
      procedure Finalize; virtual;
    public
      constructor Create(TheOwner: TComponent); override; final;
      destructor Destroy; override; final;

      procedure UpdateGame; virtual;

      procedure OnMouseMove(const aPosition: TPoint); virtual;
      procedure OnMouseButtonChange(const aButton: TMouseButton; const aState: Boolean; const aPosition: TPoint); virtual;
      procedure OnMouseWheel(const aDirection: Integer); virtual;
      procedure OnKeyChange(const aKey: Word; const aState: Boolean); virtual;

      property NewScreen: TBaseScreen read fNewScreen write fNewScreen;
      property FrameDelay: Integer read fDelay write fDelay;
      property BlockAllInput: Boolean read fBlockAllInput write fBlockAllInput;
  end;

const
  MOUSE_WHEEL_DIR_UP = -1;
  MOUSE_WHEEL_DIR_NONE = 0; // probably never needed but just in case
  MOUSE_WHEEL_DIR_DOWN = 1;

  DEFAULT_FRAME_DELAY = 60;

implementation

{$R *.lfm}

constructor TBaseScreen.Create(TheOwner: TComponent);
begin
  inherited;
  if TheOwner is TForm then
    BoundsRect := TForm(TheOwner).BoundsRect
  else
    raise Exception.Create(ClassName + ' needs a TForm owner');
  fNewScreen := self;
  fDelay := DEFAULT_FRAME_DELAY;
  Initialize;
end;

destructor TBaseScreen.Destroy;
begin
  Finalize;
  inherited;
end;

function TBaseScreen.PrepareImg32(const aLeft, aTop, aWidth, aHeight: Integer): TImage32;
begin
  Result := TImage32.Create(self);
  Result.Parent := self;
  Result.BoundsRect := Rect(aLeft, aTop, aLeft+aWidth, aTop+aHeight);
  Result.Anchors := [akLeft, akTop, akRight, akBottom];
  Result.BitmapAlign := baCenter;
  Result.ScaleMode := TScaleMode.smScale;
end;

procedure TBaseScreen.Initialize;
begin
  // Intentionally blank
end;

procedure TBaseScreen.Finalize;
begin
  // Intentionally blank
end;

procedure TBaseScreen.UpdateGame;
begin
  // Intentionally blank
end;

procedure TBaseScreen.OnMouseMove(const aPosition: TPoint);
begin
  // Intentionally blank
end;

procedure TBaseScreen.OnMouseButtonChange(const aButton: TMouseButton; const aState: Boolean; const aPosition: TPoint);
begin
  // Intentionally blank
end;

procedure TBaseScreen.OnMouseWheel(const aDirection: Integer);
begin
  // Intentionally blank
end;

procedure TBaseScreen.OnKeyChange(const aKey: Word; const aState: Boolean);
begin
  // Intentionally blank
end;

end.

