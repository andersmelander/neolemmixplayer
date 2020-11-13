unit CountBMP32;

interface

uses
  SharedGlobals,
  Classes, SysUtils,
  GR32;

type
  TCountBitmap32 = class(TBitmap32)
    public
      constructor Create; override;
      destructor Destroy; override;

      class function GetCount: Integer;
  end;

  procedure LogImages(aCaption: String);

implementation

var
  BMPCount: Integer;

procedure LogImages(aCaption: String);
begin
  Log('  ' + aCaption + ': ' + IntToStr(TCountBitmap32.GetCount));
end;

{ TCountBitmap32 }

constructor TCountBitmap32.Create;
begin
  inherited;
  Inc(BMPCount);
end;

destructor TCountBitmap32.Destroy;
begin
  Dec(BMPCount);
  inherited;
end;

class function TCountBitmap32.GetCount: Integer;
begin
  Result := BMPCount;
end;

end.
