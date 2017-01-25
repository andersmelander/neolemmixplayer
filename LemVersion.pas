unit LemVersion;

// Contains constants and functions relating to version numbers.

interface

uses
  UMisc, Classes, SysUtils;

const
  FORMAT_VERSION = 10;
  CORE_VERSION = 12;
  FEATURES_VERSION = 13;
  HOTFIX_VERSION = 0;
  COMMIT_ID = '7bcdab5';  // empty string is handled, and is uppercased when needed so don't need to do manually anymore :D

function MakeVersionString(aFormat, aCore, aFeature, aHotfix: Integer): String;
function MakeVersionID(aFormat, aCore, aFeature, aHotfix: Integer): Int64;
function CurrentVersionString: String;
function CurrentVersionID: Int64;

implementation

function CurrentVersionString: String;
begin
  Result := MakeVersionString(FORMAT_VERSION, CORE_VERSION, FEATURES_VERSION, HOTFIX_VERSION);
end;

function CurrentVersionID: Int64;
begin
  Result := MakeVersionID(FORMAT_VERSION, CORE_VERSION, FEATURES_VERSION, HOTFIX_VERSION);
end;

function MakeVersionString(aFormat, aCore, aFeature, aHotfix: Integer): String;
  function NumberToLetters(aValue: Integer): String;
  var
    n: Integer;
  begin
    Result := '';
    repeat
      n := aValue mod 26;
      Result := Char(n + 64) + Result;
      aValue := aValue div 26;
    until aValue = 0;
  end;
begin
  Result := IntToStr(aFormat);
  Result := Result + '.' + LeadZeroStr(aCore, 2);     // not having it as at least two digits would honestly feel TOO weird
  Result := Result + '.' + LeadZeroStr(aFeature, 2);
  if aHotfix > 0 then
    Result := Result + '-' + NumberToLetters(aHotfix);
end;

function MakeVersionID(aFormat, aCore, aFeature, aHotfix: Integer): Int64;
begin
  Result := aFormat;
  Result := (Result * 1000) + aCore;
  Result := (Result * 1000) + aFeature;
  Result := (Result * 1000) + aHotfix;
end;

end.