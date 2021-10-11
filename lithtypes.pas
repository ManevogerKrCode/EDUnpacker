unit lithtypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const

  PT_STRING = 0;
  PT_VECTOR = 1;
  PT_COLOR = 2;
  PT_REAL = 3;
  PT_FLAGS = 4;
  PT_BOOL = 5;
  PT_LONGINT = 6;
  PT_ROTATION = 7;

  PT_INDEX_TO_STR: array[0..PT_ROTATION] of string =
    ('String', 'Vector', 'Color', 'Real', 'Flags', 'Bool', 'LongInt', 'Rotation');

type

  { TLTColor }

  TLTColor = packed record
    r: Byte;
    g: Byte;
    b: Byte;
  end;

  { TLTVector }

  TLTVector = record
    x: Single;
    y: Single;
    z: Single;
  end;

  { TLTRotation }

  TLTRotation = record
    x: Single;
    y: Single;
    z: Single;
    w: Single;
  end;

  { TLTPlane }

  TLTPlane = record
    vNormal: TLTVector;
    fDist: Single;
  end;

  PLTVector = ^TLTVector;
  PLTRotation = ^TLTRotation;

function LTVectorToString(V: PLTVector): string;
function LTRotationToString(R: PLTRotation): string;
function FormatFloatSafe(Value: Single): string;
function FormatFloatSafeShort(Value: Single): string;

implementation

function FormatFloatSafe(Format: string; Value: Extended): string;
begin
  try
    Result := FormatFloat(Format, Value);
  except
    on E: Exception do Result := FloatToStr(Value);
  end;
end;

function FormatFloatSafeShort(Value: Single): string;
begin
  Result := FormatFloatSafe('', Value);
end;

function FormatFloatSafe(Value: Single): string;
begin
  Result := FormatFloatSafe('0.000000', Value);
end;

function LTVectorToString(V: PLTVector): string;
begin
  Result := FormatFloatSafe('0.000000', V^.x) + ' ' +
           FormatFloatSafe('0.000000', V^.y) + ' ' +
           FormatFloatSafe('0.000000', V^.z);
end;

function LTRotationToString(R: PLTRotation): string;
begin
  Result := FormatFloatSafe('0.000000', R^.x) + ' ' +
           FormatFloatSafe('0.000000', R^.y) + ' ' +
           FormatFloatSafe('0.000000', R^.z) + ' ' +
            FormatFloatSafe('0.000000', R^.w);
end;

end.

