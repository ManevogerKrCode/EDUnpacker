program EDUnpacker;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, ED;

const
  APP_VERSION = 'v0.003';

type

  { TApplication }

  TApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure LogToFile(EDFile: TEDFile; strFilename: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TApplication }

procedure TApplication.DoRun;
var strFilename: string;
    strDecFilename: string;
    MS: TMemoryStream;
    DMS: TMemoryStream;
    EDFile: TEDFile;
begin
  strFilename := GetOptionValue('f', '');

  if strFilename = '' then
  begin
    WriteLn('ED Unpacker ', APP_VERSION);
    WriteLn('Usage: EDUnpacker.exe -f InputFile.dat');
    WriteLn('Options:');
    WriteLn('   -f: Input ED world file. Versions 1247, 1249 is supported (Shogo, AVP2).');
    WriteLn('   -d: Output decompressed ED world file.');
  end
  else
  begin
    MS := TMemoryStream.Create;
    MS.LoadFromFile(strFilename);

    strDecFilename := GetOptionValue('d', '');
    if strDecFilename <> '' then
      DMS := TMemoryStream.Create;


    EDFile := TEDFile.Create(MS, DMS);
    EDFile.Load;
    LogToFile(EDFile, strFilename);
    EDFile.Free;

    if DMS <> nil then
    begin
      DMS.SaveToFile(strDecFilename);
      DMS.Free;
    end;

    MS.Free;
  end;

  Terminate;
end;

procedure TApplication.LogToFile(EDFile: TEDFile; strFilename: string);
var SL: TStringList;
  i: Cardinal;
begin
  SL := TStringList.Create;

  with EDFile.m_Header do
  begin
    SL.Add(strFilename);
    SL.Add('---- Header:');
    SL.Add('| Version = ' + IntToStr(dwVersion));
    SL.Add('| Compressed = ' + IntToStr(bCompressed));
    SL.Add('| WorldStringLength = ' + IntToStr(dwWorldStringLength));
    SL.Add('| WorldString = ' + strWorldString);
    SL.Add('| Dummy0 = ' + IntToStr(adwDummies[0]));
    SL.Add('| Dummy1 = ' + IntToStr(adwDummies[1]));
    SL.Add('| Dummy2 = ' + IntToStr(adwDummies[2]));
    SL.Add('| Dummy3 = ' + IntToStr(adwDummies[3]));
    SL.Add('| Dummy4 = ' + IntToStr(adwDummies[4]));
    SL.Add('| Dummy5 = ' + IntToStr(adwDummies[5]));
    SL.Add('| Dummy6 = ' + IntToStr(adwDummies[6]));
    SL.Add('| Dummy7 = ' + IntToStr(adwDummies[7]));
    SL.Add('------------------------------');
  end;

  with EDFile.m_CompressionTable do
  begin
    SL.Add('---- CompressionTable:');
    SL.Add('| Blocks = ' + IntToStr(dwBlocks));
    SL.Add('| MaxDecompressedBlockSize = ' + IntToStr(dwMaxDecompBlockSize));
    if dwBlocks > 0 then
    begin

      SL.Add('| CompressedBlockSizes');
      for i := 0 to dwBlocks - 1 do
      begin
        SL.Add('| | Block #' + IntToStr(i) + ' = '+ IntToStr(adwCompBlockSizes[i]));
      end;

      SL.Add('| DecompressedBlockSizes');
      for i := 0 to dwBlocks - 1 do
      begin
        SL.Add('| | Block #' + IntToStr(i) + ' = '+ IntToStr(adwDecompBlockSizes[i]));
      end;
    end;
    SL.Add('| FullCompressedSize = ' + IntToStr(dwFullCompSize));
    SL.Add('| FullDecompressedSize = ' + IntToStr(dwFullDecompSize));
    SL.Add('------------------------------');
  end;

  SL.SaveToFile('output.log');
  SL.Free;
end;

constructor TApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TApplication.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TApplication;
begin
  Application:=TApplication.Create(nil);
  Application.Title:='ED Unpacker';
  Application.Run;
  Application.Free;
end.

