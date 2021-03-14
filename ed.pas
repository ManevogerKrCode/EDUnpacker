unit ED;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PasZLib;

type

  { TEDHeader }

  TEDHeader = record
    dwVersion: Cardinal;
    bCompressed: Byte;
    dwWorldStringLength: Cardinal;
    strWorldString: string;
    adwDummies: array[0..7] of Cardinal;
  end;

  { TEDCompressionTable }

  TEDCompressionTable = record
    dwBlocks: Cardinal;
    dwMaxDecompBlockSize: Cardinal;
    adwCompBlockSizes: array of Cardinal;
    adwDecompBlockSizes: array of Cardinal;
    dwFullCompSize: Cardinal;
    dwFullDecompSize: Cardinal;
  end;

  { TEDFile }

  TEDFile = class(TObject)
  public
    m_qwHeaderEnd: Int64;
    m_bDecompress: Boolean;
    m_Stream: TMemoryStream;
    m_DecStream: TMemoryStream;
    m_Header: TEDHeader;
    m_CompressionTable: TEDCompressionTable;
    procedure ReadHeader;
    procedure ReadCompressionTable;
    procedure Load;
    procedure Decompress;
    constructor Create(MS: TMemoryStream; DMS: TMemoryStream);
    destructor Destroy; override;
  end;

implementation

{ TEDFile }

procedure TEDFile.ReadHeader;
begin
  with m_Header do
  begin
    m_Stream.Read(dwVersion, 4);
    m_Stream.Read(bCompressed, 1);
    m_Stream.Read(dwWorldStringLength, 4);
    SetLength(strWorldString, dwWorldStringLength);
    m_Stream.Read(strWorldString[1], dwWorldStringLength);
    m_Stream.Read(adwDummies[0], SizeOf(Cardinal) * 8);
  end;
end;

procedure TEDFile.ReadCompressionTable;
var
  i: Cardinal;
begin
  with m_CompressionTable do
  begin
    m_Stream.Read(dwBlocks, 4);
    m_Stream.Read(dwMaxDecompBlockSize, 4);

    if dwBlocks > 0 then
    begin
      SetLength(adwCompBlockSizes, dwBlocks);
      SetLength(adwDecompBlockSizes, dwBlocks);
      m_Stream.Read(adwCompBlockSizes[0], SizeOf(Cardinal) * dwBlocks);
      m_Stream.Read(adwDecompBlockSizes[0], SizeOf(Cardinal) * dwBlocks);

      for i := 0 to dwBlocks - 1 do
      begin
        dwFullCompSize := dwFullCompSize + adwCompBlockSizes[i];
        dwFullDecompSize := dwFullDecompSize + adwDecompBlockSizes[i];
      end;
    end;

  end;
end;

procedure TEDFile.Load;
begin
  ReadHeader;
  m_qwHeaderEnd := m_Stream.Position;
  ReadCompressionTable;

  if m_DecStream <> nil then
    Decompress;
end;

procedure TEDFile.Decompress;
var qwCompDataStart: Int64;
    DestBuffer: array of Byte = nil;
    SrcBuffer: array of Byte = nil;
    i: Cardinal;
begin
  qwCompDataStart := m_Stream.Position;
  m_Stream.Position := SizeOf(Cardinal) + SizeOf(Byte);
  m_DecStream.WriteDWord(m_Header.dwVersion);
  m_DecStream.WriteByte(0);
  m_DecStream.CopyFrom(m_Stream, m_qwHeaderEnd - m_Stream.Position);
  m_Stream.Position := qwCompDataStart;

  if m_CompressionTable.dwBlocks > 0 then
  begin
    for i := 0 to m_CompressionTable.dwBlocks - 1 do
    begin
      SetLength(SrcBuffer, m_CompressionTable.adwCompBlockSizes[i]);
      SetLength(DestBuffer, m_CompressionTable.adwDecompBlockSizes[i]);

      m_Stream.Read(SrcBuffer[0], m_CompressionTable.adwCompBlockSizes[i]);
      uncompress(@DestBuffer[0], m_CompressionTable.adwDecompBlockSizes[i], @SrcBuffer[0], m_CompressionTable.adwCompBlockSizes[i]);
      m_DecStream.Write(DestBuffer[0], m_CompressionTable.adwDecompBlockSizes[i]);

      SetLength(SrcBuffer, 0);
      SetLength(DestBuffer, 0);
    end;
  end;

end;

constructor TEDFile.Create(MS: TMemoryStream; DMS: TMemoryStream);
begin
  m_Stream := MS;
  m_DecStream := DMS;
end;

destructor TEDFile.Destroy;
begin
  with m_CompressionTable do
  begin
    SetLength(adwCompBlockSizes, 0);
    SetLength(adwDecompBlockSizes, 0);
  end;
end;

end.

