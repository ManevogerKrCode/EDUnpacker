unit ED;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PasZLib, lithtypes;

const
  NODE_NODE = 0;
  NODE_BRUSH = 1;
  NODE_OBJECT = 2;
  NODE_TYPES: array[0..2] of string = ('Node', 'Brush', 'Object');

  ED_VERSION_SHOGO = 1247;
  ED_VERSION_AVP2 = 1249;

  PROP_NAME = 'Name';

type

  TPrintCallback = procedure(strMessage: string) of object;

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

  { TEDSurface }

  TEDSurface = record
    dwPolies: Cardinal;
    awPolyList: array of Word;
    Plane: TLTPlane;
    fUScale: Single;
    fVScale: Single;
    fUOffset: Single;
    fVOffset: Single;
    fRotation: Single;
    avOPQ: array[0..2] of TLTVector;
    dwStickFlag: Cardinal;
    wTextureNameLength: Word;
    strTextureName: string;
    dwFlags: Cardinal;
    Shade: TLTColor;
  end;

  PEDSurface = ^TEDSurface;

  { TEDPolyhedron }

  TEDPolyhedron = record
    Color: TLTColor;
    dwPoints: Cardinal;
    avPointList: array of TLTVector;
    dwSurfaces: Cardinal;
    aSurfaceList: array of TEDSurface;
  end;

  { TEDProperty }

  TEDProperty = record
    wNameLength: Word;
    strName: string;
    nDataType: Byte;
    dwFlags: Cardinal;
    wDataSize: Word;
    anData: array of Byte;
  end;

  PEDProperty = ^TEDProperty;

  { TEDNodeItem }

  TEDNodeItem = record
    wSize: Word;
    wClassNameLength: Word;
    strClassName: string;
    dwProperties: Cardinal;
    aPropertyList: array of TEDProperty;
    strNameFromProps: string;
    dwUnk1: Cardinal;
    dwUnk2: Cardinal;
    wNodeNameLength: Word;
    strNodeName: string;
  end;

  PEDNodeItem = ^TEDNodeItem;

  { TEDNodeContainer }

  TEDNodeContainer = record
    wChildren: Word;
    dwType: Cardinal;
    dwBrushIndex: Cardinal;
    aChildrenList: array of TEDNodeContainer;
    Item: TEDNodeItem;
  end;

  PEDNodeContainer = ^TEDNodeContainer;

  { TEDFile }

  TEDFile = class(TObject)
  public
    m_bDecompress: Boolean;
    m_Stream: TMemoryStream;
    m_DecStream: TMemoryStream;
    m_funcPrintCallback: TPrintCallback;
    m_Header: TEDHeader;
    m_CompressionTable: TEDCompressionTable;
    m_dwPolyhedrons: Cardinal;
    m_aPolyhedronList: array of TEDPolyhedron;
    m_RootNodeContainer: TEDNodeContainer;
    m_dwNodeContainers: Cardinal;
    procedure PrintCallback(strMsg: string);
    procedure ReadHeader;
    procedure ReadCompressionTable;
    procedure ReadPolyhedrons;
    procedure ReadSurface(pSurface: PEDSurface);
    procedure ReadNodeContainers(pContainer: PEDNodeContainer);
    procedure ReadNodeItem(pNodeItem: PEDNodeItem);
    procedure ReadProperty(pProp: PEDProperty);
    function FindProperty(pItem: PEDNodeItem; strName: string): Cardinal;
    procedure ReadString(var strBuffer: string; var wLength: Word);
    procedure ReadWideString(var strBuffer: string; var dwLength: Cardinal);
    function PropDataToString(pProp: PEDProperty): string;
    procedure Load;
    procedure Decompress(qwHeaderEnd: Int64);
    procedure DestroyNodeContainers(pContainer: PEDNodeContainer);
    constructor Create(MS: TMemoryStream; DMS: TMemoryStream);
    destructor Destroy; override;
  end;

implementation

{ TEDFile }

procedure TEDFile.PrintCallback(strMsg: string);
begin
  if m_funcPrintCallback <> nil then
    m_funcPrintCallback(strMsg);
end;

procedure TEDFile.ReadHeader;
begin
  with m_Header do
  begin
    m_Stream.Read(dwVersion, 4);

    if (dwVersion <> ED_VERSION_SHOGO) and (dwVersion <> ED_VERSION_AVP2) then
      PrintCallback(Format('Warning: unsupported ED version %d!', [dwVersion]));

    m_Stream.Read(bCompressed, 1);
    ReadWideString(strWorldString, dwWorldStringLength);
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

procedure TEDFile.ReadPolyhedrons;
var i, j: Cardinal;
begin
  m_Stream.Read(m_dwPolyhedrons, 4);
  if m_dwPolyhedrons > 0 then
  begin
    SetLength(m_aPolyhedronList, m_dwPolyhedrons);
    for i := 0 to m_dwPolyhedrons - 1 do
    begin
      with m_aPolyhedronList[i] do
      begin
        m_Stream.Read(Color, SizeOf(TLTColor));
        m_Stream.Read(dwPoints, 4);

        if dwPoints > 0 then
        begin
          SetLength(avPointList, dwPoints);
          m_Stream.Read(avPointList[0], dwPoints * SizeOf(TLTVector));
        end;

        m_Stream.Read(dwSurfaces, 4);
        if dwSurfaces > 0 then
        begin
          SetLength(aSurfaceList, dwSurfaces);
          for j := 0 to dwSurfaces - 1 do
            ReadSurface(@aSurfaceList[j]);
        end;

      end;
    end;
  end;
end;

procedure TEDFile.ReadSurface(pSurface: PEDSurface);
begin
  with pSurface^ do
  begin
    m_Stream.Read(dwPolies, 4);
    if dwPolies > 0 then
    begin
      SetLength(awPolyList, dwPolies);
      m_Stream.Read(awPolyList[0], dwPolies * 2);
      m_Stream.Read(Plane, SizeOf(TLTPlane));

      if m_Header.dwVersion = ED_VERSION_SHOGO then
      begin
        m_Stream.Read(fUScale, 4);
        m_Stream.Read(fVScale, 4);
        m_Stream.Read(fUOffset, 4);
        m_Stream.Read(fVOffset, 4);
        m_Stream.Read(fRotation, 4);
      end
      else
      begin
        m_Stream.Read(avOPQ[0], SizeOf(TLTVector) * 3);
      end;

      m_Stream.Read(dwStickFlag, 4);
      ReadString(strTextureName, wTextureNameLength);
      m_Stream.Read(dwFlags, 4);
      m_Stream.Read(Shade, SizeOf(TLTColor));
    end;
  end;
end;

procedure TEDFile.ReadNodeContainers(pContainer: PEDNodeContainer);
var i: Word;
begin
  with pContainer^ do
  begin
    m_Stream.Read(wChildren, 2);
    if wChildren > 0 then
    begin
      SetLength(aChildrenList, wChildren);
      for i := 0 to wChildren - 1 do
      begin
        m_Stream.Read(aChildrenList[i].dwType, 4);
        if aChildrenList[i].dwType = NODE_BRUSH then
          m_Stream.Read(aChildrenList[i].dwBrushIndex, 4);
        ReadNodeContainers(@aChildrenList[i]);
      end;
    end;

    Inc(m_dwNodeContainers, 1);
    ReadNodeItem(@Item);
  end;
end;

procedure TEDFile.ReadNodeItem(pNodeItem: PEDNodeItem);
var i: Cardinal;
    dwNameIndex: Cardinal;
begin
  with pNodeItem^ do
  begin
    m_Stream.Read(wSize, 2);

    ReadString(strClassName, wClassNameLength);
    m_Stream.Read(dwProperties, 4);

    if dwProperties > 0 then
    begin
      SetLength(aPropertyList, dwProperties);
      for i := 0 to dwProperties - 1 do
        ReadProperty(@aPropertyList[i]);

      dwNameIndex := FindProperty(pNodeItem, PROP_NAME);
      if dwNameIndex <> MaxUIntValue then
        strNameFromProps := PropDataToString(@aPropertyList[dwNameIndex]);
    end;

    m_Stream.Read(dwUnk1, 4);
    m_Stream.Read(dwUnk2, 4);

    ReadString(strNodeName, wNodeNameLength);
  end;
end;

procedure TEDFile.ReadProperty(pProp: PEDProperty);
begin
  with pProp^ do
  begin
    ReadString(strName, wNameLength);
    m_Stream.Read(nDataType, 1);
    m_Stream.Read(dwFlags, 4);
    m_Stream.Read(wDataSize, 2);
    SetLength(anData, wDataSize);
    m_Stream.Read(anData[0], wDataSize);
  end;
end;

function TEDFile.FindProperty(pItem: PEDNodeItem; strName: string): Cardinal;
var i: Cardinal;
begin
  Result := MaxUIntValue;
  with pItem^ do
  begin
    if dwProperties > 0 then
    for i := 0 to dwProperties - 1 do
    begin
      if aPropertyList[i].strName = strName then
        Exit(i);
    end;
  end;
end;

procedure TEDFile.ReadString(var strBuffer: string; var wLength: Word);
begin
  m_Stream.Read(wLength, 2);
  if wLength > 0 then
  begin
    SetLength(strBuffer, wLength);
    m_Stream.Read(strBuffer[1], wLength);
  end;
end;

procedure TEDFile.ReadWideString(var strBuffer: string; var dwLength: Cardinal);
begin
  m_Stream.Read(dwLength, 4);
  if dwLength > 0 then
  begin
    SetLength(strBuffer, dwLength);
    m_Stream.Read(strBuffer[1], dwLength);
  end;
end;

function TEDFile.PropDataToString(pProp: PEDProperty): string;
var wLen: Word;
begin
  Result := '';
  with pProp^ do
  begin
    case nDataType of
      PT_STRING:
        begin
          wLen := PWord(@anData[0])^;
          SetLength(Result, wLen);
          if wLen > 0 then
            Move(anData[2], Result[1], wLen);
        end;
      PT_VECTOR, PT_COLOR: Result := LTVectorToString(@anData[0]);
      PT_REAL: Result := FormatFloatSafe(PSingle(@anData[0])^);
      PT_FLAGS, PT_LONGINT: Result := FormatFloatSafeShort(PSingle(@anData[0])^);
      PT_BOOL: Result := IntToStr(PByte(@anData[0])^);
      PT_ROTATION: Result := LTRotationToString(@anData[0]);
      else
        begin
          Result := Format('[Unknown sized %d]', [wDataSize])
        end;
    end;
  end;
end;

procedure TEDFile.Load;
var qwHeaderEnd: Int64;
begin
  PrintCallback('- Reading header');
  ReadHeader;
  qwHeaderEnd := m_Stream.Position;

  if m_Header.bCompressed = 1 then
  begin
    PrintCallback('- Reading compression table');
    ReadCompressionTable;

    PrintCallback('- Decompressing');
    Decompress(qwHeaderEnd);
    m_Stream := m_DecStream;
    m_Stream.Seek(qwHeaderEnd, soFromBeginning);
  end;

  PrintCallback('- Reading polyhedrons');
  ReadPolyhedrons;

  PrintCallback('- Reading node containers');
  m_RootNodeContainer.dwType := 0;
  ReadNodeContainers(@m_RootNodeContainer);
end;

procedure TEDFile.Decompress(qwHeaderEnd: Int64);
var qwCompDataStart: Int64;
    DestBuffer: array of Byte = nil;
    SrcBuffer: array of Byte = nil;
    i: Cardinal;
begin
  qwCompDataStart := m_Stream.Position;
  m_Stream.Position := SizeOf(Cardinal) + SizeOf(Byte);
  m_DecStream.WriteDWord(m_Header.dwVersion);
  m_DecStream.WriteByte(0);
  m_DecStream.CopyFrom(m_Stream, qwHeaderEnd - m_Stream.Position);
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

procedure TEDFile.DestroyNodeContainers(pContainer: PEDNodeContainer);
var i: Word;
    j: Cardinal;
begin
  with pContainer^ do
  begin
    if wChildren > 0 then
    for i := 0 to wChildren - 1 do
    begin
      DestroyNodeContainers(@aChildrenList[i]);
    end;

    if Item.dwProperties > 0 then
    for j := 0 to Item.dwProperties - 1 do
      SetLength(Item.aPropertyList[j].anData, 0);

    SetLength(Item.aPropertyList, 0);
  end;
end;

constructor TEDFile.Create(MS: TMemoryStream; DMS: TMemoryStream);
begin
  m_Stream := MS;
  m_DecStream := DMS;
end;

destructor TEDFile.Destroy;
var i, j: Cardinal;
begin
  with m_CompressionTable do
  begin
    SetLength(adwCompBlockSizes, 0);
    SetLength(adwDecompBlockSizes, 0);
  end;

  if m_dwPolyhedrons > 0 then
  for i := 0 to m_dwPolyhedrons - 1 do
  begin
    with m_aPolyhedronList[i] do
    begin
      SetLength(avPointList, 0);

      if dwSurfaces > 0 then
      for j := 0 to dwSurfaces - 1 do
        SetLength(aSurfaceList[j].awPolyList, 0);

      SetLength(aSurfaceList, 0);
    end;
  end;
  SetLength(m_aPolyhedronList, 0);

  DestroyNodeContainers(@m_RootNodeContainer);
end;

end.

