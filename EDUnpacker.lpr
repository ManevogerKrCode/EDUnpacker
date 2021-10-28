program EDUnpacker;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, ED, lithtypes;

const
  APP_VERSION = 'v0.006';
  LOG_SEPARATOR1 = '------------------------------';
  LOG_SEPARATOR2 = '| ----------------------------';
  LOG_SEPARATOR3 = '| | --------------------------';
  //LOG_SEPARATOR4 = '| | | ------------------------';

type

  { TApplication }

  TApplication = class(TCustomApplication)
  protected
    m_slLog: TStringList;
    m_EDFile: TEDFile;
    m_nTreeMaxDepth: Integer;
    m_astrTreeIndents: array of string;
    procedure DoRun; override;
    procedure LogToFile(strFilename: string);
    procedure LogHeader;
    procedure LogCompressionTable;
    procedure LogPolyhedrons;
    procedure LogSurface(nIndex: Cardinal; Surface: TEDSurface);
    procedure LogNodeContainer(pContainer: PEDNodeContainer; nDepth: Integer);
    procedure LogNodeContainerTree(pContainer: PEDNodeContainer; nDepth: Integer);
    procedure LogNodeItem(pItem: PEDNodeItem);
    procedure LogProperty(nIndex: Cardinal; pProp: PEDProperty);
    procedure PrintCallback(strMsg: string);
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
    DMS := TMemoryStream.Create;

    m_EDFile := TEDFile.Create(MS, DMS);
    m_EDFile.m_funcPrintCallback := @PrintCallback;
    m_EDFile.Load;
    LogToFile(strFilename);
    m_EDFile.Free;

    if strDecFilename <> '' then
    begin
      DMS.SaveToFile(strDecFilename);
      DMS.Free;
    end;

    MS.Free;
  end;

  Terminate;
end;

procedure TApplication.LogToFile(strFilename: string);
var i: Integer;
begin
  m_slLog := TStringList.Create;
  m_slLog.Add(strFilename);

  WriteLn('- Logging header');
  LogHeader;
  WriteLn('- Logging compression table');
  LogCompressionTable;
  WriteLn('- Logging polyhedrons');
  LogPolyhedrons;

  WriteLn('- Logging node containers');
  LogNodeContainer(@m_EDFile.m_RootNodeContainer, 0);

  SetLength(m_astrTreeIndents, m_nTreeMaxDepth + 1);
  for i := 0 to m_nTreeMaxDepth do
    m_astrTreeIndents[i] := ''.PadLeft(i, #9);

  LogNodeContainerTree(@m_EDFile.m_RootNodeContainer, 0);

  m_slLog.SaveToFile('output.log');
  m_slLog.Free;
end;

procedure TApplication.LogHeader;
begin
  with m_EDFile.m_Header do
  begin
    m_slLog.Add('---- Header');
    m_slLog.Add(Format('| Version = %d', [dwVersion]));
    m_slLog.Add(Format('| Compressed = %d', [bCompressed]));
    m_slLog.Add(Format('| WorldStringLength = %d', [dwWorldStringLength]));
    m_slLog.Add('| WorldString = ' + strWorldString);
    m_slLog.Add(Format('| Dummy0 = %d', [adwDummies[0]]));
    m_slLog.Add(Format('| Dummy1 = %d', [adwDummies[1]]));
    m_slLog.Add(Format('| Dummy2 = %d', [adwDummies[2]]));
    m_slLog.Add(Format('| Dummy3 = %d', [adwDummies[3]]));
    m_slLog.Add(Format('| Dummy4 = %d', [adwDummies[4]]));
    m_slLog.Add(Format('| Dummy5 = %d', [adwDummies[5]]));
    m_slLog.Add(Format('| Dummy6 = %d', [adwDummies[6]]));
    m_slLog.Add(Format('| Dummy7 = %d', [adwDummies[7]]));
    m_slLog.Add(LOG_SEPARATOR1);
  end;
end;

procedure TApplication.LogCompressionTable;
var i: Cardinal;
begin
  if m_EDFile.m_Header.bCompressed = 1 then
  begin
    with m_EDFile.m_CompressionTable do
    begin
      m_slLog.Add('---- CompressionTable');
      m_slLog.Add(Format('| Blocks = %d', [dwBlocks]));
      m_slLog.Add(Format('| MaxDecompressedBlockSize = %d', [dwMaxDecompBlockSize]));
      if dwBlocks > 0 then
      begin

        m_slLog.Add('| CompressedBlockSizes');
        for i := 0 to dwBlocks - 1 do
        begin
          m_slLog.Add(Format('| | Block #%d = %d', [i, adwCompBlockSizes[i]]));
        end;
        m_slLog.Add(LOG_SEPARATOR2);

        m_slLog.Add('| DecompressedBlockSizes');
        for i := 0 to dwBlocks - 1 do
        begin
          m_slLog.Add(Format('| | Block #%d = %d', [i, adwDecompBlockSizes[i]]));
        end;
        m_slLog.Add(LOG_SEPARATOR2);

      end;
      m_slLog.Add(Format('| FullCompressedSize = %d', [dwFullCompSize]));
      m_slLog.Add(Format('| FullDecompressedSize = %d', [dwFullDecompSize]));
      m_slLog.Add(LOG_SEPARATOR1);
    end;
  end;
end;

procedure TApplication.LogPolyhedrons;
var i, j: Cardinal;
begin
  m_slLog.Add(Format('Polyhedrons = %d', [m_EDFile.m_dwPolyhedrons]));
  if m_EDFile.m_dwPolyhedrons > 0 then
  for i := 0 to m_EDFile.m_dwPolyhedrons - 1 do
  begin
    with m_EDFile.m_aPolyhedronList[i] do
    begin
      m_slLog.Add(Format('---- Polyhedron #%d', [i]));
      m_slLog.Add(Format('| Color = [ %d %d %d ]', [Color.r, Color.g, Color.b]));
      m_slLog.Add(Format('| Points = %d', [dwPoints]));

      if dwPoints > 0 then
      for j := 0 to dwPoints - 1 do
        m_slLog.Add(Format('| | Vertex[%d] = %s', [j, LTVectorToString(@avPointList[j])]));
      m_slLog.Add(LOG_SEPARATOR2);

      m_slLog.Add(Format('| Surfaces = %d', [dwSurfaces]));

      if dwSurfaces > 0 then
      for j := 0 to dwSurfaces - 1 do
        LogSurface(j, aSurfaceList[j]);

      m_slLog.Add(LOG_SEPARATOR1);
    end;
  end;
end;

procedure TApplication.LogSurface(nIndex: Cardinal; Surface: TEDSurface);
var strTemp: string;
    i: Cardinal;
begin
  with Surface do
  begin
    m_slLog.Add(Format('| Surface #%d', [nIndex]));
    m_slLog.Add(Format('| | Polies = %d', [dwPolies]));

    if dwPolies > 0 then
    begin
      strTemp := '| | PolyList = [';
      for i := 0 to dwPolies - 1 do
        strTemp := strTemp + ' ' + IntToStr(awPolyList[i]);
      m_slLog.Add(strTemp + ' ]');
    end;

    if m_EDFile.m_Header.dwVersion = ED_VERSION_SHOGO then
    begin
      m_slLog.Add(Format('| | UScale = %s', [FormatFloatSafe(fUScale)]));
      m_slLog.Add(Format('| | VScale = %s', [FormatFloatSafe(fVScale)]));
      m_slLog.Add(Format('| | UOffset = %s', [FormatFloatSafe(fUOffset)]));
      m_slLog.Add(Format('| | VOffset = %s', [FormatFloatSafe(fVOffset)]));
      m_slLog.Add(Format('| | Rotation = %s', [FormatFloatSafe(fRotation)]));
    end
    else
    begin
      m_slLog.Add(Format('| | O = [ %s ]', [LTVectorToString(@avOPQ[0])]));
      m_slLog.Add(Format('| | P = [ %s ]', [LTVectorToString(@avOPQ[1])]));
      m_slLog.Add(Format('| | Q = [ %s ]', [LTVectorToString(@avOPQ[2])]));
    end;

    m_slLog.Add(Format('| | StickFlag = %d', [dwStickFlag]));
    m_slLog.Add(Format('| | TextureNameLength = %d', [wTextureNameLength]));
    m_slLog.Add('| | TextureNameLength = ' + strTextureName);
    m_slLog.Add(Format('| | Flags = %d', [dwFlags]));
    m_slLog.Add(Format('| | Shade = [ %d %d %d ]', [Shade.r, Shade.g, Shade.b]));

    m_slLog.Add(Format('| | Plane = [ %s ] -> %s', [LTVectorToString(@Plane.vNormal), FormatFloatSafe(Plane.fDist)]));
    m_slLog.Add(LOG_SEPARATOR2);
  end;
end;

procedure TApplication.LogNodeContainer(pContainer: PEDNodeContainer; nDepth: Integer);
var i: Word;
begin
  with pContainer^ do
  begin
    m_slLog.Add(Format('---- NodeContainer [%s - %s]', [Item.strNodeName, Item.strNameFromProps]));
    m_slLog.Add(Format('| Children = %d', [wChildren]));

    m_slLog.Add(Format('| Type = %s', [NODE_TYPES[dwType]]));
    if dwType = NODE_BRUSH then
      m_slLog.Add(Format('| BrushIndex = %d', [dwBrushIndex]))
    else
      m_slLog.Add('| BrushIndex = -1');

    LogNodeItem(@Item);

    m_slLog.Add(LOG_SEPARATOR1);

    if wChildren > 0 then
    begin
      Inc(nDepth, 1);
      if nDepth > m_nTreeMaxDepth then
        m_nTreeMaxDepth := nDepth;
      for i := 0 to wChildren - 1 do
        LogNodeContainer(@aChildrenList[i], nDepth);
    end;
  end;
end;

procedure TApplication.LogNodeContainerTree(pContainer: PEDNodeContainer; nDepth: Integer);
var i: Word;
begin
  with pContainer^ do
  begin
    if (m_EDFile.m_Header.dwVersion = ED_VERSION_SHOGO) or (dwType = NODE_NODE) then
      m_slLog.Add(Format('%s%s [%s]', [m_astrTreeIndents[nDepth], Item.strNodeName, Item.strClassName]))
    else
      m_slLog.Add(Format('%s%s [%s]', [m_astrTreeIndents[nDepth], Item.strNameFromProps, Item.strClassName]));

    if wChildren > 0 then
    begin
      Inc(nDepth, 1);
      for i := 0 to wChildren - 1 do
        LogNodeContainerTree(@aChildrenList[i], nDepth);
    end;
  end;
end;

procedure TApplication.LogNodeItem(pItem: PEDNodeItem);
var i: Cardinal;
begin
  with pItem^ do
  begin
    m_slLog.Add('| Item');
    m_slLog.Add(Format('| | Size = %d', [wSize]));
    m_slLog.Add(Format('| | ClassNameLength = %d', [wClassNameLength]));
    m_slLog.Add('| | ClassName = ' + strClassName);
    m_slLog.Add(Format('| | Properties = %d', [dwProperties]));

    if dwProperties > 0 then
    for i := 0 to dwProperties - 1 do
      LogProperty(i, @aPropertyList[i]);

    m_slLog.Add(Format('| | Unk1 = %d', [dwUnk1]));
    m_slLog.Add(Format('| | Unk2 = %d', [dwUnk2]));
    m_slLog.Add(Format('| | NodeNameLength = %d', [wNodeNameLength]));
    m_slLog.Add('| | NodeName = ' + strNodeName);
    m_slLog.Add(LOG_SEPARATOR2);
  end;
end;

procedure TApplication.LogProperty(nIndex: Cardinal; pProp: PEDProperty);
begin
  with pProp^ do
  begin
    m_slLog.Add(Format('| | Property #%d', [nIndex]));
    m_slLog.Add(Format('| | | NameLength = %d', [wNameLength]));
    m_slLog.Add('| | | Name = ' + strName);

    if nDataType > PT_MAX then
      m_slLog.Add(Format('| | | DataType = [Unknown %d]', [nDataType]))
    else
      m_slLog.Add(Format('| | | DataType = %s', [PT_INDEX_TO_STR[nDataType]]));

    m_slLog.Add(Format('| | | Flags = %d', [dwFlags]));
    m_slLog.Add(Format('| | | DataSize = %d', [wDataSize]));
    m_slLog.Add(Format('| | | Data = %s', [m_EDFile.PropDataToString(pProp)]));
    m_slLog.Add(LOG_SEPARATOR3);
  end;
end;

procedure TApplication.PrintCallback(strMsg: string);
begin
  WriteLn(strMsg);
end;

constructor TApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TApplication.Destroy;
begin
  inherited Destroy;
  SetLength(m_astrTreeIndents, 0);
end;

var
  Application: TApplication;

begin
  Application:=TApplication.Create(nil);
  Application.Title := 'ED Unpacker';
  Application.Run;
  Application.Free;
end.

