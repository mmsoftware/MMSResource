{-----------------------------------------------------------------------------
 Unit Name: MMSFileUnits
 Author:    bijy
 Date:      2012-11-26
 Purpose:   文件操作单元
 History:
 [2012-11-26]
   1、增加获取磁盘剩余空间的大小；
   2、文件创建、修改、最后访问时间；
-----------------------------------------------------------------------------}

unit MMSFileUnits;

interface
uses
  Forms, Classes, SysUtils, ztvUnZip, ztvBase, ztvFindFile,ztvZip, ztvZipTV, ZipFileUnit,
  ShellAPI, Windows, WinInet, WinSock, MD5;

  function GetExePath: string;
  //写日志文件
  procedure WriteLog(const APath, Astr: string);
  function IsDirNotation(const AName: string): Boolean;
  procedure FindFiles(const AFileType, APath: string; AFileList: TStrings);
  procedure GetCurFiles(const APath : string; var AFileList: TStrings);
  procedure DelFilesTorecycleBin(AFileDir : string);
  function DeleteDirDirect(APath: String): Boolean;//删除所有文件
  procedure ZipFileName(AOldName, ANewName : string); //压缩文件
  procedure ZipFile(AFileDir, AFileName : string); //压缩文件夹
  procedure UnZipFile(AFileName, AFileDir: string; Adel : Boolean = False);
  function GetVersion(const AFileName: string): string;//获得软件的版本
  function CompareVersion(const AVer1, AVer2: string): Integer;//比较两个版本的大小
  function IsConnected(const AURL: string): Boolean;//检查网络连接状态
  function CheckStrIsIP(const AValue: string) : Boolean;//检查ip是否合法
  function GetHostIP: string;//获得本机的IP地址
  function FileMD5(const AFile: string): string;//获得文件的MD5值
  function GetMD5(Buffer: Pointer; BufSize: Integer): string;
  procedure CopyDirectory(const ASourceDir, ATargetDir: string);//文件夹复制
  function DiskFreeSize(const ADiskName: string): Int64;
  //获取文件信息
  function GetFileCreateTime(const AFilename: String): string;//文件创建时间
  function GetFileLastWriteTime(const AFilename: String): string;//文件最后修改时间
  function GetFileLastAccessTime(const AFilename: String): string;//文件最后访问时间

implementation

function GetExePath: string;
begin
  Result := ExtractFilePath(Application.ExeName);
end;


{-----------------------------------------------------------------------------
  作者: bijy 2012-09-23
  参数: 
  返回: 
  功能：记录日志，将所有异常的日志全部记下来
-----------------------------------------------------------------------------}
procedure WriteLog(const APath, Astr: string);
var
  oList: TStringList;
begin
  if (APath = '') or (Astr = '') then Exit;

  oList := TStringList.Create; //创建List
  try
    if not FileExists(APath) then  //不存在文件则创建
      FileCreate(APath);
    oList.LoadFromFile(APath);  //数据读入到List
    oList.Add(DateTimeToStr(Now) + '  ' + Astr);  //追加文本到List
    oList.SaveToFile(APath);  //保存
  finally
    FreeAndNil(oList);
  end;
end;
{-----------------------------------------------------------------------------
  作者: bijy 2012-09-07
  参数: 
  返回: 
  功能：判断特殊字符
-----------------------------------------------------------------------------}
function IsDirNotation(const AName: string): Boolean;
begin
  Result := (AName = '.') or (AName = '..');
end;


{-----------------------------------------------------------------------------
  作者: bijy 2012-09-07
  参数: 
  返回: 
  功能：查找制定路径下的文件
-----------------------------------------------------------------------------}
procedure FindFiles(const AFileType, APath: string; AFileList: TStrings);
var
  strType,
  strPath: string;
  cSearchRec: TSearchRec;
begin
  Assert(AFileList <> nil);

  strPath := IncludeTrailingPathDelimiter(APath);
  if AFileType = '' then
    strType := '*.*'
  else strType := AFileType;
  if FindFirst(strPath + strType, faAnyFile and (not faDirectory), cSearchRec) = 0 then
  repeat
    if cSearchRec.Name <> '' then
      AFileList.Add(strPath + cSearchRec.Name);
  until FindNext(cSearchRec) <> 0;
  SysUtils.FindClose(cSearchRec);
  if FindFirst(strPath + '*.*', faDirectory, cSearchRec) = 0 then
  repeat
    if (cSearchRec.Name <> '') and not IsDirNotation(cSearchRec.Name) then
      FindFiles(strType, strPath + cSearchRec.Name, AFileList);
  until FindNext(cSearchRec) <> 0;
  SysUtils.FindClose(cSearchRec);
end;
{-----------------------------------------------------------------------------
  作者: bijy 2012-09-06
  参数:
  返回: 
  功能：获得所有当前文件夹下面的文件
-----------------------------------------------------------------------------}
procedure GetCurFiles(const APath : string; var AFileList: TStrings);
var
  strPath: string;
  cSearchRec : TsearchRec;
begin
  Assert(AFileList <> nil);

  strPath := IncludeTrailingPathDelimiter(APath);
  if FindFirst(strPath + '*.*', faAnyFile and (not faDirectory), cSearchRec) = 0 then
  repeat
    if cSearchRec.Name <> '' then
      AFileList.Add(strPath + cSearchRec.Name);
  until FindNext(cSearchRec) <> 0;
  SysUtils.FindClose(cSearchRec);
end;

//////////////////////////////////////////////////////////////////////
// 创建：Roube  2012.五月.07
// 接替：Roube
// 功能：删除文件到回收站，注意有提示操作
// 参数：文件路径
//////////////////////////////////////////////////////////////////////
procedure DelFilesTorecycleBin(AFileDir : string);
var
  T : TSHFileOpStruct;
begin
  with T do
  begin
    Wnd := 0;
    wFunc := FO_DELETE;
    pFrom := PChar(AFileDir);
    pTo := nil;
    fFlags := FOF_ALLOWUNDO + FOF_NOERRORUI;
    hNameMappings := nil;
    lpszProgressTitle := '正在删除文件夹';
    fAnyOperationsAborted := True;
  end;
  SHFileOperation(T);
end;

{-----------------------------------------------------------------------------
  作者: bijy 2012-09-06
  参数: 
  返回: 
  功能：删除所有的文件
-----------------------------------------------------------------------------}
function DeleteDirDirect(APath: String): Boolean;
var
  strPath,
  strFile: string;
  cSearchRec: TSearchRec;
  nAttr: Integer;
begin
  Result := True;
  if not DirectoryExists(APath) then
    Exit;
  strPath := IncludeTrailingPathDelimiter(APath);
  if FindFirst(strPath + '*.*', faAnyFile, cSearchRec) = 0 then
  repeat
    if (cSearchRec.Name = '') or IsDirNotation(cSearchRec.Name) then
      Continue;
    strFile := strPath + cSearchRec.Name;
    nAttr := cSearchRec.Attr;
    if nAttr and faDirectory <> 0 then { 递归删除目录 }
      Result := DeleteDirDirect(strFile)
    else begin
      if nAttr and faReadOnly <> 0 then { 去除只读属性 }
      begin
        nAttr := nAttr and not faReadOnly;
        SetFileAttributes(PChar(strFile), nAttr);
      end;
      if not SysUtils.DeleteFile(strFile) then
        Result := False;
    end;
  until FindNext(cSearchRec) <> 0;
  SysUtils.FindClose(cSearchRec);
  if Result then Result := SysUtils.RemoveDir(strPath);
end;
//var
//  sr : TSearchRec;
//  sPath, sFile : string;
//begin
//  //检查目录名后面是否有'\'
//  if Copy(AFileDir,Length(AFileDir),1)<>'\'then
//    sPath:=AFileDir+'\'
//  else
//    sPath:=AFileDir;
//  //------------------------------------------------------------------
//  if FindFirst(sPath+'*.*',faAnyFile,sr)=0 then
//  begin
//    repeat
//      sFile:=Trim(sr.Name);
//      if sFile='.' then Continue;
//      if sFile='..' then Continue;
//      sFile:=sPath+sr.Name;
//      if(sr.Attr and faDirectory)<>0 then
//        DeleteDirDirect(sFile)
//      else if(sr.Attr and faAnyFile)=sr.Attr then
//        SysUtils.DeleteFile(sFile);//删除文件
//    until FindNext(sr)<>0;
//    SysUtils.FindClose(sr);
//  end;
//  RemoveDir(sPath);
//end;

//////////////////////////////////////////////////////////////////////
// 创建：bijy  2012.五月.07
// 接替：bijy
// 功能：压缩文件
// 参数：zip压缩文件所在的路径  解压到的文件夹
// 使用方式：ZipFile('d:\2.txt', 'd:\2.zip');
//////////////////////////////////////////////////////////////////////
procedure ZipFileName(AOldName, ANewName : string); //压缩文件
var
  lzipFile : IZipFile;
  sPath: string;
begin
  lzipFile := CreateZipFile('');
  lzipFile.AddFromFile(ExtractFileName(AOldName), AOldName);
  sPath := ExtractFilePath(ANewName);
  if not DirectoryExists(sPath) then
    ForceDirectories(sPath);
  lzipFile .SaveToFile(ANewName);
end;


//////////////////////////////////////////////////////////////////////
// 创建：bijy  2012.五月.07
// 接替：bijy
// 功能：压缩文件夹
// 参数：zip压缩文件夹所在的路径  解压到的文件夹
// 使用方式：ZipFile('d:\2', 'd:\2.zip');
//////////////////////////////////////////////////////////////////////
procedure ZipFile(AFileDir, AFileName : string); //压缩文件夹
var
  lzipFile : IZipFile;
begin
  lzipFile := CreateZipFile('');
  lzipFile.AddFiles(AFileDir) ;
  lzipFile .SaveToFile(AFileName);
end;

//////////////////////////////////////////////////////////////////////
// 创建：bijy  2012.五月.07
// 接替：bijy
// 功能：解压缩文件 ,注意解压后压缩文件直接删除
// 参数：zip解压缩文件所在的路径  解压到的文件夹
// 使用方式：UnZipFile('d:\2.zip', 'd:\2');
//////////////////////////////////////////////////////////////////////
procedure UnZipFile(AFileName, AFileDir: string; Adel : Boolean = False);
var
    oUnzip : TUnZip;
    oZip : IZipFile;
begin
  begin
    oUnzip := TUnZip.Create(nil);
    try
      oUnzip.CpuType := cptAuto;                   // CPU工作模式
      oUnzip.OverwriteMode := omOverwrite;         // 覆盖模式
      oUnzip.ArchiveFile := AFileName;              // zip名称
      oUnzip.ConfirmOverwrites := False;           // 确认复写模式
      oUnzip.RecurseDirs := True;                  // 解压所有文件
      oUnzip.Password := '';                       // 设置解压密码
      oUnzip.FileSpec.Clear();                     // 清除文件属性
      oUnzip.FileSpec.Add('*.*');                  // 解压文件过滤
      oUnzip.ExtractDir :=AFileDir;                // 解压文件路径
      if not DirectoryExists(AFileDir) then
        ForceDirectories(AFileDir);
      if DirectoryExists(AFileDir) then
        oUnzip.Extract;                            // 解压缩
      oZip := CreateZipFile;
      oZip.AddFiles(AFileDir);
      oZip.SaveToFile(AFileName);
    finally
      oUnzip.Free;
    end;
  end;
  if Adel then
    SysUtils.DeleteFile(AFileName);//如果传递过来的值是true则删除原始压缩文件，否则保留文件
end;


{-----------------------------------------------------------------------------
  作者: bijy 2012-09-19
  参数: 
  返回: 1.0.0.12
  功能：获得软件的版本号
-----------------------------------------------------------------------------}
function GetVersion(const AFileName: string): string;
var
  n, Len: DWORD;
  Buf : PChar;
  Value: Pointer;
  szName: array [0..255] of Char;
  Transstring: string;
begin
  Result := '';
  Len := GetFileVersionInfoSize(PChar(AFileName), n);
  if Len > 0 then
  begin
    Buf := AllocMem(Len);
    if GetFileVersionInfo(Pchar(AFileName), n, Len, Buf) then
    begin
      Value := nil;
      VerQueryValue(Buf, '\VarFileInfo\Translation', Value, Len);
      if Value <> nil then
        Transstring := IntToHex(MakeLong(HiWord(LongInt(Value^)),
          LoWord(LongInt(Value^))),8);
      StrPCopy(szName, '\stringFileInfo\' + Transstring + '\FileVersion');
      if VerQueryValue(Buf, szName, Value, Len) then
        Result := StrPas(Pchar(Value));
    end;
    FreeMem(Buf, n);
  end;
end;


{-----------------------------------------------------------------------------
  作者: bijy 2012-09-19
  参数: 
  返回: 
  功能：比较两个版本的大小 
-----------------------------------------------------------------------------}
function CompareVersion(const AVer1, AVer2: string): Integer;
  function Max(const A1, A2: string): Integer;
  var
    n1, n2: Integer;
  begin
    n1 := StrToInt(A1);
    n2 := StrToInt(A2);
    if n1 > n2 then Result := 1;
    if n1 < n2 then Result := -1;
    if n1 = n2 then Result := 0;
  end;
var
  oList1, oList2: TStringList;
  I: Integer;
begin
  oList1 := TStringList.Create;
  oList2 := TStringList.Create;
  try
    oList1.StrictDelimiter := True;
    oList1.Delimiter := '.';
    oList1.DelimitedText := AVer1;

    oList2.StrictDelimiter := True;
    oList2.Delimiter := '.';
    oList2.DelimitedText := AVer2;

    if (oList1.Count <> 4) or (oList2.Count <> 4) then
    begin
      Result := -2;
      Exit;
    end;

    for I := 0 to 3 do
    begin
      Result := Max(oList1[i], oList2[i]);
      if Result <> 0 then Exit;
    end;
  finally
    FreeAndNil(oList2);
    FreeAndNil(oList1);
  end;
end;

{-----------------------------------------------------------------------------
  作者: bijy 2012-08-22
  参数: const AURL: string
  返回: Boolean
  功能：检查网络连接状态 
-----------------------------------------------------------------------------}
function IsConnected(const AURL: string): Boolean;
begin
  try
    Result := InternetCheckConnection(Pchar(AURL), 1, 0);
  except
    Result := False;
  end;
end;

{-----------------------------------------------------------------------------
  作者: bijy 2012-08-22
  参数:
  返回: None
  功能：获得本机的IP地址
-----------------------------------------------------------------------------}
function GetHostIP: string;
var
  ch: array[1..32] of char;
  wsData: TWSAData;
  myHost: PHostEnt;
  i: integer;
begin
  Result := '';

  if not IsConnected('www.baidu.com') then
  begin
    Result := '';
    Exit;
  end;

  if WSAstartup(2, wsData) <> 0 then Exit; // can’t start winsock
  try
    if GetHostName(@ch[1],32) <> 0 then Exit; // getHostName failed
  except
    Exit;
  end;
  myHost := GetHostByName(@ch[1]); // GetHostName error
  if myHost = nil then exit;
  for i := 1 to 4 do
  begin
    Result := Result + IntToStr(Ord(myHost.h_addr^[i - 1]));
    if i < 4 then
      Result := Result + '.';
  end;
end;

{-----------------------------------------------------------------------------
  作者: bijy 2012-08-22
  参数: 
  返回: None
  功能：检查字符是否为ip地址
-----------------------------------------------------------------------------}
function CheckStrIsIP(const AValue: string) : Boolean;
var
  oList: TStringList;
  temp, I: Integer;
begin
  temp := -1;
  Result := False;
  oList := TStringList.Create;
  try
    oList.Delimiter := '.';
    oList.DelimitedText := Trim(AValue);
    for I := 0 to oList.Count- 1 do
    begin
      try
        temp := StrToInt(oList[i]);
      except
        Result := False;
        Break;
      end;
      Result := (temp >= 0) and (temp <= 255) and (oList.Count=4);
    end;
  finally
    oList.Free;
  end;
end;

function FileMD5(const AFile: string): string;
var
  FileHandle: THandle;
  MapHandle: THandle;
  ViewPointer: Pointer;
begin
  FileHandle := CreateFile(PChar(AFile), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if FileHandle <> INVALID_HANDLE_VALUE then
  try
    MapHandle := CreateFileMapping(FileHandle, nil, PAGE_READONLY, 0, 0, nil);
    if MapHandle <> 0 then
    try
      ViewPointer := MapViewOfFile(MapHandle, FILE_MAP_READ, 0, 0, 0);
      if ViewPointer <> nil then
      try
        Result := GetMD5(ViewPointer, GetFileSize(FileHandle, nil));
      finally
        UnmapViewOfFile(ViewPointer);
      end;
    finally
      CloseHandle(MapHandle);
    end;
  finally
    CloseHandle(FileHandle);
  end;
end;

function GetMD5(Buffer: Pointer; BufSize: Integer): string;
var
  I: Integer;
  MD5Digest: TMD5Digest;
  MD5Context: TMD5Context;
begin
  for I := 0 to 15 do
    Byte(MD5Digest[I]) := I + 1;
  MD5Init(MD5Context);
  MD5UpdateBuffer(MD5Context, Buffer, BufSize);
  MD5Final(MD5Digest, MD5Context);
  Result := '';
  for I := 0 to 15 do
    Result := Result + IntToHex(Byte(MD5Digest[I]), 2);
end;

procedure CopyDirectory(const ASourceDir, ATargetDir: string);

  function IsRealFile(SearchRec:TSearchRec):Boolean;
  begin
    { TODO : 文件类型为1,2,3,4,6,7,32,33,34,35,36,38,39,128,2049,2080,2083,2086,
    8192,8193,8195,8224,8227,10241, 12582,16416,16419, 24577的都是实在的文件}
    if ( ((SearchRec.Attr>=1)and(SearchRec.Attr<=15)) or //[1,15]
    ((SearchRec.Attr>=32)and(SearchRec.Attr<40)) or //[32,40)
    (SearchRec.Attr=128) or (SearchRec.Attr=2049) or
    (SearchRec.Attr=2080)or (SearchRec.Attr=2083) or
    (SearchRec.Attr=2086)or (SearchRec.Attr=8192) or
    (SearchRec.Attr=8193)or (SearchRec.Attr=8195) or
    (SearchRec.Attr=8224)or (SearchRec.Attr=8227) or
    (SearchRec.Attr=10241)or (SearchRec.Attr=12582) or
    (SearchRec.Attr=16416)or (SearchRec.Attr=16419) or
    (SearchRec.Attr=24577) ) and
    (SearchRec.Name<>'.') and (SearchRec.Name<>'..')
    then Result:=True else Result:=False;
  end;

var
  sDirFrom, sDirTo: string;
  oFileName: TFileName;
  oSearchRec: TSearchRec;
  oPathList: TStringList;
  nDirIndex: integer;
begin
  sDirFrom := Trim(ASourceDir);
  sDirTo := Trim(ATargetDir);
  if (sDirFrom <> '') and (sDirTo <> '') then
  begin
    //判断目标目录中是否存在此对应的目录，如果没有则生成此目录
    if not DirectoryExists(sDirTo) then
      CreateDir(sDirTo);
    sDirFrom := IncludeTrailingPathDelimiter(sDirFrom);
    sDirTo := IncludeTrailingPathDelimiter(sDirTo);
    oPathList := TStringList.Create;
    try
      if FindFirst(sDirFrom + '*.*', faAnyFile, oSearchRec) = 0 then
      begin
        repeat
          oFileName := oSearchRec.Name;
          if (oFileName = '.') or (oFileName = '..') then continue;
          if IsRealFile(oSearchRec) then
            CopyFile(PChar(sDirFrom + oFileName), PChar(sDirTo + oFileName), False );
          //处理目录
          if (oSearchRec.Attr and faDirectory) <> 0 then
            oPathList.Add(oFileName);
        until FindNext(oSearchRec) <> 0;
        SysUtils.FindClose(oSearchRec);
        for nDirIndex := 0 to oPathList.Count - 1 do
          CopyDirectory(sDirFrom + oPathList[nDirIndex], sDirTo + oPathList[nDirIndex]);
      end;
    finally
      FreeAndNil(oPathList);
    end;
  end;
end;


{-----------------------------------------------------------------------------
  作者: bijy 2012-11-26
  说明: 
  功能：获取磁盘剩余空间大小
-----------------------------------------------------------------------------}
function DiskFreeSize(const ADiskName: string): Int64;
var
  sChar: array[0..255] of Char;
begin
  Result := 0;
  StrPCopy(sChar, ADiskName);
  Result := DiskFree(Ord(UpCase(sChar[0])) - 64);
end;


{-----------------------------------------------------------------------------
  作者: bijy 2012-11-26
  说明: 
  功能：文件创建的时间
-----------------------------------------------------------------------------}
function GetFileCreateTime(const AFilename: String): string;
var
  oFindData: TWin32FindData;
  nWord: DWord;
  lft, Time: TFileTime;
  sHandle: THandle;
begin
  Result := '';
  sHandle := Windows.FindFirstFile(PChar(AFileName), oFindData);
  if (sHandle <>INVALID_HANDLE_VALUE) then
  begin
    Time := oFindData.ftCreationTime;
    Windows.FindClose(sHandle);
    FileTimeToLocalFileTime(Time, lft);
    FileTimeToDosDateTime(lft, LongRec(nWord).HI, LongRec(nWord).Lo);
    Result:= FormatDateTime('yyyy-mm-dd hh-mm-ss', FileDateToDateTime(nWord));
  end
end;

{-----------------------------------------------------------------------------
  作者: bijy 2012-11-26
  说明: 
  功能：文件最后写入时间
-----------------------------------------------------------------------------}
function GetFileLastWriteTime(const AFilename: String): string;
var
  oFindData: TWin32FindData;
  nWord: DWord;
  lft, Time: TFileTime;
  sHandle: THandle;
begin
  Result := '';
  sHandle := Windows.FindFirstFile(PChar(AFileName), oFindData);
  if (sHandle <>INVALID_HANDLE_VALUE) then
  begin
    Time := oFindData.ftLastWriteTime;
    Windows.FindClose(sHandle);
    FileTimeToLocalFileTime(Time, lft);
    FileTimeToDosDateTime(lft, LongRec(nWord).HI, LongRec(nWord).Lo);
    Result:= FormatDateTime('yyyy-mm-dd hh-mm-ss', FileDateToDateTime(nWord));
  end
end;

{-----------------------------------------------------------------------------
  作者: bijy 2012-11-26
  说明: 
  功能：文件最后访问时间
-----------------------------------------------------------------------------}
function GetFileLastAccessTime(const AFilename: String): string;
var
  oFindData: TWin32FindData;
  nWord: DWord;
  lft, Time: TFileTime;
  sHandle: THandle;
begin
  Result := '';
  sHandle := Windows.FindFirstFile(PChar(AFileName), oFindData);
  if (sHandle <>INVALID_HANDLE_VALUE) then
  begin
    Time := oFindData.ftLastAccessTime;
    Windows.FindClose(sHandle);
    FileTimeToLocalFileTime(Time, lft);
    FileTimeToDosDateTime(lft, LongRec(nWord).HI, LongRec(nWord).Lo);
    Result:= FormatDateTime('yyyy-mm-dd hh-mm-ss', FileDateToDateTime(nWord));
  end
end;

end.
