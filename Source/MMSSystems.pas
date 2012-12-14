unit MMSSystems;

interface
uses
  Windows, Forms, SysUtils, Registry, ShlObj, Nb30;

  procedure RegisterIco(AExt : string);//注册
  procedure  AddEnvironment(const AKey, AValue: string);//添加系统的环境变量
  procedure AddDelphi2007LibraryPath(const AValue: string);
  function GetMACAdress : string; //uses NB30;
  function GetHostName: string; //获得计算机名称
  //程序自动运行
  procedure AutoRunOnSystemStart(const ATitle, AFileName: String; AAuto: Boolean);
  function KillTask(const ExeFileName: string): Integer;

implementation
uses
  TlHelp32;
const
  CMMSReg = 'MMSoftware\LAEE\';
  CEnvironmentPath = '\Environment';
  CDelphi2007LibraryPath = '\Software\Borland\BDS\5.0\Library';
  CReg_Software_Microsoft_Windows_CurrentVersion_Run_ : //自动运行注册表的位置
  string = '\Software\Microsoft\Windows\CurrentVersion\Run\';

//////////////////////////////////////////////////////////////////////
// 创建：Administrator  13-05-2012
// 接替：Administrator
// 功能：实现软件的注册，程序启动每次进行程序的关联
//////////////////////////////////////////////////////////////////////
procedure RegisterIco(AExt : string);
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    if reg.OpenKey('.' + AExt, true) then
    begin
      reg.WriteString('', AExt);
      reg.CloseKey;
    end;
    //用于打开.RSQ文件的可执行程序
    if reg.OpenKey(AExt + '\shell\open\command', true) then
    begin
      reg.WriteString('', Application.ExeName + ' ' + '"' + '%1' + '"');
      reg.CloseKey;
    end;
    //取当前可执行程序的图标为.Ext文件的图标
    if reg.OpenKey(AExt + '\DefaultIcon', true) then
    begin
      reg.WriteString('', Application.ExeName + ',0');
      reg.CloseKey;
    end;
  finally
    FreeAndNil(reg);
  end;
  //立即刷新
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

procedure  AddEnvironment(const AKey, AValue: string);//添加系统的环境变量
var
  oReg: TRegistry;
begin
  oReg := TRegistry.Create;
  try
    oReg.RootKey := HKEY_CURRENT_USER;
    if oReg.OpenKey(CEnvironmentPath, True) then
      oReg.WriteString(AKey, AValue);
    oReg.CloseKey;
  finally
    FreeAndNil(oReg);
  end;
end;

procedure AddDelphi2007LibraryPath(const AValue: string);
var
  oReg: TRegistry;
  sValue: string;
begin
  oReg := TRegistry.Create;
  try
    oReg.RootKey := HKEY_CURRENT_USER;
    if oReg.OpenKey(CDelphi2007LibraryPath, False) then
    begin
      sValue := oReg.ReadString('Search Path');
      oReg.CloseKey;
    end;
    if oReg.OpenKey(CDelphi2007LibraryPath, True) then
      oReg.WriteString('Search Path', sValue + ';' + AValue);
    oReg.CloseKey;
  finally
    FreeAndNil(oReg);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: GetHostName
  Author:    bijy
  Date:      2012--10--04
  Result:    获得本机的主机名称
-----------------------------------------------------------------------------}
function GetHostName: string; //获得计算机名称
var
  ComputerName: array[0..MAX_COMPUTERNAME_LENGTH+1] of char;
  Size: Cardinal;
begin
  Result := '';
  Size := MAX_COMPUTERNAME_LENGTH + 1;
  GetComputerName(ComputerName, Size);
  Result := StrPas(ComputerName);
end;


{-----------------------------------------------------------------------------
  作者: bijy 2012-09-06
  参数: 
  返回: 
  功能：获得本机的Mac地址 
-----------------------------------------------------------------------------}
function GetMACAdress : string; //uses NB30;
var
  NCB : PNCB;
  Adapter : PAdapterStatus;
  RetCode : char;
  I : integer;
  Lenum : PlanaEnum;
  _SystemID : string;
begin
  Result := '';
  _SystemID := '';
  Getmem(NCB, SizeOf(TNCB));
  Fillchar(NCB^, SizeOf(TNCB), 0);

  Getmem(Lenum, SizeOf(TLanaEnum));
  Fillchar(Lenum^, SizeOf(TLanaEnum), 0);

  Getmem(Adapter, SizeOf(TAdapterStatus));
  Fillchar(Adapter^, SizeOf(TAdapterStatus), 0);

  Lenum.Length := chr(0);
  NCB.ncb_command := chr(NCBENUM);
  NCB.ncb_buffer := Pointer(Lenum);
  NCB.ncb_length := SizeOf(Lenum);
  RetCode := Netbios(NCB);

  i := 0;
  repeat 
    Fillchar(NCB^, SizeOf(TNCB), 0); 
    Ncb.ncb_command := chr(NCBRESET);
    Ncb.ncb_lana_num := lenum.lana[I];
    RetCode := Netbios(Ncb); 

    Fillchar(NCB^, SizeOf(TNCB), 0); 
    Ncb.ncb_command := chr(NCBASTAT);
    Ncb.ncb_lana_num := lenum.lana[I];
    // Must be 16 
    Ncb.ncb_callname := ('*'); 

    Ncb.ncb_buffer := Pointer(Adapter); 

    Ncb.ncb_length := SizeOf(TAdapterStatus);
    RetCode := Netbios(Ncb);
    //---- calc _systemId from mac-address[2-5] XOR mac-address[1]... 
    if (RetCode = chr(0)) or (RetCode = chr(6)) then
    begin 
    _SystemId := IntToHex(Ord(Adapter.adapter_address[0]), 2) + '-' +
    IntToHex(Ord(Adapter.adapter_address[1]), 2) + '-' +
    IntToHex(Ord(Adapter.adapter_address[2]), 2) + '-' + 
    IntToHex(Ord(Adapter.adapter_address[3]), 2) + '-' + 
    IntToHex(Ord(Adapter.adapter_address[4]), 2) + '-' +
    IntToHex(Ord(Adapter.adapter_address[5]), 2); 
    end; 
    Inc(i);
  until (I >= Ord(Lenum.Length)) or (_SystemID <> '00-00-00-00-00-00'); 
  FreeMem(NCB); 
  FreeMem(Adapter);
  FreeMem(Lenum); 
  GetMacAdress := _SystemID;
end;


{-----------------------------------------------------------------------------
  作者: bijy 2012-09-20
  参数: 写入注册表的名称  可执行文件所在的全路径
  返回: 
  功能：设置程序开机的时候自动启动
-----------------------------------------------------------------------------}
procedure AutoRunOnSystemStart(const ATitle, AFileName: String; AAuto: Boolean);
var
  Reg: TRegistry;
  Key: String;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Key := CReg_Software_Microsoft_Windows_CurrentVersion_Run_;
    if Reg.OpenKey(Key, True) then
      if AAuto then
        Reg.WriteString(ATitle, AFileName)
      else
        Reg.WriteString(ATitle, '');
    Reg.CloseKey;
  finally
    FreeAndNil(Reg);
  end;
end;

{-----------------------------------------------------------------------------
  作者: bijy 2012-11-13
  说明:
  功能：关闭进程，传递进程的名字，如QQ.exe
-----------------------------------------------------------------------------}
function KillTask(const ExeFileName: string): Integer;
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop: boolean;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  Result := 0;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);

  while Integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) = UpperCase(ExeFileName))
      or (UpperCase(FProcessEntry32.szExeFile) = UpperCase(ExeFileName))) then
      Result := Integer(TerminateProcess(OpenProcess(PROCESS_TERMINATE,
        BOOL(0), FProcessEntry32.th32ProcessID), 0));
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

end.
