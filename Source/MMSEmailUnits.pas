unit MMSEmailUnits;

interface
uses
  SysUtils;
const
  CFailure = '失败';

  function IsConnected(const AURL: string): Boolean;//检查网络连接状态
  function CheckStrIsIP(const AValue: string) : Boolean;//检查ip是否合法
  function GetHostIP: string;//获得本机的IP地址
  function GetSqlAddressFromEmail : string;//从邮箱中获得数据库的地址
  function SendSqlAddressToEmail : Boolean;//从本地IP地址改变则发送邮件至邮箱

implementation

uses
  IdPOP3, IdMessage, WinInet, Classes, IdSMTP, WinSock;

const
  CHost163 = 'http://www.163.com/';
  CPopHost = 'pop3.163.com';//收服务器所在
  CSmtpHost = 'smtp.163.com';//发送邮件服务器所在
  CUsername = 'mmslaeesqladdress@163.com';//邮箱名称
  CPassword = '!@#qwe123';//邮箱对应的密码


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

  if not IsConnected(CHost163) then
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

{-----------------------------------------------------------------------------
  作者: bijy 2012-08-22
  参数: 
  返回: None
  功能：联网从邮箱中获得地址
-----------------------------------------------------------------------------}
function GetSqlAddressFromEmail : string;
var
  oIdpop: TIdPOP3;
  oIdmessage: TIdMessage;
  mailcount: Integer;
begin
  Result := CFailure;
  if not IsConnected(CHost163) then Exit;
  
  oIdpop := TIdPOP3.Create(nil);
  oIdmessage := TIdMessage.Create(nil);
  try
    oIdpop.Host := CPopHost;
    oIdpop.Username := CUsername;
    oIdpop.Password := CPassword;
    while true do
    begin
      oIdpop.Connect();//连接到POP3服务器
      if (oIdpop.Connected = true) then break;
    end;
    mailcount := oIdpop.CheckMessages;//得到邮箱中邮件的个数
    if mailcount = 0 then Exit;

    oIdmessage.Clear;
    oIdpop.retrieveHeader(mailcount, oIdmessage);  //得到邮件的头信息
    Result := oIdmessage.Subject;             //得到邮件的标题

    if not CheckStrIsIP(Result) then Exit;
  finally
    FreeAndNil(oIdmessage);
    FreeAndNil(oIdpop);
  end;
end;

{-----------------------------------------------------------------------------
  作者: bijy 2012-08-22
  参数: 
  返回: None
  功能：从本地IP地址改变则发送邮件至邮箱
-----------------------------------------------------------------------------}
function SendSqlAddressToEmail : Boolean;
var
  oSmtp: TIdSMTP;
  oIdmessage: TIdMessage;
  sIP: string;
begin
  sIP := GetHostIP;
  if not CheckStrIsIP(sIP) then
  begin
    Result := False;
    Exit;
  end;  

  oSmtp := TIdSMTP.Create(nil);
  oIdmessage := TIdMessage.Create(nil);
  try
    try
      oSmtp.Host := CSmtpHost;//发送邮件的服务器地址
      oSmtp.Username := CUsername;//发送邮件的用户
      oSmtp.Password := CPassword;//发送邮件的密码
      oSmtp.Port := 25;//端口
      oSmtp.Connect();//连接
      oIdmessage.Recipients.EMailAddresses := CUsername;//接受邮件的地址
      oIdmessage.From.Text := CUsername;//发件人
      oIdmessage.Subject := sIP;//邮件主题
      oSmtp.Authenticate;
      oSmtp.Send(oIdmessage); //发送邮件
      oSmtp.Disconnect;//关闭连接
      Result := True;
    except
      Result := False;
    end;
  finally
    FreeAndNil(oIdmessage);
    FreeAndNil(oSmtp);
  end;
end;

end.
