{ *********************************************************************** }
{                                                                         }
{   PKZIP 处理类实现单元                                                  }
{                                                                         }
{   设计：Linc 2006.01.29                                                 }
{   备注：                                                                }
{   审核：                                                                }
{                                                                         }
{   Copyright (c) 1994-2009 GrandSoft Corporation                         }
{                                                                         }
{ *********************************************************************** }
{                                                                         }
{   说明：                                                                }
{                                                                         }
{   兼容 InfoZip PKZip 文件格式的支持单元，压缩算法使用 ZLIB 实现         }
{                                                                         }
{   PKZIP 文件格式参考以下文档：                                          }
{                                                                         }
{   ftp://ftp.info-zip.org/pub/infozip/doc/appnote-iz-latest.zip          }
{   ftp://ftp.uu.net/pub/archiving/zip/doc/appnote-970311-iz.zip          }
{                                                                         }
{       接口抽提参考：ZipForge、DIZipWriter、VclZip                       }
{                                                                         }
{   非加解密算法参考：SciZipFile.pas、KAZip.pas                           }
{                                                                         }
{     加解密算法参考：Abbrevia、ZipForge                                  }
{                                                                         }
{ *********************************************************************** }
{                                                                         }
{   修改：                                                                }
{                                                                         }
{   2010.03.01 - Linc                                                     }
{                                                                         }
{     1、同时兼容按照自定义文件标示和 ZIP 文件标示读取数据头信息加载流    }
{     2、内部数据使用缓冲文件流优化对文件的读写                           }
{                                                                         }
{   2010.01.18 - Linc                                                     }
{                                                                         }
{     1、对写入文件加入 FlushFileBuffers 保证数据准确性                   }
{                                                                         }
{   2009.12.09 - Linc                                                     }
{                                                                         }
{     1、修改自定义文件头的实现，使之支持任意长度任意内容                 }
{     2、添加自定义密码事件支持                                           }
{                                                                         }
{   2009.06.23 - Linc                                                     }
{                                                                         }
{     1、加入自定义文件头支持                                             }
{                                                                         }
{   2009.02.17 - Linc                                                     }
{                                                                         }
{     1、为 IZipFileEntry 添加优化大文件压缩、解压缩算法支持              }
{                                                                         }
{   2008.07.01 - Linc                                                     }
{                                                                         }
{     1、修正对加密的 TZipDataDescriptor 的错误描述                       }
{     2、如果 AddFolder 时文件夹不存在则报异常                            }
{                                                                         }
{   2008.05.01 - Linc                                                     }
{                                                                         }
{     1、添加对 FPC 编译器的支持，同时支持 x86 和 x64 应用                }
{                                                                         }
{ *********************************************************************** }

unit ZipFileUnit;

{$I CompVers.inc}

interface

uses
  SysUtils, Windows, Classes;

{.$DEFINE USE_BZIP2}

type
  TZipCompressionLevel = (
    ctNone, ctFastest, ctDefault, ctMax, ctUnknown
  );

  EZipFileException = class(Exception);

  IZipFile = interface;

  IZipFileEntry = interface(IInterface)
  ['{F96F15DA-C9CE-4466-B64F-06DE79642F30}']
    function  GetAttriButes: Integer;
    function  GetCompressedSize: LongWord;
{
    function  GetCompressionMethod: Word;
}
    function  GetCRC32: LongWord;
    function  GetData: AnsiString;
    function  GetDateTime: TDateTime;
    function  GetIsEncrypted: Boolean;
{
    function  GetExtrafield: AnsiString;
    function  GetFileComment: AnsiString;
    function  GetInternalAttributes: Word;
    function  GetLevel: TZipCompressionLevel;
}
    function  GetName: AnsiString;
    function  GetOwner: IZipFile;
    function  GetUncompressedSize: DWORD;
{
    function  GetVersionMadeBy: Word;
}
    procedure SetAttriButes(const Value: Integer);
    procedure SetData(const Value: AnsiString);
    procedure SetDateTime(const Value: TDateTime);
{
    procedure SetExtrafield(const Value: AnsiString);
    procedure SetFileComment(const Value: AnsiString);
    procedure SetLevel(const Value: TZipCompressionLevel);
}
    procedure LoadFromFile(const AFile: AnsiString);
    procedure SaveToFile(const AFile: AnsiString);

    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);

    procedure CompressFromFile(const AFile: AnsiString);
    procedure DecompressToFile(const AFile: AnsiString);

    property  Name: AnsiString
      read GetName;
    property  Data: AnsiString
      read GetData write SetData;
    property  AttriButes: Integer
      read GetAttriButes write SetAttriButes;
    property  CompressedSize: LongWord
      read GetCompressedSize;
{
    property  CompressionMethod: Word
      read GetCompressionMethod;
}
    property  CRC32: LongWord
      read GetCRC32;
    property  DateTime: TDateTime
      read GetDateTime write SetDateTime;
    property  IsEncrypted: Boolean
      read GetIsEncrypted;
{
    property  Extrafield: AnsiString
      read GetExtrafield write SetExtrafield;
    property  FileComment: AnsiString
      read GetFileComment write SetFileComment;
    property  InternalAttributes: Word
      read GetInternalAttributes;
    property  Level: TZipCompressionLevel
      read GetLevel write SetLevel;
}
    property  UncompressedSize: DWORD
      read GetUncompressedSize;
{
    property  VersionMadeBy: Word
      read GetVersionMadeBy;
}
   property  Owner: IZipFile
     read GetOwner;
  end;

  TZipGetPassWordEvent = procedure (const AEntry: IZipFileEntry; var APassWord: AnsiString) of object;

  IZipFile = interface(IInterface)
  ['{0C5D3108-B7B9-4FFE-A233-AA19E6A5FCD0}']
    function  GetCount: Integer;
    function  GetCustomFileHeader: AnsiString;
    function  GetFileComment: AnsiString;
    function  GetItems(AIndex: Integer): IZipFileEntry;
    function  GetLevel: TZipCompressionLevel;
    function  GetPassWord: AnsiString;

    procedure SetCustomFileHeader(const Value: AnsiString);
    procedure SetFileComment(const Value: AnsiString);
    procedure SetLevel(const Value: TZipCompressionLevel);
    procedure SetPassWord(const Value: AnsiString);

    function  Add(const AName: AnsiString): IZipFileEntry;
    function  AddFiles(const AFolder: AnsiString;
      const ABase: AnsiString = ''; ARecursion: Boolean = True;
      const AFileMask: AnsiString = ''; ASearchAttr: Integer = 0): Integer;
    function  AddFromBuffer(const AName: AnsiString;
      const Buffer; Count: Integer): IZipFileEntry;
    function  AddFromFile(const AName,
      AFile: AnsiString): IZipFileEntry;
    function  AddFromStream(const AName: AnsiString;
      AStream: TStream): IZipFileEntry;

    function  IndexOf(const AName: AnsiString): Integer;
{
    function  Find(const AName: AnsiString): IZipFileEntry;
}
    procedure Clear;
    procedure Delete(AIndex: Integer);

    procedure ExtractFiles(const AFolder: AnsiString;
      const ANameMask: AnsiString = '');
    procedure ExtractToBuffer(const AName: AnsiString;
      var ABuffer; ACount: Integer);
    procedure ExtractToStream(const AName: AnsiString;
      AStream: TStream);
    procedure ExtractToString(const AName: AnsiString;
      var AText: AnsiString);

    procedure LoadFromFile(const AFile: AnsiString);
    procedure SaveToFile(const AFile: AnsiString);

    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);

    function  GetOnGetPassWord: TZipGetPassWordEvent;
    procedure SetOnGetPassWord(const Value: TZipGetPassWordEvent);

    property  Count: Integer
      read GetCount;
    property  Items[AIndex: Integer]: IZipFileEntry
      read GetItems; default;
    property  FileComment: AnsiString
      read GetFileComment write SetFileComment;
    property  Level: TZipCompressionLevel
      read GetLevel write SetLevel;
    property  PassWord: AnsiString
      read GetPassWord write SetPassWord;
    property  CustomFileHeader: AnsiString
      read GetCustomFileHeader write SetCustomFileHeader;

    property  OnGetPassWord: TZipGetPassWordEvent
      read GetOnGetPassWord write SetOnGetPassWord;
  end;

  function CreateZipFile(const APassWord: AnsiString = ''): IZipFile;

  function LoadZipFile(const AFile: AnsiString;
    const APassWord: AnsiString = ''): IZipFile; overload;
  function LoadZipFile(AStream: TStream;
    const APassWord: AnsiString = ''): IZipFile; overload;

  function FileNameToZipName(const AName: AnsiString): AnsiString;
  function ZipNameToFileName(const AName: AnsiString): AnsiString;

  function IsZipFile(const AFile: AnsiString): Boolean;
  function IsZipStream(Stream: TStream): Boolean;

  function IsEncryptedZipFile(const AFile: AnsiString): Boolean;
  function IsEncryptedZipStream(Stream: TStream): Boolean;

  function ZipText(const S: AnsiString): AnsiString;
  function UnZipText(const S: AnsiString): AnsiString;

  function ZipStream(ASrcStream,
    ADesStream: TStream; const APassWord: AnsiString = ''): Boolean; overload;
  function ZipStream(AStream: TStream;
    const APassWord: AnsiString = ''): Boolean; overload;

  function UnZipStream(ASrcStream,
    ADesStream: TStream; const APassWord: AnsiString = ''): Boolean; overload;
  function UnZipStream(AStream: TStream;
    const APassWord: AnsiString = ''): Boolean; overload;

  function CompressZipFiles(const AFile, AFolder: AnsiString;
    const ANameMask: AnsiString = ''): Integer;
  function ExtractZipFiles(const AFile, AFolder: AnsiString;
    const ANameMask: AnsiString = ''): Integer;

implementation

uses
  Math, Masks,
{$IFDEF USE_BZIP2}
  BZip2,
{$ENDIF}
  ZlibEx, GrandZipEnc;

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion < 23}
type
  NativeInt = Integer;
  {$IFEND}
{$ENDIF}

type
  TCommonFileHeader = packed record
    VersionNeededToExtract: Word;            // 2 bytes
    GeneralPurposeBitFlag: Word;             // 2 bytes
    CompressionMethod: Word;                 // 2 bytes
    LastModFileTimeDate: DWORD;              // 4 bytes
    Crc32: DWORD;                            // 4 bytes
    CompressedSize: DWORD;                   // 4 bytes
    UncompressedSize: DWORD;                 // 4 bytes
    FilenameLength: Word;                    // 2 bytes
    ExtraFieldLength: Word;                  // 2 bytes
  end;

  TCentralFileHeader = packed record
    CentralFileHeaderSignature: DWORD;       // 4 bytes  (0x02014b50, 'PK'12)
    VersionMadeBy: Word;                     // 2 bytes
    CommonFileHeader: TCommonFileHeader;     //
    FileCommentLength: Word;                 // 2 bytes
    DiskNumberStart: Word;                   // 2 bytes
    InternalFileAttributes: Word;            // 2 bytes
    ExternalFileAttributes: DWORD;           // 4 bytes
    RelativeOffsetOfLocalHeader: DWORD;      // 4 bytes
  end;

  TEndOfCentralDirectory = packed record
    EndOfCentralDirSignature: DWORD;         // 4 bytes  (0x06054b50, 'PK'56)
    NumberOfThisDisk: Word;                  // 2 bytes
    NumberOfTheDiskWithTheStart: Word;       // 2 bytes
    TotalNumberOfEntriesOnThisDisk: Word;    // 2 bytes
    TotalNumberOfEntries: Word;              // 2 bytes
    SizeOfTheCentralDirectory: DWORD;        // 4 bytes
    OffsetOfStartOfCentralDirectory: DWORD;  // 4 bytes
    ZipfileCommentLength: Word;              // 2 bytes
  end;

  TZipDataDescriptor = packed record
    DataDescSignature: DWORD;                // 4 bytes  (0x08074B50, 'PK'78)
    CRC32: DWORD;                            // 4 bytes
    CompressedSize: DWORD;                   // 4 bytes
    UncompressedSize: DWORD;                 // 4 bytes
  end;

  TZipFile = class;
  TZipFileEntry = class;

  IZipPersistent = interface(IInterface)
  ['{5090E978-509E-43F6-8C97-32B9A49B6E3B}']
    procedure LoadCentralDirectory(AStream: TStream);
    procedure LoadDataDescriptor(AStream: TStream);
    procedure LoadLocalFileHeader(AStream: TStream);
    procedure SaveCentralDirectory(AStream: TStream);
    procedure SaveDataDescriptor(AStream: TStream);
    procedure SaveLocalFileHeader(AStream: TStream);
  end;

  TZipFileCompressedData = record
    CompressedData: AnsiString;
    Crc32,
    CompressedSize,
    UncompressedSize: LongWord;
  end;

  TZipFileEntry = class(TInterfacedObject, IZipFileEntry, IZipPersistent)
  private
    FZipFile: TZipFile;
    FFileName: AnsiString;
    FCentralDirectory: TCentralFileHeader;
    FCommonFileHeader: TCommonFileHeader;
    FCompressedData: AnsiString;
    FExtrafield: AnsiString;
    FFileComment: AnsiString;
    FLevel: TZipCompressionLevel;

    function  GetBZip2Data: AnsiString;
    function  GetHasPassWord: Boolean;
    function  GetIntf: IZipFileEntry;
    function  GetStoredData: AnsiString;
    function  GetZLibStreamHeader: AnsiString;
    function  GetZLibData: AnsiString;
    procedure ParseEntry;
    procedure SetBZip2Data(const AData: AnsiString);
    procedure SetStoredData(const AData: AnsiString);
    procedure SetZLibData(const AData: AnsiString);
  private
    function  BuildDecryptStream(AStream: TStream;
      var ADecrypter: TZipDecryptStream): Boolean;
    function  BuildEncryptStream(AStream: TStream;
      var AEncrypter: TZipEncryptStream): Boolean;
    function  DecryptZipData(const AData: AnsiString;
      var AResult: AnsiString): Boolean;
    function  EncryptZipData(const AData: AnsiString;
      var AResult: AnsiString): Boolean;
    function  DecompressZLibData(const AData: AnsiString; ASize: Integer;
      var AResult: AnsiString): Boolean;
    function  CompressZLibData(const AData: AnsiString;
      var AResult: AnsiString): Boolean;
    function  DecompressZLibFile(const AData: AnsiString;
      const AFile: AnsiString): Boolean;
    function  CompressZLibFile(const AFile: AnsiString;
      out AData: TZipFileCompressedData): Boolean;
  protected
    { IZipFileEntry }
    function  GetAttriButes: Integer;
    function  GetCompressedSize: LongWord;
    function  GetCompressionMethod: Word;
    function  GetCRC32: LongWord;
    function  GetData: AnsiString;
    function  GetDateTime: TDateTime;
    function  GetExtrafield: AnsiString;
    function  GetFileComment: AnsiString;
    function  GetInternalAttributes: Word;
    function  GetIsEncrypted: Boolean;
    function  GetLevel: TZipCompressionLevel;
    function  GetName: AnsiString;
    function  GetOwner: IZipFile;
    function  GetUncompressedSize: DWORD;
    function  GetVersionMadeBy: Word;

    procedure SetAttriButes(const Value: Integer);
    procedure SetData(const Value: AnsiString);
    procedure SetDateTime(const Value: TDateTime);
    procedure SetExtrafield(const Value: AnsiString);
    procedure SetFileComment(const Value: AnsiString);
    procedure SetLevel(const Value: TZipCompressionLevel);

    procedure LoadFromFile(const AFile: AnsiString);
    procedure SaveToFile(const AFile: AnsiString);

    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);

    procedure CompressFromFile(const AFile: AnsiString);
    procedure DecompressToFile(const AFile: AnsiString);

    { IZipPersistent }
    procedure LoadLocalFileHeader(AStream: TStream);
    procedure SaveLocalFileHeader(AStream: TStream);
    procedure LoadCentralDirectory(AStream: TStream);
    procedure SaveCentralDirectory(AStream: TStream);
    procedure LoadDataDescriptor(AStream: TStream);
    procedure SaveDataDescriptor(AStream: TStream);
  protected
    property  Intf: IZipFileEntry
      read GetIntf;
    property  IsEncrypted: Boolean
      read GetIsEncrypted;
    property  HasPassWord: Boolean
      read GetHasPassWord;
  public
    constructor Create(AZipFile: TZipFile;
      const AName: AnsiString = ''; AttriBute: DWord = 0;
      ATimeStamp: TDateTime = 0);
    destructor Destroy; override;
  end;

  TZipList = class(TObject)
  private
    FList: TList;

    function  GetCount: Integer;
    function  GetItem(AIndex: Integer): IZipFileEntry;
    procedure SetItem(AIndex: Integer; const Value: IZipFileEntry);
  protected
    function  Add(const AItem: IZipFileEntry): Integer;
    function  Find(const AName: AnsiString): IZipFileEntry;
    function  IndexOf(const AItem: IZipFileEntry): Integer; overload;
    function  IndexOf(const AName: AnsiString): Integer; overload;
    function  Remove(const AItem: IZipFileEntry): Integer;

    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure Insert(AIndex: Integer; const AItem: IZipFileEntry);

    property  Count: Integer
      read GetCount;
    property  Items[AIndex: Integer]: IZipFileEntry
      read GetItem write SetItem; default;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TZipFile = class(TInterfacedObject, IZipFile)
  private
    FFiles: TZipList;
    FLevel: TZipCompressionLevel;
    FFileComment,
    FPassWord: AnsiString;
    FEndOfCentralDir: TEndOfCentralDirectory;
    FCustomFileHeader: AnsiString;
    FOnGetPassWord: TZipGetPassWordEvent;
  private
    function  FindPassWord(const AEntry: TZipFileEntry): AnsiString;
    procedure LoadEndOfCentralDirectory(AStream: TStream);
    procedure SaveEndOfCentralDirectory(AStream: TStream;
      ACentralDirectoryOffset: LongWord);
  protected
    { IZipFile }
    function  GetCount: Integer;
    function  GetCustomFileHeader: AnsiString;
    function  GetFileComment: AnsiString;
    function  GetItems(AIndex: Integer): IZipFileEntry;
    function  GetLevel: TZipCompressionLevel;
    function  GetPassWord: AnsiString;

    procedure SetCustomFileHeader(const Value: AnsiString);
    procedure SetFileComment(const Value: AnsiString);
    procedure SetLevel(const Value: TZipCompressionLevel);
    procedure SetPassWord(const Value: AnsiString);

    function  Add(const AName: AnsiString): IZipFileEntry; overload;
    function  Add(const AName: AnsiString;
      AttriBute: DWord; ATimeStamp: TDateTime): IZipFileEntry; overload;
    function  AddFiles(const AFolder: AnsiString; const ABase: AnsiString = '';
      ARecursion: Boolean = True; const AFileMask: AnsiString = '';
      ASearchAttr: Integer = 0): Integer;
    function  AddFromBuffer(const AName: AnsiString; const ABuffer;
      ACount: Integer): IZipFileEntry;
    function  AddFromFile(const AName,
      AFile: AnsiString): IZipFileEntry;
    function  AddFromStream(const AName: AnsiString;
      AStream: TStream): IZipFileEntry;
    function  IndexOf(const AName: AnsiString): Integer;
    function  Find(const AName: AnsiString): IZipFileEntry;
    procedure Clear;
    procedure Delete(AIndex: Integer);

    procedure ExtractFiles(const AFolder: AnsiString;
      const ANameMask: AnsiString = '');
    procedure ExtractToBuffer(const AName: AnsiString;
      var ABuffer; ACount: Integer);
    procedure ExtractToStream(const AName: AnsiString;
      AStream: TStream);
    procedure ExtractToString(const AName: AnsiString;
      var AText: AnsiString);

    procedure LoadFromFile(const AFile: AnsiString);
    procedure SaveToFile(const AFile: AnsiString);

    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);

    function  GetOnGetPassWord: TZipGetPassWordEvent;
    procedure SetOnGetPassWord(const Value: TZipGetPassWordEvent);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TBufferedStreamReader = class(TStream)
  private
    FStream: TStream;
    FStreamSize: Integer;
    FBuffer: array of Byte;
    FBufferSize: Integer;
    FBufferStartPosition: Integer;
    FVirtualPosition: Integer;
  private
    procedure UpdateBufferFromPosition(StartPos: Integer);
  public
    constructor Create(Stream: TStream; BufferSize: Integer = 1024);

    function  Read(var Buffer; Count: Longint): Longint; override;
    function  Write(const Buffer; Count: Longint): Longint; override;
    function  Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

  TBufferedFileReader = class(TBufferedStreamReader)
  public
    constructor Create(const AFileName: AnsiString); reintroduce;
    destructor Destroy; override;
  end;

resourcestring
  rsErrEncryptedFile    = '文件被加密，无法处理';
  rsErrEncryptedZipData = '压缩数据出错';
  rsErrFileCrc32        = '文件 %s CRC32 检验出错';
  rsErrFilePassWord     = '文件密码不正确';
  rsErrFolderNotExist   = '目录 %s 不存在';
  rsErrIndexOutOfBand   = '访问索引越界';
  rsErrUnSupportMethod  = '未知的压缩方法 %d';

  rsErrCompressFromFile = '压缩算法只能是 ZLIB，而且不能加密，主要用于超大文件的压缩，节省内存';
  rsErrDecompressFile   = '解压缩算法只能是 ZLIB，而且不能加密，主要用于超大文件的压缩，节省内存';
  rsErrDecompressToFile = '解压数据到文件出错';

  rsErrFileHeader       = '压缩文件格式非法，无法读取文件头信息';

  rsErrExtractPassWord  = '解压密码不正确';
  rsErrMaxLenPassWord   = '压缩密码长度超过 80 字符';

const
  defZipFileName: AnsiString
                        = '＝^0^＝';
{
  bit 0: If set, indicates that the file is encrypted.
  bit 3: If this bit is set, the fields crc-32, compressed size
   and uncompressed size are set to zero in the local
   header.  The correct values are put in the data descriptor
   immediately following the compressed data.
}
  defFileIsEncrypted    = $0001;
  defHasDataDescriptor  = $0008;

  conCentralFileHeaderSignature: DWORD = $02014b50;
  conLocalFileHeaderSignature  : DWORD = $04034B50;
  conEndOfCentralDirSignature  : DWORD = $06054B50;
  conDataDescSignature         : DWORD = $08074B50;

  conFileHeaderSignature       : DWORD = $20444C47;         { G.L.D._        }

  defMaxFileHeaderSize  = 64;

  defMaxPassWordLen     = 80; { Limit set by PKWare's Zip specs. }

  { TZipCompressionMethod }

  cmStored              = 0;
  cmShrunk              = 1;
  cmReduced1            = 2;
  cmReduced2            = 3;
  cmReduced3            = 4;
  cmReduced4            = 5;
  cmImploded            = 6;
  cmTokenizingReserved  = 7;
  cmDeflated            = 8;
  cmDeflated64          = 9;
  cmDCLImploding        = 10;
  cmPKWAREReserved      = 11;
  cmBZip2               = 12;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.01.29
//功能: 创建一个压缩文件对象
//参数：
////////////////////////////////////////////////////////////////////////////////
function CreateZipFile(const APassWord: AnsiString): IZipFile;
begin
  Result := TZipFile.Create;
  Result.PassWord := APassWord;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.01.29
//功能: 由文件加载对象
//参数：
////////////////////////////////////////////////////////////////////////////////
function LoadZipFile(const AFile, APassWord: AnsiString): IZipFile;
begin
  Result := CreateZipFile(APassWord);
  Result.LoadFromFile(AFile);
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.01.29
//功能: 由内存流加载对象
//参数：
////////////////////////////////////////////////////////////////////////////////
function LoadZipFile(AStream: TStream;
  const APassWord: AnsiString): IZipFile; overload;
begin
  Assert(AStream <> nil);

  Result := CreateZipFile(APassWord);
  Result.LoadFromStream(AStream);
end;

////////////////////////////////////////////////////////////////////////////////
//设计：Linc 2009.05.13
//功能：压缩文件
//参数：
////////////////////////////////////////////////////////////////////////////////
function CompressZipFiles(const AFile, AFolder: AnsiString;
  const ANameMask: AnsiString): Integer;
var
  oZip: IZipFile;
begin
  oZip := CreateZipFile;
  try
    oZip.AddFiles(AFolder, '', True, ANameMask);
    oZip.SaveToFile(AFile);
    Result := oZip.Count;
  except
    Result := 0;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计：Linc 2009.05.12
//功能：解压文件
//参数：
////////////////////////////////////////////////////////////////////////////////
function ExtractZipFiles(const AFile, AFolder, ANameMask: AnsiString): Integer;
var
  oZip: IZipFile;
begin
  try
    oZip := LoadZipFile(AFile);
    oZip.ExtractFiles(AFolder, ANameMask);
    Result := oZip.Count;
  except
    Result := 0;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.01.30
//功能: 转换文件路径
//参数：
////////////////////////////////////////////////////////////////////////////////
function FileNameToZipName(const AName: AnsiString): AnsiString;
var
  nIndex: Integer;
begin
  Result := AName;
  Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
  nIndex := Pos(':/', Result);
  if nIndex > 0 then
  begin
    System.Delete(Result, 1, nIndex + 1);
  end;
  nIndex := Pos('//', Result);
  if nIndex > 0 then
  begin
    System.Delete(Result, 1, nIndex + 1);
    nIndex := Pos('/', Result);
    if nIndex > 0 then
    begin
      System.Delete(Result, 1, nIndex);
      nIndex := Pos('/', Result);
      if nIndex > 0 then
        System.Delete(Result, 1, nIndex);
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计：Linc 2010.03.01
//功能：在缓冲区中查找指定内容
//参数：
//注意：偷懒，直接借助 Pos 的实现，多了一个开辟内存的动作
////////////////////////////////////////////////////////////////////////////////
function PosData(const AData; ALen: Integer; const S: AnsiString): Integer;
var
  strData: AnsiString;
begin
  SetLength(strData, ALen);
  Move(AData, Pointer(strData)^, ALen);
  Result := Pos(strData, S);
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.01.30
//功能: 转换文件路径
//参数：
////////////////////////////////////////////////////////////////////////////////
function ZipNameToFileName(const AName: AnsiString): AnsiString;
begin
  Result := AName;
  Result := StringReplace(Result, '/', '\', [rfReplaceAll]);
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2004.08.22
//功能: 判断一个文件是否为压缩文件
//参数：
////////////////////////////////////////////////////////////////////////////////
function IsZipFile(const AFile: AnsiString): Boolean;
var
  stream: TStream;
begin
  stream := TBufferedFileReader.Create(AFile);
  with stream do
  try
    Position := 0;
    Result := IsZipStream(stream);
  finally
    Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2004.08.22
//功能: 判断一个流是否为 ZIP 压缩流
//参数：
//注意：可能需要跳过存在的自定义数据头
//      如果非标准的自定义头，默认只判断开始的 64 字节内容 defMaxFileHeaderSize
//      读流时避免再多读头 4 个字节一次
//      返回 True 时 Position 会定位到 conLocalFileHeaderSignature 之后的位置
////////////////////////////////////////////////////////////////////////////////
function IsZipStream(Stream: TStream): Boolean;
var
  strBuffer: AnsiString;
  nSignature: DWORD;
  nLen, nPos: Integer;
begin
  Assert(Stream <> nil);
  with Stream do
  try
    ReadBuffer(nSignature, SizeOf(DWORD));
    if nSignature = conLocalFileHeaderSignature then
      Result := True
    else if nSignature = conFileHeaderSignature then
    begin
      ReadBuffer(nLen, SizeOf(DWORD));
      Seek(nLen, soCurrent);
      ReadBuffer(nSignature, SizeOf(DWORD));
      Result := nSignature = conLocalFileHeaderSignature;
    end
    else begin
      SetLength(strBuffer, defMaxFileHeaderSize);
      PDWORD(strBuffer)^ := nSignature;
      nLen := Read(Pointer(NativeInt(strBuffer) + SizeOf(DWORD))^, Length(strBuffer) - SizeOf(DWORD));
      Inc(nLen, SizeOf(DWORD));
      SetLength(strBuffer, nLen);
      nPos := PosData(conLocalFileHeaderSignature, SizeOf(DWORD), strBuffer);
      Result := nPos > 0;
      if Result then Seek(nPos - nLen + SizeOf(DWORD) - 1, soCurrent);
    end;
  except
    Result := False;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计：Linc 2009.06.23
//功能：判断文件是否加密
//参数：
////////////////////////////////////////////////////////////////////////////////
function IsEncryptedZipFile(const AFile: AnsiString): Boolean;
var
  stream: TStream;
begin
  stream := TBufferedFileReader.Create(AFile);
  with stream do
  try
    Position := 0;
    Result := IsEncryptedZipStream(stream);
  finally
    Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计：Linc 2009.06.23
//功能：判断 ZIP 数据流是否加密
//参数：
////////////////////////////////////////////////////////////////////////////////
function IsEncryptedZipStream(Stream: TStream): Boolean;
var
  nValue: Word;
begin
  Result := IsZipStream(Stream);
  if Result then
    with Stream do
  try
    // read 2 bytes for VersionNeededToExtractSize
    ReadBuffer(nValue, SizeOf(nValue));
    // read 2 bytes for GeneralPurposeBitFlagSize
    ReadBuffer(nValue, SizeOf(nValue));
    // check the encryption flag
    Result := nValue and defFileIsEncrypted <> 0;
  except
    Result := False;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2004.08.18
//功能: 压缩字串
//参数：
////////////////////////////////////////////////////////////////////////////////
function ZipText(const S: AnsiString): AnsiString;
var
  cZipFile: IZipFile;
  stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  with stream do
  try
    cZipFile := CreateZipFile;
    cZipFile.Add(defZipFileName).Data := S;
    cZipFile.SaveToStream(stream);
    SetLength(Result, Size);
    Move(Memory^, Pointer(Result)^, Size);
  finally
    stream.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2004.08.18
//功能: 解压缩字串
//参数：
////////////////////////////////////////////////////////////////////////////////
function UnZipText(const S: AnsiString): AnsiString;
var
  cZipFile: IZipFile;
  stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  with stream do
  try
    Write(Pointer(S)^, Length(S));
    Position := 0;
    cZipFile := LoadZipFile(stream);
    if cZipFile.Count > 0 then
      Result := cZipFile[0].Data
    else Result := '';
  finally
    Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2004.08.25
//功能: 压缩流
//参数：
//注意：不能直接压缩 TFileStream，改函数会把处理的结果回写到 Stream 中
////////////////////////////////////////////////////////////////////////////////
function ZipStream(AStream: TStream; const APassWord: AnsiString): Boolean;
var
  cZipFile: IZipFile;
  nSize: Integer;
  strData: AnsiString;
begin
  Assert(AStream <> nil);
  cZipFile := CreateZipFile(APassWord);
  with cZipFile do
  try
    nSize := AStream.Size;
    if nSize = 0 then
      strData := ''
    else begin
      SetLength(strData, nSize);
      AStream.Position := 0;
      AStream.Read(Pointer(strData)^, nSize);
    end;
    Add(defZipFileName).Data := strData;
    AStream.Size := 0;
    SaveToStream(AStream);
    Result := True;
  except
    Result := False;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2004.08.25
//功能: 解压压缩流
//参数：
//注意：不能直接压缩 TFileStream，改函数会把处理的结果回写到 Stream 中
////////////////////////////////////////////////////////////////////////////////
function UnZipStream(AStream: TStream; const APassWord: AnsiString): Boolean;
var
  cZipFile: IZipFile;
  strData: AnsiString;
begin
  Assert(AStream <> nil);
  AStream.Position := 0;
  cZipFile := LoadZipFile(AStream, APassWord);
  with cZipFile do
  try
    strData := Items[0].Data;
    AStream.Size := 0;
    AStream.Write(Pointer(strData)^, Length(strData));
    Result := True;
  except
    Result := False;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2004.11.20
//功能: 压缩流
//参数：
////////////////////////////////////////////////////////////////////////////////
function ZipStream(ASrcStream, ADesStream: TStream;
  const APassWord: AnsiString): Boolean; overload;
var
  cZipFile: IZipFile;
  nSize: Integer;
  strData: AnsiString;
begin
  Assert(ASrcStream <> nil);
  Assert(ADesStream <> nil);
  with ASrcStream do
  begin
    nSize := Size;
    if nSize = 0 then
      strData := ''
    else begin
      SetLength(strData, nSize);
      Position := 0;
      Read(Pointer(strData)^, nSize);
    end;
  end;
  cZipFile := CreateZipFile(APassWord);
  with cZipFile do
  try
    Add(defZipFileName);
    Items[0].Data := strData;
    ADesStream.Size := 0;
    SaveToStream(ADesStream);
    Result := True;
  except
    Result := False;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2004.11.21
//功能: 解压缩流
//参数：
////////////////////////////////////////////////////////////////////////////////
function UnZipStream(ASrcStream, ADesStream: TStream;
  const APassWord: AnsiString): Boolean; overload;
var
  cZipFile: IZipFile;
  strData: AnsiString;
begin
  Assert(ASrcStream <> nil);
  Assert(ADesStream <> nil);
  cZipFile := CreateZipFile(APassWord);
  with cZipFile do
  try
    ASrcStream.Position := 0;
    LoadFromStream(ASrcStream);
    strData := Items[0].Data;
    ADesStream.Size := 0;
    ADesStream.Write(Pointer(strData)^, Length(strData));
    Result := True;
  except
    Result := False;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2004.08.28
//功能: 抛出 ZIP 异常
//参数：
////////////////////////////////////////////////////////////////////////////////
procedure ZipFileError(const Msg: AnsiString); overload;
begin
  raise EZipFileException.Create(Msg);
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2004.08.28
//功能: 抛出 ZIP 异常
//参数：
////////////////////////////////////////////////////////////////////////////////
procedure ZipFileError(const Msg: AnsiString; const Args: array of const); overload;
begin
  raise EZipFileException.CreateFmt(Msg, Args);
end;

{ TBufferedStreamReader }

constructor TBufferedStreamReader.Create(Stream: TStream; BufferSize: Integer);
begin
  // init stream
  FStream := Stream;
  FStreamSize := Stream.Size;
  // init buffer
  FBufferSize := BufferSize;
  SetLength(FBuffer, BufferSize);
  FBufferStartPosition := -FBufferSize; { out of any useful range }
  // init virtual position
  FVirtualPosition := 0;
end;

function TBufferedStreamReader.Read(var Buffer; Count: Integer): Longint;
var
  BytesLeft: Integer;
  FirstBufferRead: Integer;
  StreamDirectRead: Integer;
  Buf: PAnsiChar;
begin
  if (FVirtualPosition >= 0) and (Count >= 0) then
  begin
    Result := FStreamSize - FVirtualPosition;
    if Result > 0 then
    begin
      if Result > Count then
        Result := Count;

      Buf := @Buffer;
      BytesLeft := Result;

      // try to read what is left in buffer
      FirstBufferRead := FBufferStartPosition + FBufferSize - FVirtualPosition;
      if (FirstBufferRead < 0) or (FirstBufferRead > FBufferSize) then
        FirstBufferRead := 0;
      FirstBufferRead := Min(FirstBufferRead, Result);
      if FirstBufferRead > 0 then
      begin
        Move(FBuffer[FVirtualPosition - FBufferStartPosition], Buf[0], FirstBufferRead);
        Dec(BytesLeft, FirstBufferRead);
      end;

      if BytesLeft > 0 then
      begin
        // The first read in buffer was not enough
        StreamDirectRead := (BytesLeft div FBufferSize) * FBufferSize;
        FStream.Position := FVirtualPosition + FirstBufferRead;
        FStream.Read(Buf[FirstBufferRead], StreamDirectRead);
        Dec(BytesLeft, StreamDirectRead);

        if BytesLeft > 0 then
        begin
          // update buffer, and read what is left
          UpdateBufferFromPosition(FStream.Position);
          Move(FBuffer[0], Buf[FirstBufferRead + StreamDirectRead], BytesLeft);
        end;
      end;

      Inc(FVirtualPosition, Result);
      Exit;
    end;
  end;
  Result := 0;
end;

function TBufferedStreamReader.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning:
      FVirtualPosition := Offset;
    soFromCurrent:
      Inc(FVirtualPosition, Offset);
    soFromEnd:
      FVirtualPosition := FStreamSize + Offset;
  end;
  Result := FVirtualPosition;
end;

procedure TBufferedStreamReader.UpdateBufferFromPosition(StartPos: Integer);
begin
  try
    FStream.Position := StartPos;
    FStream.Read(FBuffer[0], FBufferSize);
    FBufferStartPosition := StartPos;
  except
    FBufferStartPosition := -FBufferSize; { out of any useful range }
    raise;
  end;
end;

function TBufferedStreamReader.Write(const Buffer; Count: Integer): Longint;
begin
  ZipFileError('Internal Error: class can not write.');
  Result := 0;
end;

{ TBufferedFileReader }

////////////////////////////////////////////////////////////////////////////////
//设计：Linc 2010.03.01
//功能：
//参数：
//注意：不支持大于 2G 的文件，且只支持文件读
////////////////////////////////////////////////////////////////////////////////
constructor TBufferedFileReader.Create(const AFileName: AnsiString);
begin
  FStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  inherited Create(FStream);
end;

destructor TBufferedFileReader.Destroy;
begin
  inherited;
  FreeAndNil(FStream);
end;

{ TZipFileEntry }

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.02.01
//功能: 返回一个解压数据对象
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipFileEntry.BuildDecryptStream(AStream: TStream;
  var ADecrypter: TZipDecryptStream): Boolean;
var
  nEncCRC: LongWord;
  strPassWord: AnsiString;
begin
  Result := False;
  Assert(AStream <> nil);

  with FCommonFileHeader do
    if GeneralPurposeBitFlag and defHasDataDescriptor <> 0 then
      nEncCRC := LastModFileTimeDate shl $10
    else nEncCRC := CRC32;

  strPassWord := FZipFile.FindPassWord(Self);
  if strPassWord = '' then
    raise EZipFileException.Create(rsErrExtractPassWord);
  ADecrypter := TZipDecryptStream.Create(AStream, nEncCRC, strPassWord);
  try
    Result := ADecrypter.IsValid;
  finally
    if not Result then
      FreeAndNil(ADecrypter);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.02.01
//功能: 返回一个压缩数据处理对象
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipFileEntry.BuildEncryptStream(AStream: TStream;
  var AEncrypter: TZipEncryptStream): Boolean;
var
  nEncCRC: LongWord;
begin
  Assert(AStream <> nil);

  with FCommonFileHeader do
    if GeneralPurposeBitFlag and defHasDataDescriptor <> 0 then
      nEncCRC := LastModFileTimeDate shl $10
    else nEncCRC := CRC32;
  try
    AEncrypter := TZipEncryptStream.Create(AStream, nEncCRC, FZipFile.FPassWord);
    Result := True;
  except
    Result := False;
  end;
  if not Result then FreeAndNil(AEncrypter);
end;

////////////////////////////////////////////////////////////////////////////////
//设计：Linc 2009.02.17
//功能：直接使用 ZLIB 算法由文件压缩数据
//参数：
//注意：压缩算法只能是 ZLIB，而且不能加密，主要用于超大文件的压缩，节省内存
////////////////////////////////////////////////////////////////////////////////
procedure TZipFileEntry.CompressFromFile(const AFile: AnsiString);
var
  oData: TZipFileCompressedData;
begin
  if HasPassWord or (FCommonFileHeader.CompressionMethod <> cmDeflated) then
    ZipFileError(rsErrCompressFromFile);
  if not CompressZLibFile(AFile, oData) then
    ZipFileError(rsErrEncryptedZipData);
  with FCommonFileHeader do
  begin
    Crc32 := oData.Crc32;
    UncompressedSize := oData.UncompressedSize;
    FCompressedData := oData.CompressedData;
  end;
  with FCommonFileHeader do
  begin
{
    VersionNeededToExtract := 20;
    GeneralPurposeBitFlag := 0;
    CompressionMethod := cmDeflated;
    LastModFileTimeDate := DateTimeToFileDate(Now);
    Crc32 := ZipCRC32(Value);
}
    CompressedSize := Length(FCompressedData);
{
    UncompressedSize := Length(Value);
    FilenameLength := Length(FFileName);
    ExtraFieldLength := Length(FExtrafield);
}
  end;

  with FCentralDirectory do
  begin
{
    CentralFileHeaderSignature := conCentralFileHeaderSignature;
    VersionMadeBy := 20;
}
    CommonFileHeader := FCommonFileHeader;
{
    FileCommentLength := 0;
    DiskNumberStart := 0;
    InternalFileAttributes := 0;
    ExternalFileAttributes := 0;
    RelativeOffsetOfLocalHeader := 0;
}
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.02.01
//功能: 使用 ZLIB 算法压缩数据
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipFileEntry.CompressZLibData(const AData: AnsiString;
  var AResult: AnsiString): Boolean;
var
  compressor: TZCompressionStream;
  stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  try
    compressor := TZCompressionStream.Create(stream, TZCompressionLevel(FLevel));
    try
      compressor.Write(Pointer(AData)^, Length(AData));
    finally
      compressor.Free;
    end;
    { strip the 2 byte headers and 4 byte footers }
    SetLength(AResult, stream.Size - 6);
    Move(Pointer(Integer(stream.Memory) + 2)^, Pointer(AResult)^, stream.Size - 6);
    Result := True;
  finally
    stream.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2008.02.17
//功能: 使用 ZLIB 算法压缩文件
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipFileEntry.CompressZLibFile(const AFile: AnsiString;
  out AData: TZipFileCompressedData): Boolean;
const
  defBufferSize = 32768;
var
  strBuffer: AnsiString;
  nRead: Integer;
  filestream: TStream;
  strstream: TMemoryStream;
  compressor: TZCompressionStream;
  I, nCrc: LongWord;
begin
  filestream := TBufferedFileReader.Create(AFile);
  with filestream do
  try
    SetLength(strBuffer, defBufferSize);
    strstream := TMemoryStream.Create;
    compressor := TZCompressionStream.Create(strstream, zcFastest);
    nCrc := $FFFFFFFF;
    repeat
      nRead := filestream.Read(Pointer(strBuffer)^, defBufferSize);
      for I := 1 to nRead do
        nCrc := ZipUpdateCRC32(Byte(strBuffer[I]), nCrc);
      if nRead > 0 then
        compressor.Write(Pointer(strBuffer)^, nRead);
    until nRead < defBufferSize;
    nCrc := nCrc xor $FFFFFFFF;
    FreeAndNil(compressor);
    with AData do
    begin
      { strip the 2 byte headers and 4 byte footers }
      SetLength(CompressedData, strstream.Size - 6);
      Move(Pointer(Integer(strstream.Memory) + 2)^, PByte(CompressedData)^, strstream.Size - 6);
      Crc32 := nCrc;
      CompressedSize := Length(AData.CompressedData);
      UncompressedSize := Size;
    end;
    Result := True;
  finally
    Free;
    FreeAndNil(strstream);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.02.01
//功能: 初始化
//参数：
////////////////////////////////////////////////////////////////////////////////
constructor TZipFileEntry.Create(AZipFile: TZipFile; const AName: AnsiString;
  AttriBute: DWord; ATimeStamp: TDateTime);
var
  nTimeStamp: TDateTime;
begin
  FZipFile := AZipFile;

  if SameValue(ATimeStamp, 0) then
    nTimeStamp := Now
  else nTimeStamp := ATimeStamp;

  if AZipFile <> nil then
    FLevel := AZipFile.FLevel
  else FLevel := ctDefault;

  FFileName := FileNameToZipName(AName);
  { start with an empty file }
  FCompressedData := '';
  FExtrafield := '';
  with FCommonFileHeader do
  begin
    VersionNeededToExtract := 20;
    GeneralPurposeBitFlag := 0;
    CompressionMethod := cmDeflated;
    LastModFileTimeDate := DateTimeToFileDate(nTimeStamp);
    Crc32 := 0;
    CompressedSize := 0;
    UncompressedSize := 0;
    FilenameLength := Length(FFileName);
    ExtraFieldLength := Length(FExtrafield);
  end;
  with FCentralDirectory do
  begin
    CentralFileHeaderSignature := conCentralFileHeaderSignature;
    VersionMadeBy := 20;
    CommonFileHeader := FCommonFileHeader;
    FileCommentLength := 0;
    DiskNumberStart := 0;
    InternalFileAttributes := 0;
    ExternalFileAttributes := Attribute;
    RelativeOffsetOfLocalHeader := 0;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计：Linc 2009.02.17
//功能：直接使用 ZLIB 算法解压数据到文件
//参数：
//注意：压缩算法只能是 ZLIB，而且不能加密，主要用于超大文件的压缩，节省内存
////////////////////////////////////////////////////////////////////////////////
procedure TZipFileEntry.DecompressToFile(const AFile: AnsiString);
begin
  if IsEncrypted or (FCommonFileHeader.CompressionMethod <> cmDeflated) then
    ZipFileError(rsErrDecompressFile);
  if not DecompressZLibFile(FCompressedData, AFile) then
    ZipFileError(rsErrDecompressToFile);
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.02.01
//功能: 解压缩 ZLIB 压缩数据
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipFileEntry.DecompressZLibData(const AData: AnsiString; ASize: Integer;
  var AResult: AnsiString): Boolean;
var
  strData: AnsiString;
  stream: TMemoryStream;
  decompressor: TZDecompressionStream;
begin
  AResult := '';
  strData := GetZLibStreamHeader + AData;
  stream := TMemoryStream.Create;
  try
    stream.Write(Pointer(strData)^, Length(strData));
    stream.Position := 0;
    decompressor := TZDecompressionStream.Create(stream);
    try
      SetLength(AResult, ASize);
      Result := decompressor.Read(Pointer(AResult)^, ASize) = ASize;
      if not Result then AResult := '';
    finally
      decompressor.Free;
    end;
  finally
    stream.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2008.02.17
//功能: 使用 ZLIB 算法解压缩文件
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipFileEntry.DecompressZLibFile(const AData: AnsiString; const AFile:
    AnsiString): Boolean;
const
  defBufferSize = 32768;
var
  strBuffer: AnsiString;
  nRead: Integer;
  filestream: TFileStream;
  strstream: TMemoryStream;
  decompressor: TZDecompressionStream;
begin
  filestream := TFileStream.Create(AFile, fmCreate or fmOpenReadWrite or fmShareDenyWrite);
  with filestream do
  try
    strBuffer := GetZLibStreamHeader + AData;
    strstream := TMemoryStream.Create;
    strstream.Write(Pointer(strBuffer)^, Length(strBuffer));
    strstream.Position := 0;

    SetLength(strBuffer, defBufferSize);
    decompressor := TZDecompressionStream.Create(strstream);
    repeat
      nRead := decompressor.Read(Pointer(strBuffer)^, defBufferSize);
      if nRead > 0 then
        filestream.Write(Pointer(strBuffer)^, nRead);
    until nRead < defBufferSize;
    FreeAndNil(decompressor);
    FlushFileBuffers(Handle);
    Result := True;
  finally
    Free;
    FreeAndNil(strstream);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.02.01
//功能: 解密 PKZIP 数据
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipFileEntry.DecryptZipData(const AData: AnsiString;
  var AResult: AnsiString): Boolean;
var
  stream: TMemoryStream;
  decrypter: TZipDecryptStream;
  nSize: Integer;
begin
  Result := False;
  AResult := AData;
  stream := TMemoryStream.Create;
  try
    stream.Write(Pointer(AData)^, Length(AData));
    stream.Position := 0;
    if BuildDecryptStream(stream, decrypter) then
    try
      nSize := Length(AData) - SizeOf(TZipEncryptHeader); { TZipEncryptHeader }
      SetLength(AResult, nSize);
      Result := decrypter.Read(Pointer(AResult)^, nSize) = nSize;
    finally
      decrypter.Free;
    end
    else raise EZipFileException.Create(rsErrExtractPassWord);
  finally
    stream.Free;
  end;
end;

destructor TZipFileEntry.Destroy;
begin

  inherited;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.02.01
//功能: 使用 PKZIP 加密算法处理数据
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipFileEntry.EncryptZipData(const AData: AnsiString;
  var AResult: AnsiString): Boolean;
var
  stream: TMemoryStream;
  encrypter: TZipEncryptStream;
  nSize: Integer;
begin
  Result := False;
  AResult := AData;
  stream := TMemoryStream.Create;
  try
    if BuildEncryptStream(stream, encrypter) then
    try
      nSize := Length(AData);
      Result := encrypter.Write(Pointer(AData)^, nSize) = nSize;
    finally
      encrypter.Free;
    end;
    if Result then
      SetLength(AResult, stream.Size);
      Move(stream.Memory^, Pointer(AResult)^, stream.Size);
  finally
    stream.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.01.29
//功能: 保存的文件属性
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipFileEntry.GetAttriButes: Integer;
begin
  Result := FCentralDirectory.ExternalFileAttributes;
end;

{ TZipFileEntryBZip2Processor }

function TZipFileEntry.GetBZip2Data: AnsiString;
{$IFDEF USE_BZIP2}
var
  stream: TMemoryStream;
  decompressor: TBZDecompressionStream;
  nReadBytes: LongWord;
{$ENDIF}
begin
{$IFDEF USE_BZIP2}
  with FEntry.FCommonFileHeader do
  begin
    if CompressionMethod <> 12 then
      Exit;
    Result := FEntry.FCompressedData;
    stream := TMemoryStream.Create;;
    try
      stream.Write(Poiter(Result)^, CompressedSize);
      stream.Position := 0;
      decompressor := TBZDecompressionStream.Create(stream);
      try
        SetLength(Result, UncompressedSize);
        nReadBytes := decompressor.Read(Pointer(Result)^, UncompressedSize);
        if nReadBytes <> UncompressedSize then
          Result := '';
      finally
        decompressor.Free;
      end;
    finally
      stream.Free;
    end;
  end;
{$ELSE}
  Result := '';
  Assert(False);
{$ENDIF}
end;

function TZipFileEntry.GetCompressedSize: LongWord;
begin
  Result := FCommonFileHeader.CompressedSize;
end;

function TZipFileEntry.GetCompressionMethod: Word;
begin
  Result := FCommonFileHeader.CompressionMethod;
end;

function TZipFileEntry.GetCRC32: LongWord;
begin
  Result := FCommonFileHeader.Crc32;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.01.30
//功能: 解压数据
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipFileEntry.GetData: AnsiString;
var
  nCrc32: DWORD;
begin
  case FCommonFileHeader.CompressionMethod of
    cmStored:
      Result := GetStoredData;
    cmDeflated:
      Result := GetZLibData;
    cmBZip2:
      Result := GetBZip2Data;
  else
    ZipFileError(rsErrUnSupportMethod, [FCommonFileHeader.CompressionMethod]);
  end;
  if not IsEncrypted then
  begin
    nCrc32 := ZipCRC32(Result);
    if nCrc32 <> FCommonFileHeader.Crc32 then
      ZipFileError(rsErrFileCrc32, [FFileName]);
  end;
end;

function TZipFileEntry.GetDateTime: TDateTime;
begin
  Result := FileDateToDateTime(FCommonFileHeader.LastModFileTimeDate);
end;

function TZipFileEntry.GetExtrafield: AnsiString;
begin
  Result := FExtrafield;
end;

function TZipFileEntry.GetFileComment: AnsiString;
begin
  Result := FFileComment;
end;

function TZipFileEntry.GetHasPassWord: Boolean;
begin
  Result := FZipFile.FPassWord <> '';
end;

function TZipFileEntry.GetInternalAttributes: Word;
begin
  Result := FCentralDirectory.InternalFileAttributes;
end;

function TZipFileEntry.GetIntf: IZipFileEntry;
begin
  Result := Self;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.01.29
//功能: 返回是否加密标志位
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipFileEntry.GetIsEncrypted: Boolean;
begin
  Result := FCommonFileHeader.GeneralPurposeBitFlag and defFileIsEncrypted <> 0;
end;

function TZipFileEntry.GetLevel: TZipCompressionLevel;
begin
  Result := FLevel;
end;

function TZipFileEntry.GetName: AnsiString;
begin
  Result := FFileName;
end;

function TZipFileEntry.GetOwner: IZipFile;
begin
  Result := FZipFile;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.02.01
//功能: 获取直接存储的数据
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipFileEntry.GetStoredData: AnsiString;
begin
  if FCommonFileHeader.CompressionMethod <> cmStored then
    Exit;
  if not IsEncrypted then
    Result := FCompressedData
  else if not DecryptZipData(FCompressedData, Result) then
    ZipFileError(rsErrFilePassWord);
end;

function TZipFileEntry.GetUncompressedSize: DWORD;
begin
  Result := FCommonFileHeader.UncompressedSize;
end;

function TZipFileEntry.GetVersionMadeBy: Word;
begin
  Result := FCentralDirectory.VersionMadeBy;
end;

{ TZipFileEntryZLibProcessor }

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.01.31
//功能: 解压 ZLIB 数据块
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipFileEntry.GetZLibData: AnsiString;
var
  strData: AnsiString;
begin
  Result := '';
  if FCommonFileHeader.CompressionMethod <> cmDeflated then
    Exit;
  if not IsEncrypted then
    strData := FCompressedData
  else if not DecryptZipData(FCompressedData, strData) then
  begin
    ZipFileError(rsErrFilePassWord);
    Exit;
  end;
  DecompressZLibData(strData, FCommonFileHeader.UncompressedSize, Result);
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.01.31
//功能: 生成压缩流数据头
//参数：
//注意：manufacture a 2 byte header for zlib; 4 byte footer is not required.
//      Result := Chr(120) + Chr(156);
////////////////////////////////////////////////////////////////////////////////
function TZipFileEntry.GetZLibStreamHeader: AnsiString;
{
  TZLibStreamHeader = packed record
     CMF : Byte;
     FLG : Byte;
  end;
}
{
const
  ZL_DEF_COMPRESSIONINFO    = $7;  // 32k window for Deflate
  ZL_DEF_COMPRESSIONMETHOD  = $8;  // Deflate

  ZL_FASTEST_COMPRESSION    = $0;
  ZL_FAST_COMPRESSION       = $1;
  ZL_DEFAULT_COMPRESSION    = $2;
  ZL_MAXIMUM_COMPRESSION    = $3;

  ZL_PRESET_DICT            = $20;

  ZL_FCHECK_MASK            = $1F;
var
  nCMF, nFLG,
  nCompress: Byte;
  nZLH: Word;
}
begin
  case FCommonFileHeader.GeneralPurposeBitFlag and 6 of
    0:
      Result := #$78#$9C;
    2:
      Result := #$78#$DA;
    4:
      Result := #$78#$5E;
    6:
      Result := #$78#$01;
  else
    Result := #$78#$9C;
  end;
{
  case FCommonFileHeader.GeneralPurposeBitFlag and 6 of
    0:
      nCompress := ZL_DEFAULT_COMPRESSION;
    2:
      nCompress := ZL_MAXIMUM_COMPRESSION;
    4:
      nCompress := ZL_FAST_COMPRESSION;
    6:
      nCompress := ZL_FASTEST_COMPRESSION;
  else
    nCompress := ZL_DEFAULT_COMPRESSION;
  end;
  nCMF := ZL_DEF_COMPRESSIONINFO shl 4;
  nCMF := nCMF or ZL_DEF_COMPRESSIONMETHOD;
  nFLG := 0;
  nFLG := nFLG or (nCompress shl 6);
  nFLG := nFLG and not ZL_PRESET_DICT;
  nFLG := nFLG and not ZL_FCHECK_MASK;
  nZLH := (nCMF * 256) + nFLG;
  Inc(nFLG, 31 - (nZLH mod 31));
  SetLength(Result, 2 * SizeOf(Byte));
  Result[1] := Chr(nCMF);
  Result[2] := Chr(nFLG);
}
{
  case CompMode of
    0, 7, 8, 9: Result := $DA78;
    1, 2: Result := $0178;
    3, 4: Result := $5E78;
    5, 6: Result := $9C78;
  else
    Result := 0;
  end;
}
end;

procedure TZipFileEntry.LoadCentralDirectory(AStream: TStream);
begin
  Assert(AStream <> nil);

  with FCentralDirectory do
  begin
{
    CentralFileHeaderSignature := conCentralFileHeaderSignature;
    AStream.Read(VersionMadeBy, 2);
    AStream.Read(CommonFileHeader, SizeOf(CommonFileHeader));
    AStream.Read(FileCommentLength, 2);
    AStream.Read(DiskNumberStart, 2);
    AStream.Read(InternalFileAttributes, 2);
    AStream.Read(ExternalFileAttributes, 4);
    AStream.Read(RelativeOffsetOfLocalHeader, 4);
}
    AStream.Seek(-SizeOf(conCentralFileHeaderSignature), soFromCurrent);
    AStream.Read(FCentralDirectory, SizeOf(FCentralDirectory));

    SetLength(FFileName, CommonFileHeader.FilenameLength);
    AStream.Read(Pointer(FFileName)^,
      CommonFileHeader.FilenameLength);
    SetLength(FExtrafield, CommonFileHeader.ExtraFieldLength);
    AStream.Read(Pointer(FExtrafield)^,
      CommonFileHeader.ExtraFieldLength);
    SetLength(FFileComment, FileCommentLength);
    AStream.Read(Pointer(FFileComment)^, FileCommentLength);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.01.31
//功能: 加载加密数据描述信息
//参数：
////////////////////////////////////////////////////////////////////////////////
procedure TZipFileEntry.LoadDataDescriptor(AStream: TStream);
var
  cData: TZipDataDescriptor;
begin
  Assert(AStream <> nil);

  if FCommonFileHeader.GeneralPurposeBitFlag and defHasDataDescriptor > 0 then
  begin
    AStream.Read(cData, SizeOf(cData));
    if cData.DataDescSignature <> conDataDescSignature then
      AStream.Seek(-SizeOf(cData), soCurrent);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2004.08.18
//功能: 由文件加载
//参数：
////////////////////////////////////////////////////////////////////////////////
procedure TZipFileEntry.LoadFromFile(const AFile: AnsiString);
var
  stream: TStream;
begin
  stream := TBufferedFileReader.Create(AFile);
  try
    LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: LSUPER 2006.01.29
//功能: 由数据流加载
//参数：
////////////////////////////////////////////////////////////////////////////////
procedure TZipFileEntry.LoadFromStream(AStream: TStream);
var
  S: AnsiString;
  nSize: Integer;
begin
  Assert(AStream <> nil);

  with AStream do
  begin
    nSize := Size - Position;
    SetLength(S, nSize);
    Read(Pointer(S)^, nSize);

    SetData(S);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: LSUPER 2006.01.29
//功能: 由数据流初始化内容
//参数：
////////////////////////////////////////////////////////////////////////////////
procedure TZipFileEntry.LoadLocalFileHeader(AStream: TStream);
begin
  Assert(AStream <> nil);

  AStream.Read(FCommonFileHeader, SizeOf(FCommonFileHeader));
  SetLength(FFileName, FCommonFileHeader.FilenameLength);
  AStream.Read(Pointer(FFileName)^,
    FCommonFileHeader.FilenameLength);
  SetLength(FExtrafield, FCommonFileHeader.ExtraFieldLength);
  AStream.Read(Pointer(FExtrafield)^,
    FCommonFileHeader.ExtraFieldLength);
  SetLength(FCompressedData, FCommonFileHeader.CompressedSize);
  AStream.Read(Pointer(FCompressedData)^,
    FCommonFileHeader.CompressedSize);

  ParseEntry;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.01.31
//功能: 初始化存储信息
//参数：
////////////////////////////////////////////////////////////////////////////////
procedure TZipFileEntry.ParseEntry;
begin
  FLevel := ctUnknown;
{
  if bit 3 is set, then the data descriptor record is appended
  to the compressed data
}
  with FCommonFileHeader do
    if CompressionMethod = cmDeflated then
      case GeneralPurposeBitFlag and 6 of
        0:
          FLevel := ctDefault; { Normal }
        2:
          FLevel := ctMax;     { Maximum }
        4:
          FLevel := ctFastest; { Fastest }
        6:
          FLevel := ctFastest; { SuperFast }
      else
        ;
      end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2004.08.18
//功能: 由文件流加载数据
//参数：
////////////////////////////////////////////////////////////////////////////////
procedure TZipFileEntry.SaveCentralDirectory(AStream: TStream);
begin
  Assert(AStream <> nil);

  with AStream do
  begin
    Write(FCentralDirectory, SizeOf(FCentralDirectory));
    Write(Pointer(FFileName)^, Length(FFileName));
    Write(Pointer(FExtrafield)^, Length(FExtrafield));
    Write(Pointer(FFileComment)^, Length(FFileComment));
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.01.31
//功能: 保存加密数据描述信息
//参数：
////////////////////////////////////////////////////////////////////////////////
procedure TZipFileEntry.SaveDataDescriptor(AStream: TStream);
var
  cData: TZipDataDescriptor;
begin
  Assert(AStream <> nil);

  with FCommonFileHeader do
    if (CompressionMethod = cmDeflated) and (GeneralPurposeBitFlag and defHasDataDescriptor <> 0) then
  begin
    FillChar(cData, SizeOf(cData), #0);
    cData.DataDescSignature := conDataDescSignature;
    cData.CRC32 := Crc32;
    cData.CompressedSize := CompressedSize;
    cData.UncompressedSize := UncompressedSize;
    AStream.Write(cData, SizeOf(cData));
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: LSUPER 2006.01.29
//功能: 向数据流保存内容
//参数：
////////////////////////////////////////////////////////////////////////////////
procedure TZipFileEntry.SaveLocalFileHeader(AStream: TStream);
var
  nFlag: Word;
  nSignature: DWORD;
begin
  nSignature := conLocalFileHeaderSignature;

  Assert(AStream <> nil);

  if HasPassWord then
    with FCommonFileHeader do
    begin
      nFlag := GeneralPurposeBitFlag or defFileIsEncrypted;
      GeneralPurposeBitFlag := nFlag;
    end;
  with FCentralDirectory do
  begin
    CommonFileHeader := FCommonFileHeader;
    RelativeOffsetOfLocalHeader := AStream.Position;
  end;

  AStream.Write(nSignature, 4);

  AStream.Write(FCommonFileHeader, SizeOf(FCommonFileHeader));
  AStream.Write(Pointer(FFileName)^,
    FCommonFileHeader.FilenameLength);
  AStream.Write(Pointer(FExtrafield)^,
    FCommonFileHeader.ExtraFieldLength);
  AStream.Write(Pointer(FCompressedData)^,
    FCommonFileHeader.CompressedSize);
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2004.08.18
//功能: 向文件保存
//参数：
////////////////////////////////////////////////////////////////////////////////
procedure TZipFileEntry.SaveToFile(const AFile: AnsiString);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(AFile, fmCreate or fmOpenReadWrite or fmShareDenyWrite);
  try
    SaveToStream(stream);
    FlushFileBuffers(stream.Handle);
  finally
    stream.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: LSUPER 2006.01.29
//功能: 数据解压到流
//参数：
////////////////////////////////////////////////////////////////////////////////
procedure TZipFileEntry.SaveToStream(AStream: TStream);
var
  S: AnsiString;
begin
  Assert(AStream <> nil);

  with AStream do
  begin
    S := GetData;
    Write(Pointer(S)^, Length(S));
  end;
end;

procedure TZipFileEntry.SetAttriButes(const Value: Integer);
begin
  FCentralDirectory.ExternalFileAttributes := Value;
end;

procedure TZipFileEntry.SetBZip2Data(const AData: AnsiString);
{$IFDEF USE_BZIP2}
var
  compressor: TZCompressionStream;
  stream: TMemoryStream;
{$ENDIF}
begin
{$IFDEF USE_BZIP2}
  stream := TMemoryStream.Create;
  try
    compressor := TBZCompressionStream.Create(stream, TZCompressionLevel(FLevel));
    try
      compressor.Write(Pointer(AData)^, Length(AData));
    finally
      compressor.Free;
    end;
    SetLength(FCompressedData, stream.Size);
    Move(stream.Memory^, Pointer(FCompressedData)^, stream.Size);
  finally
    stream.Free;
  end;
{$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2004.08.18
//功能: 保存数据
//参数：
//注意：需要更新数据域信息
////////////////////////////////////////////////////////////////////////////////
procedure TZipFileEntry.SetData(const Value: AnsiString);
begin
  with FCommonFileHeader do
  begin
    Crc32 := ZipCRC32(Value);
    UncompressedSize := Length(Value);
  end;

  case FCommonFileHeader.CompressionMethod of
    cmStored:
      SetStoredData(Value);
    cmDeflated:
      SetZLibData(Value);
    cmBZip2:
      SetBZip2Data(Value);
  else
    ZipFileError(rsErrUnSupportMethod, [FCommonFileHeader.CompressionMethod]);
  end;

  with FCommonFileHeader do
  begin
{
    VersionNeededToExtract := 20;
    GeneralPurposeBitFlag := 0;
    CompressionMethod := cmDeflated;
    LastModFileTimeDate := DateTimeToFileDate(Now);
    Crc32 := ZipCRC32(Value);
}
    CompressedSize := Length(FCompressedData);
{
    UncompressedSize := Length(Value);
    FilenameLength := Length(FFileName);
    ExtraFieldLength := Length(FExtrafield);
}
  end;

  with FCentralDirectory do
  begin
{
    CentralFileHeaderSignature := conCentralFileHeaderSignature;
    VersionMadeBy := 20;
}
    CommonFileHeader := FCommonFileHeader;
{
    FileCommentLength := 0;
    DiskNumberStart := 0;
    InternalFileAttributes := 0;
    ExternalFileAttributes := 0;
    RelativeOffsetOfLocalHeader := 0;
}
  end;
end;

procedure TZipFileEntry.SetDateTime(const Value: TDateTime);
begin
  FCommonFileHeader.LastModFileTimeDate := DateTimeToFileDate(Value);
end;

procedure TZipFileEntry.SetExtrafield(const Value: AnsiString);
begin
  FExtrafield := Value;
  FCommonFileHeader.ExtraFieldLength := Length(FExtrafield);
end;

procedure TZipFileEntry.SetFileComment(const Value: AnsiString);
begin
  FFileComment := Value;
  FCentralDirectory.FileCommentLength := Length(FFileComment);
end;

procedure TZipFileEntry.SetLevel(const Value: TZipCompressionLevel);
begin
  FLevel := Value;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.02.01
//功能: 设置存储数据
//参数：
////////////////////////////////////////////////////////////////////////////////
procedure TZipFileEntry.SetStoredData(const AData: AnsiString);
begin
  if FCommonFileHeader.CompressionMethod <> cmStored then
    Exit;
  if not HasPassWord then
    FCompressedData := AData
  else if not EncryptZipData(AData, FCompressedData) then
    ZipFileError(rsErrEncryptedZipData);
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.02.01
//功能: 设置压缩数据
//参数：
////////////////////////////////////////////////////////////////////////////////
procedure TZipFileEntry.SetZLibData(const AData: AnsiString);
var
  strData: AnsiString;
begin
  if FCommonFileHeader.CompressionMethod <> cmDeflated then
    Exit;
  if not CompressZLibData(AData, strData) then
    ZipFileError(rsErrEncryptedZipData)
  else if not HasPassWord then
    FCompressedData := strData
  else if not EncryptZipData(strData, FCompressedData) then
    ZipFileError(rsErrEncryptedZipData);
end;

{ TZipList }

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2005.07.28
//功能: 添加一项
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipList.Add(const AItem: IZipFileEntry): Integer;
begin
  with FList do
  begin
    Result := Add(nil);
    IZipFileEntry(List[Result]) := AItem;
  end;
end;

procedure TZipList.Clear;
var
  I: Integer;
begin
  with FList do
  begin
    for I := 0 to Count - 1 do
      IZipFileEntry(List[I]) := nil;
    Clear;
  end;
end;

constructor TZipList.Create;
begin
  FList := TList.Create;
end;

procedure TZipList.Delete(AIndex: Integer);
begin
  with FList do
    if InRange(AIndex, 0, Count - 1) then
    begin
      IZipFileEntry(List[AIndex]) := nil;
      Delete(AIndex);
    end;
end;

destructor TZipList.Destroy;
begin
  Self.Clear;

  FreeAndNil(FList);

  inherited;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.01.30
//功能: 查找项
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipList.Find(const AName: AnsiString): IZipFileEntry;
var
  nIndex: Integer;
begin
  nIndex := Self.IndexOf(AName);
  if nIndex = -1 then
    Result := nil
  else Result := Items[nIndex];
end;

function TZipList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TZipList.GetItem(AIndex: Integer): IZipFileEntry;
begin
  with FList do
    if InRange(AIndex, 0, Count - 1) then
      Result := IZipFileEntry(List[AIndex])
    else begin
      Result := nil;
      ZipFileError(rsErrIndexOutOfBand);
    end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.01.30
//功能: 检索
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipList.IndexOf(const AName: AnsiString): Integer;
begin
  Result := FList.Count - 1;
  while Result >= 0 do
    if SameText(Items[Result].Name, AName) then
      Break
    else Dec(Result);
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2005.07.28
//功能: 按指针检索项
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipList.IndexOf(const AItem: IZipFileEntry): Integer;
begin
  Result := FList.IndexOf(Pointer(AItem));
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2005.07.28
//功能: 插入新项
//参数：
////////////////////////////////////////////////////////////////////////////////
procedure TZipList.Insert(AIndex: Integer; const AItem: IZipFileEntry);
begin
  with FList do
  begin
    Insert(AIndex, nil);
    IZipFileEntry(List[AIndex]) := AItem;
  end;
end;

function TZipList.Remove(const AItem: IZipFileEntry): Integer;
begin
  with FList do
  begin
    Result := IndexOf(Pointer(AItem));
    if Result > -1 then
    begin
      IZipFileEntry(List[Result]) := nil;
      Delete(Result);
    end;
  end;
end;

procedure TZipList.SetItem(AIndex: Integer; const Value: IZipFileEntry);
begin
  with FList do
    if InRange(AIndex, 0, Count - 1) then
      IZipFileEntry(List[AIndex]) := Value
    else ZipFileError(rsErrIndexOutOfBand);
end;

{ TZipFile }

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2004.08.18
//功能: 添加一个记录
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipFile.Add(const AName: AnsiString): IZipFileEntry;
begin
  Result := TZipFileEntry.Create(Self, AName);
  FFiles.Add(Result);
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.02.08
//功能: 添加一个记录
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipFile.Add(const AName: AnsiString; AttriBute: DWord;
  ATimeStamp: TDateTime): IZipFileEntry;
begin
  Result := TZipFileEntry.Create(Self, AName, AttriBute, ATimeStamp);
  FFiles.Add(Result);
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.01.31
//功能: 递归添加文件
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipFile.AddFiles(const AFolder, ABase: AnsiString;
  ARecursion: Boolean; const AFileMask: AnsiString; ASearchAttr: Integer): Integer;
var
  cSearchRec: TSearchRec;
  strMask, strPath, strName, strBase, strFile: AnsiString;
  nSearchAttr: Integer;
begin
{$WARNINGS OFF}
  Result := 0;

  if not DirectoryExists(AFolder) then
  begin
    ZipFileError(rsErrFolderNotExist, [AFolder]);
    Exit;
  end;

  if ABase = '' then
    strBase := IncludeTrailingPathDelimiter(AFolder)
  else strBase := IncludeTrailingPathDelimiter(ABase);
  if AFileMask = '' then
    strMask := '*.*'
  else strMask := AFileMask;
  if ASearchAttr = 0 then
    nSearchAttr := faReadOnly + faHidden + faSysFile + faDirectory + faArchive + faAnyFile
  else nSearchAttr := ASearchAttr;
  strPath := IncludeTrailingPathDelimiter(AFolder);

  if SysUtils.FindFirst(strPath + strMask, nSearchAttr, cSearchRec) <> 0 then
    Exit;
  repeat
    strName := cSearchRec.Name;
    if (strName = '..') or (strName = '.') then
      Continue;
    if cSearchRec.Attr and faDirectory <> 0 then
    begin
      strFile := strPath + strName + '\';
      strFile := ExtractRelativePath(strBase, strFile);
      Self.Add(strFile, cSearchRec.Attr, FileDateToDateTime(cSearchRec.Time));
      Inc(Result);
      if ARecursion then
        Inc(Result, AddFiles(strPath + strName, strBase, ARecursion, AFileMask, ASearchAttr))
      else Continue;
    end
    else begin
      strFile := strPath + strName;
      strName := ExtractRelativePath(strBase, strFile);
      Self.Add(strName, cSearchRec.Attr,
        FileDateToDateTime(cSearchRec.Time)).LoadFromFile(strFile);
      Inc(Result);
    end;
  until SysUtils.FindNext(cSearchRec) <> 0;
  SysUtils.FindClose(cSearchRec);
{$WARNINGS ON}
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.02.23
//功能: 由缓冲区直接加载
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipFile.AddFromBuffer(const AName: AnsiString; const ABuffer;
  ACount: Integer): IZipFileEntry;
var
  S: AnsiString;
begin
  Result := Self.Add(AName);
  if ACount <> 0 then
  begin
    SetLength(S, ACount);
    Move(ABuffer, Pointer(S)^, ACount);
    Result.Data := S;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.01.29
//功能: 由文件添加
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipFile.AddFromFile(const AName, AFile: AnsiString): IZipFileEntry;
var
  cSearchRec: TSearchRec;
begin
  Result := nil;
  if SysUtils.FindFirst(AFile, faAnyFile, cSearchRec) = 0 then
  try
    if cSearchRec.Attr and faDirectory <> 0 then
      Exit;
    Result := Add(AName, cSearchRec.Attr, FileDateToDateTime(cSearchRec.Time));
    Result.LoadFromFile(AFile);
  finally
    SysUtils.FindClose(cSearchRec);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: LSUPER 2006.01.29
//功能: 添加流内容
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipFile.AddFromStream(const AName: AnsiString;
  AStream: TStream): IZipFileEntry;
var
  S: AnsiString;
begin
  Assert(AStream <> nil);
  with AStream do
  begin
    Position := 0;
    if Size = 0 then
      S := ''
    else begin
      SetLength(S, Size);
      Read(Pointer(S)^, Size);
    end;
    Result := Add(AName);
    Result.Data := S;
  end;
end;

procedure TZipFile.Clear;
begin
  FFiles.Clear;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.01.29
//功能: 初始化、析构
//参数：
////////////////////////////////////////////////////////////////////////////////
constructor TZipFile.Create;
begin
  FFiles := TZipList.Create;
  FillChar(FEndOfCentralDir, SizeOf(FEndOfCentralDir), #0);
  FLevel := ctDefault;
end;

procedure TZipFile.Delete(AIndex: Integer);
begin
  FFiles.Delete(AIndex);
end;

destructor TZipFile.Destroy;
begin
  FreeAndNil(FFiles);

  inherited;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2006.02.20
//功能: 解压缩所有文件到文件夹
//参数：
////////////////////////////////////////////////////////////////////////////////
procedure TZipFile.ExtractFiles(const AFolder: AnsiString;
  const ANameMask: AnsiString = '');
var
  I: Integer;
  strFolder,
  strDir,
  strPath,
  strMask: AnsiString;
  cMask: TMask;
begin
  if ANameMask = '' then
    strMask := '*'
  else strMask := ANameMask;
  strFolder := ExcludeTrailingPathDelimiter(AFolder);
  cMask := TMask.Create(strMask);
  with cMask do
  try
    for I := 0 to FFiles.Count - 1 do
    begin
      strPath := ZipNameToFileName(FFiles[I].Name);
      if not Matches(strPath) then
        Continue;
      if Copy(strPath, 1, 1) = '\' then
        strPath := strFolder + strPath
      else strPath := IncludeTrailingPathDelimiter(strFolder) + strPath;
      if FFiles[I].AttriButes and faDirectory <> 0 then
        ForceDirectories(strPath)
      else begin
        strDir := ExtractFilePath(strPath);
        if not DirectoryExists(strDir) then
          ForceDirectories(strDir);
        FFiles[I].SaveToFile(strPath);
      end;
    end;
  finally
    Free;
  end;
end;

procedure TZipFile.ExtractToBuffer(const AName: AnsiString; var ABuffer;
  ACount: Integer);
var
  cEntry: IZipFileEntry;
  strData: AnsiString;
  nSize: Integer;
begin
  cEntry := Find(AName);
  if cEntry <> nil then
  begin
    strData := cEntry.Data;
    nSize := Min(ACount, Length(strData));
    ZeroMemory(@ABuffer, ACount);
    Move(Pointer(strData)^, ABuffer, nSize);
  end;
end;

procedure TZipFile.ExtractToStream(const AName: AnsiString; AStream: TStream);
var
  cEntry: IZipFileEntry;
begin
  cEntry := Find(AName);
  if cEntry <> nil then cEntry.SaveToStream(AStream);
end;

procedure TZipFile.ExtractToString(const AName: AnsiString; var AText: AnsiString);
var
  cEntry: IZipFileEntry;
begin
  cEntry := Find(AName);
  if cEntry <> nil then
    AText := cEntry.Data
  else AText := '';
end;

function TZipFile.Find(const AName: AnsiString): IZipFileEntry;
begin
  Result := FFiles.Find(AName);
end;

////////////////////////////////////////////////////////////////////////////////
//设计：Linc 2009.12.09
//功能：获取设置的密码
//参数：
////////////////////////////////////////////////////////////////////////////////
function TZipFile.FindPassWord(const AEntry: TZipFileEntry): AnsiString;
begin
  Result := FPassWord;
  if Assigned(FOnGetPassWord) then
    FOnGetPassWord(AEntry, Result);
  if Length(Result) > defMaxPassWordLen then
    ZipFileError(rsErrMaxLenPassWord);
end;

function TZipFile.GetCount: Integer;
begin
  Result := FFiles.Count;
end;

function TZipFile.GetCustomFileHeader: AnsiString;
begin
  Result := FCustomFileHeader;
end;

function TZipFile.GetFileComment: AnsiString;
begin
  Result := FFileComment;
end;

function TZipFile.GetItems(AIndex: Integer): IZipFileEntry;
begin
  if InRange(AIndex, 0, FFiles.Count - 1) then
    Result := FFiles[AIndex]
  else begin
    Result := nil;
    ZipFileError(rsErrIndexOutOfBand);
  end;
end;

function TZipFile.GetLevel: TZipCompressionLevel;
begin
  Result := FLevel;
end;

function TZipFile.GetOnGetPassWord: TZipGetPassWordEvent;
begin
  Result := FOnGetPassWord;
end;

function TZipFile.GetPassWord: AnsiString;
begin
  Result := FPassWord;
end;

function TZipFile.IndexOf(const AName: AnsiString): Integer;
begin
  Result := FFiles.IndexOf(AName);
end;

////////////////////////////////////////////////////////////////////////////////
//设计: LSUPER 2006.01.29
//功能: 加载文件信息
//参数：
////////////////////////////////////////////////////////////////////////////////
procedure TZipFile.LoadEndOfCentralDirectory(AStream: TStream);
begin
  Assert(AStream <> nil);

  with AStream do
  begin
    Seek(-SizeOf(conEndOfCentralDirSignature), soFromCurrent);
    Read(FEndOfCentralDir, SizeOf(FEndOfCentralDir));
    SetLength(FFileComment, FEndOfCentralDir.ZipfileCommentLength);
    Read(Pointer(FFileComment)^, FEndOfCentralDir.ZipfileCommentLength);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2004.08.18
//功能: 由文件加载
//参数：
////////////////////////////////////////////////////////////////////////////////
procedure TZipFile.LoadFromFile(const AFile: AnsiString);
var
  stream: TStream;
begin
  stream := TBufferedFileReader.Create(AFile);
  try
    LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2004.08.18
//功能: 由文件流加载数据
//参数：
//注意：读取 CustomFileHeader 时同时做 conFileHeaderSignature 和
////////////////////////////////////////////////////////////////////////////////
procedure TZipFile.LoadFromStream(AStream: TStream);
  ////////////////////////////////////////////////////////////////////////////////
  //设计：Linc 2009.02.05
  //功能：定位下一个文件描述块
  //参数：
  ////////////////////////////////////////////////////////////////////////////////
  function FindSignature(ASignature: DWORD; AEndSignature: DWORD = 0): Boolean;
  var
    nReaded: Integer;
    nSignature: DWORD;
    bMatch: Boolean;
  begin
    repeat
      nSignature := 0;
      nReaded := AStream.Read(nSignature, SizeOf(nSignature));
      if nReaded < SizeOf(nSignature) then
      begin
        Result := False;
        Exit;
      end;
      if AEndSignature = 0 then
        bMatch := nSignature = ASignature
      else bMatch := (nSignature = ASignature) or (nSignature = AEndSignature);
      if not bMatch then
        AStream.Seek(-3, soCurrent);
    until bMatch;
    Result := nSignature = ASignature;
    if (AEndSignature <> 0) and not Result then
      AStream.Seek(-SizeOf(nSignature), soCurrent);
  end;
  ////////////////////////////////////////////////////////////////////////////////
  //设计：Linc 2009.02.05
  //功能：按照自定义文件标记读取数据头信息
  //参数：
  ////////////////////////////////////////////////////////////////////////////////
  function FindCustomFileHeader: Boolean;
  var
    nReaded, nPos, nLen: Integer;
    nSignature: DWORD;
  begin
    Result := False;
    nPos := AStream.Position;
    nReaded := AStream.Read(nSignature, SizeOf(nSignature));
    if nReaded < SizeOf(nSignature) then
      Exit;
    if nSignature <> conFileHeaderSignature then
    begin
      AStream.Seek(-SizeOf(nSignature), soCurrent);
      Result := False;
      Exit;
    end;
    try
      nReaded := AStream.Read(nLen, SizeOf(nLen));
      if (nReaded < SizeOf(nLen)) or (AStream.Position + nLen >= AStream.Size) then
        Exit;
      SetLength(FCustomFileHeader, nLen);
      AStream.Read(Pointer(FCustomFileHeader)^, nLen);
      Result := True;
    finally
      if not Result then
        AStream.Position := nPos;
    end;
  end;
  ////////////////////////////////////////////////////////////////////////////////
  //设计：Linc 2009.02.05
  //功能：按照 ZIP 文件标记读取数据头信息
  //参数：
  ////////////////////////////////////////////////////////////////////////////////
  function FindCustomFileHeaderBySignature: Boolean;
  var
    nReaded, nPos: Integer;
  begin
    nPos := AStream.Position;
    Result := FindSignature(conLocalFileHeaderSignature);
    if not Result then
      Exit;
    nReaded := AStream.Position - nPos - SizeOf(Integer);
    AStream.Position := nPos;
    SetLength(FCustomFileHeader, nReaded);
    AStream.Read(Pointer(FCustomFileHeader)^, nReaded);
  end;
var
  nIndex: Integer;
  cEntity: IZipFileEntry;
  cPersistent: IZipPersistent;
begin
  Assert(AStream <> nil);
  FFiles.Clear;
  FCustomFileHeader := '';
  if not FindCustomFileHeader and not FindCustomFileHeaderBySignature then
  begin
    ZipFileError(rsErrFileHeader);
  end;
  while FindSignature(conLocalFileHeaderSignature, conCentralFileHeaderSignature) do
  begin
    cEntity := TZipFileEntry.Create(Self);
    cPersistent := cEntity as IZipPersistent;
    cPersistent.LoadLocalFileHeader(AStream);
    cPersistent.LoadDataDescriptor(AStream);
    FFiles.Add(cEntity);
  end;
  nIndex := 0;
  while FindSignature(conCentralFileHeaderSignature, conEndOfCentralDirSignature) do
  begin
    cEntity := FFiles[nIndex];
    cPersistent := cEntity as IZipPersistent;
    cPersistent.LoadCentralDirectory(AStream);
    Inc(nIndex);
  end;
  if FindSignature(conEndOfCentralDirSignature) then
  begin
    LoadEndOfCentralDirectory(AStream);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: LSUPER 2006.01.29
//功能: 保存文件信息
//参数：
////////////////////////////////////////////////////////////////////////////////
procedure TZipFile.SaveEndOfCentralDirectory(AStream: TStream;
  ACentralDirectoryOffset: LongWord);
begin
  Assert(AStream <> nil);

  with FEndOfCentralDir do
  begin
    EndOfCentralDirSignature := conEndOfCentralDirSignature;
    NumberOfThisDisk := 0;
    NumberOfTheDiskWithTheStart := 0;
    TotalNumberOfEntriesOnThisDisk := FFiles.Count;
    TotalNumberOfEntries := FFiles.Count;
    SizeOfTheCentralDirectory := AStream.Position - ACentralDirectoryOffset;
    OffsetOfStartOfCentralDirectory := ACentralDirectoryOffset;
    ZipfileCommentLength := Length(FFileComment);
  end;
  with AStream do
  begin
    Write(FEndOfCentralDir, SizeOf(FEndOfCentralDir));
    Write(Pointer(FFileComment)^, Length(FFileComment));
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2004.08.18
//功能: 向文件保存
//参数：
////////////////////////////////////////////////////////////////////////////////
procedure TZipFile.SaveToFile(const AFile: AnsiString);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(AFile, fmCreate or fmOpenReadWrite or fmShareDenyWrite);
  try
    SaveToStream(stream);
    FlushFileBuffers(stream.Handle);
  finally
    stream.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//设计: Linc 2004.08.18
//功能: 向文件流保存
//参数：
////////////////////////////////////////////////////////////////////////////////
procedure TZipFile.SaveToStream(AStream: TStream);
var
  nOffset: LongWord;
  I, nLen: Integer;
  cPersistent: IZipPersistent;
begin
  Assert(AStream <> nil);
  if FCustomFileHeader <> '' then
  begin
    AStream.Write(conFileHeaderSignature, SizeOf(conFileHeaderSignature));
    nLen := Length(FCustomFileHeader);
    AStream.Write(nLen, SizeOf(nLen));
    AStream.Write(Pointer(FCustomFileHeader)^, nLen);
  end;
  for I := 0 to FFiles.Count - 1 do
  begin
    cPersistent := FFiles[I] as IZipPersistent;
    cPersistent.SaveLocalFileHeader(AStream);
    cPersistent.SaveDataDescriptor(AStream);
  end;
  nOffset := AStream.Position;
  for I := 0 to FFiles.Count - 1 do
  begin
    cPersistent := FFiles[I] as IZipPersistent;
    cPersistent.SaveCentralDirectory(AStream);
  end;
  with Self do
  begin
    SaveEndOfCentralDirectory(AStream, nOffset);
  end;
end;

procedure TZipFile.SetCustomFileHeader(const Value: AnsiString);
begin
  FCustomFileHeader := Value;
end;

procedure TZipFile.SetFileComment(const Value: AnsiString);
begin
  FFileComment := Value;
end;

procedure TZipFile.SetLevel(const Value: TZipCompressionLevel);
begin
  FLevel := Value;
end;

procedure TZipFile.SetOnGetPassWord(const Value: TZipGetPassWordEvent);
begin
  FOnGetPassWord := Value;
end;

procedure TZipFile.SetPassWord(const Value: AnsiString);
begin
  if Length(Value) > defMaxPassWordLen then
    ZipFileError(rsErrMaxLenPassWord);
  FPassWord := Value;
end;

end.
