unit MMSOfficeUnits;

interface
uses
  ComObj, ExtCtrls, Messages, SysUtils, Variants, Classes, Dialogs,
  Windows, Graphics, Controls, Forms, StdCtrls, Spin;
type
  ExcelArray = array of array of string;//定义二维数组

  //函数实现部分，主要包含读取和写入
  procedure ExcelWrite(Aarray : ExcelArray ; APath : string;
    const AStartRow : Integer = 1; const AStartCol : Integer = 1);
  function ExcelRead(APath : string;
    const AStartRow : Integer = 1; const AStartCol : Integer = 1) : ExcelArray;


implementation

//////////////////////////////////////////////////////////////////////
// 创建：Roube  13-05-2012
// 接替：Roube
// 功能：实现将数组写入excel文件
// 参数：写入的二维string数组，以及起始行列，行列可省略
//////////////////////////////////////////////////////////////////////
procedure ExcelWrite(Aarray : ExcelArray ; APath : string;
    const AStartRow : Integer = 1; const AStartCol : Integer = 1);
var
  excelApp, WorkSheet: Variant; //声明为OLE Automation 对象
  lRow, lCol : Integer; //变量用于循环写入
begin
    try
      //创建OLE对象Excel Application与 WorkBook
      excelApp := CreateOleObject('Excel.Application');
      WorkSheet := CreateOleobject('Excel.Sheet');
    except
      ShowMessage('您的机器里未安装Microsoft Excel。');
      Exit;
    end;
    try
      WorkSheet := excelApp.workBooks.Add;
      //循环写入行列从1开始
      for lRow := Low(Aarray) to High(Aarray) do
        for lCol := Low(Aarray[lRow]) to High(Aarray[lRow]) do
        begin
          excelApp.Cells[lRow + AStartRow, lCol + AStartCol].NumberFormatLocal:='@';//设置文本为字符型
          excelApp.Cells[lRow + AStartRow, lCol + AStartCol] := Aarray[lRow][lCol];
        end;

      WorkSheet.SaveAs(APath);
      WorkSheet.Close;
      WorkSheet := excelApp.workBooks.Open(APath);
      WorkSheet.Save;

      WorkSheet.Close;
      excelApp.Quit;
      excelApp := Unassigned;
    except
      ShowMessage('不能正确操作Excel文件。可能是该文件已被其他程序打开, 或系统错误。');
      WorkSheet.Close;
      excelApp.Quit;//退出Excel Application
      excelApp:=Unassigned;//释放VARIANT变量
    end;
end;
//////////////////////////////////////////////////////////////////////
// 创建：Roube  13-05-2012
// 接替：Roube
// 功能：实现读取excel文件，返回二维数组
// 参数：excel所在的路径 d:\1.xls，以及读取起始行列，行列可省略，省略行列则从1开始
//////////////////////////////////////////////////////////////////////
function ExcelRead(APath : string;
  const AStartRow : Integer = 1; const AStartCol : Integer = 1) : ExcelArray;
var
  excelApp, WorkSheet: Variant; //声明为OLE Automation 对象
  lRow, lCol, lMaxRow, lMaxCol : Integer; //变量用于循环写入
begin
  APath := trim(APath);
  try
    excelApp := CreateOLEObject('Excel.Application');
  except
    ShowMessage('您的机器里未安装Microsoft Excel。');
    Exit;
  end;
  try
    excelApp.Visible := false;
    excelApp.WorkBooks.Open(APath);
    //获得Excel的行、列
    lMaxRow :=excelApp.WorkSheets[1].UsedRange.Rows.Count;
    lMaxCol :=excelApp.WorkSheets[1].UsedRange.Columns.Count;

    SetLength(Result, lMaxRow + 1 - AStartRow);
    for lRow := Low(Result) to High(Result) do
    begin
      SetLength(Result[lRow], lMaxCol + 1 - AStartCol);
      for lCol := Low(Result[lRow]) to High(Result[lRow]) do
      begin
        Result[lRow][lCol] := trim(excelApp.WorkSheets[1].Cells[lRow + AStartRow,
          lCol + AStartCol].value)
      end;
    end;
    ExcelApp.WorkBooks.Close; //关闭工作簿
    ExcelApp.Quit; //退出 Excel
    ExcelApp:=Unassigned;//释放excel进程
  except
    ShowMessage('不能正确操作Excel文件。可能是该文件已被其他程序打开, 或系统错误。');
    WorkSheet.Close;
    excelApp.Quit;//退出Excel Application
    excelApp:=Unassigned;//释放VARIANT变量
  end;
end;


end.
