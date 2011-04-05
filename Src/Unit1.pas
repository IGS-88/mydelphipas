unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uScreenCapture, StdCtrls, ExtCtrls, uLogger,
  AudioCapture, AudioTask, MyUtils ,TlHelp32, ActnList;

type
  TForm1 = class(TForm)
    mmo1: TMemo;
    img1: TImage;
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    btn4: TButton;
    btn5: TButton;
    Edit1: TEdit;
    lbl1: TLabel;
    edt1: TEdit;
    edt2: TEdit;
    btn6: TButton;
    Listbox: TListBox;
    edtPid: TEdit;
    btn7: TButton;
    actlst1: TActionList;
    act_point: TAction;
    procedure FormCreate(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure btn5Click(Sender: TObject);
    procedure btn6Click(Sender: TObject);
    procedure btn7Click(Sender: TObject);
    procedure act_pointExecute(Sender: TObject);
  private
    { Private declarations }
    procedure OnLog(Sender: TObject; const ALogInfo: uLogger.TLogInfo);
    procedure OnPreviewBitmap(Sender: TObject; const APreviewInfo: TPreviewInfo);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  ScreenCapture1: TScreenCapture;
  ScreenCapture2: TScreenCapture;
  PID: Cardinal;
//  Ac: TAudioCapture;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
  ScreenCapture1 := TScreenCapture.Create(Self);
//  ScreenCapture1.OnError := OnError;
  ScreenCapture1.OnPreviewBitmap := OnPreviewBitmap;
  mmo1.Lines.Add('Create ScreenCapture');
  SetOnLogEvent(OnLog);
  Edit1.Text := 'offset=0,0;framesize=500,500;framerate=15/1;showframe=1;cursor=1;usedc=true;';
  edt2.Text := '百度与作家团体关键问题仍对峙 110328 北京您早 - 视频 - 优酷视频 - 在线观看 - Windows Internet Explorer';



end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ScreenCapture1.PreviewBitmap := True;
  ScreenCapture1.ProgressInterval := 100;
  ScreenCapture1.SetCaptureOptions(Edit1.Text);
  ScreenCapture1.UseDefaultOO;
  if not ScreenCapture1.Start('ScreenCapture1.mp4') then
  ShowMessage('Error');

//  Ac.Start;

end;

procedure TForm1.btn3Click(Sender: TObject);
begin
  ScreenCapture1.Stop;
  ScreenCapture2.Stop;

//  Ac.Stop;
//  Ac.MakeAudioFile;
//  UninjectTarge(PID);
end;

procedure TForm1.OnPreviewBitmap(Sender: TObject;
  const APreviewInfo: TPreviewInfo);
begin
  img1.Picture.Assign(APreviewInfo.Bitmap);
  
end;

procedure TForm1.OnLog(Sender: TObject; const ALogInfo: TLogInfo);
begin
  mmo1.Lines.Add('#'+inttostr(ALogInfo.ThreadID)+{' PntGUID:'+ALogInfo.PntGUID+}
                 ' LogLevel:'+inttostr(ord(ALogInfo.LogLevel))+' LogMsg:'+ALogInfo.LogMsg);
end;

procedure TForm1.btn2Click(Sender: TObject);
begin
  ScreenCapture1.Pause;
end;

procedure TForm1.btn4Click(Sender: TObject);
begin
  ScreenCapture1.Resume;
end;

procedure TForm1.btn5Click(Sender: TObject);
var
  hwnd: Cardinal;
  ProcessID: Cardinal;
begin
  hwnd := FindWindow(nil, PAnsiChar(edt2.text));
   GetWindowThreadProcessId(hwnd,@ProcessID);
  PID := StrToInt(edtPid.Text);
  Edit1.Text := Edit1.Text + 'hwnd='+inttostr(hwnd);

  Ac:= TAudioCapture.Create(PID,ExePath);
  if not InjectTarge(PID) then
  begin
    ShowMessage('1');
  end;
  
end;

procedure TForm1.btn6Click(Sender: TObject);
var
  ProcessName : string; //进程名
  ProcessID  : integer; //进程表示符
  ContinueLoop:BOOL;
  FSnapshotHandle:THandle; //进程快照句柄
  FProcessEntry32:TProcessEntry32; //进程入口的结构体信息
begin
  Listbox.Clear;
  FSnapshotHandle:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0); //创建一个进程快照
  FProcessEntry32.dwSize:=Sizeof(FProcessEntry32);
  ContinueLoop:=Process32First(FSnapshotHandle,FProcessEntry32); //得到系统中第一个进程
  //循环例举
  while ContinueLoop  do
  begin
    ProcessName := FProcessEntry32.szExeFile;
    ProcessID := FProcessEntry32.th32ProcessID;
    Listbox.Items.add('Name:'+ProcessName +'  #ID:'+ inttostr(ProcessID));
    //Listbox.Items.add(inttostr(ProcessID));
    ContinueLoop:=Process32Next(FSnapshotHandle,FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

procedure TForm1.btn7Click(Sender: TObject);
begin
  ScreenCapture2 := TScreenCapture.Create(Self);
  ScreenCapture2.SetCaptureOptions(Edit1.Text);
  ScreenCapture2.UseDefaultOO;
  if not ScreenCapture2.Start('ScreenCapture2.mp4') then
  begin
    ShowMessage('Second Error');
  end;
end;

procedure TForm1.act_pointExecute(Sender: TObject);
var
  pos: TPoint;
  hd: HWND;
begin
  GetCursorPos(pos);
  hd := WindowFromPoint(pos);
  ShowMessage(IntToStr(hd));
end;

end.
