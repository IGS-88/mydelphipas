unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uCapture, StdCtrls, ExtCtrls, uLogger, {uMerger,}
  AudioCapture, AudioTask, MyUtils ,TlHelp32, ActnList, uCaptureTypes;

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
    btn8: TButton;
    edt3: TEdit;
    edt4: TEdit;
    edt5: TEdit;
    btn9: TButton;
    btn10: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure btn5Click(Sender: TObject);
    procedure btn6Click(Sender: TObject);
    procedure btn7Click(Sender: TObject);
    procedure act_pointExecute(Sender: TObject);
    procedure btn8Click(Sender: TObject);
    procedure btn9Click(Sender: TObject);
    procedure btn10Click(Sender: TObject);
  private
    { Private declarations }
    s: TTerminateInfo;
    procedure OnLog(Sender: TObject; const ALogInfo: uLogger.TLogInfo);
    procedure OnPreviewBitmap(Sender: TObject; const APreviewInfo: TPreviewInfo);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  ScreenCapture1: TScreenCapture;
  ScreenCapture2: TScreenCapture;
  TargetPID: Cardinal= 0;

  VIO: TVideoInputOption;
//  Merger: TMerger;

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
//  Edit1.Text := 'offset=0,0;framesize=500,500;framerate=15/1;showframe=1;cursor=1;grabmode=1;';
  Edit1.Text := '1443066';
  edt2.Text := '百度与作家团体关键问题仍对峙 110328 北京您早 - 视频 - 优酷视频 - 在线观看 - Windows Internet Explorer';

  CreateAudioTasks();
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ScreenCapture1.PreviewBitmap := True;
  ScreenCapture1.ProgressInterval := 100;
//  ScreenCapture1.SetCaptureOptions(Edit1.Text);
  InitVideoInputOption(@VIO);
  VIO.Handle := StrToInt(Edit1.Text);
  VIO.Height := 500;
  VIO.Width := 500;
  VIO.ShowFrame := 1;
  VIO.Cursor := 1;
  ScreenCapture1.SetCaptureOptions(VIO);
  ScreenCapture1.UseDefaultOO;
  if not ScreenCapture1.Start('ScreenCapture1.mp4') then
  ShowMessage('Error');
  StartTask(TargetPID);
end;

procedure TForm1.btn3Click(Sender: TObject);
begin
  ScreenCapture1.Stop;
  if Assigned(ScreenCapture2) then
  ScreenCapture2.Stop;

  StopTask(TargetPID);
  MakeWavAsOne(TargetPID);
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
  PauseTask(TargetPID);
end;

procedure TForm1.btn4Click(Sender: TObject);
begin
  ScreenCapture1.Resume;
  ContinueTask(TargetPID);
end;

procedure TForm1.btn5Click(Sender: TObject);
var
  hwnd: Cardinal;
  ProcessID: Cardinal;
  pa: string;
begin
  hwnd := FindWindow(nil, PAnsiChar(edt2.text));
   GetWindowThreadProcessId(hwnd,@ProcessID);
  TargetPID := StrToInt(edtPid.Text);
  Edit1.Text := Edit1.Text + 'hwnd='+inttostr(hwnd);

  pa:= ExePath;
  AddTask(TargetPID, ExePath, Self);

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
//  ScreenCapture2.SetCaptureOptions(Edit1.Text);
  InitVideoInputOption(@VIO);
  VIO.Handle := StrToInt(Edit1.Text);
  VIO.Height := 500;
  VIO.Width := 500;
  VIO.ShowFrame := 1;
  VIO.Cursor := 1;
  VIO.GrabMode := TGrabMode(1);
  ScreenCapture2.SetCaptureOptions(VIO);
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

procedure TForm1.btn8Click(Sender: TObject);
begin
//  if not Assigned(Merger) then
//    Merger := TMerger.Create(Self);
//  Merger.AddVideo(edt3.Text);
//  Merger.AddAudio(edt4.Text);
//  Merger.UseDefaultOO;
//  Merger.Start(edt5.Text);
end;

procedure TForm1.btn9Click(Sender: TObject);
begin
//  if not Assigned(Merger) then
//    Merger := TMerger.Create(Self);
//  Merger.VideoStreamID := Merger.GetFirstVideoStream(edt3.Text);
//  Merger.AudioStreamID := Merger.GetFirstAudioStream(edt4.Text);
end;


procedure TForm1.btn10Click(Sender: TObject);
begin
  ScreenCapture1.ResetHandle(StrToInt(Edit1.Text));
end;

end.
