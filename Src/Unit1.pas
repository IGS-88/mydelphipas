unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uScreenCapture, FFBaseComponent, FFLog, StdCtrls, ExtCtrls, uLogger;

type
  TForm1 = class(TForm)
    FFLogger1: TFFLogger;
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
    procedure FormCreate(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure btn5Click(Sender: TObject);
  private
    { Private declarations }
    procedure OnLog(Sender: TObject; const ALogInfo: TLogInfo);
    procedure OnPreviewBitmap(Sender: TObject; const APreviewInfo: TPreviewInfo);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  ScreenCapture1: TScreenCapture;
  ScreenCapture2: TScreenCapture;
  PID: Cardinal;
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
  Edit1.Text := 'offset=0,0;framesize=500,500;framerate=15/1;showframe=1;cursor=1;';
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
end;

procedure TForm1.btn3Click(Sender: TObject);
begin
  ScreenCapture1.Stop;
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
  PID := ProcessID;
  Edit1.Text := Edit1.Text + 'hwnd='+inttostr(hwnd);
end;

end.
