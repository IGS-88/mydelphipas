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
    procedure FormCreate(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
  private
    { Private declarations }
    procedure OnLog(Sender: TObject; const ALogInfo: TLogInfo);
    procedure OnPreviewBitmap(Sender: TObject; const APreviewInfo: TPreviewInfo);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  ScreenCapture: TScreenCapture;
implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
  ScreenCapture := TScreenCapture.Create(Self);
//  ScreenCapture.OnError := OnError;
  ScreenCapture.OnPreviewBitmap := OnPreviewBitmap;
  mmo1.Lines.Add('Create ScreenCapture');
  SetOnLogEvent(OnLog);
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ScreenCapture.PreviewBitmap := True;
  ScreenCapture.ProgressInterval := 100;
  ScreenCapture.SetCaptureOptions(Edit1.Text);
  ScreenCapture.UseDefaultOO;
  if not ScreenCapture.Start('ScreenCapture.mp4') then
  ShowMessage('Error');
end;

procedure TForm1.btn3Click(Sender: TObject);
begin
  ScreenCapture.Stop;
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
  ScreenCapture.Pause;
end;

procedure TForm1.btn4Click(Sender: TObject);
begin
  ScreenCapture.Resume;
end;

end.
