unit uWebCapture;

interface
uses
  Windows, SysUtils, Classes, MyUtils, uScreenCapture, webProcessWaveCapture, FFEncode, uGUID, uLogger, uCaptureTypes;
type
  {PProgressInfo = ^TProgressInfo;
  TProgressInfo = record
    TaskIndex: Integer;     // index of converting tasks
    FileIndex: Integer;     // index of input files in the current task
    FrameNumber: Integer;   // current frame number
    FPS: Integer;           // video converting speed, frames per second, not valid when only audio output
    Quality: Single;        // quality
    BitRate: Single;        // bitrate
    CurrentSize: Int64;     // current output file size in bytes
    CurrentDuration: Int64; // current duration time in microsecond
    TotalDuration: Int64;   // total output duration time in microsecond
  end;}
  TProgressInfo = FFEncode.TProgressInfo;
  TProgressEvent = procedure(Sender: TObject; AProgressInfo: PProgressInfo) of object;

  {TTerminateInfo = record
    TaskIndex: Integer;     // index of converting tasks, (-1) means all tasks are terminated
    Finished: Boolean;      // True means converted success, False means converting breaked
    Exception: Boolean;     // True means Exception occured, False please ignore
    ExceptionMsg: string;   // Exception message
  end;}
  TTerminateInfo = FFEncode.TTerminateInfo;
  TTerminateEvent = procedure(Sender: TObject; const ATerminateInfo: TTerminateInfo) of object;

  {TPreviewInfo = record
    TaskIndex: Integer;     // index of converting tasks
    Bitmap: TBitmap;        // bitmap filled with the target video picture, you can save it,
                            //  or paint it on any canvas(or any other control) which you want.
    FrameNumber: Integer;   // frame index number, first is 1 not 0
    PTS: Int64;             // presentation time stamp of current picture, in microseconds
  end;}
  TPreviewInfo = FFEncode.TPreviewInfo;

  // only triggered with property PreviewBitmap = True
  TPreviewBitmapEvent = procedure(Sender: TObject; const APreviewInfo: TPreviewInfo) of object;

  TErrorEvent = procedure(Sender: TObject; const ErrorInfo: string) of object;

  TCaptureStatus = (csStopped, csWorking, csPaused);

  TWebCapture = class(TObject)
  private
    { FFEncoder referenced }
    FLibAVPath: string;       //DLLs path
    LIndex: Integer;          //任务编号
    FOutFileName: WideString; //输出路径
    FOO: TOutputOptions;
    FIO: TInputOptions;       //视频截图的IO
    FAO: TInputOptions;       //音频录制的IO
    FEncode: TFFEncoder;
    FStatus: TCaptureStatus;     //FEncode的工作状态
    FScreenOption: String;       //录屏参数设置

    FAudioOption: string;        //声音录制参数设置
    FMute: Boolean;              //是否静默录制
    FWavePauseFlag: Integer;     //声音暂停标志位 1为暂停，0为录制

    FID: String;                 //唯一的身份识别码  { GUID }
    FLastError: String;
    FGrabMode: TGrabMode;        //GrabMode
    FMutex: THandle;             //线程锁访问
    FSCaptureForm: TCaptureForm; //虚拟的截屏窗口，表示所截窗口。Shared with threads;
    FCaptureForm: TCaptureForm;  //Used in current thread only;使用SynCpForm与FSCaptureForm同步

    FOnAudioHook: TAudioHookEvent;
    FOnPreviewBitmap: TPreviewBitmapEvent;
    FOnProgress:  TProgressEvent;
    FOnTerminate: TTerminateEvent;
    FOnInputVideoHook:  TVideoHookEvent;
    FOnOutputVideoHook: TVideoHookEvent;

    procedure DoProgress(Sender: TObject; AProgressInfo: PProgressInfo);
    procedure DoTerminate(Sender: TObject; const ATerminateInfo: TTerminateInfo);
    procedure DoPreviewBitmap(Sender: TObject; const APreviewInfo: TPreviewInfo);
    procedure DoAudioHook(Sender: TObject; ATaskIndex: Integer; const APTS: Int64; ASample: PByte;
                          ASize, ASampleRate, AChannels: Integer; ASampleFormat: TSampleFormat);
    procedure DoVieoInputHook(Sender: TObject; AHookInfo: PHookInfo);
    procedure DoVieoOutputHook(Sender: TObject; AHookInfo: PHookInfo);

    procedure DoError(ErrorMsg: string);
    procedure SetPreviewBitmap(Value: Boolean);
    function  ReadPreviewBitmap: Boolean;
    procedure SetProgressIntegerval(Value: Integer);
    function  ReadProgressIntegerval: Integer;

    procedure InitFCaptureForm;     //初始化FCaptureForm
    procedure SetCpForm(Hwnd: HWND;Left, Top: Integer; GrabMode: TGrabMode; ShowFrame: Integer);  //设置FCaptureForm
    function  ReadFCaptureForm: TCaptureForm; //从FSCaptureForm同步FCaptureForm然后传给外部。
    procedure SynToShared;    //将FCaptureForm同步到FSCaptureForm
    procedure SynFromShared;  //从FSCaptureForm同步FCaptureForm

  protected
    //
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  published
    {  事件代理 }
    property  OnAudioHook: TAudioHookEvent read FOnAudioHook write FOnAudioHook;
    property  OnPreviewBitmap: TPreviewBitmapEvent read FOnPreviewBitmap write FOnPreviewBitmap;
    property  OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property  OnTerminate: TTerminateEvent read FOnTerminate write FOnTerminate;
    property  OnVideoInputHook: TVideoHookEvent read FOnInputVideoHook write FOnInputVideoHook;
    property  OnVideoOutputHook: TVideoHookEvent read FOnOutputVideoHook write FOnOutputVideoHook;

    { 可读写的属性 }
    property  LibAVPath: string read FLibAVPath write FLibAVPath;
    property  PreviewBitmap: Boolean read ReadPreviewBitmap write SetPreviewBitmap;
    property  ProgressInterval: Integer read ReadProgressIntegerval write SetProgressIntegerval;

    { 只读属性 }
    property  Encoder: TFFEncoder read FEncode;
    property  ID: string read FID;
    property  Status: TCaptureStatus read FStatus;
    property  LastError: string read FLastError;
    property  CaptureForm: TCaptureForm read ReadFCaptureForm;

    { 对外接口 }
    function  Start(OutPutFile: WideString): Boolean;
    procedure Pause;
    procedure Resume;
    procedure Stop;

    procedure  SetCaptureOptions(VIO: TVideoInputOption; AIO: TAudioInputOption);
    function   SetOutputOptins({参数待定}): Boolean;  { TODO : 初始化并设置 TOutputOption  FOO }
    procedure  UseDefaultOO;

    procedure  SetVoice(Mute: Boolean);
    function   AskAudioFormat: TAudioInputOption;

    procedure  ResetHandle(NewHwnd: HWND);
    procedure  ResetOffset(x_off, y_off: Integer);
    procedure  ResetGrabMode(NewGrabMode: TGrabMode);
    procedure  ResetShowFrame(ShowFrame: Integer);
  end;

implementation

{ TScreenCapture }

constructor TWebCapture.Create;
begin
  FLibAVPath := ExePath + 'LibAV';
  FID := GetGUID;
  FOO.FileName := 'UnInit';
  FMutex := CreateMutex(nil, False, 'Mutex');  //创建Mutex锁
  FGrabMode := gmDC;
  FWavePauseFlag := 0;
  FMute := False;
  InitFCaptureForm;

  FOnAudioHook := nil;
  FOnPreviewBitmap := nil;
  FOnProgress := nil;
  FOnTerminate := nil;
  FOnOutputVideoHook := nil;
  FOnInputVideoHook := nil;

  FEncode := TFFEncoder.Create(AOwner);
  if FEncode = nil then
  begin
    DoError('Create FFEncode Failed!');
    Exit;
  end;
  with FEncode do
  begin
    Preview := False;
    PreviewBitmap := False;
    ProgressInterval := 500;
    ThreadPriority := tpNormal;
    TriggerEventInMainThread := False;

    OnAudioHook := DoAudioHook;
    OnPreviewBitmap := DoPreviewBitmap;
    OnProgress := DoProgress;
    OnTerminate := DoTerminate;
    OnVideoInputHook := DoVieoInputHook;
    OnVideoOutputHook := DoVieoOutputHook;
  end;

  FStatus := csStopped;
end;

destructor TWebCapture.Destroy;
begin
  if FStatus <> csStopped then
  begin
    Stop;
  end;
  CloseHandle(FMutex); //释放Mutex句柄
  FEncode.Destroy;
  inherited;
end;

procedure TWebCapture.DoAudioHook(Sender: TObject; ATaskIndex: Integer;
  const APTS: Int64; ASample: PByte; ASize, ASampleRate,
  AChannels: Integer; ASampleFormat: TSampleFormat);
begin
  if Assigned(FOnAudioHook) then
  begin
    FOnAudioHook(Self, ATaskIndex, APTS, ASample, ASize, ASampleRate, AChannels, ASampleFormat);
  end;
end;

procedure TWebCapture.DoError(ErrorMsg: string);
begin
  FLastError := ErrorMsg;
//  if Assigned(FOnError) then
//  begin
//    FOnError(Self, FLastError);
//  end;
  WriteLog(GetCurrentThreadId, Self.ID, llerror, ErrorMsg+#10);
end;

procedure TWebCapture.DoPreviewBitmap(Sender: TObject;
  const APreviewInfo: TPreviewInfo);
begin
  if Assigned(FOnPreviewBitmap) then
  begin
    FOnPreviewBitmap(Self, APreviewInfo);
  end;
end;

procedure TWebCapture.DoProgress(Sender: TObject;
  AProgressInfo: PProgressInfo);
begin
  if Assigned(FOnProgress) then
  begin
    FOnProgress(Self, AProgressInfo);
  end;
end;

procedure TWebCapture.DoTerminate(Sender: TObject;
  const ATerminateInfo: TTerminateInfo);
begin
  if Assigned(FOnTerminate) then
  begin
    FOnTerminate(Self, ATerminateInfo);
  end;
end;

procedure TWebCapture.DoVieoInputHook(Sender: TObject;
  AHookInfo: PHookInfo);
begin
  if Assigned(FOnInputVideoHook) then
  begin
    FOnInputVideoHook(Self, AHookInfo);
  end;
end;

procedure TWebCapture.DoVieoOutputHook(Sender: TObject;
  AHookInfo: PHookInfo);
begin
  if Assigned(FOnOutputVideoHook) then
  begin
    FOnOutputVideoHook(Self, AHookInfo);
  end;
end;

function TWebCapture.ReadFCaptureForm: TCaptureForm;
begin
  SynFromShared;
  Result := FCaptureForm;
end;

procedure TWebCapture.InitFCaptureForm;
begin
  with FCaptureForm do
  begin
    Handle := 0;
    Left := 0;
    Top := 0;
    GrabMode := gmDC;
    ShowFrame := 1;
  end;
  FSCaptureForm := FCaptureForm;
end;

procedure TWebCapture.Pause;
begin
  FEncode.Pause;
  FWavePauseFlag := 1;
  FStatus := csPaused;
  WriteLog(GetCurrentThreadId, ID, llDebug, 'Capture Paused!'#10);
end;

function TWebCapture.ReadPreviewBitmap: Boolean;
begin
  Result := FEncode.PreviewBitmap;
end;

function TWebCapture.ReadProgressIntegerval: Integer;
begin
  Result := FEncode.ProgressInterval;
end;

procedure TWebCapture.Resume;
begin
  FEncode.Resume;
  FWavePauseFlag := 0;
  FStatus := csWorking;
  WriteLog(GetCurrentThreadId, ID, llDebug, 'Capture Resume!'#10);
end;

procedure TWebCapture.SetCaptureOptions(VIO: TVideoInputOption; AIO: TAudioInputOption);
var
  st: string;
begin
  FScreenOption := '';
  SetCpForm(VIO.Handle,VIO.x_off,VIO.y_off,VIO.GrabMode,VIO.ShowFrame);
  SynToShared;
  st := 'framerate=' + VIO.FrameRate + ';';
  st := 'framesize=' + IntToStr(VIO.Width) + ',' + IntToStr(VIO.Height) + ';';
  st := st + 'client=' + IntToStr(VIO.Client) + ';';
  st := st + 'cursor=' + IntToStr(VIO.Cursor) + ';';
  st := st + 'parentguid=' + FID + ';';
  st := st + 'mutex=' + IntToStr(FMutex) + ';';
  st := st + 'point_captureform=' + IntToStr(dword(@FSCaptureForm)) + ';';
  FScreenOption := st;

  FAudioOption := '';
  FAudioOption := 'r=' + IntToStr(AIO.sample_rate) + ';c=' + IntToStr(AIO.channels) + ';f=' + IntToStr(AIO.sample_format) +
                  ';voice=' + IntToStr(AIO.Voice) + ';pause_pointer=' + IntToStr(Integer(@FWavePauseFlag));
end;

procedure TWebCapture.SetCpForm(Hwnd: HWND;Left, Top: Integer; GrabMode: TGrabMode; ShowFrame: Integer);
begin
  FCaptureForm.Handle := Hwnd;
  FCaptureForm.Left := Left;
  FCaptureForm.Top := Top;
  FCaptureForm.GrabMode := GrabMode;
  FCaptureForm.ShowFrame := ShowFrame;
end;

function TWebCapture.SetOutputOptins: Boolean;
begin
  { TODO : 初始化并设置 TOutputOption  FOO }
  Result := True;
end;

procedure TWebCapture.SetPreviewBitmap(Value: Boolean);
begin
  FEncode.Preview := Value;
  FEncode.PreviewBitmap := Value;
end;

procedure TWebCapture.SetProgressIntegerval(Value: Integer);
begin
  FEncode.ProgressInterval := Value;
  WriteLog(GetCurrentThreadId, ID, llDebug, 'Set ProgressIntegerval = '+ IntToStr(Value)+#10);
end;

function TWebCapture.Start(OutPutFile: WideString): Boolean;
begin
  Result := False;
  if FStatus = csWorking then
  begin
    WriteLog(GetCurrentThreadId, FID, llWarning, 'Capture is working!');
    Exit;
  end;
  if FStatus = csPaused then
  begin
    WriteLog(GetCurrentThreadId, FID, llWarning, 'Capture is pausing, now resume!');
    Resume;
    Exit;
  end;

  // Load dynamic link libraries
  if not FEncode.AVLibLoaded then
  begin
    // TPathFileName = type WideString;
    // FFEncoder.LoadAVLib(const APath: TPathFileName): Boolean;
    // APath: Full path indicates location of FFmpeg DLLs.
    //        It can be empty, let Windows search DLLs in current dir or environment <PATH>
    //if not FFEncoder.LoadAVLib(ExtractFilePath(Application.ExeName) + CLibAVPath) then
    // the routine ExePath() is implemented in unit MyUtils which returns WideString type
    // of ExtractFilePath(Application.ExeName)
    if not FEncode.LoadAVLib(FLibAVPath) then
    begin
      DoError(FEncode.LastErrMsg);
      Exit;
    end;
    WriteLog(GetCurrentThreadId, ID, llDebug, 'Load AVLib, Path: '+ FLibAVPath);
    // register screen capture demuxer
    uScreenCapture.register_screencapture;
    webProcessWaveCapture.register_webprocesswavecapture;
  end;

  if FScreenOption = EmptyStr then
  begin
    DoError('OptionCaption is Empty!');
    Exit;
  end;

  if FOO.FileName = 'UnInit' then
  begin
    DoError('OutputOption is empty, please set OutputOption!');
    Exit;
  end;

  FOutFileName := OutPutFile;
  FEncode.ClearTasks;

  InitInputOptions(@FIO);
  FIO.ForceFormat := 'screencapture';

  LIndex := FEncode.AddTask(FScreenOption, @FIO);
  if LIndex < 0 then
  begin
    DoError('File open error: ' + FEncode.LastErrMsg);
    Exit;
  end;

  InitInputOptions(@FAO);
  FAO.ForceFormat := 'webprocesswavecapture';
  FWavePauseFlag := 0;

  if not FEncode.AddInputFile(LIndex, FAudioOption, @FAO) then
  begin
    DoError('Audio Failed');
    Exit;
  end;

  if not FEncode.SetOutputFile(LIndex, FOutFileName, @FOO) then
  begin
    FEncode.RemoveTask(LIndex);
    DoError('Cannot do convert, error: ' + FEncode.LastErrMsg);
    Exit;
  end;

  FEncode.Start(1);
  FStatus := csWorking;
  Result := True;
  WriteLog(GetCurrentThreadId, ID, llDebug, 'Capture Start!'#10);
end;

procedure TWebCapture.Stop;
begin
  FEncode.Stop;
  FStatus := csStopped;
  WriteLog(GetCurrentThreadId, ID, llDebug, 'Capture Stopped!'#10);
end;

procedure TWebCapture.SynToShared;
begin
  if WaitForSingleObject(FMutex, INFINITE) = WAIT_OBJECT_0 then
  begin
    FSCaptureForm := FCaptureForm;
  end;
  ReleaseMutex(FMutex);
end;

procedure TWebCapture.UseDefaultOO;
begin
  InitOutputOptions(@FOO);
  FOO.VideoCodec := 'mpeg4';
  WriteLog(GetCurrentThreadId, ID, llDebug, 'Use Default OutputOptions.'#10);
end;

procedure TWebCapture.SynFromShared;
begin
  if WaitForSingleObject(FMutex, INFINITE) = WAIT_OBJECT_0 then
  begin
    FCaptureForm := FSCaptureForm;
  end;
  ReleaseMutex(FMutex);
end;

procedure TWebCapture.ResetGrabMode(NewGrabMode: TGrabMode);
begin
  FCaptureForm.GrabMode := NewGrabMode;
  SynToShared;
end;

procedure TWebCapture.ResetHandle(NewHwnd: HWND);
begin
  FCaptureForm.Handle := NewHwnd;
  SynToShared;
end;

procedure TWebCapture.ResetOffset(x_off, y_off: Integer);
begin
  FCaptureForm.Left := x_off;
  FCaptureForm.Top := y_off;
  SynToShared;
end;

procedure TWebCapture.ResetShowFrame(ShowFrame: Integer);
begin
  FCaptureForm.ShowFrame := ShowFrame;
  SynToShared;
end;

function TWebCapture.AskAudioFormat: TAudioInputOption;
var
  param: string;
  N, V: string;
begin
  param := webProcessWaveCapture.AskFormat;
  if param = EmptyStr then
  begin
    DoError('Get Audio Format Failed!');
    Exit;
  end;
  while param <> '' do
  begin
    V := Fetch(param, ';');
    N := Fetch(V, '=');

    if SameText(N, 'r') then
      // sample rate
      Result.sample_rate := StrToIntDef(V, 44100)
    else if SameText(N, 'c') then
      // channels
      Result.channels := StrToIntDef(V, 2)
    else if SameText(N, 'f') then
      // sample format
      Result.sample_format := StrToIntDef(V, 16);
  end;
  if FMute then
    Result.Voice := 0
  else
    Result.Voice := 1;
end;

procedure TWebCapture.SetVoice(Mute: Boolean);
begin
  if Mute then
  begin
    webProcessWaveCapture.SendVoice(False);
    WriteLog(GetCurrentThreadId, FID, llInfo, 'Mute, now voice off.');
  end
  else
  begin
    webProcessWaveCapture.SendVoice(True);
    WriteLog(GetCurrentThreadId, FID, llInfo, 'Voice, now voice on.');
  end;

  FMute := Mute;
end;


end.
